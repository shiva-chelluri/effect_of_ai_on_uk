# ============================================================
# LIBRARIES
# ============================================================
library(tidyverse)
library(tikzDevice)
library(kableExtra)
library(panelsummary)
library(fixest)
library(WeightIt)
library(cobalt)
library(ggfixest)
library(arrow)
library(modelsummary)

# ============================================================
# DATA LOADING & BASELINE CLEANING
# ============================================================
DATA_DIR <- "./data/output/main.parquet"
data <- read_parquet(DATA_DIR)

data <- data %>%
  mutate(
    baseline_age = 2019 - year(start_date),
    sic_1digit = floor(sic_2digit / 10),
    ta = ntile(total_assets, 10),
    post = ifelse(year > 2021, 1, 0), 
    post_2019 = ifelse(year > 2019, 1, 0),
    post_2018 = ifelse(year > 2018, 1, 0), 
    post_2017 = ifelse(year > 2017, 1, 0)
  ) %>%
  filter(year > 2015, exists == 1, !is.na(lemp))

# ============================================================
# BASELINE TOTAL ASSETS
# ============================================================
# First observed total_assets for each firm (earliest year)
baseline_ta <- data %>%
  filter(!is.na(total_assets)) %>%
  arrange(bvd_id, year) %>%
  group_by(bvd_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(bvd_id, baseline_ta = total_assets)

data <- data %>% left_join(baseline_ta, by = "bvd_id")

# ============================================================
# ENTROPY BALANCING WEIGHTS (WeightIt)
# ============================================================
# Cross-sectional baseline for weighting (one row per firm)
baseline <- data %>%
  distinct(bvd_id, .keep_all = TRUE) %>%
  select(bvd_id, ai_2021, llm_2021, baseline_ta, baseline_age, sic_1digit) %>%
  filter(complete.cases(.))

# Entropy balancing for AI exposure
wb_ai <- weightit(ai_2021 ~ baseline_ta + baseline_age,
  data = baseline, method = "ebal",
  estimand = "ATE"
)

# Entropy balancing for LLM exposure
wb_llm <- weightit(llm_2021 ~ baseline_ta + baseline_age,
  data = baseline, method = "ebal",
  estimand = "ATE"
)

# Map weights back to the full panel
baseline <- baseline %>%
  mutate(
    w_ai = wb_ai$weights,
    w_llm = wb_llm$weights
  )

data <- data %>%
  left_join(baseline %>% select(bvd_id, w_ai, w_llm), by = "bvd_id")

rm(baseline_ta, baseline, wb_ai, wb_llm)

# ============================================================
# OUTPUT SETUP
# ============================================================
FIG_DIR   <- "./output/figures"
TABLE_DIR <- "./output/tables"
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

FIG_W <- 4
FIG_H <- 3

save_tikz <- function(plot_obj, filename) {
  filepath <- file.path(FIG_DIR, paste0(filename, ".tex"))
  tryCatch({
    tikz(filepath, width = FIG_W, height = FIG_H, standAlone = FALSE)
    print(plot_obj)
    dev.off()
    cat(sprintf("Saved: %s\n", filepath))
  }, error = function(e) {
    if (dev.cur() > 1) try(dev.off(), silent = TRUE)
    cat(sprintf("Warning (tikz): %s — %s\n", filepath, conditionMessage(e)))
  })
}

# ============================================================
# HELPER: Event study plot with Wald p-value footnote
# ============================================================
make_event_study_plot <- function(model, wald_obj) {
  # Extract p-value from the fixest_wald object
  pval <- wald_obj$p
  pval_text <- ifelse(pval < 0.001, "< 0.001", sprintf("%.3f", pval))

  # Generate plot using ggfixest
  p <- ggiplot(model) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_vline(xintercept = 2021, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    labs(
      title = "",
      x = "Year",
      y = "Estimate",
      caption = sprintf("Pre-trend Wald test p-value: %s", pval_text)
    ) +
    theme_classic(base_size = 9) +
    theme(
      panel.grid = element_blank(),
      plot.caption = element_text(hjust = 0, size = 7, face = "italic", color = "gray30"),
      plot.caption.position = "plot"
    )

  return(p)
}

# ============================================================
# CUSTOM GLANCE FOR IV DIAGNOSTICS (modelsummary hook)
# ============================================================
# This injects KP F-stat, Wu-Hausman, and model F-stat into the
# glance() output so that gof_map can pick them up in panelsummary.
glance_custom.fixest <- function(x, ...) {
  out <- data.frame(row.names = "")

  is_iv <- !is.null(x$iv) && !isFALSE(x$iv)

  # Kleibergen-Paap F-stat (IV models only)
  if (is_iv) {
    kp <- fitstat(x, ~ivwald1)
    out$kp_fstat <- kp$ivwald1$stat
  } else if (!is.null(x$kp_fstat)) {
    out$kp_fstat <- x$kp_fstat
  }

  # Wu-Hausman test p-value (IV models only)
  if (is_iv) {
    wh <- fitstat(x, ~wh)
    out$wu_hausman <- wh$wh$p
  } else if (!is.null(x$wu_hausman)) {
    out$wu_hausman <- x$wu_hausman
  }

  # Plain F-stat of excluded instruments
  if (is_iv) {
    ivf <- fitstat(x, ~ivf1)
    out$ivf1_stat <- ivf$ivf1$stat
  } else if (!is.null(x$ivf1_stat)) {
    out$ivf1_stat <- x$ivf1_stat
  }

  # Model F-statistic
  fst <- fitstat(x, ~f)
  out$f_stat <- fst$f$stat

  out
}

# =============================================================
# DYNAMIC MODELS
# =============================================================

# OLS — AI & LLM
ols.ai <- feols(
  lemp ~ i(year, ai_2011, ref = 2021) |
    bvd_id + year + sic_1digit[year],
  cluster = ~sic_2digit, nthreads = 10,
  weights = ~w_ai, data = data
)

ols.llm <- feols(
  lemp ~ i(year, llm_2011, ref = 2021) |
    bvd_id + year + sic_1digit[year],
  cluster = ~sic_2digit, nthreads = 10,
  weights = ~w_llm, data = data
)

ols.ai$wald_pval  <- wald(ols.ai,  keep = "year::(201[6-9]|2020):ai_2011")
ols.llm$wald_pval <- wald(ols.llm, keep = "year::(201[6-9]|2020):llm_2011")

# GENERATE & SAVE PLOTS

# AI Plot
p_ai <- make_event_study_plot(ols.ai, ols.ai$wald_pval)
save_tikz(p_ai, "ols_ai_main_spec")

# LLM Plot
p_llm <- make_event_study_plot(ols.llm, ols.llm$wald_pval)
save_tikz(p_llm, "ols_llm_main_spec")

# Dynamic 2SLS IV — AI & LLM
iv.ai <- feols(
  lemp ~ 1 | bvd_id + year + sic_1digit[year] |
    i(year, ai_2021, ref = 2021) ~ i(year, ai_2011, ref = 2021),
  cluster = ~sic_2digit, nthreads = 10,
  weights = ~w_ai, data = data
)

iv.llm <- feols(
  lemp ~ 1 | bvd_id + year + sic_1digit[year] |
    i(year, llm_2021, ref = 2021) ~ i(year, llm_2011, ref = 2021),
  cluster = ~sic_2digit, nthreads = 10,
  weights = ~w_llm, data = data
)

iv.ai$wald_pval  <- wald(iv.ai,  keep = "fit_year::(201[6-9]|2020):ai_2021")
iv.llm$wald_pval <- wald(iv.llm, keep = "fit_year::(201[6-9]|2020):llm_2021")

p_iv_ai <- make_event_study_plot(iv.ai, iv.ai$wald_pval)
save_tikz(p_iv_ai, "ols_ai_main_spec_iv")

p_iv_llm <- make_event_study_plot(iv.llm, iv.llm$wald_pval)
save_tikz(p_iv_llm, "ols_llm_main_spec_iv")

# =============================================================
# STATIC DiD MODELS (OLS, REDUCED-FORM, IV, FIRST STAGE)
# =============================================================

# --- AI Exposure ---
static.ols.ai <- feols(
  lemp ~ i(post, ai_2021, ref = 0) |
    bvd_id + year + sic_1digit[year],
  cluster = ~sic_2digit, nthreads = 10,
  weights = ~w_ai, data = data
)

static.rf.ai <- feols(
  lemp ~ i(post, ai_2011, ref = 0) |
    bvd_id + year + sic_1digit[year],
  cluster = ~sic_2digit, nthreads = 10,
  weights = ~w_ai, data = data
)

static.iv.ai <- feols(
  lemp ~ 1 | bvd_id + year + sic_1digit[year] |
    i(post, ai_2021, ref = 0) ~ i(post, ai_2011, ref = 0),
  cluster = ~sic_2digit, nthreads = 10,
  weights = ~w_ai, data = data
)

static.fs.ai <- summary(static.iv.ai, stage = 1)
static.fs.ai$kp_fstat   <- fitstat(static.iv.ai, ~ivwald1)$ivwald1$stat
static.fs.ai$wu_hausman <- fitstat(static.iv.ai, ~wh)$wh$p

# --- LLM Exposure ---
static.ols.llm <- feols(
  lemp ~ i(post, llm_2021, ref = 0) |
    bvd_id + year + sic_1digit[year],
  cluster = ~sic_2digit, nthreads = 10,
  weights = ~w_llm, data = data
)

static.rf.llm <- feols(
  lemp ~ i(post, llm_2011, ref = 0) |
    bvd_id + year + sic_1digit[year],
  cluster = ~sic_2digit, nthreads = 10,
  weights = ~w_llm, data = data
)

static.iv.llm <- feols(
  lemp ~ 1 | bvd_id + year + sic_1digit[year] |
    i(post, llm_2021, ref = 0) ~ i(post, llm_2011, ref = 0),
  cluster = ~sic_2digit, nthreads = 10,
  weights = ~w_llm, data = data
)

static.fs.llm <- summary(static.iv.llm, stage = 1)
static.fs.llm$kp_fstat   <- fitstat(static.iv.llm, ~ivwald1)$ivwald1$stat
static.fs.llm$wu_hausman <- fitstat(static.iv.llm, ~wh)$wh$p

# =============================================================
# MAIN RESULTS TABLE (main_table.tex)
# =============================================================
gm_main <- tribble(
  ~raw,                   ~clean,                   ~fmt,
  "nobs",                 "Observations",           0,
  "kp_fstat",             "Kleibergen-Paap F-stat", 3,
  "wu_hausman",           "Wu-Hausman p-value",     3,
  "f_stat",               "F-statistic",            3,
  "FE: sic_1digit[year]", "Industry-Trend",         NA_real_,
  "FE: bvd_id",           "Firm FE",                NA_real_,
  "FE: year",             "Year FE",                NA_real_
)

panelsummary(
  list(static.ols.ai, static.rf.ai, static.iv.ai),
  list(static.ols.llm, static.rf.llm, static.iv.llm),
  panel_labels = c("Panel A: AI Exposure", "Panel B: LLM Exposure"),
  italic = TRUE,
  format = "latex",
  hline_after = TRUE,
  hline_before_fe = TRUE,
  pretty_num = TRUE,
  collapse_fe = TRUE,
  fmt = 3,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gm_main,
  coef_map = c(
    "post::1:ai_2021"      = "Exposure × Post",
    "post::1:ai_2011"      = "Exposure × Post",
    "fit_post::1:ai_2021"  = "Exposure × Post",
    "post::1:llm_2021"     = "Exposure × Post",
    "post::1:llm_2011"     = "Exposure × Post",
    "fit_post::1:llm_2021" = "Exposure × Post"
  ),
  caption = "Effect of Exposure on Semi-Elasticity of Employment",
  colnames = c("", "OLS", "Reduced-Form", "IV")
) %>%
  kable_styling(
    latex_options = c("booktabs", "hold_position"),
    position = "center",
    full_width = FALSE
  ) %>%
  save_kable(file.path(TABLE_DIR, "main_table.tex"))

# =============================================================
# FIRST-STAGE TABLE (first_stage_table.tex)
# =============================================================
gm_fs <- tribble(
  ~raw,                   ~clean,                   ~fmt,
  "nobs",                 "Observations",           0,
  "FE: bvd_id",           "Firm FE",                NA_real_,
  "FE: year",             "Year FE",                NA_real_,
  "FE: sic_1digit[year]", "Industry-trend",         NA_real_,
  "kp_fstat",             "Kleibergen-Paap F-stat", 3,
  "wu_hausman",           "Wu-Hausman p-value",     3
)

panelsummary(
  list(static.fs.ai),
  list(static.fs.llm),
  panel_labels = c("Panel A: AI Exposure First Stage", "Panel B: LLM Exposure First Stage"),
  italic = TRUE,
  format = "latex",
  hline_after = TRUE,
  hline_before_fe = TRUE,
  pretty_num = TRUE,
  collapse_fe = TRUE,
  fmt = 3,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gm_fs,
  coef_map = c(
    "post::1:ai_2011"  = "Instrument (AI 2011) × Post",
    "post::1:llm_2011" = "Instrument (LLM 2011) × Post"
  ),
  caption = "First-Stage: Effect of Instrument on Exposure",
  colnames = c("", "First Stage")
) %>%
  kable_styling(
    latex_options = c("booktabs", "hold_position"),
    position = "center",
    full_width = FALSE
  ) %>%
  save_kable(file.path(TABLE_DIR, "first_stage_table.tex"))

# =============================================================
# MECHANISM TABLES: TURNOVER & REMUNERATION
# =============================================================
data <- data %>%
  mutate(lremun = ifelse(remuneration > 0, log(remuneration), NA_real_))

# Turnover (levels)
turn.ols.ai  <- feols(turnover ~ i(post, ai_2021, ref = 0) | bvd_id + year + sic_1digit[year],
                      cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai, data = data)
turn.iv.ai   <- feols(turnover ~ 1 | bvd_id + year + sic_1digit[year] | i(post, ai_2021, ref = 0) ~ i(post, ai_2011, ref = 0),
                      cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai, data = data)
turn.ols.llm <- feols(turnover ~ i(post, llm_2021, ref = 0) | bvd_id + year + sic_1digit[year],
                      cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = data)
turn.iv.llm  <- feols(turnover ~ 1 | bvd_id + year + sic_1digit[year] | i(post, llm_2021, ref = 0) ~ i(post, llm_2011, ref = 0),
                      cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = data)

panelsummary(
  list(turn.ols.ai, turn.iv.ai),
  list(turn.ols.llm, turn.iv.llm),
  panel_labels = c("Panel A: AI Exposure", "Panel B: LLM Exposure"),
  italic = TRUE, format = "latex", hline_after = TRUE, hline_before_fe = TRUE,
  pretty_num = TRUE, collapse_fe = TRUE, fmt = 3,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gm_fs,
  coef_map = c(
    "post::1:ai_2021"      = "Exposure × Post",
    "fit_post::1:ai_2021"  = "Exposure × Post",
    "post::1:llm_2021"     = "Exposure × Post",
    "fit_post::1:llm_2021" = "Exposure × Post"
  ),
  caption = "Effect of Exposure on Turnover",
  colnames = c("", "OLS", "IV")
) %>%
  kable_styling(latex_options = c("booktabs", "hold_position"), position = "center", full_width = FALSE) %>%
  save_kable(file.path(TABLE_DIR, "turnover_table.tex"))

# Log Remuneration
rem.ols.ai  <- feols(lremun ~ i(post, ai_2021, ref = 0) | bvd_id + year + sic_1digit[year],
                     cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai, data = data)
rem.iv.ai   <- feols(lremun ~ 1 | bvd_id + year + sic_1digit[year] | i(post, ai_2021, ref = 0) ~ i(post, ai_2011, ref = 0),
                     cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai, data = data)
rem.ols.llm <- feols(lremun ~ i(post, llm_2021, ref = 0) | bvd_id + year + sic_1digit[year],
                     cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = data)
rem.iv.llm  <- feols(lremun ~ 1 | bvd_id + year + sic_1digit[year] | i(post, llm_2021, ref = 0) ~ i(post, llm_2011, ref = 0),
                     cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = data)

panelsummary(
  list(rem.ols.ai, rem.iv.ai),
  list(rem.ols.llm, rem.iv.llm),
  panel_labels = c("Panel A: AI Exposure", "Panel B: LLM Exposure"),
  italic = TRUE, format = "latex", hline_after = TRUE, hline_before_fe = TRUE,
  pretty_num = TRUE, collapse_fe = TRUE, fmt = 3,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gm_fs,
  coef_map = c(
    "post::1:ai_2021"      = "Exposure × Post",
    "fit_post::1:ai_2021"  = "Exposure × Post",
    "post::1:llm_2021"     = "Exposure × Post",
    "fit_post::1:llm_2021" = "Exposure × Post"
  ),
  caption = "Effect of Exposure on Log Remuneration",
  colnames = c("", "OLS", "IV")
) %>%
  kable_styling(latex_options = c("booktabs", "hold_position"), position = "center", full_width = FALSE) %>%
  save_kable(file.path(TABLE_DIR, "remuneration_table.tex"))

# =============================================================
# SUMMARY STATISTICS TABLE (summary_statistics.tex)
# =============================================================
datasummary(
  (`log employment` = lemp) +
  (`total assets (in thousands)` = total_assets) +
  (`baseline age` = baseline_age) +
  (`log remuneration` = lremun) +
  (`turnover (in thousands)` = turnover) +
  (`SIC 2-digit Codes` = sic_2digit) +
  (`SIC 1-digit Codes` = sic_1digit) ~
    Mean + SD + Min + Median + Max + N,
  data = data,
  output = file.path(TABLE_DIR, "summary_statistics.tex")
)

rm(list = ls()); gc()
