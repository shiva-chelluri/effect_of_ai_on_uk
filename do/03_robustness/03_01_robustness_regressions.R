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
baseline <- data %>%
  distinct(bvd_id, .keep_all = TRUE) %>%
  select(bvd_id, ai_2021, llm_2021, baseline_ta, baseline_age, sic_1digit) %>%
  filter(complete.cases(.))

wb_ai <- weightit(ai_2021 ~ baseline_ta + baseline_age,
  data = baseline, method = "ebal", estimand = "ATE"
)

wb_llm <- weightit(llm_2021 ~ baseline_ta + baseline_age,
  data = baseline, method = "ebal", estimand = "ATE"
)

baseline <- baseline %>%
  mutate(
    w_ai  = wb_ai$weights,
    w_llm = wb_llm$weights
  )

data <- data %>%
  left_join(baseline %>% select(bvd_id, w_ai, w_llm), by = "bvd_id")

rm(baseline_ta, baseline, wb_ai, wb_llm)

# ============================================================
# OUTPUT SETUP
# ============================================================
FIG_DIR <- "./output/figures"
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
# SHARED HELPERS
# ============================================================

# Compact gof_map used across robustness tables (no IV diagnostics needed for
# most checks; IV-specific tables add their own rows).
gm_compact <- tribble(
  ~raw,                   ~clean,           ~fmt,
  "nobs",                 "Observations",   0,
  "FE: bvd_id",           "Firm FE",        NA_real_,
  "FE: year",             "Year FE",        NA_real_,
  "FE: sic_1digit[year]", "Sector-Year FE", NA_real_
)

gm_iv <- tribble(
  ~raw,                   ~clean,                   ~fmt,
  "nobs",                 "Observations",           0,
  "FE: bvd_id",           "Firm FE",                NA_real_,
  "FE: year",             "Year FE",                NA_real_,
  "FE: sic_1digit[year]", "Sector-Year FE",         NA_real_,
  "kp_fstat",             "Kleibergen-Paap F-stat", 2,
  "wu_hausman",           "Wu-Hausman p-value",     3
)

# glance hook so panelsummary can pick up IV diagnostics
glance_custom.fixest <- function(x, ...) {
  out <- data.frame(row.names = "")
  if (!is.null(x$iv)) {
    out$kp_fstat <- fitstat(x, ~ivwald1)$ivwald1$stat
    out$wu_hausman <- fitstat(x, ~wh)$wh$p
  }
  out
}

cluster_note <- "\\textit{Note:} Standard errors are clustered at the 2-digit SIC level. "
star_note <- "\\textit{Note:} *** p<0.01, ** p<0.05, * p<0.1"

# ============================================================
# R2 — STACKED PLACEBO EVENT STUDY (2017–2021 as "event")
# ============================================================
# Run the dynamic event-study spec treating each year 2017–2021 as the
# reform year (i.e., the reference year).  All coefficients should be null
# if AI exposure has no pre-existing relationship with employment trends.

placebo_years <- 2017:2020

make_event_study_plot_r2 <- function(model, ref_year) {
  ggiplot(model) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_vline(
      xintercept = ref_year, linetype = "dashed",
      color = "gray50", linewidth = 0.5
    ) +
    labs(
      title   = sprintf("Placebo event year: %d", ref_year),
      x       = "Year",
      y       = "Estimate",
      caption = paste(star_note, cluster_note)
    ) +
    theme_classic(base_size = 9) +
    theme(
      panel.grid = element_blank(),
      plot.caption = element_text(
        hjust = 0, size = 6,
        face = "italic", color = "gray30"
      ),
      plot.caption.position = "plot"
    )
}

for (ref_yr in placebo_years) {
  # OLS AI
  m <- feols(
    as.formula(sprintf(
      "lemp ~ i(year, ai_2021, ref = %d) | bvd_id + year + sic_1digit[year]",
      ref_yr
    )),
    cluster = ~sic_2digit, nthreads = 10,
    weights = ~w_ai, data = data
  )
  p <- make_event_study_plot_r2(m, ref_yr)
  save_tikz(p, sprintf("r2_placebo_ai_%d", ref_yr))
  rm(m, p)

  # OLS LLM
  m <- feols(
    as.formula(sprintf(
      "lemp ~ i(year, llm_2021, ref = %d) | bvd_id + year + sic_1digit[year]",
      ref_yr
    )),
    cluster = ~sic_2digit, nthreads = 10,
    weights = ~w_llm, data = data
  )
  p <- make_event_study_plot_r2(m, ref_yr)
  save_tikz(p, sprintf("r2_placebo_llm_%d", ref_yr))
  rm(m, p)
  gc()
}

# ============================================================
# R1 & R2 TABLES — PLACEBO TIMING & STACKED PLACEBO (STATIC DiD)
# ============================================================
data <- data %>%
  mutate(post_2020 = ifelse(year > 2020, 1, 0))

r_ai_2017  <- feols(lemp ~ i(post_2017, ai_2021, ref = 0)  | bvd_id + year + sic_1digit[year], cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai,  data = data)
r_ai_2018  <- feols(lemp ~ i(post_2018, ai_2021, ref = 0)  | bvd_id + year + sic_1digit[year], cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai,  data = data)
r_ai_2019  <- feols(lemp ~ i(post_2019, ai_2021, ref = 0)  | bvd_id + year + sic_1digit[year], cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai,  data = data)
r_ai_2020  <- feols(lemp ~ i(post_2020, ai_2021, ref = 0)  | bvd_id + year + sic_1digit[year], cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai,  data = data)
r_ai_2021  <- feols(lemp ~ i(post,      ai_2021, ref = 0)  | bvd_id + year + sic_1digit[year], cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai,  data = data)

r_llm_2017 <- feols(lemp ~ i(post_2017, llm_2021, ref = 0) | bvd_id + year + sic_1digit[year], cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = data)
r_llm_2018 <- feols(lemp ~ i(post_2018, llm_2021, ref = 0) | bvd_id + year + sic_1digit[year], cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = data)
r_llm_2019 <- feols(lemp ~ i(post_2019, llm_2021, ref = 0) | bvd_id + year + sic_1digit[year], cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = data)
r_llm_2020 <- feols(lemp ~ i(post_2020, llm_2021, ref = 0) | bvd_id + year + sic_1digit[year], cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = data)
r_llm_2021 <- feols(lemp ~ i(post,      llm_2021, ref = 0) | bvd_id + year + sic_1digit[year], cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = data)

placebo_cmap <- c(
  "post_2017::1:ai_2021"  = "Exposure × Post",
  "post_2018::1:ai_2021"  = "Exposure × Post",
  "post_2019::1:ai_2021"  = "Exposure × Post",
  "post_2020::1:ai_2021"  = "Exposure × Post",
  "post::1:ai_2021"       = "Exposure × Post",
  "post_2017::1:llm_2021" = "Exposure × Post",
  "post_2018::1:llm_2021" = "Exposure × Post",
  "post_2019::1:llm_2021" = "Exposure × Post",
  "post_2020::1:llm_2021" = "Exposure × Post",
  "post::1:llm_2021"      = "Exposure × Post"
)

# R1: Placebo Treatment Timing (2017–2019)
panelsummary(
  list(r_ai_2017, r_ai_2018, r_ai_2019),
  list(r_llm_2017, r_llm_2018, r_llm_2019),
  panel_labels = c("Panel A: AI Exposure", "Panel B: LLM Exposure"),
  italic = TRUE, format = "latex", hline_after = TRUE, hline_before_fe = TRUE,
  pretty_num = TRUE, collapse_fe = TRUE, fmt = 3,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gm_compact, coef_map = placebo_cmap,
  caption = "Placebo Treatment Timing (OLS)",
  colnames = c("", "Post > 2017", "Post > 2018", "Post > 2019")
) %>%
  kable_styling(latex_options = c("booktabs", "hold_position"), position = "center", full_width = FALSE) %>%
  save_kable(file.path(TABLE_DIR, "r1_placebo_timing.tex"))

# R2: Stacked Placebo Years (2017–2021)
panelsummary(
  list(r_ai_2017, r_ai_2018, r_ai_2019, r_ai_2020, r_ai_2021),
  list(r_llm_2017, r_llm_2018, r_llm_2019, r_llm_2020, r_llm_2021),
  panel_labels = c("Panel A: AI Exposure", "Panel B: LLM Exposure"),
  italic = TRUE, format = "latex", hline_after = TRUE, hline_before_fe = TRUE,
  pretty_num = TRUE, collapse_fe = TRUE, fmt = 3,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gm_compact, coef_map = placebo_cmap,
  caption = "Stacked Placebo Years (OLS, Static DiD)",
  colnames = c("Post =", "2017", "2018", "2019", "2020", "2021")
) %>%
  kable_styling(latex_options = c("booktabs", "hold_position"), position = "center", full_width = FALSE) %>%
  save_kable(file.path(TABLE_DIR, "r2_stacked_placebo.tex"))

rm(r_ai_2017, r_ai_2018, r_ai_2019, r_ai_2020, r_ai_2021,
   r_llm_2017, r_llm_2018, r_llm_2019, r_llm_2020, r_llm_2021); gc()

# ============================================================
# R4 — IV MODELS WITH AND WITHOUT INDUSTRY TRENDS (r4_fstat_trends.tex)
# ============================================================
iv.ai.notrend  <- feols(lemp ~ 1 | bvd_id + year | i(post, ai_2021, ref = 0) ~ i(post, ai_2011, ref = 0),
                        cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai, data = data)
iv.ai.trend    <- feols(lemp ~ 1 | bvd_id + year + sic_1digit[year] | i(post, ai_2021, ref = 0) ~ i(post, ai_2011, ref = 0),
                        cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai, data = data)
iv.llm.notrend <- feols(lemp ~ 1 | bvd_id + year | i(post, llm_2021, ref = 0) ~ i(post, llm_2011, ref = 0),
                        cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = data)
iv.llm.trend   <- feols(lemp ~ 1 | bvd_id + year + sic_1digit[year] | i(post, llm_2021, ref = 0) ~ i(post, llm_2011, ref = 0),
                        cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = data)

panelsummary(
  list(iv.ai.notrend, iv.ai.trend),
  list(iv.llm.notrend, iv.llm.trend),
  panel_labels = c("Panel A: AI Exposure", "Panel B: LLM Exposure"),
  italic = TRUE, format = "latex", hline_after = TRUE, hline_before_fe = TRUE,
  pretty_num = TRUE, collapse_fe = TRUE, fmt = 3,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gm_iv,
  coef_map = c(
    "fit_post::1:ai_2021"  = "Exposure × Post (IV)",
    "fit_post::1:llm_2021" = "Exposure × Post (IV)"
  ),
  caption = "IV Models With and Without Trends",
  colnames = c("", "No Trend", "With Trend")
) %>%
  kable_styling(latex_options = c("booktabs", "hold_position"), position = "center", full_width = FALSE) %>%
  save_kable(file.path(TABLE_DIR, "r4_fstat_trends.tex"))

rm(iv.ai.notrend, iv.ai.trend, iv.llm.notrend, iv.llm.trend); gc()

# ============================================================
# R5 — BALANCED PANEL ROBUSTNESS (r5_balanced_panel.tex)
# ============================================================
balanced_ids <- data %>%
  filter(year >= 2016, year <= 2024) %>%
  group_by(bvd_id) %>%
  summarise(n_yr = n_distinct(year), .groups = "drop") %>%
  filter(n_yr == 9) %>%
  pull(bvd_id)

data_bal <- data %>% filter(bvd_id %in% balanced_ids, year <= 2024)

bal.ols.ai  <- feols(lemp ~ i(post, ai_2021, ref = 0) | bvd_id + year + sic_1digit[year],
                     cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai, data = data_bal)
bal.iv.ai   <- feols(lemp ~ 1 | bvd_id + year + sic_1digit[year] | i(post, ai_2021, ref = 0) ~ i(post, ai_2011, ref = 0),
                     cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai, data = data_bal)
bal.ols.llm <- feols(lemp ~ i(post, llm_2021, ref = 0) | bvd_id + year + sic_1digit[year],
                     cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = data_bal)
bal.iv.llm  <- feols(lemp ~ 1 | bvd_id + year + sic_1digit[year] | i(post, llm_2021, ref = 0) ~ i(post, llm_2011, ref = 0),
                     cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = data_bal)

panelsummary(
  list(bal.ols.ai, bal.iv.ai),
  list(bal.ols.llm, bal.iv.llm),
  panel_labels = c("Panel A: AI Exposure", "Panel B: LLM Exposure"),
  italic = TRUE, format = "latex", hline_after = TRUE, hline_before_fe = TRUE,
  pretty_num = TRUE, collapse_fe = TRUE, fmt = 3,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gm_iv,
  coef_map = c(
    "post::1:ai_2021"      = "Exposure × Post",
    "fit_post::1:ai_2021"  = "Exposure × Post",
    "post::1:llm_2021"     = "Exposure × Post",
    "fit_post::1:llm_2021" = "Exposure × Post"
  ),
  caption = "Balanced Panel Robustness",
  colnames = c("", "OLS", "IV")
) %>%
  kable_styling(latex_options = c("booktabs", "hold_position"), position = "center", full_width = FALSE) %>%
  save_kable(file.path(TABLE_DIR, "r5_balanced_panel.tex"))

rm(balanced_ids, data_bal, bal.ols.ai, bal.iv.ai, bal.ols.llm, bal.iv.llm); gc()

# ============================================================
# R7 — ALTERNATIVE CLUSTERING AT 1-DIGIT SIC (r7_alt_clustering.tex)
# ============================================================
cl1.ols.ai  <- feols(lemp ~ i(post, ai_2021, ref = 0) | bvd_id + year + sic_1digit[year],
                     cluster = ~sic_1digit, nthreads = 10, weights = ~w_ai, data = data)
cl1.iv.ai   <- feols(lemp ~ 1 | bvd_id + year + sic_1digit[year] | i(post, ai_2021, ref = 0) ~ i(post, ai_2011, ref = 0),
                     cluster = ~sic_1digit, nthreads = 10, weights = ~w_ai, data = data)
cl1.ols.llm <- feols(lemp ~ i(post, llm_2021, ref = 0) | bvd_id + year + sic_1digit[year],
                     cluster = ~sic_1digit, nthreads = 10, weights = ~w_llm, data = data)
cl1.iv.llm  <- feols(lemp ~ 1 | bvd_id + year + sic_1digit[year] | i(post, llm_2021, ref = 0) ~ i(post, llm_2011, ref = 0),
                     cluster = ~sic_1digit, nthreads = 10, weights = ~w_llm, data = data)

panelsummary(
  list(cl1.ols.ai, cl1.iv.ai),
  list(cl1.ols.llm, cl1.iv.llm),
  panel_labels = c("Panel A: AI Exposure", "Panel B: LLM Exposure"),
  italic = TRUE, format = "latex", hline_after = TRUE, hline_before_fe = TRUE,
  pretty_num = TRUE, collapse_fe = TRUE, fmt = 3,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gm_iv,
  coef_map = c(
    "post::1:ai_2021"      = "Exposure × Post",
    "fit_post::1:ai_2021"  = "Exposure × Post",
    "post::1:llm_2021"     = "Exposure × Post",
    "fit_post::1:llm_2021" = "Exposure × Post"
  ),
  caption = "Alternative Clustering at 1-Digit SIC",
  colnames = c("", "OLS", "IV")
) %>%
  kable_styling(latex_options = c("booktabs", "hold_position"), position = "center", full_width = FALSE) %>%
  save_kable(file.path(TABLE_DIR, "r7_alt_clustering.tex"))

rm(cl1.ols.ai, cl1.iv.ai, cl1.ols.llm, cl1.iv.llm); gc()
rm(list = ls()); gc()
