# ============================================================
# LIBRARIES
# ============================================================
library(tidyverse)
library(kableExtra)
library(fixest)
library(panelsummary)
library(arrow)

# ============================================================
# FILE PATHS
# ============================================================
DATA_DIR <- "./data/temp/final_before_attrition_test.parquet"
OUT_DIR  <- "./output/tables/attrition_tests.tex"

# ============================================================
# LOAD DATA
# ============================================================
data <- read_parquet(DATA_DIR)

# entry, exit, reporting, exists, post all pre-generated in 01_04_raw_merger.R
data <- data %>% 
  mutate(post = as.integer(year > 2021), 
         sic_1digit = floor(sic_2digit/10))

# ============================================================
# ENTRY & EXIT MODELS
# ============================================================
attrition_models <- feols(
  c(entry, exit) ~ sw(ai_2021:post, llm_2021:post) | bvd_id + year + sic_1digit[year],
  data      = data,
  subset    = ~year > 2015 & year < 2025,
  cluster   = ~sic_2digit,
  nthreads  = 10
)

iv_entry_ai  <- feols(entry ~ 1 | bvd_id + year + sic_1digit[year] | ai_2021:post  ~ ai_2011:post,
                      data = data, cluster = ~sic_2digit, subset = ~year > 2015 & year < 2025, nthreads = 10)
iv_exit_ai   <- feols(exit  ~ 1 | bvd_id + year + sic_1digit[year] | ai_2021:post  ~ ai_2011:post,
                      data = data, cluster = ~sic_2digit, subset = ~year > 2015 & year < 2025, nthreads = 10)
iv_entry_llm <- feols(entry ~ 1 | bvd_id + year + sic_1digit[year] | llm_2021:post ~ llm_2011:post,
                      data = data, cluster = ~sic_2digit, subset = ~year > 2015 & year < 2025, nthreads = 10)
iv_exit_llm  <- feols(exit  ~ 1 | bvd_id + year + sic_1digit[year] | llm_2021:post ~ llm_2011:post,
                      data = data, cluster = ~sic_2digit, subset = ~year > 2015 & year < 2025, nthreads = 10)

# ============================================================
# REPORTING MODELS
# ============================================================
reporting_models <- feols(
  reporting ~ sw(ai_2021:post, llm_2021:post) | bvd_id + year + sic_1digit[year],
  data     = data,
  subset   = ~year > 2015 & year < 2025,
  cluster  = ~sic_2digit,
  nthreads = 10
)

iv_reporting_ai  <- feols(reporting ~ 1 | bvd_id + year  + sic_1digit[year] | ai_2021:post  ~ ai_2011:post,
                          data = data, cluster = ~sic_2digit, subset = ~year > 2015 & year < 2025, nthreads = 10)
iv_reporting_llm <- feols(reporting ~ 1 | bvd_id + year + sic_1digit[year] | llm_2021:post ~ llm_2011:post,
                          data = data, cluster = ~sic_2digit, subset = ~year > 2015 & year < 2025, nthreads = 10)

# ============================================================
# TABLE: ENTRY, EXIT, AND REPORTING
# ============================================================
panelsummary(
  list(attrition_models[[1]], iv_entry_ai,  attrition_models[[3]], iv_exit_ai,  reporting_models[[1]], iv_reporting_ai),
  list(attrition_models[[2]], iv_entry_llm, attrition_models[[4]], iv_exit_llm, reporting_models[[2]], iv_reporting_llm),
  panel_labels = c("Panel A: AI Exposure", "Panel B: LLM Exposure"),
  italic = TRUE, hline_after = FALSE, pretty_num = TRUE, collapse_fe = TRUE,
  fmt = 3, stars = c("*" = .1, "**" = .05, "***" = .01),
  hline_before_fe = TRUE, gof_omit = "AIC|BIC|R2|R2 Adj.|RMSE|Std.Errors",
  # format = "latex",
  coef_map = c("ai_2021:post"       = "Exposure × Post",
               "fit_ai_2021:post"   = "Exposure × Post",
               "llm_2021:post"      = "Exposure × Post",
               "fit_llm_2021:post"  = "Exposure × Post"),
  caption  = "Entry, Exit and Reporting explained by Exposure",
  colnames = c("", "OLS", "IV", "OLS", "IV", "OLS", "IV")
) %>%
  add_header_above(c(` ` = 1, `Entry` = 2, `Exit` = 2, `Reporting` = 2)) %>%
  kable_styling(latex_options = c("booktabs", "hold_position"),
                position = "center", full_width = FALSE, font_size = 10)
 #%>%  save_kable(file = OUT_DIR)

# ============================================================
# CLEANUP
# ============================================================
rm(list = ls()); gc()
