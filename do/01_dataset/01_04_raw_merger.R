# ============================================================
# LIBRARIES
# ============================================================
library(tidyverse)
library(arrow)
library(lubridate)

# ============================================================
# HELPER: winsorise
# ============================================================
winsorise <- function(x, p_lo = 0, p_hi = 1) {
  pmax(pmin(x, quantile(x, p_hi, na.rm = TRUE)),
       quantile(x, p_lo, na.rm = TRUE))
}

# ============================================================
# FILE PATHS
# ============================================================
EMPL_DIR <- "data/temp/raw_financials.parquet"
ID_DIR   <- "data/temp/identifiers.parquet"
SOC_DIR  <- "data/temp/sic_exposure_summary.parquet"
OUT_DIR  <- "data/output/main.parquet"

FINANCIAL_VARS <- c("intangible_assets", "investment", "liquidity_ratio",
                    "long_term_debt", "profit_loss_before_taxation",
                    "remuneration", "roa", "total_assets", "turnover")

# ============================================================
# LOAD & MERGE
# ============================================================
employment  <- read_parquet(EMPL_DIR) %>% distinct(bvd_id, year, .keep_all = TRUE)
identifiers <- read_parquet(ID_DIR)   %>% distinct(bvd_id,       .keep_all = TRUE)
soc         <- read_parquet(SOC_DIR)

cat(sprintf("Loaded: %d obs, %d firms\n", nrow(employment), n_distinct(employment$bvd_id)))

combined <- employment %>%
  left_join(identifiers, by = "bvd_id") %>%
  mutate(
    sic_string = str_pad(as.character(sic), width = 2, pad = "0"),
    sic_2digit = str_sub(sic_string, 1, 2),
    sic_2digit = case_when(sic_2digit %in% c("41","42","43") ~ "41", TRUE ~ sic_2digit),
    sic_2digit = as.numeric(sic_2digit)
  ) %>%
  left_join(soc, by = "sic_2digit")

# ============================================================
# TYPE CONVERSIONS
# ============================================================
combined <- combined %>%
  #rename(comp_name = comp_name.x) %>%
  select(-comp_name.y) %>%
  mutate(
    bvd_id      = as.character(bvd_id),
    comp_name   = as.character(comp_name),
    year        = as.integer(as.character(year)),
    sic         = as.integer(sic),
    sic_2digit  = as.character(sic_2digit),
    status_date = as.Date(as.numeric(status_date), origin = "1899-12-30"),
    across(c(employment, all_of(FINANCIAL_VARS), ai_2021, llm_2021, ai_2011, llm_2011),
           ~ suppressWarnings(as.numeric(.)))
  ) %>%
  mutate(
    start_date = case_when(
      str_detect(start_date, "^\\d+$") ~ as.character(as.Date(as.numeric(start_date), origin = "1899-12-30")),
      TRUE ~ as.character(dmy(start_date))
    ),
    start_date = as.Date(start_date)
  )

# ============================================================
# DERIVED DATE COLUMNS
# ============================================================
combined <- combined %>%
  mutate(
    birth_year = year(start_date),
    death_year = if_else(status %in% c("Dissolved", "In liquidation"), year(status_date), 3000L)
  )

# ============================================================
# ATTRITION INDICATORS (consumed by 01_05_attrition_tests.R)
# ============================================================
combined <- combined %>%
  mutate(
    entry     = as.integer(birth_year == year),
    exit      = as.integer(death_year == year & death_year < 3000L),
    reporting = as.integer(!is.na(employment)),
    exists    = as.integer(year > birth_year & year < death_year)
  )

# ============================================================
# S2 — ACTIVITY WINDOW
# ============================================================
# Pre-entry: 987,365 obs | Post-exit: 85,770 obs (see 01_04_raw_merger_log.txt)
combined <- combined %>%
  mutate(
    s2_active = as.integer((is.na(birth_year) | year >= birth_year) & year <= death_year),
    lemp      = log(employment)
  )

cat(sprintf("S2 activity window    — flagged out: %d obs\n", sum(combined$s2_active == 0)))

# ============================================================
# S3 — EMPLOYMENT VALIDITY
# ============================================================
# S3a: zero/negative employment confirmed absent in this dataset
# S3b: extreme YoY growth (|delta%| > 200%)
combined <- combined %>%
  arrange(bvd_id, year) %>%
  group_by(bvd_id) %>%
  mutate(
    emp_growth = (employment - lag(employment)) / lag(employment),
    s3_valid   = as.integer(is.na(emp_growth) | abs(emp_growth) <= 2)
  ) %>%
  ungroup() %>%
  select(-emp_growth)

cat(sprintf("S3 employment validity — flagged out: %d obs\n", sum(combined$s3_valid == 0, na.rm = TRUE)))

# ============================================================
# S4 — WINSORISE FINANCIAL VARIABLES (within-year, p1/p99)
# ============================================================
combined <- combined %>%
  group_by(year) %>%
  mutate(across(all_of(intersect(FINANCIAL_VARS, names(combined))), winsorise)) %>%
  ungroup()

cat("S4 winsorising         — complete\n")

# ============================================================
# S5 — MINIMUM PANEL PRESENCE (>= 3 consecutive years)
# ============================================================
consec_flag <- combined %>%
  arrange(bvd_id, year) %>%
  group_by(bvd_id) %>%
  summarise(
    max_consec = {
      d <- diff(sort(unique(year)))
      r <- rle(d == 1)
      if (length(r$lengths) == 0) 1L else max(r$lengths[r$values] + 1L, 1L, na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  mutate(s5_consec = as.integer(max_consec >= 3)) %>%
  select(bvd_id, s5_consec)

combined <- combined %>% left_join(consec_flag, by = "bvd_id")

cat(sprintf("S5 min panel presence — flagged out: %d firms, %d obs\n",
            sum(consec_flag$s5_consec == 0),
            sum(combined$s5_consec == 0)))

# ============================================================
# S6 — SIC DIAGNOSTIC (no flag applied)
# ============================================================
# All candidate exclusion SICs (01-03, 64-66, 84, 97-99) have full treatment coverage.
# Shares: Agriculture 0.99% | Finance 2.55% | Public admin 0.14% | Extraterritorial 0.22%
# Retain all; drop as sensitivity robustness if needed (see Slide 31 battery).

# ============================================================
# S7 — BASELINE SIC ASSIGNMENT (2019 anchor, earliest-year fallback)
# ============================================================
# No SIC switchers in this dataset. Assignment is a formality to lock in time-invariant d_i.
baseline_sic <- combined %>%
  filter(!is.na(sic_2digit)) %>%
  arrange(bvd_id, year) %>%
  group_by(bvd_id) %>%
  summarise(
    sic_2digit_base = if (any(year == 2019, na.rm = TRUE))
      sic_2digit[which(year == 2019)[1]]
    else
      sic_2digit[which.min(year)],
    .groups = "drop"
  )

n_anchored <- combined %>% filter(year == 2019) %>% distinct(bvd_id) %>% nrow()
cat(sprintf("S7 baseline SIC       — 2019 anchor: %d firms | fallback: %d firms\n",
            n_anchored, nrow(baseline_sic) - n_anchored))

combined <- combined %>%
  select(-sic_2digit, -any_of(c("ai_2021","llm_2021","ai_2011","llm_2011"))) %>%
  left_join(
    baseline_sic %>% transmute(bvd_id, sic_2digit = as.numeric(sic_2digit_base)),
    by = "bvd_id"
  ) %>%
  left_join(soc, by = "sic_2digit")

# ============================================================
# S8 — TREATMENT MATCH (all missingness = NA sic_2digit; 11,286 firms)
# ============================================================
combined <- combined %>%
  group_by(bvd_id) %>%
  mutate(
    s8_treated = as.integer(
      all(!is.na(ai_2021)) & all(!is.na(llm_2021)) &
        all(!is.na(ai_2011)) & all(!is.na(llm_2011))
    )
  ) %>%
  ungroup()

cat(sprintf("S8 treatment match    — flagged out: %d firms, %d obs\n",
            n_distinct(combined$bvd_id[combined$s8_treated == 0]),
            sum(combined$s8_treated == 0)))

# ============================================================
# IN_SAMPLE COMPOSITE FLAG
# ============================================================
combined <- combined %>%
  mutate(in_sample = as.integer(s2_active & s3_valid  & s8_treated)) %>%
  select(-matches("comp_name\\.[xy]")) %>%
  distinct()

cat(sprintf("\nFull dataset : %d obs, %d firms\n", nrow(combined), n_distinct(combined$bvd_id)))
cat(sprintf("in_sample = 1: %d obs, %d firms\n",
            sum(combined$in_sample), n_distinct(combined$bvd_id[combined$in_sample == 1])))

stopifnot(sum(duplicated(combined %>% select(bvd_id, year))) == 0)
stopifnot(sum(is.infinite(combined$lemp), na.rm = TRUE) == 0)

# ============================================================
# SAVE OUTPUT
# ============================================================
write_parquet(combined, OUT_DIR)

rm(list = ls()); gc()
