# ============================================================
# LIBRARIES
# ============================================================
library(tidyverse)
library(readxl)
library(arrow)
library(haven)
library(tikzDevice)

# ============================================================
# FILE PATHS
# ============================================================
CENSUS_2021_FILE <- "data/input/treatment_data/census_2021.csv"
CENSUS_2011_FILE <- "data/input/treatment_data/census_2011.csv"
ISCO_SOC_MAP <- "data/input/treatment_data/isco2d_to_soc2010_2d_crosswalk.csv"
RTI_2021_FILE <- "data/input/rti_102_countries/rti_country_specific_survey_predicted.dta"
MAPPING_FILE <- "data/input/treatment_data/soc_2020_to_2010_mapping_lfs.csv"
AIOE_FILE <- "data/input/treatment_data/aioe.xlsx"
OUT_DIR <- "data/temp/sic_exposure_summary.parquet"

# Occupations to exclude: elected officers, armed forces officers,
# waste disposal managers, and non-commissioned officers
EXCLUDED_SOC2010 <- c(1116, 1171, 1255, 3311)

# ============================================================
# AIOE EXPOSURE DATA (shared across both census years)
# ============================================================
aioe <- read_excel(AIOE_FILE, sheet = "AIOE by education level", skip = 2) %>%
  select(soc_2010 = SOC, aioe_raw = `All AI`, llm_raw = LLM)

# ============================================================
# SOC 2020 → SOC 2010 CROSSWALK
# ============================================================
soc_map <- read_csv(MAPPING_FILE) %>%
  mutate(
    soc_2020 = as.numeric(soc_2020),
    soc_2010 = as.numeric(soc_2010)
  )

# ============================================================
# CENSUS 2021: WEIGHT MATRIX (412 SOC-2020 × 87 SIC-2digit)
# ============================================================

# Read, drop trailing summary row, normalise each SIC column to sum to 1
census_2021_weight_matrix <- read_csv(CENSUS_2021_FILE, col_types = cols(.default = col_number())) %>%
  slice(-n()) %>%
  mutate(across(2:88, function(x) x / sum(x))) %>%
  rename(soc_2020 = ...1)

# Crosswalk from SOC 2020 → SOC 2010 via weighted aggregation (412 × 88 → 369 × 88)
census_2021_weight_matrix_soc2010 <- census_2021_weight_matrix %>%
  inner_join(soc_map, by = "soc_2020") %>%
  select(-soc_2020) %>%
  group_by(soc_2010) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  mutate(across(matches("^\\d+$"), ~ . / sum(.))) # ← re-normalise post-aggregation

# Attach AIOE scores and drop excluded occupations (365 SOC-2010 × 89)
combined_weight_matrix_2021 <- census_2021_weight_matrix_soc2010 %>%
  left_join(aioe, by = "soc_2010", keep = FALSE) %>%
  filter(!soc_2010 %in% EXCLUDED_SOC2010)

# ============================================================
# CENSUS 2021: AI EXPOSURE SCORES (inner product → n_sic × 3)
# ============================================================

sic_cols <- combined_weight_matrix_2021 %>%
  select(matches("^\\d+$")) %>%
  colnames()

sic_mat <- combined_weight_matrix_2021 %>%
  select(all_of(sic_cols)) %>%
  as.matrix()

aioe_vec <- combined_weight_matrix_2021$aioe_raw
llm_vec <- combined_weight_matrix_2021$llm_raw

result_2021 <- tibble(
  sic_2digit = as.integer(sic_cols),
  ai_2021    = as.vector(t(sic_mat) %*% aioe_vec),
  llm_2021   = as.vector(t(sic_mat) %*% llm_vec)
)

# ============================================================
# CENSUS 2011: WEIGHT MATRIX (389 SOC-2010 × 87 SIC-2digit)
# ============================================================

# Read, collapse duplicate SIC columns (same name, different sub-codes) by rowsum
census_2011_weight_matrix <- read_csv(CENSUS_2011_FILE, col_types = cols(.default = col_number())) %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(cols = -row_id, names_to = "col_name", values_to = "value") %>%
  mutate(col_name = str_replace(col_name, "\\.\\..*$", "")) %>%
  group_by(row_id, col_name) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  pivot_wider(names_from = col_name, values_from = value) %>%
  select(-row_id) %>%
  mutate(across(-soc_2010, function(x) x / sum(x)))

# Attach AIOE scores and drop excluded occupations
combined_weight_matrix_2011 <- census_2011_weight_matrix %>%
  left_join(aioe, by = "soc_2010", keep = FALSE) %>%
  filter(!soc_2010 %in% EXCLUDED_SOC2010)

# ============================================================
# CENSUS 2011: AI EXPOSURE SCORES (inner product → n_sic × 3)
# ============================================================

sic_cols <- combined_weight_matrix_2011 %>%
  select(matches("^\\d+$")) %>%
  colnames()

sic_mat <- combined_weight_matrix_2011 %>%
  select(all_of(sic_cols)) %>%
  as.matrix()

aioe_vec <- combined_weight_matrix_2011$aioe_raw
llm_vec <- combined_weight_matrix_2011$llm_raw

result_2011 <- tibble(
  sic_2digit = as.integer(sic_cols),
  ai_2011    = as.vector(t(sic_mat) %*% aioe_vec),
  llm_2011   = as.vector(t(sic_mat) %*% llm_vec)
)

# ============================================================
# COMBINE AND EXPORT
# ============================================================

combined_result <- result_2021 %>%
  left_join(result_2011, by = "sic_2digit", keep = FALSE)

write_parquet(x = combined_result, OUT_DIR)

# ===================================================
# ROTEMBERG WEIGHTS
# ====================================================
library(dplyr)
library(tibble)
library(knitr)

# 1. Setup the shared Z Matrix (Exogenous Shares)
sic_cols <- as.character(result_2021$sic_2digit)
Z <- as.matrix(combined_weight_matrix_2011[, sic_cols]) # 365 x 87

# ==========================================
# 2. AI Weights Calculation
# ==========================================
X_ai <- combined_weight_matrix_2011$aioe_raw
g_ai <- result_2021$ai_2021

Z_cross_X_ai <- t(Z) %*% X_ai
num_ai <- g_ai * Z_cross_X_ai
alpha_ai <- num_ai / sum(num_ai)

df_ai <- tibble(
  SIC = result_2021$sic_2digit,
  Weight = as.numeric(alpha_ai),
  Shift = g_ai,
  Z_cross_X = as.numeric(Z_cross_X_ai)
) %>% arrange(desc(Weight))

# Extract Top 5 and Bottom 5 for AI
table_ai <- bind_rows(
  head(df_ai, 5) %>% mutate(Rank = "Top 5"),
  tail(df_ai, 5) %>% mutate(Rank = "Bottom 5")
) %>% select(Rank, SIC, Weight, Shift, Z_cross_X)

# ==========================================
# 3. LLM Weights Calculation
# ==========================================
X_llm <- combined_weight_matrix_2011$llm_raw
g_llm <- result_2021$llm_2021

Z_cross_X_llm <- t(Z) %*% X_llm
num_llm <- g_llm * Z_cross_X_llm
alpha_llm <- num_llm / sum(num_llm)

df_llm <- tibble(
  SIC = result_2021$sic_2digit,
  Weight = as.numeric(alpha_llm),
  Shift = g_llm,
  Z_cross_X = as.numeric(Z_cross_X_llm)
) %>% arrange(desc(Weight))

# Extract Top 5 and Bottom 5 for LLM
table_llm <- bind_rows(
  head(df_llm, 5) %>% mutate(Rank = "Top 5"),
  tail(df_llm, 5) %>% mutate(Rank = "Bottom 5")
) %>% select(Rank, SIC, Weight, Shift, Z_cross_X)

# ==========================================
# 4. Print LaTeX Tables to Console
# ==========================================

cat("\n%%% AI ROTEMBERG WEIGHTS %%%\n")
cat(kable(table_ai, format = "latex", booktabs = TRUE, digits = 4,
          caption = "Top and Bottom 5 SIC Codes by AI Rotemberg Weight",
          col.names = c("Group", "SIC Code", "Rotemberg Weight", "AI Shift", "Z'X (Covariance)"),
          row.names = FALSE))

cat("\n\n%%% LLM ROTEMBERG WEIGHTS %%%\n")
cat(kable(table_llm, format = "latex", booktabs = TRUE, digits = 4,
          caption = "Top and Bottom 5 SIC Codes by LLM Rotemberg Weight",
          col.names = c("Group", "SIC Code", "Rotemberg Weight", "LLM Shift", "Z'X (Covariance)"),
          row.names = FALSE))

rm(list = ls())
gc()
