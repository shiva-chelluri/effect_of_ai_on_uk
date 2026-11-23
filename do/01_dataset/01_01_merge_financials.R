# ============================================================
# LIBRARIES
# ============================================================
library(tidyverse)
library(readxl)
library(arrow)

# ============================================================
# FILE PATHS
# ============================================================
FIN_DIR <- "./data/input/financials"
OUT_DIR <- "./data/temp/raw_financials.parquet"

# ============================================================
# HELPER: process each financial subfolder
# ============================================================
process_financial_folder <- function(folder_path) {
  # Get folder name to use as variable prefix
  var_name <- basename(folder_path)

  # List all Excel files in subfolder
  files <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

  # Read and bind all files in the folder
  df_raw <- files %>%
    lapply(read_excel, sheet = "Results", col_types = "text") %>%
    bind_rows()

  # Basic cleaning and pivoting
  df_long <- df_raw %>%
    rename(
      comp_name = `Company name`,
      bvd_id    = `BvD ID number`
    ) %>%
    # Pivot all columns that end with a 4-digit year
    pivot_longer(
      cols = matches("\\d{4}$"),
      names_to = "year",
      values_to = var_name
    ) %>%
    # Extract just the year digits from the column name
    mutate(
      year = str_extract(year, "\\d{4}$")
    ) %>%
    # Keep only necessary columns and drop duplicates
    select(bvd_id, comp_name, year, !!sym(var_name)) %>%
    distinct(bvd_id, year, .keep_all = TRUE)

  return(df_long)
}

# ============================================================
# PROCESS ALL FOLDERS
# ============================================================

# Get list of subdirectories
subfolders <- list.dirs(FIN_DIR, recursive = FALSE)

# Assign folder basenames as names to the list so we can identify them later
names(subfolders) <- basename(subfolders)

# Process all folders into a named list of dataframes
all_dfs <- lapply(subfolders, process_financial_folder)

# Identify the exact name of the employment dataset (case-insensitive search)
emp_key <- "employment"

# Separate the employment dataset from the rest
df_base <- all_dfs[[emp_key]]
dfs_others <- all_dfs[names(all_dfs) != emp_key]

# Left-join all other datasets to the base employment dataset
final_data <- reduce(
  dfs_others,
  left_join,
  by = c("bvd_id", "year"),
  .init = df_base
)

gc()

# ============================================================
# SAVE OUTPUT
# ============================================================
write_parquet(final_data, OUT_DIR)

rm(list = ls())
gc()
