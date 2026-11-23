# author: u2235356
# This script contains the merger of the 19 Orbis Excel Sheets into one singular 
# dataframe for further analysis. 

# The following is performed:
#  1. Takes all the 7 .xlsx files and merges them. 
#  2. Parses dates robustly handling a mix of Excel serials, bare years, and strings.
#  3. Identifies exact dissolution dates using legal_event_date cross-referencing.
#  4. Renames variables and assigns each variable to be of the correct type.
#  6. Generates age of a company, industry code.
#  7. Drops observations based on the following reasons:
#    i) Observations with age < 0 are removed because they will contain NAs
#       not because a company has missing data but rather it did not exist.
#    ii) Observations with missing SIC codes are dropped as they are highly 
#        indicative of shell firms.
#  8. Saves it to the temp folder as a parquet file using nanoparquet.

# Load Libraries
library(tidyverse)
library(readxl)
library(arrow)

# Set the directory paths
id_dir    <- "./data/input/identifiers"
save_dir  <- "./data/temp/identifiers.parquet"

# -------------------------------------------------------------------------
# Step 1: Read and Merge All Files
# -------------------------------------------------------------------------

# Get all Excel files in the directory
id_list   <- list.files(path = id_dir, pattern = "^Identifiers - Sheet.*\\.xlsx$", full.names = TRUE) %>% sort()

# Read "Results" sheet from all files 
identifier_data <- id_list %>%
  lapply(function(file){ read_excel(file, sheet = "Results", col_types = "text") }) %>%
  bind_rows()

# -------------------------------------------------------------------------
# Step 3: Clean and Process Identifier Data
# -------------------------------------------------------------------------

identifier_clean <- identifier_data %>%
  select(-`...1`) %>% # dropping index columns
  mutate(across(where(is.character), ~na_if(., "n.a."))) %>% # replacing "n.a." with NA
  rename(
    comp_name        = `Company name`,
    sic              = `Primary UK SIC (2007) code`,
    bvd_id           = `BvD ID number`,  
    start_date       = `Date of incorporation`, 
    status_date      = `Status date`, 
    status           = `Company status`, 
    legal_form       = `Legal form`,
    sic_des          = `Primary UK SIC (2007) description`
  )

# saving identifier data
write_parquet(identifier_clean, save_dir)

rm(list = ls())
gc()
