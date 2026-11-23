# ============================================================
# EC910 Group 9 Project — Master Runner Script
# ============================================================

# Ensure required directory structure exists
dirs <- c(
  "./data/input/financials",
  "./data/input/identifiers",
  "./data/input/treatment_data",
  "./data/input/rti_102_countries",
  "./data/temp",
  "./data/output",
  "./output/figures",
  "./output/tables"
)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Ordered pipeline scripts
scripts <- c(
  "./do/01_dataset/01_01_merge_financials.R",
  "./do/01_dataset/01_02_merge_identifiers.R",
  "./do/01_dataset/01_03_sic_treatment_map_manually_made.R",
  "./do/01_dataset/01_04_raw_merger.R",
  "./do/01_dataset/01_05_attrition_tests.R",
  "./do/02_analysis/02_01_raw_trend_plots.R",
  "./do/02_analysis/02_03_main_analysis.R",
  "./do/03_robustness/03_01_robustness_regressions.R",
  "./do/03_robustness/03_02_mechanisms_and_heterogeneity.R"
)

log_file <- "ec910_group9_project_log.txt"
sink(log_file, split = TRUE)

on.exit({
  while (sink.number() > 0) sink()
}, add = TRUE)

for (script in scripts) {
  cat(sprintf("### %s ###\n\n", script))
  tryCatch(
    source(script, echo = TRUE, max.deparse.length = Inf),
    error = function(e) {
      cat("ERROR: \n", conditionMessage(e), "\n \n")
    }
  )
}

while (sink.number() > 0) sink()
