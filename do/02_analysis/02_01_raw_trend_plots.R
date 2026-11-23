library(tikzDevice)
library(tidyverse)
library(fixest)
library(ggfixest)
library(arrow)

# Load data
setwd("./")
DATA_DIR <- "./data/output/main.parquet"
df <- read_parquet(DATA_DIR)
gc()

# ============================================================
# OUTPUT SETUP
# ============================================================
FIG_DIR <- "./output/figures"
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

# A4 side-by-side subfigure dimensions:
# A4 text width ~ 160mm with 25mm margins each side
# Two subfigures with a small gap → each panel ~ 78mm ≈ 3.07in
# Aspect ratio ~ 0.8 → height ~ 2.5in
FIG_W <- 4   # inches (width per subfigure panel)
FIG_H <- 3   # inches (height per subfigure panel)

# ============================================================
# HELPER: save a ggplot as .tex via tikzDevice
# ============================================================
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
# HELPER: prepare summarised quartile data for a given score
# ============================================================
prepare_quartile_data <- function(data, score_var) {
  data %>%
    filter(!is.na(.data[[score_var]]), year < 2025, in_sample == 1) %>%
    mutate(exposure_quartile = ntile(.data[[score_var]], 4)) %>%
    mutate(Group = factor(exposure_quartile,
                          levels = c(1, 2, 3, 4),
                          labels = c("Q1 (Lowest Exposure)",
                                     "Q2 (Low-Mid Exposure)",
                                     "Q3 (Mid-High Exposure)",
                                     "Q4 (Highest Exposure)"))) %>%
    group_by(year, Group) %>%
    summarize(mean_lemp = mean(lemp, na.rm = TRUE), .groups = "drop")
}

# ============================================================
# HELPER: all-quartile trend plot
# ============================================================
make_all_plot <- function(data) {
  ggplot(data, aes(x = year, y = mean_lemp,
                   color = Group, shape = Group, linetype = Group)) +
    annotate("rect", xmin = 2016, xmax = 2021, ymin = -Inf, ymax = Inf,
             alpha = 0.2, fill = "gray60") +
    annotate("rect", xmin = 2021, xmax = Inf, ymin = -Inf, ymax = Inf,
             alpha = 0.2, fill = "gray40") +
    geom_vline(xintercept = 2016, linetype = "dashed", color = "black", linewidth = 0.5) +
    geom_vline(xintercept = 2021, linetype = "dashed", color = "black", linewidth = 0.5) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2, fill = "white", stroke = 0.8) +
    scale_color_manual(values = c("black", "gray30", "gray50", "gray70")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    labs(x = "Year", y = "Log Employment", color = NULL, shape = NULL, linetype = NULL) +
    theme_classic(base_size = 9) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    theme(panel.grid       = element_blank(),
          legend.position   = "bottom",
          legend.background = element_blank(),
          legend.key        = element_blank(),
          legend.key.size   = unit(0.35, "cm"),
          legend.text       = element_text(size = 6),
          legend.margin     = margin(t = -2))
}

# ============================================================
# HELPER: pairwise quartile plot
# ============================================================
make_pair_plot <- function(data, q_a, q_b) {
  filtered_data <- data %>%
    filter(as.numeric(Group) %in% c(q_a, q_b))

  ggplot(filtered_data, aes(x = year, y = mean_lemp,
                            color = Group, shape = Group, linetype = Group)) +
    annotate("rect", xmin = 2016, xmax = 2021, ymin = -Inf, ymax = Inf,
             alpha = 0.2, fill = "gray60") +
    annotate("rect", xmin = 2021, xmax = Inf, ymin = -Inf, ymax = Inf,
             alpha = 0.2, fill = "gray40") +
    geom_vline(xintercept = 2016, linetype = "dashed", color = "black", linewidth = 0.5) +
    geom_vline(xintercept = 2021, linetype = "dashed", color = "black", linewidth = 0.5) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2, fill = "white", stroke = 0.8) +
    scale_color_manual(values = c("black", "gray40")) +
    labs(x = "Year", y = "Log Employment", color = NULL, shape = NULL, linetype = NULL) +
    theme_classic(base_size = 9) +
    theme(panel.grid       = element_blank(),
          legend.position   = "bottom",
          legend.background = element_blank(),
          legend.key        = element_blank(),
          legend.key.size   = unit(0.35, "cm"),
          legend.text       = element_text(size = 7),
          legend.margin     = margin(t = -2))
}

# ============================================================
# GENERATE & SAVE PLOTS FOR BOTH EXPOSURE MEASURES
# ============================================================
score_configs <- list(
  list(var = "ai_2021",  prefix = "ai"),
  list(var = "llm_2021", prefix = "llm")
)

pair_specs <- list(
  list(qa = 1, qb = 2, tag = "q1_q2"),
  list(qa = 3, qb = 4, tag = "q3_q4"),
  list(qa = 1, qb = 4, tag = "q1_q4"),
  list(qa = 2, qb = 3, tag = "q2_q3"),
  list(qa = 2, qb = 4, tag = "q2_q4"),
  list(qa = 1, qb = 3, tag = "q1_q3")
)

for (cfg in score_configs) {
  cat(sprintf("\n=== %s exposure plots ===\n", toupper(cfg$prefix)))

  plot_data <- prepare_quartile_data(df, cfg$var)
  gc()

  # All-quartile plot
  p_all <- make_all_plot(plot_data)
  save_tikz(p_all, paste0("trend_", cfg$prefix, "_all"))

  # Pairwise plots
  for (sp in pair_specs) {
    p <- make_pair_plot(plot_data, sp$qa, sp$qb)
    save_tikz(p, paste0("trend_", cfg$prefix, "_", sp$tag))
  }
}
