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
library(sf)

# ============================================================
# DATA LOADING & BASELINE CLEANING
# ============================================================
DATA_DIR <- "./data/output/main.parquet"
GEO_DIR <- "./data/input/codepo_gb.gpkg"

data <- read_parquet(DATA_DIR)
post_code <- st_read(GEO_DIR)

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
# SHARED TABLE HELPERS
# ============================================================
dir.create("./output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/figures", recursive = TRUE, showWarnings = FALSE)

gm <- tribble(
  ~raw,                   ~clean,                   ~fmt,
  "nobs",                 "Observations",           0,
  "FE: bvd_id",           "Firm FE",                NA_real_,
  "FE: year",             "Year FE",                NA_real_,
  "FE: sic_1digit[year]", "Industry-trend",         NA_real_
)

# ============================================================
# H1 — HETEROGENEITY BY FIRM AGE QUARTILE (h1_age_quartile.tex)
# ============================================================
data <- data %>%
  mutate(age_q = ntile(baseline_age, 4))

h1_ai  <- list()
h1_llm <- list()

for (q in 1:4) {
  sub_df <- data[data$age_q == q, ]
  h1_ai[[q]] <- feols(
    lemp ~ i(post, ai_2021, ref = 0) | bvd_id + year + sic_1digit[year],
    cluster = ~sic_2digit, nthreads = 10, weights = ~w_ai, data = sub_df
  )
  h1_llm[[q]] <- feols(
    lemp ~ i(post, llm_2021, ref = 0) | bvd_id + year + sic_1digit[year],
    cluster = ~sic_2digit, nthreads = 10, weights = ~w_llm, data = sub_df
  )
  rm(sub_df)
}

panelsummary(
  h1_ai,
  h1_llm,
  panel_labels = c("Panel A: AI Exposure", "Panel B: LLM Exposure"),
  italic = TRUE, format = "latex", hline_after = TRUE, hline_before_fe = TRUE,
  pretty_num = TRUE, collapse_fe = TRUE, fmt = 3,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gm,
  coef_map = c(
    "post::1:ai_2021"  = "Exposure × Post",
    "post::1:llm_2021" = "Exposure × Post"
  ),
  caption = "Heterogeneity by Firm Age Quartile (OLS)",
  colnames = c("", "Q1 (Youngest)", "Q2", "Q3", "Q4 (Oldest)")
) %>%
  kable_styling(latex_options = c("booktabs", "hold_position"), position = "center", full_width = FALSE) %>%
  save_kable("output/tables/h1_age_quartile.tex")

# ============================================================
# HETEROGENEITY BY LOCATION (DENSITY-WEIGHTED HEXBINS) [EXTREEMELY MEMORY INTENSIVE]
# ============================================================

# # 1. Standardize postcodes & aggregate scores
# data_aggregated <- data %>%
#   group_by(postcode) %>%
#   summarise(
#     ai_score = mean(w_ai, na.rm = TRUE),
#     llm_score = mean(w_llm, na.rm = TRUE)
#   ) %>%
#   mutate(postcode = str_replace_all(toupper(postcode), " ", ""))

# # 2. Join with spatial data, remove NAs, and crop rogue coordinates
# firms_sf <- post_code %>%
#   mutate(postcode = str_replace_all(toupper(postcode), " ", "")) %>%
#   left_join(data_aggregated, by = "postcode") %>%
#   filter(!is.na(ai_score) & !is.na(llm_score)) %>%
#   st_crop(xmin = 0, ymin = 0, xmax = 700000, ymax = 1300000) %>%
#   st_as_sf()

# # 3. Create a physical hexagonal grid over the UK (15km cells)
# hex_grid <- st_make_grid(firms_sf, cellsize = 15000, square = FALSE) %>% 
#   st_sf() %>%
#   mutate(grid_id = row_number())

# # 4. Intersect the firms with the grid
# firms_in_hex <- st_join(firms_sf, hex_grid, join = st_intersects)

# # 5. Aggregate BOTH AI and LLM scores AND count density per hexagon
# hex_aggregated <- firms_in_hex %>%
#   st_drop_geometry() %>%
#   group_by(grid_id) %>%
#   summarise(
#     avg_ai = mean(ai_score, na.rm = TRUE),
#     avg_llm = mean(llm_score, na.rm = TRUE),
#     firm_density = n()
#   ) %>%
#   filter(!is.na(avg_ai) & !is.na(avg_llm))

# # 6. Merge the aggregated stats back onto the spatial grid shapes
# final_hex_map <- hex_grid %>%
#   inner_join(hex_aggregated, by = "grid_id")

# ============================================================
# PLOTTING
# ============================================================

# # Plot 1: AI (Standard Blue Palette)
# plot_ai_density <- ggplot(data = final_hex_map) +
#   geom_sf(aes(fill = avg_ai, alpha = firm_density), color = NA) +
#   scale_fill_distiller(
#     palette = "Blues", 
#     direction = 1, # 1 makes darker = higher score
#     name = "Avg AI Score",
#     limits = quantile(final_hex_map$avg_ai, probs = c(0.05, 0.95), na.rm = TRUE),
#     oob = scales::squish
#   ) +
#   scale_alpha_continuous(
#     name = "Firm Count",
#     range = c(0.15, 1), 
#     trans = "log10"     
#   ) +
#   theme_void() +
#   theme(legend.position = "right") 

# # Plot 2: LLM (Purple-Blue Palette)
# plot_llm_density <- ggplot(data = final_hex_map) +
#   geom_sf(aes(fill = avg_llm, alpha = firm_density), color = NA) +
#   scale_fill_distiller(
#     palette = "PuBu", 
#     direction = 1, 
#     name = "Avg LLM Score",
#     limits = quantile(final_hex_map$avg_llm, probs = c(0.05, 0.95), na.rm = TRUE),
#     oob = scales::squish
#   ) +
#   scale_alpha_continuous(
#     name = "Firm Count",
#     range = c(0.15, 1), 
#     trans = "log10"     
#   ) +
#   theme_void() +
#   theme(legend.position = "right")

# # Save as compact, crisp PDFs
# ggsave(filename = "output/figures/map_ai_density_weighted.pdf", plot = plot_ai_density, device = "pdf", width = 5, height = 6, units = "in")
# ggsave(filename = "output/figures/map_llm_density_weighted.pdf", plot = plot_llm_density, device = "pdf", width = 5, height = 6, units = "in")


# data <- data %>%
#   mutate(
#     # 1. Temporarily strip spaces and make uppercase just to be safe
#     pc_clean = str_replace_all(toupper(postcode), " ", ""),
    
#     # 2. Use regex to find postcodes starting with the core London letters 
#     # IMMEDIATELY followed by a number (to prevent 'W' matching 'WA' for Warrington, etc.)
#     in_london = ifelse(str_detect(pc_clean, "^(E|EC|N|NW|SE|SW|W|WC)[0-9]"), 1, 0)
#   ) %>%
#   # 3. Drop the temporary clean column
#   select(-pc_clean)

# # OLS — AI — London
# ols.ai.london <- feols(
#   lemp ~ i(post, ai_2021, ref = 0) |
#     bvd_id + year + sic_1digit[year],
#   cluster = ~sic_2digit, nthreads = 10,
#   weights = ~w_ai, data = data[data$in_london == 1, ]
# )

# # OLS — AI — Rest of UK
# ols.ai.ruk <- feols(
#   lemp ~ i(post, ai_2021, ref = 0) |
#     bvd_id + year + sic_1digit[year],
#   cluster = ~sic_2digit, nthreads = 10,
#   weights = ~w_ai, data = data[data$in_london == 0, ]
# )

# # IV — AI — London
# iv.ai.london <- feols(
#   lemp ~ 1 | bvd_id + year + sic_1digit[year] |
#     i(post, ai_2021, ref = 0) ~ i(post, ai_2011, ref = 0),
#   cluster = ~sic_2digit, nthreads = 10,
#   weights = ~w_ai, data = data[data$in_london == 1, ]
# )

# # IV — AI — Rest of UK
# iv.ai.ruk <- feols(
#   lemp ~ 1 | bvd_id + year + sic_1digit[year] |
#     i(post, ai_2021, ref = 0) ~ i(post, ai_2011, ref = 0),
#   cluster = ~sic_2digit, nthreads = 10,
#   weights = ~w_ai, data = data[data$in_london == 0, ]
# )

# # OLS — LLM — London
# ols.llm.london <- feols(
#   lemp ~ i(post, llm_2021, ref = 0) |
#     bvd_id + year + sic_1digit[year],
#   cluster = ~sic_2digit, nthreads = 10,
#   weights = ~w_llm, data = data[data$in_london == 1, ]
# )

# # OLS — LLM — Rest of UK
# ols.llm.ruk <- feols(
#   lemp ~ i(post, llm_2021, ref = 0) |
#     bvd_id + year + sic_1digit[year],
#   cluster = ~sic_2digit, nthreads = 10,
#   weights = ~w_llm, data = data[data$in_london == 0, ]
# )

# # IV — LLM — London
# iv.llm.london <- feols(
#   lemp ~ 1 | bvd_id + year + sic_1digit[year] |
#     i(post, llm_2021, ref = 0) ~ i(post, llm_2011, ref = 0),
#   cluster = ~sic_2digit, nthreads = 10,
#   weights = ~w_llm, data = data[data$in_london == 1, ]
# )

# # IV — LLM — Rest of UK
# iv.llm.ruk <- feols(
#   lemp ~ 1 | bvd_id + year + sic_1digit[year] |
#     i(post, llm_2021, ref = 0) ~ i(post, llm_2011, ref = 0),
#   cluster = ~sic_2digit, nthreads = 10,
#   weights = ~w_llm, data = data[data$in_london == 0, ]
# )

# # ============================================================
# # LONDON vs REST OF UK HETEROGENEITY TABLE
# # ============================================================
# panelsummary(
#   list(ols.ai.london, iv.ai.london, ols.ai.ruk, iv.ai.ruk),
#   list(ols.llm.london, iv.llm.london, ols.llm.ruk, iv.llm.ruk),
#   panel_labels = c("Panel A: AI Exposure", "Panel B: LLM Exposure"),
#   italic = TRUE,
#   format = "latex",
#   hline_after = TRUE,
#   hline_before_fe = TRUE,
#   pretty_num = TRUE,
#   collapse_fe = TRUE,
#   fmt = 3,
#   stars = c("*" = .1, "**" = .05, "***" = .01),
#   gof_map = gm,
#   coef_map = c(
#     "post::1:ai_2021"      = "Exposure × Post",
#     "post::1:ai_2011"      = "Exposure × Post",
#     "fit_post::1:ai_2021"  = "Exposure × Post",
#     "post::1:llm_2021"     = "Exposure × Post",
#     "post::1:llm_2011"     = "Exposure × Post",
#     "fit_post::1:llm_2021" = "Exposure × Post"
#   ),
#   caption = "Heterogeneity by Firm Location: London vs Rest of UK",
#   colnames = c("", "OLS", "IV", "OLS", "IV")
# ) %>%
#   add_header_above(
#     c(" " = 1, "London" = 2, "Rest of UK" = 2)
#   ) %>%
#   kable_styling(
#     latex_options = c("booktabs", "hold_position"),
#     position = "center",
#     full_width = FALSE
#   ) %>%
#   save_kable("output/tables/london_heterogeneity.tex")
