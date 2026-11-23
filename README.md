# The Effect of Generative AI on Employment in the U.K

> This is a term paper written for EC910: Econometrics B at the University of Warwick with a 3000-word limit. Therefore, the paper is "incomplete" because continuous Difference-in-Difference estimators were not explored, attenuation bias of the IV may have been misinterpreted as a null effect, heterogeneity analysis was limited, and computational constraints prevented analysis of larger samples and demand-side estimation. Nevertheless, this project received the [highest grade](https://drive.google.com/file/d/1LVNrWkVeO4LJZhmdFF6wZiNTQOp6y7UG/view?usp=sharing) in the [cohort.](https://drive.google.com/file/d/1HxDbNAD7VIu8h6BBziRfpIXsXwpYehiz/view?usp=sharing)

Compiled paper: [`writeup/main.pdf`](writeup/main.pdf)

---

## Overview & Methodology

This paper aims to identify the causal impact of GenAI and LLM exposure on firm-level employment dynamics in the United Kingdom following their widespread commercial introduction in 2022. While a growing body of literature has begun to examine the aggregate labour market impacts of GenAI in the United States and Nordic countries, comprehensive firm-level evidence for the UK remains limited. We address this gap by providing a large-scale empirical analysis utilising a comprehensive dataset of over 380,000 active firm-level records in Great Britain sourced from Moody's Analytics. To quantify the technological shock, we construct a firm-level measure of LLM exposure by adapting the Artificial Intelligence Occupational Exposure (AIOE) and LLM-specific (LLME) scores developed by Felten et al. (2021) to the UK's Standard Occupational Classification (SOC). A common challenge in this literature is the inherent endogeneity of contemporaneous AI exposure; for instance, unobserved firm-specific restructuring may simultaneously drive both GenAI adoption and employment contraction. To overcome this, we employ a Bartik-style shift-share instrumental variable design. By interacting the historical occupational composition of industries from the 2011 UK Census with aggregate occupation-level LLM exposure, we attempt to isolate exogenous variation in the technological shock to consistently estimate the employment effect. Our main outcome variable evaluates changes in the log count of firm employees. We aim to determine whether AI acts as a substitute or a complement by evaluating the underlying firm financials. We test for the displacement effect by analysing changes in the firm's wage bill; a reduction in employment accompanied by a stagnant or falling wage bill suggests that firms are substituting traditional labour with potentially cheaper, AI-automated roles. Conversely, to capture the augmentation and productivity effects, we examine operating revenue. An increase in operating revenues without a corresponding increase in the wage bill provides empirical confirmation of AI-driven productivity gains, indicating a positive synergy between LLMs and the existing workforce. Our findings, consistent with the existing literature, indicate that these effects likely counteract each other, resulting in a short run aggregate null effect of GenAI and LLMs on employment.

To ensure that we are making appropriate comparisons between high and low exposure firms, we use inverse propensity score weighting based on a firm's baseline total assets and liquidity ratios. First, we do not include the covariates directly in the regression, interacting them with the event, because that changes the interpretation of the estimand (Baker, 2025). Secondly, we only use these two variables due to their high-disclosure rates relative to employment, leaving us with fewer missing values. Finally, we use entropy-balancing to generate the weights, as it solves a globally convex optimisation problem, and it reliably eradicates correlations between covariates and the treatment intensity (Tübbicke, 2022).

---

## Key Findings

In all three specifications (OLS, Reduced-Form, and IV) and both exposure measures, the difference-in-difference coefficient is economically small and statistically indistinguishable from zero. Under the preferred IV specification, the estimated semi-elasticity is 0.005 (SE = 0.013) for AI exposure and -0.002 (SE = 0.012) for LLM exposure. Neither the OLS nor the TSLS specification shows a visible divergence in the post-2021 period between high- and low-exposure firms. Wald pre-trend tests also do not reject the null of parallel pre-trends for both AI (p = 0.104 under TSLS) and LLM exposure (p = 0.291), supporting the identifying assumption.

The Kleibergen-Paap F-statistics are 396.939 (Panel A) and 392.835 (Panel B). These values exceed both the standard threshold of 10 and the modern benchmark of 100 recommended by Lee et al. (2022). Thus, the first stage avoids weak-instrument bias, and the null result is not due to instrument weakness. The Wu-Hausman test indicates that naive OLS estimates are inconsistent, thereby justifying the use of an IV approach.

Similarly, the table shows no significant effect on log remunerations (AI: 0.011, SE=0.021; LLM: 0.008, SE=0.022). This evidence suggests wages are not falling, so firms are not replacing expensive workers with cheaper AI-augmented roles. Firm turnover shows a positive directional IV effect, but it is not significant. While not a strong signal, this aligns with the finding that AI-exposed firms generate more revenue with an unchanged workforce. The joint pattern across employment, remuneration, and turnover suggests that AI is labour-augmenting rather than labour-displacing in this window. Additionally, the Wu-Hausman test indicates that OLS is consistent for the latter two, so the endogeneity driving the employment results does not appear in the financial data. In short, in the short run, AI seems to raise firm productivity without displacing labour.

The results are robust to variations in specification through placebo treatments, alternative clustering, exclusion of industry trends and running the regressions on a balanced sub-sample of 112,583 firms. This allows us to confirm that the null is not driven by pre-existing trends, is not sensitive to the choice of clustering, industry trends yield a stronger first stage and leaving the estimates statistically unchanged, and that compositional effects from the entry and exit of firms are not driving the result. Heterogeneity analysis reveals that older firms show positive and statistically significant employment effects under both exposure measures, consistent with established firms complementing GenAI and accumulated organisational capital. London and the rest of the UK produce indistinguishable results, ruling out geography-specific explanations.

---

## Repository Structure

```text
├── main.R                                                 # Master runner script
├── ec910_group9_project_log.txt                           # Full R execution log
├── do/
│   ├── 01_dataset/
│   │   ├── 01_01_merge_financials.R
│   │   ├── 01_02_merge_identifiers.R
│   │   ├── 01_03_sic_treatment_map_manually_made.R
│   │   ├── 01_04_raw_merger.R
│   │   └── 01_05_attrition_tests.R
│   ├── 02_analysis/
│   │   ├── 02_01_raw_trend_plots.R
│   │   └── 02_03_main_analysis.R
│   └── 03_robustness/
│       ├── 03_01_robustness_regressions.R
│       └── 03_02_mechanisms_and_heterogeneity.R
├── output/
│   ├── figures/
│   └── tables/
└── writeup/
    └── main.pdf
```
## Replication Instructions
### Prerequisites
* **R** ($\ge 4.2$) with packages:
  ```r
  install.packages(c(
    "tidyverse", "readxl", "arrow", "haven", "lubridate",
    "fixest", "ggfixest", "WeightIt", "cobalt",
    "panelsummary", "modelsummary", "kableExtra", "knitr",
    "tikzDevice", "sf"
  ))
  ```
* Raw data files (not tracked in Git due to size and licensing) placed under `./data/input/`:
  * `./data/input/financials/` — FAME/Orbis firm financial `.xlsx` exports by variable subfolder
  * `./data/input/identifiers/` — `Identifiers - Sheet*.xlsx` files
  * `./data/input/treatment_data/` — `aioe.xlsx`, `census_2021.csv`, `census_2011.csv`, `soc_2020_to_2010_mapping_lfs.csv`, `isco2d_to_soc2010_2d_crosswalk.csv`
  * `./data/input/rti_102_countries/rti_country_specific_survey_predicted.dta`
  * `./data/input/codepo_gb.gpkg` — OS Code-Point Open GB spatial geopackage
### Running the Pipeline
From the project root directory, run:
```bash
Rscript main.R
```
This executes all scripts in `./do/` sequentially, writes intermediate/final Parquet files to `./data/temp/` and `./data/output/`, exports all figures and tables to `./output/`, and logs console output to `ec910_group9_project_log.txt`.
