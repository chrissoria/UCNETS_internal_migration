# CLAUDE.md — code/

Instructions for working with R scripts in this folder.

## Script Numbering & Pipeline Order

Scripts are numbered and must be understood in sequence:

**Data Preparation (00–03):** Run in order — each depends on the prior step.
- `00_create_long_data.Rmd` — Entry point. Loads raw data, sources 01–03.
- `01_create_long_variables.Rmd` — Lagged, cumulative, transition variables.
- `02_create_long_decomposition.Rmd` — Between/within decomposition, demographics, weights.
- `03_merge_external_data.Rmd` — External data merges.

**Descriptives (04–09):** Independent of each other, but all require the dataset from 00.
- `04_move_descriptives.Rmd` — Move rates, demographics of movers.
- `05_friendship_strategy_descriptives.Rmd` — Friendship-making strategies + plots.
- `06_move_reason_descriptives.Rmd` — Reasons for moving + plots.
- `07_plan_action_gap.Rmd` — Plan-to-move conversion, stuck planners + plots.
- `08_psych_distress_scale.Rmd` — Distress by mobility status + plots.
- `09_friendship_strategy_predicts_outcomes.Rmd` — Friendship strategies and mental health.

**Panel Models (10–13):** Independent of each other, all require the dataset from 00.
- `10_fe_models.Rmd` — Fixed effects models (young adults only).
- `10_mixed_models.Rmd` — Mixed effects / hybrid between-within models.
- `10.5_network_change_visualizations.Rmd` — Network change plots.
- `11_fe_mixed_validation.Rmd` — Model comparison and validation.
- `12_causal_robustness.Rmd` — Placebo and mechanism tests.
- `13_mediation_analysis.Rmd` — Mediation analysis for network effects.

**Legacy scripts (plain .R):** `main_results.R`, `FE_mixed.R`, `FE_robust.R`, `*_sensitivity.R` — older versions; the .Rmd files above supersede them.

**Outtakes:** `outtakes/` contains earlier .R versions of scripts 00–03 and legacy model scripts. Do not modify these.

## Conventions

### R Markdown Structure
- All active analysis scripts are `.Rmd` files with `html_document` output.
- Every script sets `root.dir = here::here()` in the setup chunk — all paths are relative to project root via `here()`.
- Scripts use `source(here("code", "helpers", "*.R"))` to load helper functions.

### Helper Files (`helpers/`)
Each helper file serves specific scripts:
- `descriptive_helpers.R` — Formatting (`format_pct`, `format_n_pct`, `format_mean_sd`), table builders. Used by 04–08.
- `plan_action_helpers.R` — Plan-action gap analysis. Used by 07.
- `panel_helpers.R` — General panel model utilities.
- `fe_model_helpers.R` — `run_fe_model()` wrapper for plm + HC1 robust SEs. Used by 10_fe_models.
- `mixed_model_helpers.R` — Mixed model fitting. Used by 10_mixed_models.

### Data Access Pattern
```r
library(here)
df <- readRDS(here("data", "R_DF.rds"))
# or: df <- haven::read_dta(here("data", "R_DF.dta"))
```
The analysis dataset is always loaded from `data/R_DF.rds` (or `.dta`). Raw source is `data/R_LONG_Moving_Paper_CS_file_24_02_24.dta`.

### Key Identifiers
- `prim_key` — Person identifier (panel ID).
- `WAVE` — Survey wave (1, 2, or 3).
- `agegroup` — 0 = young adults (21–30), 1 = older adults.

### Model Fitting Patterns
```r
# Fixed effects (plm)
plm(formula, data = pdata, model = "within", effect = "individual")
vcovHC(model, type = "HC1")

# Mixed effects (lme4)
lmer(outcome ~ predictors + (1 | prim_key))
vcovCR(model, type = "CR2")  # clubSandwich

# Hybrid between-within
# Uses within_* (deviations) and between_* (means) variable prefixes
```

### Output
- HTML renders go to `code/` alongside the .Rmd (gitignored).
- Model result tables/CSVs go to `out/`.

## When Editing These Scripts

- Preserve the `here::here()` root dir pattern — do not use `setwd()` or hardcoded paths.
- Keep observation-level vs respondent-level distinctions clear in section headers.
- Use associational language ("associated with", "higher rates among"), never causal claims.
- When adding plots, follow existing conventions: age group facets, error bars, sample size annotations, angled x-axis labels.
- Do not add new package dependencies without noting them.
