# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an academic research project analyzing how personal networks (kinship and non-kinship ties) influence residential mobility decisions in the San Francisco Bay Area. The project uses longitudinal panel data from the UCNets study.

**Primary Language:** R
**Research Focus:** Social network effects on migration patterns across age groups

## Running the Analysis

### Data Processing Pipeline

All commands should be run from R with working directory set to project root.

```r
# 1. Process raw data and create long-format analysis dataset
source("code/00_create_long_data.R")
# Creates: data/R_DF.dta, data/R_DF.rds (long format: one row per person-wave)

# 2. Run main random effects models
source("code/main_results.R")

# 3. Run mixed effects models
source("code/FE_mixed.R")

# 4. Run fixed effects models with robust SEs
source("code/FE_robust.R")

# 5. Run sensitivity analyses (as needed)
source("code/directionality_sensitivity.R")
source("code/cumulative_sensitivity.R")
source("code/simultanous_sensitivity.R")
source("code/simultanous_directionality_sensitivity.R")
```

## Architecture

### Directory Structure

```
Migration_Networks_UCNets/
├── code/           # R scripts for all analyses
├── data/           # Raw and processed data files
├── out/            # Model results (CSV, Excel, RDS)
├── lit/            # Literature review and methods references
├── paper/          # Academic paper drafts and abstracts
└── CLAUDE.md
```

### Code Files

**Long-format data creation (R Markdown):**
- `00_create_long_data.Rmd` - Master script: loads data, sources variable creation scripts
- `01_create_long_variables.Rmd` - Creates lagged, cumulative, transition, and derived variables
- `02_create_long_decomposition.Rmd` - Between/within decomposition, demographics, weights
- `03_merge_external_data.Rmd` - Merges external data sources

**Descriptive analysis scripts (R Markdown with visualizations):**
- `04_move_descriptives.Rmd` - Move rates, demographics of movers (observation & respondent-level)
- `05_friendship_strategy_descriptives.Rmd` - Friendship-making strategies by demographics, includes plots
- `06_move_reason_descriptives.Rmd` - Reasons for moving by demographics, includes plots
- `07_plan_action_gap.Rmd` - Plan-to-move conversion rates, stuck planners analysis, includes plots
- `08_psych_distress_scale.Rmd` - Psychological distress by mobility status (observation & respondent-level), includes plots
- `09_friendship_strategy_predicts_outcomes.Rmd` - Friendship strategies and mental health outcomes

**Panel model scripts (R Markdown):**
- `10_fe_models.Rmd` / `10_mixed_models.Rmd` - Fixed effects and mixed effects models
- `11_fe_mixed_validation.Rmd` - Model validation and comparison
- `12_causal_robustness.Rmd` - Placebo tests and mechanism tests
- `13_mediation_analysis.Rmd` - Mediation analysis for network effects

**Legacy analysis scripts (R):**
- `main_results.R` - Core random effects panel models
- `FE_mixed.R` / `FE_robust.R` - Fixed/mixed effects models with robust SEs
- `*_sensitivity.R` - Robustness checks and specification tests

**Helper functions:**
- `code/helpers/descriptive_helpers.R` - Table creation, export functions
- `code/helpers/panel_helpers.R` - Panel model helpers
- `code/helpers/plan_action_helpers.R` - Plan-action gap analysis functions
- `code/helpers/fe_model_helpers.R` / `mixed_model_helpers.R` - Model fitting functions

### Data Files

- **Source data:** `data/R_LONG_Moving_Paper_CS_file_24_02_24.dta`
- **Analysis dataset:** `data/R_DF.dta` / `data/R_DF.rds` (long format, created by 00_create_long_data.R)
- **Variable documentation:** `data/Variables_for_Models_CS_cf_*.xlsx`

## Key Variables

### Outcomes
- `Moved_out_of_Bay_w23` - Regional out-migration
- `Moved_in_Bay_w23` - Within-region moves
- `Plan_to_Move` - Stated intention to move
- `Moved_w23` - Any residential move

### Network Predictors (Kin_*/Nonkin_* prefix)
- `*_in_HH_N` - Ties in household
- `*_in_5_N` - Ties within 5 minutes
- `*_in_5_to_60_N` - Ties 5-60 minutes away
- `*_GT_Hour_N` - Ties more than 60 minutes away
- `*_within_60_N` - Ties within 60 minutes (combined)

### Life Events (LE_* prefix)
- `LE_NewJob`, `LE_NewSchool`, `LE_FinanProb`, `LE_MovedTown`, `LEX_Birth`

### Variable Transformations
- `lag_*` - Lagged versions for directionality testing
- `between_*` - Between-person means
- `within_*` - Within-person deviations
- Transition variables (e.g., `Homeowner_to_renter`, `Married_to_divorced`)
- Cumulative variables with decay (e.g., `cu_NewJob`, `cu_FinanProb`)

## Analysis Conventions

### Observation-Level vs Respondent-Level

**Observation-Level (default for most analyses):**
- One row per person-wave (longitudinal data)
- Each person can contribute multiple observations
- Used for panel models and most descriptive analyses
- Example: Scripts 05-07 (observation-level by default)

**Respondent-Level (for person-based summaries):**
- One row per person (collapsed across waves)
- Uses "ever" indicators (ever_moved, ever_planned) or means (mean distress)
- Used for comparison with observation-level patterns
- Example: Script 08 Section 5 (observation-level) vs Section 6 (respondent-level)

**When to specify level:**
- Always clarify which level is being used in section headers
- Both levels are reported in scripts 04 and 08 for comparison
- Use person-level for: "How many unique people moved?" questions
- Use observation-level for: Panel models, time-varying effects

### Interpretation Guidelines

**Language conventions (established in scripts 07-09):**
- ✓ Use associational language: "associated with", "shows higher/lower rates"
- ✓ Use descriptive comparisons: "X group shows pattern Y"
- ✗ Avoid causal claims: "causes", "leads to", "drives", "buffers against"
- ✗ Avoid mechanistic language: "take longer to", "defer moves until"
- ✗ Avoid longitudinal implications from cross-sectional data

**Example corrections made:**
- Before: "Networks cause people to stay in place"
- After: "Larger local kin networks are associated with lower mobility"

### Visualization Standards

**Color schemes:**
- Age groups: Young (blue #2166AC), Old (red/orange #B2182B)
- Plan-action: Stuck (red #D73027), Moved (blue #4575B4)
- Demographics: Distinct colors for each group

**Plot types:**
- Bar plots with error bars for means (see scripts 07, 08)
- Faceted plots for age group comparisons (see scripts 05, 06, 07)
- Boxplots for distributions by category (see script 08)

**Common features:**
- Sample sizes displayed (n=X) where appropriate
- Y-axis scales appropriate to data range
- Angled x-axis labels for readability
- Titles clearly indicate observation vs respondent level

## R Conventions

### Panel Data Methods

```r
# Random effects with robust SEs (plm package)
plm(..., model = "random", effect = "individual")
vcovHC(..., type = "HC1")

# Mixed effects with random intercepts (lme4 package)
lmer(outcome ~ predictors + (1 | prim_key))
vcovCR(..., type = "CR2")  # clubSandwich for robust SEs

# Fixed effects with robust SEs
plm(..., model = "within", effect = "individual")
```

### Stratification
- `agegroup == 0` (young adults)
- `agegroup == 1` (older adults)

### Survey Weights
- `wt_dem_95_inf_w3` - Wave 3 demographic weights

## Required R Packages

```r
install.packages(c("haven", "dplyr", "tidyr", "plm", "lme4",
                   "lmerTest", "sandwich", "clubSandwich", "car"))
```

## Documentation

- `paper/Modeling_Plan_8-15-24.docx` - Comprehensive analysis plan
- `paper/Sensitivity_Tests.docx` - Testing documentation
- `out/model_interpretation_and_explanation.docx` - Model interpretation notes

## Git Repository Setup

**Repository:** `git@github.com:chrissoria/UCNETS_internal_migration.git`

**Excluded from version control (.gitignore):**
- Data files (`data/`, `*.dta`, `*.csv`, `*.rds`, etc.)
- Output files (`out/`, `*.html`, `*.pdf`)
- Microsoft Office files (`*.docx`, `*.xlsx`)
- Literature PDFs (`lit/`)
- R temporary files (`.RData`, `.Rhistory`, `.Rproj.user/`)
- System files (`.DS_Store`)

**Included in version control:**
- All R code and R Markdown scripts
- Helper function files
- Configuration files (`.here`, `.Rproj`, `CLAUDE.md`)
- Paper source files (`.qmd`, `.bib`)

## Recent Updates

**Session 2025-01-07:**
- Added visualization plots to scripts 05, 06, 07, 08
- Established observation-level vs respondent-level analysis conventions
- Added respondent-level section (Section 6) to script 08
- Fixed interpretation language across scripts 07-09 to remove causal claims
- Added N rows to demographic tables in script 07
- Created .gitignore and initialized git repository
- Added faceted plots for demographic comparisons (gender, race/ethnicity)
- Added network anchor plots comparing stuck vs moved planners
