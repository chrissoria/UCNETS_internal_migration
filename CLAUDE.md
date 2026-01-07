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

**Long-format data creation:**
- `00_create_long_data.R` - Master script: loads data, sources variable creation scripts
- `01_create_long_variables.R` - Creates lagged, cumulative, transition, and derived variables
- `02_create_long_decomposition.R` - Between/within decomposition, demographics, weights

**Analysis scripts:**
- `main_results.R` - Core random effects panel models
- `FE_mixed.R` - Mixed effects models with random intercepts
- `FE_robust.R` - Fixed effects models with robust standard errors
- `*_sensitivity.R` - Robustness checks and specification tests

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
