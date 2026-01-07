# 00_create_long_data.R
# Master script for creating the LONG format panel dataset
# Translated from FE/do/read.do
#
# This script:
# 1. Loads the raw panel data (long format: one row per person-wave)
# 2. Sources 01_create_long_variables.R to create lagged, cumulative, and transition variables
# 3. Sources 02_create_long_decomposition.R to create between/within decomposition
# 4. Sources 03_merge_external_data.R to merge in external datasets
# 5. Saves the processed long dataset for analysis scripts

library(haven)
library(dplyr)
library(tidyr)

# Set working directory (adjust path as needed)
# setwd("/Users/chrissoria/Documents/Research/Migration_Networks_UCNets")

# ==============================================================================
# LOAD RAW DATA
# ==============================================================================
cat("Loading raw data...\n")
df <- read_dta("data/R_LONG_Moving_Paper_CS_file_24_02_24.dta")

# Ensure data is sorted by prim_key and WAVE
df <- df %>% arrange(prim_key, WAVE)

cat(sprintf("Loaded %d observations for %d unique individuals\n",
            nrow(df),
            n_distinct(df$prim_key)))

# Print WAVE distribution
cat("\nWAVE distribution:\n")
print(table(df$WAVE))

# ==============================================================================
# CREATE VARIABLES
# Sources: lagged variables, cumulative variables, transition variables,
#          derived variables, and variable aliases
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("Creating variables...\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

source("code/01_create_long_variables.R")

# Print WAVE by Moved_w23 cross-tabulation
cat("\nWAVE by Moved_w23 cross-tabulation:\n")
print(table(df$WAVE, df$Moved_w23, useNA = "ifany"))

# ==============================================================================
# PANEL DECOMPOSITION
# Sources: between/within decomposition, demographics, weights, data saving
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("Creating panel decomposition...\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

source("code/02_create_long_decomposition.R")

# ==============================================================================
# MERGE EXTERNAL DATA
# Sources: external datasets merged on prim_key_wave
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("Merging external data...\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

source("code/03_merge_external_data.R")

# ==============================================================================
# SUMMARY
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("DATA PROCESSING COMPLETE\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

cat(sprintf("\nFinal dataset: %d observations, %d variables\n",
            nrow(df), ncol(df)))

cat("\nKey outcome variables:\n")
cat("  - Moved_w23: Any move between waves 2-3\n")
cat("  - Moved_in_Bay_w23: Moved within Bay Area\n")
cat("  - Moved_out_of_Bay_w23: Moved out of Bay Area\n")

cat("\nData files saved:\n")
cat("  - data/R_DF.dta (Stata format)\n")
cat("  - data/R_DF.rds (R format)\n")
