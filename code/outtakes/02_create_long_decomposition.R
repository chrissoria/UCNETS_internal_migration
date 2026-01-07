# 02_create_long_decomposition.R
# Creates between-person means and within-person deviations for LONG format panel data
# Also expands time-invariant demographics and creates Wave 3 weights
# This script expects 'df' (long format) to already be loaded in the environment
# Called by 00_create_long_data.R

# ==============================================================================
# MULTICOLLINEARITY CHECK
# ==============================================================================
cor_vars <- c("Marr_Cohab", "Full_Time", "LEX_FinanProb", "LT_BA", "LE_NewJob",
              "lag_New_School", "lag_Moved_Town", "Plan_to_Move", "lag_Kin_in_5_N",
              "lag_Nonkin_in_5_N", "lag_Nonkin_in_HH_N", "lag_Kin_in_HH_N", "lag_Owns_Home")

cat("\nCorrelation matrix for multicollinearity check:\n")
cor_matrix <- df %>%
  select(all_of(cor_vars)) %>%
  cor(use = "pairwise.complete.obs")
print(round(cor_matrix, 3))

# Run linear model for VIF check
vif_model <- lm(Moved_w23 ~ Marr_Cohab + Full_Time + LEX_FinanProb + LT_BA +
                  LE_NewJob + lag_New_School + lag_Moved_Town + Plan_to_Move +
                  lag_Kin_in_5_N + lag_Nonkin_in_5_N + lag_Nonkin_in_HH_N +
                  lag_Kin_in_HH_N + lag_Owns_Home,
                data = df)

cat("\nVIF values:\n")
if (requireNamespace("car", quietly = TRUE)) {
  print(car::vif(vif_model))
} else {
  cat("Install 'car' package for VIF calculation: install.packages('car')\n")
}

# ==============================================================================
# BETWEEN/WITHIN PERSON DECOMPOSITION
# Creates means (between) and deviations from mean (within) for each variable
# ==============================================================================
vars_for_decomposition <- c(
  # Current variables
  "LE_MovedTown", "Owns_home", "Full_Time", "LT_BA", "LE_NewJob", "LE_NewSchool",
  "LEX_Birth", "LE_FinanProb", "Marr_Cohab", "Kin_within_60_N", "Nonkin_within_60_N",
  "Nonkin_in_HH_N", "Kin_in_HH_N", "Kin_GT_Hour_N", "Nonkin_GT_Hour_N",
  "Kin_GT_Hour", "Nonkin_GT_Hour",  # Aliases without _N (as used in analysis scripts)
  "Mother_in_Hour", "Father_in_Hour", "Plan_to_Move", "web",

  # Lagged variables
  "lag_Moved_Town", "lag_Owns_Home", "lag_Full_Time", "lag_LT_BA", "lag_LEX_Birth",
  "lag_Finan_Prob", "lag_Marr_Cohab", "lag_Kin_within_60_N", "lag_Nonkin_within_60_N",
  "lag_Nonkin_in_HH_N", "lag_Kin_in_HH_N",
  "lag_Kin_GT_Hour", "lag_Nonkin_GT_Hour",  # Aliases without _N (as used in analysis scripts)
  "lag_Kin_GT_Hour_N", "lag_Nonkin_GT_Hour_N",  # Original names with _N
  "lag_Plan_Move",

  # Transition variables
  "Homeowner_to_renter", "Renter_to_homeowner", "Fulltime_to_less",
  "Married_to_divorced", "lag_Married_to_divorced", "lag_Fulltime_to_less",
  "lag_Homeowner_to_renter", "lag_Renter_to_homeowner", "Not_to_Married",
  "lag_Not_to_Married", "Less_to_fulltime", "lag_Less_to_fulltime",

  # Age
  "age",

  # Cumulative variables
  "cu_NewJob", "cu_NewSchool", "cu_Plan_to_Move", "cu_FinanProb", "cu_Birth",
  "lag_cu_NewJob", "lag_cu_NewSchool", "lag_cu_Plan_to_Move", "lag_cu_FinanProb",
  "lag_cu_Birth",

  # Other
  "lag_New_School", "lag_New_Job", "employed", "past_finan_prob",
  "FinanProb_to_not", "Not_to_FinanProb", "lag_FinanProb_to_not", "lag_Not_to_FinanProb"
)

# Create between (mean) and within (deviation from mean) variables
for (var in vars_for_decomposition) {
  if (var %in% names(df)) {
    between_name <- paste0("between_", var)
    within_name <- paste0("within_", var)

    df <- df %>%
      group_by(prim_key) %>%
      mutate(
        !!between_name := mean(.data[[var]], na.rm = TRUE),
        !!within_name := .data[[var]] - mean(.data[[var]], na.rm = TRUE)
      ) %>%
      ungroup()
  }
}

# ==============================================================================
# TIME-INVARIANT DEMOGRAPHIC VARIABLES
# Expand across all waves for each individual
# ==============================================================================
df <- df %>%
  mutate(black_race = Eth_black) %>%
  group_by(prim_key) %>%
  mutate(black_race = first(na.omit(black_race))) %>%
  ungroup()

df <- df %>%
  mutate(asian_race = Eth_asian) %>%
  group_by(prim_key) %>%
  mutate(asian_race = first(na.omit(asian_race))) %>%
  ungroup()

df <- df %>%
  mutate(other_race = Eth_other) %>%
  group_by(prim_key) %>%
  mutate(other_race = first(na.omit(other_race))) %>%
  ungroup()

df <- df %>%
  mutate(latino_eth = Eth_latino) %>%
  group_by(prim_key) %>%
  mutate(latino_eth = first(na.omit(latino_eth))) %>%
  ungroup()

df <- df %>%
  group_by(prim_key) %>%
  mutate(gender = first(na.omit(gender))) %>%
  ungroup()

# ==============================================================================
# WAVE 3 WEIGHTS
# Create Wave 3 demographic weights for all observations
# ==============================================================================
df <- df %>%
  group_by(prim_key) %>%
  mutate(
    wt_dem_95_inf_w3 = max(if_else(WAVE == 3, wt_dem_95_inf, NA_real_), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(wt_dem_95_inf_w3 = if_else(is.infinite(wt_dem_95_inf_w3), NA_real_, wt_dem_95_inf_w3))

# ==============================================================================
# SAVE PROCESSED DATA
# ==============================================================================
write_dta(df, "data/R_DF.dta")
cat("\nDataset saved to data/R_DF.dta\n")

# Also save as RDS for faster R loading
saveRDS(df, "data/R_DF.rds")
cat("Dataset also saved to data/R_DF.rds\n")

# ==============================================================================
# EXPLORATORY ANALYSIS: OUT-OF-BAY MOVERS
# Examine young people who moved out of Bay in multiple waves
# ==============================================================================
out_of_bay_young <- df %>%
  filter(Moved_out_of_Bay_w23 == 1) %>%
  filter(WAVE != 1) %>%
  filter(agegroup == 0) %>%
  group_by(prim_key) %>%
  mutate(prim_key_dup = n() - 1) %>%
  ungroup()

cat("\nDuplicate prim_key counts among young out-of-Bay movers:\n")
print(table(out_of_bay_young$prim_key_dup))

# Keep only those with duplicates
out_of_bay_duplicates <- out_of_bay_young %>%
  filter(prim_key_dup == 1) %>%
  select(prim_key, Moved_out_of_Bay_w23)

cat("\nYoung individuals who moved out of Bay in both Wave 2 and 3:\n")
print(out_of_bay_duplicates)

cat("\nPanel decomposition complete.\n")
