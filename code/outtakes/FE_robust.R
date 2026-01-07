# FE_robust.R
# Translated from FE/do/FE_robust.do
# Runs fixed effects panel models with robust standard errors
# Models run for both young (agegroup == 0) and old (agegroup == 1) groups
# Uses raw lagged variables (not within/between decomposition)

library(haven)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)

# Set working directory (adjust path as needed)
# setwd("/Users/chrissoria/Documents/Research/Migration_Networks_UCNets")

# Load the processed data
df <- read_dta("data/R_DF.dta")
# Or use: df <- readRDS("data/R_DF.rds")

# ------------------------------------------------------------------------------
# Define predictors
# Note: This uses RAW lagged variables (not within/between decomposition)
# ------------------------------------------------------------------------------

# Predictors for mobility outcomes (Out of Bay, Within Bay) - no plan
mobility_predictors_no_plan <- c(

  "lag_Moved_Town", "lag_Owns_Home", "lag_Full_Time", "lag_LT_BA",
  "LE_NewJob", "LE_NewSchool", "lag_LEX_Birth", "lag_Finan_Prob",
  "lag_Marr_Cohab", "lag_Kin_within_60_N", "lag_Nonkin_within_60_N",
  "lag_Nonkin_in_HH_N", "lag_Kin_in_HH_N", "lag_Kin_GT_Hour_N", "lag_Nonkin_GT_Hour_N"
)

# Predictors for mobility outcomes - with plan
mobility_predictors_plan <- c(mobility_predictors_no_plan, "lag_Plan_Move")

# Predictors for Plan to Move outcome (contemporaneous, not lagged)
plan_predictors <- c(
  "LE_MovedTown", "Owns_home", "Full_Time", "LT_BA",
  "LE_NewJob", "LE_NewSchool", "LEX_Birth", "LEX_FinanProb",
  "Marr_Cohab", "Kin_within_60_N", "Nonkin_within_60_N",
  "Nonkin_in_HH_N", "Kin_in_HH_N", "Kin_GT_Hour_N", "Nonkin_GT_Hour_N"
)

# ------------------------------------------------------------------------------
# Helper function to run FE model with robust SEs
# ------------------------------------------------------------------------------
run_fe_model <- function(formula, data, model_name) {
  # Fit fixed effects model
  model <- plm(formula, data = data, model = "within", index = c("prim_key", "WAVE"))

  # Get robust standard errors (HC1 for cluster-robust)
  robust_vcov <- vcovHC(model, type = "HC1", cluster = "group")
  robust_se <- sqrt(diag(robust_vcov))

  # Get coefficients
  coefs <- coef(model)

  # Calculate t-statistics and p-values with robust SEs
  t_stats <- coefs / robust_se
  p_values <- 2 * pt(abs(t_stats), df = model$df.residual, lower.tail = FALSE)

  # Create results data frame
  results <- data.frame(
    term = names(coefs),
    estimate = as.numeric(coefs),
    std.error = robust_se,
    statistic = t_stats,
    p.value = p_values,
    model = model_name,
    stringsAsFactors = FALSE
  )
  rownames(results) <- NULL

  # Add significance stars
  results$stars <- case_when(
    results$p.value < 0.01 ~ "***",
    results$p.value < 0.05 ~ "**",
    results$p.value < 0.10 ~ "*",
    TRUE ~ ""
  )

  # Store metadata
  attr(results, "nobs") <- nobs(model)
  attr(results, "r2") <- summary(model)$r.squared["rsq"]
  attr(results, "model_object") <- model

  return(results)
}

# ------------------------------------------------------------------------------
# YOUNG GROUP MODELS (agegroup == 0)
# ------------------------------------------------------------------------------
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("YOUNG GROUP MODELS (agegroup == 0)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

df_young <- df %>% filter(agegroup == 0)

# Check variation in outcome
df_young <- df_young %>%
  group_by(prim_key) %>%
  mutate(variation = sd(Moved_out_of_Bay_w23, na.rm = TRUE)) %>%
  ungroup()

# Create panel data frame
pdata_young <- pdata.frame(df_young, index = c("prim_key", "WAVE"))

# Model 1: Out of Bay - No Plan (Young)
cat("\nModel 1: Out of Bay - No Plan (Young)\n")
formula_out_no_plan <- as.formula(
 paste("Moved_out_of_Bay_w23 ~", paste(mobility_predictors_no_plan, collapse = " + "))
)
out_bay_young_no_plan <- run_fe_model(formula_out_no_plan, pdata_young, "out_bay_young_no_plan")
cat("N =", attr(out_bay_young_no_plan, "nobs"), "\n")

# Model 2: Out of Bay - With Plan (Young)
cat("\nModel 2: Out of Bay - With Plan (Young)\n")
formula_out_plan <- as.formula(
 paste("Moved_out_of_Bay_w23 ~", paste(mobility_predictors_plan, collapse = " + "))
)
out_bay_young_plan <- run_fe_model(formula_out_plan, pdata_young, "out_bay_young_plan")
cat("N =", attr(out_bay_young_plan, "nobs"), "\n")

# Model 3: Plan to Move (Young)
cat("\nModel 3: Plan to Move (Young)\n")
formula_plan <- as.formula(
 paste("Plan_to_Move ~", paste(plan_predictors, collapse = " + "))
)
young_plan_to_move <- run_fe_model(formula_plan, pdata_young, "young_plan_to_move")
cat("N =", attr(young_plan_to_move, "nobs"), "\n")

# Filter out those who moved out of Bay for within-Bay models
df_young_within <- df_young %>%
  filter(Moved_out_of_Bay_w23 != 1 | is.na(Moved_out_of_Bay_w23))
pdata_young_within <- pdata.frame(df_young_within, index = c("prim_key", "WAVE"))

# Model 4: Within Bay - No Plan (Young)
cat("\nModel 4: Within Bay - No Plan (Young)\n")
formula_within_no_plan <- as.formula(
 paste("Moved_in_Bay_w23 ~", paste(mobility_predictors_no_plan, collapse = " + "))
)
within_bay_young_no_plan <- run_fe_model(formula_within_no_plan, pdata_young_within, "within_bay_young_no_plan")
cat("N =", attr(within_bay_young_no_plan, "nobs"), "\n")

# Model 5: Within Bay - With Plan (Young)
cat("\nModel 5: Within Bay - With Plan (Young)\n")
formula_within_plan <- as.formula(
 paste("Moved_in_Bay_w23 ~", paste(mobility_predictors_plan, collapse = " + "))
)
within_bay_young_plan <- run_fe_model(formula_within_plan, pdata_young_within, "within_bay_young_plan")
cat("N =", attr(within_bay_young_plan, "nobs"), "\n")

# ------------------------------------------------------------------------------
# OLD GROUP MODELS (agegroup == 1)
# ------------------------------------------------------------------------------
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("OLD GROUP MODELS (agegroup == 1)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

df_old <- df %>% filter(agegroup == 1)

# Check variation in outcome
df_old <- df_old %>%
  group_by(prim_key) %>%
  mutate(variation = sd(Moved_out_of_Bay_w23, na.rm = TRUE)) %>%
  ungroup()

# Create panel data frame
pdata_old <- pdata.frame(df_old, index = c("prim_key", "WAVE"))

# Model 6: Out of Bay - No Plan (Old)
cat("\nModel 6: Out of Bay - No Plan (Old)\n")
out_bay_old_no_plan <- run_fe_model(formula_out_no_plan, pdata_old, "out_bay_old_no_plan")
cat("N =", attr(out_bay_old_no_plan, "nobs"), "\n")

# Model 7: Out of Bay - With Plan (Old)
cat("\nModel 7: Out of Bay - With Plan (Old)\n")
out_bay_old_plan <- run_fe_model(formula_out_plan, pdata_old, "out_bay_old_plan")
cat("N =", attr(out_bay_old_plan, "nobs"), "\n")

# Model 8: Plan to Move (Old)
cat("\nModel 8: Plan to Move (Old)\n")
old_plan_to_move <- run_fe_model(formula_plan, pdata_old, "old_plan_to_move")
cat("N =", attr(old_plan_to_move, "nobs"), "\n")

# Filter out those who moved out of Bay for within-Bay models
df_old_within <- df_old %>%
  filter(Moved_out_of_Bay_w23 != 1 | is.na(Moved_out_of_Bay_w23))
pdata_old_within <- pdata.frame(df_old_within, index = c("prim_key", "WAVE"))

# Model 9: Within Bay - No Plan (Old)
cat("\nModel 9: Within Bay - No Plan (Old)\n")
within_bay_old_no_plan <- run_fe_model(formula_within_no_plan, pdata_old_within, "within_bay_old_no_plan")
cat("N =", attr(within_bay_old_no_plan, "nobs"), "\n")

# Model 10: Within Bay - With Plan (Old)
cat("\nModel 10: Within Bay - With Plan (Old)\n")
within_bay_old_plan <- run_fe_model(formula_within_plan, pdata_old_within, "within_bay_old_plan")
cat("N =", attr(within_bay_old_plan, "nobs"), "\n")

# ------------------------------------------------------------------------------
# Export results to CSV
# ------------------------------------------------------------------------------

format_for_export <- function(results, model_label) {
  results %>%
    mutate(
      coef_display = sprintf("%.3f%s", estimate, stars),
      se_display = sprintf("(%.3f)", std.error)
    ) %>%
    select(term, coef_display, se_display) %>%
    rename(!!paste0(model_label, "_coef") := coef_display,
           !!paste0(model_label, "_se") := se_display)
}

# Out of Bay results (young and old)
out_of_bay_results <- format_for_export(out_bay_young_no_plan, "Young_No_Plan") %>%
  full_join(format_for_export(out_bay_young_plan, "Young_Plan"), by = "term") %>%
  full_join(format_for_export(out_bay_old_no_plan, "Old_No_Plan"), by = "term") %>%
  full_join(format_for_export(out_bay_old_plan, "Old_Plan"), by = "term")

# Add R-squared and observation counts
r2_row <- data.frame(
  term = "R-squared",
  Young_No_Plan_coef = sprintf("%.3f", attr(out_bay_young_no_plan, "r2")),
  Young_No_Plan_se = "",
  Young_Plan_coef = sprintf("%.3f", attr(out_bay_young_plan, "r2")),
  Young_Plan_se = "",
  Old_No_Plan_coef = sprintf("%.3f", attr(out_bay_old_no_plan, "r2")),
  Old_No_Plan_se = "",
  Old_Plan_coef = sprintf("%.3f", attr(out_bay_old_plan, "r2")),
  Old_Plan_se = ""
)

obs_row_out <- data.frame(
  term = "Observations",
  Young_No_Plan_coef = as.character(attr(out_bay_young_no_plan, "nobs")),
  Young_No_Plan_se = "",
  Young_Plan_coef = as.character(attr(out_bay_young_plan, "nobs")),
  Young_Plan_se = "",
  Old_No_Plan_coef = as.character(attr(out_bay_old_no_plan, "nobs")),
  Old_No_Plan_se = "",
  Old_Plan_coef = as.character(attr(out_bay_old_plan, "nobs")),
  Old_Plan_se = ""
)

out_of_bay_results <- bind_rows(out_of_bay_results, r2_row, obs_row_out)

write.csv(out_of_bay_results, "out/out_of_bay_area_moves_results.csv", row.names = FALSE)
cat("\nOut of Bay results saved to out/out_of_bay_area_moves_results.csv\n")

# Plan to Move results (young and old)
plan_results <- format_for_export(young_plan_to_move, "Young") %>%
  full_join(format_for_export(old_plan_to_move, "Old"), by = "term")

r2_row_plan <- data.frame(
  term = "R-squared",
  Young_coef = sprintf("%.3f", attr(young_plan_to_move, "r2")),
  Young_se = "",
  Old_coef = sprintf("%.3f", attr(old_plan_to_move, "r2")),
  Old_se = ""
)

obs_row_plan <- data.frame(
  term = "Observations",
  Young_coef = as.character(attr(young_plan_to_move, "nobs")),
  Young_se = "",
  Old_coef = as.character(attr(old_plan_to_move, "nobs")),
  Old_se = ""
)

plan_results <- bind_rows(plan_results, r2_row_plan, obs_row_plan)

write.csv(plan_results, "out/plan_to_move_results.csv", row.names = FALSE)
cat("Plan to Move results saved to out/plan_to_move_results.csv\n")

# Within Bay results (young and old)
within_bay_results <- format_for_export(within_bay_young_no_plan, "Young_No_Plan") %>%
  full_join(format_for_export(within_bay_young_plan, "Young_Plan"), by = "term") %>%
  full_join(format_for_export(within_bay_old_no_plan, "Old_No_Plan"), by = "term") %>%
  full_join(format_for_export(within_bay_old_plan, "Old_Plan"), by = "term")

r2_row_within <- data.frame(
  term = "R-squared",
  Young_No_Plan_coef = sprintf("%.3f", attr(within_bay_young_no_plan, "r2")),
  Young_No_Plan_se = "",
  Young_Plan_coef = sprintf("%.3f", attr(within_bay_young_plan, "r2")),
  Young_Plan_se = "",
  Old_No_Plan_coef = sprintf("%.3f", attr(within_bay_old_no_plan, "r2")),
  Old_No_Plan_se = "",
  Old_Plan_coef = sprintf("%.3f", attr(within_bay_old_plan, "r2")),
  Old_Plan_se = ""
)

obs_row_within <- data.frame(
  term = "Observations",
  Young_No_Plan_coef = as.character(attr(within_bay_young_no_plan, "nobs")),
  Young_No_Plan_se = "",
  Young_Plan_coef = as.character(attr(within_bay_young_plan, "nobs")),
  Young_Plan_se = "",
  Old_No_Plan_coef = as.character(attr(within_bay_old_no_plan, "nobs")),
  Old_No_Plan_se = "",
  Old_Plan_coef = as.character(attr(within_bay_old_plan, "nobs")),
  Old_Plan_se = ""
)

within_bay_results <- bind_rows(within_bay_results, r2_row_within, obs_row_within)

write.csv(within_bay_results, "out/within_bay_area_moves_results.csv", row.names = FALSE)
cat("Within Bay results saved to out/within_bay_area_moves_results.csv\n")

# ------------------------------------------------------------------------------
# Print summary tables
# ------------------------------------------------------------------------------

print_model_summary <- function(results, title) {
  cat("\n", paste(rep("-", 70), collapse = ""), "\n")
  cat(title, "\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  cat(sprintf("%-35s %10s %10s %s\n", "Variable", "Coef", "SE", ""))
  cat(paste(rep("-", 70), collapse = ""), "\n")

  for (i in 1:nrow(results)) {
    cat(sprintf("%-35s %10.3f %10.3f %s\n",
                substr(results$term[i], 1, 35),
                results$estimate[i],
                results$std.error[i],
                results$stars[i]))
  }
  cat(paste(rep("-", 70), collapse = ""), "\n")
  cat("R-squared =", round(attr(results, "r2"), 3), "\n")
  cat("N =", attr(results, "nobs"), "\n")
  cat("Significance: * p<0.10, ** p<0.05, *** p<0.01\n")
}

# Young group summaries
print_model_summary(out_bay_young_no_plan, "Out of Bay - No Plan (Young)")
print_model_summary(out_bay_young_plan, "Out of Bay - With Plan (Young)")
print_model_summary(young_plan_to_move, "Plan to Move (Young)")
print_model_summary(within_bay_young_no_plan, "Within Bay - No Plan (Young)")
print_model_summary(within_bay_young_plan, "Within Bay - With Plan (Young)")

# Old group summaries
print_model_summary(out_bay_old_no_plan, "Out of Bay - No Plan (Old)")
print_model_summary(out_bay_old_plan, "Out of Bay - With Plan (Old)")
print_model_summary(old_plan_to_move, "Plan to Move (Old)")
print_model_summary(within_bay_old_no_plan, "Within Bay - No Plan (Old)")
print_model_summary(within_bay_old_plan, "Within Bay - With Plan (Old)")

# ------------------------------------------------------------------------------
# Store model objects for later use
# ------------------------------------------------------------------------------
models <- list(
  # Young models
  out_bay_young_no_plan = attr(out_bay_young_no_plan, "model_object"),
  out_bay_young_plan = attr(out_bay_young_plan, "model_object"),
  young_plan_to_move = attr(young_plan_to_move, "model_object"),
  within_bay_young_no_plan = attr(within_bay_young_no_plan, "model_object"),
  within_bay_young_plan = attr(within_bay_young_plan, "model_object"),
  # Old models
  out_bay_old_no_plan = attr(out_bay_old_no_plan, "model_object"),
  out_bay_old_plan = attr(out_bay_old_plan, "model_object"),
  old_plan_to_move = attr(old_plan_to_move, "model_object"),
  within_bay_old_no_plan = attr(within_bay_old_no_plan, "model_object"),
  within_bay_old_plan = attr(within_bay_old_plan, "model_object")
)

saveRDS(models, "out/fe_robust_models.rds")
cat("\nModel objects saved to out/fe_robust_models.rds\n")
