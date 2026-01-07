# main_results.R
# Translated from FE/do/main_results.do
# Runs random effects panel models with robust standard errors for young age group
# Outcomes: Moved_out_of_Bay_w23, Moved_in_Bay_w23, Plan_to_Move

library(haven)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(broom)

# Set working directory (adjust path as needed)
# setwd("/Users/chrissoria/Documents/Research/Migration_Networks_UCNets")

# Load the processed data
df <- read_dta("data/R_DF.dta")
# Or use: df <- readRDS("data/R_DF.rds")

# Filter to young age group only
df_young <- df %>% filter(agegroup == 0)

# Calculate within-person variation in outcome
df_young <- df_young %>%
  group_by(prim_key) %>%
  mutate(variation_out_bay = sd(Moved_out_of_Bay_w23, na.rm = TRUE)) %>%
  ungroup()

# Create panel data frame
pdata <- pdata.frame(df_young, index = c("prim_key", "WAVE"))

# ------------------------------------------------------------------------------
# Define common predictors (used across all models)
# ------------------------------------------------------------------------------

# Base predictors (without Plan_to_Move)
base_predictors <- c(
  # Status variables
  "within_BA_or_greater", "between_BA_or_greater",
  # Network variables
  "within_Kin_within_60_N", "between_Kin_within_60_N",
  "within_Nonkin_within_60_N", "between_Nonkin_within_60_N",
  "within_Nonkin_in_HH_N", "between_Nonkin_in_HH_N",
  "within_Kin_in_HH_N", "between_Kin_in_HH_N",
  "within_Kin_GT_Hour_N", "between_Kin_GT_Hour_N",
  "within_Nonkin_GT_Hour_N", "between_Nonkin_GT_Hour_N",
  # Directional transition variables
  "within_Homeowner_to_renter", "within_Renter_to_homeowner",
  "within_Married_to_divorced", "within_Not_to_Married",
  "within_Fulltime_to_less", "within_Less_to_fulltime",
  "within_FinanProb_to_not", "within_Not_to_FinanProb",
  # Life events
  "within_LE_NewJob",
  "within_lag_Moved_Town",
  "within_LE_NewSchool",
  # Demographics (time-invariant)
  "black_race", "asian_race", "other_race", "latino_eth", "female",
  "between_age", "between_web"
)

# Predictors with Plan_to_Move
plan_predictors <- c(
  "within_Plan_to_Move", "between_Plan_to_Move",
  base_predictors
)

# ------------------------------------------------------------------------------
# Helper function to run RE model with robust SEs and format output
# ------------------------------------------------------------------------------
run_re_model <- function(formula, data, model_name) {
  # Fit random effects model
  model <- plm(formula, data = data, model = "random")

  # Get robust standard errors
  robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))

  # Get coefficients and construct summary
  coefs <- coef(model)

  # Calculate t-statistics and p-values with robust SEs
  t_stats <- coefs / robust_se
  p_values <- 2 * pt(abs(t_stats), df = model$df.residual, lower.tail = FALSE)

  # Create results data frame
  results <- data.frame(
    term = names(coefs),
    estimate = coefs,
    std.error = robust_se,
    statistic = t_stats,
    p.value = p_values,
    model = model_name
  )
  rownames(results) <- NULL

  # Add significance stars
  results$stars <- case_when(
    results$p.value < 0.01 ~ "***",
    results$p.value < 0.05 ~ "**",
    results$p.value < 0.10 ~ "*",
    TRUE ~ ""
  )

  # Store number of observations
  attr(results, "nobs") <- nobs(model)
  attr(results, "model_object") <- model

  return(results)
}

# ------------------------------------------------------------------------------
# Model 1: Moved Out of Bay - No Plan
# ------------------------------------------------------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("MODEL 1: Moved Out of Bay - Without Plan to Move\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

formula_out_bay_no_plan <- as.formula(
  paste("Moved_out_of_Bay_w23 ~", paste(base_predictors, collapse = " + "))
)

out_bay_young_no_plan <- run_re_model(formula_out_bay_no_plan, pdata, "out_bay_no_plan")
cat("N =", attr(out_bay_young_no_plan, "nobs"), "\n\n")

# ------------------------------------------------------------------------------
# Model 2: Moved Out of Bay - With Plan
# ------------------------------------------------------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("MODEL 2: Moved Out of Bay - With Plan to Move\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

formula_out_bay_plan <- as.formula(
  paste("Moved_out_of_Bay_w23 ~", paste(plan_predictors, collapse = " + "))
)

out_bay_young_plan <- run_re_model(formula_out_bay_plan, pdata, "out_bay_plan")
cat("N =", attr(out_bay_young_plan, "nobs"), "\n\n")

# ------------------------------------------------------------------------------
# Model 3: Plan to Move
# ------------------------------------------------------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("MODEL 3: Plan to Move\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

formula_plan <- as.formula(
  paste("Plan_to_Move ~", paste(base_predictors, collapse = " + "))
)

young_plan_to_move <- run_re_model(formula_plan, pdata, "plan_to_move")
cat("N =", attr(young_plan_to_move, "nobs"), "\n\n")

# ------------------------------------------------------------------------------
# Models 4-5: Moved Within Bay (excluding out-of-Bay movers)
# ------------------------------------------------------------------------------

# Filter out those who moved out of Bay
df_within_bay <- df_young %>% filter(Moved_out_of_Bay_w23 != 1 | is.na(Moved_out_of_Bay_w23))

# Calculate within-person variation
df_within_bay <- df_within_bay %>%
  group_by(prim_key) %>%
  mutate(variation_within_bay = sd(Moved_in_Bay_w23, na.rm = TRUE)) %>%
  ungroup()

# Create panel data frame
pdata_within <- pdata.frame(df_within_bay, index = c("prim_key", "WAVE"))

# Model 4: Within Bay - No Plan
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("MODEL 4: Moved Within Bay - Without Plan to Move\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

formula_within_bay_no_plan <- as.formula(
  paste("Moved_in_Bay_w23 ~", paste(base_predictors, collapse = " + "))
)

within_bay_young_no_plan <- run_re_model(formula_within_bay_no_plan, pdata_within, "within_bay_no_plan")
cat("N =", attr(within_bay_young_no_plan, "nobs"), "\n\n")

# Model 5: Within Bay - With Plan
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("MODEL 5: Moved Within Bay - With Plan to Move\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

formula_within_bay_plan <- as.formula(
  paste("Moved_in_Bay_w23 ~", paste(plan_predictors, collapse = " + "))
)

within_bay_young_plan <- run_re_model(formula_within_bay_plan, pdata_within, "within_bay_plan")
cat("N =", attr(within_bay_young_plan, "nobs"), "\n\n")

# ------------------------------------------------------------------------------
# Export results to CSV (matching Stata estout format)
# ------------------------------------------------------------------------------

# Function to format results for export
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

# Combine mobility models
mobility_results <- out_bay_young_no_plan %>%
  format_for_export("Out_Bay_No_Plan") %>%
  full_join(
    format_for_export(out_bay_young_plan, "Out_Bay_Plan"),
    by = "term"
  ) %>%
  full_join(
    format_for_export(within_bay_young_no_plan, "Within_Bay_No_Plan"),
    by = "term"
  ) %>%
  full_join(
    format_for_export(within_bay_young_plan, "Within_Bay_Plan"),
    by = "term"
  )

# Add observation counts as final row
obs_row <- data.frame(
  term = "Observations",
  Out_Bay_No_Plan_coef = as.character(attr(out_bay_young_no_plan, "nobs")),
  Out_Bay_No_Plan_se = "",
  Out_Bay_Plan_coef = as.character(attr(out_bay_young_plan, "nobs")),
  Out_Bay_Plan_se = "",
  Within_Bay_No_Plan_coef = as.character(attr(within_bay_young_no_plan, "nobs")),
  Within_Bay_No_Plan_se = "",
  Within_Bay_Plan_coef = as.character(attr(within_bay_young_plan, "nobs")),
  Within_Bay_Plan_se = ""
)

mobility_results <- bind_rows(mobility_results, obs_row)

# Save mobility results
write.csv(mobility_results, "out/main_table_mobility.csv", row.names = FALSE)
cat("\nMobility results saved to out/main_table_mobility.csv\n")

# Save plan to move results
plan_results <- young_plan_to_move %>%
  format_for_export("Plan_to_Move")

plan_obs_row <- data.frame(
  term = "Observations",
  Plan_to_Move_coef = as.character(attr(young_plan_to_move, "nobs")),
  Plan_to_Move_se = ""
)

plan_results <- bind_rows(plan_results, plan_obs_row)

write.csv(plan_results, "out/main_table_plan.csv", row.names = FALSE)
cat("Plan to Move results saved to out/main_table_plan.csv\n")

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
  cat("N =", attr(results, "nobs"), "\n")
  cat("Significance: * p<0.10, ** p<0.05, *** p<0.01\n")
}

print_model_summary(out_bay_young_no_plan, "Out of Bay - No Plan")
print_model_summary(out_bay_young_plan, "Out of Bay - With Plan")
print_model_summary(young_plan_to_move, "Plan to Move")
print_model_summary(within_bay_young_no_plan, "Within Bay - No Plan")
print_model_summary(within_bay_young_plan, "Within Bay - With Plan")

# ------------------------------------------------------------------------------
# Store model objects for later use
# ------------------------------------------------------------------------------
models <- list(
  out_bay_no_plan = attr(out_bay_young_no_plan, "model_object"),
  out_bay_plan = attr(out_bay_young_plan, "model_object"),
  plan_to_move = attr(young_plan_to_move, "model_object"),
  within_bay_no_plan = attr(within_bay_young_no_plan, "model_object"),
  within_bay_plan = attr(within_bay_young_plan, "model_object")
)

saveRDS(models, "out/main_results_models.rds")
cat("\nModel objects saved to out/main_results_models.rds\n")
