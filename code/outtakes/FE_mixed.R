# FE_mixed.R
# Mixed effects models with random intercepts and robust standard errors
# Models run for both young (agegroup == 0) and old (agegroup == 1) groups

library(dplyr)
library(lme4)
library(lmerTest)
library(clubSandwich)

# Load the processed data
df <- readRDS("data/out/R_DF.rds")

# ==============================================================================
# PREDICTOR CONFIGURATION
# ==============================================================================
# Define predictor groups - easy to modify in one place

within_predictors <- c(

"within_lag_Moved_Town",
  "within_lag_Owns_Home",
  "within_lag_Full_Time",
  "within_lag_LT_BA",
  "within_lag_New_Job",
  "within_lag_New_School",
  "within_lag_Finan_Prob",
  "within_lag_Marr_Cohab",
  "within_lag_Kin_within_60_N",
  "within_lag_Nonkin_within_60_N",
  "within_lag_Nonkin_in_HH_N",
  "within_lag_Kin_in_HH_N",
  "within_lag_Kin_GT_Hour",
  "within_lag_Nonkin_GT_Hour",
  "within_lag_LEX_Birth"
)

between_predictors <- c(
  "between_web",
  "between_LT_BA",
  "between_Owns_home",
  "between_employed",
  "between_Marr_Cohab"
)

demographic_predictors <- c(
  "black_race",
  "asian_race",
  "other_race",
  "latino_eth",
  "gender",
  "between_age"
)

# Combine into base predictors
base_predictors <- c(within_predictors, between_predictors, demographic_predictors)

# Plan predictor (added to base for "with plan" models)
plan_predictor <- "within_lag_Plan_Move"

# ==============================================================================
# MODEL CONFIGURATION
# ==============================================================================
# Define all models in a single configuration list
# Each model specifies: outcome, age_group, include_plan, exclude_out_of_bay

model_specs <- list(
  # Young group models
  list(name = "out_bay_young_no_plan",   outcome = "Moved_out_of_Bay_w23", age_group = 0, include_plan = FALSE, exclude_out_of_bay = FALSE),
  list(name = "out_bay_young_plan",      outcome = "Moved_out_of_Bay_w23", age_group = 0, include_plan = TRUE,  exclude_out_of_bay = FALSE),
  list(name = "young_plan_to_move",      outcome = "Plan_to_Move",         age_group = 0, include_plan = FALSE, exclude_out_of_bay = FALSE),
  list(name = "within_bay_young_no_plan", outcome = "Moved_in_Bay_w23",    age_group = 0, include_plan = FALSE, exclude_out_of_bay = TRUE),
  list(name = "within_bay_young_plan",   outcome = "Moved_in_Bay_w23",     age_group = 0, include_plan = TRUE,  exclude_out_of_bay = TRUE),

  # Old group models
  list(name = "out_bay_old_no_plan",     outcome = "Moved_out_of_Bay_w23", age_group = 1, include_plan = FALSE, exclude_out_of_bay = FALSE),
  list(name = "out_bay_old_plan",        outcome = "Moved_out_of_Bay_w23", age_group = 1, include_plan = TRUE,  exclude_out_of_bay = FALSE),
  list(name = "old_plan_to_move",        outcome = "Plan_to_Move",         age_group = 1, include_plan = FALSE, exclude_out_of_bay = FALSE),
  list(name = "within_bay_old_no_plan",  outcome = "Moved_in_Bay_w23",     age_group = 1, include_plan = FALSE, exclude_out_of_bay = TRUE),
  list(name = "within_bay_old_plan",     outcome = "Moved_in_Bay_w23",     age_group = 1, include_plan = TRUE,  exclude_out_of_bay = TRUE)
)

# ==============================================================================
# MODEL FUNCTIONS
# ==============================================================================

#' Run a mixed effects model with robust standard errors
#' @param outcome Character: outcome variable name
#' @param predictors Character vector: predictor variable names
#' @param data Data frame: analysis data
#' @param model_name Character: name for the model
#' @return Data frame with coefficients, robust SEs, and significance
run_mixed_model <- function(outcome, predictors, data, model_name) {
  # Build formula with random intercept
  formula_str <- paste(outcome, "~", paste(predictors, collapse = " + "), "+ (1 | prim_key)")
  formula <- as.formula(formula_str)

  # Fit mixed effects model
  model <- lmer(formula, data = data, REML = TRUE,
                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

  # Get robust standard errors (CR2 estimator)
  robust_vcov <- vcovCR(model, type = "CR2")
  robust_se <- sqrt(diag(robust_vcov))

  # Get coefficients and compute test statistics
  coefs <- fixef(model)
  df_resid <- df.residual(model)
  t_stats <- coefs / robust_se
  p_values <- 2 * pt(abs(t_stats), df = df_resid, lower.tail = FALSE)

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
  attr(results, "model_object") <- model

  return(results)
}

#' Run a single model from specification
#' @param spec List: model specification
#' @param data Data frame: full dataset
#' @param base_preds Character vector: base predictors
#' @param plan_pred Character: plan predictor to add if include_plan = TRUE
#' @return Model results data frame
run_model_from_spec <- function(spec, data, base_preds, plan_pred) {
  # Filter by age group
  model_data <- data %>% filter(agegroup == spec$age_group)

  # Exclude out-of-bay movers if needed (for within-bay models)
  if (spec$exclude_out_of_bay) {
    model_data <- model_data %>%
      filter(Moved_out_of_Bay_w23 != 1 | is.na(Moved_out_of_Bay_w23))
  }

  # Build predictor list
  predictors <- if (spec$include_plan) c(base_preds, plan_pred) else base_preds

  # Run model
  age_label <- if (spec$age_group == 0) "Young" else "Old"
  cat(sprintf("\nRunning: %s (%s)\n", spec$name, age_label))

  results <- run_mixed_model(spec$outcome, predictors, model_data, spec$name)
  cat("N =", attr(results, "nobs"), "\n")

  return(results)
}

# ==============================================================================
# RUN ALL MODELS
# ==============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("RUNNING ALL MIXED EFFECTS MODELS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

# Run all models and store results in a named list
all_results <- lapply(model_specs, function(spec) {
  run_model_from_spec(spec, df, base_predictors, plan_predictor)
})
names(all_results) <- sapply(model_specs, `[[`, "name")

# ==============================================================================
# EXPORT FUNCTIONS
# ==============================================================================

#' Format model results for export
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

#' Combine multiple model results into a single table
combine_results <- function(result_list, labels) {
  combined <- format_for_export(result_list[[1]], labels[1])
  for (i in 2:length(result_list)) {
    combined <- full_join(combined, format_for_export(result_list[[i]], labels[i]), by = "term")
  }

  # Add observation counts
  obs_row <- data.frame(term = "Observations", stringsAsFactors = FALSE)
  for (i in seq_along(labels)) {
    obs_row[[paste0(labels[i], "_coef")]] <- as.character(attr(result_list[[i]], "nobs"))
    obs_row[[paste0(labels[i], "_se")]] <- ""
  }

  bind_rows(combined, obs_row)
}

#' Print model summary to console
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

# ==============================================================================
# EXPORT RESULTS
# ==============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("EXPORTING RESULTS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

# Out of Bay results
out_of_bay_results <- combine_results(
  list(all_results$out_bay_young_no_plan, all_results$out_bay_young_plan,
       all_results$out_bay_old_no_plan, all_results$out_bay_old_plan),
  c("Young_No_Plan", "Young_Plan", "Old_No_Plan", "Old_Plan")
)
write.csv(out_of_bay_results, "out/out_of_bay_area_moves_results_mixed.csv", row.names = FALSE)
cat("\nSaved: out/out_of_bay_area_moves_results_mixed.csv\n")

# Plan to Move results
plan_results <- combine_results(
  list(all_results$young_plan_to_move, all_results$old_plan_to_move),
  c("Young", "Old")
)
write.csv(plan_results, "out/plan_to_move_results_mixed.csv", row.names = FALSE)
cat("Saved: out/plan_to_move_results_mixed.csv\n")

# Within Bay results
within_bay_results <- combine_results(
  list(all_results$within_bay_young_no_plan, all_results$within_bay_young_plan,
       all_results$within_bay_old_no_plan, all_results$within_bay_old_plan),
  c("Young_No_Plan", "Young_Plan", "Old_No_Plan", "Old_Plan")
)
write.csv(within_bay_results, "out/within_bay_area_moves_results_mixed.csv", row.names = FALSE)
cat("Saved: out/within_bay_area_moves_results_mixed.csv\n")

# ==============================================================================
# PRINT SUMMARIES
# ==============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("MODEL SUMMARIES\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

for (name in names(all_results)) {
  print_model_summary(all_results[[name]], name)
}

# ==============================================================================
# SAVE MODEL OBJECTS
# ==============================================================================

models <- lapply(all_results, function(x) attr(x, "model_object"))
saveRDS(models, "out/mixed_models.rds")
cat("\nModel objects saved to out/mixed_models.rds\n")
