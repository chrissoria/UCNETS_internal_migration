# cumulative_sensitivity.R
# Translated from FE/do/cumulative_sensitivity.do
# Tests sensitivity of results to using cumulative life event variables
# instead of single-period indicators

library(haven)
library(dplyr)
library(lme4)
library(lmerTest)
library(clubSandwich)

# Set working directory (adjust path as needed)
# setwd("/Users/chrissoria/Documents/Research/Migration_Networks_UCNets")

# Load the processed data
df <- read_dta("data/R_DF.dta")

# ------------------------------------------------------------------------------
# Helper function to run mixed model with robust SEs
# ------------------------------------------------------------------------------
run_mixed_model <- function(outcome, predictors, data, model_name) {
  formula_str <- paste(outcome, "~", paste(predictors, collapse = " + "), "+ (1 | prim_key)")
  formula <- as.formula(formula_str)

  model <- tryCatch({
    lmer(formula, data = data, REML = TRUE,
         control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
  }, error = function(e) {
    cat("Error in model", model_name, ":", e$message, "\n")
    return(NULL)
  })

  if (is.null(model)) return(NULL)

  robust_vcov <- vcovCR(model, type = "CR2")
  robust_se <- sqrt(diag(robust_vcov))
  coefs <- fixef(model)
  df_resid <- df.residual(model)
  t_stats <- coefs / robust_se
  p_values <- 2 * pt(abs(t_stats), df = df_resid, lower.tail = FALSE)

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

  results$stars <- case_when(
    results$p.value < 0.01 ~ "***",
    results$p.value < 0.05 ~ "**",
    results$p.value < 0.10 ~ "*",
    TRUE ~ ""
  )

  attr(results, "nobs") <- nobs(model)
  attr(results, "model_object") <- model

  return(results)
}

# ------------------------------------------------------------------------------
# Define predictor sets
# ------------------------------------------------------------------------------

# Baseline predictors (non-cumulative)
baseline_predictors <- c(
  "within_lag_Moved_Town", "within_lag_Owns_Home", "within_lag_Full_Time", "within_lag_BA_or_greater",
  "within_lag_New_Job", "within_lag_New_School", "within_lag_LEX_Birth", "within_lag_Finan_Prob",
  "within_lag_Marr_Cohab", "within_lag_Kin_within_60_N", "within_lag_Nonkin_within_60_N",
  "within_lag_Nonkin_in_HH_N", "within_lag_Kin_in_HH_N", "within_lag_Kin_GT_Hour",
  "within_lag_Nonkin_GT_Hour", "within_web", "within_lag_Plan_Move",
  "between_BA_or_greater", "between_Owns_home", "between_employed", "between_Marr_Cohab",
  "black_race", "asian_race", "other_race", "latino_eth", "female", "between_age"
)

# Baseline for within-bay (uses contemporaneous job/school)
baseline_within_predictors <- c(
  "within_lag_Moved_Town", "within_lag_Owns_Home", "within_lag_Full_Time", "within_lag_BA_or_greater",
  "within_LE_NewJob", "within_LE_NewSchool", "within_lag_LEX_Birth", "within_lag_Finan_Prob",
  "within_lag_Marr_Cohab", "within_lag_Kin_within_60_N", "within_lag_Nonkin_within_60_N",
  "within_lag_Nonkin_in_HH_N", "within_lag_Kin_in_HH_N", "within_lag_Kin_GT_Hour",
  "within_lag_Nonkin_GT_Hour", "within_web", "within_lag_Plan_Move",
  "between_BA_or_greater", "between_Owns_home", "between_employed", "between_Marr_Cohab",
  "black_race", "asian_race", "other_race", "latino_eth", "female", "between_age"
)

# Cumulative predictors (uses cu_ variables instead of single indicators)
cumulative_predictors <- c(
  "within_lag_Moved_Town", "within_lag_Owns_Home", "within_lag_Full_Time", "within_lag_BA_or_greater",
  "within_lag_cu_NewJob", "within_lag_cu_NewSchool", "within_lag_cu_Birth", "within_lag_cu_FinanProb",
  "within_lag_Marr_Cohab", "within_lag_Kin_within_60_N", "within_lag_Nonkin_within_60_N",
  "within_lag_Nonkin_in_HH_N", "within_lag_Kin_in_HH_N", "within_lag_Kin_GT_Hour",
  "within_lag_Nonkin_GT_Hour", "within_web", "within_cu_Plan_to_Move",
  "between_BA_or_greater", "between_Owns_home", "between_employed", "between_Marr_Cohab",
  "black_race", "asian_race", "other_race", "latino_eth", "female", "between_age"
)

# ------------------------------------------------------------------------------
# YOUNG GROUP (agegroup == 0)
# ------------------------------------------------------------------------------
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("YOUNG GROUP - CUMULATIVE SENSITIVITY ANALYSIS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

df_young <- df %>% filter(agegroup == 0)

# Baseline: Out of Bay
cat("\nBaseline: Out of Bay (Young)\n")
baseline_out_young <- run_mixed_model("Moved_out_of_Bay_w23", baseline_predictors,
                                       df_young, "baseline_out_young")
if (!is.null(baseline_out_young)) cat("N =", attr(baseline_out_young, "nobs"), "\n")

# Baseline: Within Bay
df_young_within <- df_young %>% filter(Moved_out_of_Bay_w23 != 1 | is.na(Moved_out_of_Bay_w23))
cat("\nBaseline: Within Bay (Young)\n")
baseline_within_young <- run_mixed_model("Moved_in_Bay_w23", baseline_within_predictors,
                                          df_young_within, "baseline_within_young")
if (!is.null(baseline_within_young)) cat("N =", attr(baseline_within_young, "nobs"), "\n")

# Cumulative: Out of Bay
df_young <- df %>% filter(agegroup == 0)  # Reset
cat("\nCumulative: Out of Bay (Young)\n")
cumulative_out_young <- run_mixed_model("Moved_out_of_Bay_w23", cumulative_predictors,
                                         df_young, "cumulative_out_young")
if (!is.null(cumulative_out_young)) cat("N =", attr(cumulative_out_young, "nobs"), "\n")

# Cumulative: Within Bay
df_young_within <- df_young %>% filter(Moved_out_of_Bay_w23 != 1 | is.na(Moved_out_of_Bay_w23))
cat("\nCumulative: Within Bay (Young)\n")
cumulative_within_young <- run_mixed_model("Moved_in_Bay_w23", cumulative_predictors,
                                            df_young_within, "cumulative_within_young")
if (!is.null(cumulative_within_young)) cat("N =", attr(cumulative_within_young, "nobs"), "\n")

# ------------------------------------------------------------------------------
# OLD GROUP (agegroup == 1)
# ------------------------------------------------------------------------------
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("OLD GROUP - CUMULATIVE SENSITIVITY ANALYSIS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

df_old <- df %>% filter(agegroup == 1)

# Baseline: Out of Bay
cat("\nBaseline: Out of Bay (Old)\n")
baseline_out_old <- run_mixed_model("Moved_out_of_Bay_w23", baseline_predictors,
                                     df_old, "baseline_out_old")
if (!is.null(baseline_out_old)) cat("N =", attr(baseline_out_old, "nobs"), "\n")

# Baseline: Within Bay
df_old_within <- df_old %>% filter(Moved_out_of_Bay_w23 != 1 | is.na(Moved_out_of_Bay_w23))
cat("\nBaseline: Within Bay (Old)\n")
baseline_within_old <- run_mixed_model("Moved_in_Bay_w23", baseline_within_predictors,
                                        df_old_within, "baseline_within_old")
if (!is.null(baseline_within_old)) cat("N =", attr(baseline_within_old, "nobs"), "\n")

# Cumulative: Out of Bay
df_old <- df %>% filter(agegroup == 1)  # Reset
cat("\nCumulative: Out of Bay (Old)\n")
cumulative_out_old <- run_mixed_model("Moved_out_of_Bay_w23", cumulative_predictors,
                                       df_old, "cumulative_out_old")
if (!is.null(cumulative_out_old)) cat("N =", attr(cumulative_out_old, "nobs"), "\n")

# Cumulative: Within Bay
df_old_within <- df_old %>% filter(Moved_out_of_Bay_w23 != 1 | is.na(Moved_out_of_Bay_w23))
cat("\nCumulative: Within Bay (Old)\n")
cumulative_within_old <- run_mixed_model("Moved_in_Bay_w23", cumulative_predictors,
                                          df_old_within, "cumulative_within_old")
if (!is.null(cumulative_within_old)) cat("N =", attr(cumulative_within_old, "nobs"), "\n")

# ------------------------------------------------------------------------------
# Store all models
# ------------------------------------------------------------------------------
models <- list(
  baseline_out_young = if (!is.null(baseline_out_young)) attr(baseline_out_young, "model_object") else NULL,
  baseline_within_young = if (!is.null(baseline_within_young)) attr(baseline_within_young, "model_object") else NULL,
  cumulative_out_young = if (!is.null(cumulative_out_young)) attr(cumulative_out_young, "model_object") else NULL,
  cumulative_within_young = if (!is.null(cumulative_within_young)) attr(cumulative_within_young, "model_object") else NULL,
  baseline_out_old = if (!is.null(baseline_out_old)) attr(baseline_out_old, "model_object") else NULL,
  baseline_within_old = if (!is.null(baseline_within_old)) attr(baseline_within_old, "model_object") else NULL,
  cumulative_out_old = if (!is.null(cumulative_out_old)) attr(cumulative_out_old, "model_object") else NULL,
  cumulative_within_old = if (!is.null(cumulative_within_old)) attr(cumulative_within_old, "model_object") else NULL
)

saveRDS(models, "out/cumulative_sensitivity_models.rds")
cat("\nModels saved to out/cumulative_sensitivity_models.rds\n")

# Store results
results <- list(
  baseline_out_young = baseline_out_young,
  baseline_within_young = baseline_within_young,
  cumulative_out_young = cumulative_out_young,
  cumulative_within_young = cumulative_within_young,
  baseline_out_old = baseline_out_old,
  baseline_within_old = baseline_within_old,
  cumulative_out_old = cumulative_out_old,
  cumulative_within_old = cumulative_within_old
)

saveRDS(results, "out/cumulative_sensitivity_results.rds")
cat("Results saved to out/cumulative_sensitivity_results.rds\n")
