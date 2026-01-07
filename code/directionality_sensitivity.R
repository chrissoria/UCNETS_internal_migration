# directionality_sensitivity.R
# Translated from FE/do/directionality_sensitivity.do
# Tests whether directionality of status changes matters
# (e.g., homeowner→renter vs renter→homeowner, married→divorced vs single→married)
# Uses LAGGED within-person variables

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
# Define base predictor set (baseline with standard ownership variable)
# ------------------------------------------------------------------------------
base_predictors <- c(
  "within_lag_Moved_Town", "within_lag_Owns_Home", "within_lag_Full_Time", "within_lag_BA_or_greater",
  "within_lag_New_Job", "within_lag_New_School", "within_lag_LEX_Birth", "within_lag_Finan_Prob",
  "within_lag_Marr_Cohab", "within_lag_Kin_within_60_N", "within_lag_Nonkin_within_60_N",
  "within_lag_Nonkin_in_HH_N", "within_lag_Kin_in_HH_N", "within_lag_Kin_GT_Hour",
  "within_lag_Nonkin_GT_Hour", "within_web", "within_lag_Plan_Move"
)

# Extended base with between-person controls
base_extended <- c(
  "within_lag_Moved_Town", "within_lag_Owns_Home", "within_lag_Full_Time", "within_lag_BA_or_greater",
  "within_lag_New_Job", "within_lag_New_School", "within_lag_LEX_Birth",
  "within_lag_Marr_Cohab", "within_lag_Kin_within_60_N", "within_lag_Nonkin_within_60_N",
  "within_lag_Nonkin_in_HH_N", "within_lag_Kin_in_HH_N", "within_lag_Kin_GT_Hour",
  "within_lag_Nonkin_GT_Hour", "within_web",
  "between_BA_or_greater", "between_Owns_home", "between_employed", "between_Marr_Cohab",
  "black_race", "asian_race", "other_race", "latino_eth", "female", "between_age"
)

# Function to create predictor set with specific directional variable
make_predictors <- function(base, replace_var, with_var) {
  preds <- base
  preds[preds == replace_var] <- with_var
  return(preds)
}

# Store all results
all_results <- list()

# ==============================================================================
# YOUNG GROUP - OUT OF BAY
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("YOUNG GROUP - DIRECTIONALITY SENSITIVITY (OUT OF BAY)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

df_young <- df %>% filter(agegroup == 0)

# Baseline
cat("\nBaseline model\n")
all_results$young_out_baseline <- run_mixed_model("Moved_out_of_Bay_w23", base_predictors,
                                                   df_young, "young_out_baseline")

# Ownership directionality
cat("\nRenter to Homeowner\n")
preds_r2h <- make_predictors(base_predictors, "within_lag_Owns_Home", "within_lag_Renter_to_homeowner")
all_results$young_out_renter_to_home <- run_mixed_model("Moved_out_of_Bay_w23", preds_r2h,
                                                         df_young, "young_out_renter_to_home")

cat("\nHomeowner to Renter\n")
preds_h2r <- make_predictors(base_predictors, "within_lag_Owns_Home", "within_lag_Homeowner_to_renter")
all_results$young_out_home_to_renter <- run_mixed_model("Moved_out_of_Bay_w23", preds_h2r,
                                                         df_young, "young_out_home_to_renter")

# Marriage directionality
cat("\nMarried to Divorced\n")
preds_m2d <- make_predictors(base_predictors, "within_lag_Marr_Cohab", "within_lag_Married_to_divorced")
all_results$young_out_married_to_divorced <- run_mixed_model("Moved_out_of_Bay_w23", preds_m2d,
                                                              df_young, "young_out_married_to_divorced")

cat("\nNot Married to Married\n")
preds_n2m <- make_predictors(base_predictors, "within_lag_Marr_Cohab", "lag_Not_to_Married")
all_results$young_out_not_to_married <- run_mixed_model("Moved_out_of_Bay_w23", preds_n2m,
                                                         df_young, "young_out_not_to_married")

# Employment directionality
cat("\nFulltime to Less\n")
preds_f2l <- make_predictors(base_predictors, "within_lag_Full_Time", "within_lag_Fulltime_to_less")
all_results$young_out_fulltime_to_less <- run_mixed_model("Moved_out_of_Bay_w23", preds_f2l,
                                                           df_young, "young_out_fulltime_to_less")

cat("\nLess to Fulltime\n")
preds_l2f <- make_predictors(base_predictors, "within_lag_Full_Time", "within_lag_Less_to_fulltime")
all_results$young_out_less_to_fulltime <- run_mixed_model("Moved_out_of_Bay_w23", preds_l2f,
                                                           df_young, "young_out_less_to_fulltime")

# Financial problem directionality (with extended controls)
cat("\nNot to Financial Problems\n")
preds_n2fp <- make_predictors(base_extended, "within_lag_Owns_Home", "within_lag_Owns_Home")
preds_n2fp <- c(preds_n2fp[!preds_n2fp %in% c("within_lag_LEX_Birth")], "within_lag_Not_to_FinanProb", "within_lag_LEX_Birth")
all_results$young_out_not_to_finanprob <- run_mixed_model("Moved_out_of_Bay_w23", preds_n2fp,
                                                           df_young, "young_out_not_to_finanprob")

cat("\nFinancial Problems to Not\n")
preds_fp2n <- c(base_extended[!base_extended %in% c("within_lag_LEX_Birth")], "within_lag_FinanProb_to_not", "within_lag_LEX_Birth")
all_results$young_out_finanprob_to_not <- run_mixed_model("Moved_out_of_Bay_w23", preds_fp2n,
                                                           df_young, "young_out_finanprob_to_not")

# ==============================================================================
# YOUNG GROUP - WITHIN BAY
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("YOUNG GROUP - DIRECTIONALITY SENSITIVITY (WITHIN BAY)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

df_young_within <- df %>%
  filter(agegroup == 0) %>%
  filter(Moved_out_of_Bay_w23 != 1 | is.na(Moved_out_of_Bay_w23))

# Baseline
cat("\nBaseline model\n")
all_results$young_within_baseline <- run_mixed_model("Moved_in_Bay_w23", base_predictors,
                                                      df_young_within, "young_within_baseline")

# Ownership directionality
cat("\nRenter to Homeowner\n")
all_results$young_within_renter_to_home <- run_mixed_model("Moved_in_Bay_w23", preds_r2h,
                                                            df_young_within, "young_within_renter_to_home")

cat("\nHomeowner to Renter\n")
all_results$young_within_home_to_renter <- run_mixed_model("Moved_in_Bay_w23", preds_h2r,
                                                            df_young_within, "young_within_home_to_renter")

# Marriage directionality
cat("\nMarried to Divorced\n")
all_results$young_within_married_to_divorced <- run_mixed_model("Moved_in_Bay_w23", preds_m2d,
                                                                 df_young_within, "young_within_married_to_divorced")

cat("\nNot Married to Married\n")
all_results$young_within_not_to_married <- run_mixed_model("Moved_in_Bay_w23", preds_n2m,
                                                            df_young_within, "young_within_not_to_married")

# Employment directionality
cat("\nFulltime to Less\n")
all_results$young_within_fulltime_to_less <- run_mixed_model("Moved_in_Bay_w23", preds_f2l,
                                                              df_young_within, "young_within_fulltime_to_less")

cat("\nLess to Fulltime\n")
all_results$young_within_less_to_fulltime <- run_mixed_model("Moved_in_Bay_w23", preds_l2f,
                                                              df_young_within, "young_within_less_to_fulltime")

# Financial problem directionality
cat("\nNot to Financial Problems\n")
all_results$young_within_not_to_finanprob <- run_mixed_model("Moved_in_Bay_w23", preds_n2fp,
                                                              df_young_within, "young_within_not_to_finanprob")

cat("\nFinancial Problems to Not\n")
all_results$young_within_finanprob_to_not <- run_mixed_model("Moved_in_Bay_w23", preds_fp2n,
                                                              df_young_within, "young_within_finanprob_to_not")

# ==============================================================================
# OLD GROUP - OUT OF BAY
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("OLD GROUP - DIRECTIONALITY SENSITIVITY (OUT OF BAY)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

df_old <- df %>% filter(agegroup == 1)

# Baseline
cat("\nBaseline model\n")
all_results$old_out_baseline <- run_mixed_model("Moved_out_of_Bay_w23", base_predictors,
                                                 df_old, "old_out_baseline")

# Ownership directionality
cat("\nRenter to Homeowner\n")
all_results$old_out_renter_to_home <- run_mixed_model("Moved_out_of_Bay_w23", preds_r2h,
                                                       df_old, "old_out_renter_to_home")

cat("\nHomeowner to Renter\n")
all_results$old_out_home_to_renter <- run_mixed_model("Moved_out_of_Bay_w23", preds_h2r,
                                                       df_old, "old_out_home_to_renter")

# Marriage directionality
cat("\nMarried to Divorced\n")
all_results$old_out_married_to_divorced <- run_mixed_model("Moved_out_of_Bay_w23", preds_m2d,
                                                            df_old, "old_out_married_to_divorced")

cat("\nNot Married to Married\n")
all_results$old_out_not_to_married <- run_mixed_model("Moved_out_of_Bay_w23", preds_n2m,
                                                       df_old, "old_out_not_to_married")

# Employment directionality
cat("\nFulltime to Less\n")
all_results$old_out_fulltime_to_less <- run_mixed_model("Moved_out_of_Bay_w23", preds_f2l,
                                                         df_old, "old_out_fulltime_to_less")

cat("\nLess to Fulltime\n")
all_results$old_out_less_to_fulltime <- run_mixed_model("Moved_out_of_Bay_w23", preds_l2f,
                                                         df_old, "old_out_less_to_fulltime")

# Financial problem directionality
cat("\nNot to Financial Problems\n")
all_results$old_out_not_to_finanprob <- run_mixed_model("Moved_out_of_Bay_w23", preds_n2fp,
                                                         df_old, "old_out_not_to_finanprob")

cat("\nFinancial Problems to Not\n")
all_results$old_out_finanprob_to_not <- run_mixed_model("Moved_out_of_Bay_w23", preds_fp2n,
                                                         df_old, "old_out_finanprob_to_not")

# ==============================================================================
# OLD GROUP - WITHIN BAY
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("OLD GROUP - DIRECTIONALITY SENSITIVITY (WITHIN BAY)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

df_old_within <- df %>%
  filter(agegroup == 1) %>%
  filter(Moved_out_of_Bay_w23 != 1 | is.na(Moved_out_of_Bay_w23))

# Baseline
cat("\nBaseline model\n")
all_results$old_within_baseline <- run_mixed_model("Moved_in_Bay_w23", base_predictors,
                                                    df_old_within, "old_within_baseline")

# Ownership directionality
cat("\nRenter to Homeowner\n")
all_results$old_within_renter_to_home <- run_mixed_model("Moved_in_Bay_w23", preds_r2h,
                                                          df_old_within, "old_within_renter_to_home")

cat("\nHomeowner to Renter\n")
all_results$old_within_home_to_renter <- run_mixed_model("Moved_in_Bay_w23", preds_h2r,
                                                          df_old_within, "old_within_home_to_renter")

# Marriage directionality
cat("\nMarried to Divorced\n")
all_results$old_within_married_to_divorced <- run_mixed_model("Moved_in_Bay_w23", preds_m2d,
                                                               df_old_within, "old_within_married_to_divorced")

cat("\nNot Married to Married\n")
all_results$old_within_not_to_married <- run_mixed_model("Moved_in_Bay_w23", preds_n2m,
                                                          df_old_within, "old_within_not_to_married")

# Employment directionality
cat("\nFulltime to Less\n")
all_results$old_within_fulltime_to_less <- run_mixed_model("Moved_in_Bay_w23", preds_f2l,
                                                            df_old_within, "old_within_fulltime_to_less")

cat("\nLess to Fulltime\n")
all_results$old_within_less_to_fulltime <- run_mixed_model("Moved_in_Bay_w23", preds_l2f,
                                                            df_old_within, "old_within_less_to_fulltime")

# Financial problem directionality
cat("\nNot to Financial Problems\n")
all_results$old_within_not_to_finanprob <- run_mixed_model("Moved_in_Bay_w23", preds_n2fp,
                                                            df_old_within, "old_within_not_to_finanprob")

cat("\nFinancial Problems to Not\n")
all_results$old_within_finanprob_to_not <- run_mixed_model("Moved_in_Bay_w23", preds_fp2n,
                                                            df_old_within, "old_within_finanprob_to_not")

# ------------------------------------------------------------------------------
# Save all results
# ------------------------------------------------------------------------------
saveRDS(all_results, "out/directionality_sensitivity_results.rds")
cat("\nAll results saved to out/directionality_sensitivity_results.rds\n")

# Print summary of key findings
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SUMMARY OF DIRECTIONALITY TESTS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("\nConclusions from original Stata analysis:\n")
cat("- Directionality in home ownership status matters for simultaneous variables\n")
cat("- Going from homeowner to renter is positively associated with moving\n")
cat("- However, directionality of home ownership makes no difference in lagged variables\n")
cat("- Directionality in marriage and employment does not significantly change results\n")
