# simultanous_directionality_sensitivity.R
# Translated from FE/do/simultanous_directionality_sensitivity.do
# Tests directionality of status changes using CONTEMPORANEOUS (simultaneous) variables
# instead of lagged variables

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
# Define base predictor sets (CONTEMPORANEOUS - no lag)
# ------------------------------------------------------------------------------

# Base with standard ownership
base_owns <- c(
  "within_Owns_home", "within_Full_Time", "within_BA_or_greater",
  "within_LE_NewJob", "within_LE_NewSchool", "within_LE_FinanProb",
  "within_Marr_Cohab", "within_Kin_within_60_N", "within_Nonkin_within_60_N",
  "within_Nonkin_in_HH_N", "within_Kin_in_HH_N", "within_Kin_GT_Hour",
  "within_Nonkin_GT_Hour", "within_web", "within_Plan_to_Move"
)

# Base with renter to homeowner
base_r2h <- c(
  "within_Renter_to_homeowner", "within_Full_Time", "within_BA_or_greater",
  "within_LE_NewJob", "within_LE_NewSchool", "within_LE_FinanProb",
  "within_Marr_Cohab", "within_Kin_within_60_N", "within_Nonkin_within_60_N",
  "within_Nonkin_in_HH_N", "within_Kin_in_HH_N", "within_Kin_GT_Hour",
  "within_Nonkin_GT_Hour", "within_web", "within_Plan_to_Move"
)

# Base with homeowner to renter
base_h2r <- c(
  "within_Homeowner_to_renter", "within_Full_Time", "within_BA_or_greater",
  "within_LE_NewJob", "within_LE_NewSchool", "within_LE_FinanProb",
  "within_Marr_Cohab", "within_Kin_within_60_N", "within_Nonkin_within_60_N",
  "within_Nonkin_in_HH_N", "within_Kin_in_HH_N", "within_Kin_GT_Hour",
  "within_Nonkin_GT_Hour", "within_web", "within_Plan_to_Move"
)

# Married to divorced (with renter to homeowner for ownership)
base_m2d <- c(
  "within_Renter_to_homeowner", "within_Full_Time", "within_BA_or_greater",
  "within_LE_NewJob", "within_LE_NewSchool", "within_LE_FinanProb",
  "within_Married_to_divorced", "within_Kin_within_60_N", "within_Nonkin_within_60_N",
  "within_Nonkin_in_HH_N", "within_Kin_in_HH_N", "within_Kin_GT_Hour",
  "within_Nonkin_GT_Hour", "within_web", "within_Plan_to_Move"
)

# Not to married
base_n2m <- c(
  "within_Owns_home", "within_Full_Time", "within_BA_or_greater",
  "within_LE_NewJob", "within_LE_NewSchool", "within_LE_FinanProb",
  "within_Not_to_Married", "within_Kin_within_60_N", "within_Nonkin_within_60_N",
  "within_Nonkin_in_HH_N", "within_Kin_in_HH_N", "within_Kin_GT_Hour",
  "within_Nonkin_GT_Hour", "within_web", "within_Plan_to_Move"
)

# Fulltime to less
base_f2l <- c(
  "within_Owns_home", "within_Fulltime_to_less", "within_BA_or_greater",
  "within_LE_NewJob", "within_LE_NewSchool", "within_LE_FinanProb",
  "within_Marr_Cohab", "within_Kin_within_60_N", "within_Nonkin_within_60_N",
  "within_Nonkin_in_HH_N", "within_Kin_in_HH_N", "within_Kin_GT_Hour",
  "within_Nonkin_GT_Hour", "within_web", "within_Plan_to_Move"
)

# Less to fulltime
base_l2f <- c(
  "within_Owns_home", "within_Less_to_fulltime", "within_BA_or_greater",
  "within_LE_NewJob", "within_LE_NewSchool", "within_LE_FinanProb",
  "within_Marr_Cohab", "within_Kin_within_60_N", "within_Nonkin_within_60_N",
  "within_Nonkin_in_HH_N", "within_Kin_in_HH_N", "within_Kin_GT_Hour",
  "within_Nonkin_GT_Hour", "within_web", "within_Plan_to_Move"
)

# Financial problems to not
base_fp2n <- c(
  "within_Owns_home", "within_Full_Time", "within_BA_or_greater",
  "within_LE_NewJob", "within_LE_NewSchool", "within_FinanProb_to_not",
  "within_Marr_Cohab", "within_Kin_within_60_N", "within_Nonkin_within_60_N",
  "within_Nonkin_in_HH_N", "within_Kin_in_HH_N", "within_Kin_GT_Hour",
  "within_Nonkin_GT_Hour", "within_web", "within_Plan_to_Move"
)

# Not to financial problems
base_n2fp <- c(
  "within_Owns_home", "within_Full_Time", "within_BA_or_greater",
  "within_LE_NewJob", "within_LE_NewSchool", "within_Not_to_FinanProb",
  "within_Marr_Cohab", "within_Kin_within_60_N", "within_Nonkin_within_60_N",
  "within_Nonkin_in_HH_N", "within_Kin_in_HH_N", "within_Kin_GT_Hour",
  "within_Nonkin_GT_Hour", "within_web", "within_Plan_to_Move"
)

# Store all results
all_results <- list()

# ==============================================================================
# YOUNG GROUP - OUT OF BAY (agegroup == 0)
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("YOUNG GROUP - SIMULTANEOUS DIRECTIONALITY (OUT OF BAY)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

df_young <- df %>% filter(agegroup == 0)

cat("\nRenter to Homeowner\n")
all_results$young_out_r2h <- run_mixed_model("Moved_out_of_Bay_w23", base_r2h,
                                              df_young, "young_out_r2h")

cat("\nHomeowner to Renter\n")
all_results$young_out_h2r <- run_mixed_model("Moved_out_of_Bay_w23", base_h2r,
                                              df_young, "young_out_h2r")

cat("\nMarried to Divorced\n")
all_results$young_out_m2d <- run_mixed_model("Moved_out_of_Bay_w23", base_m2d,
                                              df_young, "young_out_m2d")

cat("\nNot to Married\n")
all_results$young_out_n2m <- run_mixed_model("Moved_out_of_Bay_w23", base_n2m,
                                              df_young, "young_out_n2m")

cat("\nFulltime to Less\n")
all_results$young_out_f2l <- run_mixed_model("Moved_out_of_Bay_w23", base_f2l,
                                              df_young, "young_out_f2l")

cat("\nLess to Fulltime\n")
all_results$young_out_l2f <- run_mixed_model("Moved_out_of_Bay_w23", base_l2f,
                                              df_young, "young_out_l2f")

cat("\nFinancial Problems to Not\n")
all_results$young_out_fp2n <- run_mixed_model("Moved_out_of_Bay_w23", base_fp2n,
                                               df_young, "young_out_fp2n")

cat("\nNot to Financial Problems\n")
all_results$young_out_n2fp <- run_mixed_model("Moved_out_of_Bay_w23", base_n2fp,
                                               df_young, "young_out_n2fp")

# ==============================================================================
# YOUNG GROUP - WITHIN BAY
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("YOUNG GROUP - SIMULTANEOUS DIRECTIONALITY (WITHIN BAY)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

df_young_within <- df %>%
  filter(agegroup == 0) %>%
  filter(Moved_out_of_Bay_w23 != 1 | is.na(Moved_out_of_Bay_w23))

cat("\nBaseline with Owns_home\n")
all_results$young_within_baseline <- run_mixed_model("Moved_in_Bay_w23", base_owns,
                                                      df_young_within, "young_within_baseline")

cat("\nRenter to Homeowner\n")
all_results$young_within_r2h <- run_mixed_model("Moved_in_Bay_w23", base_r2h,
                                                 df_young_within, "young_within_r2h")

cat("\nHomeowner to Renter\n")
all_results$young_within_h2r <- run_mixed_model("Moved_in_Bay_w23", base_h2r,
                                                 df_young_within, "young_within_h2r")

cat("\nMarried to Divorced\n")
all_results$young_within_m2d <- run_mixed_model("Moved_in_Bay_w23", base_m2d,
                                                 df_young_within, "young_within_m2d")

cat("\nNot to Married\n")
all_results$young_within_n2m <- run_mixed_model("Moved_in_Bay_w23", base_n2m,
                                                 df_young_within, "young_within_n2m")

cat("\nFulltime to Less\n")
all_results$young_within_f2l <- run_mixed_model("Moved_in_Bay_w23", base_f2l,
                                                 df_young_within, "young_within_f2l")

cat("\nLess to Fulltime\n")
all_results$young_within_l2f <- run_mixed_model("Moved_in_Bay_w23", base_l2f,
                                                 df_young_within, "young_within_l2f")

cat("\nFinancial Problems to Not\n")
all_results$young_within_fp2n <- run_mixed_model("Moved_in_Bay_w23", base_fp2n,
                                                  df_young_within, "young_within_fp2n")

cat("\nNot to Financial Problems\n")
all_results$young_within_n2fp <- run_mixed_model("Moved_in_Bay_w23", base_n2fp,
                                                  df_young_within, "young_within_n2fp")

# Note: Old group models are commented out in the original Stata file
# Uncomment below if you want to run them

# ==============================================================================
# OLD GROUP - OUT OF BAY (commented out in original)
# ==============================================================================
# cat("\n", paste(rep("=", 70), collapse = ""), "\n")
# cat("OLD GROUP - SIMULTANEOUS DIRECTIONALITY (OUT OF BAY)\n")
# cat(paste(rep("=", 70), collapse = ""), "\n")
#
# df_old <- df %>% filter(agegroup == 1)
# ... (similar structure as young group)

# ==============================================================================
# OLD GROUP - WITHIN BAY (commented out in original)
# ==============================================================================
# df_old_within <- df %>%
#   filter(agegroup == 1) %>%
#   filter(Moved_out_of_Bay_w23 != 1 | is.na(Moved_out_of_Bay_w23))
# ... (similar structure as young group)

# ------------------------------------------------------------------------------
# Save all results
# ------------------------------------------------------------------------------
saveRDS(all_results, "out/simultanous_directionality_sensitivity_results.rds")
cat("\nAll results saved to out/simultanous_directionality_sensitivity_results.rds\n")

# Print summary
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SUMMARY\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("\nThis analysis tests whether directionality of status changes matters\n")
cat("when using CONTEMPORANEOUS (same-period) variables instead of lagged.\n")
cat("\nKey finding from original analysis:\n")
cat("- Going from homeowner to renter is positively associated with moving\n")
cat("  in the simultaneous specification\n")
