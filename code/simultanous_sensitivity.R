# simultanous_sensitivity.R
# Translated from FE/do/simultanous_sensitivity.do
# Tests sensitivity of results to using contemporaneous (simultaneous) variables
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
# Define predictor sets
# ------------------------------------------------------------------------------

# Baseline: LAGGED within-person variables
baseline_predictors <- c(
  "within_lag_Moved_Town", "within_lag_Owns_Home", "within_lag_Full_Time", "within_lag_BA_or_greater",
  "within_lag_New_Job", "within_lag_New_School", "within_lag_Finan_Prob",
  "within_lag_Marr_Cohab", "within_lag_Kin_within_60_N", "within_lag_Nonkin_within_60_N",
  "within_lag_Nonkin_in_HH_N", "within_lag_Kin_in_HH_N", "within_lag_Kin_GT_Hour",
  "within_lag_Nonkin_GT_Hour", "within_web", "within_lag_Plan_Move"
)

# Between-person controls (used in random part of model)
between_controls <- c(
  "between_BA_or_greater", "between_Owns_home", "between_employed", "between_Marr_Cohab",
  "black_race", "asian_race", "other_race", "latino_eth", "female", "between_age"
)

# Combined baseline with between controls
baseline_full <- c(baseline_predictors, between_controls)

# Simultaneous: CONTEMPORANEOUS within-person variables (not lagged)
simultaneous_predictors <- c(
  "within_lag_Moved_Town",  # Keep lagged for moved town (makes sense causally)
  "within_Owns_home", "within_Full_Time", "within_BA_or_greater",
  "within_LE_NewJob", "within_LE_NewSchool", "within_LE_FinanProb",
  "within_Marr_Cohab", "within_Kin_within_60_N", "within_Nonkin_within_60_N",
  "within_Nonkin_in_HH_N", "within_Kin_in_HH_N", "within_Kin_GT_Hour",
  "within_Nonkin_GT_Hour", "within_web", "within_Plan_to_Move"
)

# Combined simultaneous with between controls
simultaneous_full <- c(simultaneous_predictors, between_controls)

# Store all results
all_results <- list()

# ==============================================================================
# YOUNG GROUP (agegroup == 0)
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("YOUNG GROUP - SIMULTANEOUS SENSITIVITY ANALYSIS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

df_young <- df %>% filter(agegroup == 0)

# --- OUT OF BAY ---
cat("\n--- OUT OF BAY ---\n")

cat("\nBaseline (Lagged): Out of Bay\n")
all_results$young_out_baseline <- run_mixed_model("Moved_out_of_Bay_w23", baseline_full,
                                                   df_young, "young_out_baseline")
if (!is.null(all_results$young_out_baseline)) {
  cat("N =", attr(all_results$young_out_baseline, "nobs"), "\n")
}

cat("\nSimultaneous: Out of Bay\n")
all_results$young_out_simultaneous <- run_mixed_model("Moved_out_of_Bay_w23", simultaneous_full,
                                                       df_young, "young_out_simultaneous")
if (!is.null(all_results$young_out_simultaneous)) {
  cat("N =", attr(all_results$young_out_simultaneous, "nobs"), "\n")
}

# --- WITHIN BAY ---
cat("\n--- WITHIN BAY ---\n")

cat("\nBaseline (Lagged): Within Bay\n")
all_results$young_within_baseline <- run_mixed_model("Moved_in_Bay_w23", baseline_full,
                                                      df_young, "young_within_baseline")
if (!is.null(all_results$young_within_baseline)) {
  cat("N =", attr(all_results$young_within_baseline, "nobs"), "\n")
}

cat("\nSimultaneous: Within Bay\n")
all_results$young_within_simultaneous <- run_mixed_model("Moved_in_Bay_w23", simultaneous_full,
                                                          df_young, "young_within_simultaneous")
if (!is.null(all_results$young_within_simultaneous)) {
  cat("N =", attr(all_results$young_within_simultaneous, "nobs"), "\n")
}

# ==============================================================================
# OLD GROUP (agegroup == 1)
# ==============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("OLD GROUP - SIMULTANEOUS SENSITIVITY ANALYSIS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

df_old <- df %>% filter(agegroup == 1)

# --- OUT OF BAY ---
cat("\n--- OUT OF BAY ---\n")

cat("\nBaseline (Lagged): Out of Bay\n")
all_results$old_out_baseline <- run_mixed_model("Moved_out_of_Bay_w23", baseline_full,
                                                 df_old, "old_out_baseline")
if (!is.null(all_results$old_out_baseline)) {
  cat("N =", attr(all_results$old_out_baseline, "nobs"), "\n")
}

cat("\nSimultaneous: Out of Bay\n")
all_results$old_out_simultaneous <- run_mixed_model("Moved_out_of_Bay_w23", simultaneous_full,
                                                     df_old, "old_out_simultaneous")
if (!is.null(all_results$old_out_simultaneous)) {
  cat("N =", attr(all_results$old_out_simultaneous, "nobs"), "\n")
}

# --- WITHIN BAY ---
cat("\n--- WITHIN BAY ---\n")

# Filter out out-of-bay movers for within-bay analysis
df_old_within <- df_old %>%
  filter(Moved_out_of_Bay_w23 != 1 | is.na(Moved_out_of_Bay_w23))

cat("\nBaseline (Lagged): Within Bay\n")
all_results$old_within_baseline <- run_mixed_model("Moved_in_Bay_w23", baseline_full,
                                                    df_old_within, "old_within_baseline")
if (!is.null(all_results$old_within_baseline)) {
  cat("N =", attr(all_results$old_within_baseline, "nobs"), "\n")
}

cat("\nSimultaneous: Within Bay\n")
all_results$old_within_simultaneous <- run_mixed_model("Moved_in_Bay_w23", simultaneous_full,
                                                        df_old_within, "old_within_simultaneous")
if (!is.null(all_results$old_within_simultaneous)) {
  cat("N =", attr(all_results$old_within_simultaneous, "nobs"), "\n")
}

# ------------------------------------------------------------------------------
# Save all results
# ------------------------------------------------------------------------------
saveRDS(all_results, "out/simultanous_sensitivity_results.rds")
cat("\nAll results saved to out/simultanous_sensitivity_results.rds\n")

# ------------------------------------------------------------------------------
# Print comparison summary
# ------------------------------------------------------------------------------
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("COMPARISON: LAGGED vs SIMULTANEOUS SPECIFICATIONS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

print_comparison <- function(baseline_result, simul_result, outcome_name) {
  if (is.null(baseline_result) || is.null(simul_result)) {
    cat("\nCannot compare - one or both models failed to run\n")
    return()
  }

  cat("\n", outcome_name, "\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")

  # Find common terms (network variables)
  network_terms <- c("Kin_within_60", "Nonkin_within_60", "Kin_in_HH", "Nonkin_in_HH",
                     "Kin_GT_Hour", "Nonkin_GT_Hour")

  for (term in network_terms) {
    baseline_row <- baseline_result[grep(term, baseline_result$term), ]
    simul_row <- simul_result[grep(term, simul_result$term), ]

    if (nrow(baseline_row) > 0 && nrow(simul_row) > 0) {
      cat(sprintf("%-25s Lagged: %7.3f%s  Simul: %7.3f%s\n",
                  term,
                  baseline_row$estimate[1], baseline_row$stars[1],
                  simul_row$estimate[1], simul_row$stars[1]))
    }
  }
}

print_comparison(all_results$young_out_baseline, all_results$young_out_simultaneous,
                 "Young - Out of Bay")
print_comparison(all_results$young_within_baseline, all_results$young_within_simultaneous,
                 "Young - Within Bay")
print_comparison(all_results$old_out_baseline, all_results$old_out_simultaneous,
                 "Old - Out of Bay")
print_comparison(all_results$old_within_baseline, all_results$old_within_simultaneous,
                 "Old - Within Bay")

cat("\n\nNote: This analysis tests whether using contemporaneous (same-period)\n")
cat("variables instead of lagged variables changes the substantive conclusions.\n")
