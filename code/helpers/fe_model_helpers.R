# fe_model_helpers.R
# Helper functions for fixed effects panel models
# Used by: 10_fe_models.Rmd

library(dplyr)
library(plm)
library(sandwich)
library(lmtest)

# ==============================================================================
# MODEL FITTING
# ==============================================================================

#' Run a fixed effects model with robust standard errors
#' @param outcome Character: outcome variable name
#' @param predictors Character vector: predictor variable names
#' @param data Data frame: analysis data
#' @param model_name Character: name for the model
#' @param id_var Character: individual identifier (default: prim_key)
#' @param time_var Character: time variable (default: WAVE)
#' @return Data frame with coefficients, robust SEs, and significance
run_fe_model <- function(outcome, predictors, data, model_name,
                         id_var = "prim_key", time_var = "WAVE") {

  # Build formula
  formula_str <- paste(outcome, "~", paste(predictors, collapse = " + "))
  formula <- as.formula(formula_str)

  # Create panel data frame
  pdata <- pdata.frame(data, index = c(id_var, time_var), drop.index = FALSE)

  # Fit fixed effects model
  model <- plm(formula, data = pdata, model = "within", effect = "individual")

  # Get robust standard errors (HC1)
  robust_vcov <- vcovHC(model, type = "HC1")
  robust_se <- sqrt(diag(robust_vcov))

  # Get coefficients and compute test statistics
  coefs <- coef(model)
  t_stats <- coefs / robust_se
  df_resid <- df.residual(model)
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
  attr(results, "nobs") <- nrow(model$model)
  attr(results, "n_individuals") <- length(unique(data[[id_var]]))
  attr(results, "model_object") <- model

  return(results)
}

#' Run a model from a specification list
#' @param spec List: model specification with name, outcome, age_group, include_plan, exclude_out_of_bay
#' @param data Data frame: full dataset
#' @param base_preds Character vector: base predictors (time-varying only)
#' @param plan_pred Character: plan predictor to add if include_plan = TRUE
#' @param age_var Character: age group variable name (default: agegroup)
#' @param exclude_var Character: variable to use for exclusion filter (default: Moved_out_of_Bay_w23)
#' @return Model results data frame
run_fe_model_from_spec <- function(spec, data, base_preds, plan_pred = NULL,
                                    age_var = "agegroup", exclude_var = "Moved_out_of_Bay_w23") {
  # Filter by age group
  model_data <- data %>% filter(.data[[age_var]] == spec$age_group)

  # Exclude out-of-bay movers if needed (for within-bay models)
  if (isTRUE(spec$exclude_out_of_bay)) {
    model_data <- model_data %>%
      filter(.data[[exclude_var]] != 1 | is.na(.data[[exclude_var]]))
  }

  # Build predictor list
  predictors <- if (isTRUE(spec$include_plan) && !is.null(plan_pred)) {
    c(base_preds, plan_pred)
  } else {
    base_preds
  }

  # Run model
  age_label <- if (spec$age_group == 0) "Young" else "Old"
  cat(sprintf("\nRunning: %s (%s)\n", spec$name, age_label))

  results <- run_fe_model(spec$outcome, predictors, model_data, spec$name)
  cat("N =", attr(results, "nobs"), "observations,",
      attr(results, "n_individuals"), "individuals\n")

  return(results)
}

#' Run all models from a list of specifications
#' @param model_specs List of model specification lists
#' @param data Data frame: full dataset
#' @param base_preds Character vector: base predictors (time-varying only)
#' @param plan_pred Character: plan predictor (optional)
#' @return Named list of model results
run_all_fe_models <- function(model_specs, data, base_preds, plan_pred = NULL) {
  results <- lapply(model_specs, function(spec) {
    run_fe_model_from_spec(spec, data, base_preds, plan_pred)
  })
  names(results) <- sapply(model_specs, `[[`, "name")
  return(results)
}

# ==============================================================================
# EXPORT FUNCTIONS
# ==============================================================================

#' Format model results for export
#' @param results Model results data frame
#' @param model_label Label for column names
#' @return Formatted data frame for export
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
#' @param result_list List of model results data frames
#' @param labels Character vector of labels for each model
#' @return Combined data frame ready for export
combine_results <- function(result_list, labels) {
  combined <- format_for_export(result_list[[1]], labels[1])

  if (length(result_list) > 1) {
    for (i in 2:length(result_list)) {
      combined <- full_join(combined,
                            format_for_export(result_list[[i]], labels[i]),
                            by = "term")
    }
  }

  # Add observation counts
  obs_row <- data.frame(term = "Observations", stringsAsFactors = FALSE)
  indiv_row <- data.frame(term = "Individuals", stringsAsFactors = FALSE)
  for (i in seq_along(labels)) {
    obs_row[[paste0(labels[i], "_coef")]] <- as.character(attr(result_list[[i]], "nobs"))
    obs_row[[paste0(labels[i], "_se")]] <- ""
    indiv_row[[paste0(labels[i], "_coef")]] <- as.character(attr(result_list[[i]], "n_individuals"))
    indiv_row[[paste0(labels[i], "_se")]] <- ""
  }

  bind_rows(combined, obs_row, indiv_row)
}

#' Export results to CSV
#' @param result_list List of model results to combine
#' @param labels Labels for each model
#' @param filename Output filename (relative to working directory)
#' @param out_dir Output directory (default: "out")
export_fe_results <- function(result_list, labels, filename, out_dir = "out") {
  combined <- combine_results(result_list, labels)
  filepath <- file.path(out_dir, filename)
  write.csv(combined, filepath, row.names = FALSE)
  cat(sprintf("Saved: %s\n", filepath))
  return(invisible(combined))
}

# ==============================================================================
# PRINTING FUNCTIONS
# ==============================================================================

#' Print model summary to console
#' @param results Model results data frame
#' @param title Title for the summary
print_fe_summary <- function(results, title) {
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
  cat("N =", attr(results, "nobs"), "observations,",
      attr(results, "n_individuals"), "individuals\n")
  cat("Significance: * p<0.10, ** p<0.05, *** p<0.01\n")
}

#' Print all model summaries
#' @param all_results Named list of model results
print_all_fe_summaries <- function(all_results) {
  for (name in names(all_results)) {
    print_fe_summary(all_results[[name]], name)
  }
}

# ==============================================================================
# MODEL OBJECT EXTRACTION
# ==============================================================================

#' Extract model objects from results list
#' @param all_results Named list of model results
#' @return Named list of plm model objects
extract_fe_model_objects <- function(all_results) {
  lapply(all_results, function(x) attr(x, "model_object"))
}
