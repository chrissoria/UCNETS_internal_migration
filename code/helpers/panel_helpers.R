# panel_helpers.R
# Helper functions for panel data processing
# Used by: 01_create_long_variables.Rmd, 02_create_long_decomposition.Rmd

library(dplyr)

# ==============================================================================
# LAGGED VARIABLE CREATION
# ==============================================================================

#' Create lagged variables for panel data
#' @param df Data frame with panel structure
#' @param var_specs Named list where names are new lag variable names and values are source variable names
#' @param id_var Panel ID variable (default: prim_key)
#' @param time_var Time variable for ordering (default: WAVE)
#' @return Data frame with lagged variables added
create_lagged_vars <- function(df, var_specs, id_var = "prim_key", time_var = "WAVE") {
  df <- df %>%
    group_by(across(all_of(id_var)))

  for (lag_name in names(var_specs)) {
    source_var <- var_specs[[lag_name]]
    df <- df %>%
      mutate(!!lag_name := lag(.data[[source_var]], order_by = .data[[time_var]]))
  }

  df <- df %>% ungroup()
  return(df)
}

# ==============================================================================
# CUMULATIVE VARIABLE WITH DECAY
# ==============================================================================

#' Create cumulative variable with decay
#' Accumulates events but decays by 1 each period if event doesn't recur
#' @param x Numeric vector of events (1 = event occurred, 0 = no event)
#' @return Numeric vector of cumulative values with decay
create_cumulative <- function(x) {
  n <- length(x)
  result <- numeric(n)
  if (n == 0) return(result)

  result[1] <- if (is.na(x[1])) 0 else x[1]

  if (n > 1) {
    for (i in 2:n) {
      if (is.na(x[i]) || x[i] == 0) {
        result[i] <- max(0, result[i-1] - 1)
      } else {
        result[i] <- result[i-1] + x[i]
      }
    }
  }
  return(result)
}

#' Create lagged cumulative variable with wave 3 adjustment
#' @param df Data frame
#' @param var_name Name of cumulative variable to lag
#' @param id_var Panel ID variable (default: prim_key)
#' @param time_var Time variable (default: WAVE)
#' @return Data frame with lagged cumulative variable added
create_lagged_cumulative <- function(df, var_name, id_var = "prim_key", time_var = "WAVE") {
  lag_var <- paste0("lag_", var_name)

  df <- df %>%
    group_by(across(all_of(id_var))) %>%
    mutate(!!lag_var := lag(.data[[var_name]], order_by = .data[[time_var]])) %>%
    ungroup()

  # Check if any individual has lag value of 2 at wave 3 and adjust
  df <- df %>%
    group_by(across(all_of(id_var))) %>%
    mutate(
      has_lag_2 = any(.data[[lag_var]] == 2 & .data[[time_var]] == 3, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(!!lag_var := if_else(has_lag_2 == TRUE,
                                 .data[[lag_var]] - 1,
                                 .data[[lag_var]])) %>%
    select(-has_lag_2)

  return(df)
}

# ==============================================================================
# TRANSITION VARIABLES
# ==============================================================================

#' Create transition variable (change from one state to another)
#' @param current Current value
#' @param lagged Lagged value
#' @param from_val Value transitioning from
#' @param to_val Value transitioning to
#' @return 1 if transition occurred, 0 if not, NA if either value is missing
create_transition <- function(current, lagged, from_val, to_val) {
  case_when(
    is.na(current) | is.na(lagged) ~ NA_real_,
    lagged == from_val & current == to_val ~ 1,
    TRUE ~ 0
  )
}

# ==============================================================================
# BETWEEN/WITHIN DECOMPOSITION
# ==============================================================================

#' Create between-person mean and within-person deviation for a variable
#' @param df Data frame
#' @param var_name Variable name to decompose
#' @param id_var Panel ID variable (default: prim_key)
#' @return Data frame with between_ and within_ versions of variable added
create_between_within <- function(df, var_name, id_var = "prim_key") {
  between_name <- paste0("between_", var_name)
  within_name <- paste0("within_", var_name)

  df <- df %>%
    group_by(across(all_of(id_var))) %>%
    mutate(
      !!between_name := mean(.data[[var_name]], na.rm = TRUE),
      !!within_name := .data[[var_name]] - mean(.data[[var_name]], na.rm = TRUE)
    ) %>%
    ungroup()

  return(df)
}

#' Create between/within decomposition for multiple variables
#' @param df Data frame
#' @param var_names Character vector of variable names
#' @param id_var Panel ID variable (default: prim_key)
#' @return Data frame with decomposition for all specified variables
create_between_within_multi <- function(df, var_names, id_var = "prim_key") {
  for (var in var_names) {
    if (var %in% names(df)) {
      df <- create_between_within(df, var, id_var)
    }
  }
  return(df)
}

# ==============================================================================
# TIME-INVARIANT VARIABLE EXPANSION
# ==============================================================================

#' Expand time-invariant variable across all waves
#' Takes first non-NA value for each individual and applies to all waves
#' @param df Data frame
#' @param source_var Source variable name
#' @param target_var Target variable name (default: same as source)
#' @param id_var Panel ID variable (default: prim_key)
#' @return Data frame with expanded variable
expand_time_invariant <- function(df, source_var, target_var = NULL, id_var = "prim_key") {
  if (is.null(target_var)) target_var <- source_var

  df <- df %>%
    mutate(!!target_var := .data[[source_var]]) %>%
    group_by(across(all_of(id_var))) %>%
    mutate(!!target_var := first(na.omit(.data[[target_var]]))) %>%
    ungroup()

  return(df)
}

# ==============================================================================
# MERGE HELPERS
# ==============================================================================

#' Safe left join with validation
#' @param left_df Left data frame
#' @param right_df Right data frame
#' @param by Join key
#' @param check_unmatched If TRUE, warn about unmatched records in right df
#' @return Merged data frame
safe_left_join <- function(left_df, right_df, by, check_unmatched = TRUE) {
  n_before <- nrow(left_df)

  if (check_unmatched) {
    unmatched <- right_df %>%
      filter(!(!!sym(by) %in% left_df[[by]]))

    if (nrow(unmatched) > 0) {
      warning(sprintf("%d records in right dataset have no match in left dataset", nrow(unmatched)))
    }
  }

  result <- left_df %>%
    left_join(right_df, by = by)

  stopifnot(nrow(result) == n_before)

  return(result)
}
