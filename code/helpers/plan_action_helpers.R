# plan_action_helpers.R
# Helper functions for analyzing the gap between planning to move and actually moving
# Used by: 06_plan_action_gap.Rmd

library(dplyr)
library(tidyr)

# Source the base descriptive helpers for formatting functions
source(here::here("code", "helpers", "descriptive_helpers.R"))

# ==============================================================================
# FOLLOW-THROUGH TABLE: Among planners, who actually moves?
# ==============================================================================

#' Create table comparing planners who moved vs. planners who didn't move
#' @param df Data frame
#' @param vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of continuous variable names
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @param move_var Name of move variable (default: "Moved_w23")
#' @return Data frame comparing characteristics of planners who moved vs. didn't move
create_follow_through_table <- function(df, vars, continuous_vars = c("age"),
                                         id_var = "prim_key", age_var = "agegroup",
                                         plan_var = "Plan_to_Move", move_var = "Moved_w23") {

  # Get all var names
  all_vars <- unlist(vars)

  # Collapse to person level
  person_df <- df %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      ever_planned = as.integer(any(.data[[plan_var]] == 1, na.rm = TRUE)),
      ever_moved = as.integer(any(.data[[move_var]] == 1, na.rm = TRUE)),
      .groups = "drop"
    )

  # Add each variable
  for (var in all_vars) {
    if (var %in% continuous_vars) {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    } else {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := max(.data[[var]], na.rm = TRUE), .groups = "drop")
    }
    person_df <- left_join(person_df, var_summary, by = id_var)
  }

  # Filter to only those who ever planned to move
  planners <- person_df %>% filter(ever_planned == 1)

  results <- data.frame(Variable = names(vars), stringsAsFactors = FALSE)

  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_planners <- planners %>% filter(agegroup == age_val)

    moved <- age_planners %>% filter(ever_moved == 1)
    didnt_move <- age_planners %>% filter(ever_moved == 0)

    moved_col <- paste0(age_label, "_Moved")
    didnt_col <- paste0(age_label, "_NoMove")

    results[[moved_col]] <- NA_character_
    results[[didnt_col]] <- NA_character_

    for (i in seq_along(vars)) {
      var_name <- vars[[i]]

      if (var_name %in% continuous_vars) {
        results[[moved_col]][i] <- format_mean_sd(moved[[var_name]])
        results[[didnt_col]][i] <- format_mean_sd(didnt_move[[var_name]])
      } else {
        n_moved <- sum(moved[[var_name]] == 1, na.rm = TRUE)
        n_didnt <- sum(didnt_move[[var_name]] == 1, na.rm = TRUE)

        results[[moved_col]][i] <- format_n_pct(n_moved, nrow(moved))
        results[[didnt_col]][i] <- format_n_pct(n_didnt, nrow(didnt_move))
      }
    }
  }

  # Add N row
  n_row <- data.frame(Variable = "N", stringsAsFactors = FALSE)
  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_planners <- planners %>% filter(agegroup == age_val)

    n_moved <- sum(age_planners$ever_moved == 1, na.rm = TRUE)
    n_didnt <- sum(age_planners$ever_moved == 0, na.rm = TRUE)

    n_row[[paste0(age_label, "_Moved")]] <- as.character(n_moved)
    n_row[[paste0(age_label, "_NoMove")]] <- as.character(n_didnt)
  }

  bind_rows(results, n_row)
}

# ==============================================================================
# UNPLANNED MOVERS TABLE: Who moves without planning?
# ==============================================================================

#' Create table comparing unplanned movers vs. planned movers
#' @param df Data frame
#' @param vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of continuous variable names
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @param move_var Name of move variable (default: "Moved_w23")
#' @return Data frame comparing unplanned movers vs. planned movers
create_unplanned_movers_table <- function(df, vars, continuous_vars = c("age"),
                                           id_var = "prim_key", age_var = "agegroup",
                                           plan_var = "Plan_to_Move", move_var = "Moved_w23") {

  # Get all var names
  all_vars <- unlist(vars)

  # Collapse to person level
  person_df <- df %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      ever_planned = as.integer(any(.data[[plan_var]] == 1, na.rm = TRUE)),
      ever_moved = as.integer(any(.data[[move_var]] == 1, na.rm = TRUE)),
      .groups = "drop"
    )

  # Add each variable
  for (var in all_vars) {
    if (var %in% continuous_vars) {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    } else {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := max(.data[[var]], na.rm = TRUE), .groups = "drop")
    }
    person_df <- left_join(person_df, var_summary, by = id_var)
  }

  # Filter to only movers
  movers <- person_df %>% filter(ever_moved == 1)

  results <- data.frame(Variable = names(vars), stringsAsFactors = FALSE)

  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_movers <- movers %>% filter(agegroup == age_val)

    unplanned <- age_movers %>% filter(ever_planned == 0)
    planned <- age_movers %>% filter(ever_planned == 1)

    unplanned_col <- paste0(age_label, "_Unplanned")
    planned_col <- paste0(age_label, "_Planned")

    results[[unplanned_col]] <- NA_character_
    results[[planned_col]] <- NA_character_

    for (i in seq_along(vars)) {
      var_name <- vars[[i]]

      if (var_name %in% continuous_vars) {
        results[[unplanned_col]][i] <- format_mean_sd(unplanned[[var_name]])
        results[[planned_col]][i] <- format_mean_sd(planned[[var_name]])
      } else {
        n_unplanned <- sum(unplanned[[var_name]] == 1, na.rm = TRUE)
        n_planned <- sum(planned[[var_name]] == 1, na.rm = TRUE)

        results[[unplanned_col]][i] <- format_n_pct(n_unplanned, nrow(unplanned))
        results[[planned_col]][i] <- format_n_pct(n_planned, nrow(planned))
      }
    }
  }

  # Add N row
  n_row <- data.frame(Variable = "N", stringsAsFactors = FALSE)
  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_movers <- movers %>% filter(agegroup == age_val)

    n_unplanned <- sum(age_movers$ever_planned == 0, na.rm = TRUE)
    n_planned <- sum(age_movers$ever_planned == 1, na.rm = TRUE)

    n_row[[paste0(age_label, "_Unplanned")]] <- as.character(n_unplanned)
    n_row[[paste0(age_label, "_Planned")]] <- as.character(n_planned)
  }

  bind_rows(results, n_row)
}

# ==============================================================================
# FOUR-WAY TABLE: All combinations of plan x action
# ==============================================================================

#' Create table comparing all four plan-action combinations
#' @param df Data frame
#' @param vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of continuous variable names
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @param move_var Name of move variable (default: "Moved_w23")
#' @return Data frame comparing all four plan-action groups
create_four_way_table <- function(df, vars, continuous_vars = c("age"),
                                   id_var = "prim_key", age_var = "agegroup",
                                   plan_var = "Plan_to_Move", move_var = "Moved_w23") {

  # Get all var names
  all_vars <- unlist(vars)

  # Collapse to person level
  person_df <- df %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      ever_planned = as.integer(any(.data[[plan_var]] == 1, na.rm = TRUE)),
      ever_moved = as.integer(any(.data[[move_var]] == 1, na.rm = TRUE)),
      .groups = "drop"
    )

  # Add each variable
  for (var in all_vars) {
    if (var %in% continuous_vars) {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    } else {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := max(.data[[var]], na.rm = TRUE), .groups = "drop")
    }
    person_df <- left_join(person_df, var_summary, by = id_var)
  }

  # Create four groups
  person_df <- person_df %>%
    mutate(
      group = case_when(
        ever_planned == 0 & ever_moved == 0 ~ "NoPlanned_NoMoved",
        ever_planned == 1 & ever_moved == 0 ~ "Planned_NoMoved",
        ever_planned == 0 & ever_moved == 1 ~ "NoPlanned_Moved",
        ever_planned == 1 & ever_moved == 1 ~ "Planned_Moved",
        TRUE ~ NA_character_
      )
    )

  results <- data.frame(Variable = names(vars), stringsAsFactors = FALSE)

  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- person_df %>% filter(agegroup == age_val)

    # Four groups
    no_plan_no_move <- age_data %>% filter(group == "NoPlanned_NoMoved")
    plan_no_move <- age_data %>% filter(group == "Planned_NoMoved")
    no_plan_move <- age_data %>% filter(group == "NoPlanned_Moved")
    plan_move <- age_data %>% filter(group == "Planned_Moved")

    col1 <- paste0(age_label, "_NoPlan_NoMove")
    col2 <- paste0(age_label, "_Plan_NoMove")
    col3 <- paste0(age_label, "_NoPlan_Move")
    col4 <- paste0(age_label, "_Plan_Move")

    results[[col1]] <- NA_character_
    results[[col2]] <- NA_character_
    results[[col3]] <- NA_character_
    results[[col4]] <- NA_character_

    for (i in seq_along(vars)) {
      var_name <- vars[[i]]

      if (var_name %in% continuous_vars) {
        results[[col1]][i] <- format_mean_sd(no_plan_no_move[[var_name]])
        results[[col2]][i] <- format_mean_sd(plan_no_move[[var_name]])
        results[[col3]][i] <- format_mean_sd(no_plan_move[[var_name]])
        results[[col4]][i] <- format_mean_sd(plan_move[[var_name]])
      } else {
        results[[col1]][i] <- format_n_pct(sum(no_plan_no_move[[var_name]] == 1, na.rm = TRUE), nrow(no_plan_no_move))
        results[[col2]][i] <- format_n_pct(sum(plan_no_move[[var_name]] == 1, na.rm = TRUE), nrow(plan_no_move))
        results[[col3]][i] <- format_n_pct(sum(no_plan_move[[var_name]] == 1, na.rm = TRUE), nrow(no_plan_move))
        results[[col4]][i] <- format_n_pct(sum(plan_move[[var_name]] == 1, na.rm = TRUE), nrow(plan_move))
      }
    }
  }

  # Add N row
  n_row <- data.frame(Variable = "N", stringsAsFactors = FALSE)
  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- person_df %>% filter(agegroup == age_val)

    n_row[[paste0(age_label, "_NoPlan_NoMove")]] <- as.character(sum(age_data$group == "NoPlanned_NoMoved", na.rm = TRUE))
    n_row[[paste0(age_label, "_Plan_NoMove")]] <- as.character(sum(age_data$group == "Planned_NoMoved", na.rm = TRUE))
    n_row[[paste0(age_label, "_NoPlan_Move")]] <- as.character(sum(age_data$group == "NoPlanned_Moved", na.rm = TRUE))
    n_row[[paste0(age_label, "_Plan_Move")]] <- as.character(sum(age_data$group == "Planned_Moved", na.rm = TRUE))
  }

  bind_rows(results, n_row)
}

# ==============================================================================
# STUCK PLANNERS TABLE: Planned but didn't move vs. Never planned
# ==============================================================================

#' Create table comparing stuck planners (planned but didn't move) vs. never planners
#' @param df Data frame
#' @param vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of continuous variable names
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @param move_var Name of move variable (default: "Moved_w23")
#' @return Data frame comparing stuck planners vs. never planners
create_stuck_planners_table <- function(df, vars, continuous_vars = c("age"),
                                         id_var = "prim_key", age_var = "agegroup",
                                         plan_var = "Plan_to_Move", move_var = "Moved_w23") {

  # Get all var names
  all_vars <- unlist(vars)

  # Collapse to person level
  person_df <- df %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      ever_planned = as.integer(any(.data[[plan_var]] == 1, na.rm = TRUE)),
      ever_moved = as.integer(any(.data[[move_var]] == 1, na.rm = TRUE)),
      .groups = "drop"
    )

  # Add each variable
  for (var in all_vars) {
    if (var %in% continuous_vars) {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    } else {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := max(.data[[var]], na.rm = TRUE), .groups = "drop")
    }
    person_df <- left_join(person_df, var_summary, by = id_var)
  }

  results <- data.frame(Variable = names(vars), stringsAsFactors = FALSE)

  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- person_df %>% filter(agegroup == age_val)

    # Stuck planners: planned but didn't move
    stuck <- age_data %>% filter(ever_planned == 1 & ever_moved == 0)
    # Never planned
    never_planned <- age_data %>% filter(ever_planned == 0)

    stuck_col <- paste0(age_label, "_Stuck")
    never_col <- paste0(age_label, "_NeverPlan")

    results[[stuck_col]] <- NA_character_
    results[[never_col]] <- NA_character_

    for (i in seq_along(vars)) {
      var_name <- vars[[i]]

      if (var_name %in% continuous_vars) {
        results[[stuck_col]][i] <- format_mean_sd(stuck[[var_name]])
        results[[never_col]][i] <- format_mean_sd(never_planned[[var_name]])
      } else {
        n_stuck <- sum(stuck[[var_name]] == 1, na.rm = TRUE)
        n_never <- sum(never_planned[[var_name]] == 1, na.rm = TRUE)

        results[[stuck_col]][i] <- format_n_pct(n_stuck, nrow(stuck))
        results[[never_col]][i] <- format_n_pct(n_never, nrow(never_planned))
      }
    }
  }

  # Add N row
  n_row <- data.frame(Variable = "N", stringsAsFactors = FALSE)
  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- person_df %>% filter(agegroup == age_val)

    n_stuck <- sum(age_data$ever_planned == 1 & age_data$ever_moved == 0, na.rm = TRUE)
    n_never <- sum(age_data$ever_planned == 0, na.rm = TRUE)

    n_row[[paste0(age_label, "_Stuck")]] <- as.character(n_stuck)
    n_row[[paste0(age_label, "_NeverPlan")]] <- as.character(n_never)
  }

  bind_rows(results, n_row)
}

# ==============================================================================
# PLANNING RATES BY DEMOGRAPHIC GROUP
# ==============================================================================

#' Create table showing planning rates by demographic group (with complement)
#' @param df Data frame
#' @param vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of continuous variable names
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @return Data frame with planning rates and non-planning rates by group
create_planning_rates_by_group <- function(df, vars, continuous_vars = c("age"),
                                           id_var = "prim_key", age_var = "agegroup",
                                           plan_var = "Plan_to_Move") {

  # Get all var names
  all_vars <- unlist(vars)

  # Collapse to person level
  person_df <- df %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      ever_planned = as.integer(any(.data[[plan_var]] == 1, na.rm = TRUE)),
      .groups = "drop"
    )

  # Add each variable
  for (var in all_vars) {
    if (var %in% continuous_vars) {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    } else {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := max(.data[[var]], na.rm = TRUE), .groups = "drop")
    }
    person_df <- left_join(person_df, var_summary, by = id_var)
  }

  results <- data.frame(Variable = names(vars), stringsAsFactors = FALSE)

  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- person_df %>% filter(agegroup == age_val)

    plan_col <- paste0(age_label, "_Planned")
    no_plan_col <- paste0(age_label, "_NeverPlanned")

    results[[plan_col]] <- NA_character_
    results[[no_plan_col]] <- NA_character_

    for (i in seq_along(vars)) {
      var_name <- vars[[i]]

      if (var_name %in% continuous_vars) {
        # For continuous: split by planning status and show mean
        planners <- age_data %>% filter(ever_planned == 1)
        non_planners <- age_data %>% filter(ever_planned == 0)

        results[[plan_col]][i] <- format_mean_sd(planners[[var_name]])
        results[[no_plan_col]][i] <- format_mean_sd(non_planners[[var_name]])
      } else {
        # For binary: show % who planned among this demographic group
        group_data <- age_data %>% filter(.data[[var_name]] == 1)
        n_total_in_group <- nrow(group_data)
        n_planned_in_group <- sum(group_data$ever_planned == 1, na.rm = TRUE)
        n_not_planned_in_group <- n_total_in_group - n_planned_in_group

        results[[plan_col]][i] <- format_pct(n_planned_in_group / n_total_in_group)
        results[[no_plan_col]][i] <- format_pct(n_not_planned_in_group / n_total_in_group)
      }
    }
  }

  # Total columns
  results$Total_Planned <- NA_character_
  results$Total_NeverPlanned <- NA_character_

  for (i in seq_along(vars)) {
    var_name <- vars[[i]]

    if (var_name %in% continuous_vars) {
      planners <- person_df %>% filter(ever_planned == 1)
      non_planners <- person_df %>% filter(ever_planned == 0)

      results$Total_Planned[i] <- format_mean_sd(planners[[var_name]])
      results$Total_NeverPlanned[i] <- format_mean_sd(non_planners[[var_name]])
    } else {
      group_data <- person_df %>% filter(.data[[var_name]] == 1)
      n_total_in_group <- nrow(group_data)
      n_planned_in_group <- sum(group_data$ever_planned == 1, na.rm = TRUE)
      n_not_planned_in_group <- n_total_in_group - n_planned_in_group

      results$Total_Planned[i] <- format_pct(n_planned_in_group / n_total_in_group)
      results$Total_NeverPlanned[i] <- format_pct(n_not_planned_in_group / n_total_in_group)
    }
  }

  results
}

# ==============================================================================
# CONVERSION RATES BY DEMOGRAPHIC GROUP
# ==============================================================================

#' Create table showing conversion rates by demographic group
#' @param df Data frame
#' @param vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of continuous variable names
#' @param show_opposites Character vector of variable labels to show opposites for
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param wave_var Name of wave variable (default: "WAVE")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @param move_var Name of move variable (default: "Moved_w23")
#' @return Data frame with conversion rates by demographic group
create_conversion_rates_by_group <- function(df, vars, continuous_vars = c("age"),
                                              show_opposites = c("Homeowner", "BA or Greater",
                                                                "Full-time Employed", "Married/Cohabiting"),
                                              id_var = "prim_key", age_var = "agegroup",
                                              wave_var = "WAVE", plan_var = "Plan_to_Move",
                                              move_var = "Moved_w23") {

  # Get all var names
  all_vars <- unlist(vars)

  # Get timing of first plan and first move for each person
  person_df <- df %>%
    arrange(.data[[id_var]], .data[[wave_var]]) %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      first_plan_wave = ifelse(any(.data[[plan_var]] == 1, na.rm = TRUE),
                                min(.data[[wave_var]][.data[[plan_var]] == 1], na.rm = TRUE), NA),
      first_move_wave = ifelse(any(.data[[move_var]] == 1, na.rm = TRUE),
                                min(.data[[wave_var]][.data[[move_var]] == 1], na.rm = TRUE), NA),
      .groups = "drop"
    )

  # Add each variable
  for (var in all_vars) {
    if (var %in% continuous_vars) {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    } else {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := max(.data[[var]], na.rm = TRUE), .groups = "drop")
    }
    person_df <- left_join(person_df, var_summary, by = id_var)
  }

  # Only valid planners (planned before or same wave as first move)
  person_df <- person_df %>%
    mutate(
      valid_planner = !is.na(first_plan_wave) &
                     (is.na(first_move_wave) | first_plan_wave <= first_move_wave),
      planned_then_moved = !is.na(first_plan_wave) & !is.na(first_move_wave) &
                           first_plan_wave <= first_move_wave
    )

  # Initialize results list to build rows
  results_list <- list()

  for (i in seq_along(vars)) {
    var_label <- names(vars)[i]
    var_name <- vars[[i]]

    if (var_name %in% continuous_vars) {
      # For continuous: just one row
      row_data <- list(Variable = var_label)

      for (age_val in c(0, 1)) {
        age_label <- if (age_val == 0) "Young" else "Old"
        age_data <- person_df %>% filter(agegroup == age_val)

        planners <- age_data %>% filter(valid_planner)
        moved <- planners %>% filter(planned_then_moved)

        row_data[[paste0(age_label, "_ConvRate")]] <- format_mean_sd(moved[[var_name]])
      }

      # Total
      planners <- person_df %>% filter(valid_planner)
      moved <- planners %>% filter(planned_then_moved)
      row_data$Total_ConvRate <- format_mean_sd(moved[[var_name]])

      results_list[[length(results_list) + 1]] <- row_data

    } else {
      # For binary: create one or two rows depending on show_opposites

      # Row 1: var=1 (e.g., Homeowner)
      row_data_1 <- list(Variable = var_label)

      for (age_val in c(0, 1)) {
        age_label <- if (age_val == 0) "Young" else "Old"
        age_data <- person_df %>% filter(agegroup == age_val)

        group_planners <- age_data %>% filter(valid_planner, .data[[var_name]] == 1)
        n_total <- nrow(group_planners)
        n_moved <- sum(group_planners$planned_then_moved, na.rm = TRUE)

        row_data_1[[paste0(age_label, "_ConvRate")]] <- format_pct(n_moved / n_total)
      }

      # Total for var=1
      group_planners <- person_df %>% filter(valid_planner, .data[[var_name]] == 1)
      n_total <- nrow(group_planners)
      n_moved <- sum(group_planners$planned_then_moved, na.rm = TRUE)
      row_data_1$Total_ConvRate <- format_pct(n_moved / n_total)

      results_list[[length(results_list) + 1]] <- row_data_1

      # Row 2: var=0 (e.g., Renter) - only if requested
      if (var_label %in% show_opposites) {
        # Create opposite label
        opposite_label <- paste0("Not ", var_label)
        if (var_label == "Homeowner") opposite_label <- "Renter"

        row_data_0 <- list(Variable = opposite_label)

        for (age_val in c(0, 1)) {
          age_label <- if (age_val == 0) "Young" else "Old"
          age_data <- person_df %>% filter(agegroup == age_val)

          group_planners <- age_data %>% filter(valid_planner, .data[[var_name]] == 0)
          n_total <- nrow(group_planners)
          n_moved <- sum(group_planners$planned_then_moved, na.rm = TRUE)

          row_data_0[[paste0(age_label, "_ConvRate")]] <- format_pct(n_moved / n_total)
        }

        # Total for var=0
        group_planners <- person_df %>% filter(valid_planner, .data[[var_name]] == 0)
        n_total <- nrow(group_planners)
        n_moved <- sum(group_planners$planned_then_moved, na.rm = TRUE)
        row_data_0$Total_ConvRate <- format_pct(n_moved / n_total)

        results_list[[length(results_list) + 1]] <- row_data_0
      }
    }
  }

  # Convert list to data frame
  results <- bind_rows(results_list)

  # Rename columns for clarity
  names(results) <- c("Variable", "Young_% Moved", "Old_% Moved", "Total_% Moved")

  results
}

# ==============================================================================
# STUCK RATES BY DEMOGRAPHIC GROUP
# ==============================================================================

#' Create table showing stuck rates by demographic group (complement of conversion rates)
#' @param df Data frame
#' @param vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of continuous variable names
#' @param show_opposites Character vector of variable labels to show opposites for
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param wave_var Name of wave variable (default: "WAVE")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @param move_var Name of move variable (default: "Moved_w23")
#' @return Data frame with stuck rates by demographic group
create_stuck_rates_by_group <- function(df, vars, continuous_vars = c("age"),
                                        show_opposites = c("Homeowner", "BA or Greater",
                                                          "Full-time Employed", "Married/Cohabiting"),
                                        id_var = "prim_key", age_var = "agegroup",
                                        wave_var = "WAVE", plan_var = "Plan_to_Move",
                                        move_var = "Moved_w23") {

  # Get all var names
  all_vars <- unlist(vars)

  # Get timing of first plan and first move for each person
  person_df <- df %>%
    arrange(.data[[id_var]], .data[[wave_var]]) %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      first_plan_wave = ifelse(any(.data[[plan_var]] == 1, na.rm = TRUE),
                                min(.data[[wave_var]][.data[[plan_var]] == 1], na.rm = TRUE), NA),
      first_move_wave = ifelse(any(.data[[move_var]] == 1, na.rm = TRUE),
                                min(.data[[wave_var]][.data[[move_var]] == 1], na.rm = TRUE), NA),
      .groups = "drop"
    )

  # Add each variable
  for (var in all_vars) {
    if (var %in% continuous_vars) {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    } else {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := max(.data[[var]], na.rm = TRUE), .groups = "drop")
    }
    person_df <- left_join(person_df, var_summary, by = id_var)
  }

  # Only valid planners (planned before or same wave as first move)
  person_df <- person_df %>%
    mutate(
      valid_planner = !is.na(first_plan_wave) &
                     (is.na(first_move_wave) | first_plan_wave <= first_move_wave),
      planned_stuck = !is.na(first_plan_wave) & is.na(first_move_wave)
    )

  # Initialize results list to build rows
  results_list <- list()

  for (i in seq_along(vars)) {
    var_label <- names(vars)[i]
    var_name <- vars[[i]]

    if (var_name %in% continuous_vars) {
      # For continuous: just one row
      row_data <- list(Variable = var_label)

      for (age_val in c(0, 1)) {
        age_label <- if (age_val == 0) "Young" else "Old"
        age_data <- person_df %>% filter(agegroup == age_val)

        planners <- age_data %>% filter(valid_planner)
        stuck <- planners %>% filter(planned_stuck)

        row_data[[paste0(age_label, "_StuckRate")]] <- format_mean_sd(stuck[[var_name]])
      }

      # Total
      planners <- person_df %>% filter(valid_planner)
      stuck <- planners %>% filter(planned_stuck)
      row_data$Total_StuckRate <- format_mean_sd(stuck[[var_name]])

      results_list[[length(results_list) + 1]] <- row_data

    } else {
      # For binary: create one or two rows depending on show_opposites

      # Row 1: var=1 (e.g., Homeowner)
      row_data_1 <- list(Variable = var_label)

      for (age_val in c(0, 1)) {
        age_label <- if (age_val == 0) "Young" else "Old"
        age_data <- person_df %>% filter(agegroup == age_val)

        group_planners <- age_data %>% filter(valid_planner, .data[[var_name]] == 1)
        n_total <- nrow(group_planners)
        n_stuck <- sum(group_planners$planned_stuck, na.rm = TRUE)

        row_data_1[[paste0(age_label, "_StuckRate")]] <- format_pct(n_stuck / n_total)
      }

      # Total for var=1
      group_planners <- person_df %>% filter(valid_planner, .data[[var_name]] == 1)
      n_total <- nrow(group_planners)
      n_stuck <- sum(group_planners$planned_stuck, na.rm = TRUE)
      row_data_1$Total_StuckRate <- format_pct(n_stuck / n_total)

      results_list[[length(results_list) + 1]] <- row_data_1

      # Row 2: var=0 (e.g., Renter) - only if requested
      if (var_label %in% show_opposites) {
        # Create opposite label
        opposite_label <- paste0("Not ", var_label)
        if (var_label == "Homeowner") opposite_label <- "Renter"

        row_data_0 <- list(Variable = opposite_label)

        for (age_val in c(0, 1)) {
          age_label <- if (age_val == 0) "Young" else "Old"
          age_data <- person_df %>% filter(agegroup == age_val)

          group_planners <- age_data %>% filter(valid_planner, .data[[var_name]] == 0)
          n_total <- nrow(group_planners)
          n_stuck <- sum(group_planners$planned_stuck, na.rm = TRUE)

          row_data_0[[paste0(age_label, "_StuckRate")]] <- format_pct(n_stuck / n_total)
        }

        # Total for var=0
        group_planners <- person_df %>% filter(valid_planner, .data[[var_name]] == 0)
        n_total <- nrow(group_planners)
        n_stuck <- sum(group_planners$planned_stuck, na.rm = TRUE)
        row_data_0$Total_StuckRate <- format_pct(n_stuck / n_total)

        results_list[[length(results_list) + 1]] <- row_data_0
      }
    }
  }

  # Convert list to data frame
  results <- bind_rows(results_list)

  # Rename columns for clarity
  names(results) <- c("Variable", "Young_% Stuck", "Old_% Stuck", "Total_% Stuck")

  results
}

# ==============================================================================
# NETWORK ANCHORS VS SUPPORTS: Compare stuck planners to successful movers
# ==============================================================================

#' Create table comparing stuck planners (planned but didn't move) to successful movers (planned and moved)
#' @param df Data frame
#' @param vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of continuous variable names
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param wave_var Name of wave variable (default: "WAVE")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @param move_var Name of move variable (default: "Moved_w23")
#' @return Data frame comparing stuck vs. moved planners
create_network_anchor_table <- function(df, vars, continuous_vars = character(),
                                        id_var = "prim_key", age_var = "agegroup",
                                        wave_var = "WAVE", plan_var = "Plan_to_Move",
                                        move_var = "Moved_w23") {

  # Get all var names
  all_vars <- unlist(vars)

  # Get timing of first plan and first move for each person
  person_df <- df %>%
    arrange(.data[[id_var]], .data[[wave_var]]) %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      first_plan_wave = ifelse(any(.data[[plan_var]] == 1, na.rm = TRUE),
                                min(.data[[wave_var]][.data[[plan_var]] == 1], na.rm = TRUE), NA),
      first_move_wave = ifelse(any(.data[[move_var]] == 1, na.rm = TRUE),
                                min(.data[[wave_var]][.data[[move_var]] == 1], na.rm = TRUE), NA),
      .groups = "drop"
    )

  # Add each variable
  for (var in all_vars) {
    if (var %in% continuous_vars) {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
        mutate(!!var := ifelse(is.nan(.data[[var]]), NA_real_, .data[[var]]))
    } else {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := max(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
        mutate(!!var := ifelse(is.infinite(.data[[var]]), NA_real_, .data[[var]]))
    }
    person_df <- left_join(person_df, var_summary, by = id_var)
  }

  # Create categories: stuck (planned but didn't move) vs. moved (planned and moved)
  person_df <- person_df %>%
    mutate(
      valid_planner = !is.na(first_plan_wave) &
                     (is.na(first_move_wave) | first_plan_wave <= first_move_wave),
      planned_stuck = !is.na(first_plan_wave) & is.na(first_move_wave),
      planned_moved = !is.na(first_plan_wave) & !is.na(first_move_wave) &
                      first_plan_wave <= first_move_wave
    )

  # Filter to only valid planners
  planners <- person_df %>% filter(valid_planner)

  results <- data.frame(Variable = names(vars), stringsAsFactors = FALSE)

  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_planners <- planners %>% filter(agegroup == age_val)

    stuck <- age_planners %>% filter(planned_stuck)
    moved <- age_planners %>% filter(planned_moved)

    stuck_col <- paste0(age_label, "_Stuck")
    moved_col <- paste0(age_label, "_Moved")

    results[[stuck_col]] <- NA_character_
    results[[moved_col]] <- NA_character_

    for (i in seq_along(vars)) {
      var_name <- vars[[i]]

      if (var_name %in% continuous_vars) {
        results[[stuck_col]][i] <- format_mean_sd(stuck[[var_name]])
        results[[moved_col]][i] <- format_mean_sd(moved[[var_name]])
      } else {
        n_stuck <- sum(stuck[[var_name]] == 1, na.rm = TRUE)
        n_moved <- sum(moved[[var_name]] == 1, na.rm = TRUE)

        results[[stuck_col]][i] <- format_n_pct(n_stuck, nrow(stuck))
        results[[moved_col]][i] <- format_n_pct(n_moved, nrow(moved))
      }
    }
  }

  # Add N row
  n_row <- data.frame(Variable = "N", stringsAsFactors = FALSE)
  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_planners <- planners %>% filter(agegroup == age_val)

    n_stuck <- sum(age_planners$planned_stuck, na.rm = TRUE)
    n_moved <- sum(age_planners$planned_moved, na.rm = TRUE)

    n_row[[paste0(age_label, "_Stuck")]] <- as.character(n_stuck)
    n_row[[paste0(age_label, "_Moved")]] <- as.character(n_moved)
  }

  bind_rows(results, n_row)
}

# ==============================================================================
# UNPLANNED MOVER RATES BY DEMOGRAPHIC GROUP
# ==============================================================================

#' Create table showing unplanned mover rates by demographic group
#' @param df Data frame
#' @param vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of continuous variable names
#' @param show_opposites Character vector of variable labels to show opposites for
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param wave_var Name of wave variable (default: "WAVE")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @param move_var Name of move variable (default: "Moved_w23")
#' @return Data frame with unplanned mover rates by demographic group
create_unplanned_rates_by_group <- function(df, vars, continuous_vars = c("age"),
                                            show_opposites = c("Homeowner", "BA or Greater",
                                                              "Full-time Employed", "Married/Cohabiting"),
                                            id_var = "prim_key", age_var = "agegroup",
                                            wave_var = "WAVE", plan_var = "Plan_to_Move",
                                            move_var = "Moved_w23") {

  # Get all var names
  all_vars <- unlist(vars)

  # Get timing of first plan and first move for each person
  person_df <- df %>%
    arrange(.data[[id_var]], .data[[wave_var]]) %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      ever_planned = as.integer(any(.data[[plan_var]] == 1, na.rm = TRUE)),
      ever_moved = as.integer(any(.data[[move_var]] == 1, na.rm = TRUE)),
      .groups = "drop"
    )

  # Add each variable
  for (var in all_vars) {
    if (var %in% continuous_vars) {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
        mutate(!!var := ifelse(is.nan(.data[[var]]), NA_real_, .data[[var]]))
    } else {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := max(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
        mutate(!!var := ifelse(is.infinite(.data[[var]]), NA_real_, .data[[var]]))
    }
    person_df <- left_join(person_df, var_summary, by = id_var)
  }

  # Filter to only movers
  movers <- person_df %>% filter(ever_moved == 1)

  # Initialize results list to build rows
  results_list <- list()

  for (i in seq_along(vars)) {
    var_label <- names(vars)[i]
    var_name <- vars[[i]]

    if (var_name %in% continuous_vars) {
      # For continuous: just one row
      row_data <- list(Variable = var_label)

      for (age_val in c(0, 1)) {
        age_label <- if (age_val == 0) "Young" else "Old"
        age_movers <- movers %>% filter(agegroup == age_val)

        unplanned <- age_movers %>% filter(ever_planned == 0)

        row_data[[paste0(age_label, "_UnplannedRate")]] <- format_mean_sd(unplanned[[var_name]])
      }

      # Total
      unplanned <- movers %>% filter(ever_planned == 0)
      row_data$Total_UnplannedRate <- format_mean_sd(unplanned[[var_name]])

      results_list[[length(results_list) + 1]] <- row_data

    } else {
      # For binary: create one or two rows depending on show_opposites

      # Row 1: var=1 (e.g., Homeowner)
      row_data_1 <- list(Variable = var_label)

      for (age_val in c(0, 1)) {
        age_label <- if (age_val == 0) "Young" else "Old"
        age_movers <- movers %>% filter(agegroup == age_val)

        group_movers <- age_movers %>% filter(.data[[var_name]] == 1)
        n_total <- nrow(group_movers)
        n_unplanned <- sum(group_movers$ever_planned == 0, na.rm = TRUE)

        row_data_1[[paste0(age_label, "_UnplannedRate")]] <- format_pct(n_unplanned / n_total)
      }

      # Total for var=1
      group_movers <- movers %>% filter(.data[[var_name]] == 1)
      n_total <- nrow(group_movers)
      n_unplanned <- sum(group_movers$ever_planned == 0, na.rm = TRUE)
      row_data_1$Total_UnplannedRate <- format_pct(n_unplanned / n_total)

      results_list[[length(results_list) + 1]] <- row_data_1

      # Row 2: var=0 (e.g., Renter) - only if requested
      if (var_label %in% show_opposites) {
        # Create opposite label
        opposite_label <- paste0("Not ", var_label)
        if (var_label == "Homeowner") opposite_label <- "Renter"

        row_data_0 <- list(Variable = opposite_label)

        for (age_val in c(0, 1)) {
          age_label <- if (age_val == 0) "Young" else "Old"
          age_movers <- movers %>% filter(agegroup == age_val)

          group_movers <- age_movers %>% filter(.data[[var_name]] == 0)
          n_total <- nrow(group_movers)
          n_unplanned <- sum(group_movers$ever_planned == 0, na.rm = TRUE)

          row_data_0[[paste0(age_label, "_UnplannedRate")]] <- format_pct(n_unplanned / n_total)
        }

        # Total for var=0
        group_movers <- movers %>% filter(.data[[var_name]] == 0)
        n_total <- nrow(group_movers)
        n_unplanned <- sum(group_movers$ever_planned == 0, na.rm = TRUE)
        row_data_0$Total_UnplannedRate <- format_pct(n_unplanned / n_total)

        results_list[[length(results_list) + 1]] <- row_data_0
      }
    }
  }

  # Convert list to data frame
  results <- bind_rows(results_list)

  # Rename columns for clarity
  names(results) <- c("Variable", "Young_% Unplanned", "Old_% Unplanned", "Total_% Unplanned")

  results
}

# ==============================================================================
# WHO PLANS TABLE: Characteristics of all planners vs. non-planners
# ==============================================================================

#' Create table comparing all who planned to move vs. all who never planned
#' @param df Data frame
#' @param vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of continuous variable names
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @return Data frame comparing planners vs. non-planners
create_who_plans_table <- function(df, vars, continuous_vars = c("age"),
                                    id_var = "prim_key", age_var = "agegroup",
                                    plan_var = "Plan_to_Move") {

  # Get all var names
  all_vars <- unlist(vars)

  # Collapse to person level
  person_df <- df %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      ever_planned = as.integer(any(.data[[plan_var]] == 1, na.rm = TRUE)),
      .groups = "drop"
    )

  # Add each variable
  for (var in all_vars) {
    if (var %in% continuous_vars) {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    } else {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := max(.data[[var]], na.rm = TRUE), .groups = "drop")
    }
    person_df <- left_join(person_df, var_summary, by = id_var)
  }

  results <- data.frame(Variable = names(vars), stringsAsFactors = FALSE)

  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- person_df %>% filter(agegroup == age_val)

    planners <- age_data %>% filter(ever_planned == 1)
    non_planners <- age_data %>% filter(ever_planned == 0)

    plan_col <- paste0(age_label, "_Planners")
    no_plan_col <- paste0(age_label, "_NonPlanners")

    results[[plan_col]] <- NA_character_
    results[[no_plan_col]] <- NA_character_

    for (i in seq_along(vars)) {
      var_name <- vars[[i]]

      if (var_name %in% continuous_vars) {
        results[[plan_col]][i] <- format_mean_sd(planners[[var_name]])
        results[[no_plan_col]][i] <- format_mean_sd(non_planners[[var_name]])
      } else {
        n_plan <- sum(planners[[var_name]] == 1, na.rm = TRUE)
        n_no_plan <- sum(non_planners[[var_name]] == 1, na.rm = TRUE)

        results[[plan_col]][i] <- format_n_pct(n_plan, nrow(planners))
        results[[no_plan_col]][i] <- format_n_pct(n_no_plan, nrow(non_planners))
      }
    }
  }

  # Add N row
  n_row <- data.frame(Variable = "N", stringsAsFactors = FALSE)
  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- person_df %>% filter(agegroup == age_val)

    n_plan <- sum(age_data$ever_planned == 1, na.rm = TRUE)
    n_no_plan <- sum(age_data$ever_planned == 0, na.rm = TRUE)

    n_row[[paste0(age_label, "_Planners")]] <- as.character(n_plan)
    n_row[[paste0(age_label, "_NonPlanners")]] <- as.character(n_no_plan)
  }

  bind_rows(results, n_row)
}
