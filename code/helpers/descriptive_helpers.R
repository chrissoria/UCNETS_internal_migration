# descriptive_helpers.R
# Helper functions for generating descriptive statistics tables
# Used by: 04_descriptives.Rmd

library(dplyr)
library(tidyr)

# ==============================================================================
# FORMATTING HELPERS
# ==============================================================================

#' Format a percentage with specified decimal places
#' @param x Numeric value (proportion, not percentage)
#' @param digits Number of decimal places (default: 1)
#' @return Character string formatted as percentage
format_pct <- function(x, digits = 1) {
  sprintf(paste0("%.", digits, "f%%"), x * 100)
}

#' Format count with percentage in parentheses
#' @param n Count
#' @param total Total for percentage calculation
#' @param digits Decimal places for percentage
#' @return Character string like "123 (45.6%)"
format_n_pct <- function(n, total, digits = 1) {
  pct <- if (total > 0) n / total else 0
  sprintf("%d (%s)", n, format_pct(pct, digits))
}

#' Format mean with SD in parentheses
#' @param x Numeric vector
#' @param digits Decimal places
#' @return Character string like "45.2 (12.3)"
format_mean_sd <- function(x, digits = 1) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), m, s)
}

# ==============================================================================
# MOVE RATES TABLE
# ==============================================================================

#' Create move rates table by age group
#' @param df Data frame with outcome and agegroup variables
#' @param outcomes Character vector of outcome variable names
#' @param outcome_labels Character vector of labels for outcomes
#' @param age_var Name of age group variable (default: "agegroup")
#' @return Data frame with move rates by age group
create_move_rates_table <- function(df, outcomes, outcome_labels = NULL,
                                     age_var = "agegroup") {
  if (is.null(outcome_labels)) outcome_labels <- outcomes

  results <- data.frame(Outcome = outcome_labels, stringsAsFactors = FALSE)

  # Calculate for each age group
  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- df %>% filter(.data[[age_var]] == age_val)
    n_total <- nrow(age_data)

    n_col <- paste0(age_label, "_N")
    pct_col <- paste0(age_label, "_Pct")

    results[[n_col]] <- NA_integer_
    results[[pct_col]] <- NA_character_

    for (i in seq_along(outcomes)) {
      n_outcome <- sum(age_data[[outcomes[i]]] == 1, na.rm = TRUE)
      results[[n_col]][i] <- n_outcome
      results[[pct_col]][i] <- format_pct(n_outcome / n_total)
    }
  }

  # Calculate totals
  n_total_all <- nrow(df)
  results$Total_N <- NA_integer_
  results$Total_Pct <- NA_character_

  for (i in seq_along(outcomes)) {
    n_outcome <- sum(df[[outcomes[i]]] == 1, na.rm = TRUE)
    results$Total_N[i] <- n_outcome
    results$Total_Pct[i] <- format_pct(n_outcome / n_total_all)
  }

  # Add sample size row
  sample_row <- data.frame(
    Outcome = "Total Observations",
    Young_N = sum(df[[age_var]] == 0),
    Young_Pct = "",
    Old_N = sum(df[[age_var]] == 1),
    Old_Pct = "",
    Total_N = nrow(df),
    Total_Pct = "",
    stringsAsFactors = FALSE
  )

  bind_rows(results, sample_row)
}

# ==============================================================================
# WHO MOVES TABLE (DEMOGRAPHICS COMPARISON)
# ==============================================================================

#' Create demographic comparison table for movers vs non-movers
#' @param df Data frame
#' @param move_var Name of the move outcome variable
#' @param demo_vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of variable names that are continuous
#' @param age_var Name of age group variable (default: "agegroup")
#' @return Data frame comparing demographics of movers vs non-movers by age group
create_who_moves_table <- function(df, move_var, demo_vars,
                                    continuous_vars = c("age"),
                                    age_var = "agegroup") {

  results <- data.frame(Variable = names(demo_vars), stringsAsFactors = FALSE)

  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- df %>% filter(.data[[age_var]] == age_val)

    movers <- age_data %>% filter(.data[[move_var]] == 1)
    stayers <- age_data %>% filter(.data[[move_var]] == 0 | is.na(.data[[move_var]]))

    moved_col <- paste0(age_label, "_Moved")
    stayed_col <- paste0(age_label, "_Stayed")

    results[[moved_col]] <- NA_character_
    results[[stayed_col]] <- NA_character_

    for (i in seq_along(demo_vars)) {
      var_name <- demo_vars[[i]]

      if (var_name %in% continuous_vars) {
        # Continuous variable: show mean (SD)
        results[[moved_col]][i] <- format_mean_sd(movers[[var_name]])
        results[[stayed_col]][i] <- format_mean_sd(stayers[[var_name]])
      } else {
        # Binary variable: show N (%)
        n_moved <- sum(movers[[var_name]] == 1, na.rm = TRUE)
        n_stayed <- sum(stayers[[var_name]] == 1, na.rm = TRUE)

        results[[moved_col]][i] <- format_n_pct(n_moved, nrow(movers))
        results[[stayed_col]][i] <- format_n_pct(n_stayed, nrow(stayers))
      }
    }
  }

  # Add N row
  n_row <- data.frame(Variable = "N", stringsAsFactors = FALSE)
  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- df %>% filter(.data[[age_var]] == age_val)

    n_moved <- sum(age_data[[move_var]] == 1, na.rm = TRUE)
    n_stayed <- sum(age_data[[move_var]] == 0 | is.na(age_data[[move_var]]))

    n_row[[paste0(age_label, "_Moved")]] <- as.character(n_moved)
    n_row[[paste0(age_label, "_Stayed")]] <- as.character(n_stayed)
  }

  bind_rows(results, n_row)
}

# ==============================================================================
# SAMPLE CHARACTERISTICS TABLE
# ==============================================================================

#' Create sample characteristics table by age group
#' @param df Data frame
#' @param vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of continuous variable names
#' @param age_var Name of age group variable (default: "agegroup")
#' @return Data frame with sample characteristics by age group
create_sample_characteristics <- function(df, vars, continuous_vars = c("age"),
                                           age_var = "agegroup") {

  results <- data.frame(Variable = names(vars), stringsAsFactors = FALSE)

  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- df %>% filter(.data[[age_var]] == age_val)

    col_name <- age_label
    results[[col_name]] <- NA_character_

    for (i in seq_along(vars)) {
      var_name <- vars[[i]]

      if (var_name %in% continuous_vars) {
        results[[col_name]][i] <- format_mean_sd(age_data[[var_name]])
      } else {
        n_yes <- sum(age_data[[var_name]] == 1, na.rm = TRUE)
        results[[col_name]][i] <- format_n_pct(n_yes, nrow(age_data))
      }
    }
  }

  # Total column
  results$Total <- NA_character_
  for (i in seq_along(vars)) {
    var_name <- vars[[i]]

    if (var_name %in% continuous_vars) {
      results$Total[i] <- format_mean_sd(df[[var_name]])
    } else {
      n_yes <- sum(df[[var_name]] == 1, na.rm = TRUE)
      results$Total[i] <- format_n_pct(n_yes, nrow(df))
    }
  }

  # Add N row
  n_row <- data.frame(
    Variable = "N",
    Young = as.character(sum(df[[age_var]] == 0)),
    Old = as.character(sum(df[[age_var]] == 1)),
    Total = as.character(nrow(df)),
    stringsAsFactors = FALSE
  )

  bind_rows(results, n_row)
}

#' Create person-level sample characteristics table by age group
#' @param df Data frame
#' @param vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of continuous variable names
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @return Data frame with sample characteristics by age group at person level
create_sample_characteristics_person <- function(df, vars, continuous_vars = c("age"),
                                                   id_var = "prim_key", age_var = "agegroup") {

  # Get all var names
  all_vars <- unlist(vars)

  # Collapse to person level
  person_df <- df %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      .groups = "drop"
    )

  # Add each variable separately

  for (var in all_vars) {
    if (var %in% continuous_vars) {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    } else {
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := first(na.omit(.data[[var]])), .groups = "drop")
    }
    person_df <- left_join(person_df, var_summary, by = id_var)
  }

  results <- data.frame(Variable = names(vars), stringsAsFactors = FALSE)

  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- person_df %>% filter(agegroup == age_val)

    col_name <- age_label
    results[[col_name]] <- NA_character_

    for (i in seq_along(vars)) {
      var_name <- vars[[i]]

      if (var_name %in% continuous_vars) {
        results[[col_name]][i] <- format_mean_sd(age_data[[var_name]])
      } else {
        n_yes <- sum(age_data[[var_name]] == 1, na.rm = TRUE)
        results[[col_name]][i] <- format_n_pct(n_yes, nrow(age_data))
      }
    }
  }

  # Total column
  results$Total <- NA_character_
  for (i in seq_along(vars)) {
    var_name <- vars[[i]]

    if (var_name %in% continuous_vars) {
      results$Total[i] <- format_mean_sd(person_df[[var_name]])
    } else {
      n_yes <- sum(person_df[[var_name]] == 1, na.rm = TRUE)
      results$Total[i] <- format_n_pct(n_yes, nrow(person_df))
    }
  }

  # Add N row
  n_row <- data.frame(
    Variable = "N",
    Young = as.character(sum(person_df$agegroup == 0)),
    Old = as.character(sum(person_df$agegroup == 1)),
    Total = as.character(nrow(person_df)),
    stringsAsFactors = FALSE
  )

  bind_rows(results, n_row)
}

# ==============================================================================
# PERSON-LEVEL MOVE RATES TABLE
# ==============================================================================

#' Create person-level move rates table (ever moved during study)
#' @param df Data frame with outcome and agegroup variables
#' @param outcomes Character vector of outcome variable names
#' @param outcome_labels Character vector of labels for outcomes
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @return Data frame with move rates by age group at person level
create_move_rates_table_person <- function(df, outcomes, outcome_labels = NULL,
                                            id_var = "prim_key", age_var = "agegroup") {
  if (is.null(outcome_labels)) outcome_labels <- outcomes

  # Collapse to person level - ever experienced outcome
  person_df <- df %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      across(all_of(outcomes), ~ as.integer(any(.x == 1, na.rm = TRUE))),
      .groups = "drop"
    )

  results <- data.frame(Outcome = outcome_labels, stringsAsFactors = FALSE)

  # Calculate for each age group
  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- person_df %>% filter(agegroup == age_val)
    n_total <- nrow(age_data)

    n_col <- paste0(age_label, "_N")
    pct_col <- paste0(age_label, "_Pct")

    results[[n_col]] <- NA_integer_
    results[[pct_col]] <- NA_character_

    for (i in seq_along(outcomes)) {
      n_outcome <- sum(age_data[[outcomes[i]]] == 1, na.rm = TRUE)
      results[[n_col]][i] <- n_outcome
      results[[pct_col]][i] <- format_pct(n_outcome / n_total)
    }
  }

  # Calculate totals
  n_total_all <- nrow(person_df)
  results$Total_N <- NA_integer_
  results$Total_Pct <- NA_character_

  for (i in seq_along(outcomes)) {
    n_outcome <- sum(person_df[[outcomes[i]]] == 1, na.rm = TRUE)
    results$Total_N[i] <- n_outcome
    results$Total_Pct[i] <- format_pct(n_outcome / n_total_all)
  }

  # Add sample size row
  sample_row <- data.frame(
    Outcome = "Total Respondents",
    Young_N = sum(person_df$agegroup == 0),
    Young_Pct = "",
    Old_N = sum(person_df$agegroup == 1),
    Old_Pct = "",
    Total_N = nrow(person_df),
    Total_Pct = "",
    stringsAsFactors = FALSE
  )

  bind_rows(results, sample_row)
}

# ==============================================================================
# PERSON-LEVEL WHO MOVES TABLE
# ==============================================================================

#' Create person-level demographic comparison for movers vs non-movers
#' @param df Data frame
#' @param move_var Name of the move outcome variable
#' @param demo_vars Named list: names are labels, values are variable names
#' @param continuous_vars Character vector of variable names that are continuous
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @return Data frame comparing demographics of movers vs non-movers by age group
create_who_moves_table_person <- function(df, move_var, demo_vars,
                                           continuous_vars = c("age"),
                                           id_var = "prim_key", age_var = "agegroup") {

  # Get all demo var names
  all_demo_vars <- unlist(demo_vars)

  # Collapse to person level using a simpler approach
  # First get the basic person-level info
  person_df <- df %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      ever_moved = as.integer(any(.data[[move_var]] == 1, na.rm = TRUE)),
      .groups = "drop"
    )

  # Now add each demo var separately using first non-NA value
  for (var in all_demo_vars) {
    if (var %in% continuous_vars) {
      # For continuous: use mean
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    } else {
      # For binary: use first non-NA
      var_summary <- df %>%
        group_by(.data[[id_var]]) %>%
        summarise(!!var := first(na.omit(.data[[var]])), .groups = "drop")
    }
    person_df <- left_join(person_df, var_summary, by = id_var)
  }

  results <- data.frame(Variable = names(demo_vars), stringsAsFactors = FALSE)

  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- person_df %>% filter(agegroup == age_val)

    movers <- age_data %>% filter(ever_moved == 1)
    stayers <- age_data %>% filter(ever_moved == 0)

    moved_col <- paste0(age_label, "_Moved")
    stayed_col <- paste0(age_label, "_Stayed")

    results[[moved_col]] <- NA_character_
    results[[stayed_col]] <- NA_character_

    for (i in seq_along(demo_vars)) {
      var_name <- demo_vars[[i]]

      if (var_name %in% continuous_vars) {
        results[[moved_col]][i] <- format_mean_sd(movers[[var_name]])
        results[[stayed_col]][i] <- format_mean_sd(stayers[[var_name]])
      } else {
        n_moved <- sum(movers[[var_name]] == 1, na.rm = TRUE)
        n_stayed <- sum(stayers[[var_name]] == 1, na.rm = TRUE)

        results[[moved_col]][i] <- format_n_pct(n_moved, nrow(movers))
        results[[stayed_col]][i] <- format_n_pct(n_stayed, nrow(stayers))
      }
    }
  }

  # Add N row
  n_row <- data.frame(Variable = "N", stringsAsFactors = FALSE)
  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- person_df %>% filter(agegroup == age_val)

    n_moved <- sum(age_data$ever_moved == 1, na.rm = TRUE)
    n_stayed <- sum(age_data$ever_moved == 0, na.rm = TRUE)

    n_row[[paste0(age_label, "_Moved")]] <- as.character(n_moved)
    n_row[[paste0(age_label, "_Stayed")]] <- as.character(n_stayed)
  }

  bind_rows(results, n_row)
}

# ==============================================================================
# NETWORK VARIABLES TABLE
# ==============================================================================

#' Create network variables summary table (means/SDs by group)
#' @param df Data frame
#' @param network_vars Named list: names are labels, values are variable names
#' @param group_vars Named list of grouping variables: names are column prefixes,
#'        values are lists with 'var' (variable name) and 'levels' (named list of value=label pairs)
#' @return Data frame with network variable means/SDs by group
create_network_summary <- function(df, network_vars, group_vars = NULL) {

 # Default to age group if no group_vars specified
  if (is.null(group_vars)) {
    group_vars <- list(
      "Age" = list(
        var = "agegroup",
        levels = list("0" = "Young", "1" = "Old")
      )
    )
  }

  results <- data.frame(Variable = names(network_vars), stringsAsFactors = FALSE)

  # Loop through each grouping variable
 for (group_name in names(group_vars)) {
    group_info <- group_vars[[group_name]]
    var_name <- group_info$var
    levels <- group_info$levels

    for (level_val in names(levels)) {
      level_label <- levels[[level_val]]

      # Filter data for this group level
      if (is.numeric(df[[var_name]])) {
        group_data <- df %>% filter(.data[[var_name]] == as.numeric(level_val))
      } else {
        group_data <- df %>% filter(.data[[var_name]] == level_val)
      }

      col_name <- level_label
      results[[col_name]] <- NA_character_

      for (i in seq_along(network_vars)) {
        net_var <- network_vars[[i]]
        results[[col_name]][i] <- format_mean_sd(group_data[[net_var]], digits = 2)
      }
    }
  }

  # Total column
  results$Total <- NA_character_
  for (i in seq_along(network_vars)) {
    net_var <- network_vars[[i]]
    results$Total[i] <- format_mean_sd(df[[net_var]], digits = 2)
  }

  results
}

# ==============================================================================
# SIMPLIFIED NETWORK TABLE (groups as rows, network types as columns)
# ==============================================================================

#' Create simplified network table with groups as rows and network types as columns
#' @param df Data frame
#' @param network_vars Named vector: names are column labels, values are variable names
#' @param group_definitions List of group definitions, each with 'label', 'var', and 'value'
#' @return Data frame with groups as rows and network types as columns
create_network_table_simple <- function(df, network_vars, group_definitions) {

  results <- data.frame(Group = character(), stringsAsFactors = FALSE)

  for (i in seq_along(group_definitions)) {
    group <- group_definitions[[i]]
    label <- group$label
    var_name <- group$var
    value <- group$value

    # Filter data for this group
    if (is.null(var_name)) {
      # Total row - use all data
      group_data <- df
    } else if (is.numeric(df[[var_name]])) {
      group_data <- df %>% filter(.data[[var_name]] == as.numeric(value))
    } else {
      group_data <- df %>% filter(.data[[var_name]] == value)
    }

    row_data <- data.frame(Group = label, stringsAsFactors = FALSE)

    for (j in seq_along(network_vars)) {
      col_label <- names(network_vars)[j]
      net_var <- network_vars[[j]]
      row_data[[col_label]] <- format_mean_sd(group_data[[net_var]], digits = 2)
    }

    results <- bind_rows(results, row_data)
  }

  results
}

# ==============================================================================
# SIMPLIFIED LIFE EVENTS TABLE (groups as rows, events as columns)
# ==============================================================================

#' Create simplified life events table with groups as rows and events as columns
#' @param df Data frame
#' @param event_vars Named vector: names are column labels, values are variable names
#' @param group_definitions List of group definitions, each with 'label', 'var', and 'value'
#' @return Data frame with groups as rows and life events as columns (showing percentages)
create_life_events_table_simple <- function(df, event_vars, group_definitions) {

  results <- data.frame(Group = character(), stringsAsFactors = FALSE)

  for (i in seq_along(group_definitions)) {
    group <- group_definitions[[i]]
    label <- group$label
    var_name <- group$var
    value <- group$value

    # Filter data for this group
    if (is.null(var_name)) {
      # Total row - use all data
      group_data <- df
    } else if (is.numeric(df[[var_name]])) {
      group_data <- df %>% filter(.data[[var_name]] == as.numeric(value))
    } else {
      group_data <- df %>% filter(.data[[var_name]] == value)
    }

    row_data <- data.frame(Group = label, stringsAsFactors = FALSE)
    n_total <- nrow(group_data)

    for (j in seq_along(event_vars)) {
      col_label <- names(event_vars)[j]
      event_var <- event_vars[[j]]
      n_event <- sum(group_data[[event_var]] == 1, na.rm = TRUE)
      row_data[[col_label]] <- format_pct(n_event / n_total)
    }

    results <- bind_rows(results, row_data)
  }

  results
}

# ==============================================================================
# LIFE EVENTS TABLE
# ==============================================================================

#' Create life events frequency table by age group
#' @param df Data frame
#' @param event_vars Named list: names are labels, values are variable names
#' @param age_var Name of age group variable (default: "agegroup")
#' @return Data frame with life event frequencies by age group
create_life_events_table <- function(df, event_vars, age_var = "agegroup") {
  results <- data.frame(Event = names(event_vars), stringsAsFactors = FALSE)

  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- df %>% filter(.data[[age_var]] == age_val)
    n_total <- nrow(age_data)

    n_col <- paste0(age_label, "_N")
    pct_col <- paste0(age_label, "_Pct")

    results[[n_col]] <- NA_integer_
    results[[pct_col]] <- NA_character_

    for (i in seq_along(event_vars)) {
      var_name <- event_vars[[i]]
      n_event <- sum(age_data[[var_name]] == 1, na.rm = TRUE)
      results[[n_col]][i] <- n_event
      results[[pct_col]][i] <- format_pct(n_event / n_total)
    }
  }

  # Total column
  n_total_all <- nrow(df)
  results$Total_N <- NA_integer_
  results$Total_Pct <- NA_character_

  for (i in seq_along(event_vars)) {
    var_name <- event_vars[[i]]
    n_event <- sum(df[[var_name]] == 1, na.rm = TRUE)
    results$Total_N[i] <- n_event
    results$Total_Pct[i] <- format_pct(n_event / n_total_all)
  }

  results
}

# ==============================================================================
# ATTRITION TABLE
# ==============================================================================

#' Create attrition/wave participation table
#' @param df Data frame
#' @param id_var Person identifier variable (default: "prim_key")
#' @param wave_var Wave variable (default: "WAVE")
#' @param age_var Age group variable (default: "agegroup")
#' @return Data frame with observations per wave and attrition patterns
create_attrition_table <- function(df, id_var = "prim_key", wave_var = "WAVE",
                                    age_var = "agegroup") {
  # Observations per wave by age group
  wave_counts <- df %>%
    group_by(.data[[wave_var]], .data[[age_var]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = all_of(age_var), values_from = n,
                names_prefix = "agegroup_")

  # Rename columns
  names(wave_counts) <- c("Wave", "Young", "Old")
  wave_counts$Total <- wave_counts$Young + wave_counts$Old

  # Add participation patterns
  participation <- df %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      n_waves = n(),
      agegroup = first(.data[[age_var]]),
      .groups = "drop"
    )

  pattern_summary <- participation %>%
    group_by(agegroup, n_waves) %>%
    summarise(n_people = n(), .groups = "drop") %>%
    pivot_wider(names_from = agegroup, values_from = n_people,
                names_prefix = "agegroup_")

  names(pattern_summary) <- c("Waves_Participated", "Young", "Old")
  pattern_summary$Total <- pattern_summary$Young + pattern_summary$Old

  list(
    wave_counts = wave_counts,
    participation_patterns = pattern_summary
  )
}

# ==============================================================================
# CORRELATION MATRIX
# ==============================================================================

#' Create correlation matrix for key predictors
#' @param df Data frame
#' @param vars Character vector of variable names
#' @param var_labels Optional named vector of labels
#' @param digits Number of decimal places (default: 2)
#' @return Correlation matrix as data frame
create_correlation_matrix <- function(df, vars, var_labels = NULL, digits = 2) {
  # Subset to relevant variables
  cor_data <- df %>% select(all_of(vars))

  # Calculate correlation matrix
  cor_mat <- cor(cor_data, use = "pairwise.complete.obs")

  # Convert to data frame
  cor_df <- as.data.frame(cor_mat)

  # Apply labels if provided

  if (!is.null(var_labels)) {
    names(cor_df) <- var_labels[vars]
    rownames(cor_df) <- var_labels[vars]
  }

  # Round values
  cor_df <- round(cor_df, digits)

  # Add row names as first column
  cor_df <- cbind(Variable = rownames(cor_df), cor_df)
  rownames(cor_df) <- NULL

  cor_df
}

# ==============================================================================
# PLAN TO MOVE CONVERSION TABLE
# ==============================================================================

#' Create table showing what proportion of those who planned to move actually moved
#' @param df Data frame
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param wave_var Name of wave variable (default: "WAVE")
#' @param move_var Name of move variable (default: "Moved_w23")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @return Data frame with conversion rates by age group
create_plan_to_move_conversion <- function(df, id_var = "prim_key", age_var = "agegroup",
                                            wave_var = "WAVE", move_var = "Moved_w23",
                                            plan_var = "Plan_to_Move") {

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

  # Only count people who planned BEFORE or SAME WAVE as they moved
  # Exclude those who moved before they planned
  person_df <- person_df %>%
    mutate(
      valid_planner = !is.na(first_plan_wave),  # Ever planned
      planned_then_moved = !is.na(first_plan_wave) & !is.na(first_move_wave) &
                           first_plan_wave <= first_move_wave,  # Planned at or before move
      planned_no_move = !is.na(first_plan_wave) & is.na(first_move_wave)  # Planned but never moved
    )

  results <- data.frame(
    Group = c("Young", "Old", "Total"),
    stringsAsFactors = FALSE
  )

  # Calculate for each age group
  for (i in 1:2) {
    age_val <- i - 1
    age_data <- person_df %>% filter(agegroup == age_val)

    # Only those who planned first (before or same wave as move)
    planners <- age_data %>% filter(planned_then_moved | planned_no_move)
    n_planners <- nrow(planners)
    n_moved <- sum(planners$planned_then_moved, na.rm = TRUE)

    results$N_Planned[i] <- n_planners
    results$N_Moved[i] <- n_moved
    results$Pct_Moved[i] <- format_pct(n_moved / n_planners)
  }

  # Total
  planners_total <- person_df %>% filter(planned_then_moved | planned_no_move)
  n_planners_total <- nrow(planners_total)
  n_moved_total <- sum(planners_total$planned_then_moved, na.rm = TRUE)

  results$N_Planned[3] <- n_planners_total
  results$N_Moved[3] <- n_moved_total
  results$Pct_Moved[3] <- format_pct(n_moved_total / n_planners_total)

  results
}

# ==============================================================================
# PLANNER DESTINATION TABLE
# ==============================================================================

#' Create table showing where planners moved (by direction)
#' @param df Data frame
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param wave_var Name of wave variable (default: "WAVE")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @return Data frame showing destinations of planners
create_planner_destinations <- function(df, id_var = "prim_key", age_var = "agegroup",
                                         wave_var = "WAVE", plan_var = "Plan_to_Move") {

  # Get timing for each person
  person_df <- df %>%
    arrange(.data[[id_var]], .data[[wave_var]]) %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      first_plan_wave = ifelse(any(.data[[plan_var]] == 1, na.rm = TRUE),
                                min(.data[[wave_var]][.data[[plan_var]] == 1], na.rm = TRUE), NA),
      first_move_any = ifelse(any(Moved_w23 == 1, na.rm = TRUE),
                              min(.data[[wave_var]][Moved_w23 == 1], na.rm = TRUE), NA),
      first_move_within = ifelse(any(Moved_in_Bay_w23 == 1, na.rm = TRUE),
                                 min(.data[[wave_var]][Moved_in_Bay_w23 == 1], na.rm = TRUE), NA),
      first_move_out = ifelse(any(Moved_out_of_Bay_w23 == 1, na.rm = TRUE),
                              min(.data[[wave_var]][Moved_out_of_Bay_w23 == 1], na.rm = TRUE), NA),
      .groups = "drop"
    )

  # Only valid planners (planned before or same wave as first move)
  person_df <- person_df %>%
    mutate(
      valid_planner = !is.na(first_plan_wave) &
                     (is.na(first_move_any) | first_plan_wave <= first_move_any),
      moved_within = valid_planner & !is.na(first_move_within) & first_plan_wave <= first_move_within,
      moved_out = valid_planner & !is.na(first_move_out) & first_plan_wave <= first_move_out,
      moved_any = valid_planner & !is.na(first_move_any) & first_plan_wave <= first_move_any,
      no_move = valid_planner & is.na(first_move_any)
    )

  results <- data.frame(
    Group = c("Young", "Old", "Total"),
    stringsAsFactors = FALSE
  )

  # Calculate for each age group
  for (i in 1:2) {
    age_val <- i - 1
    age_data <- person_df %>% filter(agegroup == age_val)

    planners <- age_data %>% filter(valid_planner)
    n_planners <- nrow(planners)

    n_within <- sum(planners$moved_within, na.rm = TRUE)
    n_out <- sum(planners$moved_out, na.rm = TRUE)
    n_any <- sum(planners$moved_any, na.rm = TRUE)
    n_no_move <- sum(planners$no_move, na.rm = TRUE)

    results$N_Planned[i] <- n_planners
    results$Moved_Within[i] <- n_within
    results$Moved_Out[i] <- n_out
    results$Moved_Any[i] <- n_any
    results$No_Move[i] <- n_no_move
    results$Pct_Within[i] <- format_pct(n_within / n_planners)
    results$Pct_Out[i] <- format_pct(n_out / n_planners)
  }

  # Total
  planners_total <- person_df %>% filter(valid_planner)
  n_planners_total <- nrow(planners_total)

  n_within_total <- sum(planners_total$moved_within, na.rm = TRUE)
  n_out_total <- sum(planners_total$moved_out, na.rm = TRUE)
  n_any_total <- sum(planners_total$moved_any, na.rm = TRUE)
  n_no_move_total <- sum(planners_total$no_move, na.rm = TRUE)

  results$N_Planned[3] <- n_planners_total
  results$Moved_Within[3] <- n_within_total
  results$Moved_Out[3] <- n_out_total
  results$Moved_Any[3] <- n_any_total
  results$No_Move[3] <- n_no_move_total
  results$Pct_Within[3] <- format_pct(n_within_total / n_planners_total)
  results$Pct_Out[3] <- format_pct(n_out_total / n_planners_total)

  results
}

#' Create simplified planner destinations table (percentages only)
#' @param df Data frame
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param wave_var Name of wave variable (default: "WAVE")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @return Data frame with percentages only
create_planner_destinations_simple <- function(df, id_var = "prim_key", age_var = "agegroup",
                                                wave_var = "WAVE", plan_var = "Plan_to_Move") {

  full_table <- create_planner_destinations(df, id_var, age_var, wave_var, plan_var)

  # Calculate percentage who didn't move
  simple_table <- full_table %>%
    mutate(Pct_No_Move = format_pct(No_Move / N_Planned)) %>%
    select(Group, Pct_Within, Pct_Out, Pct_No_Move)

  # Rename for clarity
  names(simple_table) <- c("Group", "Moved Within Bay", "Moved Out of Bay", "Did Not Move")

  simple_table
}

# ==============================================================================
# MOVER PLANNING STATUS TABLE
# ==============================================================================

#' Create table showing planning status among all movers
#' @param df Data frame
#' @param id_var Person identifier variable (default: "prim_key")
#' @param age_var Name of age group variable (default: "agegroup")
#' @param wave_var Name of wave variable (default: "WAVE")
#' @param move_var Name of move variable (default: "Moved_w23")
#' @param plan_var Name of plan to move variable (default: "Plan_to_Move")
#' @return Data frame with planning status breakdown among all movers
create_mover_planning_status <- function(df, id_var = "prim_key", age_var = "agegroup",
                                          wave_var = "WAVE", move_var = "Moved_w23",
                                          plan_var = "Plan_to_Move") {

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

  # Categorize movers by planning status
  # Note: People who planned AFTER moving are counted as "not planned" because
  # they didn't plan THIS move (may be planning a future move)
  person_df <- person_df %>%
    mutate(
      mover_status = case_when(
        is.na(first_move_wave) ~ NA_character_,  # Not a mover
        !is.na(first_plan_wave) & first_plan_wave <= first_move_wave ~ "Planned",
        TRUE ~ "Not Planned"  # Includes never planned + planned after moving
      )
    )

  results <- data.frame(
    Group = c("Young", "Old", "Total"),
    stringsAsFactors = FALSE
  )

  # Calculate for each age group
  for (i in 1:2) {
    age_val <- i - 1
    age_data <- person_df %>% filter(agegroup == age_val)

    # Only movers
    movers <- age_data %>% filter(!is.na(first_move_wave))
    n_movers <- nrow(movers)

    n_planned <- sum(movers$mover_status == "Planned", na.rm = TRUE)
    n_not_planned <- sum(movers$mover_status == "Not Planned", na.rm = TRUE)

    results$Total_Movers[i] <- n_movers
    results$Planned[i] <- n_planned
    results$Not_Planned[i] <- n_not_planned
    results$Pct_Planned[i] <- format_pct(n_planned / n_movers)
  }

  # Total
  movers_total <- person_df %>% filter(!is.na(first_move_wave))
  n_movers_total <- nrow(movers_total)

  n_planned_total <- sum(movers_total$mover_status == "Planned", na.rm = TRUE)
  n_not_planned_total <- sum(movers_total$mover_status == "Not Planned", na.rm = TRUE)

  results$Total_Movers[3] <- n_movers_total
  results$Planned[3] <- n_planned_total
  results$Not_Planned[3] <- n_not_planned_total
  results$Pct_Planned[3] <- format_pct(n_planned_total / n_movers_total)

  results
}

# ==============================================================================
# USABLE CASES TABLE
# ==============================================================================

#' Create table showing usable cases for different model specifications
#' @param df Data frame (should be filtered to completed cases)
#' @param id_var Person identifier variable (default: "prim_key")
#' @param wave_var Wave variable (default: "WAVE")
#' @param age_var Age group variable (default: "agegroup")
#' @return Data frame with usable case counts by model type and age group
create_usable_cases_table <- function(df, id_var = "prim_key", wave_var = "WAVE",
                                       age_var = "agegroup") {

  # Get participation pattern per person
  person_waves <- df %>%
    group_by(.data[[id_var]]) %>%
    summarise(
      agegroup = first(.data[[age_var]]),
      n_waves = n_distinct(.data[[wave_var]]),
      has_w1 = any(.data[[wave_var]] == 1),
      has_w2 = any(.data[[wave_var]] == 2),
      has_w3 = any(.data[[wave_var]] == 3),
      .groups = "drop"
    )

  # Create usable case categories
  person_waves <- person_waves %>%
    mutate(
      all_3_waves = has_w1 & has_w2 & has_w3,
      waves_2_and_3 = has_w2 & has_w3,
      any_2_plus = n_waves >= 2
    )

  # Build results table
  results <- data.frame(
    Model_Type = c("All 3 waves (full panel)",
                   "Waves 2 & 3 (lagged models)",
                   "Any 2+ waves"),
    stringsAsFactors = FALSE
  )

  # Calculate counts by age group
  for (age_val in c(0, 1)) {
    age_label <- if (age_val == 0) "Young" else "Old"
    age_data <- person_waves %>% filter(agegroup == age_val)

    results[[age_label]] <- c(
      sum(age_data$all_3_waves),
      sum(age_data$waves_2_and_3),
      sum(age_data$any_2_plus)
    )
  }

  # Total column
  results$Total <- c(
    sum(person_waves$all_3_waves),
    sum(person_waves$waves_2_and_3),
    sum(person_waves$any_2_plus)
  )

  results
}

# ==============================================================================
# EXPORT FUNCTION
# ==============================================================================

#' Export descriptive table to CSV
#' @param table Data frame to export
#' @param filename Filename (without path)
#' @param out_dir Output directory (default: "out")
export_descriptive_table <- function(table, filename, out_dir = "out") {
  filepath <- file.path(out_dir, filename)
  write.csv(table, filepath, row.names = FALSE)
  cat(sprintf("Saved: %s\n", filepath))
  return(invisible(table))
}
