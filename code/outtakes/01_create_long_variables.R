# 01_create_long_variables.R
# Creates lagged, cumulative, transition, and derived variables for LONG format data
# This script expects 'df' (long format) to already be loaded in the environment
# Called by 00_create_long_data.R

# ==============================================================================
# LAGGED VARIABLES
# ==============================================================================
df <- df %>%
  group_by(prim_key) %>%
  mutate(
    lag_Kin_in_HH_N = lag(Kin_in_HH_N, order_by = WAVE),
    lag_Owns_Home = lag(Owns_home, order_by = WAVE),
    lag_Plan_Move = lag(Plan_to_Move, order_by = WAVE),
    lag_New_Job = lag(LE_NewJob, order_by = WAVE),
    lag_Full_Time = lag(Full_Time, order_by = WAVE),
    lag_LT_BA = lag(LT_BA, order_by = WAVE),
    lag_New_School = lag(LE_NewSchool, order_by = WAVE),
    lag_Finan_Prob = lag(LE_FinanProb, order_by = WAVE),
    lag_Moved_Town = lag(LE_MovedTown, order_by = WAVE),
    lag_Mother_in_Hour = lag(Mother_in_Hour, order_by = WAVE),
    lag_Father_in_Hour = lag(Father_in_Hour, order_by = WAVE),
    lag_Nonkin_in_HH_N = lag(Nonkin_in_HH_N, order_by = WAVE),
    lag_Kin_GT_Hour_N = lag(Kin_GT_Hour_N, order_by = WAVE),
    lag_Kin_in_5_N = lag(Kin_in_5_N, order_by = WAVE),
    lag_Nonkin_in_5_N = lag(Nonkin_in_5_N, order_by = WAVE),
    lag_Nonkin_GT_Hour_N = lag(Nonkin_GT_Hour_N, order_by = WAVE),
    lag_LEX_Birth = lag(LEX_Birth, order_by = WAVE),
    lag_Marr_Cohab = lag(Marr_Cohab, order_by = WAVE),
    lag_web = lag(web, order_by = WAVE)
  ) %>%
  ungroup()

# ==============================================================================
# PAST FINANCIAL PROBLEMS VARIABLE
# Captures financial problems before wave 3
# ==============================================================================
df <- df %>%
  mutate(
    # Start with lagged financial problems (0 if NA)
    past_finan_prob = if_else(lag_Finan_Prob == 1, 1, 0, missing = 0),
    # Add current financial problems (treat NA as 0 to avoid NA propagation)
    past_finan_prob = past_finan_prob + coalesce(LE_FinanProb, 0),
    # Cap at 1 (binary indicator)
    past_finan_prob = if_else(past_finan_prob >= 1, 1, 0)
  )

df <- df %>%
  group_by(prim_key) %>%
  mutate(past_finan_prob = if_else(WAVE == 3 & LEX_FinanProb == 1,
                                    past_finan_prob - 1,
                                    past_finan_prob)) %>%
  ungroup()

# ==============================================================================
# CUMULATIVE VARIABLES
# Track accumulated events that decay by 1 each period if event doesn't recur
# ==============================================================================
create_cumulative <- function(x) {
  n <- length(x)
  result <- numeric(n)
  if (n == 0) return(result)

  # Use base R if/else for scalar operation (not dplyr::if_else)
  result[1] <- if (is.na(x[1])) 0 else x[1]

  if (n > 1) {
    for (i in 2:n) {
      if (is.na(x[i])) {
        result[i] <- max(0, result[i-1] - 1)
      } else if (x[i] == 0) {
        result[i] <- max(0, result[i-1] - 1)
      } else {
        result[i] <- result[i-1] + x[i]
      }
    }
  }
  return(result)
}

df <- df %>%
  group_by(prim_key) %>%
  mutate(
    cu_LE_NewJob = create_cumulative(LE_NewJob),
    cu_LE_NewSchool = create_cumulative(LE_NewSchool),
    cu_past_finan_prob = create_cumulative(past_finan_prob),
    cu_Plan_to_Move = create_cumulative(Plan_to_Move),
    cu_LEX_Birth = create_cumulative(LEX_Birth),
    cu_LE_FinanProb = create_cumulative(LE_FinanProb)
  ) %>%
  ungroup()

# Rename cumulative variables
df <- df %>%
  rename(
    cu_NewJob = cu_LE_NewJob,
    cu_NewSchool = cu_LE_NewSchool,
    cu_FinanProb = cu_LE_FinanProb,
    cu_Birth = cu_LEX_Birth
  )

# Create lagged cumulative variables and adjust for those with value 2 at wave 3
create_lagged_cumulative <- function(df, var_name) {
  lag_var <- paste0("lag_", var_name)

  df <- df %>%
    group_by(prim_key) %>%
    mutate(!!lag_var := lag(.data[[var_name]], order_by = WAVE)) %>%
    ungroup()

  # Check if any individual has lag value of 2 at wave 3
  df <- df %>%
    group_by(prim_key) %>%
    mutate(
      has_lag_2 = any(.data[[lag_var]] == 2 & WAVE == 3, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(!!lag_var := if_else(has_lag_2 == TRUE,
                                 .data[[lag_var]] - 1,
                                 .data[[lag_var]])) %>%
    select(-has_lag_2)

  return(df)
}

df <- create_lagged_cumulative(df, "cu_NewJob")
df <- create_lagged_cumulative(df, "cu_NewSchool")
df <- create_lagged_cumulative(df, "cu_Plan_to_Move")
df <- create_lagged_cumulative(df, "cu_FinanProb")
df <- create_lagged_cumulative(df, "cu_Birth")

# ==============================================================================
# TRANSITION/DIRECTIONAL VARIABLES
# Capture status changes between waves
# Note: Transitions require both current and lagged values to be non-NA
# ==============================================================================
df <- df %>%
  mutate(
    # Marital status (simple indicator, NA if missing)
    Marr_Cohab_Only = if_else(Marr_Cohab == 1, 1, 0, missing = NA_real_),

    # Marital status transitions (require both current and lagged to be non-NA)
    Married_to_divorced = case_when(
      is.na(Marr_Cohab) | is.na(lag_Marr_Cohab) ~ NA_real_,
      lag_Marr_Cohab == 1 & Marr_Cohab == 0 ~ 1,
      TRUE ~ 0
    ),
    Not_to_Married = case_when(
      is.na(Marr_Cohab) | is.na(lag_Marr_Cohab) ~ NA_real_,
      lag_Marr_Cohab == 0 & Marr_Cohab == 1 ~ 1,
      TRUE ~ 0
    ),

    # Employment transitions
    Fulltime_to_less = case_when(
      is.na(Full_Time) | is.na(lag_Full_Time) ~ NA_real_,
      lag_Full_Time == 1 & Full_Time == 0 ~ 1,
      TRUE ~ 0
    ),
    Less_to_fulltime = case_when(
      is.na(Full_Time) | is.na(lag_Full_Time) ~ NA_real_,
      lag_Full_Time == 0 & Full_Time == 1 ~ 1,
      TRUE ~ 0
    ),

    # Housing transitions
    Homeowner_to_renter = case_when(
      is.na(Owns_home) | is.na(lag_Owns_Home) ~ NA_real_,
      lag_Owns_Home == 1 & Owns_home == 0 ~ 1,
      TRUE ~ 0
    ),
    Renter_to_homeowner = case_when(
      is.na(Owns_home) | is.na(lag_Owns_Home) ~ NA_real_,
      lag_Owns_Home == 0 & Owns_home == 1 ~ 1,
      TRUE ~ 0
    ),

    # Financial problem transitions
    FinanProb_to_not = case_when(
      is.na(LE_FinanProb) | is.na(lag_Finan_Prob) ~ NA_real_,
      lag_Finan_Prob == 1 & LE_FinanProb == 0 ~ 1,
      TRUE ~ 0
    ),
    Not_to_FinanProb = case_when(
      is.na(LE_FinanProb) | is.na(lag_Finan_Prob) ~ NA_real_,
      lag_Finan_Prob == 0 & LE_FinanProb == 1 ~ 1,
      TRUE ~ 0
    )
  )

# Create lagged transition variables
df <- df %>%
  group_by(prim_key) %>%
  mutate(
    lag_Married_to_divorced = lag(Married_to_divorced, order_by = WAVE),
    lag_Not_to_Married = lag(Not_to_Married, order_by = WAVE),
    lag_Fulltime_to_less = lag(Fulltime_to_less, order_by = WAVE),
    lag_Less_to_fulltime = lag(Less_to_fulltime, order_by = WAVE),
    lag_Homeowner_to_renter = lag(Homeowner_to_renter, order_by = WAVE),
    lag_Renter_to_homeowner = lag(Renter_to_homeowner, order_by = WAVE),
    lag_FinanProb_to_not = lag(FinanProb_to_not, order_by = WAVE),
    lag_Not_to_FinanProb = lag(Not_to_FinanProb, order_by = WAVE)
  ) %>%
  ungroup()

# ==============================================================================
# DERIVED VARIABLES
# ==============================================================================

# Employed variable
df <- df %>%
  mutate(
    employed = case_when(
      is.na(mode) ~ NA_real_,
      Full_Time == 1 | Part_Time == 1 ~ 1,
      TRUE ~ 0
    )
  )

# Adjust Moved_w23 for Wave 1 based on a19c (time at current residence)
# a19c <= 2 means 2 years or less at current residence (moved recently)
# a19c >= 3 means more than 2 years at current residence (did not move recently)
df <- df %>%
  mutate(
    Moved_w23 = case_when(
      WAVE == 1 & a19c >= 3 ~ 0,  # 3+ years at residence = no recent move
      WAVE == 1 & a19c <= 2 ~ 1,  # 0-2 years at residence = recent move
      TRUE ~ Moved_w23
    )
  )

# Combined network proximity variables
df <- df %>%
  mutate(
    Kin_within_60_N = Kin_in_5_N + Kin_in_5_to_60_N,
    Nonkin_within_60_N = Nonkin_in_5_N + Nonkin_in_5_to_60_N
  ) %>%
  group_by(prim_key) %>%
  mutate(
    lag_Kin_within_60_N = lag(Kin_within_60_N, order_by = WAVE),
    lag_Nonkin_within_60_N = lag(Nonkin_within_60_N, order_by = WAVE)
  ) %>%
  ungroup()

# Fix specific observations for Moved_out_of_Bay_w23
# Use character strings to preserve exact integer values (avoids floating point precision issues)
problem_ids <- c("5000000000027", "6000000003997", "7000000003067",
                 "8000000000852", "8000000000885")

df <- df %>%
  mutate(
    Moved_out_of_Bay_w23 = if_else(
      as.character(prim_key) %in% problem_ids & WAVE == 3,
      0,
      Moved_out_of_Bay_w23
    )
  )

# Create variable aliases (some analysis scripts use names without _N suffix)
df <- df %>%
  mutate(
    Kin_GT_Hour = Kin_GT_Hour_N,
    Nonkin_GT_Hour = Nonkin_GT_Hour_N,
    lag_Kin_GT_Hour = lag_Kin_GT_Hour_N,
    lag_Nonkin_GT_Hour = lag_Nonkin_GT_Hour_N
  )

cat("Variable creation complete.\n")
