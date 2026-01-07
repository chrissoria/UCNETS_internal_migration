# Quick table: Average network size by strategy use with t-tests
library(here)
library(dplyr)
library(haven)

# Load a19f data
a19f_raw <- read.csv(here("data", "in", "a19f_Master_with_prim_key.csv"), stringsAsFactors = FALSE)
a19f_raw$prim_key_wave <- as.character(a19f_raw$prim_key_wave)

# Load main data with network variables
df_net <- read_dta(here("data", "R_DF.dta"))
df_net <- df_net %>%
  mutate(
    prim_key_wave = paste0(as.character(prim_key), WAVE),
    TotalKin = ImmedKin + ExtKin
  ) %>%
  select(prim_key_wave, NonKin, TotalKin)

# Merge
a19f_net <- a19f_raw %>%
  left_join(df_net, by = "prim_key_wave")

cat("=== Average Network Size by Strategy Use (with t-tests) ===\n\n")

# Strategies with their variable names
strategies <- list(
  "Workplace" = "Workplace.Socializing",
  "Through Connections" = "Friendship.Through.Connections",
  "Community Orgs" = c("Religious.Community.Involvement", "Secular.Volunteer.Service"),
  "Activities" = c("Athletic.Engagement", "Cultural.Hobby.Participation", "Broader.Event.and.Group.Participation"),
  "Local Places" = c("Local.Venue.Visitation", "Neighborhood.Integration"),
  "Online" = "Online.Social.Networking",
  "Non-Engagement" = "Non.Engagement.Preference",
  "Other/Unclear" = c("Alternative.Strategies", "Not.Clear...Uncodeable.Responses")
)

# Function to check if strategy is used
uses_strategy <- function(data, vars) {
  if (length(vars) == 1) {
    return(data[[vars]] == 1)
  } else {
    result <- rep(FALSE, nrow(data))
    for (v in vars) {
      result <- result | (data[[v]] == 1)
    }
    return(result)
  }
}

# Function to format p-value with stars
format_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  if (p < 0.10) return("†")
  return("")
}

# Calculate mean network size for users vs non-users with t-tests
results <- data.frame(Strategy = character(), stringsAsFactors = FALSE)

for (strat_name in names(strategies)) {
  vars <- strategies[[strat_name]]

  use_idx <- uses_strategy(a19f_net, vars)
  users <- a19f_net[use_idx, ]
  non_users <- a19f_net[!use_idx, ]

  # T-tests for NonKin and Kin
  ttest_nonkin <- t.test(users$NonKin, non_users$NonKin)
  ttest_kin <- t.test(users$TotalKin, non_users$TotalKin)

  results <- rbind(results, data.frame(
    Strategy = strat_name,
    N_Use = nrow(users),
    NonKin_Use = sprintf("%.2f", mean(users$NonKin, na.rm = TRUE)),
    NonKin_NoUse = sprintf("%.2f", mean(non_users$NonKin, na.rm = TRUE)),
    NonKin_p = ttest_nonkin$p.value,
    NonKin_sig = format_p(ttest_nonkin$p.value),
    Kin_Use = sprintf("%.2f", mean(users$TotalKin, na.rm = TRUE)),
    Kin_NoUse = sprintf("%.2f", mean(non_users$TotalKin, na.rm = TRUE)),
    Kin_p = ttest_kin$p.value,
    Kin_sig = format_p(ttest_kin$p.value),
    stringsAsFactors = FALSE
  ))
}

# Print header
cat(sprintf("%-20s %5s %8s %8s %4s   %8s %8s %4s\n",
            "Strategy", "N", "NK_Use", "NK_No", "sig", "Kin_Use", "Kin_No", "sig"))
cat(paste(rep("-", 80), collapse = ""), "\n")

# Print rows
for (i in 1:nrow(results)) {
  cat(sprintf("%-20s %5s %8s %8s %4s   %8s %8s %4s\n",
              results$Strategy[i],
              results$N_Use[i],
              results$NonKin_Use[i],
              results$NonKin_NoUse[i],
              results$NonKin_sig[i],
              results$Kin_Use[i],
              results$Kin_NoUse[i],
              results$Kin_sig[i]))
}

cat(paste(rep("-", 80), collapse = ""), "\n")
cat("\nSignificance: *** p<0.001, ** p<0.01, * p<0.05, † p<0.10\n")

# Overall means
cat(sprintf("\nOverall means: NonKin = %.2f, Kin = %.2f\n",
            mean(a19f_net$NonKin, na.rm = TRUE),
            mean(a19f_net$TotalKin, na.rm = TRUE)))

# Print full p-values
cat("\n\nFull p-values:\n")
cat(sprintf("%-20s %12s %12s\n", "Strategy", "NonKin_p", "Kin_p"))
for (i in 1:nrow(results)) {
  cat(sprintf("%-20s %12.4f %12.4f\n",
              results$Strategy[i],
              results$NonKin_p[i],
              results$Kin_p[i]))
}
