# 03_merge_external_data.R
# Merges external datasets into the LONG format panel data
# This script expects 'df' (long format) to already be loaded in the environment
# Called by 00_create_long_data.R
#
# Merge key: prim_key_wave (combination of prim_key and WAVE)

# ==============================================================================
# CREATE MERGE KEY IN MAIN DATA
# ==============================================================================
# Create prim_key_wave without underscore to match external data format
df <- df %>%
  mutate(prim_key_wave = paste0(as.character(prim_key), WAVE))

cat(sprintf("Created prim_key_wave merge key for %d observations\n", nrow(df)))

# ==============================================================================
# EXTERNAL DATA SOURCE 1: Steps to Make Friends (a19f)
# Open-ended responses about steps taken to meet new people
# With coded categories
# ==============================================================================
cat("\n--- Merging Steps to Make Friends (a19f) ---\n")

a19f <- read.csv("data/in/a19f_Master_with_prim_key.csv", stringsAsFactors = FALSE)
cat(sprintf("Loaded %d records\n", nrow(a19f)))

# Convert prim_key_wave to character for matching
a19f <- a19f %>%
  mutate(prim_key_wave = as.character(prim_key_wave))

# Check for duplicates
n_dups <- sum(duplicated(a19f$prim_key_wave))
if (n_dups > 0) {
  warning(sprintf("a19f has %d duplicate prim_key_wave values", n_dups))
}

# Rename and select columns
a19f <- a19f %>%
  rename(
    a19f_response = Response,
    a19f_religious = Religious.Community.Involvement,
    a19f_local_venue = Local.Venue.Visitation,
    a19f_volunteer = Secular.Volunteer.Service,
    a19f_online = Online.Social.Networking,
    a19f_workplace = Workplace.Socializing,
    a19f_athletic = Athletic.Engagement,
    a19f_cultural_hobby = Cultural.Hobby.Participation,
    a19f_neighborhood = Neighborhood.Integration,
    a19f_events_groups = Broader.Event.and.Group.Participation,
    a19f_through_connections = Friendship.Through.Connections,
    a19f_non_engagement = Non.Engagement.Preference,
    a19f_alternative = Alternative.Strategies,
    a19f_not_clear = Not.Clear...Uncodeable.Responses,
    a19f_no_match = No_Match
  ) %>%
  select(prim_key_wave, a19f_response,
         a19f_religious, a19f_local_venue, a19f_volunteer, a19f_online,
         a19f_workplace, a19f_athletic, a19f_cultural_hobby, a19f_neighborhood,
         a19f_events_groups, a19f_through_connections, a19f_non_engagement,
         a19f_alternative, a19f_not_clear, a19f_no_match)

# Check for unmatched records in right dataset (a19f records not in main data)
unmatched_right <- a19f %>%
  filter(!(prim_key_wave %in% df$prim_key_wave))

if (nrow(unmatched_right) > 0) {
  cat(sprintf("WARNING: %d records in a19f have no match in main data:\n", nrow(unmatched_right)))
  print(unmatched_right %>% select(prim_key_wave, a19f_response) %>% head(20))
  if (nrow(unmatched_right) > 20) {
    cat(sprintf("... and %d more\n", nrow(unmatched_right) - 20))
  }
} else {
  cat("All a19f records matched to main data.\n")
}

# Merge
n_before <- nrow(df)
df <- df %>%
  left_join(a19f, by = "prim_key_wave")
stopifnot(nrow(df) == n_before)

n_matched <- sum(!is.na(df$a19f_response))
cat(sprintf("Matched: %d of %d main observations (%.1f%%)\n",
            n_matched, nrow(df), 100 * n_matched / nrow(df)))

# ==============================================================================
# SUMMARY
# ==============================================================================
cat("\n--- External Data Merge Summary ---\n")
cat("Variables added from a19f (Steps to Make Friends):\n")
cat("  a19f_response           - Open-ended response text\n")
cat("  a19f_religious          - Religious Community Involvement\n")
cat("  a19f_local_venue        - Local Venue Visitation\n")
cat("  a19f_volunteer          - Secular Volunteer Service\n")
cat("  a19f_online             - Online Social Networking\n")
cat("  a19f_workplace          - Workplace Socializing\n")
cat("  a19f_athletic           - Athletic Engagement\n")
cat("  a19f_cultural_hobby     - Cultural-Hobby Participation\n")
cat("  a19f_neighborhood       - Neighborhood Integration\n")
cat("  a19f_events_groups      - Broader Event and Group Participation\n")
cat("  a19f_through_connections - Friendship Through Connections\n")
cat("  a19f_non_engagement     - Non-Engagement Preference\n")
cat("  a19f_alternative        - Alternative Strategies\n")
cat("  a19f_not_clear          - Not Clear / Uncodeable Responses\n")
cat("  a19f_no_match           - No Match flag\n")

cat("\nExternal data merge complete.\n")
