# Check Merge Quality for a19f and a19i
# Purpose: Verify that external data is correctly matching to main UCNets data

library(here)
library(dplyr)
library(haven)
library(readxl)

cat("=== Merge Quality Check ===\n\n")

# -----------------------------------------------------------------------------
# Load main data
# -----------------------------------------------------------------------------
df <- read_dta(here("data", "R_DF.dta"))
cat("Main data loaded:", nrow(df), "observations\n")

# Create prim_key_wave merge key (matching format in 03_merge_external_data.Rmd)
df <- df %>%
  mutate(prim_key_wave = paste0(as.character(prim_key), WAVE))

cat("Unique prim_key_wave in main data:", n_distinct(df$prim_key_wave), "\n\n")

# -----------------------------------------------------------------------------
# Check a19f (Steps to Make Friends)
# -----------------------------------------------------------------------------
cat("--- a19f (Steps to Make Friends) ---\n")

a19f <- read.csv(here("data", "in", "a19f_Master_with_prim_key.csv"), stringsAsFactors = FALSE)
a19f <- a19f %>%
  mutate(prim_key_wave = as.character(prim_key_wave))

cat("a19f records:", nrow(a19f), "\n")
cat("Unique prim_key_wave in a19f:", n_distinct(a19f$prim_key_wave), "\n")

# Check for duplicates in a19f
n_dups_a19f <- sum(duplicated(a19f$prim_key_wave))
if (n_dups_a19f > 0) {
  cat("WARNING: a19f has", n_dups_a19f, "duplicate prim_key_wave values\n")
  cat("Duplicate keys:\n")
  print(a19f$prim_key_wave[duplicated(a19f$prim_key_wave)])
}

# Match check
a19f_in_main <- sum(a19f$prim_key_wave %in% df$prim_key_wave)
a19f_not_in_main <- sum(!(a19f$prim_key_wave %in% df$prim_key_wave))

cat("\nMatch results:\n")
cat("  a19f records found in main data:", a19f_in_main,
    sprintf("(%.1f%%)\n", 100 * a19f_in_main / nrow(a19f)))
cat("  a19f records NOT in main data:", a19f_not_in_main, "\n")

if (a19f_not_in_main > 0) {
  cat("\nUnmatched a19f prim_key_wave values:\n")
  unmatched <- a19f %>% filter(!(prim_key_wave %in% df$prim_key_wave))
  print(head(unmatched %>% select(prim_key_wave), 20))
}

# Check what waves are in a19f
cat("\na19f wave distribution:\n")
a19f_waves <- a19f %>%
  mutate(wave = substr(prim_key_wave, nchar(prim_key_wave), nchar(prim_key_wave)))
print(table(a19f_waves$wave))

# -----------------------------------------------------------------------------
# Check a19i (Reasons for Moving)
# -----------------------------------------------------------------------------
cat("\n--- a19i (Reasons for Moving) ---\n")

a19i <- read_excel(here("data", "in", "Final_Merged_Codes_a19i_alternate.xlsx"))
a19i <- a19i %>%
  mutate(prim_key_wave = as.character(as.numeric(prim_key_wave)))

cat("a19i records:", nrow(a19i), "\n")
cat("Unique prim_key_wave in a19i:", n_distinct(a19i$prim_key_wave), "\n")

# Check for duplicates in a19i
n_dups_a19i <- sum(duplicated(a19i$prim_key_wave))
if (n_dups_a19i > 0) {
  cat("WARNING: a19i has", n_dups_a19i, "duplicate prim_key_wave values\n")
  cat("Duplicate keys:\n")
  print(a19i$prim_key_wave[duplicated(a19i$prim_key_wave)])
}

# Match check
a19i_in_main <- sum(a19i$prim_key_wave %in% df$prim_key_wave)
a19i_not_in_main <- sum(!(a19i$prim_key_wave %in% df$prim_key_wave))

cat("\nMatch results:\n")
cat("  a19i records found in main data:", a19i_in_main,
    sprintf("(%.1f%%)\n", 100 * a19i_in_main / nrow(a19i)))
cat("  a19i records NOT in main data:", a19i_not_in_main, "\n")

if (a19i_not_in_main > 0) {
  cat("\nUnmatched a19i prim_key_wave values:\n")
  unmatched_i <- a19i %>% filter(!(prim_key_wave %in% df$prim_key_wave))
  print(head(unmatched_i %>% select(prim_key_wave), 20))
}

# Check what waves are in a19i
cat("\na19i wave distribution:\n")
a19i_waves <- a19i %>%
  mutate(wave = substr(prim_key_wave, nchar(prim_key_wave), nchar(prim_key_wave)))
print(table(a19i_waves$wave))

# -----------------------------------------------------------------------------
# Check main data wave distribution
# -----------------------------------------------------------------------------
cat("\n--- Main data wave distribution ---\n")
print(table(df$WAVE))

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------
cat("\n=== Summary ===\n")
cat("a19f: ", a19f_in_main, "/", nrow(a19f), " matched (",
    sprintf("%.1f%%", 100 * a19f_in_main / nrow(a19f)), ")\n", sep = "")
cat("a19i: ", a19i_in_main, "/", nrow(a19i), " matched (",
    sprintf("%.1f%%", 100 * a19i_in_main / nrow(a19i)), ")\n", sep = "")
