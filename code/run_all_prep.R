# run_all_prep.R
# Master script to run all data preparation notebooks (00-03)
# Renders the main notebook which sources all child documents

library(here)
library(rmarkdown)

cat("=======================================================================\n")
cat("Running all data preparation scripts\n")
cat("=======================================================================\n\n")

# Render the master notebook (which sources 01, 02, 03 as child documents)
rmarkdown::render(
  input = here("code", "00_create_long_data.Rmd"),
  output_dir = here("code"),
  quiet = FALSE
)

cat("\n=======================================================================\n")
cat("Data preparation complete!\n")
cat("Output saved to: data/out/R_DF.rds\n")
cat("=======================================================================\n")
