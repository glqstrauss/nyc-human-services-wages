# run_all.R
# Executes all replication steps in order.
# Run from the project root: Rscript replication/R/run_all.R

steps <- c(
  "replication/R/00_setup.R",
  "replication/R/01_acs_prep.R",
  "replication/R/02_demographics.R",
  "replication/R/03_wages.R",
  "replication/R/04_hardship.R",
  "replication/R/07_figures.R"
)

for (step in steps) {
  message("\n── Running ", step, " ──────────────────────────────")
  source(here::here(step), echo = FALSE)
  message("   Done: ", step)
}

message("\nAll replication steps complete.")

