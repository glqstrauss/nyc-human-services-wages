# run_all.R
# Executes all replication steps in order.
# Run from the project root: Rscript replication/R/run_all.R

steps <- c(
  # Data preparation (0x)
  "replication/R/00_setup.R",
  "replication/R/01_acs_prep.R",
  # "replication/R/02_ces_prep.R",   # TODO
  "replication/R/03_qcew_prep.R",

  # Analysis (1x)
  "replication/R/10_demographics.R",
  "replication/R/11_wages.R",
  "replication/R/12_hardship.R",
  "replication/R/13_qcew_ranking.R",

  # Output (2x)
  "replication/R/20_figures.R"
)

for (step in steps) {
  message("\n── Running ", step, " ──────────────────────────────")
  source(here::here(step), echo = FALSE)
  message("   Done: ", step)
}

message("\nAll replication steps complete.")
