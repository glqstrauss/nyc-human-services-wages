# 13_qcew_ranking.R
# Module 5: Industry-level wage ranking from QCEW data.
#
# Replicates Figure 9 from Parrott (2025): bar chart ranking the 10 largest
# low-paying 4-digit private-sector NAICS industries in NYC by average
# annual wage, highlighting Individual & Family Services (NAICS 6241) as
# the third-lowest at $36,688.
#
# Inputs:  replication/data/processed/qcew_nyc.rds (from 03_qcew_prep.R)
# Outputs: replication/data/processed/qcew_ranking.rds

source(here::here("replication/R/00_setup.R"))

# ── Load ─────────────────────────────────────────────────────────────────────

qcew_data <- readRDS(file.path(PROC_DIR, "qcew_nyc.rds"))
nyc    <- qcew_data$by_industry
totals <- qcew_data$totals

# ── Figure 9: Low-paying large industries ────────────────────────────────────
# Filter to 2023, industries with > 30,000 average employment,
# sort by average annual pay ascending, take the 10 lowest-paying.

TARGET_YEAR   <- 2023
MIN_EMPLVL    <- 30000
TOP_N_LOWEST  <- 10

ranking <- nyc |>
  filter(
    year == TARGET_YEAR,
    avg_emplvl > MIN_EMPLVL
  ) |>
  arrange(avg_annual_pay)

cat(sprintf(
  "\n=== All private-sector 4-digit industries with >%s employment, %d ===\n",
  format(MIN_EMPLVL, big.mark = ","), TARGET_YEAR
))
cat(sprintf("  Total industries: %d\n\n", nrow(ranking)))
print(ranking, n = nrow(ranking))

# Top N lowest-paying
bottom_n <- ranking |>
  slice_head(n = TOP_N_LOWEST)

cat(sprintf(
  "\n=== %d Lowest-Paying Large Industries (Figure 9) ===\n\n",
  TOP_N_LOWEST
))
print(bottom_n |> select(industry_code, avg_emplvl, avg_annual_pay))

# ── Key verification ─────────────────────────────────────────────────────────

ifs <- ranking |> filter(industry_code == "6241")
if (nrow(ifs) == 1) {
  cat(sprintf(
    "\nNAICS 6241 (Individual & Family Services):\n  Employment: %s\n  Avg annual pay: $%s\n  Rank (lowest): %d of %d\n",
    format(ifs$avg_emplvl, big.mark = ","),
    format(ifs$avg_annual_pay, big.mark = ","),
    which(ranking$industry_code == "6241"),
    nrow(ranking)
  ))
  cat("  Parrott target: $36,688 avg pay, rank 3rd lowest\n")
} else {
  warning("NAICS 6241 not found in filtered data")
}

# ── Also compute all-private and government benchmarks ───────────────────────
# These use own_code-level totals (agglvl_code differs), which aren't in our
# 4-digit-only extract. We report what we have and note the limitation.

cat("\n=== Reference: All-industry benchmarks ===\n")
benchmarks <- totals |>
  filter(year == TARGET_YEAR,
         sector %in% c("all_private", "local_govt")) |>
  select(sector, avg_emplvl, avg_annual_pay)
print(benchmarks)
cat("Parrott Figure 9: All Private=$116,863; Government=$96,189\n")

# ── Save ─────────────────────────────────────────────────────────────────────

qcew_tables <- list(
  ranking_all   = ranking,
  ranking_bottom = bottom_n,
  target_year   = TARGET_YEAR,
  min_emplvl    = MIN_EMPLVL
)

out_path <- file.path(PROC_DIR, "qcew_ranking.rds")
saveRDS(qcew_tables, out_path)
cat(sprintf("\nSaved: %s\n", out_path))
