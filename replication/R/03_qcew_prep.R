# 03_qcew_prep.R
# Download and clean QCEW annual average data for NYC.
#
# Downloads one CSV per county per year from the BLS QCEW API, aggregates
# across the five NYC counties, and saves a combined RDS.
#
# Raw CSVs are cached as replication/data/raw/qcew/{YEAR}_{FIPS}.csv
# so subsequent runs skip the download.
#
# Inputs:  BLS QCEW API (no key required)
# Outputs: replication/data/processed/qcew_nyc.rds

source(here::here("replication/R/00_setup.R"))

# ── Parameters ───────────────────────────────────────────────────────────────

QCEW_YEARS <- 2023  # extend as needed for longitudinal work
QCEW_DIR   <- file.path(RAW_DIR, "qcew")

NYC_FIPS <- c("36005", "36047", "36061", "36081", "36085")

# BLS QCEW API: annual averages by area
# https://www.bls.gov/cew/about-data/downloadable-file-layouts/annual/naics-based-annual-layout.htm
qcew_url <- function(year, fips) {
  sprintf("https://data.bls.gov/cew/data/api/%d/a/area/%s.csv", year, fips)
}

# ── Download ─────────────────────────────────────────────────────────────────

dir.create(QCEW_DIR, showWarnings = FALSE, recursive = TRUE)

for (yr in QCEW_YEARS) {
  for (fips in NYC_FIPS) {
    dest <- file.path(QCEW_DIR, sprintf("%d_%s.csv", yr, fips))
    if (file.exists(dest)) {
      message(sprintf("  Cached: %s", basename(dest)))
      next
    }
    url <- qcew_url(yr, fips)
    message(sprintf("  Downloading: %s", url))
    download.file(url, dest, quiet = TRUE)
    Sys.sleep(0.5)  # polite rate limiting
  }
}

# ── Read and combine ─────────────────────────────────────────────────────────

message("\nReading QCEW CSVs...")

raw_list <- list()
for (yr in QCEW_YEARS) {
  for (fips in NYC_FIPS) {
    path <- file.path(QCEW_DIR, sprintf("%d_%s.csv", yr, fips))
    df <- read_csv(path, show_col_types = FALSE) |>
      mutate(area_fips = as.character(area_fips))
    raw_list[[paste(yr, fips)]] <- df
  }
}

raw <- bind_rows(raw_list)
message(sprintf("  Total rows: %s", format(nrow(raw), big.mark = ",")))

# ── Filter to private sector, 4-digit NAICS ──────────────────────────────────
# own_code: 5 = private
# agglvl_code: 74 = county, NAICS sector (2-digit), private
#              75 = county, NAICS 3-digit, private
#              76 = county, NAICS 4-digit, private  ← what we want
#              77 = county, NAICS 5-digit, private
#              78 = county, NAICS 6-digit, private

qcew <- raw |>
  filter(
    own_code == 5,
    agglvl_code == 76
  ) |>
  select(
    year, area_fips, industry_code,
    avg_emplvl    = annual_avg_emplvl,
    avg_wkly_wage = annual_avg_wkly_wage,
    avg_annual_pay,
    total_wages   = total_annual_wages,
    establishments = annual_avg_estabs
  )

message(sprintf("  After private/4-digit filter: %s rows", format(nrow(qcew), big.mark = ",")))

# ── Aggregate across NYC counties ────────────────────────────────────────────
# Sum employment, total wages, and establishments across the 5 counties.
# Recompute average annual pay as total_wages / avg_emplvl.

nyc <- qcew |>
  group_by(year, industry_code) |>
  summarise(
    avg_emplvl     = sum(avg_emplvl),
    total_wages    = sum(total_wages),
    establishments = sum(establishments),
    .groups = "drop"
  ) |>
  mutate(
    avg_annual_pay = round(total_wages / avg_emplvl)
  )

message(sprintf("  NYC aggregated: %s industry-years", format(nrow(nyc), big.mark = ",")))

# ── Sector-level totals (all-private, government) ────────────────────────────
# agglvl_code 71 = County, Total, by Ownership

totals <- raw |>
  filter(agglvl_code == 71) |>
  select(
    year, area_fips, own_code,
    avg_emplvl    = annual_avg_emplvl,
    total_wages   = total_annual_wages
  ) |>
  group_by(year, own_code) |>
  summarise(
    avg_emplvl  = sum(avg_emplvl),
    total_wages = sum(total_wages),
    .groups = "drop"
  ) |>
  mutate(
    avg_annual_pay = round(total_wages / avg_emplvl),
    sector = case_when(
      own_code == 5 ~ "all_private",
      own_code == 3 ~ "local_govt",
      own_code == 1 ~ "federal_govt",
      own_code == 2 ~ "state_govt",
      TRUE ~ "other"
    )
  )

message("Sector-level totals:")
print(totals |> select(year, sector, avg_emplvl, avg_annual_pay))

# ── Save ─────────────────────────────────────────────────────────────────────

out_path <- file.path(PROC_DIR, "qcew_nyc.rds")
saveRDS(list(by_industry = nyc, totals = totals), out_path)
message(sprintf("Saved: %s", out_path))
