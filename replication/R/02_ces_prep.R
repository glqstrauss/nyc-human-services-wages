# 02_ces_prep.R
# Download BLS Current Employment Statistics (CES) monthly series for NYC
# from FRED, and compute "core social assistance" employment
# (NAICS 624 minus 6244 child care).
#
# Replicates Figure 1 from Parrott (2025): employment growth index,
# 1990-2024 (168% growth in core social assistance vs 40% all private).
#
# Inputs:  FRED API (requires FRED_API_KEY env var)
# Outputs: replication/data/processed/ces_nyc.rds
#
# Raw series are cached as replication/data/raw/ces/{SERIES_ID}.rds

source(here::here("replication/R/00_setup.R"))

library(fredr)

# ── FRED API key ─────────────────────────────────────────────────────────────
# Set via environment variable or .Renviron:
#   FRED_API_KEY=your_key_here

if (Sys.getenv("FRED_API_KEY") == "") {
  stop("FRED_API_KEY not set. Get one at https://fred.stlouisfed.org/docs/api/api_key.html")
}
fredr_set_key(Sys.getenv("FRED_API_KEY"))

# ── Series IDs ───────────────────────────────────────────────────────────────
# All are NYC (New York City), seasonally adjusted, all employees (thousands)

CES_SERIES <- c(
  total_private  = "SMU36935610500000001SA",
  social_asst    = "PLACEHOLDER_NAICS_624",     # TODO: confirm series ID
  child_care     = "SMU36935616562440001SA",
  home_health    = "PLACEHOLDER_NAICS_6216"      # TODO: confirm series ID
)

CES_DIR <- file.path(RAW_DIR, "ces")
dir.create(CES_DIR, showWarnings = FALSE, recursive = TRUE)

# ── Download helper ──────────────────────────────────────────────────────────

fetch_fred <- function(series_id, label) {
  cache_path <- file.path(CES_DIR, paste0(series_id, ".rds"))

  if (file.exists(cache_path)) {
    message(sprintf("  Cached: %s (%s)", label, series_id))
    return(readRDS(cache_path))
  }

  message(sprintf("  Downloading: %s (%s)", label, series_id))
  df <- fredr(
    series_id          = series_id,
    observation_start  = as.Date("1990-01-01"),
    observation_end    = as.Date("2024-12-31"),
    frequency          = "m"
  ) |>
    select(date, value) |>
    mutate(series = label)

  saveRDS(df, cache_path)
  Sys.sleep(0.3)
  df
}

# ── Fetch all series ─────────────────────────────────────────────────────────

message("Fetching CES series from FRED...")

ces_list <- imap(CES_SERIES, function(id, label) {
  fetch_fred(id, label)
})

ces <- bind_rows(ces_list)

message(sprintf("  Total observations: %s", format(nrow(ces), big.mark = ",")))
message(sprintf("  Date range: %s to %s",
                min(ces$date), max(ces$date)))

# ── Compute core social assistance ───────────────────────────────────────────
# Core = Social Assistance (624) minus Child Day Care (6244)
# The report also notes that home health care (6216) is excluded from "core"
# but 6216 is classified under Health Care, not Social Assistance, so it
# doesn't need subtracting — it's already separate.

ces_wide <- ces |>
  select(date, series, value) |>
  pivot_wider(names_from = series, values_from = value)

ces_wide <- ces_wide |>
  mutate(
    core_social_asst = social_asst - child_care
  )

# ── Compute growth indices (base = Jan 1990) ─────────────────────────────────

base_vals <- ces_wide |>
  filter(date == as.Date("1990-01-01")) |>
  select(
    base_private = total_private,
    base_core    = core_social_asst
  )

ces_wide <- ces_wide |>
  mutate(
    idx_private = total_private / base_vals$base_private * 100,
    idx_core    = core_social_asst / base_vals$base_core * 100
  )

# ── Summary stats for replication check ──────────────────────────────────────

latest <- ces_wide |> filter(!is.na(idx_core)) |> slice_tail(n = 1)

cat("\n=== CES Employment Growth Index (1990 = 100) ===\n")
cat(sprintf(
  "Latest month: %s\n  Total private: %.1f (growth: %.0f%%)\n  Core social asst: %.1f (growth: %.0f%%)\n",
  latest$date,
  latest$idx_private, latest$idx_private - 100,
  latest$idx_core, latest$idx_core - 100
))
cat("Parrott target: ~168% growth core SA, ~40% all private\n")

# ── Save ─────────────────────────────────────────────────────────────────────

out_path <- file.path(PROC_DIR, "ces_nyc.rds")
saveRDS(ces_wide, out_path)
message(sprintf("\nSaved: %s", out_path))
