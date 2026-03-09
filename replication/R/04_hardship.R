# 04_hardship.R
# Module 6: Economic hardship indicators — SNAP receipt and poverty rates.
# Replicates figures from Parrott (2025) showing food insecurity and low income
# among core human services workers vs. public and private sector workers.
#
# Inputs:  replication/data/processed/acs_prepared.rds
# Outputs: replication/data/processed/hardship_tables.rds

source(here::here("replication/R/00_setup.R"))

# ── Load data ─────────────────────────────────────────────────────────────────

d <- readRDS(file.path(PROC_DIR, "acs_prepared.rds"))

# Hardship analysis uses ALL workers (full- and part-time), not just full-time,
# to capture the full economic reality of the workforce.
# Three-way sector comparison using analysis_sector.

d_hw <- d |>
  filter(!is.na(analysis_sector)) |>
  mutate(
    sector       = analysis_sector,
    snap_receipt = FOODSTMP == 2L,              # 2 = yes, received SNAP
    below_200pct = POVERTY < 200 & POVERTY > 0  # below 200% federal poverty line
    # Note: POVERTY == 0 means N/A (not in poverty universe); exclude.
  )

svy <- d_hw |>
  as_survey_design(weights = PERWT)

# ── Alternative universe definitions for HS nonprofit group (additive) ────────
# Narrow (current): is_hs == TRUE  = Social Asst nonprofit, excl. homecare OCC
# Broad1:           Social Asst nonprofit, INCLUDE homecare OCC
# Broad2:           All Social Asst workers, any sector, any OCC
#
# Note: `in_social_asst` and `sector` are available from acs_prepared.rds.
# `homecare_occ` is also available as a column.

d_broad1 <- d |>
  filter(in_social_asst, sector == "priv_nonprofit") |>   # same sector, add homecare
  mutate(
    snap_receipt = FOODSTMP == 2L,
    below_200pct = POVERTY < 200 & POVERTY > 0
  )

d_broad2 <- d |>
  filter(in_social_asst) |>                               # all sectors, all OCC
  mutate(
    snap_receipt = FOODSTMP == 2L,
    below_200pct = POVERTY < 200 & POVERTY > 0
  )

svy_broad1 <- d_broad1 |> as_survey_design(weights = PERWT)
svy_broad2 <- d_broad2 |> as_survey_design(weights = PERWT)

# ── Table H1: SNAP receipt rate by sector ─────────────────────────────────────
# Parrott finding: 19% of HS workers receive SNAP vs. 11% public, 14% all private.

cat("\n=== Table H1: SNAP Receipt Rate by Sector ===\n")

h1 <- svy |>
  group_by(sector) |>
  summarise(
    snap_rate    = survey_mean(snap_receipt, na.rm = TRUE, vartype = "ci"),
    n_unweighted = unweighted(n()),
    n_weighted   = round(survey_total(1, vartype = NULL))
  ) |>
  as_tibble() |>
  mutate(snap_rate_pct = round(snap_rate * 100, 1))

# Additive: broad1 and broad2 SNAP rates joined to hs_nonprofit row
h1_b1 <- svy_broad1 |>
  summarise(
    snap_rate_broad1     = survey_mean(snap_receipt, na.rm = TRUE, vartype = NULL),
    n_unweighted_broad1  = unweighted(n()),
    n_weighted_broad1    = round(survey_total(1, vartype = NULL))
  ) |> as_tibble() |>
  mutate(snap_rate_broad1_pct = round(snap_rate_broad1 * 100, 1),
         sector = factor("hs_nonprofit", levels = levels(h1$sector)))

h1_b2 <- svy_broad2 |>
  summarise(
    snap_rate_broad2     = survey_mean(snap_receipt, na.rm = TRUE, vartype = NULL),
    n_unweighted_broad2  = unweighted(n()),
    n_weighted_broad2    = round(survey_total(1, vartype = NULL))
  ) |> as_tibble() |>
  mutate(snap_rate_broad2_pct = round(snap_rate_broad2 * 100, 1),
         sector = factor("hs_nonprofit", levels = levels(h1$sector)))

h1 <- h1 |>
  left_join(h1_b1 |> select(sector, snap_rate_broad1_pct,
                             n_unweighted_broad1, n_weighted_broad1), by = "sector") |>
  left_join(h1_b2 |> select(sector, snap_rate_broad2_pct,
                             n_unweighted_broad2, n_weighted_broad2), by = "sector")

print(h1 |> select(sector, snap_rate_pct, snap_rate_broad1_pct, snap_rate_broad2_pct,
                   n_weighted, n_weighted_broad1, n_weighted_broad2))
cat("\nParrott target: hs_nonprofit ~19%, govt ~11%, priv_forprofit ~14%\n")
cat("broad1 = Social Asst nonprofit incl. homecare OCC\n")
cat("broad2 = All Social Asst workers any sector\n")

# ── Table H2: Below 200% poverty rate by sector ───────────────────────────────
# Parrott finding: 16% of HS workers below 200% FPL vs. 10% public sector.

cat("\n=== Table H2: Share Below 200% Federal Poverty Line by Sector ===\n")

h2 <- svy |>
  filter(POVERTY > 0) |>   # exclude N/A poverty universe
  group_by(sector) |>
  summarise(
    below_200pct_rate = survey_mean(below_200pct, na.rm = TRUE, vartype = "ci"),
    n_unweighted      = unweighted(n()),
    n_weighted        = round(survey_total(1, vartype = NULL))
  ) |>
  as_tibble() |>
  mutate(below_200pct_pct = round(below_200pct_rate * 100, 1))

# Additive: broad1 and broad2 poverty rates joined to hs_nonprofit row
h2_b1 <- svy_broad1 |>
  filter(POVERTY > 0) |>
  summarise(
    below_200pct_broad1     = survey_mean(below_200pct, na.rm = TRUE, vartype = NULL),
    n_unweighted_broad1     = unweighted(n()),
    n_weighted_broad1       = round(survey_total(1, vartype = NULL))
  ) |> as_tibble() |>
  mutate(below_200pct_broad1_pct = round(below_200pct_broad1 * 100, 1),
         sector = factor("hs_nonprofit", levels = levels(h2$sector)))

h2_b2 <- svy_broad2 |>
  filter(POVERTY > 0) |>
  summarise(
    below_200pct_broad2     = survey_mean(below_200pct, na.rm = TRUE, vartype = NULL),
    n_unweighted_broad2     = unweighted(n()),
    n_weighted_broad2       = round(survey_total(1, vartype = NULL))
  ) |> as_tibble() |>
  mutate(below_200pct_broad2_pct = round(below_200pct_broad2 * 100, 1),
         sector = factor("hs_nonprofit", levels = levels(h2$sector)))

h2 <- h2 |>
  left_join(h2_b1 |> select(sector, below_200pct_broad1_pct,
                             n_unweighted_broad1, n_weighted_broad1), by = "sector") |>
  left_join(h2_b2 |> select(sector, below_200pct_broad2_pct,
                             n_unweighted_broad2, n_weighted_broad2), by = "sector")

print(h2 |> select(sector, below_200pct_pct, below_200pct_broad1_pct, below_200pct_broad2_pct,
                   n_weighted, n_weighted_broad1, n_weighted_broad2))
cat("\nParrott target: hs_nonprofit ~16%, govt ~10%, all private ~19%\n")
cat("broad1 = Social Asst nonprofit incl. homecare OCC\n")
cat("broad2 = All Social Asst workers any sector\n")

# ── Save ──────────────────────────────────────────────────────────────────────

hardship_tables <- list(
  H1_snap_by_sector    = h1,
  H2_poverty_by_sector = h2
)

saveRDS(hardship_tables, file.path(PROC_DIR, "hardship_tables.rds"))
cat("\nSaved hardship_tables.rds\n")
