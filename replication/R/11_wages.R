# =============================================================================
# 11_wages.R
# Module 4: Wage Comparison Tables
#
# Computes all wage comparison tables replicating Parrott et al. findings on
# human services nonprofit worker wages relative to government and private
# for-profit workers.
#
# Inputs:  replication/data/processed/acs_prepared.rds
# Outputs: replication/data/processed/wage_tables.rds
# =============================================================================

source(here::here("replication/R/00_setup.R"))

# -----------------------------------------------------------------------------
# 0. Load data
# -----------------------------------------------------------------------------

acs <- readRDS(file.path(PROC_DIR, "acs_prepared.rds"))

# Wage analysis filter:
#   - Full-time workers only (UHRSWORK >= 35)
#   - Exclude top-coded wages (INCWAGE == 999999)
#   - Exclude zero or negative wages (INCWAGE > 0)
#
# NOTE on dollars: INCWAGE in the ACS refers to prior-year earnings. The
# 2018-2022 5-year sample is treated as "2022 dollars" in the Parrott report
# (pooled, no additional deflation applied).

acs_wages <- acs |>
  filter(
    full_time == TRUE,
    INCWAGE < 999999,
    INCWAGE > 0
  )

# ── CPI-U deflation to 2022 dollars (additive) ───────────────────────────────
# ACS INCWAGE = prior-year earnings; income year ≈ MULTYEAR − 1.
# CPI-U all items, annual average (BLS series CUUR0000SA0):
#   income 2017 (MULTYEAR 2018): 245.120   income 2020 (MULTYEAR 2021): 258.811
#   income 2018 (MULTYEAR 2019): 251.107   income 2021 (MULTYEAR 2022): 270.970
#   income 2019 (MULTYEAR 2020): 255.657   base year 2022:              292.655
CPI_FACTORS <- c(
  "2018" = 292.655 / 245.120,
  "2019" = 292.655 / 251.107,
  "2020" = 292.655 / 255.657,
  "2021" = 292.655 / 258.811,
  "2022" = 292.655 / 270.970
)
acs_wages <- acs_wages |>
  mutate(incwage_real = round(INCWAGE * CPI_FACTORS[as.character(MULTYEAR)]))

# Three comparison sectors used throughout all wage tables:
#   1. hs_nonprofit      — core HS nonprofit workers (is_hs_wages == TRUE;
#                           excludes homecare occupations)
#   2. govt              — all government workers
#   3. priv_forprofit    — all private for-profit workers
#
# analysis_sector_wages is constructed in 01_acs_prep.R; it uses is_hs_wages
# (which excludes homecare occupations from the HS nonprofit group) to avoid
# downward skew in the wage distribution. Workers outside these three groups
# have analysis_sector_wages == NA and are dropped here.
SECTORS_3 <- c("hs_nonprofit", "govt", "priv_forprofit")

acs_3sec <- acs_wages |>
  filter(!is.na(analysis_sector_wages)) |>
  mutate(sector = analysis_sector_wages)

# NOTE: survey::svyquantile has a version-compatibility issue with vctrs that
# breaks srvyr's survey_median() inside summarise(). We instead use a manual
# weighted median helper, which is equivalent for point estimates.
wt_median <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]
    w <- w[keep]
  }
  if (length(x) == 0) {
    return(NA_real_)
  }
  o <- order(x)
  x <- x[o]
  w <- w[o]
  cum <- cumsum(w) / sum(w)
  x[which(cum >= 0.5)[1]]
}

# -----------------------------------------------------------------------------
# Helper: compute gap relative to hs_nonprofit
#   gap = (comparison_median - hs_median) / comparison_median * 100
#   (positive = hs workers earn less than the comparison group)
# -----------------------------------------------------------------------------

compute_gap <- function(df, median_col = "median_wage") {
  hs_val <- df[[median_col]][df$sector == "hs_nonprofit"]
  df |>
    mutate(
      gap_vs_hs_pct = round(
        (.data[[median_col]] - hs_val) / .data[[median_col]] * 100,
        1
      )
    )
}

# =============================================================================
# Table W1: Overall median wage by sector
# Replicates: Parrott report — headline wage gap figure
# =============================================================================

cat("\n=== Table W1: Overall Median Wage by Sector ===\n")

w1 <- acs_3sec |>
  group_by(sector) |>
  summarise(
    median_wage      = wt_median(INCWAGE, PERWT),
    median_wage_real = wt_median(incwage_real, PERWT),
    n_unweighted     = n(),
    n_weighted       = round(sum(PERWT))
  ) |>
  arrange(factor(sector, levels = SECTORS_3)) |>
  compute_gap(median_col = "median_wage") |>
  mutate(
    gap_vs_hs_pct_real = round(
      (median_wage_real - median_wage_real[sector == "hs_nonprofit"]) /
        median_wage_real * 100, 1
    )
  )

cat("Median annual wage (2022 dollars), full-time workers:\n")
print(w1)
cat("\ngap_vs_hs_pct: positive = comparison sector earns more than hs_nonprofit\n")
cat("(gap = (comparison - hs) / comparison * 100)\n")

# =============================================================================
# Table W2: Median wage by education × sector
# Replicates: Parrott Figure 10
# Key finding: bachelor's = ~33% below for-profit, ~22% below govt;
#              postgrad    = ~37% below for-profit, ~29% below govt
# =============================================================================

cat("\n=== Table W2: Median Wage by Education × Sector (Figure 10) ===\n")

w2_long <- acs_3sec |>
  group_by(educ_cat, sector) |>
  summarise(
    median_wage = wt_median(INCWAGE, PERWT),
    median_wage_real = wt_median(incwage_real, PERWT),
    n_unweighted = n(),
    n_weighted = round(sum(PERWT)),
    .groups = "drop"
  )

# Pivot wide so we can compute gaps cleanly, then reshape for output
w2_wide <- w2_long |>
  select(educ_cat, sector, median_wage) |>
  pivot_wider(
    names_from = sector,
    values_from = median_wage,
    names_prefix = "median_"
  ) |>
  mutate(
    # Gap: (comparison - hs_nonprofit) / comparison * 100
    gap_vs_govt_pct = round(
      (median_govt - median_hs_nonprofit) / median_govt * 100, 1
    ),
    gap_vs_forprofit_pct = round(
      (median_priv_forprofit - median_hs_nonprofit) / median_priv_forprofit * 100, 1
    )
  )

# Attach unweighted n for hs_nonprofit (smallest cell)
w2_n <- w2_long |>
  filter(sector == "hs_nonprofit") |>
  select(educ_cat, n_hs_unweighted = n_unweighted, n_hs_weighted = n_weighted)

# CPI-deflated gaps by education (additive)
w2_wide_real <- w2_long |>
  select(educ_cat, sector, median_wage_real) |>
  pivot_wider(
    names_from = sector,
    values_from = median_wage_real,
    names_prefix = "median_real_"
  ) |>
  mutate(
    gap_vs_govt_pct_real = round(
      (median_real_govt - median_real_hs_nonprofit) / median_real_govt * 100, 1
    ),
    gap_vs_forprofit_pct_real = round(
      (median_real_priv_forprofit - median_real_hs_nonprofit) /
        median_real_priv_forprofit * 100, 1
    )
  )

w2 <- left_join(w2_wide, w2_n, by = "educ_cat") |>
  left_join(w2_wide_real, by = "educ_cat") |>
  arrange(educ_cat)

cat("Median annual wage by education level (full-time, 2022 dollars):\n")
print(w2)
cat("\nKey check — bachelor's level:\n")
print(filter(w2, educ_cat == "bachelors") |>
  select(educ_cat, starts_with("gap")))
cat("Key check — postgrad level:\n")
print(filter(w2, educ_cat == "postgrad") |>
  select(educ_cat, starts_with("gap")))
cat("(Parrott target: bachelor's ~33% below for-profit, ~22% below govt;\n")
cat("                 postgrad  ~37% below for-profit, ~29% below govt)\n")

# =============================================================================
# Table W3: Median wage by occupation × sector
# Replicates: Parrott Figures 12-13
#
# Three occupational comparison groups vs three sectors:
#   Sectors: hs_nonprofit, govt, priv_hospital
#     (priv_hospital = is_hs==FALSE & priv_hosp==TRUE)
#
# Occupations: social_workers, counselors, hs_assistants
#   + reference: admin_support, janitors_security
# =============================================================================

cat("\n=== Table W3: Median Wage by Occupation × Sector (Figures 12-13) ===\n")

OCC_GROUPS <- c(
  "social_workers", "counselors", "hs_assistants",
  "admin_support", "janitors"
)

# Build occupation-sector frame with three custom sectors:
#   hs_nonprofit  — core HS nonprofit (is_hs & sector == "hs_nonprofit")
#   govt          — government (sector == "govt")
#   priv_hospital — private hospital workers (is_hs==FALSE & priv_hosp==TRUE)
#
# For hs_nonprofit and govt we use only workers in the listed occ_groups.
# For priv_hospital there is no occ_group filter — we use the full private
# hospital population and sub-set only when computing gaps at occupation level.

occ_hs <- acs_wages |>
  filter(is_hs_wages == TRUE, occ_group %in% OCC_GROUPS) |>
  mutate(comp_sector = "hs_nonprofit")

occ_govt <- acs_wages |>
  filter(analysis_sector_wages == "govt", occ_group %in% OCC_GROUPS) |>
  mutate(comp_sector = "govt")

occ_hosp <- acs_wages |>
  filter(INDNAICS == "621M", CLASSWKRD == 22L) |>
  mutate(comp_sector = "priv_hospital")

occ_frame <- bind_rows(occ_hs, occ_govt, occ_hosp)

svy_occ <- occ_frame |>
  as_survey_design(weights = PERWT)

# Median by occ_group × comp_sector
# For priv_hospital, occ_group may be NA for some workers; we include all of
# them when comp_sector == "priv_hospital" and then separately compute the
# overall priv_hospital median (pooled across occupations) as the benchmark.

w3_by_occ <- occ_frame |>
  filter(!is.na(occ_group)) |>
  group_by(occ_group, comp_sector) |>
  summarise(
    median_wage = wt_median(INCWAGE, PERWT),
    median_wage_real = wt_median(incwage_real, PERWT),
    n_unweighted = n(),
    n_weighted = round(sum(PERWT)),
    .groups = "drop"
  )

# Overall priv_hospital median (all occupations combined) as reference
w3_hosp_overall <- occ_frame |>
  filter(comp_sector == "priv_hospital") |>
  summarise(
    median_wage      = wt_median(INCWAGE, PERWT),
    median_wage_real = wt_median(incwage_real, PERWT),
    n_unweighted     = n(),
    n_weighted       = round(sum(PERWT))
  ) |>
  mutate(occ_group = "all_occupations", comp_sector = "priv_hospital")

# Pivot wide for gap computation
w3_wide <- w3_by_occ |>
  select(occ_group, comp_sector, median_wage) |>
  pivot_wider(
    names_from = comp_sector,
    values_from = median_wage,
    names_prefix = "median_"
  ) |>
  mutate(
    gap_vs_govt_pct = round(
      (median_govt - median_hs_nonprofit) / median_govt * 100, 1
    ),
    gap_vs_priv_hosp_pct = round(
      (median_priv_hospital - median_hs_nonprofit) / median_priv_hospital * 100, 1
    )
  ) |>
  arrange(factor(occ_group, levels = OCC_GROUPS))

# Attach unweighted n for hs_nonprofit
w3_n <- w3_by_occ |>
  filter(comp_sector == "hs_nonprofit") |>
  select(occ_group, n_hs_unweighted = n_unweighted, n_hs_weighted = n_weighted)

# CPI-deflated gaps by occupation (additive)
w3_wide_real <- w3_by_occ |>
  select(occ_group, comp_sector, median_wage_real) |>
  pivot_wider(
    names_from = comp_sector,
    values_from = median_wage_real,
    names_prefix = "median_real_"
  ) |>
  mutate(
    gap_vs_govt_pct_real = round(
      (median_real_govt - median_real_hs_nonprofit) /
        median_real_govt * 100, 1
    ),
    gap_vs_priv_hosp_pct_real = round(
      (median_real_priv_hospital - median_real_hs_nonprofit) /
        median_real_priv_hospital * 100, 1
    )
  ) |>
  arrange(factor(occ_group, levels = OCC_GROUPS))

w3 <- left_join(w3_wide, w3_n, by = "occ_group") |>
  left_join(w3_wide_real, by = "occ_group")

cat("Median annual wage by occupation and sector (full-time, 2022 dollars):\n")
print(w3)
cat("\nPrivate hospital overall reference:\n")
print(w3_hosp_overall)
cat("\ngap_vs_govt_pct / gap_vs_priv_hosp_pct: positive = comparison earns more\n")

# =============================================================================
# Table W4: Median wage by occupation × education × sector
# Replicates: Parrott Figure 14
#
# Restrict to:
#   occ_group   %in% c("social_workers","counselors")
#   educ_cat    %in% c("bachelors","postgrad")
#   sector      %in% c("hs_nonprofit","govt")
#
# Flag cells with unweighted n < 100.
# =============================================================================

cat("\n=== Table W4: Median Wage by Occupation × Education × Sector (Figure 14) ===\n")

OCC_PROF <- c("social_workers", "counselors")
EDUC_HIGH <- c("bachelors", "postgrad")
SEC_2 <- c("hs_nonprofit", "govt")

w4_long <- acs_wages |>
  filter(
    occ_group %in% OCC_PROF,
    educ_cat %in% EDUC_HIGH,
    analysis_sector_wages %in% SEC_2
  ) |>
  mutate(
    occ_group = factor(occ_group, levels = OCC_PROF),
    educ_cat  = factor(as.character(educ_cat), levels = EDUC_HIGH),
    sector    = factor(as.character(analysis_sector_wages), levels = SEC_2)
  ) |>
  group_by(occ_group, educ_cat, sector) |>
  summarise(
    median_wage = wt_median(INCWAGE, PERWT),
    median_wage_real = wt_median(incwage_real, PERWT),
    n_unweighted = n(),
    n_weighted = round(sum(PERWT)),
    .groups = "drop"
  ) |>
  mutate(low_n_flag = n_unweighted < 100)

# Pivot sector wide for easy gap reading
w4_wide <- w4_long |>
  select(occ_group, educ_cat, sector, median_wage, n_unweighted, n_weighted, low_n_flag) |>
  pivot_wider(
    names_from   = sector,
    values_from  = c(median_wage, n_unweighted, n_weighted, low_n_flag),
    names_glue   = "{.value}_{sector}"
  ) |>
  mutate(
    gap_vs_govt_pct = round(
      (median_wage_govt - median_wage_hs_nonprofit) / median_wage_govt * 100, 1
    )
  ) |>
  arrange(occ_group, educ_cat)

cat("2x2x2 cross-tab: occupation × education × sector (full-time, 2022 dollars):\n")
print(w4_wide)
cat("\nCells flagged low_n_flag_* == TRUE have unweighted n < 100.\n")

w4 <- w4_wide # store for output list

# =============================================================================
# Table W5: Racial pay gaps within occupation × education × sector
# Replicates: Parrott Figure 15
#
# Restrict to:
#   occ_group %in% c("social_workers","counselors")  [combined]
#   educ_cat  %in% c("bachelors","postgrad")
#   sector    %in% c("hs_nonprofit","govt")
#
# Cross: poc (TRUE/FALSE) × educ_cat × sector
# Within-sector gap = (white_median - poc_median) / white_median * 100
# (positive = workers of color earn less than white workers)
# =============================================================================

cat("\n=== Table W5: Racial Pay Gaps (Figure 15) ===\n")

w5_long <- acs_wages |>
  filter(
    occ_group %in% OCC_PROF,
    educ_cat %in% EDUC_HIGH,
    analysis_sector_wages %in% SEC_2
  ) |>
  mutate(
    educ_cat  = factor(as.character(educ_cat), levels = EDUC_HIGH),
    sector    = factor(as.character(analysis_sector_wages), levels = SEC_2),
    race_grp  = if_else(poc, "poc", "white_nh")
  ) |>
  group_by(sector, educ_cat, race_grp) |>
  summarise(
    median_wage = wt_median(INCWAGE, PERWT),
    median_wage_real = wt_median(incwage_real, PERWT),
    n_unweighted = n(),
    n_weighted = round(sum(PERWT)),
    .groups = "drop"
  )

# Pivot race wide to compute within-sector gap
w5_wide <- w5_long |>
  select(sector, educ_cat, race_grp, median_wage, n_unweighted, n_weighted) |>
  pivot_wider(
    names_from  = race_grp,
    values_from = c(median_wage, n_unweighted, n_weighted),
    names_glue  = "{.value}_{race_grp}"
  ) |>
  mutate(
    # Racial wage gap: how far below white workers are workers of color
    racial_gap_pct = round(
      (median_wage_white_nh - median_wage_poc) / median_wage_white_nh * 100, 1
    ),
    # Absolute gap
    racial_gap_abs = round(median_wage_white_nh - median_wage_poc, 0),
    low_n_white_flag = n_unweighted_white_nh < 100,
    low_n_poc_flag = n_unweighted_poc < 100
  ) |>
  arrange(sector, educ_cat)

cat("Racial pay gap within social workers + counselors, by education and sector:\n")
cat("(positive racial_gap_pct = workers of color earn less than white workers)\n\n")
print(w5_wide)

w5 <- w5_wide # store for output list

# =============================================================================
# Save all tables to RDS
# =============================================================================

wage_tables <- list(
  W1_overall_by_sector        = w1,
  W2_by_educ_sector           = w2,
  W2_long                     = w2_long, # long form for plotting
  W3_by_occ_sector            = w3,
  W3_hosp_overall             = w3_hosp_overall,
  W4_occ_educ_sector          = w4,
  W4_long                     = w4_long, # long form (with low_n flag)
  W5_racial_gaps              = w5,
  W5_long                     = w5_long # long form for plotting
)

out_path <- file.path(PROC_DIR, "wage_tables.rds")
saveRDS(wage_tables, out_path)
cat("\nAll wage tables saved to:", out_path, "\n")

# -----------------------------------------------------------------------------
# Summary printout of key findings
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("KEY FINDINGS SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("W1 — Overall median wage gap:\n")
w1 |>
  filter(sector != "hs_nonprofit") |>
  select(sector, median_wage, gap_vs_hs_pct) |>
  print()

cat("\nW2 — Gap at bachelor's level:\n")
w2 |>
  filter(educ_cat == "bachelors") |>
  select(educ_cat, gap_vs_govt_pct, gap_vs_forprofit_pct) |>
  print()

cat("\nW2 — Gap at postgrad level:\n")
w2 |>
  filter(educ_cat == "postgrad") |>
  select(educ_cat, gap_vs_govt_pct, gap_vs_forprofit_pct) |>
  print()

cat("\nW5 — Racial gap within hs_nonprofit (social workers + counselors):\n")
w5 |>
  filter(sector == "hs_nonprofit") |>
  select(
    sector, educ_cat, median_wage_white_nh, median_wage_poc,
    racial_gap_pct, racial_gap_abs
  ) |>
  print()

cat("\nDone.\n")
