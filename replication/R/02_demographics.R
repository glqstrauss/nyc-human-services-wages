# =============================================================================
# 02_demographics.R
# Module 3: Workforce Demographic and Education Composition
#
# Replicates Figures 4-8 from:
#   Parrott (2025), "Moving Beyond COLAs to Salary Parity for New York City's
#   Nonprofit Human Services Workers," Center for New York City Affairs,
#   The New School, January 2025.
#
# Inputs:
#   replication/data/processed/acs_prepared.rds  (created by 01_acs_prep.R)
#
# Outputs:
#   replication/data/processed/demographics_tables.rds
#
# All estimates use srvyr survey design with PERWT weights. Confidence intervals
# are computed via survey_mean(vartype = "ci"). Cells with unweighted n < 50
# are suppressed with a warning.
# =============================================================================

# Resolve the directory containing this script so that 00_setup.R can be
# sourced regardless of whether the script is run interactively in RStudio
# or non-interactively via Rscript.
source(here::here("replication/R/00_setup.R"))

library(tidyverse)
library(srvyr)

# =============================================================================
# 1. Load prepared ACS data
# =============================================================================

message("Loading prepared ACS data...")
d <- readRDS(file.path(PROC_DIR, "acs_prepared.rds"))

message(sprintf("  Rows loaded: %s", format(nrow(d), big.mark = ",")))
message(sprintf("  Years:       %s", paste(sort(unique(d$MULTYEAR)), collapse = ", ")))
message(sprintf("  Sectors:     %s", paste(levels(d$sector), collapse = ", ")))

# =============================================================================
# 2. Helper utilities
# =============================================================================

# -----------------------------------------------------------------------------
# suppress_small_cells()
#
# Given a data frame of survey estimates that includes an unweighted count
# column `n_unweighted`, this function replaces estimate columns with NA and
# emits a warning for any row where n_unweighted < MIN_N.
#
# Args:
#   df          - data frame of estimates
#   min_n       - minimum unweighted cell size (default 50)
#   value_cols  - character vector of columns to suppress; defaults to all
#                 numeric columns except n_unweighted
# -----------------------------------------------------------------------------
suppress_small_cells <- function(df, min_n = 50, value_cols = NULL) {
  if (!"n_unweighted" %in% names(df)) {
    warning("suppress_small_cells: 'n_unweighted' column not found; skipping suppression.")
    return(df)
  }

  if (is.null(value_cols)) {
    value_cols <- names(df)[sapply(df, is.numeric) & !names(df) %in% c("n_unweighted", "n_weighted")]
  }

  small <- which(df$n_unweighted < min_n)

  if (length(small) > 0) {
    warning(sprintf(
      "Suppressing %d cell(s) with unweighted n < %d:\n%s",
      length(small), min_n,
      paste(capture.output(print(df[small, , drop = FALSE])), collapse = "\n")
    ))
    df[small, value_cols] <- NA_real_
  }

  df
}

# -----------------------------------------------------------------------------
# compute_share()
#
# Compute weighted share of a binary indicator within a grouped survey design.
# This is a general helper used by the table-specific functions below.
#
# Args:
#   svy_design  - srvyr survey design object (already filtered to the desired
#                 group/sector before calling this function)
#   indicator   - unquoted logical expression (e.g., SEX == 2)
#   group_label - character scalar added as a "group" column
#
# Returns a one-row tibble with columns:
#   group, share, share_low, share_upp, n_unweighted
# -----------------------------------------------------------------------------
compute_share <- function(svy_design, indicator, group_label) {
  ind <- enquo(indicator)

  svy_design |>
    summarise(
      share        = survey_mean(!!ind, vartype = "ci", na.rm = TRUE),
      n_unweighted = unweighted(n()),
      n_weighted   = round(survey_total(1, vartype = NULL))
    ) |>
    mutate(group = group_label, .before = 1)
}

# NOTE: For tables involving multi-level categorical variables (Tables 3-5),
# we use map_dfr() over each level rather than srvyr's survey_prop(), because
# the latter's list-column output requires extra unnesting steps that reduce
# clarity. Each table section below has its own purpose-built compute_*()
# function.

# =============================================================================
# 3. Build survey design objects
#
# We create two primary designs:
#   svy_full      - all NYC workers in the prepared data
#   svy_ft        - full-time workers only (UHRSWORK >= 35)
# =============================================================================

message("\nBuilding survey designs...")

svy_full <- d |>
  as_survey_design(weights = PERWT)

svy_ft <- d |>
  filter(full_time) |>
  as_survey_design(weights = PERWT)

# Sector subsets used repeatedly below (full-time workers)
# hs_nonprofit : core human services nonprofit workers
# all_private  : all private sector (for-profit + all nonprofits, including hs_nonprofit)
# govt         : all government workers

make_sector_subset <- function(svy, sector_expr) {
  svy |> filter({{ sector_expr }})
}

# =============================================================================
# 4. Table 1: Gender share
#
# Replicates Figure 4 (Parrott 2025):
#   Share of full-time workers who are women, comparing core human services
#   nonprofit workers to all NYC private sector workers.
#
# Groups:
#   hs_nonprofit   : is_hs == TRUE
#   all_private    : sector %in% c("priv_forprofit","priv_nonprofit_other",
#                                  "hs_nonprofit")
# =============================================================================

message("\nComputing Table 1: Gender share...")

compute_gender_share <- function(svy_design, group_label) {
  svy_design |>
    summarise(
      share_women  = survey_mean(SEX == 2, vartype = "ci", na.rm = TRUE),
      n_unweighted = unweighted(n()),
      n_weighted   = round(survey_total(1, vartype = NULL))
    ) |>
    mutate(group = group_label, .before = 1)
}

tbl1_hs <- svy_ft |>
  filter(is_hs == TRUE) |>
  compute_gender_share("hs_nonprofit")

tbl1_priv <- svy_ft |>
  filter(sector %in% c("priv_forprofit", "priv_nonprofit")) |>
  compute_gender_share("all_private")

tbl1 <- bind_rows(tbl1_hs, tbl1_priv) |>
  suppress_small_cells(value_cols = c("share_women", "share_women_low", "share_women_upp"))

cat("\n=== Table 1: Share Women (Full-Time Workers) ===\n")
cat("Replicates Figure 4 in Parrott (2025)\n\n")
tbl1 |>
  mutate(across(where(is.numeric) & !all_of(c("n_unweighted", "n_weighted")), ~ scales::percent(.x, accuracy = 0.1))) |>
  print()

# =============================================================================
# 5. Table 2: Race/ethnicity share
#
# Replicates Figure 5 (Parrott 2025):
#   Share of full-time workers who are workers of color, Black, Hispanic,
#   and Asian, comparing hs_nonprofit to all private sector.
# =============================================================================

message("\nComputing Table 2: Race/ethnicity share...")

compute_race_share <- function(svy_design, group_label) {
  svy_design |>
    summarise(
      share_poc      = survey_mean(race_eth != "white_nh", vartype = "ci", na.rm = TRUE),
      share_black    = survey_mean(race_eth == "black_nh",  vartype = "ci", na.rm = TRUE),
      share_hispanic = survey_mean(race_eth == "hispanic",  vartype = "ci", na.rm = TRUE),
      share_asian    = survey_mean(race_eth == "asian_nh",  vartype = "ci", na.rm = TRUE),
      n_unweighted   = unweighted(n()),
      n_weighted     = round(survey_total(1, vartype = NULL))
    ) |>
    mutate(group = group_label, .before = 1)
}

tbl2_hs <- svy_ft |>
  filter(is_hs == TRUE) |>
  compute_race_share("hs_nonprofit")

tbl2_priv <- svy_ft |>
  filter(sector %in% c("priv_forprofit", "priv_nonprofit")) |>
  compute_race_share("all_private")

tbl2 <- bind_rows(tbl2_hs, tbl2_priv) |>
  suppress_small_cells(
    value_cols = c(
      "share_poc",      "share_poc_low",      "share_poc_upp",
      "share_black",    "share_black_low",    "share_black_upp",
      "share_hispanic", "share_hispanic_low", "share_hispanic_upp",
      "share_asian",    "share_asian_low",    "share_asian_upp"
    )
  )

cat("\n=== Table 2: Race/Ethnicity Share (Full-Time Workers) ===\n")
cat("Replicates Figure 5 in Parrott (2025)\n\n")
tbl2 |>
  mutate(across(
    where(is.numeric) & !all_of(c("n_unweighted", "n_weighted")),
    ~ scales::percent(.x, accuracy = 0.1)
  )) |>
  print()

# =============================================================================
# 6. Table 3: Gender × Race share
#
# Replicates Figure 6 (Parrott 2025):
#   Share of full-time workers in each gender × race category:
#   white_men, white_women, men_of_color, women_of_color.
#   Comparing hs_nonprofit vs. all private sector.
# =============================================================================

message("\nComputing Table 3: Gender x Race share...")

compute_gender_race_share <- function(svy_design, group_label) {
  # Compute share of each gender_race category within the group
  levels_gr <- c("white_men", "white_women", "men_of_color", "women_of_color")

  map_dfr(levels_gr, function(lvl) {
    svy_design |>
      summarise(
        share        = survey_mean(gender_race == lvl, vartype = "ci", na.rm = TRUE),
        n_unweighted = unweighted(n()),
        n_weighted   = round(survey_total(1, vartype = NULL))
      ) |>
      mutate(gender_race = lvl, group = group_label, .before = 1)
  })
}

tbl3_hs <- svy_ft |>
  filter(is_hs == TRUE) |>
  compute_gender_race_share("hs_nonprofit")

tbl3_priv <- svy_ft |>
  filter(sector %in% c("priv_forprofit", "priv_nonprofit")) |>
  compute_gender_race_share("all_private")

tbl3 <- bind_rows(tbl3_hs, tbl3_priv) |>
  suppress_small_cells(value_cols = c("share", "share_low", "share_upp"))

cat("\n=== Table 3: Gender x Race Share (Full-Time Workers) ===\n")
cat("Replicates Figure 6 in Parrott (2025)\n\n")
tbl3 |>
  mutate(across(
    where(is.numeric) & !all_of(c("n_unweighted", "n_weighted")),
    ~ scales::percent(.x, accuracy = 0.1)
  )) |>
  print()

# =============================================================================
# 7. Table 4: Education share
#
# Replicates Figure 7 (Parrott 2025):
#   Share of full-time workers at each education level:
#   lths, hs, some_college, bachelors, postgrad.
#   Comparing hs_nonprofit vs. all private sector vs. govt.
# =============================================================================

message("\nComputing Table 4: Education share...")

compute_educ_share <- function(svy_design, group_label) {
  educ_levels <- c("lths", "hs", "some_college", "bachelors", "postgrad")

  map_dfr(educ_levels, function(lvl) {
    svy_design |>
      summarise(
        share        = survey_mean(as.character(educ_cat) == lvl,
                                   vartype = "ci", na.rm = TRUE),
        n_unweighted = unweighted(n()),
        n_weighted   = round(survey_total(1, vartype = NULL))
      ) |>
      mutate(educ_cat = lvl, group = group_label, .before = 1)
  })
}

tbl4_hs <- svy_ft |>
  filter(is_hs == TRUE) |>
  compute_educ_share("hs_nonprofit")

tbl4_priv <- svy_ft |>
  filter(sector %in% c("priv_forprofit", "priv_nonprofit")) |>
  compute_educ_share("all_private")

tbl4_govt <- svy_ft |>
  filter(sector == "govt") |>
  compute_educ_share("govt")

tbl4 <- bind_rows(tbl4_hs, tbl4_priv, tbl4_govt) |>
  # Restore ordered factor for display ordering
  mutate(educ_cat = factor(
    educ_cat,
    levels = c("lths", "hs", "some_college", "bachelors", "postgrad"),
    ordered = TRUE
  )) |>
  arrange(group, educ_cat) |>
  suppress_small_cells(value_cols = c("share", "share_low", "share_upp"))

cat("\n=== Table 4: Education Share (Full-Time Workers) ===\n")
cat("Replicates Figure 7 in Parrott (2025)\n\n")
tbl4 |>
  mutate(across(
    where(is.numeric) & !all_of(c("n_unweighted", "n_weighted")),
    ~ scales::percent(.x, accuracy = 0.1)
  )) |>
  print()

# =============================================================================
# 8. Table 5: Full-time vs. part-time share
#
# Replicates Figure 8 (Parrott 2025):
#   Share of all workers (not filtered to full-time) working full-time vs.
#   part-time, comparing hs_nonprofit to all private sector.
#
# Note: This table uses svy_full (all workers) not svy_ft.
# =============================================================================

message("\nComputing Table 5: Full-time vs. part-time share...")

compute_ft_share <- function(svy_design, group_label) {
  svy_design |>
    summarise(
      share_fulltime = survey_mean(full_time,  vartype = "ci", na.rm = TRUE),
      share_parttime = survey_mean(!full_time, vartype = "ci", na.rm = TRUE),
      n_unweighted   = unweighted(n()),
      n_weighted     = round(survey_total(1, vartype = NULL))
    ) |>
    mutate(group = group_label, .before = 1)
}

tbl5_hs <- svy_full |>
  filter(is_hs == TRUE) |>
  compute_ft_share("hs_nonprofit")

tbl5_priv <- svy_full |>
  filter(sector %in% c("priv_forprofit", "priv_nonprofit")) |>
  compute_ft_share("all_private")

tbl5 <- bind_rows(tbl5_hs, tbl5_priv) |>
  suppress_small_cells(
    value_cols = c(
      "share_fulltime", "share_fulltime_low", "share_fulltime_upp",
      "share_parttime", "share_parttime_low", "share_parttime_upp"
    )
  )

cat("\n=== Table 5: Full-Time vs. Part-Time Share (All Workers) ===\n")
cat("Replicates Figure 8 in Parrott (2025)\n\n")
tbl5 |>
  mutate(across(
    where(is.numeric) & !all_of(c("n_unweighted", "n_weighted")),
    ~ scales::percent(.x, accuracy = 0.1)
  )) |>
  print()

# =============================================================================
# 9. Collect all tables into a named list and save
# =============================================================================

message("\nSaving demographics tables...")

demographics_tables <- list(
  # Figure 4: gender share by sector (full-time workers)
  tbl1_gender_share       = tbl1,

  # Figure 5: race/ethnicity share by sector (full-time workers)
  tbl2_race_share         = tbl2,

  # Figure 6: gender x race share by sector (full-time workers)
  tbl3_gender_race_share  = tbl3,

  # Figure 7: education share by sector (full-time workers)
  tbl4_educ_share         = tbl4,

  # Figure 8: full-time vs. part-time share by sector (all workers)
  tbl5_ft_share           = tbl5
)

out_path <- file.path(PROC_DIR, "demographics_tables.rds")
saveRDS(demographics_tables, out_path)
message(sprintf("  Saved: %s", out_path))

message("\n02_demographics.R complete.\n")
