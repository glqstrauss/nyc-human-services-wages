# 01_acs_prep.R
# Load IPUMS ACS extract, filter to NYC 2018-2022, construct all derived
# variables used by downstream scripts. Saves acs_prepared.rds.
#
# Run time: ~2-3 min (reading compressed .dat.gz)

source(here::here("replication/R/00_setup.R"))

# ── 1. Load IPUMS extract ─────────────────────────────────────────────────────

message("Reading IPUMS DDI...")
ddi <- read_ipums_ddi(IPUMS_DDI)

message("Reading microdata...")
raw <- read_ipums_micro(ddi, verbose = FALSE)

message(paste("Loaded", format(nrow(raw), big.mark = ","), "rows total."))

# ── 2. Filter to NYC, 2018-2022 sample, employed civilians ───────────────────

d <- raw |>
  filter(
    STATEFIP == 36L, # New York State
    COUNTYFIP %in% c(5, 47, 61, 81, 85), # Bronx, Kings, New York, Queens, Richmond
    YEAR == 2022, # Data extract includes multiple surveys.
    EMPSTAT == 1L, # employed last week
    GQ %in% 1:2 # exclude group quarters (prisons, dorms)
  )

message(
  paste("After NYC/year/employed filters:", format(nrow(d), big.mark = ","), "rows.")
)

# ── 3. Sector variable ────────────────────────────────────────────────────────
# Based on CLASSWKRD (detailed class of worker)
# https://usa.ipums.org/usa-action/variables/CLASSWKR#codes_section

d <- d |>
  mutate(
    sector = case_when(
      CLASSWKRD %in% c(13L, 14L) ~ "self_employed",
      CLASSWKRD == 22L ~ "priv_forprofit",
      CLASSWKRD == 23L ~ "priv_nonprofit",
      CLASSWKRD %in% c(25L, 27L, 28L) ~ "govt",
      TRUE ~ NA_character_ # Capures NA and Unpaid Family Worker
    ) |> factor()
  )

# ── 4. Core human services flag ───────────────────────────────────────────────
# INDNAICS is an alphanumeric string in this extract.
#
# Core human services industry definition (Parrott 2025):
#   NAICS 624 — Social Assistance (6241, 6242, 6243, plus merged codes)
#   NAICS 623 — Residential Care Facilities (6231, 623M)
#   EXCLUDE:  6244 — Child Day Care Services
#
# The 623 codes capture group homes, residential mental health/substance abuse
# facilities, supportive housing, and I/DD residences — all major components
# of NYC human services contracting (DOHMH, DHS, HRA program areas).
#
# This broader definition yields ~61k weighted nonprofit workers, matching the
# report's Figure 5 total of 60,095. The original ^624-only definition yielded
# only ~48k, which was too narrow.
#
# `is_hs` is the primary sample flag for demographics/headcount (Figs 4-8).
# It does NOT exclude homecare occupations, matching the report's Figure 5
# total of ~60,095 workers.
#
# `is_hs_wages` further excludes home health aides, personal care aides, and
# nursing assistants. Used for wage analysis (Figs 10-15) because "including
# [homecare workers] would have skewed the wage distribution downward"
# (Parrott 2025, p. 9).
# Occupations to EXCLUDE from Social Assistance (home health / personal care)
# Per Parrott: "all home care and personal care aide occupational employment
# is excluded even when those workers appear in the individual and family
# services industry"

d <- d |>
  mutate(
    # Social Assistance (624*) + Residential Care (623*)  - Childcare (6244)
    in_hs_industry = grepl("^62[34]", INDNAICS) &
      INDNAICS != 6244,
    homecare_occ = OCC %in% c(
      3601L, # Home health aides
      3602L, # Personal care aides
      3603L # Nursing assistants
    ),
    is_hs = in_hs_industry &
      !is.na(sector) & sector == "priv_nonprofit",
    is_hs_wages = is_hs & !homecare_occ
  )

message(paste(
  "Core HS nonprofit (is_hs):",
  format(sum(d$is_hs), big.mark = ","), "unweighted,",
  format(round(sum(d$PERWT[d$is_hs])), big.mark = ","), "weighted."
))
message(paste(
  "  Wage-eligible (is_hs_wages, excl. homecare):",
  format(sum(d$is_hs_wages), big.mark = ","), "unweighted.",
  format(round(sum(d$PERWT[d$is_hs_wages])), big.mark = ","), "weighted."
))

# Flag private hospitals (comparison group for occupation-level tables)
# 621M: ACS PUMS merged hospital code (covers NAICS 6221?)
d <- d |>
  mutate(
    priv_hosp = INDNAICS == "621M" & sector == "priv_forprofit"
  )

# ── 5. Occupation group variable ─────────────────────────────────────────────

d <- d |>
  mutate(
    occ_group = case_when(
      OCC %in% OCC_SOCIAL_WORKERS ~ "social_workers",
      OCC %in% OCC_COUNSELORS ~ "counselors",
      OCC %in% OCC_HS_ASSISTANTS ~ "hs_assistants",
      OCC %in% OCC_MANAGERS ~ "managers",
      OCC %in% OCC_ADMIN_SUPPORT ~ "admin_support",
      OCC %in% OCC_JANITORS ~ "janitors",
      TRUE ~ NA_character_
    ) |> factor()
  )

# ── 6. Education category ─────────────────────────────────────────────────────
# EDUC general variable codes (see 00_setup.R for full codebook)

d <- d |>
  mutate(
    educ_cat = case_when(
      EDUC %in% 0:5 ~ "lths", # less than high school
      EDUC == 6 ~ "hs", # high school diploma / GED
      EDUC %in% 7:9 ~ "some_college", # some college or associate's degree
      EDUC == 10 ~ "bachelors", # four-year college degree
      EDUC == 11 ~ "postgrad", # master's degree or better
      TRUE ~ NA_character_
    ) |> factor(
      levels = c("lths", "hs", "some_college", "bachelors", "postgrad"),
      ordered = TRUE
    )
  )

# ── 7. Race / ethnicity ───────────────────────────────────────────────────────
# Priority: Hispanic (regardless of race), then non-Hispanic race categories.

d <- d |>
  mutate(
    race_eth = case_when(
      HISPAN != 0 ~ "hispanic",
      RACE == 1 ~ "white_nh",
      RACE == 2 ~ "black_nh",
      RACE %in% 4:6 ~ "asian_nh",
      TRUE ~ "other_nh"
    ) |> factor(),
    poc = race_eth != "white_nh", # person of color indicator
    female = SEX == 2L
  )

# ── 8. Gender × race combined variable ───────────────────────────────────────

d <- d |>
  mutate(
    gender_race = case_when(
      !poc & !female ~ "white_men",
      !poc & female ~ "white_women",
      poc & !female ~ "men_of_color",
      poc & female ~ "women_of_color"
    ) |> factor()
  )

# ── 9. Work intensity and wage ────────────────────────────────────────────────

d <- d |>
  mutate(
    full_time = UHRSWORK >= 35L,
    # Exclude top-coded wages and zero wages for wage analysis
    wage_valid = INCWAGE > 0L & INCWAGE < INCWAGE_TOPCODE
  )

# ── 10. Unweighted cell-size check ───────────────────────────────────────────

check_cells <- d |>
  filter(is_hs, full_time) |>
  summarize(n_unweighted = n(), n_weighted = sum(PERWT), .by = educ_cat)
message("Core HS nonprofit full-time workers by education (unweighted):")
print(check_cells)

check_occ <- d |>
  filter(is_hs, full_time) |>
  summarize(n_unweighted = n(), n_weighted = sum(PERWT), .by = occ_group) |>
  arrange(desc(n_unweighted))
message("Core HS nonprofit full-time workers by occupation group (unweighted):")
print(check_occ)

# ── 11. Three-way analysis sector variable ────────────────────────────────────
# Clean classification for all downstream demographic and wage tables:
#   "hs_nonprofit"   = core human services nonprofit workers
#   "govt"           = all government workers
#   "priv_forprofit" = all private for-profit workers (any industry)
# Workers not in these three groups (other nonprofits, self-employed) get NA.
#
# analysis_sector uses is_hs (broad, for demographics).
# analysis_sector_wages uses is_hs_wages (excl. homecare, for wage tables).

d <- d |>
  mutate(
    analysis_sector = case_when(
      is_hs ~ "hs_nonprofit",
      sector == "govt" ~ "govt",
      sector == "priv_forprofit" ~ "priv_forprofit",
      TRUE ~ NA_character_
    ) |> factor(levels = c("hs_nonprofit", "govt", "priv_forprofit")),
    analysis_sector_wages = case_when(
      is_hs_wages ~ "hs_nonprofit",
      sector == "govt" ~ "govt",
      sector == "priv_forprofit" ~ "priv_forprofit",
      TRUE ~ NA_character_
    ) |> factor(levels = c("hs_nonprofit", "govt", "priv_forprofit"))
  )

message("analysis_sector distribution:")
print(summarize(d, n_unweighted = n(), n_weighted = sum(PERWT), .by = analysis_sector))

# ── 12. Save ──────────────────────────────────────────────────────────────────

saveRDS(d, file.path(PROC_DIR, "acs_prepared.rds"))
message(paste("Saved acs_prepared.rds (", format(nrow(d), big.mark = ","), "rows)."))
