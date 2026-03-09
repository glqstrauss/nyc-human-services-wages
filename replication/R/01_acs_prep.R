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
    COUNTYFIP %in% NYC_COUNTIES, # five NYC boroughs
    YEAR == CENSUS_YEAR, # 2022 census only (not 2023)
    EMPSTAT == 1L, # employed last week
    GQ %in% 1:2 # exclude group quarters (prisons, dorms)
  )

message(paste("After NYC/year/employed filters:", format(nrow(d), big.mark = ","), "rows."))

# ── 3. Sector variable ────────────────────────────────────────────────────────
# Based on CLASSWKRD (detailed class of worker)

d <- d |>
  mutate(
    sector = case_when(
      CLASSWKRD == CLASSWKRD_NONPROFIT ~ "hs_nonprofit", # renamed below
      CLASSWKRD == CLASSWKRD_FORPROFIT ~ "priv_forprofit",
      CLASSWKRD == CLASSWKRD_LOCAL_GOVT ~ "local_govt",
      CLASSWKRD %in% CLASSWKRD_OTHER_GOVT ~ "other_govt", 
      CLASSWKRD %in% c(13L, 14L) ~ "self_employed",
      TRUE ~ NA_character_
    ) |> factor()
  )

# ── 4. Core human services flag ───────────────────────────────────────────────
# Definition (Parrott 2025):
#   - Industry: Social Assistance (NAICS 624) + Residential Care (NAICS 623),
#     excluding Child Day Care Services (NAICS 6244)
#   - Sector: nonprofit only (CLASSWKRD == 23)
#
# `is_hs` is the primary sample flag for demographics/headcount (Figs 4-8).
# It does NOT exclude homecare occupations, matching the report's Figure 5
# total of ~60,095 workers.
#
# `is_hs_wages` further excludes home health aides, personal care aides, and
# nursing assistants. Used for wage analysis (Figs 10-15) because "including
# [homecare workers] would have skewed the wage distribution downward"
# (Parrott 2025, p. 9).

d <- d |>
  mutate(
    in_hs_industry = grepl(INDNAICS_HS_INDUSTRY, INDNAICS) &
      INDNAICS != INDNAICS_CHILD_CARE,
    homecare_occ = OCC %in% OCC_EXCLUDE_HOMECARE,
    is_hs = in_hs_industry &
      !is.na(sector) & sector == "hs_nonprofit",
    is_hs_wages = is_hs & !homecare_occ
  )

message(paste(
  "Core HS nonprofit (is_hs):",
  format(sum(d$is_hs), big.mark = ","), "unweighted,",
  format(round(sum(d$PERWT[d$is_hs])), big.mark = ","), "weighted."
))
message(paste(
  "  Wage-eligible (is_hs_wages, excl. homecare):",
  format(sum(d$is_hs_wages), big.mark = ","), "unweighted."
))

# Rename sector label for hs_nonprofit to be explicit
# (all nonprofit workers, not just HS; is_hs flag identifies HS nonprofits)
# sector == "hs_nonprofit" actually means "private nonprofit (any industry)"
# We'll use is_hs as the primary HS group; rename sector level for clarity
d <- d |>
  mutate(
    sector = fct_recode(sector, "priv_nonprofit" = "hs_nonprofit")
  )

# Flag private hospitals (comparison group for occupation-level tables)
d <- d |>
  mutate(
    priv_hosp = INDNAICS == INDNAICS_PRIV_HOSP & sector == "priv_forprofit"
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
  count(educ_cat, name = "n_unweighted")
message("Core HS nonprofit full-time workers by education (unweighted):")
print(check_cells)

check_occ <- d |>
  filter(is_hs, full_time) |>
  count(occ_group, name = "n_unweighted") |>
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
      sector %in% c("local_govt", "other_govt") ~ "govt",
      sector == "priv_forprofit" ~ "priv_forprofit",
      TRUE ~ NA_character_
    ) |> factor(levels = c("hs_nonprofit", "govt", "priv_forprofit")),
    analysis_sector_wages = case_when(
      is_hs_wages ~ "hs_nonprofit",
      sector %in% c("local_govt", "other_govt") ~ "govt",
      sector == "priv_forprofit" ~ "priv_forprofit",
      TRUE ~ NA_character_
    ) |> factor(levels = c("hs_nonprofit", "govt", "priv_forprofit"))
  )

message("analysis_sector distribution:")
print(table(d$analysis_sector, useNA = "always"))

# ── 12. Save ──────────────────────────────────────────────────────────────────

saveRDS(d, file.path(PROC_DIR, "acs_prepared.rds"))
message(paste("Saved acs_prepared.rds (", format(nrow(d), big.mark = ","), "rows)."))
