# 02_acs_long_prep.R
# Load IPUMS ACS extract, filter to NYC 2018-2022, construct all derived
# variables used by downstream scripts. Saves acs_prepared.rds.

source(here::here("replication/R/00_setup.R"))

# ── Load IPUMS extract ─────────────────────────────────────────────────────

message("Reading IPUMS DDI...")
ddi <- read_ipums_ddi(IPUMS_DDI)

message("Reading microdata...")
raw <- read_ipums_micro(ddi, verbose = FALSE)

message(paste("Loaded", format(nrow(raw), big.mark = ","), "rows total."))

# ── Filter to NYC, 2018-2022 sample, employed civilians ───────────────────

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

# ── Sector variable ────────────────────────────────────────────────────────--
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
    ) |> factor(),
  )

# ── Core human services flag ───────────────────────────────────────────────
# INDNAICS is an alphanumeric string in this extract.
#
# Core human services industry definition (Parrott 2025):
#   NAICS 624 — Social Assistance (6241, 6242, 6243, plus merged codes)
#   NAICS 623 — Residential Care Facilities (6231, 623M)
#   EXCLUDE:  6244 — Child Day Care Services
#
# 62142 is Outpatient Mental Health and Substance Abuse Centers,
# which cannot be included because it is merged into 6214 ("Outpatient Care Centers")
# which includes such disparate industries as Kidney Dialysis and HMO Medical Centers.
# The breakdown of workers in this merged code that we must exclude is:
#   sector             n   nwt
#   <fct>          <int> <dbl>
# 1 govt             109  2915
# 2 priv_forprofit  1053 26736
# 3 priv_nonprofit   407 10204
# 4 self_employed    137  3466
# 5 NA                 5   110
#
# The 623 codes capture group homes, residential mental health/substance abuse
# facilities, supportive housing, and I/DD residences — all major components
# of NYC human services contracting (DOHMH, DHS, HRA program areas).
#
# This broader definition yields ~61k weighted nonprofit workers, matching the
# report's Figure 5 total of 60,095. The original ^624-only definition yielded
# only ~48k, which was too narrow.
#
# `parrott_hs` is the primary sample flag for demographics/headcount (Figs 4-8).
# It does NOT exclude homecare occupations, matching the report's Figure 5
# total of ~60,095 workers.
#
# `parrott_hs_wages` further excludes home health aides, personal care aides, and
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
    is_hs_industry = grepl("^62[34]", INDNAICS) &
      INDNAICS != 6244,
    occ_group = case_when(
      OCC %in% OCC_SOCIAL_WORKERS ~ "social_workers",
      OCC %in% OCC_COUNSELORS ~ "counselors",
      OCC %in% OCC_HS_ASSISTANTS ~ "hs_assistants",
      OCC %in% OCC_MANAGERS ~ "managers",
      OCC %in% OCC_ADMIN_SUPPORT ~ "admin_support",
      OCC %in% OCC_JANITORS ~ "janitors",
      # homecare will be *excluded* from wage analysis, but not demo analysis
      OCC %in% OCC_HOMECARE ~ "homecare",
      TRUE ~ NA_character_
    ) |> factor(),
    is_hs_occ = !is.na(occ_group),
    is_hs_occ_nh = !is.na(occ_group) & occ_group != "homecare"
  )

d <- d |>
  mutate(
    occ_name = as.character(haven::as_factor(US2022C_OCCP)),
    occ_class = substr(occ_name, 1, 3)
  )

d <- d |>
  mutate(
    # https://www.bls.gov/tus/iocodes/census18ocodes.pdf
    # This is a subset manual coding of the full classification,
    # focused on occupations that dont match is_hs_occ, in order
    # to say something about the large "other" category of non-core
    # occupations in the report.
    occ_class_desc = case_match(
      occ_class,
      "MGR" ~ "Managers",
      "BUS" ~ "Business Operations",
      "FIN" ~ "Financial Operations",
      "CMM" ~ "Computer and Mathematical",
      "SCI" ~ "Life, Physical, and Social Science",
      "LGL" ~ "Legal",
      "EDU" ~ "Educational Instruction and Library",
      "ENT" ~ "Arts, Design, Entertainment, Sports, and Media",
      "MED" ~ "Healthcare Practitioners and Technical",
      "HLS" ~ "Healthcare Support",
      "PRT" ~ "Protective Service",
      "EAT" ~ "Food Preparation and Serving",
      "CLN" ~ "Building and Grounds Cleaning and Maintenance",
      "PRS" ~ "Personal Care and Service",
      "OFF" ~ "Office and Administrative Support",
      "TRN" ~ "Transportation and Material Moving",
      .default = NA_character_
    ),
    occ_broad_class = case_match(
      occ_class,
      c("BUS", "FIN", "LGL") ~ "Business, Financial, and Legal",
      c("EAT", "CLN", "TRN", "PRT") ~ "Service and Maintenance",
      c("MED", "HLS") ~ "Healthcare",
      .default = occ_class_desc # if not grouped, use original classification
    )
  )

# ── Sample flags for Parrott replication ───────────────────────────────────────────--

d <- d |> mutate(
  parrott_hs = is_hs_industry &
    !is.na(sector) & sector == "priv_nonprofit",
  parrott_hs_wages = parrott_hs & occ_group != "homecare"
)

message(paste(
  "Core HS nonprofit (parrott_hs):",
  format(sum(d$parrott_hs), big.mark = ","), "unweighted,",
  format(round(sum(d$PERWT[d$parrott_hs])), big.mark = ","), "weighted."
))
message(paste(
  "  Wage-eligible (parrott_hs_wages, excl. homecare):",
  format(sum(d$parrott_hs_wages), big.mark = ","), "unweighted.",
  format(round(sum(d$PERWT[d$parrott_hs_wages])), big.mark = ","), "weighted."
))

# Flag private hospitals (comparison group for occupation-level tables)
# 621M: ACS PUMS merged hospital code (covers NAICS 6221?)
d <- d |>
  mutate(
    priv_hosp = INDNAICS == "621M" & sector == "priv_forprofit"
  )

# ── Education category ─────────────────────────────────────────────────────--
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

# ── Imputed experience ─────────────────────────────────────────────────────--

# https://economics.stackexchange.com/questions/53650
# Added

d <- d |>
  mutate(
    est_age_started_work = case_when(
      educ_cat == "lths" ~ 17L,
      educ_cat == "hs" ~ 19L,
      educ_cat == "some_college" ~ 21L,
      educ_cat == "bachelors" ~ 23L,
      educ_cat == "postgrad" ~ 25L,
    ),
    experience = AGE - est_age_started_work,
    experience_cat = case_when(
      experience < 5 ~ "0-5 years",
      experience < 10 ~ "5-10 years",
      experience < 20 ~ "10-20 years",
      experience >= 20 ~ "20+ years",
      TRUE ~ NA_character_
    ) |> factor(
      levels = c("0-5 years", "5-10 years", "10-20 years", "20+ years"),
      ordered = TRUE
    )
  ) |>
  select(-est_age_started_work)

# ── Race / ethnicity ───────────────────────────────────────────────────────--

d <- d |>
  mutate(
    race_eth = case_when(
      HISPAN != 0 ~ "hispanic", # regardless of race
      RACE == 1 ~ "white_nh",
      RACE == 2 ~ "black_nh",
      RACE %in% 4:6 ~ "asian_nh",
      TRUE ~ "other_nh"
    ) |> factor(),
    poc = race_eth != "white_nh",
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
    part_time = UHRSWORK > 0L & UHRSWORK < 35L,
    # Exclude top-coded wages and zero wages for wage analysis
    wage_valid = INCWAGE > 0L & INCWAGE < INCWAGE_TOPCODE
  )

# ── 10. Analysis Sector Flags ───────────────────────────────────────────

d <- d |> mutate(
  is_city_wkr = CLASSWKRD == 28L, # Local Government sector
  is_priv_fp_wkr = sector == "priv_forprofit",
  is_priv_np_wkr = sector == "priv_nonprofit",
  # for govt-level breakdown justifying restriction to city workers...
  govt_level = case_when(
    CLASSWKRD == 25L ~ "federal",
    CLASSWKRD == 27L ~ "state",
    CLASSWKRD == 28L ~ "local",
    TRUE ~ NA_character_
  )
)

# ── 11. Unweighted cell-size check ───────────────────────────────────────────

check_cells <- d |>
  filter(parrott_hs, full_time) |>
  summarize(n_unweighted = n(), n_weighted = sum(PERWT), .by = educ_cat)
message("Core HS nonprofit full-time workers by education (unweighted):")
print(check_cells)

check_occ <- d |>
  filter(parrott_hs, full_time) |>
  summarize(n_unweighted = n(), n_weighted = sum(PERWT), .by = occ_group) |>
  arrange(desc(n_unweighted))
message("Core HS nonprofit full-time workers by occupation group (unweighted):")
print(check_occ)

# ── Three-way analysis sector variable ────────────────────────────────────
# Clean classification for all downstream demographic and wage tables:
#   "hs_nonprofit"   = core human services nonprofit workers
#   "govt"           = all government workers
#   "priv_forprofit" = all private for-profit workers (any industry)
# Workers not in these three groups (other nonprofits, self-employed) get NA.
#
# parrott_analysis_sector uses parrott_hs (broad, for demographics).
# parrott_analysis_sector_wages uses parrott_hs_wages (excl. homecare, for wage tables).

d <- d |>
  mutate(
    parrott_analysis_sector = case_when(
      parrott_hs ~ "hs_nonprofit",
      sector == "govt" ~ "govt",
      sector == "priv_forprofit" ~ "priv_forprofit",
      TRUE ~ NA_character_
    ) |> factor(levels = c("hs_nonprofit", "govt", "priv_forprofit")),
    parrott_analysis_sector_wages = case_when(
      parrott_hs_wages ~ "hs_nonprofit",
      sector == "govt" ~ "govt",
      sector == "priv_forprofit" ~ "priv_forprofit",
      TRUE ~ NA_character_
    ) |> factor(levels = c("hs_nonprofit", "govt", "priv_forprofit"))
  )

message("parrott_analysis_sector distribution:")
print(
  d |>
    group_by(parrott_analysis_sector) |>
    summarize(n_unweighted = n(), n_weighted = sum(PERWT))
)

# ── Extension analysis groups ─────────────────────────────────────────────────────

# TODO should categorize these up front once initial round of analysis is done...

# ── INDNAICS crosswalk ─────────────────────────────────────────────────────
# Loads the IPUMS-provided crosswalk CSV, which covers all Census/ACS-specific
# merged codes (M, P, S, Z suffixes) not found in official NAICS documentation.
# Source: https://usa.ipums.org/usa/volii/indnaics.shtml

indnaics_xwalk <- read_csv(
  INDNAICS_XWALK,
  col_types = cols(.default = "c"),
  show_col_types = FALSE
) |>
  select(-1) |> # drop unnamed row-index column
  rename(
    indnaics = `2018-2022 ACS/PRCS INDNAICS CODE`,
    industry_title = `Industry Title`
  ) |>
  select(indnaics, industry_title) |>
  filter(!is.na(indnaics))

d <- d |>
  left_join(
    indnaics_xwalk,
    by = join_by(INDNAICS == indnaics),
    relationship = "many-to-one"
  )

# ── 12. Save ──────────────────────────────────────────────────────────────────

saveRDS(d, file.path(PROC_DIR, "acs_prepared.rds"))
message(paste("Saved acs_prepared.rds (", format(nrow(d), big.mark = ","), "rows)."))
