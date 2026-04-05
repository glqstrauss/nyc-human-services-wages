# 02_acs_long_prep.R

source(here::here("extension/R/00_setup.R"))

IPUMS_DDI <- file.path(RAW_DIR, "ipums_longitudinal", "usa_00009.xml")
IPUMS_DAT <- file.path(RAW_DIR, "ipums_longitudinal", "usa_00009.dat.gz")

# Core human services professional occupations
OCC_COUNSELORS <- c(2000L)

OCC_SOCIAL_WORKERS <- c(2010L)

OCC_HS_ASSISTANTS <- c(
  2016L, # Social and human service assistants
  2025L # Other community and social service specialists
)

OCC_MANAGERS <- c(
  420L # Social and community service managers
)

OCC_HOMECARE <- c(
  3600L
)

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
    EMPSTAT == 1L, # employed last week
    GQ %in% 1:2 # exclude group quarters (prisons, dorms)
  )

message(
  paste("After NYC/year/employed filters:", format(nrow(d), big.mark = ","), "rows.")
)

d |>
  group_by(YEAR) |>
  summarize(obs = n(), pop = sum(PERWT))

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

d |>
  group_by(YEAR, sector) |>
  summarize(pop = sum(PERWT)) |>
  pivot_wider(names_from = sector, values_from = c(pop))

# ── Core human services flag ───────────────────────────────────────────────
# Core human services industry definition (Parrott 2025):
#   NAICS 624 — Social Assistance (6241, 6242, 6243, plus merged codes)
#   NAICS 623 — Residential Care Facilities (6231, 623M)
#   EXCLUDE:  6244 — Child Day Care Services
#
# The 623 codes capture group homes, residential mental health/substance abuse
# facilities, supportive housing, and I/DD residences — all major components
# of NYC human services contracting (DOHMH, DHS, HRA program areas).
# NOTE: The relevant INDNAICS codes are mostly stable across all years in the
# longitudinal extract. 6231 and 623M end up getting split slightly differently
# across years, but since we're including both it doesn't matter.
# Crosswalk: https://usa.ipums.org/usa/volii/indnaics.shtml

# Validate that the industry codes are consistent with expected counts in the longitudinal extract.
d |>
  filter(
    grepl("^62[34]", INDNAICS),
    INDNAICS != "6244", # exclude childcare
    sector %in% c("priv_nonprofit", "govt")
  ) |>
  group_by(INDNAICS, YEAR) |>
  summarize(n = sum(PERWT)) |>
  pivot_wider(names_from = YEAR, values_from = c(n)) |>
  arrange(INDNAICS)

d <- d |>
  mutate(
    # Social Assistance (624*) + Residential Care (623*)  - Childcare (6244)
    is_hs_industry = grepl("^62[34]", INDNAICS) &
      INDNAICS != 6244,
    occ_group = case_when(
      OCC2010 %in% OCC_SOCIAL_WORKERS ~ "social_workers",
      OCC2010 %in% OCC_COUNSELORS ~ "counselors",
      OCC2010 %in% OCC_HS_ASSISTANTS ~ "hs_assistants",
      OCC2010 %in% OCC_MANAGERS ~ "managers",
      # homecare will be *excluded* from wage analysis, but not demo analysis
      OCC2010 %in% OCC_HOMECARE ~ "homecare",
      TRUE ~ NA_character_
    ) |> factor(),
    is_hs_occ = !is.na(occ_group),
    is_hs_occ_nh = !is.na(occ_group) & occ_group != "homecare"
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

# ── 9. Work intensity and wage ────────────────────────────────────────────────
d |>
  group_by(YEAR) |>
  summarize(mw = max(INCWAGE))

d <- d |>
  mutate(
    full_time = UHRSWORK >= 35L,
    part_time = UHRSWORK > 0L & UHRSWORK < 35L,
    # Top code is a bit annoying for multi-year samples,
    # but it's not really relevant since we're looking at medians,
    # in an industry where very high wages are rare.
    wage_valid = INCWAGE > 0L
  )

# ── 10. Analysis Sector Flags ───────────────────────────────────────────

d <- d |> mutate(
  # baseline/alt baseline
  is_private_wkr = sector %in% c("priv_forprofit", "priv_nonprofit"),
  is_govt_wkr = sector == "govt",
  is_city_wkr = CLASSWKRD == 28L,
  is_forprofit_wkr = sector == "priv_forprofit",
  is_nonprofit_wkr = sector == "priv_nonprofit",
  # for govt-level breakdown showing dominance of local govt
  govt_level = case_when(
    CLASSWKRD == 25L ~ "federal",
    CLASSWKRD == 27L ~ "state",
    CLASSWKRD == 28L ~ "local",
    TRUE ~ NA_character_
  )
)

saveRDS(d, file.path(PROC_DIR, "acs_long_prepared.rds"))
message(paste("Saved acs_long_prepared.rds (", format(nrow(d), big.mark = ","), "rows)."))
