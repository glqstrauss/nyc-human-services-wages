# 02_acs_long_prep.R

source(here::here("extension/R/00_setup.R"))

IPUMS_DDI_2005_2024 <- file.path(RAW_DIR, "ipums_longitudinal", "usa_00009.xml")
IPUMS_DDI_2000 <- file.path(RAW_DIR, "ipums_longitudinal", "usa_00010.xml")

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
  3600L, # Nursing, Psychiatric, and Home Health Aides
  4610L  # Personal care aides
)

# CPI-W
# https://www.ssa.gov/oact/STATS/avgcpi.html
CPI <- tibble(
  year = 2000:2024,
  cpi_u_rs = c(
    252.5,
    259.7,
    263.8,
    269.8,
    277.0,
    286.4,
    295.7,
    304.1,
    315.8,
    314.7,
    319.8,
    330.0,
    336.9,
    342.0,
    347.7,
    348.3,
    352.8,
    360.3,
    369.1,
    375.8,
    380.8,
    399.2,
    431.5,
    449.3,
    462.5
  )
)

CPI_2024 <- CPI$cpi_u_rs[CPI$year == 2024]

# ── Load IPUMS extract ─────────────────────────────────────────────────────

message("Reading IPUMS DDI for 1-year extracts...")
ddi <- read_ipums_ddi(IPUMS_DDI_2005_2024)

message("Reading microdata for 1-year extracts...")
raw <- read_ipums_micro(ddi, verbose = FALSE)

message(paste("Loaded", format(nrow(raw), big.mark = ","), "rows total."))

message("Reading IPUMS DDI for 2000 5% census...")
ddi_y2k <- read_ipums_ddi(IPUMS_DDI_2000)

message("Reading microdata for 2000 5% census...")
raw_y2k <- read_ipums_micro(ddi_y2k, verbose = FALSE)

message(paste("Loaded", format(nrow(raw_y2k), big.mark = ","), "rows total."))

raw <- bind_rows(raw, raw_y2k)
message(paste("Combined dataset has", format(nrow(raw), big.mark = ","), "rows total."))

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
    sector_detail = case_when(
      CLASSWKRD %in% c(13L, 14L) ~ "self_employed",
      CLASSWKRD == 22L ~ "priv_forprofit",
      CLASSWKRD == 23L ~ "priv_nonprofit",
      CLASSWKRD == 25L ~ "fed_govt",
      CLASSWKRD == 27L ~ "state_govt",
      CLASSWKRD == 28L ~ "local_govt",
      TRUE ~ NA_character_ # Capures NA and Unpaid Family Worker
    ) |> factor(),
    sector_broad = case_when(
      CLASSWKRD %in% c(22L, 23L) ~ "private",
      CLASSWKRD %in% c(25L, 27L, 28L) ~ "govt",
      # Ignore self-employed as well as NA and Unpaid Family Worker
      TRUE ~ NA_character_
    ) |> factor()
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
    grepl("^624", INDNAICS),
    INDNAICS != "6244", # exclude childcare
    sector_broad %in% c("private", "govt")
  ) |>
  group_by(INDNAICS, YEAR) |>
  summarize(n = sum(PERWT)) |>
  pivot_wider(names_from = YEAR, values_from = c(n)) |>
  arrange(INDNAICS)

d <- d |>
  mutate(
    # Social Assistance (624*) + Residential Care (623*)  - Childcare (6244)
    is_sa_industry = grepl("^624", INDNAICS) &
      INDNAICS != 6244,
    occ_group = case_when(
      OCC2010 %in% OCC_SOCIAL_WORKERS ~ "social_workers",
      OCC2010 %in% OCC_COUNSELORS ~ "counselors",
      OCC2010 %in% OCC_HS_ASSISTANTS ~ "hs_assistants",
      OCC2010 %in% OCC_MANAGERS ~ "managers",
      TRUE ~ NA_character_
    ) |> factor(),
    is_hs_occ = !is.na(occ_group),
    is_hs_occ_nh = !is.na(occ_group) & occ_group != "homecare",
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

# TODO remove
# d <- d |> mutate(
#   # baseline/alt baseline
#   is_private_wkr = sector %in% c("priv_forprofit", "priv_nonprofit"),
#   is_govt_wkr = sector == "govt",
#   is_city_wkr = CLASSWKRD == 28L,
#   is_forprofit_wkr = sector == "priv_forprofit",
#   is_nonprofit_wkr = sector == "priv_nonprofit",
#   # for govt-level breakdown showing dominance of local govt
#   govt_level = case_when(
#     CLASSWKRD == 25L ~ "federal",
#     CLASSWKRD == 27L ~ "state",
#     CLASSWKRD == 28L ~ "local",
#     TRUE ~ NA_character_
#   )
# )

# Adjust wadges for inflation using CPI-W

d <- d |>
  left_join(CPI, by = c("YEAR" = "year")) |>
  mutate(
    incwage_real = INCWAGE * (CPI_2024 / cpi_u_rs)
  ) |>
  select(-cpi_u_rs)

# ── 3-year non-overlapping pools (2020 excluded — COVID experimental weights) ──
# 2024 has no complete 3-year pool; pool is NA and excluded from pooled analyses.

d <- d |>
  mutate(
    pool = case_when(
      YEAR %in% 2005:2007 ~ 2006L,
      YEAR %in% 2008:2010 ~ 2009L,
      YEAR %in% 2011:2013 ~ 2012L,
      YEAR %in% 2014:2016 ~ 2015L,
      YEAR %in% 2017:2019 ~ 2018L,
      YEAR %in% 2021:2023 ~ 2022L,
      TRUE ~ NA_integer_ # 2020 (excluded) and 2024 (incomplete pool)
    ),
    across(
      c(PERWT, matches("REPWTP[0-9]+")),
      \(w) w / 3,
      .names = "{.col}_pooled"
    )
  )

saveRDS(d, file.path(PROC_DIR, "acs_long_prepared.rds"))
message(paste("Saved acs_long_prepared.rds (", format(nrow(d), big.mark = ","), "rows)."))
