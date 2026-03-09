# 00_setup.R
# Package loading, constants, and path definitions for the replication.
# Source this at the top of every other script: source(here::here("replication/R/00_setup.R"))

library(here)
library(tidyverse)
library(ipumsr)
library(srvyr)      # tidyverse-style survey-weighted statistics
library(survey)     # underlying survey design objects
library(scales)     # number formatting for figures
library(ggtext)     # rich-text annotations in ggplot
library(readxl)

# ── Paths ────────────────────────────────────────────────────────────────────

PROJ_ROOT   <- here::here()
REPLIC_DIR  <- file.path(PROJ_ROOT, "replication")
RAW_DIR     <- file.path(REPLIC_DIR, "data", "raw")
PROC_DIR    <- file.path(REPLIC_DIR, "data", "processed")
FIG_DIR     <- file.path(REPLIC_DIR, "output", "figures")

IPUMS_DDI   <- file.path(RAW_DIR, "ipums", "usa_00003.xml")
IPUMS_DAT   <- file.path(RAW_DIR, "ipums", "usa_00003.dat.gz")

# ── ACS sample years ─────────────────────────────────────────────────────────

# The extract contains two 5-year samples: 2018-2022 and 2019-2023.
# For the replication we use only the 2018-2022 sample (YEAR 2022).
CENSUS_YEAR <- 2022

# ── NYC county FIPS codes ────────────────────────────────────────────────────

NYC_COUNTIES <- c(5, 47, 61, 81, 85)   # Bronx, Kings, New York, Queens, Richmond

# ── CLASSWKRD codes ──────────────────────────────────────────────────────────
# 22 = Wage/salary, private for-profit
# 23 = Wage/salary at non-profit
# 24 = Wage/salary, government (general; rarely appears in ACS)
# 25 = Federal government employee
# 27 = State government employee
# 28 = Local government employee

CLASSWKRD_NONPROFIT  <- 23L
CLASSWKRD_FORPROFIT  <- 22L
CLASSWKRD_LOCAL_GOVT <- c(28L)
CLASSWKRD_OTHER_GOVT <- c(24,25,27)

# ── INDNAICS codes ───────────────────────────────────────────────────────────
# INDNAICS is an alphanumeric string in this extract.
# Social Assistance sector (NAICS 624): INDNAICS starts with "624"
# Child Day Care Services (NAICS 6244): INDNAICS == "6244"  -- EXCLUDE
# Individual & Family Services (NAICS 6241): INDNAICS == "6241"
# General Medical & Surgical Hospitals (NAICS 6221): INDNAICS == "6221"

INDNAICS_SOC_ASST     <- "^624"    # regex: all Social Assistance sub-industries
INDNAICS_CHILD_CARE   <- "6244"    # exact: child day care -- EXCLUDE
INDNAICS_PRIV_HOSP    <- "621M"    # ACS PUMS merged hospital code (covers NAICS 6221)

# ── OCC codes (2018 Census occupation codes) ─────────────────────────────────
# Source: 2018 Census Occupation Code List (Census Bureau)

# Core human services professional occupations (KEEP)
OCC_COUNSELORS <- c(
  2001L,  # Substance abuse and behavioral disorder counselors
  2002L,  # Educational, guidance, and career counselors and advisors
  2003L,  # Marriage and family therapists
  2004L,  # Mental health counselors
  2005L,  # Rehabilitation counselors
  2006L   # Counselors, all other
)

OCC_SOCIAL_WORKERS <- c(
  2011L,  # Child, family, and school social workers
  2012L,  # Healthcare social workers
  2013L,  # Mental health and substance abuse social workers
  2014L   # Social workers, all other
)

OCC_HS_ASSISTANTS <- c(
  2016L,  # Social and human service assistants
  2025L   # Other community and social service specialists
)

OCC_MANAGERS <- c(
  420L    # Social and community service managers (= Census code 0420)
)

# Non-professional reference occupations (used in Figures 12-13 as benchmarks)
OCC_ADMIN_SUPPORT <- c(5740L, 5860L, 5940L)  # secretaries, office clerks, admin support
OCC_JANITORS      <- c(4220L)                  # janitors and building cleaners
# Security guards: look up separately -- likely ~3600 range or 3930

# Occupations to EXCLUDE from Social Assistance (home health / personal care)
# Per Parrott: "all home care and personal care aide occupational employment
# is excluded even when those workers appear in the individual and family
# services industry"
OCC_EXCLUDE_HOMECARE <- c(
  3601L,  # Home health aides
  3602L,  # Personal care aides
  3603L   # Nursing assistants
)

# EDUC codes (general version)
# 00 = N/A or no schooling
# 01 = Nursery school to grade 4
# 02 = Grade 5-8
# 03 = Grade 9
# 04 = Grade 10
# 05 = Grade 11
# 06 = Grade 12 (includes HS diploma and GED in the general variable)
# 07 = 1 year of college
# 08 = 2 years of college
# 09 = 3 years of college
# 10 = 4 years of college (bachelor's degree)
# 11 = 5+ years of college (master's degree or better)

# ── Wage top-code ─────────────────────────────────────────────────────────────
# ACS INCWAGE top-code: 999999. Exclude from wage analysis.
INCWAGE_TOPCODE <- 999999L

# ── ggplot theme ──────────────────────────────────────────────────────────────
theme_parrott <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_markdown(face = "bold"),
      plot.subtitle = element_text(color = "grey40"),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}
