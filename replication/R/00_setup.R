# 00_setup.R
# Package loading, constants, and path definitions for the replication.
# Source this at start of each script: source(here::here("replication/R/00_setup.R"))

library(here)
library(tidyverse)
library(ipumsr)
library(srvyr)
library(survey)
library(scales)
library(ggtext)
library(readxl)
library(labelled)

# ── Paths ────────────────────────────────────────────────────────────────────

PROJ_ROOT <- here::here()
REPLIC_DIR <- file.path(PROJ_ROOT, "replication")
RAW_DIR <- file.path(REPLIC_DIR, "data", "raw")
PROC_DIR <- file.path(REPLIC_DIR, "data", "processed")
FIG_DIR <- file.path(REPLIC_DIR, "output", "figures")

IPUMS_DDI <- file.path(RAW_DIR, "ipums", "usa_00007.xml")
IPUMS_DAT <- file.path(RAW_DIR, "ipums", "usa_00007.dat.gz")

INDNAICS_XWALK <- file.path(RAW_DIR, "ipums", "indnaics_crosswalk_2000_onward_with_code_descriptions.csv")

# ── OCC codes (2018 Census occupation codes) ─────────────────────────────────
# Source: 2018 Census Occupation Code List (Census Bureau)

# Core human services professional occupations (KEEP)
OCC_COUNSELORS <- c(
  2001L, # Substance abuse and behavioral disorder counselors
  2002L, # Educational, guidance, and career counselors and advisors
  2003L, # Marriage and family therapists
  2004L, # Mental health counselors
  2005L, # Rehabilitation counselors
  2006L # Counselors, all other
)

OCC_SOCIAL_WORKERS <- c(
  2011L, # Child, family, and school social workers
  2012L, # Healthcare social workers
  2013L, # Mental health and substance abuse social workers
  2014L # Social workers, all other
)

OCC_HS_ASSISTANTS <- c(
  2016L, # Social and human service assistants
  2025L # Other community and social service specialists
)

OCC_MANAGERS <- c(
  420L # Social and community service managers (= Census code 0420)
)

# Non-professional reference occupations (used in Figures 12-13 as benchmarks)
OCC_ADMIN_SUPPORT <- c(5740L, 5860L, 5940L) # secretaries, office clerks, admin support
OCC_JANITORS <- c(4220L) # janitors and building cleaners
# Security guards: look up separately -- likely ~3600 range or 3930


OCC_HOMECARE <- c(
  3601L, # Home health aides
  3602L, # Personal care aides
  3603L # Nursing assistants
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
      plot.title = element_markdown(face = "bold"),
      plot.subtitle = element_text(color = "grey40"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}
