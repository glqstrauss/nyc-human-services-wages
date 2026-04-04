# 00_setup.R
# Package loading, constants, and path definitions for the replication.
# Source this at start of each script: source(here::here("replication/R/00_setup.R"))


# ── Paths ────────────────────────────────────────────────────────────────────

PROJ_ROOT <- here::here()
REPLIC_DIR <- file.path(PROJ_ROOT, "replication")
RAW_DIR <- file.path(REPLIC_DIR, "data", "raw")
PROC_DIR <- file.path(REPLIC_DIR, "data", "processed")
FIG_DIR <- file.path(REPLIC_DIR, "output", "figures")

ACS_5YR_2022_DDI <- file.path(RAW_DIR, "ipums", "usa_00007.xml")
ACS_5YR_2022_DAT <- file.path(RAW_DIR, "ipums", "usa_00007.dat.gz")

INDNAICS_XWALK <- file.path(RAW_DIR, "ipums", "indnaics_crosswalk_2000_onward_with_code_descriptions.csv")
