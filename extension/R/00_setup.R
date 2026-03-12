# 00_setup.R
# Package loading, constants, and path definitions for the extension.
# Source this at start of each script: source(here::here("extension/R/00_setup.R"))

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

PROJ_ROOT  <- here::here()
REPLIC_DIR <- file.path(PROJ_ROOT, "replication")
EXT_DIR    <- file.path(PROJ_ROOT, "extension")

# Replication outputs (read-only)
REPLIC_PROC_DIR <- file.path(REPLIC_DIR, "data", "processed")
REPLIC_FIG_DIR  <- file.path(REPLIC_DIR, "output", "figures")

# Extension data
EXT_RAW_DIR  <- file.path(EXT_DIR, "data", "raw")
EXT_PROC_DIR <- file.path(EXT_DIR, "data", "processed")
EXT_FIG_DIR  <- file.path(EXT_DIR, "output", "figures")
