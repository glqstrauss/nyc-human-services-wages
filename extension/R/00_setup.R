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
library(matrixStats)

# ── Plot constants ───────────────────────────────────────────────────────────

role_colors <- c(base = "#1b9e77", comparison = "#d95f02", reference = "#7570b3")

# ── Paths ────────────────────────────────────────────────────────────────────

PROJ_ROOT <- here::here()
EXT_DIR <- file.path(PROJ_ROOT, "extension")
RAW_DIR <- file.path(EXT_DIR, "data", "raw")
PROC_DIR <- file.path(EXT_DIR, "data", "processed")
FIG_DIR <- file.path(EXT_DIR, "output", "figures")

# ----- Because the VSCode R Extension doesn't like haven labels...

safe_view <- \(df) View(haven::zap_labels(df))
vw <- safe_view
