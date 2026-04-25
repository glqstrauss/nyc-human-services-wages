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
library(kableExtra)

# Observations threshold for inclusion

MIN_OBS <- 30L
MAX_CV <- 0.3

# ── Sector labels ────────────────────────────────────────────────────────────

SECTOR_LABELS <- c(
  govt           = "Public",
  private        = "Private",
  self_employed  = "Self-Employed",
  local_govt     = "Local Government",
  state_govt     = "State Government",
  fed_govt       = "Federal Government",
  priv_nonprofit = "Private Non-Profit",
  priv_forprofit = "Private For-Profit"
)

NAICS_LABELS <- c(
  "6241" = "Individual and family services",
  "6242" = "Community food and housing, and emergency services",
  "6243" = "Vocational rehabilitation services"
)

METRIC_LABELS <- c(
  pct_female       = "Female",
  pct_male         = "Male",
  pct_female_poc   = "Female, POC",
  pct_male_poc     = "Male, POC",
  pct_female_white = "Female, White",
  pct_male_white   = "Male, White"
)

EDUCAT_LABELS <- c(
  lths         = "Less than High School",
  hs           = "High School",
  some_college = "Some College",
  bachelors    = "Bachelor's Degree",
  postgrad     = "Postgraduate Degree",
  all          = "All"
)

OCC_LABELS <- c(
  admin_support="Admin Support",
  counselors="Counselors",
  hs_assistants="Social and Human Service Assistants, Other Community and Social Service Specialists",
  janitors_cooks_guards="Janitors, Cooks, and Security",
  managers="Social and Community Service Managers",
  social_workers="Social Workers"
)

label_sector <- function(x) unname(SECTOR_LABELS[as.character(x)])
label_naics  <- function(x) unname(NAICS_LABELS[as.character(x)])
label_educat <- function(x) unname(EDUCAT_LABELS[as.character(x)])
label_occ <- function(x) unname(OCC_LABELS[as.character(x)])

hs_kable <- function(df, caption = NULL, col.names = NULL, ...) {
  knitr::kable(df,
    caption     = caption,
    col.names   = if (!is.null(col.names)) col.names else colnames(df),
    digits      = 0,
    format.args = list(big.mark = ","),
    ...
  ) |>
    kableExtra::kable_styling(full_width = FALSE)
}

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

print_and_save_df <- function(df, name, n = 10) {
  print(df, n = n)
  saveRDS(df, file.path(PROC_DIR, paste0(name, ".rds")))
}
