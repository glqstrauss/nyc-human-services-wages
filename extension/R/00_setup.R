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

# Compatibility helper for quantiles using survey::svyby + survey::svyquantile.
# This avoids some list/vector issues seen with srvyr::survey_quantile().
cust_survey_quantile <- function(data,
                                 var,
                                 by,
                                 quantiles = 0.5,
                                 vartype = c("se"),
                                 na.rm = TRUE,
                                 ci = "ci" %in% vartype,
                                 interval.type = "Wald") {
  var_q <- rlang::enquo(var)
  by_q <- rlang::enquo(by)

  var_nm <- rlang::as_name(var_q)
  by_nm <- rlang::as_name(by_q)

  quantiles <- as.numeric(quantiles)
  if (length(quantiles) == 0L || any(!is.finite(quantiles)) ||
    any(quantiles < 0 | quantiles > 1)) {
    stop("`quantiles` must be numeric values in [0, 1].")
  }

  if (!inherits(data, "tbl_svy") && !inherits(data, "survey.design") && !inherits(data, "svyrep.design")) {
    stop("`data` must be a srvyr tbl_svy or a survey design object.")
  }

  design <- if (inherits(data, "tbl_svy")) {
    data$survey
  } else {
    data
  }

  out <- survey::svyby(
    formula = stats::as.formula(paste0("~", var_nm)),
    by = stats::as.formula(paste0("~", by_nm)),
    design = design,
    FUN = survey::svyquantile,
    quantiles = quantiles,
    ci = ci,
    keep.var = TRUE,
    na.rm = na.rm,
    interval.type = interval.type,
    covmat = FALSE
  )

  out <- tibble::as_tibble(out)

  stat_cols <- setdiff(names(out), by_nm)
  q_labels <- paste0("q", formatC(quantiles * 100, width = 0, format = "f", digits = 0))

  # svyby names can vary by survey version; normalize names where possible.
  for (i in seq_along(quantiles)) {
    q_col <- stat_cols[i]
    names(out)[names(out) == q_col] <- paste0(var_nm, "_", q_labels[i])
  }

  out
}
