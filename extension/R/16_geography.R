source(here::here("replication/R/00_setup.R"))

# ── Load data ─────────────────────────────────────────────────────────────────

acs <- readRDS(file.path(PROC_DIR, "acs_prepared.rds"))
svy <- acs |>
  as_survey_rep(
    weights = PERWT,
    repweights = matches("REPWTP[0-9]+"),
    type = "ACS",
    mse = TRUE
  ) |>
  filter(is_city_wkr | is_priv_np_wkr | is_priv_fp_wkr, is_hs_industry)
