source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(PROC_DIR, "acs_prepared.rds"))

svy <- acs |>
  as_survey_rep(
    weights = PERWT,
    repweights = matches("REPWTP[0-9]+"),
    type = "ACS",
    mse = TRUE
  )

# 1.3.1
# title: Occupational distribution of nonprofit vs public sector social assistance
# workers, 2018/22
# description: This table should show the distribution of occupations using the
# "core occupation" definition. We should also have an "other" category to show
# what share of workers in each sector are in core social assistance occupations
# vs other occupations.

# What is the occupational distribution of workers in is_hs_industry, by sector?
svy |>
  filter(
    !is.na(sector_broad),
    is_hs_industry,
  ) |>
  mutate(
    occ_group = if_else(is.na(occ_group), "other", occ_group)
  ) |>
  group_by(sector_broad, occ_group) |>
  summarize(
    # proportion is a little weird here. Homecare skews
    prop = survey_prop(vartype = "cv"),
    n = survey_total(vartype = NULL),
    obs = unweighted(n())
  ) |>
  filter(prop_cv < 0.3) |>
  select(sector_broad, occ_group, prop, prop_cv, n, obs) |>
  pivot_wider(
    names_from = sector_broad,
    values_from = c(prop, prop_cv, n, obs),
    values_fill = NA_real_
  )

# Who are the workers in the "other" category working in the nonprofit sector?
# These are very small numbers only worth noting as occupations that exist.
svy |>
  filter(
    sector_broad == "private",
    is_hs_industry,
    is.na(occ_group)
  ) |>
  group_by(occ_broad_class) |>
  summarize(
    n = survey_total(vartype = "cv"),
    obs = unweighted(n())
  ) |>
  arrange(desc(n))

# Who are the workers in the "other" category working in the public sector?
# These are very small numbers only worth noting as occupations that exist.
svy |>
  filter(
    sector_broad == "govt",
    is_hs_industry,
    is.na(occ_group)
  ) |>
  group_by(occ_broad_class) |>
  summarize(
    n = survey_total(vartype = "cv"),
    obs = unweighted(n())
  ) |>
  arrange(desc(n))
