source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(REPLIC_PROC_DIR, "acs_prepared.rds"))

# 1.3.1
# title: Occupational distribution of nonprofit vs public sector social assistance
# workers, 2018/22
# description: This table should show the distribution of occupations using the
# "core occupation" definition. We should also have an "other" category to show
# what share of workers in each sector are in core social assistance occupations
# vs other occupations.

table131_core_occs_by_sector <- svy |>
  filter(
    (is_city_wkr | is_priv_np_wkr | is_priv_fp_wkr),
    is_hs_industry,
  ) |>
  mutate(
    occ_group = if_else(is.na(occ_group), "other", occ_group)
  ) |>
  group_by(sector, occ_group) |>
  summarize(
    n = survey_total(vartype = "cv"),
    obs = unweighted(n())
  ) |>
  filter(n_cv < 0.3) |>
  select(sector, occ_group, n, n_cv, obs) |>
  pivot_wider(
    names_from = sector,
    values_from = c(n, n_cv, obs),
    values_fill = NA_real_
  )

# 1.3.2
# title: Non-"core social assistance" occupations of nonprofit vs public sector
# workers, 2018/22
# description: This table should show the distribution of occupations in the
# other category of table 1.1.2.
# analysis: these are interesting but we lack sufficient observations for most
# occupations. it is probably most useful as an aside with the observation
# from the preceding table that there are a lot of workers in the "other" category

table132_other_occs <- svy |>
  filter(
    (is_city_wkr | is_priv_np_wkr),
    is_hs_industry,
    is.na(occ_group)
  ) |>
  group_by(sector, occ_name, OCC) |>
  summarize(
    n = survey_total(vartype = "cv"),
    obs = unweighted(n())
  )

table132_other_occs_by_class <- svy |>
  filter(
    (is_city_wkr | is_priv_np_wkr),
    is_hs_industry,
    is.na(occ_group)
  ) |>
  group_by(occ_class) |>
  # occ_range mainly helpful for constructing the manual occ_class_desc mapping
  mutate(occ_range = paste0(min(OCC), "-", max(OCC))) |>
  ungroup() |>
  group_by(sector, occ_class, occ_range) |>
  summarize(
    n = survey_total(vartype = "cv"),
    obs = unweighted(n()),
  ) |>
  filter(n_cv < 0.3) |>
  pivot_wider(
    names_from = sector,
    values_from = c(n, n_cv, obs),
    values_fill = NA_real_
  )

# This one is almost useful, we have ALMOST big enough numbers for some key groups
# but the govt CVs are still rather large :(
# The one thing it might still be worth getting into is trying to break up the managers group
# to specificlly split out 0420 (social service and community managers) and potentially also
# 0350 (medical and health services managers)
table132_other_occs_by_broad_class <- svy |>
  filter(
    (is_city_wkr | is_priv_np_wkr),
    is_hs_industry,
    is.na(occ_group)
  ) |>
  group_by(sector, occ_broad_class) |>
  summarize(
    n = survey_total(vartype = "cv"),
    obs = unweighted(n()),
  ) |>
  filter(n_cv < 0.3) |>
  pivot_wider(
    names_from = sector,
    values_from = c(n, n_cv, obs),
    values_fill = NA_real_
  )
