# This file explores the population of nonprofit human services workers and public
# sector comparison groups. We end up with the following:
# 1. The "universe" of public sector workers is restricted to municipal government
# workers, which is the most relevant comparison group for insourcing vs outsourcing.
# 2. We have two definitions of "human services" that we use to filter down both the
# nonprofit and public sector workforces:
#   a. A "core occupations" definition that includes only specific occupations.
#   b. An "industry-wide" definition that includes all workers in specific industries.


#  IMPORTANT: Implement using srvyr so that we can calculate error.

source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(REPLIC_PROC_DIR, "acs_prepared.rds"))

# TODO: move this to acs_prep!
acs <- acs |> as_survey_rep(
  weights = PERWT,
  repweights = matches("REPWTP[0-9]+"),
  type = "ACS",
  mse = TRUE
)

# 1.1.1
# title: How large are the sectors of interest?
# description: This table should include weighted and unweighted counts of workers in
# each sector of interest. For both non-profit and city workers, we should see:
#   - All workers
#   - Workers in social assistance industries
#   - Workers in "core social assistance occupations"
# NB: these should perhaps be three different tables?
# We should also determine how to flag any cells that are too small to use.

# All NYC workers in the sample (for reference)
table111_all_workers <- acs |>
  group_by(sector) |>
  summarize(
    obs = unweighted(n()),
    n = survey_total(vartype = "ci")
  )

# All workers in the sample in social assistance industries
table111_hs_industry <- acs |>
  filter(
    (is_city_wkr | is_priv_np_wkr | is_priv_fp_wkr),
    is_hs_industry
  ) |>
  group_by(sector) |>
  summarize(
    n = survey_total(vartype = "ci"),
    obs = unweighted(n())
  )

table111_hs_occs <- acs |>
  filter(
    (is_city_wkr | is_priv_np_wkr | is_priv_fp_wkr),
    is_hs_industry,
    is_hs_occ,
    occ_group != "homecare"
  ) |>
  group_by(sector) |>
  summarize(
    n = survey_total(vartype = "ci"),
    obs = unweighted(n())
  )


# 1.1.2
# title: Occupational distribution of nonprofit vs public sector social assistance
# workers, 2018/22
# description: This table should show the distribution of occupations using the
# "core occupation" definition. We should also have an "other" category to show
# what share of workers in each sector are in core social assistance occupations
# vs other occupations.

table112_core_occs_by_sector <- acs |>
  filter(
    (is_city_wkr | is_priv_np_wkr | is_priv_fp_wkr),
    is_hs_industry,
  ) |>
  mutate(
    occ_group = if_else(is.na(occ_group), "other", occ_group)
  ) |>
  group_by(sector, occ_group) |>
  summarize(
    n = survey_total(vartype = NULL),
    obs = unweighted(n())
  ) |>
  filter(obs >= 40) |>
  select(sector, occ_group, n) |>
  pivot_wider(
    names_from = sector,
    values_from = c(n),
    values_fill = NA_integer_
  ) |>
  arrange(occ_group)

# 1.1.3
# title: Non-"core social assistance" occupations of nonprofit vs public sector
# workers, 2018/22
# description: This table should show the distribution of occupations in the
# other category of table 1.1.2.
# analysis: these are interesting but we lack sufficient observations for most
# occupations.

table113_other_occs <- acs |>
  filter(
    (is_city_wkr | is_priv_np_wkr | is_priv_fp_wkr),
    is_hs_industry,
    is.na(occ_group)
  ) |>
  group_by(sector, occ_name, OCC) |>
  summarize(
    n = survey_total(vartype = NULL),
    obs = unweighted(n())
  )

table113_other_occs_non_prof_top_10 <- table113_other_occs |>
  filter(sector == "priv_nonprofit") |>
  ungroup() |>
  arrange(desc(n)) |>
  select(occ_name, n, obs) |>
  slice_head(n = 20)

table113_other_occs_govt_top_10 <- table113_other_occs |>
  filter(sector == "govt") |>
  ungroup() |>
  arrange(desc(n)) |>
  select(occ_name, n, obs) |>
  slice_head(n = 20)


# 1.1.4
# title: Full-time vs part-time status of nonprofit vs public sector social assistance
# workers 2018/22
# description: This should show the share for both the industry definition and the core
# occupations definition. It's probably going to be negligble for public sector...

table114_pct_fulltime <- acs |>
  filter(
    (is_city_wkr | is_priv_np_wkr),
    is_hs_industry,
    is_hs_occ,
    occ_group != "homecare"
  ) |>
  group_by(sector) |>
  summarize(
    n = survey_total(vartype = "ci"),
    pct_ft = survey_mean(full_time, vartype = "ci")
  )


# 1.1.5
# title: Percent of hours worked by part-time employees in nonprofit vs public sector
# description: Shows the share of hours workers by part-time employees in both sectors
# ("intensity") of

# table of percent total hours worked by part time employees, by sector
table114_part_time_contrib <- acs |>
  filter(
    (is_city_wkr | is_priv_np_wkr),
    is_hs_industry,
    is_hs_occ,
    occ_group != "homecare",
    UHRSWORK > 0 # "regular" part-timers worked last week
  ) |>
  group_by(sector) |>
  summarize(
    pct_hrs_part_time = survey_ratio(
      # part timer hours
      numerator = if_else(full_time, 0, UHRSWORK),
      # all hours
      denominator = UHRSWORK,
      vartype = "ci"
    )
  )

# There may be other tables that arise to answer specific questions as they come up!
# For instance I previously noticed that there are tons of "managers" in the nonprofit
# sector that don't appear at all in the public sector group.
