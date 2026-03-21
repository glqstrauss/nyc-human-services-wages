# This file explores the population of nonprofit human services workers and public
# sector comparison groups. We end up with the following:
# 1. The "universe" of public sector workers is restricted to municipal government
# workers, which is the most relevant comparison group for insourcing vs outsourcing.
# 2. We have two definitions of "human services" that we use to filter down both the
# nonprofit and public sector workforces:
#   a. A "core occupations" definition that includes only specific occupations.
#   b. An "industry-wide" definition that includes all workers in specific industries.


source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(REPLIC_PROC_DIR, "acs_prepared.rds"))

# TODO: move this to acs_prep!
svy <- acs |> as_survey_rep(
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

table111_sector_sizes <- svy |>
  filter(is_city_wkr | is_priv_np_wkr | is_priv_fp_wkr) |>
  group_by(sector) |>
  summarize(
    # all workers in sample
    n_all = survey_total(),
    obs_all = unweighted(n()),
    # workers in hs industry only
    n_hs_industry = survey_total(is_hs_industry),
    obs_hs_industry = unweighted(sum(is_hs_industry)),
    # workers in hs industry + core occs only (no homecare)
    n_hs_occs = survey_total(is_hs_industry & is_hs_occ_nh),
    obs_hs_occs = unweighted(sum(is_hs_industry & is_hs_occ_nh))
  )


# 1.1.2
# title: Size of municipal vs state vs federal government workforces in NYC, 2018/22
# description: This table shows the counts of workers in each level of government in
# the sample, for all workers, for hs industry, and for hs core occs.
# analysis: NYC Municipal govt is by *far* the largest group.

table112_govt_levels <- svy |>
  filter(sector == "govt") |>
  group_by(govt_level) |>
  summarize(
    n_all = survey_total(),
    obs_all = unweighted(n()),
    n_hs_industry = survey_total(is_hs_industry),
    obs_hs_industry = unweighted(sum(is_hs_industry)),
    n_hs_occs = survey_total(is_hs_industry & is_hs_occ_nh),
    obs_hs_occs = unweighted(sum(is_hs_industry & is_hs_occ_nh))
  )

# 1.1.4
# title: Full-time vs part-time status of nonprofit vs public sector social assistance
# workers 2018/22
# description: This should show the share for both the industry definition and the core
# occupations definition. It's probably going to be negligble for public sector...

# NB: this diff gets even more dramatic with the core occs, but the CV is too high for govt.
# We can get a lower CV for "all hs_industry except homecare specifically", but that's
# not part of our population for other parts of the survey...
# table114_pct_parttime <-
svy |>
  filter(
    (is_city_wkr | is_priv_np_wkr),
    full_time | part_time,
    is_hs_industry,
    is_hs_occ_nh
  ) |>
  group_by(sector) |>
  summarize(
    pct_pt = survey_mean(part_time, vartype = "cv")
  )

# 1.1.5
# title: Part time worker stats


# NOTE pair this with the density plot of part time hours by sector.
# The govt CV is too small to say anything definitive about but it doesnt hurt.
svy |>
  filter(
    (is_city_wkr | is_priv_np_wkr),
    is_hs_industry,
    is_hs_occ_nh,
    part_time
  ) |>
  group_by(sector) |>
  summarize(
    avg_hrs_worked = survey_mean(UHRSWORK, vartype = "cv")
  )

# 1.1.6
# title: Percent of hours worked by part-time employees in nonprofit vs public sector
# description: Shows the share of hours workers by part-time employees in both sectors
# ("intensity") of

# table of percent total hours worked by part time employees, by sector
table114_part_time_contrib <-
  svy |>
  filter(
    (is_city_wkr | is_priv_np_wkr),
    is_hs_industry,
    is_hs_occ_nh,
    part_time | full_time
  ) |>
  group_by(sector) |>
  summarize(
    pct_hrs_part_time = survey_ratio(
      # part timer hours
      numerator = if_else(full_time, 0, UHRSWORK),
      # all hours
      denominator = UHRSWORK,
      vartype = "cv"
    )
  )
