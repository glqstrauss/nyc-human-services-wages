# This file explores the population of nonprofit human services workers and public
# sector comparison groups. We end up with the following:
# 1. The "universe" of public sector workers is restricted to municipal government
# workers, which is the most relevant comparison group for insourcing vs outsourcing.
# 2. We have two definitions of "human services" that we use to filter down both the
# nonprofit and public sector workforces:
#   a. A "core occupations" definition that includes only specific occupations.
#   b. An "industry-wide" definition that includes all workers in specific industries.


source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(PROC_DIR, "acs_prepared.rds"))

svy <- acs |> as_survey_rep(
  weights = PERWT,
  repweights = matches("REPWTP[0-9]+"),
  type = "ACS",
  mse = TRUE
)

# POPULATION --------------------------------------------------------------------------

# How large is the workforce in NYC?
svy |>
  group_by(sector) |>
  summarize(n = survey_total(), obs = unweighted(n()))

# How large is the human services industry in NYC?
svy |>
  filter(is_hs_industry) |>
  group_by(sector) |>
  summarize(n = survey_total(), obs = unweighted(n()))

# Now let's ignore self-employed and NA...
svy <- svy |> filter(!is.na(sector), sector != "self_employed")


# How large is the "core occupations" (with home healthcare) definition of human services workers in NYC?
svy |>
  filter(is_hs_industry, is_hs_occ) |>
  group_by(sector) |>
  summarize(n = survey_total(), obs = unweighted(n()))

# How large is the "core occupations" definition of human services workers in NYC?
svy |>
  filter(is_hs_industry, is_hs_occ_nh) |>
  group_by(sector) |>
  summarize(n = survey_total(), obs = unweighted(n()))

# How much of the govt hs industry is made up of city workers?
svy |>
  filter(is_govt_wkr, is_hs_industry) |>
  group_by(govt_level) |>
  summarize(n = survey_total(), obs = unweighted(n()))

# How much of the govt hs core occs is made up of city workers?
svy |>
  filter(is_govt_wkr, is_hs_industry, is_hs_occ_nh) |>
  group_by(govt_level) |>
  summarize(n = survey_total(), obs = unweighted(n()))

# HOURS -------------------------------------------------------------------------------

# What percentage of workers in hs_industry are full-time, by sector?
# Note: we don't directly observe part-time because of insufficient obs for govt.
svy |>
  filter(
    (is_city_wkr | is_nonprofit_wkr),
    full_time | part_time,
    is_hs_industry
  ) |>
  group_by(sector) |>
  summarize(
    pct_ft = survey_mean(full_time),
    obs = unweighted(n())
  )

# What percentage of workers in hs_occ_nh are full time, by sector?
svy |>
  filter(
    (is_city_wkr | is_nonprofit_wkr),
    full_time | part_time,
    is_hs_industry,
    is_hs_occ_nh
  ) |>
  group_by(sector) |>
  summarize(
    pct_ft = survey_mean(full_time, vartype = "cv"),
    obs = unweighted(n())
  )

# What is the distribution of hours worked for full-time workers in hs_industry, by sector?
svy |>
  filter(
    (is_city_wkr | is_nonprofit_wkr),
    is_hs_industry,
    is_hs_occ_nh,
    full_time
  ) |>
  group_by(sector) |>
  summarize(
    avg_hrs_worked = survey_mean(UHRSWORK),
    # quantiles using svyby
    hrs_25 = Hmisc::wtd.quantile(UHRSWORK, weights = cur_svy_wts(), probs = 0.25),
    hrs_50 = Hmisc::wtd.quantile(UHRSWORK, weights = cur_svy_wts(), probs = 0.5),
    hrs_75 = Hmisc::wtd.quantile(UHRSWORK, weights = cur_svy_wts(), probs = 0.75),
    obs = unweighted(n())
  )
