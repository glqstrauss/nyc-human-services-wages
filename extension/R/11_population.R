source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(PROC_DIR, "acs_prepared.rds"))

svy <- acs |>
  haven::zap_labels() |>
  filter(!is.na(sector)) |>  # ignore self employed, unemployed etc
  as_survey_rep(
    weights = PERWT,
    repweights = matches("REPWTP[0-9]+"),
    type = "ACS",
    mse = TRUE
  )

# POPULATION --------------------------------------------------------------------------

# How large is the workforce in NYC?
pop_nyc <- svy |>
  # NOTE that self-employed was coded to NA because we don't care about it in this analysis....
  group_by(sector, YEAR) |>
  summarize(n = survey_total(), obs = unweighted(n())) |>
  ungroup()


pop_nyc_by_sector_detail <- svy |>
  # NOTE that self-employed was coded to NA because we don't care about it in this analysis....
  group_by(sector, sector_detail, YEAR) |>
  summarize(n = survey_total(), obs = unweighted(n())) |>
  ungroup()


# What is the Core HS count by sector?
pop_hs_by_sector <- svy |>
  filter(is_hs_industry, !is_homecare) |>
  group_by(sector, YEAR) |>
  summarize(
    n = survey_total()
  ) |>
  ungroup()



# WHat is the Core HS count by detailed sector (govt level, nonprofit vs forprofit)
pop_hs_by_sector_detail <- svy |>
  filter(is_hs_industry, !is_homecare) |>
  group_by(sector, sector_detail, YEAR) |>
  summarize(
    n = survey_total()
  ) |>
  ungroup()

# What specific industries are largest in each sector?
pop_hs_by_sector_naics <- svy |> 
  filter(is_hs_industry, !is_homecare, YEAR == 2024) |>
  group_by(sector, INDNAICS) |>
  summarize(n = survey_total(), obs = unweighted(n())) |>
  ungroup()

# What specific industries are largest in each subsector?
pop_hs_by_sector_detail_naics <- svy |> 
  filter(is_hs_industry, !is_homecare, YEAR == 2024) |>
  group_by(sector, sector_detail, INDNAICS) |>
  summarize(n = survey_total(), obs = unweighted(n()))

# How large is the "core occupations" definition of human services workers in NYC?
pop_hs_core_occs <- svy |>
  filter(occ_group %in% c("social_workers", "counselors", "managers", "hs_assistants"), YEAR == 2024) |>
  group_by(sector_detail, INDNAICS) |>
  summarize(n = survey_total(), obs = unweighted(n()))

# HOURS -------------------------------------------------------------------------------

# What percentage of workers in hs_industry are full-time, by sector?
# Note: we don't directly observe part-time because of insufficient obs for govt.
fulltime_hs_by_sector <- svy |>
  filter(
    YEAR == 2024,
    full_time | part_time,
    is_hs_industry,
    !is_homecare
  ) |>
  group_by(sector) |>
  summarize(
    pct_ft = survey_mean(full_time),
    obs = unweighted(n())
  )

# What is the distribution of hours worked for full-time workers in hs_industry, by sector?
hours_quants_by_sector <- svy |>
  filter(
    YEAR == 2024,
    is_hs_industry,
    !is_homecare,
    full_time
  ) |>
  group_by(sector) |>
  summarize(
    avg_hrs_worked = survey_mean(UHRSWORK),
    # SEs are too big for upper tail 0.9, 0.95 
    hrs = survey_quantile(UHRSWORK, c(0.25, 0.5, 0.75, 0.9)),
    obs = unweighted(n())
  )