# This file performs wage comparisons between our sectors of interest.
# There are a lot of different ways we could do this....

source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(PROC_DIR, "acs_prepared.rds"))

svy <- acs |>
  haven::zap_labels() |> # pesky labels break survey_median
  as_survey_rep(
    weights = PERWT,
    repweights = matches("REPWTP[0-9]+"),
    type = "ACS",
    mse = TRUE
  ) |>
  filter(
    full_time == TRUE,
    INCWAGE < 999999,
    INCWAGE > 0
  )


calculate_group_sector_stats <- function(data, group_vars) {
  data |>
    group_by(across({{ group_vars }})) |>
    summarize(
      med_wage = survey_median(INCWAGE, vartype = "cv"),
      med_wag_unwt = unweighted(median(INCWAGE)),
      obs = unweighted(n())
    )
  #  |>
  # mutate(med_wage = ifelse(med_wage_cv > 0.3, NA_real_, med_wage))
}

# 1.4.1
# title: Median Pay for full time workers in social assistance industries,
# non-profit vs public sector, 2018/22
# description: Reproduction of Parrott's Figure 10 using industry-level definition

# What is the median pay for full-time workers in is_hs_industry, by sector?
svy |>
  filter(is_city_wkr | is_nonprofit_wkr, is_hs_industry) |>
  calculate_group_sector_stats(sector) |>
  bind_rows(
    svy |>
      filter(is_private_wkr) |>
      calculate_group_sector_stats() |>
      mutate(sector = "all_private")
  )

# What is the median pay for full-time workers in is_hs_industry, by sector, excluding
# home healthcare workers?
svy |>
  filter(is_city_wkr | is_nonprofit_wkr, is_hs_industry, !is_homecare) |>
  calculate_group_sector_stats(sector) |>
  bind_rows(
    svy |>
      filter(is_private_wkr) |>
      calculate_group_sector_stats() |>
      mutate(sector = "all_private")
  )

# What is the median pay for full-time workers in is_hs_industry, excluding
# home healthcare workers, by sector x education, compared to all_public and
# all_private workers? (Parrott comparison)

# Calculate groups separately, since the group overlaps with nonprofit sector...
bind_rows(
  svy |>
    filter(is_nonprofit_wkr & is_hs_industry & !is_homecare) |>
    calculate_group_sector_stats(educ_cat) |>
    mutate(sector = "hs_nonprofit"),
  svy |>
    # Parrott uses all-govt
    filter(is_govt_wkr) |>
    calculate_group_sector_stats(educ_cat) |>
    mutate(sector = "all_public"),
  svy |>
    filter(is_private_wkr) |>
    calculate_group_sector_stats(educ_cat) |>
    mutate(sector = "all_private")
) |>
  arrange(educ_cat, sector)


# What is the median pay for full-time workers in is_hs_industry, excluding
# home healthcare workers, by sector x education?
# Only diff from above is that we only look at public sector HS.
bind_rows(
  svy |>
    filter(is_nonprofit_wkr & is_hs_industry & !is_homecare) |>
    calculate_group_sector_stats(educ_cat) |>
    mutate(sector = "hs_nonprofit"),
  svy |>
    filter(is_city_wkr & is_hs_industry & !is_homecare) |>
    calculate_group_sector_stats(educ_cat) |>
    mutate(sector = "hs_public"),
  svy |>
    filter(is_private_wkr) |>
    calculate_group_sector_stats(educ_cat) |>
    mutate(sector = "all_private")
) |>
  arrange(educ_cat, sector)

# What is the median pay for full-time workers in non-profit human services vs public
# sector for each core occupation group? Note that the public sector is not filtered
# by industry
bind_rows(
  svy |>
    filter(is_nonprofit_wkr & is_hs_industry & is_hs_occ_nh) |>
    calculate_group_sector_stats(occ_group) |>
    mutate(sector = "hs_nonprofit"),
  svy |>
    filter(is_city_wkr & is_hs_occ_nh) |>
    calculate_group_sector_stats(occ_group) |>
    mutate(sector = "all_public")
) |>
  arrange(occ_group, sector)

# 1.4.3
# title: Wage distribution for full time workers in social assistance industries,
# non-profit vs public sector, 2018/22
# description: A density plot comparison of the wage distributions. The non-profit
# distribution has a longer tail, but we don't just want to trim high wages. Is it
# misleading to change the top-code to something relatively low like 200k?
# We should also try a cumlative distribution plot.

# 1.4.4
# title: How much do executives make?
# description: ...
