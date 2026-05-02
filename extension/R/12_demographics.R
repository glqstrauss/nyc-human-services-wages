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

svy <- svy |> 
  filter(YEAR == 2024) |> 
  # Parrot analysis only considers demographics of full-time workers
  filter(full_time == TRUE)

calc_demo_ratios <- function(df, label) {
  df |>
    summarize(                                                                                                                                    
      pct_female       = survey_ratio(female, 1),
      pct_male         = survey_ratio(!female, 1),                                                                                                
      pct_female_poc   = survey_ratio(female & poc, 1),
      pct_male_poc     = survey_ratio(!female & poc, 1),                                                                                          
      pct_female_white = survey_ratio(female & !poc, 1),                                                                                          
      pct_male_white   = survey_ratio(!female & !poc, 1)
    ) |>                                                                                                                                          
    pivot_longer(
      cols      = !ends_with("_se"),
      names_to  = "metric",                                                                                                                       
      values_to = "pct"
    ) |>                                                                                                                                          
    select(metric, pct) |>
    mutate(label = label)
}

sex_race_by_sector <- bind_rows (
  calc_demo_ratios(svy |> filter(sector == "private"), "All Private Sector"),
  calc_demo_ratios(svy |> filter(sector == "govt", is_hs_industry, !is_homecare), "Public Sector Human Services"),
  calc_demo_ratios(svy |> filter(sector == "private", is_hs_industry, !is_homecare), "Private Sector Human Services")
)

# EDUCATION AND EXPERIENCE ------------------------------------------------------------

# What percent of workers in is_hs_industry have each level of education?
educat_hs_by_sector <- svy |>
  filter(is_hs_industry, !is_homecare) |>
  group_by(sector, educ_cat) |>
  summarize(
    prop = survey_prop(),
  )

educat_all_by_sector <- svy |>
  group_by(sector, educ_cat) |>
  summarize(
    prop = survey_prop(),
  )


# What is the median age of workers in is_hs_occ_nh, by sector?
age_hs_by_sector <- svy |>
  filter(is_hs_industry, !is_homecare) |>
  group_by(sector) |>
  summarize(
    age_median = survey_median(AGE),
  )


# Following calculations use measure of experience as age minus estimated age of
# workforce entry, based on level of education
# https://economics.stackexchange.com/questions/53650

# What is the average experience of workers in is_hs_industry, by sector?
avg_exp_hs_by_sector <- svy |>
  filter(is_hs_industry, !is_homecare) |>
  group_by(sector) |>
  summarize(
    avg_exp = survey_mean(experience),
    obs = unweighted(n())
  )

# What is the average experience in the private sector?
avg_exp_priv_sector <- svy |>
  filter(sector == "private") |>
  summarize(
    avg_exp = survey_mean(experience),
    obs = unweighted(n())
  )

# What is the average experience for hs industry by sector and education level?
avg_exp_hs_by_sector_educ_cat <- svy |>
    filter(is_hs_industry, !is_homecare) |>
    group_by(sector, educ_cat) |>
    summarize(
      avg_exp = survey_mean(experience),
      obs = unweighted(n())
    )

# What is the average experience for counselors and social workers
avg_exp_core_hs_by_sector <- svy |>
  filter(is_hs_industry, occ_group %in% c("counselors", "social_workers")) |>
  group_by(sector) |>
  summarize(
    avg_exp = survey_mean(experience),
    obs = unweighted(n())
  )

# What is the average experience for is_hs_occ, looking only at highly educated workers?
avg_exp_high_educ_hs_by_sector <- svy |>
  filter(
    is_hs_industry,
    !is_homecare,
    educ_cat %in% c("bachelors", "postgrad"),
  ) |>
  group_by(sector) |>
  summarize(
    avg_exp = survey_mean(experience, vartype = "se"),
    obs = unweighted(n())
  ) |>
  pivot_wider(
    names_from = sector,
    values_from = c(avg_exp, avg_exp_se, obs)
  ) |>
  mutate(
    diff=avg_exp_govt - avg_exp_private,
    diff_se=sqrt(avg_exp_se_govt^2 + avg_exp_se_private^2)
  )
