# This file produces some of the same demographic tables as Parrott,
# but with two additions.
# 1. We include the public sector as a comparison group, whereas Parrott
# compares demographics to the private sector as a whole. This is in line with this
# paper's focus on questions of insourcing vs outsourcing, rather than looking at
# nonprofit human services workers as private sector labor market participants.
# 2. Analyze the age of workers in non-profits vs public sector social
# assistance occupations, as a proxy for experience levels.
# 3. (OPEN QUESTION) we focus only on core social assistance occupations
# IF THE POPULATION IS LARGE ENOUGH. Otherwise we look at the "industry-based"
# definition. If we look at industry-wide codes, we will need to be able to
# answer more questions about the composition of the public sector comparison group.

source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(PROC_DIR, "acs_prepared.rds"))

svy <- acs |>
  as_survey_rep(
    weights = PERWT,
    repweights = matches("REPWTP[0-9]+"),
    type = "ACS",
    mse = TRUE
  ) |>

# What percent of workers in is_hs_industry are women and/or people of color?
svy |>
  filter(is_city_wkr | is_nonprofit_wkr, is_hs_industry) |>
  group_by(sector) |>
  summarize(
    pct_women = survey_mean(female),
    pct_poc = survey_mean(poc),
    obs = unweighted(n())
  )

# What percent of workers in is_hs_occ are women and/or POC?
svy |>
  filter(is_city_wkr | is_nonprofit_wkr, is_hs_industry, is_hs_occ) |>
  group_by(sector) |>
  summarize(
    pct_women = survey_mean(female),
    pct_poc = survey_mean(poc),
    obs = unweighted(n())
  )

# What percent of the private sector vs govt workforce more broadly is women and/or POC?
svy |>
  filter(is_private_wkr | is_govt_wkr) |>
  mutate(sector=if_else(is_private_wkr, "private", "govt")) |>
  group_by(sector) |>
  summarize(
    pct_women = survey_mean(female),
    pct_poc = survey_mean(poc),
    obs = unweighted(n())
  )

# What percent of workers in is_hs_industry are women x POC (crosstab)?
svy |>
  filter(is_city_wkr | is_nonprofit_wkr, is_hs_industry) |>
  group_by(sector, gender_race) |>
  summarize(
    prop = survey_prop()
  ) |>
  pivot_wider(
    names_from = sector,
    values_from = c(prop, prop_se)
  )

# EDUCATION AND EXPERIENCE ------------------------------------------------------------

# What percent of workers in is_hs_industry have each level of education?
svy |>
  filter(is_city_wkr | is_nonprofit_wkr, is_hs_industry) |>
  group_by(sector, educ_cat) |>
  summarize(
    prop = survey_prop(),
  ) |>
  pivot_wider(
    names_from = sector,
    values_from = c(prop, prop_se)
  )

# What percent of workers in is_hs_occ have each level of education?
svy |>
  filter(is_city_wkr | is_nonprofit_wkr, is_hs_industry, is_hs_occ) |>
  group_by(sector, educ_cat) |>
  summarize(
    prop = survey_prop()
  ) |>
  pivot_wider(
    names_from = sector,
    values_from = c(prop, prop_se)
  )

# What is the median age of workers in is_hs_occ, by sector?
svy |>
  filter(is_city_wkr | is_nonprofit_wkr, is_hs_industry, is_hs_occ) |>
  group_by(sector) |>
  summarize(
    age_median = Hmisc::wtd.quantile(AGE, weights = cur_svy_wts(), probs = 0.5)
  )


# Following calculations use measure of experience as age minus estimated age of
# workforce entry, based on level of education
# https://economics.stackexchange.com/questions/53650

# What is the average experience of workers in is_hs_industry, by sector?
svy |>
  filter(is_city_wkr | is_nonprofit_wkr, is_hs_industry) |>
  group_by(sector) |>
  summarize(
    avg_exp = survey_mean(experience),
    obs = unweighted(n())
  )

# What is the average experience of workers in is_hs_occ, by sector?
svy |>
  filter(is_city_wkr | is_nonprofit_wkr, is_hs_industry, is_hs_occ) |>
  group_by(sector) |>
  summarize(
    avg_exp = survey_mean(experience),
    obs = unweighted(n())
  )

# What is the average experience in the private sector?
svy |>
  filter(is_private_wkr) |>
  summarize(
    avg_exp = survey_mean(experience),
    obs = unweighted(n())
  )

# What is the average experience for is_hs_occ, by sector and education level?
table123_experience_occ <- bind_rows(
  svy |>
    filter(is_city_wkr | is_nonprofit_wkr, is_hs_industry, is_hs_occ) |>
    group_by(sector, educ_cat) |>
    summarize(
      avg_exp = survey_mean(experience),
      obs = unweighted(n())
    ),
  svy |>
    filter(is_city_wkr | is_nonprofit_wkr, is_hs_industry, is_hs_occ) |>
    group_by(sector) |>
    summarize(
      avg_exp = survey_mean(experience),
      obs = unweighted(n())
    ) |>
    mutate(educ_cat = "all")
) |> pivot_wider(
  names_from = sector,
  values_from = c(avg_exp, avg_exp_se, obs)
)

# What is the average experience for is_hs_occ, looking only at highly educated workers?
svy |>
  filter(
    is_city_wkr | is_nonprofit_wkr, 
    is_hs_industry, 
    is_hs_occ, 
    educ_cat %in% c("bachelors", "postgrad"),
  ) |>
  group_by(sector) |>
  summarize(
      avg_exp = survey_mean(experience),
      obs = unweighted(n())
  ) |>
  pivot_wider(
    names_from = sector,
    values_from = c(avg_exp, avg_exp_se, obs)
  )

# What is the average experience for is_hs_industry, looking only at highly educated workers?
svy |>
  filter(
    is_city_wkr | is_nonprofit_wkr, 
    is_hs_industry,
    educ_cat %in% c("bachelors", "postgrad"),
  ) |>
  group_by(sector) |>
  summarize(
      avg_exp = survey_mean(experience),
      obs = unweighted(n())
  ) |>
  pivot_wider(
    names_from = sector,
    values_from = c(avg_exp, avg_exp_se, obs)
  )
