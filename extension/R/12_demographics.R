source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(PROC_DIR, "acs_prepared.rds"))

svy <- acs |>
  haven::zap_labels() |> # Breaks survey_median
  as_survey_rep(
    weights = PERWT,
    repweights = matches("REPWTP[0-9]+"),
    type = "ACS",
    mse = TRUE
  )

# What percent of workers in is_hs_industry are women and/or people of color?
svy |>
  filter(
    !is.na(sector_broad) & # only govt vs priv
      is_hs_industry
  ) |>
  group_by(sector_broad) |>
  summarize(
    pct_women = survey_mean(female),
    pct_poc = survey_mean(poc),
    obs = unweighted(n())
  )

# What percent of workers in core HS occ are women and/or POC?
svy |>
  filter(!is.na(sector_broad), is_hs_industry, is_hs_occ_nh) |>
  group_by(sector_broad) |>
  summarize(
    pct_women = survey_mean(female),
    pct_poc = survey_mean(poc),
    obs = unweighted(n())
  )

# What percent of workers in is_hs_industry are women x POC (crosstab)?
svy |>
  filter(!is.na(sector_broad), is_hs_industry) |>
  group_by(sector_broad, gender_race) |>
  summarize(
    prop = survey_prop()
  ) |>
  pivot_wider(
    names_from = sector_broad,
    values_from = c(prop, prop_se)
  )

# And for the public vs private sector as a whole
svy |>
  filter(!is.na(sector_broad)) |>
  group_by(sector_broad, gender_race) |>
  summarize(
    prop = survey_prop()
  ) |>
  pivot_wider(
    names_from = sector_broad,
    values_from = c(prop, prop_se)
  )

# EDUCATION AND EXPERIENCE ------------------------------------------------------------

# What percent of workers in is_hs_industry have each level of education?
svy |>
  filter(!is.na(sector_broad), is_hs_industry) |>
  group_by(sector_broad, educ_cat) |>
  summarize(
    prop = survey_prop(),
  ) |>
  pivot_wider(
    names_from = sector_broad,
    values_from = c(prop, prop_se)
  )

# What percent of workers in is_hs_occ_nh have each level of education?
svy |>
  filter(!is.na(sector_broad), is_hs_industry, is_hs_occ_nh) |>
  group_by(sector_broad, educ_cat) |>
  summarize(
    prop = survey_prop()
  ) |>
  pivot_wider(
    names_from = sector_broad,
    values_from = c(prop, prop_se)
  )

# What is the median age of workers in is_hs_occ_nh, by sector?
svy |>
  filter(!is.na(sector_broad), is_hs_industry, is_hs_occ_nh) |>
  group_by(sector_broad) |>
  summarize(
    age_median = survey_median(AGE),
  )


# Following calculations use measure of experience as age minus estimated age of
# workforce entry, based on level of education
# https://economics.stackexchange.com/questions/53650

# What is the average experience of workers in is_hs_industry, by sector?
svy |>
  filter(!is.na(sector_broad), is_hs_industry) |>
  group_by(sector_broad) |>
  summarize(
    avg_exp = survey_mean(experience),
    obs = unweighted(n())
  )

# What is the average experience of workers in is_hs_occ, by sector?
svy |>
  filter(!is.na(sector_broad), is_hs_industry, is_hs_occ_nh) |>
  group_by(sector_broad) |>
  summarize(
    avg_exp = survey_mean(experience),
    obs = unweighted(n())
  )

# What is the average "experience" in the private sector?
svy |>
  filter(sector_broad == "private") |>
  summarize(
    avg_exp = survey_mean(experience),
    obs = unweighted(n())
  )

# What is the average experience for is_hs_occ, by sector and education level?
table123_experience_occ <- bind_rows(
  svy |>
    filter(!is.na(sector_broad), is_hs_industry, is_hs_occ) |>
    group_by(sector_broad, educ_cat) |>
    summarize(
      avg_exp = survey_mean(experience),
      obs = unweighted(n())
    ),
  svy |>
    filter(!is.na(sector_broad), is_hs_industry, is_hs_occ) |>
    group_by(sector_broad) |>
    summarize(
      avg_exp = survey_mean(experience),
      obs = unweighted(n())
    ) |>
    mutate(educ_cat = "all")
) |> pivot_wider(
  names_from = sector_broad,
  values_from = c(avg_exp, avg_exp_se, obs)
)

# What is the average experience for is_hs_occ, looking only at highly educated workers?
svy |>
  filter(
    !is.na(sector_broad),
    is_hs_industry,
    is_hs_occ,
    educ_cat %in% c("bachelors", "postgrad"),
  ) |>
  group_by(sector_broad) |>
  summarize(
    avg_exp = survey_mean(experience),
    obs = unweighted(n())
  ) |>
  pivot_wider(
    names_from = sector_broad,
    values_from = c(avg_exp, avg_exp_se, obs)
  )

# What is the average experience for is_hs_industry, looking only at highly educated workers?
svy |>
  filter(
    !is.na(sector_broad),
    is_hs_industry,
    educ_cat %in% c("bachelors", "postgrad"),
  ) |>
  group_by(sector_broad) |>
  summarize(
    avg_exp = survey_mean(experience),
    obs = unweighted(n())
  ) |>
  pivot_wider(
    names_from = sector_broad,
    values_from = c(avg_exp, avg_exp_se, obs)
  )
