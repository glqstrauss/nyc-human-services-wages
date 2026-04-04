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

acs <- readRDS(file.path(REPLIC_PROC_DIR, "acs_prepared.rds"))

svy <- acs |>
  as_survey_rep(
    weights = PERWT,
    repweights = matches("REPWTP[0-9]+"),
    type = "ACS",
    mse = TRUE
  ) |>
  mutate(
    # hs_industry city and nonprofit workers
    # plus all private sector workers as baseline
    ind_analysis = (is_hs_industry & is_city_wkr) |
      (is_hs_industry & is_priv_np_wkr) |
      (is_priv_fp_wkr),
    # hs_occ city and nonprofit workers
    # plus all private sector workers as baseline
    # For city, we do NOT require that they be in the industry,
    # since civil service titles dictate pay standards across
    # "industry" for city workers and the sample is too small
    # to subset down to just those in the industry.
    occ_analysis = (is_hs_industry & is_hs_occ & is_city_wkr) |
      (is_hs_industry & is_hs_occ & is_priv_np_wkr) |
      (is_priv_fp_wkr)
  )

# 1.2.1
# title: Gender and race/ethnic characteristics of public vs nonprofit core human
# service workers, 2018/22
# description: This is similar to Parrott's Figure 5 (2025) but with the public sector
# as the comparison group instead of the private sector as a whole. We should also
# add percentages to the table in order to more easily compare across the two groups.
# These are what Parrott uses in Figure 6 (which is a bar chart not a table).
# We could also consider producing TWO versions: one for industry and one for "core
# occupations" only.

# What percent of workers in hs_industry are women?

svy``

table121_pct_women_poc <- svy |>
  filter(ind_analysis) |>
  group_by(sector) |>
  summarize(
    pct_women = survey_mean(female, vartype = "cv"),
    pct_poc = survey_mean(poc, vartype = "cv"),
    n = survey_total(),
    obs = unweighted(n())
  )

table121_pct_gender_x_race <- svy |>
  filter(ind_analysis) |>
  group_by(sector, gender_race) |>
  summarize(
    prop = survey_prop(vartype = "cv")
  ) |>
  pivot_wider(
    names_from = sector,
    values_from = c(prop, prop_cv)
  )

# 1.2.2
# title: Education levels of public vs nonprofit core human service workers, 2018/22
# description: This is similar to Parrott's Figure 7 (2025) but comparing public vs
# nonprofit. Produce versions for core occupations and industry-wide definitions.
# analysis: city workers on average are more experienced (older) but similarly likely to
# have a college degree. This says something about the pay gap, and perhaps says
# something about working conditions and turnover as well.

table122_pct_educ_cat_industry <- svy |>
  filter(ind_analysis) |>
  group_by(sector, educ_cat) |>
  summarize(
    prop = survey_prop(vartype = "cv"),
  ) |>
  pivot_wider(
    names_from = sector,
    values_from = c(prop, prop_cv)
  )

# Robust to using the core occupations definition -- still large gaps for postgrad and college
table122_pct_educ_cat_occ <- svy |>
  filter(occ_analysis) |>
  group_by(sector, educ_cat) |>
  summarize(
    prop = survey_prop(vartype = "cv")
  ) |>
  pivot_wider(
    names_from = sector,
    values_from = c(prop, prop_cv)
  )

# 1.2.3
# title: Years of experience in public vs nonprofit core human service workers, 2018/22
# description: compares imputed experience (age - imputed workforce entry age). Produce
# versions for core occupations and industry-wide definitions. We should probably
# bucket experience into categories (0-5 years, 5-10 years, 10-20 years, 20+ years).
# TODO: we need to add imputed experience to the ACS prep. See the following discussion:
# https://economics.stackexchange.com/questions/53650

summarize_experience <- \(x) summarize(
  x,
  avg_exp = survey_mean(experience),
  n = survey_total(),
  obs = unweighted(n())
)

table123_experience_occ <- bind_rows(
  svy |>
    filter(occ_analysis) |>
    group_by(sector, educ_cat) |>
    summarize_experience(),
  svy |>
    filter(occ_analysis) |>
    group_by(sector) |>
    summarize_experience() |>
    mutate(educ_cat = "all")
) |> pivot_wider(
  names_from = sector,
  values_from = c(avg_exp, avg_exp_se, n, n_se, obs)
)

# KEY FINDING: the experience gap is 5-6 years among highly-educated workers
table123_experience_high_ed_occ <- svy |>
  filter(occ_analysis, educ_cat %in% c("bachelors", "postgrad")) |>
  group_by(sector) |>
  summarize_experience() |>
  pivot_wider(
    names_from = sector,
    values_from = c(avg_exp, avg_exp_se, n, n_se, obs)
  )

# Robust to using the industry definition, though the gap is slightly smaller.
table123_experience_high_ed_ind <- svy |>
  filter(ind_analysis, educ_cat %in% c("bachelors", "postgrad")) |>
  group_by(sector) |>
  summarize_experience() |>
  pivot_wider(
    names_from = sector,
    values_from = c(avg_exp, avg_exp_se, n, n_se, obs)
  )


table123_experience_high_ed_social_workers <- svy |>
  filter(
    ind_analysis,
    educ_cat %in% c("bachelors", "postgrad"),
  ) |>
  group_by(sector, occ_group) |>
  summarize_experience() |>
  pivot_wider(
    names_from = sector,
    values_from = c(avg_exp, avg_exp_se, n, n_se, obs)
  ) |>
  mutate(
    t_stat = (
      (avg_exp_govt - avg_exp_priv_nonprofit) /
        sqrt(avg_exp_se_govt^2 + avg_exp_se_priv_nonprofit^2)
    )
  )
