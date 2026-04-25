# This file performs wage comparisons between our sectors of interest.
# There are a lot of different ways we could do this....

source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(PROC_DIR, "acs_prepared.rds"))

svy <- acs |>
  haven::zap_labels() |> # pesky labels break survey_median
  filter(!is.na(sector)) |>  # ignore self employed, unemployed etc
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
    ) |>
    mutate(med_wage = ifelse(med_wage_cv > 0.3, NA_real_, med_wage))
}

# what is the overall median pay for all govt vs all private sector?
wages_all <- svy |>
  calculate_group_sector_stats(c(sector, YEAR))

# What is the median pay for full-time workers in non-profit hs industry, vs public
# sector hs_industry? (A lot closer than the Parrott comparison...)
wages_hs <- svy |>
  filter(is_hs_industry, !is_homecare) |>
  calculate_group_sector_stats(c(sector, YEAR))

# What is the median pay for full-time workers by education level?
# A: In all private vs all govt?
wages_by_educ_all <- svy |>
  calculate_group_sector_stats(c(sector, educ_cat, YEAR)) |>
  mutate(sector = paste0("all_", sector))

# B: in_hs_industry, excluding homecare (and childcare)?
wages_by_educ_hs <- svy |>
  filter(is_hs_industry & !is_homecare) |>
  calculate_group_sector_stats(c(sector, educ_cat, YEAR)) |>
  mutate(sector = paste0("hs_", sector))

# What is the median pay for full-time workers in non-profit human services vs public
# sector for each core occupation group? Note that the public sector is not filtered
# by industry

# IMPORTANT: the public sector is not filtered by industry. We do this to
# get a bigger sample group but SHOULD VERIFY that number looks similar with
# industry filter.
wages_by_occ_group <- bind_rows(
  svy |>
    filter(sector == "private" & is_hs_industry & is_hs_occ_nh) |>
    calculate_group_sector_stats(c(occ_group, YEAR)) |>
    mutate(sector = "hs_private"),
  svy |>
    filter(sector == "govt" & is_hs_occ_nh) |>
    calculate_group_sector_stats(c(occ_group, YEAR)) |>
    mutate(sector = "hs_govt")
) |>
  arrange(occ_group, sector)

# How much do the following groups make, at nonprofit HS vs govt?
wages_core_occ_by_educ <- svy |>
  filter(
    occ_group %in% c("counselors", "social_workers"),
    educ_cat %in% c("bachelors", "postgrad"),
    sector == "govt" | (sector == "private" & is_hs_industry),
  ) |>
  calculate_group_sector_stats(c(sector, occ_group, educ_cat, YEAR))

avg_exp_core_occ_by_educ <- svy |>
  filter(
    occ_group %in% c("counselors", "social_workers"),
    educ_cat %in% c("bachelors", "postgrad"),
    sector == "govt" | (sector == "private" & is_hs_industry),
  ) |>
  group_by(sector, occ_group, educ_cat, YEAR) |>
  summarize(
    avg_exp = survey_mean(experience, vartype = "cv"),
  ) |>
  pivot_wider(names_from = c(sector, YEAR), values_from = c(avg_exp, avg_exp_cv))


wages_core_occ_by_exp <- svy |>
  filter(
    occ_group %in% c("counselors", "social_workers"),
    educ_cat %in% c("bachelors", "postgrad"),
    YEAR == 2022L
  ) |>
  group_by(sector, experience_cat) |>
  summarize(
    median_wage = survey_median(INCWAGE),
    obs=unweighted(n()),
    n=survey_total(),
    prop=survey_prop(),
  )


#-------------------------------------------- 
# UNFINISHED
# -------------------------------------------

# How much of the pay gap between nonprofit HS and govt is explained by
# differences in experience?

# The point of this exercise is not to point to "experience" as a useful concept
# in itself.
# 
# sub <- svy |>
#   filter(
#     occ_group %in% c("counselors", "social_workers"),
#     educ_cat %in% c("bachelors", "postgrad"),
#     YEAR == 2022L
#   ) |>
#   mutate(
#     exp = experience,
#     exp2 = experience^2
#   )
# 
# # What is the median wage for this group at each experience_cat level, by sector?
# # As a dodged column chart
# wages_core_occ_by_exp |>
#   ggplot(aes(x = experience_cat, y = median_wage, fill = sector)) +
#   geom_col(position = "dodge") +
#   theme_minimal()
# 
# # Scatterplot of exp vs log wage, by sector
# sub |>
#   ggplot(aes(x = exp, y = log(INCWAGE), color = sector)) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
#   theme_minimal()
# 
# # Line plot of avg wage at each experience category, by sector
# sub |>
#   group_by(sector, exp) |>
#   summarize(mean_wage = survey_mean(INCWAGE, vartype = NULL)) |>
#   ggplot(aes(x = exp, y = mean_wage, color = sector)) +
#   geom_line() +
#   theme_minimal()
# 
# summary(model)
# 
# exp_by_sector <- sub |>
#   group_by(sector) |>
#   summarize(mean_exp = survey_mean(exp, vartype = NULL))
# 
# exp_govt <- exp_by_sector$mean_exp[exp_by_sector$sector == "govt"]
# exp_nonprofit <- exp_by_sector$mean_exp[exp_by_sector$sector == "priv_nonprofit"]
# 
# # Step 2: apply regression coefficients
# b1 <- coef(model)["exp"]
# b2 <- coef(model)["exp2"]
# 
# lw_govt <- b1 * exp_govt + b2 * exp_govt^2
# lw_nonprofit <- b1 * exp_nonprofit + b2 * exp_nonprofit^2
# 
# exp_contribution_logpts <- lw_govt - lw_nonprofit
# exp_contribution_pct <- exp(exp_contribution_logpts) - 1
# 
# # Step 3: share of the total sector gap
# sector_gap <- abs(coef(model)["sectorpriv_nonprofit"])
# share_explained <- exp_contribution_logpts / sector_gap
# 
# cat(sprintf(
#   "Experience gap: %.1f years (govt %.1f vs nonprofit %.1f)\n
#   Wage contribution: %.1f log pts (%.1f%%)\n
#   Share of sector gap explained: %.0f%%\n",
#   exp_govt - exp_nonprofit, exp_govt, exp_nonprofit,
#   exp_contribution_logpts * 100, exp_contribution_pct * 100,
#   share_explained * 100
# ))
