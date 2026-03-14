# This file was initially exploring various defintions of both the nonprofit human services
# workforce and the public sector comparison group, but it was also doing wage calculations
# across those groups. What we need to do here is extract the population exploration stuff
# into 11_population.R and have this file focus on wage comparisons once the populations
# are defined...

source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(REPLIC_PROC_DIR, "acs_prepared.rds"))

######################################
# Questions:
# - What is the correct comparison group to nonprofit human services workers?
#   - All-private and all-public sector?
#   - Only city government workers?
#   - Only workers in social assistance occupations?
#   - Only workers in social assistance industries?
# - How do median wages compare across education levels, between the groups?
# - How does the wage distribution compare between the groups?
##############################################
# Tentatively:
#    - For most analayses
##############################################

acs_wages <- acs |> filter(
  full_time == TRUE,
  INCWAGE < 999999,
  INCWAGE > 0
)

make_wage_by_group_and_sector_plot <- function(data, xvar, title) {
  data |>
    ggplot(aes(x = {{ xvar }}, y = med_wage, fill = sector)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_text(
      aes(label = scales::dollar(med_wage, accuracy = 1)),
      vjust = -0.5,
      position = position_dodge(width = 0.9)
    ) +
    labs(
      title = title,
      x = "Education Level",
      y = "Median Wage",
      fill = "Sector"
    ) +
    theme_minimal()
}

##############################################

calculate_group_sector_stats <- function(data, group_var) {
  summarize(
    data,
    n = n(),
    n_wt = sum(PERWT),
    # NOTE: Parrott uses the unweighted median, but we use the weighted median here.
    med_wage_nw = median(INCWAGE),
    med_wage = weightedMedian(INCWAGE, w = PERWT, na.rm = TRUE),
    .by = c({{ group_var }}, sector)
  ) |>
    arrange({{ group_var }}, sector)
}

##############################################

make_wage_by_sector_density_plot <- function(data, title, xlim = c(0, 200000)) {
  data |>
    ggplot(aes(x = INCWAGE, color = sector, weight = PERWT)) +
    geom_density() +
    scale_x_continuous(labels = scales::dollar, limits = xlim) +
    scale_color_manual(values = unname(role_colors)) +
    labs(title = title, x = "Annual Wage", y = NULL, color = "Sector") +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

##############################################

acs_parrott_sectors <- acs_wages |>
  mutate(
    sector = case_when(
      is_hs_wages ~ "Core HS Nonprofit",
      sector == "govt" ~ "All Public Sector",
      sector == "priv_forprofit" ~ "All Private Sector",
    )
  ) |>
  filter(!is.na(sector))

acs_parrott_sectors_stats <- acs_parrott_sectors |>
  calculate_group_sector_stats(educ_cat)

make_wage_by_group_and_sector_plot(
  acs_parrott_sectors_stats,
  educ_cat,
  paste(
    "Parrott's Comparison Between Core HS Nonprofit and ",
    "Economy-wide Wages in Public and Private Sectors"
  )
)

make_wage_by_sector_density_plot(
  acs_parrott_sectors,
  "Parrott's Comparison: Wage Distributions by Sector"
)

##############################################

acs_hs_sectors <- acs_wages |>
  mutate(
    sector = case_when(
      sector == "priv_nonprofit" ~ "Nonprofit",
      sector == "govt" ~ "Public Sector",
      sector == "priv_forprofit" ~ "Private Sector",
    )
  ) |>
  filter(!is.na(occ_group) & !is.na(sector))

acs_hs_sectors_stats <- acs_hs_sectors |>
  calculate_group_sector_stats(educ_cat)

make_wage_by_group_and_sector_plot(
  acs_hs_sectors_stats,
  educ_cat,
  paste(
    "Comparison Between Social Assistance Occupations",
    "in Nonprofit, Public, and Private Sectors"
  )
)

make_wage_by_sector_density_plot(
  acs_hs_sectors,
  "Social Assistance Occupations: Wage Distributions by Sector"
)

##############################################

# How is the non-profit sector like the private sector?
# Well as you can see, govt wage distribution is a lot flatter
# across education level, while private and non-profit have a steeper slop.

acs_all_sectors <- acs_wages |>
  mutate(
    sector = case_when(
      sector == "govt" ~ "Government",
      sector == "priv_forprofit" ~ "Private Sector",
      sector == "priv_nonprofit" ~ "Nonprofit",
    )
  ) |>
  filter(!is.na(sector))

acs_all_sectors_stats <- acs_all_sectors |>
  calculate_group_sector_stats(educ_cat)


make_wage_by_group_and_sector_plot(
  acs_all_sectors_stats,
  educ_cat,
  paste(
    "Economy-Wide Comparison of Sectors in NYC"
  )
)

make_wage_by_sector_density_plot(
  acs_all_sectors,
  "Economy-Wide Wage Distributions by Sector"
)

##############################################

acs_city_only_hs_occs <- acs_wages |>
  filter(!is.na(occ_group)) |>
  filter(
    sector == "priv_nonprofit" |
      (sector == "govt" & is_city_wkr)
  )

acs_city_only_hs_occs_stats <- acs_city_only_hs_occs |>
  calculate_group_sector_stats(educ_cat)


make_wage_by_group_and_sector_plot(
  acs_city_only_hs_occs_stats,
  educ_cat,
  paste(
    "Comparison Between Social Assistance Occupations",
    "in Nonprofit and City Government Sectors"
  )
)

make_wage_by_group_and_sector_plot(
  acs_city_only_hs_occs |>
    calculate_group_sector_stats(occ_group),
  occ_group,
  paste(
    "Comparison Between Social Assistance Occupations",
    "in Nonprofit and City Government Sectors"
  )
)

# NB: the xlim removes more non-profit workers than city workers,
# might want to consider binning the top-coded wages instead of removing them entirely.
make_wage_by_sector_density_plot(
  acs_city_only_hs_occs |> mutate(INCWAGE = if_else(INCWAGE > 150000, 150000, INCWAGE)),
  "Social Assistance Occupations: Nonprofit vs City Government Wage Distributions",
)

##############################################

acs_city_only_hs_inds <- acs_wages |>
  filter(in_hs_industry) |>
  mutate(
    sector = case_when(
      sector == "priv_nonprofit" ~ "Nonprofit",
      sector == "govt" & is_city_wkr ~ "Public Sector",
    ),
  ) |>
  filter(!is.na(sector))

acs_city_only_hs_inds_stats <- acs_city_only_hs_inds |>
  calculate_group_sector_stats(educ_cat)

make_wage_by_group_and_sector_plot(
  acs_city_only_hs_inds_stats,
  educ_cat,
  paste(
    "Comparison Between Social Assistance Industries",
    "in Nonprofit and City Government Sectors"
  )
)

# NB: the xlim removes more non-profit workers than city workers,
# might want to consider binning the top-coded wages instead of removing them entirely.
make_wage_by_sector_density_plot(
  acs_city_only_hs_inds,
  "Social Assistance Industries: Nonprofit vs City Government Wage Distributions",
)
