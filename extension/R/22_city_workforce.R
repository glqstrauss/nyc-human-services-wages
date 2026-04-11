source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(PROC_DIR, "acs_long_prepared.rds"))

svy <- acs |>
  haven::zap_labels() |>
  filter(YEAR != 2000L) |>
  as_survey_rep(
    weights = PERWT,
    repweights = matches("REPWTP[0-9]+"),
    type = "ACS",
    mse = TRUE
  )

city_emp <- svy |>
  filter(is_city_wkr) |>
  group_by(YEAR) |>
  summarize(
    emp = survey_total(na.rm = TRUE),
  )

city_sa_emp <- svy |>
  filter(is_city_wkr, is_sa_industry) |>
  group_by(YEAR) |>
  summarize(
    emp = survey_total(na.rm = TRUE),
  )

bind_rows(
  city_emp |> mutate(group = "all_city"),
  city_sa_emp |> mutate(group = "sa_city")
) |> ggplot(aes(x = YEAR, y = emp, color = group)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Employment in City Government",
    subtitle = "Total and Social Assistance Industry Only",
    x = "Year",
    y = "Estimated Employment (weighted)"
  ) +
  theme_minimal() +
  scale_color_manual(values = role_colors[c("base", "comparison")], labels = c("All City Workers", "City Social Assistance Workers")) +
  theme(legend.title = element_blank())
