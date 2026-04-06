source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(PROC_DIR, "acs_long_prepared.rds"))

# Insufficient sample sizes for govt to look at OCC-level
acs <- acs |> mutate(
  occ_group_broad = case_when(
    occ_group %in% c("counselors", "social_workers") ~ "counselors & social workers",
    occ_group %in% c("hs_assistants", "managers") ~ "assistants and managers",
    TRUE ~ "other"
  ),
  sector_broad = case_when(
    is_private_wkr ~ "private",
    is_govt_wkr ~ "government",
    TRUE ~ "other"
  ) |> factor(levels = c("private", "government", "other"))
)

svy <- acs |>
  haven::zap_labels() |>
  filter(!is.na(pool)) |>
  as_survey_rep(
    weights = PERWT_pooled,
    repweights = matches("REPWTP[0-9]+_pooled"),
    type = "ACS",
    mse = TRUE
  )
y2k <- acs |> filter(YEAR == 2000L)

calculate_group_emp_and_wages <- function(df_y2k, df_svy, condition, group_vars = c()) {
  y2k_calc <- df_y2k |>
    filter({{ condition }}) |>
    mutate(pool = 2000L) |>
    group_by(pool, across({{ group_vars }})) |>
    summarize(
      emp = sum(PERWT, na.rm = TRUE),
      wages = matrixStats::weightedMedian(INCWAGE, w = PERWT, na.rm = TRUE),
      wages_real = matrixStats::weightedMedian(incwage_real, w = PERWT, na.rm = TRUE)
    )


  svy_calc <- df_svy |>
    filter({{ condition }}) |>
    group_by(pool, across({{ group_vars }})) |>
    summarize(
      emp = survey_total(na.rm = TRUE, vartype = "cv"),
      wages = survey_median(INCWAGE, w = PERWT_pooled, na.rm = TRUE, vartype = "cv"),
      wages_real = survey_median(incwage_real, w = PERWT_pooled, na.rm = TRUE, vartype = NULL)
    )

  bind_rows(y2k_calc, svy_calc) |>
    arrange(pool) |>
    mutate(
      d_emp = emp / lag(emp) - 1,
      d_wages = wages / lag(wages) - 1,
      d_wages_real = wages_real / lag(wages_real) - 1
    )
}

private_emp <- calculate_group_emp_and_wages(
  y2k, svy,
  condition = is_private_wkr
)

govt_emp <- calculate_group_emp_and_wages(
  y2k, svy,
  condition = is_govt_wkr
)

core_sa_emp <- calculate_group_emp_and_wages(
  y2k, svy,
  condition = is_private_wkr & is_sa_industry & is_hs_occ
)

govt_sa_emp <- calculate_group_emp_and_wages(
  y2k, svy,
  condition = is_govt_wkr & is_sa_industry & is_hs_occ
)


core_sa_emp_by_occ <- calculate_group_emp_and_wages(
  y2k, svy,
  condition = is_private_wkr & is_sa_industry & is_hs_occ,
  group_vars = c(occ_group)
)

all_sa_emp_by_occ_sector <- calculate_group_emp_and_wages(
  y2k, svy,
  condition = (is_private_wkr | is_govt_wkr) & is_sa_industry & is_hs_occ,
  group_vars = c(sector_broad, occ_group_broad)
)

all_sa_emp_by_occ_sector |>
  filter(pool %in% c(2000L, 2022L)) |>
  select(pool, sector_broad, occ_group_broad, emp) |>
  pivot_wider(names_from = pool, values_from = emp, names_prefix = "emp_") |>
  mutate(
    d_emp = emp_2022 / emp_2000 - 1
  )


core_sa_emp_by_occ |>
  filter(pool %in% c(2000L, 2022L)) |>
  arrange(desc(emp)) |>
  ggplot(aes(x = pool, y = emp, fill = occ_group)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = occ_group),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 6
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Employment in core social assistance occupations, private sector",
    x = "Year",
    y = "Employment (weighted)",
    fill = "Occupation group"
  ) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 36))

# faceted pie chart of occupational distribution in 2000 vs 2022

core_sa_emp_by_occ |>
  filter(pool %in% c(2000L, 2022L)) |>
  group_by(pool) |>
  mutate(share = emp / sum(emp)) |>
  ungroup() |>
  mutate(pool = factor(pool)) |>
  ggplot(aes(x = "", y = share, fill = occ_group)) +
  geom_col(width = 1, color = "white") +
  geom_text(
    aes(label = scales::percent(share, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 5
  ) +
  coord_polar("y") +
  facet_wrap(~pool) +
  labs(
    title = "Occupational distribution in core social assistance, private sector",
    fill = "Occupation group"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    text = element_text(size = 14)
  )

core_sa_occs_emp |>
  filter(pool %in% c(2000L, 2022L)) |>
  select(-emp_cv) |>
  pivot_wider(names_from = pool, values_from = emp, names_prefix = "emp_") |>
  mutate(
    d_emp = emp_2022 / emp_2000 - 1
  ) |>
  arrange(desc(d_emp)) |>
  ggplot(aes(reorder(occ_group, d_emp), d_emp)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Growth in employment in core social assistance occupations, private sector",
    x = "Occupation group",
    y = "Employment growth, 2000-2022"
  ) +
  theme_minimal() +
  # text needs to be larger...
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

core_sa_occs_emp |>
  ggplot(aes(pool, emp, color = occ_group)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Employment in core social assistance occupations, private sector",
    x = "Year",
    y = "Employment (weighted)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
