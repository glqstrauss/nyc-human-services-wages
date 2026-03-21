source(here::here("extension/R/00_setup.R"))

payroll <- readRDS(file.path(EXT_PROC_DIR, "nyc_payroll_prepared.rds"))

table211_total_ft_headcounts <- payroll |>
  filter(
    leave_status_as_of_june_30 == "ACTIVE",
    pay_basis == "per Annum"
  ) |>
  group_by(fiscal_year) |>
  summarize(n = n())
