# TODO: This is a work in progress and more interesting as a cross-validation
# Since we have administrative data for city workers, we can theoretically
# Grab everyone in a specific title...

source(here::here("extension/R/00_setup.R"))

payroll <- readRDS(file.path(EXT_PROC_DIR, "nyc_payroll_prepared.rds"))

table211_total_ft_headcounts <- payroll |>
  filter(
    leave_status_as_of_june_30 == "ACTIVE",
    pay_basis == "per Annum"
  ) |>
  group_by(fiscal_year) |>
  summarize(n = n())

Yes gov't fails but what are th limits of the current dependence on nonprofits?
What are the limits of the "non-profit solution"
1. You get what you pay for
2. Symptom *management* vs addressing root causes
