source(here::here("extension/R/00_setup.R"))

payroll <- readr::read_csv(file.path(EXT_RAW_DIR, "nyc_open_data", "citywide_payroll_data_20260316.csv"))
payroll <- payroll |>
  rename_with(\(x) tolower(str_replace_all(x, " ", "_")))

saveRDS(payroll, file.path(EXT_PROC_DIR, "nyc_payroll_prepared.rds"))

agency_codes <- readr::read_csv(file.path(EXT_RAW_DIR, "nyc_open_data", "agency_codes.csv"), col_names = c("agency_code", "agency_name", "agency_short_name")) |>
  rename_with(\(x) tolower(str_replace_all(x, " ", "_")))

saveRDS(agency_codes, file.path(EXT_PROC_DIR, "nyc_agency_codes_prepared.rds"))

civil_list <- readr::read_csv(file.path(EXT_RAW_DIR, "nyc_open_data", "civil_list_20260317.csv")) |>
  rename_with(\(x) tolower(str_replace_all(x, " ", "_")))

saveRDS(civil_list, file.path(EXT_PROC_DIR, "nyc_civil_list_prepared.rds"))

message(paste("Saved nyc_payroll_prepared.rds (", format(nrow(payroll), big.mark = ","), "rows)."))
