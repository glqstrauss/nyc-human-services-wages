source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(REPLIC_PROC_DIR, "acs_prepared.rds"))

acs <- acs |>
  mutate(
    OCCNAME = as.character(haven::as_factor(US2022C_OCCP)),
    OCCGRP = substr(OCCNAME, 1, 3)
  ) |>
  filter(!is.na(occ_group) & (is_city_wkr | sector == "priv_nonprofit"))



occ_groups <- acs |>
  summarize(nw = sum(PERWT), .by = c(OCCGRP, sector)) |>
  mutate(pct = nw / sum(nw) * 100, .by = sector) |>
  select(OCCGRP, sector, pct) |>
  pivot_wider(
    names_from = sector, values_from = pct,
    names_prefix = "pct_", values_fill = 0
  ) |>
  arrange(OCCGRP)

acs |>
  summarize(n = sum(PERWT), .by = c(occ_group, sector)) |>
  arrange(occ_group, sector)
