# 20_figures.R
# Assemble all ggplot figures from saved analysis tables.
# Run after 10_demographics.R, 11_wages.R, and 12_hardship.R.
#
# Inputs:  replication/data/processed/{demographics,wage,hardship}_tables.rds
# Outputs: replication/output/figures/*.png

source(here::here("replication/R/00_setup.R"))

# ── Load tables ───────────────────────────────────────────────────────────────

demo <- readRDS(file.path(PROC_DIR, "demographics_tables.rds"))
wages <- readRDS(file.path(PROC_DIR, "wage_tables.rds"))
hard <- readRDS(file.path(PROC_DIR, "hardship_tables.rds"))

# ── Shared helpers ────────────────────────────────────────────────────────────

save_fig <- function(p, name, width = 8, height = 5) {
  path <- file.path(FIG_DIR, paste0(name, ".png"))
  ggsave(path, p, width = width, height = height, dpi = 150)
  message("Saved: ", basename(path))
  invisible(p)
}

SECTOR_LABELS <- c(
  hs_nonprofit   = "Human Services\n(Nonprofit)",
  govt           = "Public Sector",
  priv_forprofit = "Private For-Profit"
)

GROUP_LABELS <- c(
  hs_nonprofit = "Human Services\n(Nonprofit)",
  all_private  = "All Private\nSector"
)

EDUC_LABELS <- c(
  lths         = "Less than\nHigh School",
  hs           = "High School\nDiploma/GED",
  some_college = "Some College\nor Associate's",
  bachelors    = "Bachelor's\nDegree",
  postgrad     = "Master's or\nHigher"
)

SECTOR_COLORS <- c(
  "Human Services\n(Nonprofit)" = "#1f77b4",
  "Public Sector"               = "#ff7f0e",
  "Private For-Profit"          = "#2ca02c"
)

# =============================================================================
# Figure 4: Share women by sector
# =============================================================================

fig4_data <- demo$tbl1_gender_share |>
  mutate(
    group_label = GROUP_LABELS[group],
    share_pct   = share_women * 100
  )

fig4 <- ggplot(fig4_data, aes(x = group_label, y = share_pct, fill = group_label)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(share_pct, 1), "%")),
    vjust = -0.4, size = 4
  ) +
  scale_y_continuous(limits = c(0, 85), labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = c(
    "Human Services\n(Nonprofit)" = "#1f77b4",
    "All Private\nSector"         = "#aec7e8"
  )) +
  labs(
    title = "Share of Full-Time Workers Who Are Women",
    subtitle = "NYC, 2018-2022 ACS five-year average",
    x = NULL, y = "Share of full-time workers"
  ) +
  theme_parrott()

save_fig(fig4, "fig04_gender_share")

# =============================================================================
# Figure 5: Race/ethnicity shares
# =============================================================================

fig5_data <- demo$tbl2_race_share |>
  select(group, share_poc, share_black, share_hispanic, share_asian) |>
  pivot_longer(-group, names_to = "race_group", values_to = "share") |>
  mutate(
    group_label = GROUP_LABELS[group],
    race_label = recode(race_group,
      share_poc      = "Workers of\nColor (total)",
      share_black    = "Black\n(non-Hispanic)",
      share_hispanic = "Hispanic/\nLatinx",
      share_asian    = "Asian\n(non-Hispanic)"
    ),
    share_pct = share * 100
  )

fig5 <- ggplot(fig5_data, aes(x = race_label, y = share_pct, fill = group_label)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(share_pct, 1), "%")),
    position = position_dodge(0.7), vjust = -0.4, size = 3
  ) +
  scale_y_continuous(limits = c(0, 95), labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = c(
    "Human Services\n(Nonprofit)" = "#1f77b4",
    "All Private\nSector"         = "#aec7e8"
  ), name = NULL) +
  labs(
    title = "Race/Ethnicity Composition of Full-Time Workers",
    subtitle = "NYC, 2018-2022 ACS five-year average",
    x = NULL, y = "Share of full-time workers"
  ) +
  theme_parrott()

save_fig(fig5, "fig05_race_share")

# =============================================================================
# Figure 6: Gender x race shares
# =============================================================================

fig6_data <- demo$tbl3_gender_race_share |>
  mutate(
    group_label = GROUP_LABELS[group],
    gender_race_lbl = recode(gender_race,
      white_men      = "White Men",
      white_women    = "White Women",
      men_of_color   = "Men of Color",
      women_of_color = "Women of Color"
    ),
    share_pct = share * 100
  )

fig6 <- ggplot(fig6_data, aes(x = gender_race_lbl, y = share_pct, fill = group_label)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(share_pct, 1), "%")),
    position = position_dodge(0.7), vjust = -0.4, size = 3
  ) +
  scale_y_continuous(limits = c(0, 60), labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = c(
    "Human Services\n(Nonprofit)" = "#1f77b4",
    "All Private\nSector"         = "#aec7e8"
  ), name = NULL) +
  labs(
    title = "Gender and Race Composition of Full-Time Workers",
    subtitle = "NYC, 2018-2022 ACS five-year average",
    x = NULL, y = "Share of full-time workers"
  ) +
  theme_parrott()

save_fig(fig6, "fig06_gender_race_share")

# =============================================================================
# Figure 7: Education shares
# =============================================================================

fig7_data <- demo$tbl4_educ_share |>
  mutate(
    group_label = case_when(
      group == "hs_nonprofit" ~ "Human Services\n(Nonprofit)",
      group == "govt" ~ "Public Sector",
      group == "all_private" ~ "All Private\nSector",
      TRUE ~ group
    ),
    educ_label = factor(EDUC_LABELS[as.character(educ_cat)],
      levels = unname(EDUC_LABELS)
    ),
    share_pct = share * 100
  )

fig7 <- ggplot(fig7_data, aes(x = educ_label, y = share_pct, fill = group_label)) +
  geom_col(position = "dodge", width = 0.75) +
  geom_text(aes(label = paste0(round(share_pct, 1), "%")),
    position = position_dodge(0.75), vjust = -0.4, size = 2.8
  ) +
  scale_y_continuous(limits = c(0, 42), labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = c(
    "Human Services\n(Nonprofit)" = "#1f77b4",
    "Public Sector"               = "#ff7f0e",
    "All Private\nSector"         = "#aec7e8"
  ), name = NULL) +
  labs(
    title = "Education Level of Full-Time Workers",
    subtitle = "NYC, 2018-2022 ACS five-year average",
    x = NULL, y = "Share of full-time workers"
  ) +
  theme_parrott()

save_fig(fig7, "fig07_educ_share", width = 10, height = 5)

# =============================================================================
# Figure 10: Median wage by education × sector
# =============================================================================

fig10_data <- wages$W2_long |>
  filter(sector %in% names(SECTOR_LABELS)) |>
  mutate(
    sector_label = factor(SECTOR_LABELS[as.character(sector)],
      levels = unname(SECTOR_LABELS)
    ),
    educ_label = factor(EDUC_LABELS[as.character(educ_cat)],
      levels = unname(EDUC_LABELS)
    )
  ) |>
  filter(!is.na(educ_label))

fig10 <- ggplot(
  fig10_data,
  aes(x = educ_label, y = median_wage, fill = sector_label)
) +
  geom_col(position = "dodge", width = 0.75) +
  geom_text(aes(label = paste0("$", round(median_wage / 1000, 0), "k")),
    position = position_dodge(0.75), vjust = -0.4, size = 2.8
  ) +
  scale_y_continuous(
    labels = function(x) paste0("$", x / 1000, "k"),
    limits = c(0, 135000)
  ) +
  scale_fill_manual(values = unname(SECTOR_COLORS), name = NULL) +
  labs(
    title = "Median Annual Pay by Education Level and Sector",
    subtitle = "Full-time workers, NYC, 2018-2022 ACS (pooled dollars)",
    x = NULL, y = "Median annual wage"
  ) +
  theme_parrott() +
  theme(axis.text.x = element_text(size = 9))

save_fig(fig10, "fig10_wage_by_educ_sector", width = 10, height = 5)

# =============================================================================
# Figure 12: Median wage by occupation × sector
# =============================================================================

OCC_LABELS <- c(
  social_workers = "Social Workers",
  counselors     = "Counselors",
  hs_assistants  = "Human Service\nAssistants",
  admin_support  = "Admin/Office\nSupport",
  janitors       = "Janitors &\nBuilding Cleaners"
)

fig12_data <- wages$W3_by_occ_sector |>
  filter(occ_group %in% names(OCC_LABELS)) |>
  select(occ_group, median_hs_nonprofit, median_govt, median_priv_hospital) |>
  pivot_longer(-occ_group, names_to = "comp_sector", values_to = "median_wage") |>
  mutate(
    comp_label = recode(comp_sector,
      median_hs_nonprofit   = "Human Services\n(Nonprofit)",
      median_govt           = "Public Sector",
      median_priv_hospital  = "Private Hospitals"
    ),
    occ_label = factor(OCC_LABELS[occ_group], levels = unname(OCC_LABELS))
  ) |>
  filter(!is.na(median_wage), !is.na(occ_label))

fig12 <- ggplot(
  fig12_data,
  aes(x = occ_label, y = median_wage, fill = comp_label)
) +
  geom_col(position = "dodge", width = 0.75) +
  geom_text(aes(label = paste0("$", round(median_wage / 1000, 0), "k")),
    position = position_dodge(0.75), vjust = -0.4, size = 2.8
  ) +
  scale_y_continuous(
    labels = function(x) paste0("$", x / 1000, "k"),
    limits = c(0, 100000)
  ) +
  scale_fill_manual(
    values = c(
      "Human Services\n(Nonprofit)" = "#1f77b4",
      "Public Sector"               = "#ff7f0e",
      "Private Hospitals"           = "#d62728"
    ),
    name = NULL
  ) +
  labs(
    title = "Median Annual Pay by Occupation and Sector",
    subtitle = "Full-time workers, NYC, 2018-2022 ACS (pooled dollars)",
    x = NULL, y = "Median annual wage"
  ) +
  theme_parrott() +
  theme(axis.text.x = element_text(size = 9))

save_fig(fig12, "fig12_wage_by_occ_sector", width = 10, height = 5)

# =============================================================================
# Figure 14: Occupation × education × sector (2x2x2)
# =============================================================================

fig14_data <- wages$W4_long |>
  mutate(
    occ_label = recode(as.character(occ_group),
      social_workers = "Social Workers",
      counselors     = "Counselors"
    ),
    educ_label = recode(as.character(educ_cat),
      bachelors = "Bachelor's",
      postgrad  = "Master's+"
    ),
    sector_label = recode(as.character(sector),
      hs_nonprofit = "Human Services\n(Nonprofit)",
      govt         = "Public Sector"
    ),
    panel_x = paste(educ_label, occ_label, sep = "\n")
  )

fig14 <- ggplot(
  fig14_data,
  aes(x = panel_x, y = median_wage, fill = sector_label)
) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0("$", round(median_wage / 1000, 0), "k")),
    position = position_dodge(0.7), vjust = -0.4, size = 2.8
  ) +
  scale_y_continuous(
    labels = function(x) paste0("$", x / 1000, "k"),
    limits = c(0, 105000)
  ) +
  scale_fill_manual(
    values = c(
      "Human Services\n(Nonprofit)" = "#1f77b4",
      "Public Sector"               = "#ff7f0e"
    ),
    name = NULL
  ) +
  labs(
    title = "Median Pay by Occupation, Education, and Sector",
    subtitle = "Full-time workers, NYC, 2018-2022 ACS (pooled dollars)",
    x = NULL, y = "Median annual wage"
  ) +
  theme_parrott() +
  theme(axis.text.x = element_text(size = 8))

save_fig(fig14, "fig14_wage_occ_educ_sector", width = 10, height = 5)

# =============================================================================
# Figure 15: Racial pay gaps
# =============================================================================

fig15_data <- wages$W5_long |>
  filter(!is.na(median_wage)) |>
  mutate(
    educ_label = recode(as.character(educ_cat),
      bachelors = "Bachelor's", postgrad = "Master's+"
    ),
    sector_label = recode(as.character(sector),
      hs_nonprofit = "Human Services\n(Nonprofit)",
      govt         = "Public Sector"
    ),
    race_label = recode(race_grp,
      white_nh = "White (non-Hispanic)",
      poc      = "Workers of Color"
    ),
    panel_x = interaction(educ_label, sector_label, sep = "\n")
  )

fig15 <- ggplot(
  fig15_data,
  aes(x = panel_x, y = median_wage, fill = race_label)
) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0("$", round(median_wage / 1000, 0), "k")),
    position = position_dodge(0.7), vjust = -0.4, size = 2.8
  ) +
  scale_y_continuous(
    labels = function(x) paste0("$", x / 1000, "k"),
    limits = c(0, 108000)
  ) +
  scale_fill_manual(
    values = c("White (non-Hispanic)" = "#2ca02c", "Workers of Color" = "#98df8a"),
    name = NULL
  ) +
  labs(
    title = "Racial Pay Gaps: Social Workers and Counselors",
    subtitle = "By education level and sector, full-time workers, NYC, 2018-2022 ACS",
    x = NULL, y = "Median annual wage"
  ) +
  theme_parrott() +
  theme(axis.text.x = element_text(size = 7))

save_fig(fig15, "fig15_racial_pay_gaps", width = 10, height = 5)

# =============================================================================
# SNAP receipt rate
# =============================================================================

snap_data <- hard$H1_snap_by_sector |>
  mutate(
    sector_label = recode(as.character(sector),
      hs_nonprofit   = "Human Services\n(Nonprofit)",
      govt           = "Public Sector",
      priv_forprofit = "Private For-Profit"
    )
  )

fig_snap <- ggplot(snap_data, aes(
  x = sector_label, y = snap_rate_pct,
  fill = sector_label
)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_errorbar(aes(ymin = snap_rate_low * 100, ymax = snap_rate_upp * 100),
    width = 0.2
  ) +
  geom_text(aes(label = paste0(snap_rate_pct, "%")),
    vjust = -1.0, size = 4
  ) +
  scale_y_continuous(limits = c(0, 22), labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = c(
    "Human Services\n(Nonprofit)" = "#1f77b4",
    "Public Sector"               = "#ff7f0e",
    "Private For-Profit"          = "#2ca02c"
  )) +
  labs(
    title = "Share of Workers Receiving SNAP (Food Stamps)",
    subtitle = "All workers, NYC, 2018-2022 ACS five-year average",
    x = NULL, y = "Share receiving SNAP"
  ) +
  theme_parrott()

save_fig(fig_snap, "fig_snap_by_sector")

cat("\nAll figures saved to:", FIG_DIR, "\n")
cat("Files written:\n")
cat(paste0("  ", list.files(FIG_DIR, pattern = "\\.png$"), collapse = "\n"), "\n")
