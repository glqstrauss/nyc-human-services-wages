source(here::here("extension/R/00_setup.R"))

acs <- readRDS(file.path(REPLIC_PROC_DIR, "acs_prepared.rds"))

acs_wages <- acs |> filter(
    full_time == TRUE,
    INCWAGE < 999999,
    INCWAGE > 0
)


##############################################

make_sectoral_plot <- function(data, xvar, title) {
    data |>
        ggplot(aes(x = {{ xvar }}, y = med_wage, fill = sector)) +
        geom_col(position = position_dodge(width = 0.9)) +
        geom_text(
            aes(label = round(med_wage, 0)),
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

acs_parrott_sectors <- acs_wages |>
    mutate(
        sector = case_when(
            is_hs_wages ~ "Core HS Nonprofit",
            sector == "govt" ~ "All Public Sector",
            sector == "priv_forprofit" ~ "All Private Sector",
        )
    ) |>
    filter(!is.na(sector)) |>
    summarize(
        med_wage = median(INCWAGE),
        .by = c(educ_cat, sector)
    )

make_sectoral_plot(
    acs_parrott_sectors,
    educ_cat,
    paste(
        "Parrott's Comparison Between Core HS Nonprofit and ",
        "Economy-wide Wages in Public and Private Sectors"
    )
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
    filter(!is.na(occ_group) & !is.na(sector)) |>
    summarize(
        med_wage = median(INCWAGE),
        .by = c(educ_cat, sector)
    )


make_sectoral_plot(
    acs_hs_sectors,
    educ_cat,
    paste(
        "Comparison Between Social Assistance Occupations",
        "in Nonprofit, Public, and Private Sectors"
    )
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
    filter(!is.na(sector)) |>
    summarize(
        med_wage = median(INCWAGE),
        .by = c(educ_cat, sector)
    )


make_sectoral_plot(
    acs_all_sectors,
    educ_cat,
    paste(
        "Economy-Wide Comparison of Sectors in NYC"
    )
)
