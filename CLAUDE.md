# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Replication and extension of Parrott (2025), "Moving Beyond COLAs to Salary Parity for New York City's Nonprofit Human Services Workers" (CNYCA/The New School). The project is split into two independent workstreams:

- **`replication/`** — NYC 2018–22 snapshot replication using ACS, QCEW, CES, and NYC OMB data. Spec: `SPEC.md`.
- **`extension/`** — Two extensions: (1) multi-city three-way wage gap across major U.S. metros; (2) longitudinal NYC wage gap trend 2005–2022. Spec: `EXTENSION_SPEC.md`.

## Repository Structure

```
human_services_wages/
├── SPEC.md               # replication plan
├── EXTENSION_SPEC.md     # extension plan
├── replication/
│   ├── data/raw/         # ipums/, qcew/, ces/, omb/
│   ├── data/processed/
│   ├── R/                # 0x = data prep, 1x = analysis, 2x = figures
│   └── output/figures/
└── extension/
    ├── data/raw/         # ipums_multicity/, ipums_longitudinal/, ces/
    ├── data/processed/
    ├── R/                # 00_setup.R through 05_figures.R
    └── output/figures/
```

The CES data in `replication/data/raw/ces/` is shared — extension code references it rather than duplicating it.

## Key Architectural Decision

The industry/occupation filter that defines "core human services workers" lives in `replication/R/01_acs_prep.R` as exported functions. Extension scripts source or import these functions rather than redefining the filter logic.

## R Environment

VS Code with `reditorsupport.r` extension; `languageserver` installed for IDE integration.

```bash
Rscript replication/R/11_wages.R        # run a script
Rscript -e "source('R/01_acs_prep.R')"  # source interactively
```

## R Packages

```r
install.packages(c(
  "tidyverse", "ipumsr",          # core data wrangling + IPUMS extract loading
  "survey", "srvyr",              # weighted median estimation
  "blsR", "fredr",                # BLS CES and QCEW API access
  "ggplot2", "scales", "ggtext",  # figures
  "readxl", "pdftools"            # OMB budget extraction
))
```

## Data Sources

All primary data must be obtained externally before running analysis:

| Source | Access | Used in |
|---|---|---|
| ACS 2018–22 five-year (IPUMS) | ipums.org — register and build extract | replication, extension |
| ACS 2005–09, 2010–14, 2015–19 five-year (IPUMS) | Same — separate extracts per period | extension (longitudinal) |
| 2000 Decennial Census (IPUMS) | Same | extension (longitudinal, optional) |
| BLS QCEW — NYC 2023 annual | bls.gov/cew | replication |
| BLS CES — NYC monthly 1990–2024 | bls.gov/ces or FRED | replication, extension |
| NYC OMB Adopted Budget FY2025 and FY2016 | nyc.gov/omb | replication |
| Self-Sufficiency Standard 2023 NY | selfsufficiencystandard.org/new-york | replication |