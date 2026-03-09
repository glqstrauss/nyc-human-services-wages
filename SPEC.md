# SPEC.md — Replication of CNYCA Human Services Wage Study (January 2025)

## Source Report
"Moving Beyond COLAs to Salary Parity for New York City's Nonprofit Human Services Workers"
James A. Parrott, Center for New York City Affairs, The New School — January 2025

See `EXTENSION_SPEC.md` for the multi-city and longitudinal extensions. All replication code and data live under `replication/`; all extension code and data live under `extension/`.

---

## Overview of Analyses to Replicate

The study has five major empirical components:

1. Employment growth trends (BLS CES)
2. Contract spending by program area (NYC OMB budget)
3. Demographic and education composition (ACS via IPUMS)
4. Wage comparisons by education, occupation, and race (ACS via IPUMS)
5. Industry-level average wage ranking (BLS QCEW)

Plus two supplemental benchmarks: True Cost of Living and fringe benefit estimates (not directly replicable from public microdata — see notes).

---

## Data Sources

### 1. American Community Survey (ACS) — Primary microdata source
- **Access**: IPUMS USA (https://usa.ipums.org)
- **Sample**: ACS 5-year 2018–2022 pooled (labeled "2018/22" in the report)
- **Universe**: New York City (five counties: Bronx, Kings, New York, Queens, Richmond — IPUMS `countyfip` codes to be confirmed)
- **Variables to request from IPUMS**:
  - Geography: `statefip`, `countyfip`, `puma`
  - Demographics: `sex`, `race`, `hispan`, `educ`
  - Labor market: `empstat`, `classwkr`, `ind` (industry, 2017 NAICS-based codes), `occ` (occupation, 2018 SOC-based codes), `wkswork2`, `uhrswork`
  - Income: `incwage`
  - Program participation: `foodstmp` (SNAP receipt)
  - Poverty: `poverty` (ratio of family income to poverty threshold, expressed as percent)
  - Weight: `perwt`
- **Supplemental samples** for trend figures:
  - ACS 3-year 2015–2017 pooled (for Figure 8: women-of-color education trend)
  - ACS 3-year 2020–2022 pooled (for Figure 8)
  - 2000 Decennial Census (for 2000-baseline employment count)

### 2. BLS Current Employment Statistics (CES)
- **Access**: BLS data tools (https://www.bls.gov/ces/) or FRED
- **Series needed**:
  - Total NYC private sector payroll employment, monthly, 1990–2024
  - NYC Social Assistance sector employment (NAICS 624), monthly, 1990–2024
  - NYC Child Day Care Services (NAICS 6244), monthly — to subtract from Social Assistance
  - NYC Home Health Care Services (NAICS 6216) — classified under Health Care; may need separate series
- **Note**: "Core Social Assistance" = Social Assistance (NAICS 624) minus Child Day Care Services (NAICS 6244) minus any home health/personal care workers in Social Assistance. Determine the exact CES series codes available for NYC at the sub-sector level.

### 3. BLS Quarterly Census of Employment and Wages (QCEW)
- **Access**: BLS QCEW API or data downloads (https://www.bls.gov/cew/)
- **Geography**: New York City (combine five-county FIPS codes: 36005, 36047, 36061, 36081, 36085)
- **Period**: 2023 annual
- **Coverage**: Private sector; 4-digit NAICS industries with employment > 30,000
- **Key target industry**: NAICS 6241 — Individual and Family Services ($36,688 avg. annual wage in 2023 per report)
- **Task**: Download all private-sector 4-digit NAICS industries for NYC 2023 and filter to those with > 30,000 employment to reproduce Figure 9 (wage ranking across 34 industries)

### 4. NYC Office of Management and Budget (OMB) — Contract Budget Data
- **Access**: NYC OMB Expense, Revenue, and Contract Budgets (published PDFs or accompanying Excel files)
  - Adopted FY 2025 Budget, June 2024
  - Adopted FY 2016 Budget, June 2015 (for FY 2015 baseline)
- **Agencies in scope**: Administration for Children's Services (ACS), Department for the Aging (DFTA), Human Resources Administration (HRA), Department of Homeless Services (DHS), Department of Health and Mental Hygiene (DOHMH), Department of Youth and Community Development (DYCD), Mayor's Office of Criminal Justice (MOCJ), workforce contracts via Department of Small Business Services (SBS)
- **Exclusion**: Early Childhood Education contracts
- **Task**: Extract total contract dollar amounts and contract counts by program area; compute year-over-year and FY15-to-FY25 growth. This data may require manual extraction from the OMB published documents.

### 5. True Cost of Living / Self-Sufficiency Standard
- **Source**: "The Problem of Income Inadequacy, 2023 New York State dataset," Center for Women's Welfare, University of Washington. Available at https://selfsufficiencystandard.org/new-york/
- **Geographies**: Bronx and Brooklyn (Kings County)
- **Family types to use**: Single adult with preschooler; single adult with preschooler and school-age child; two-adult, two-child household
- **Task**: Download or manually extract the relevant self-sufficiency standard dollar amounts for Bronx and Brooklyn for those family configurations.

---

## Defining "Core Human Services" Workers

### In ACS microdata (industry + occupation filters)
1. **Industry filter**: Retain observations in Social Assistance (NAICS 624) **and** Residential Care Facilities (NAICS 623), **excluding** Child Day Care Services (NAICS 6244).
   - NAICS 623 captures group homes, residential mental health/substance abuse facilities, supportive housing, and I/DD residences — all major NYC human services contract areas.
   - IPUMS `INDNAICS` codes: 6241, 6242, 6243 (under 624); 6231, 623M (under 623).
2. **Occupation exclusion (wage analysis only)**: For wage tables (Figures 10–15), exclude home health aides, personal care aides, and nursing assistants. This exclusion is **not** applied for demographic headcounts (Figures 4–8), matching the report's Figure 5 total of ~60,095 workers.
   - IPUMS `occ` codes: 3601 (Home Health Aides), 3602 (Personal Care Aides), 3603 (Nursing Assistants).
3. **Sector split**: Within this universe, further split by `classwkr`:
   - Private nonprofit = "core human services workers" (primary analysis group)
   - Government = "public sector" (comparator)

### Key occupations of interest for occupation-level wage tables
Determine the correct IPUMS `occ` codes for:
- Social and Community Service Managers
- Social Workers (all subtypes: Child, Family & School; Healthcare; Mental Health & Substance Abuse; All Other)
- Counselors (Substance Abuse; Mental Health; Rehabilitation; Educational; Vocational; All Other)
- Social and Human Service Assistants

### Comparison groups (also from ACS)
- **All NYC private sector workers**: `classwkr` = private (employee); all industries; NYC geography
- **All NYC public sector workers**: `classwkr` = government; all levels; NYC geography (note: ~2/3 are NYC City employees)
- **Private hospital workers**: Determine correct IPUMS `ind` code for General Medical and Surgical Hospitals (NAICS 6221) in the private sector

---

## Analysis Modules

### Module 1: Employment Growth Trends (CES)
**Figures to replicate**: Figure 1 (employment growth index, 1990–2024)
- Download monthly CES series for NYC total private employment and Core Social Assistance
- Compute index: set 1990 = 100, calculate cumulative % change through 2024
- Replicate: 168% growth in core social assistance vs. 40% for all private sector

### Module 2: Contract Spending (NYC OMB)
**Figures to replicate**: Figures 2 and 3 (contract totals by agency and growth FY15–FY25)
- Manual extraction of contract dollar totals and counts from OMB budget documents
- Compute growth in total core human services contracts and by sub-program (homeless services is notably 4x growth)

### Module 3: Workforce Demographic Composition (ACS)
**Figures to replicate**: Figures 4–8

**Race/ethnicity variable construction**:
- Create a derived variable from `race` and `hispan`:
  - Hispanic/Latinx: `hispan` != 0 (not Not Hispanic)
  - White (non-Hispanic): `race` == 1 & `hispan` == 0
  - Black (non-Hispanic): `race` == 2 & `hispan` == 0
  - Asian (non-Hispanic): `race` in {4,5,6} & `hispan` == 0
  - Workers of color: all non-White non-Hispanic

**Gender-race combined categories**:
- White men, White women, Men of color, Women of color

**Calculations** (weighted using `perwt`):
- Share of workforce that is women, workers of color, women of color, Black, Latinx, Asian
- Compare: core human services vs. all NYC private sector
- Include both full-time and part-time workers for headcount shares

**Full-time vs. part-time definition**:
- Full-time: `uhrswork` >= 35 (or use `wkswork2` categories as needed for annual earnings)
- The report uses full-time workers for wage comparisons

**Education categories** (from `educ`):
- Less than high school diploma
- High school diploma or GED
- Some college or associate's degree
- Four-year college degree (bachelor's)
- Master's degree or better (master's, professional, doctorate)

### Module 4: Wage Comparisons (ACS)
**Unit of analysis**: Full-time workers (as defined above); median annual `incwage` in 2022 dollars

**Inflation adjustment**:
- All wages to 2022 dollars using CPI-U or PCE deflator; confirm exact deflator used

**Figure 10: Median pay by education level and sector**
- Compute weighted median `incwage` by: {sector} x {education level}
- Sectors: core human services, all private, public sector
- Education levels: 5 categories as above
- Reproduce gap percentages: e.g., bachelor's = 33% below private, 22% below public

**Figures 12–13: Median pay by occupation and sector**
- Compute weighted median `incwage` by: {sector} x {occupation}
- Sectors: core human services, government, private hospitals
- Occupations: Social Workers, Counselors, Social Service Assistants, plus non-professional occupations (Office/Admin Support, Maintenance/Security, Food Service)
- Non-professional occupations are included as a reference point; determine relevant `occ` codes

**Figure 14: Median pay by occupation x education x sector**
- 2x2x2 cross-tabulation: {counselors, social workers} x {bachelor's, post-graduate} x {core human services, government}
- Note small cell sizes — may need to aggregate counselor sub-types for statistical reliability

**Figure 15: Racial pay gaps by occupation x education x sector**
- {workers of color vs. white workers} x {bachelor's, post-graduate} x {core human services, government}
- Restrict to counselors and social workers combined (for sample size)
- Report does NOT further split by gender at this stage

### Module 5: Industry-Level Wage Ranking (QCEW)
**Figure 9**: Bar chart ranking all 4-digit private-sector NAICS industries in NYC with > 30,000 employment, by average annual wage (2023)
- Download QCEW data for NYC private sector, 4-digit NAICS, 2023 annual
- Filter to industries with avg monthly employment > 30,000
- Sort by average annual wage ascending
- Highlight Individual & Family Services (NAICS 6241) as third-lowest at $36,688

### Module 6: Economic Hardship Indicators (ACS)
- Share of core human services workers receiving SNAP (`foodstmp` == 2) vs. public sector and all private sector
- Share with family income < 200% federal poverty line (`poverty` < 200)

### Module 7: True Cost of Living Comparison
- Use self-sufficiency standard values from the downloaded dataset (see Data Sources §5)
- Compare human services worker median salary ($53,600) to self-sufficiency thresholds for representative family types in Bronx/Brooklyn
- This may be a simple lookup/manual table rather than computed from microdata

---

## R Packages

```r
# Core data wrangling
library(tidyverse)
library(data.table)      # optional, for large ACS files

# IPUMS ACS access
library(ipumsr)          # read IPUMS extracts (.dat + .xml)

# BLS data
library(blsR)            # or bls (for CES and QCEW API access)
library(fredr)           # alternative: pull CES series from FRED

# Weighted statistics
library(survey)          # design-based weighted medians
library(srvyr)           # tidyverse-style wrapper for survey

# Visualization
library(ggplot2)
library(scales)
library(ggtext)          # rich text annotations in plots

# Excel/PDF data extraction (for OMB budget tables)
library(readxl)
library(pdftools)        # if extracting from OMB PDFs
```

---

## Output Structure

All replication code and data live under `replication/`:

```
replication/
├── data/
│   ├── raw/
│   │   ├── ipums/          # ACS 2018-22 five-year extract from IPUMS
│   │   ├── qcew/           # QCEW NYC 2023 annual data
│   │   ├── ces/            # BLS CES monthly series (also referenced by extension/)
│   │   └── omb/            # NYC OMB FY2025 and FY2016 budget files
│   └── processed/
├── R/
│   ├── 00_setup.R          # package loading, constants, paths
│   ├── 01_acs_prep.R       # load + clean IPUMS extract; industry/occ filters
│   ├── 02_ces_prep.R       # download + clean BLS CES series
│   ├── 03_qcew_prep.R      # download + clean QCEW annual data
│   ├── 10_demographics.R   # workforce composition (Figs 4-8)
│   ├── 11_wages.R          # wage comparison tables (Figs 10-15)
│   ├── 12_hardship.R       # SNAP receipt + poverty (Module 6)
│   ├── 13_qcew_ranking.R   # industry wage ranking (Fig 9)
│   └── 20_figures.R        # assemble all ggplot figures
└── output/figures/
```

**Note**: Write the industry/occupation filter logic in `01_acs_prep.R` as exportable functions so extension code can reuse them without duplication.

---

## Manual Resolution Steps

The following items require human judgment or external research before the code can be written:

1. **NAICS → IPUMS `ind` code mapping**: Confirm exact IPUMS industry codes for Social Assistance (NAICS 624), Child Day Care Services (NAICS 6244), General Medical and Surgical Hospitals (NAICS 6221), and any relevant sub-industries. IPUMS uses its own harmonized industry codes — consult the IPUMS variable documentation.

2. **SOC → IPUMS `occ` code mapping**: Confirm IPUMS occupation codes for Social Workers (all subtypes), Counselors (all subtypes), Social and Human Service Assistants, Social and Community Service Managers, Home Health Aides, and Personal Care Aides. IPUMS 2018 occupation codes are based on 2018 SOC.

3. **CES sub-sector series availability for NYC**: Verify which 4- and 5-digit NAICS breakdowns are available in the CES for New York City specifically, as state-area CES data may not publish all detailed sub-industries.

4. **QCEW NYC geography**: Confirm whether QCEW provides pre-aggregated NYC (5-county) data or requires summing county-level records (FIPS: 36005, 36047, 36061, 36081, 36085).

5. **OMB contract data format**: Examine whether the OMB Adopted Budget contract schedules are available as machine-readable Excel or only as PDF tables requiring extraction.

6. **Self-Sufficiency Standard download**: Obtain the 2023 New York State dataset from https://selfsufficiencystandard.org/new-york/ and identify the correct family-type rows for Bronx and Brooklyn.

7. **Inflation deflator**: Confirm whether the report used CPI-U (national), CPI-U for New York metro area, or another deflator to express all wages in 2022 dollars.

8. **Weighted median method**: Confirm the survey design for ACS weighted medians — the `survey` / `srvyr` packages handle replicate weights if using the full ACS PUMS with replicate weights for variance estimation.
