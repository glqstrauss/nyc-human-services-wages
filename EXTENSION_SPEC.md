# EXTENSION_SPEC.md — Extensions to CNYCA Human Services Wage Study

## Overview

This spec covers two extensions beyond the NYC 2018–22 replication (see `SPEC.md`):

1. **Multi-city cross-section**: Three-way wage gap (nonprofit vs. public vs. for-profit private) across major U.S. cities using ACS 2018–22
2. **NYC longitudinal**: How the nonprofit wage gap has evolved over time within New York City, from 2005 to 2022 (and optionally back to 2000/1990)

Both extensions share the same industry/occupation filters and worker definitions established in the replication. The replication code in `replication/R/01_acs_prep.R` should be written so its filter logic can be imported as functions by the extension code.

---

## Directory Structure

```
human_services_wages/
├── SPEC.md                  # replication spec
├── EXTENSION_SPEC.md        # this file
├── replication/
│   ├── data/raw/            # NYC 2018-22 data (ACS, QCEW, CES, OMB)
│   ├── data/processed/
│   ├── R/
│   └── output/figures/
└── extension/
    ├── data/raw/
    │   ├── ipums_multicity/     # ACS 2018-22 multi-metro extract
    │   ├── ipums_longitudinal/  # ACS 5-year extracts by period; 2000/1990 Census
    │   └── ces/                 # CES series for longitudinal context (already in replication; symlink or copy)
    ├── data/processed/
    ├── R/
    │   ├── 00_setup.R
    │   ├── 01_multicity_prep.R
    │   ├── 02_multicity_analysis.R
    │   ├── 03_longitudinal_prep.R
    │   ├── 04_longitudinal_analysis.R
    │   └── 05_figures.R
    └── output/figures/
```

---

## Extension 1: Multi-City Three-Way Wage Gap

### Research Question
Across major U.S. cities, how does the median wage of nonprofit human services workers compare to (a) all public sector workers and (b) all private for-profit workers in the same city? Does the structure of the gap — which comparator group is further from nonprofit workers — vary across cities?

### Cities in Scope
Starting set: New York City, Los Angeles, Chicago, San Francisco, Boston. Can expand to other large metros (Houston, Philadelphia, Seattle, Washington DC) if sample sizes permit.

### Three-Way Worker Classification

Using ACS `classwkr`:
- **Nonprofit human services**: `ind` in core human services range AND `classwkr` = private not-for-profit
- **Public sector**: `classwkr` = government (federal, state, or local combined)
- **For-profit private**: `classwkr` = private for-profit (all industries, excluding the human services nonprofit group)

This mirrors Parrott's comparator logic — the public and for-profit groups are citywide, not restricted to the human services industry — while cleanly separating nonprofits from for-profits within the private sector.

**Note**: Parrott's "all private sector" comparator silently includes nonprofits in other industries (hospitals, universities, etc.) which pull the comparator median upward. Stripping them out via `classwkr` makes the for-profit comparator cleaner and slightly changes the gap magnitude relative to Parrott's figures. Flag this in a methods note.

### Geography

Use IPUMS `met2013` (2013 OMB metro area definitions) to identify metros. Each city should be defined as its core MSA, not the broader CBSA, for consistency.

- Determine the correct `met2013` codes for each target city.
- For NYC: confirm whether to use the NYC MSA (which includes NJ suburbs) or restrict to the five boroughs using `countyfip`. Recommend restricting to five boroughs for consistency with replication.

### Primary Metric: Wage Gap Index

For each city, compute:
- **Nonprofit-to-public gap**: `(median_public - median_nonprofit) / median_public` — same formula as Parrott
- **Nonprofit-to-forprofit gap**: `(median_forprofit - median_nonprofit) / median_forprofit`

Express as percent shortfall. This is comparable across cities without cost-of-living adjustment because it is a ratio within city. A separate cost-of-living-adjusted nominal comparison (e.g., using regional CPI or Self-Sufficiency Standards) can be added as a supplemental exhibit.

### Stratifications (in order of priority)

Run the gap index for each city across these cuts:
1. All workers combined (education-pooled)
2. By education level: bachelor's degree holders; post-graduate degree holders
3. By gender: women vs. men (if sample sizes permit per city)

Occupation-level cuts (social workers, counselors) are feasible for NYC and LA but likely too small for Boston and SF — compute by city and suppress cells below a minimum n (suggest n < 50 suppressed, n < 100 flagged).

### Data: IPUMS Extract for Multi-City

A single IPUMS extract can cover all cities:
- **Sample**: ACS 2018–2022 five-year
- **Geographic filter at extract time**: restrict to states containing target metros (reduces file size)
- **Variables**: same as replication extract plus `met2013`
- No additional variables needed beyond the replication list

### Sample Size Check

Before finalizing the analysis plan, compute unweighted cell counts for each city × sector × education combination. If any target city has fewer than 100 unweighted nonprofit human services observations in the five-year ACS, consider dropping it or collapsing education categories.

---

## Extension 2: NYC Longitudinal Wage Gap

### Research Question
How has the three-way wage gap (nonprofit vs. public vs. for-profit private) within New York City's human services sector evolved from 2005 to 2022? Does the gap widen or narrow as the nonprofit sector grows? Can we extend meaningfully to 2000 or 1990?

### Time Periods and Data Sources

| Period | Source | Notes |
|---|---|---|
| 2018–22 | ACS 5-year | Replication baseline; already in `replication/data/` |
| 2015–19 | ACS 5-year | Clean; NAICS-based, same occupation codes |
| 2010–14 | ACS 5-year | Clean; minor occupation code revision in 2012 (flag) |
| 2005–09 | ACS 5-year | Earliest ACS 5-year; industry/occupation codes consistent with later periods |
| 2000 | Decennial Census | Transitional NAICS/SIC hybrid; one additional decade; note coding uncertainty |
| 1990 | Decennial Census | SIC-based; feasible for education-level gap only; treat as directional |

**Recommended primary series**: 2005–09, 2010–14, 2015–19, 2018–22 (four non-overlapping 5-year windows). Clean and publishable without caveats.

**Supplemental**: Add 2000 Census with a methods note on the SIC/NAICS transition.

**Use caution / flag prominently**: 1990 Census. Report the education-level gap only; suppress occupation-level cuts.

### Industry/Occupation Code Handling Across Time

**Industry codes (IPUMS `ind`)**:

IPUMS harmonizes industry codes but the underlying NAICS revisions (2002, 2007, 2012, 2017) and the SIC→NAICS transition create known discontinuities:

- 2005–2022 ACS: All NAICS-based. The Social Assistance (NAICS 624) and child day care (NAICS 6244) definitions are stable. Use the same `ind` filter as the replication across all four ACS windows.
- 2000 Census: IPUMS uses a partially NAICS-based scheme. Determine whether the `ind` codes for Social Assistance and child day care map cleanly to the post-2002 scheme. Consult IPUMS comparability flags for `ind`.
- 1990 Census: SIC-based. SIC 83 (Social Services) ≈ NAICS 624. Exclude SIC 8351 (child day care) and confirm home health (SIC 808) is classified under Health Services and thus already excluded.

**Occupation codes (IPUMS `occ`)**:

SOC revisions occurred in 2000, 2010, and 2018. IPUMS maps these but granular categories shift:
- For 2005–2022: Occupation-level wage gaps are feasible. Use broad groupings (social workers aggregate; counselors aggregate) rather than sub-types to minimize crosswalk uncertainty.
- For 2000: Social workers and counselors are identifiable but boundary definitions differ slightly. Use with a note.
- For 1990: Do not produce occupation-level cuts. Education-level analysis only.

**Practical approach**: Write a function in `03_longitudinal_prep.R` that accepts a loaded ACS/Census dataset and a `year_group` argument (`"2005"`, `"2010"`, `"2015"`, `"2018"`, `"2000"`, `"1990"`) and applies the appropriate `ind` and `occ` filters for that period, with any year-specific adjustments documented inline.

### Longitudinal Wage Gap Metrics

For each time period compute for NYC:
- Median nominal wage by sector (nonprofit HS, public, for-profit private)
- Median real wage (2022 dollars) — confirm deflator; recommend BLS CPI-U for NYC-Newark-Jersey City metro
- Nonprofit-to-public gap index and nonprofit-to-forprofit gap index (same formula as Extension 1)
- Share of workforce by sector type (to track the relative size of nonprofit HS vs. public vs. for-profit over time)

### Combining with CES Employment Trends

The CES series from the replication (core Social Assistance employment, 1990–2024) provides a more reliable employment count than ACS microdata. Use CES to contextualize the wage gap trend:
- Plot CES employment growth alongside the ACS wage gap index on a dual-axis or side-by-side panel
- Test visually whether periods of rapid nonprofit sector expansion (e.g., post-2010 homeless services growth) correspond to widening gaps

The CES data is already in `replication/data/raw/ces/`. Reference it from the extension rather than duplicating.

### Data: IPUMS Extracts for Longitudinal

Separate extracts are needed per time period (IPUMS does not allow mixing ACS multi-year samples with Census samples in a single extract):
- One extract: ACS 5-year 2005–09
- One extract: ACS 5-year 2010–14
- One extract: ACS 5-year 2015–19
- Replication extract (ACS 5-year 2018–22): already in `replication/data/raw/ipums/`
- One extract: 2000 Decennial Census (if including)
- One extract: 1990 Decennial Census (if including)

Variables are the same as the replication extract for all periods. For 1990, verify that `classwkr` (private not-for-profit) and `foodstmp` are available; confirm with IPUMS variable availability table.

---

## Shared Methods Notes for Both Extensions

### Weighted Median Estimation
Use `srvyr` with `survey_median()` for all median estimates. For variance estimation, use replicate weights if available (ACS PUMS replicate weights are available from IPUMS as a separate download — `repwtp` series). This matters for confidence intervals on small cells in Boston/SF.

### Wage Deflation
- For multi-city cross-section (2018–22): no deflation needed; all periods averaged together
- For longitudinal: deflate to 2022 dollars using BLS CPI-U for the NYC metro area (series CUURA101SA0). Confirm this is the deflator implied by Parrott's "2022 dollars" figures.

### Suppression Rules
- Suppress cells with unweighted n < 50
- Flag cells with unweighted n 50–99
- Document all suppressed cells in a summary table

---

## Manual Resolution Steps (Extension-Specific)

1. **IPUMS `met2013` codes**: Look up the correct codes for LA, Chicago, SF, and Boston in the IPUMS geographic variables documentation.
2. **NYC five-borough restriction**: Decide whether to use `met2013` == NYC MSA or `countyfip` == {Bronx, Kings, New York, Queens, Richmond} for consistency with replication. The latter is more restrictive and preferred.
3. **ACS 5-year 2005–09 industry code mapping**: Confirm with IPUMS comparability documentation that the `ind` codes for NAICS 624 and 6244 are identical between 2005–09 and 2018–22 extracts.
4. **2000 Census `ind` codes**: Determine whether IPUMS maps 2000 Census industry codes to the same `ind` values as ACS years, or whether a crosswalk is needed.
5. **1990 SIC codes**: Look up IPUMS `ind` codes corresponding to SIC 83 (Social Services) and SIC 8351 (Child Day Care) in the 1990 Census extract.
6. **CPI-U NYC metro series**: Download BLS series CUURA101SA0 (All items, NYC-Newark-Jersey City) for deflation. Verify Parrott's 2022-dollar figures are consistent with this deflator.
7. **Replicate weights**: Decide whether to use replicate weights (`repwtp`) for confidence intervals. Required if any cells are small enough that standard errors matter for the findings.
8. **Sample size feasibility check**: Before writing any Extension 1 analysis code, compute unweighted nonprofit human services worker counts for each target city from the multi-city IPUMS extract and confirm all cities clear the minimum threshold.
