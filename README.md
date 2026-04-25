# Meat Demand and Carbon Tax Simulation — Replication Code

This repository contains the full replication code for the empirical analysis
in my thesis. The analysis estimates a demand system for meat in Italy using
household-level survey data, simulates the effect of a carbon tax on consumption,
and evaluates the welfare and environmental consequences across different household
types and income groups.

---

## Data

The analysis uses two data sources:

**1. Italian Household Budget Survey (HBS)**
Micro-level data on household expenditure collected by ISTAT, for the year 2023. Each row represents
one household and contains monthly expenditure on individual food items, household
size, region, survey weights, and demographic information on each household member.
The data is publicly available from ISTAT at: https://www.istat.it/microdati/indagine-sulle-spese-delle-famiglie-uso-pubblico/

**2. Regional retail price data (Osservatorio Prezzi)**
Monthly average retail prices for meat products at the provincial level, manually
collected from the Italian Ministry of Enterprise and Made in Italy's Osservatorio
Prezzi e Tariffe, at: https://osservaprezzi.mise.gov.it/prezzi/livelli/beni-e-servizi-di-largo-consumo
Prices were recorded for each available province and food category, then cleaned and harmonised using the Python scripts
described below.

---

## Method

The core demand model is a **Linear Approximation Almost Ideal Demand System
(LA-AIDS)**, estimated on four meat categories: beef, pork, poultry, and processed
meat. The model is estimated with homogeneity and symmetry restrictions imposed,
using the Stone price index.

Elasticities are computed analytically from the model coefficients and standard
errors are obtained via parametric bootstrap (1000 draws from the joint distribution
of the estimated coefficients).

The carbon tax is set at **EUR 0.09 per kg CO2e** and applied proportionally to
each meat category based on its GHG emissions intensity.
Welfare effects are measured using the **compensating variation (CV)**, computed
via Hicksian elasticities derived from the Slutsky equation. Tax revenue is
assumed to be recycled as a uniform per-capita rebate.

---

## Pipeline

The analysis runs in two stages: data preparation in Python, followed by
estimation and simulation in R. The scripts must be run in the order listed below.

```
Raw ISTAT HBS data
       │
       ▼
01_hbs_extraction.py          — select relevant columns from raw microdata
       │
       ▼
02_hbs_meat_categories.py     — label meat categories and compute budget shares
       │
       ▼
Raw Osservatorio Prezzi data (manually collected)
       │
       ▼
03_price_coverage_matrix.py   — map which categories are available per province
       │
       ▼
04_price_harmonisation.py     — combine all provinces into one long-format file
       │
       ▼
05_price_meat_filter.py       — keep only the six meat price categories
       │
       ▼
meat_demand_LA_AIDS.R         — merge data, estimate LA-AIDS, compute elasticities
       │
       ▼
tax_simulation.R              — simulate carbon tax, compute new prices/quantities
       │
       ▼
demographic_classification.R  — classify households by type and life-cycle group
       │
       ▼
welfare_analysis.R            — compute CV, CS, GHG reductions, quintile results
       │
       ▼
results_tables.R              — assemble and export all results to Excel
```

---

## Scripts

### Python — Data Preparation

#### `01_hbs_extraction.py`
Loads the raw ISTAT HBS microdata file and extracts only the columns needed
for the analysis: household composition variables, member-level age and
relationship codes, monthly food expenditure items, total household expenditure,
survey weights, and regional identifiers. ISTAT encodes missing values as `"."`
— these are converted to `NaN` and expenditure columns are cast to numeric.

**Input:** `data/microdata_2023_ISTAT.xlsx`
**Output:** `micro_protein_cols.csv`

---

#### `02_hbs_meat_categories.py`
Maps the raw ISTAT expenditure codes to descriptive meat category names and
computes budget shares within total meat expenditure for each household. Missing
expenditure values are treated as zero. Households with zero total meat expenditure
receive a budget share of zero rather than a missing value.

**Input:** `micro_protein_cols.csv`
**Output:** `micro_identified_proteins.csv`

---

#### `03_price_coverage_matrix.py`
The Osservatorio Prezzi does not report the same food categories for every
province. This script reads a manually compiled reference table listing the
categories available in each province and builds a presence/absence matrix
showing coverage across all provinces. The matrix was used to decide which
categories had sufficient coverage to be included in the analysis.

**Input:** `data/categories_by_province.xlsx`
**Output:** `presence_matrix.xlsx`

---

#### `04_price_harmonisation.py`
Reads the raw Osservatorio Prezzi data, stored as one Excel sheet per province
with categories as rows and months as columns. Filters to only the categories
retained after the coverage check, then reshapes all provinces into a single
long-format dataset with one row per province-category-month combination.

**Input:** `data/prezzi_oss.xlsx`, `presence_matrix.xlsx`
**Output:** `all_categories_prices_by_province.xlsx`

---

#### `05_price_meat_filter.py`
Filters the harmonised price dataset down to the six meat categories used in
the demand model. These map to the four model goods as follows: beef and pork
each have a direct price series; poultry uses chicken breast; the processed meat
price is constructed in R as the average of prosciutto cotto, prosciutto crudo,
and pancetta (with `na.rm = TRUE` to handle Firenze where pancetta is not reported).

**Input:** `all_categories_prices_by_province.xlsx`
**Output:** `filtered_prices.xlsx`

---

### R — Estimation and Simulation

#### `meat_demand_LA_AIDS.R`
Loads and merges the prepared HBS and price data, constructs the four meat
categories and their budget shares, and estimates the LA-AIDS model with
homogeneity and symmetry imposed. Computes expenditure and price elasticities
at sample means with bootstrap standard errors, and repeats the elasticity
calculation separately for each expenditure quintile. Exports all elasticity
results to Excel.

**Key outputs:** `meat_data_final`, `aids_meat`, `all_coefs`, `expenditure_elas`,
`price_elas`, `quintile_elasticities`

---

#### `tax_simulation.R`
Simulates a carbon tax on meat based on GHG emissions intensity. Computes
ad-valorem tax rates relative to observed household prices and derives post-tax
prices for each household. Quantity responses are estimated using two methods:
(A) a first-order elasticity approximation, and (B) full structural recomputation
of budget shares from the LA-AIDS share equations at new prices.

**Key outputs:** `dt` extended with post-tax prices and quantities, `ghg`,
`Pop`, `R`, `r`

---

#### `demographic_classification.R`
Classifies each household into demographic groups using member-level age and
relationship codes from the HBS. Constructs two variables: `hh_type` (family
with children / adult-only / elderly household) and `dominant_adult_final`
(the plurality life-cycle group among adult members, with ties broken by the
reference person's age group).

**Key outputs:** `hh_type` and `dominant_adult_final` columns added to `dt`

---

#### `welfare_analysis.R`
Computes household-level welfare effects of the carbon tax: tax burden (`T_h`),
per-capita rebate (`R_h`), compensating variation (`CV_h`) via Hicksian
elasticities, consumer surplus change (`CS_h`) via trapezoid approximation,
net welfare (`net_welfare = CV_h - R_h`), and GHG emissions before and after
the tax. All measures are summarised by expenditure quintile.

**Key outputs:** `CV_h`, `T_h`, `R_h`, `net_welfare`, `ghg_0`, `ghg_1`,
`results_quintile`, `CV_per_capita`, `ghg_reduction_per_capita`

---

#### `results_tables.R`
Assembles all results into labelled tables organised into six sections
(elasticities, quantities, emissions, demographics, efficiency, welfare)
and exports them to Excel and RData.

**Key outputs:** `policy_results_tables.xlsx`, `policy_results_tables.RData`

---

## Requirements

### Python
Tested on Python 3.11. Required packages:

```
pandas
numpy
openpyxl
```

Install with:
```bash
pip install pandas numpy openpyxl
```

### R
Required packages:

| Package | Purpose |
|---------|---------|
| `data.table` | Data manipulation throughout |
| `micEconAids` | LA-AIDS model estimation |
| `readxl` | Loading input data from Excel |
| `MASS` | Parametric bootstrap |
| `Hmisc` | Weighted quintile construction |
| `writexl` | Excel export (elasticities) |
| `openxlsx` | Excel export (results tables) |

All R packages can be installed from CRAN. Each script checks for missing
packages and installs them automatically if needed.

---

## Replication

1. Place the raw data files in the `data/` folder:
   - `microdata_2023_ISTAT.xlsx` — raw HBS microdata from ISTAT
   - `prezzi_oss.xlsx` — manually collected Osservatorio Prezzi data
   - `categories_by_province.xlsx` — manually compiled category reference table

2. Run the Python scripts in order:
   ```bash
   python 01_hbs_extraction.py
   python 02_hbs_meat_categories.py
   python 03_price_coverage_matrix.py
   python 04_price_harmonisation.py
   python 05_price_meat_filter.py
   ```

3. Run the R scripts in order:
   ```
   meat_demand_LA_AIDS.R
   tax_simulation.R
   demographic_classification.R
   welfare_analysis.R
   results_tables.R
   ```

---

## Notes

- All monetary values are in euros (EUR)
- Quantities are in kilograms (kg) per month at the household level
- GHG emissions are in kg CO2 equivalent (kg CO2e)
- Survey weights (`w_anno`) are used throughout for all aggregate calculations
- The `prezzi_oss.xlsx` and `categories_by_province.xlsx` files were compiled
  manually from the Osservatorio Prezzi website and are not included in this
  repository, but the Python scripts document exactly how they were structured
