
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Codecov test coverage](https://codecov.io/gh/HanLabCollaboration/ReproduceYost/graph/badge.svg)](https://app.codecov.io/gh/HanLabCollaboration/ReproduceYost)
<!-- badges: end -->

# ReproduceYost

`ReproduceYost` is an R package for computing the **Yost Index**, a composite measure of neighborhood-level socioeconomic status (SES), using data from the US Census Bureau's American Community Survey (ACS).

## What is the Yost Index?

The Yost Index is derived via factor analysis on 7 ACS variables:

| Variable | Description |
|---|---|
| `income` | Median Household Income |
| `wkcls` | Percent working class |
| `unemp` | Percent unemployed |
| `educ` | Education score |
| `poverty150` | Percent below 150% poverty line |
| `rent` | Median Gross Rent |
| `hval` | Median Home Value |

Higher Yost scores indicate higher neighborhood SES.

## Features

- **Automatic data fetching**: wraps `tidycensus` to pull all required ACS variables
- **Flexible geographies**: county, tract, or block group
- **Flexible scopes**: rank geographies nationally, within state, or within county
- **Shrinkage stabilization**: Empirical Bayes-style smoothing for unreliable small-area estimates (recommended for block groups and tracts)
- **Spatial imputation**: neighbor-based imputation for missing values
- **Dual output**: always returns a baseline `df_yost_raw` (no shrinkage, no imputation) alongside the requested `df_yost`

## Installation

```r
# install.packages("devtools")
devtools::install_github("HanLabCollaboration/ReproduceYost")
```

You will also need a free **Census API key**:

```r
# 1. Get a key at: https://api.census.gov/data/key_signup.html
# 2. Install it:
tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)
```

## Basic Usage

```r
library(ReproduceYost)

yost_ca <- computeYostIndex(
  geo    = "tract",   # "state","county", "tract", or "block group"
  year   = 2022,      # ACS 5-year survey year (>= 2011)
  states = "CA",      # state abbreviation(s), or "all"
  scope  = "state",   # "national", "state", or "county"
  shrink = TRUE,      # shrinkage stabilization (recommended for tracts/block groups)
  impute = TRUE,      # spatial imputation for missing values
  keep_geometry = TRUE
)

# Always present: baseline Yost (no shrinkage, no imputation)
head(yost_ca$df_yost_raw)

# Requested output — column name reflects parameters used:
# shrink=F, impute=F -> Yost / YostQuintile
# shrink=T           -> YostShrunk / YostShrunkQuintile
# impute=T           -> YostImputed / YostImputedQuintile
# shrink=T, impute=T -> YostShrunkImputed / YostShrunkImputedQuintile
head(yost_ca$df_yost)
```

### Output structure (`return_format = "detailed"`)

| Element | Description |
|---|---|
| `df_yost_raw` | Baseline Yost score — always no shrinkage, no imputation |
| `df_yost` | Requested Yost score, dynamically named by parameters |
| `df_raw_values` | Raw ACS estimates and margins of error |
| `df_geometry` | Spatial geometries (if `keep_geometry = TRUE`) |
| `df_imputed` | Variable values after imputation |
| `df_rank` | Ranked/standardized variables used in factor analysis |
| `obj_factor` | Factor analysis object(s) from `psych::fa` |

For a minimal output (GEOID + score + quintile only), use `return_format = "minimal"`.

## Shrinkage Stabilization

At fine geographic levels, ACS margins of error (MOEs) can be large relative to the estimates themselves. The shrinkage method stabilizes noisy estimates by borrowing strength from the parent geography:

$$w = \frac{t^2}{s^2 + t^2}$$

where $t^2$ is within-parent heterogeneity and $s^2 = (\text{MOE}/1.645)^2$ is the sampling variance. Unreliable estimates are pulled toward the parent; reliable ones are left unchanged.

| Geography | Recommendation |
|---|---|
| County | Generally not needed |
| Tract | Consider using, especially for smaller states |
| Block group | **Strongly recommended** |

## Spatial Imputation

### The Problem: Missing ACS Data

Some geographies — particularly small census tracts and block groups — have missing values for one or more ACS variables. This can happen when the Census Bureau suppresses estimates due to small sample sizes or when a geography has zero population. Missing values in any of the 7 component variables would otherwise prevent a Yost score from being computed.

### The Solution: Neighbor-Based Imputation

When `impute = TRUE`, missing values are filled using a spatial lag of neighboring geographies (Queen contiguity). For each missing variable, the imputed value is the (optionally population-weighted) mean of all neighboring units that have a valid estimate.

```r
# Weighted imputation using population size
yost_imputed <- computeYostIndex(
  geo        = "tract",
  year       = 2022,
  states     = "CA",
  scope      = "state",
  impute     = TRUE,
  weight_var = "tot_pop"   # or "none" for unweighted
)
```

Each row in `df_imputed` includes:

- `nvar_imputed` — number of variables that were imputed
- `nvar_still_missing` — number still missing after imputation (e.g., isolated geographies with no neighbors)

The `annotation` column in `df_yost` records the imputation status of each geography:

| Annotation | Meaning |
|---|---|
| `Complete data` | No imputation needed |
| `Imputation completed` | All missing values were successfully imputed |
| `Imputation incomplete` | Some variables still missing after imputation |
| `No population` | Geography excluded (zero population) |

| Geography | Recommendation |
|---|---|
| County | Rarely needed |
| Tract | Recommended |
| Block group | **Strongly recommended** |

## References

- Yost, K., et al. (2001). Socioeconomic status and breast cancer incidence in California for different race/ethnic groups. *Cancer Causes & Control*, 12(8), 703–711.
- Yang, J., et al. (2014). Developing an area-based socioeconomic measure from American Community Survey data. *Cancer Prevention Research*, 7(7).

## License

MIT
