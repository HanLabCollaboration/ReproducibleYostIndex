
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Codecov test coverage](https://codecov.io/gh/HanLabCollaboration/ReproduceYostIndex/graph/badge.svg)](https://app.codecov.io/gh/HanLabCollaboration/ReproduceYostIndex)
<!-- badges: end -->

# ReproduceYostIndex

`ReproduceYostIndex` is an R package for computing the **Yost Index**, a composite measure of neighborhood-level socioeconomic status (SES), using data from the US Census Bureau's American Community Survey (ACS).

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

---

## Two Ways to Get Yost Index Data

| | `getYostIndex()` | `computeYostIndex()` |
|---|---|---|
| **Census API key** | Not required | Required |
| **Speed** | Instant (pre-computed) | Minutes to hours |
| **Years available** | 2013–2023 | 2011–present |
| **Geographies** | County, tract, block group | County, tract, block group, state |
| **Scope** | National, state | National, state, county |
| **Variants returned** | All four at once | One (your chosen parameters) |
| **Best for** | Most analyses, quick exploration | Custom pipelines, newest ACS years |

---

## `getYostIndex()` — Recommended Starting Point

Downloads pre-computed Yost Index values directly from GitHub. No Census API key needed, no wait time. All four Yost variants are returned in a single call.

### What is pre-computed

| Dimension | Coverage |
|---|---|
| **Geographies** | County, tract, census block group |
| **Years** | 2013–2023 (county and tract from 2011) |
| **Scopes** | National, state |
| **Variants** | `Yost`, `YostStabilized`, `YostImputed`, `YostStabilizedImputed` (+ quintiles for each) |

### Installation

```r
# install.packages("devtools")
devtools::install_github("HanLabCollaboration/ReproduceYostIndex")
```

### Usage

```r
library(ReproduceYostIndex)

# All US counties, 2022 — no Census API key needed
county_yost <- getYostIndex(geo = "county", year = 2022)
head(county_yost)

# Tracts for California and New York, state scope
tracts <- getYostIndex(
  geo    = "tract",
  year   = 2022,
  states = c("CA", "NY"),
  scope  = "state"
)

# Block groups, national scope
bg <- getYostIndex(geo = "block group", year = 2021)
```

### Output columns

Each call returns a tibble with:

| Column | Description |
|---|---|
| `GEOID` | Census geography identifier |
| `Yost` / `YostQuintile` | Baseline — no stabilization, no imputation |
| `YostStabilized` / `YostStabilizedQuintile` | Empirical Bayes stabilization applied |
| `YostImputed` / `YostImputedQuintile` | Spatial imputation for missing values |
| `YostStabilizedImputed` / `YostStabilizedImputedQuintile` | Both stabilization and imputation |

### Caching

Files are downloaded once and cached locally. Subsequent calls with the same arguments are instantaneous.

```r
# Force a fresh download
getYostIndex(geo = "county", year = 2022, cache = FALSE)

# See where the cache lives
tools::R_user_dir("ReproduceYostIndex", "cache")
```

---

## `computeYostIndex()` — Full Pipeline

Use `computeYostIndex()` when you need:

- A **year not yet pre-computed** (e.g., 2024 ACS once released)
- **County-level scope** — quintiles computed within each county
- **Intermediate outputs** — raw ACS values, factor loadings, imputed data, the 7 component variables
- A **custom pipeline** — change rescaling method (`rank` vs `standardize`), imputation weights, or number of factors

### Census API key (required)

```r
# Get a free key at: https://api.census.gov/data/key_signup.html
tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)
```

### Usage

```r
library(ReproduceYostIndex)

yost_ca <- computeYostIndex(
  geo       = "tract",
  year      = 2022,
  states    = "CA",
  scope     = "state",    # "national", "state", or "county"
  stabilize = TRUE,       # empirical Bayes stabilization (recommended for tracts/block groups)
  impute    = TRUE,       # spatial imputation for missing values
  keep_geometry = TRUE
)

# Baseline Yost — always present regardless of parameters
head(yost_ca$df_yost_raw)   # Yost, YostQuintile

# Requested variant — column name reflects parameters used:
# stabilize=FALSE, impute=FALSE  →  Yost / YostQuintile
# stabilize=TRUE                 →  YostStabilized / YostStabilizedQuintile
# impute=TRUE                    →  YostImputed / YostImputedQuintile
# stabilize=TRUE, impute=TRUE    →  YostStabilizedImputed / YostStabilizedImputedQuintile
head(yost_ca$df_yost)
```

### Output structure (`return_format = "detailed"`)

| Element | Description |
|---|---|
| `df_yost_raw` | Baseline Yost — always no stabilization, no imputation |
| `df_yost` | Requested Yost, column name reflects parameters used |
| `df_raw_values` | Raw ACS estimates and margins of error |
| `df_geometry` | Spatial geometries (if `keep_geometry = TRUE`) |
| `df_imputed` | Component variables after imputation |
| `df_rank` | Ranked/standardized variables used in factor analysis |
| `obj_factor` | `psych::fa` object(s) |

Use `return_format = "minimal"` for a compact output with only GEOID, score, and quintile.

### Stabilization

At fine geographic levels, ACS margins of error can be large relative to the estimates themselves. The empirical Bayes stabilization method pulls noisy lower-level estimates toward their parent geography using a data-adaptive weight:

$$w = \frac{t^2}{s^2 + t^2}$$

where $t^2$ is within-parent heterogeneity and $s^2 = (\text{MOE}/1.645)^2$ is the sampling variance. Unreliable estimates are pulled toward the parent; reliable ones are left unchanged.

| Geography | Recommendation |
|---|---|
| County | Generally not needed |
| Tract | Consider using, especially for smaller states |
| Block group | **Strongly recommended** |

### Spatial Imputation

When `impute = TRUE`, missing values are filled using the population-weighted spatial mean of neighboring geographies (Queen contiguity). Each row in `df_imputed` includes `nvar_imputed` and `nvar_still_missing`. The `annotation` column in `df_yost` records the imputation status:

| Annotation | Meaning |
|---|---|
| `Complete data` | No imputation needed |
| `Imputation completed` | All missing values successfully imputed |
| `Imputation incomplete` | Some variables still missing after imputation |
| `No population` | Geography excluded (zero population) |

| Geography | Recommendation |
|---|---|
| County | Rarely needed |
| Tract | Recommended |
| Block group | **Strongly recommended** |

---

## References

- Yost, K., et al. (2001). Socioeconomic status and breast cancer incidence in California for different race/ethnic groups. *Cancer Causes & Control*, 12(8), 703–711.
- Yang, J., et al. (2014). Developing an area-based socioeconomic measure from American Community Survey data. *Cancer Prevention Research*, 7(7).

## License

MIT
