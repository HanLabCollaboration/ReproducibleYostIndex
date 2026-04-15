library(testthat)
library(ReproduceYostIndex)

# =============================================================================
# test-getYost.R
#
# Unlike computeYostIndex tests, these do NOT use vcr cassettes.
# getYost downloads static, permanent release assets from a public GitHub
# repo — no API key, no dynamic responses. Integration tests use
# skip_if_offline() and hit the real URL.
# =============================================================================

# --- Validation tests (no network required) ----------------------------------

test_that("getYost rejects invalid geo", {
  expect_error(
    getYost(geo = "zipcode", year = 2022),
    "must be one of"
  )
})

test_that("getYost rejects invalid scope", {
  expect_error(
    getYost(geo = "tract", year = 2022, scope = "county"),
    "must be one of"
  )
})

test_that("getYost rejects year too early for county/tract", {
  expect_error(
    getYost(geo = "county", year = 2010),
    "starts at year 2011"
  )
})

test_that("getYost rejects year too early for block group", {
  expect_error(
    getYost(geo = "block group", year = 2012),
    "starts at year 2013"
  )
})

test_that("getYost rejects year beyond available data", {
  expect_error(
    getYost(geo = "tract", year = 2099),
    "available through year"
  )
})

test_that("getYost rejects invalid state abbreviations", {
  expect_error(
    getYost(geo = "county", year = 2022, states = c("CA", "ZZ")),
    "Invalid state abbreviation"
  )
})

test_that("getYost accepts 'cbg' as alias for 'block group'", {
  # Should fail on year range, not on geo validation — confirming cbg is accepted
  expect_error(
    getYost(geo = "cbg", year = 2012),
    "starts at year 2013"
  )
})

# --- Integration tests (requires internet) -----------------------------------

test_that("getYost returns correct structure for county/national", {
  skip_if_offline()

  df <- getYost(geo = "county", year = 2022, scope = "national", quiet = TRUE)

  expect_s3_class(df, "data.frame")
  expect_named(df, c(
    "GEOID", "year", "geo", "scope",
    "Yost", "YostQuintile",
    "YostStabilized", "YostStabilizedQuintile",
    "YostImputed", "YostImputedQuintile",
    "YostStabilizedImputed", "YostStabilizedImputedQuintile"
  ))
  expect_equal(nrow(df), 3143)  # all US counties
  expect_true(all(df$geo == "county"))
  expect_true(all(df$scope == "national"))
  expect_true(all(df$year == 2022))
})

test_that("getYost returns rows sorted by GEOID", {
  skip_if_offline()

  df <- getYost(geo = "county", year = 2022, quiet = TRUE)
  expect_equal(df$GEOID, sort(df$GEOID))
})

test_that("getYost state filter returns only matching GEOIDs", {
  skip_if_offline()

  df <- getYost(geo = "county", year = 2022, states = c("CA", "NY"), quiet = TRUE)

  state_prefixes <- substr(df$GEOID, 1, 2)
  expect_true(all(state_prefixes %in% c("06", "36")))  # CA = 06, NY = 36
  expect_gt(nrow(df), 0)
})

test_that("getYost returns all quintile values in 1:5 range", {
  skip_if_offline()

  df <- getYost(geo = "county", year = 2022, quiet = TRUE)

  check_quintile <- function(x) all(x[!is.na(x)] %in% 1:5)
  expect_true(check_quintile(df$YostQuintile))
  expect_true(check_quintile(df$YostStabilizedQuintile))
  expect_true(check_quintile(df$YostImputedQuintile))
  expect_true(check_quintile(df$YostStabilizedImputedQuintile))
})

test_that("getYost uses cache on second call", {
  skip_if_offline()

  # First call — downloads
  getYost(geo = "county", year = 2022, quiet = TRUE)

  # Second call — should use cache (message says "Using cached")
  expect_message(
    getYost(geo = "county", year = 2022, cache = TRUE),
    "Using cached"
  )
})

test_that("getYost cache = FALSE re-downloads", {
  skip_if_offline()

  expect_message(
    getYost(geo = "county", year = 2022, cache = FALSE),
    "Downloading"
  )
})

test_that("getYost works for tract/state scope", {
  skip_if_offline()

  df <- getYost(geo = "tract", year = 2022, scope = "state",
                states = "RI", quiet = TRUE)

  expect_s3_class(df, "data.frame")
  expect_true(all(substr(df$GEOID, 1, 2) == "44"))  # RI FIPS = 44
  expect_true(all(df$scope == "state"))
  expect_gt(nrow(df), 0)
})

test_that("getYost works for block group with cbg alias", {
  skip_if_offline()

  df <- getYost(geo = "cbg", year = 2022, states = "RI", quiet = TRUE)

  expect_s3_class(df, "data.frame")
  expect_true(all(df$geo == "block group"))
  expect_gt(nrow(df), 0)
})
