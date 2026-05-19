library(testthat)
library(ReproducibleYostIndex)

# =============================================================================
# test-getYostIndex.R
#
# Unlike computeYostIndex tests, these do NOT use vcr cassettes.
# getYostIndex downloads static, permanent release assets from a public GitHub
# repo — no API key, no dynamic responses. Integration tests use
# skip_if_offline() and hit the real URL.
# =============================================================================

# --- Validation tests (no network required) ----------------------------------

test_that("getYostIndex rejects invalid geo", {
  expect_error(
    getYostIndex(geo = "zipcode", year = 2022),
    "must be one of"
  )
})

test_that("getYostIndex rejects invalid scope", {
  expect_error(
    getYostIndex(geo = "tract", year = 2022, scope = "county"),
    "must be one of"
  )
})

test_that("getYostIndex rejects year too early for county/tract", {
  expect_error(
    getYostIndex(geo = "county", year = 2010),
    "2011 onwards"
  )
})

test_that("getYostIndex rejects year too early for block group", {
  expect_error(
    getYostIndex(geo = "block group", year = 2012),
    "2013 onwards"
  )
})

test_that("getYostIndex rejects year beyond available data", {
  expect_error(
    getYostIndex(geo = "tract", year = 2099),
    "available through year"
  )
})

test_that("getYostIndex rejects invalid state abbreviations", {
  expect_error(
    getYostIndex(geo = "county", year = 2022, states = c("CA", "ZZ")),
    "Invalid state abbreviation"
  )
})

test_that("getYostIndex accepts 'cbg' as alias for 'block group'", {
  # Should fail on year range, not on geo validation — confirming cbg is accepted
  expect_error(
    getYostIndex(geo = "cbg", year = 2012),
    "2013 onwards"
  )
})

# --- Integration tests (requires internet) -----------------------------------

test_that("getYostIndex returns correct structure for county/national", {
  skip_if_offline()

  df <- getYostIndex(geo = "county", year = 2022, scope = "national", quiet = TRUE)

  expect_s3_class(df, "data.frame")
  expect_named(df, c(
    "GEOID", "NAME", "year", "geo", "scope",
    "Yost", "YostQuintile",
    "YostStabilized", "YostStabilizedQuintile",
    "YostImputed", "YostImputedQuintile",
    "YostStabilizedImputed", "YostStabilizedImputedQuintile"
  ))
  expect_equal(nrow(df), 3144)  # all US counties
  expect_true(all(df$geo == "county"))
  expect_true(all(df$scope == "national"))
  expect_true(all(df$year == 2022))
  expect_s3_class(df$YostQuintile, "ordered")
})

test_that("getYostIndex returns rows sorted by GEOID", {
  skip_if_offline()

  df <- getYostIndex(geo = "county", year = 2022, quiet = TRUE)
  expect_equal(df$GEOID, sort(df$GEOID))
})

test_that("getYostIndex state filter returns only matching GEOIDs", {
  skip_if_offline()

  df <- getYostIndex(geo = "county", year = 2022, states = c("CA", "NY"), quiet = TRUE)

  state_prefixes <- substr(df$GEOID, 1, 2)
  expect_true(all(state_prefixes %in% c("06", "36")))  # CA = 06, NY = 36
  expect_gt(nrow(df), 0)
})

test_that("getYostIndex returns quintile columns as ordered factors with levels 1:5", {
  skip_if_offline()

  df <- getYostIndex(geo = "county", year = 2022, quiet = TRUE)

  quintile_cols <- c("YostQuintile", "YostStabilizedQuintile",
                     "YostImputedQuintile", "YostStabilizedImputedQuintile")
  for (col in quintile_cols) {
    expect_s3_class(df[[col]], "ordered")
    expect_equal(levels(df[[col]]), as.character(1:5))
  }
})

test_that("getYostIndex Yost and quintile columns may contain NA", {
  skip_if_offline()

  df <- getYostIndex(geo = "county", year = 2022, quiet = TRUE)

  yost_cols <- c(
    "Yost", "YostQuintile",
    "YostStabilized", "YostStabilizedQuintile",
    "YostImputed", "YostImputedQuintile",
    "YostStabilizedImputed", "YostStabilizedImputedQuintile"
  )
  for (col in yost_cols) {
    expect_true(anyNA(df[[col]]), label = glue::glue("{col} can have NA"))
  }
})

test_that("getYostIndex uses cache on second call", {
  skip_if_offline()

  # First call — downloads
  getYostIndex(geo = "county", year = 2022, quiet = TRUE)

  # Second call — should use cache (message says "Using cached")
  expect_message(
    getYostIndex(geo = "county", year = 2022, cache = TRUE),
    "Using cached"
  )
})

test_that("getYostIndex cache = FALSE re-downloads", {
  skip_if_offline()

  expect_message(
    getYostIndex(geo = "county", year = 2022, cache = FALSE),
    "Downloading"
  )
})

test_that("getYostIndex works for tract/state scope", {
  skip_if_offline()

  df <- getYostIndex(geo = "tract", year = 2022, scope = "state",
                states = "RI", quiet = TRUE)

  expect_s3_class(df, "data.frame")
  expect_true(all(substr(df$GEOID, 1, 2) == "44"))  # RI FIPS = 44
  expect_true(all(df$scope == "state"))
  expect_gt(nrow(df), 0)
})

test_that("getYostIndex works for block group with cbg alias", {
  skip_if_offline()

  df <- getYostIndex(geo = "cbg", year = 2022, states = "RI", quiet = TRUE)

  expect_s3_class(df, "data.frame")
  expect_true(all(df$geo == "block group"))
  expect_gt(nrow(df), 0)
})
