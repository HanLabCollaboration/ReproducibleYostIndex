library(testthat)
library(ReproduceYost)
library(vcr)

# Note: fetchYostAcs is an internal function, so we use ::: to access it

test_that("fetchYostAcs returns data with correct structure (county level)", {

  vcr::use_cassette("fetch-yost-acs-county", {
    result <- ReproduceYost:::fetchYostAcs(
      geo = "county",
      year = 2022,
      states = "CA",
      get_geometry = FALSE
    )
  })

  # Check basic structure
  expect_s3_class(result, "data.frame")
  expect_true("GEOID" %in% colnames(result))
  expect_true("NAME" %in% colnames(result))

  # Check that we have ACS estimate columns (ending in 'E')
  e_cols <- colnames(result)[grepl("E$", colnames(result))]
  expect_true(length(e_cols) > 0)

  # Check attribute with variable names
  acs_vars <- attr(result, "acs_vars")
  expect_type(acs_vars, "list")

  # Verify all required variable groups exist
  required_vars <- c("var_POPTOT", "var_MHHINC", "var_WKCLS", "var_WKCLS_pop",
                     "var_UNEMP", "var_UNEMP_pop", "var_Edu_denom", "var_Edu_P1",
                     "var_Edu_P2", "var_Edu_P3", "var_RATIOP", "var_RATIOP_pop",
                     "var_MGRRNT", "var_MVALUE")

  expect_true(all(required_vars %in% names(acs_vars)))
})

test_that("fetchYostAcs returns data with geometry when requested", {

  vcr::use_cassette("fetch-yost-acs-county-geom", {
    result <- ReproduceYost:::fetchYostAcs(
      geo = "county",
      year = 2022,
      states = "CA",
      get_geometry = TRUE
    )
  })

  # Check that geometry column exists
  expect_true("geometry" %in% colnames(result))
  expect_s3_class(result, "sf")
})

test_that("fetchYostAcs works for tract geography", {

  vcr::use_cassette("fetch-yost-acs-tract", {
    result <- ReproduceYost:::fetchYostAcs(
      geo = "tract",
      year = 2022,
      states = "RI",  # Use a small state for faster testing
      get_geometry = FALSE
    )
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("GEOID" %in% colnames(result))
})

test_that("fetchYostAcs works for state geography", {

  vcr::use_cassette("fetch-yost-acs-state", {
    result <- ReproduceYost:::fetchYostAcs(
      geo = "state",
      year = 2022,
      states = c("CA", "NY"),  # Multiple states
      get_geometry = FALSE
    )
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # For state geography, all states should be included regardless of the states parameter
  # because tidycensus doesn't accept state filter for state geography
})

test_that("fetchYostAcs handles multiple states", {

  vcr::use_cassette("fetch-yost-acs-multi-state", {
    result <- ReproduceYost:::fetchYostAcs(
      geo = "county",
      year = 2022,
      states = c("RI", "VT"),  # Two small states
      get_geometry = FALSE
    )
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # We should have counties from both states
  # (Can't directly verify without parsing NAME, but check row count is reasonable)
  expect_true(nrow(result) > 10)
})

test_that("fetchYostAcs attribute contains all required variable names", {

  vcr::use_cassette("fetch-yost-acs-vars-check", {
    result <- ReproduceYost:::fetchYostAcs(
      geo = "county",
      year = 2022,
      states = "CA",
      get_geometry = FALSE
    )
  })

  acs_vars <- attr(result, "acs_vars")

  # Check that all variable groups are character vectors or lists
  expect_type(acs_vars$var_POPTOT, "character")
  expect_type(acs_vars$var_MHHINC, "character")
  expect_type(acs_vars$var_WKCLS, "character")
  expect_type(acs_vars$var_UNEMP, "character")

  # Check that estimate suffix 'E' is applied
  expect_true(grepl("E$", acs_vars$var_POPTOT))
  expect_true(all(grepl("E$", acs_vars$var_WKCLS)))

  # Check that variable vectors have expected lengths
  expect_length(acs_vars$var_WKCLS, 14)  # There are 14 work class variables
  expect_length(acs_vars$var_Edu_P1, 16) # Education Part 1: 00(3:9) + 010 + 0(20:27) = 7+1+8 = 16
  expect_length(acs_vars$var_Edu_P2, 2)  # Education Part 2
  expect_length(acs_vars$var_Edu_P3, 14) # Education Part 3: 0(12:18) + 0(29:35) = 7+7 = 14
  expect_length(acs_vars$var_RATIOP, 4)  # Poverty ratio
})

test_that("fetchYostAcs returns both estimate (E) and MOE (M) columns", {

  vcr::use_cassette("fetch-yost-acs-estimates-only", {
    result <- ReproduceYost:::fetchYostAcs(
      geo = "county",
      year = 2022,
      states = "CA",
      get_geometry = FALSE
    )
  })

  acs_cols <- setdiff(colnames(result), c("GEOID", "NAME", "geometry"))

  # Should have estimate columns (E suffix)
  e_cols <- acs_cols[grepl("E$", acs_cols)]
  expect_true(length(e_cols) > 0)

  # Should also have MOE columns (M suffix) — needed for shrinkage
  m_cols <- acs_cols[grepl("M$", acs_cols)]
  expect_true(length(m_cols) > 0)

  # Each estimate variable should have a matching MOE variable
  expect_equal(length(e_cols), length(m_cols))
})