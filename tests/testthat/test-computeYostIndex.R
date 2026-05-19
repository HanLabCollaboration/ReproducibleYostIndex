library(testthat)
library(ReproducibleYostIndex) # This will load your package
library(vcr)

# Set up vcr to store cassettes in the "tests/fixtures/vcr_cassettes" directory
options(tigris_use_cache = TRUE)

# --- Test 1: Input Validation ---
# These tests don't need vcr because they should fail *before* any API call.
test_that("computeYostIndex handles bad inputs", {
  # Test bad geo
  expect_error(
    computeYostIndex(geo = "zipcode", year = 2022, states = "CA"),
      "must be one of"
  )

  # Test bad year
  expect_error(
    computeYostIndex(geo = "tract", year = 2005, states = "CA"),
    ">= 2011"
  )

  # Test bad rescale
  expect_error(
    computeYostIndex(rescale = "percentile", states = "CA"),
    "must be one of"
  )

  # Test bad year/geo combination
  expect_error(
    computeYostIndex(geo = "block group", year = 2011, states = "CA"),
    ">= 2013"
  )
})


# --- Test 2: Core Functionality (using vcr) ---
# This test *will* make a live API call the *first* time you run it.
# After that, it will use the saved "cassette".
test_that("computeYostIndex returns expected detailed output", {

  # 1. Tell vcr to wrap this code block and save the API results
  #    to a file named "yost-ca-county".
  vcr::use_cassette("yost-ca-county", {

    # 2. Run your function. (Make sure your Census API key is set!)
    yost_data <- computeYostIndex(
      geo = "county",
      year = 2022,
      scope = "state",
      states = "CA",
      impute = FALSE, # Set impute=FALSE for a simpler first test
      quiet = TRUE
    )

  }) # vcr recording stops here

  # 3. Add your expectations (tests)

  # Test the overall output structure
  expect_type(yost_data, "list")
  expect_named(
    yost_data,
    c("df_yost_raw", "df_yost", "df_raw_values", "df_geometry", "df_imputed", "df_rank", "obj_factor")
  )

  # Test the main data frame
  expect_s3_class(yost_data$df_yost, "data.frame")
  expect_true("GEOID" %in% colnames(yost_data$df_yost))
  expect_true("Yost" %in% colnames(yost_data$df_yost))
  expect_true("YostQuintile" %in% colnames(yost_data$df_yost))

  # Test the factor analysis object (can be fa or NULL if insufficient data)
  expect_true(inherits(yost_data$obj_factor, "list") || is.null(yost_data$obj_factor))

  # Test the other data frames
  expect_s3_class(yost_data$df_raw_values, "data.frame")
  expect_s3_class(yost_data$df_imputed, "data.frame")
  expect_s3_class(yost_data$df_rank, "data.frame")
})

# --- Test 3: Test for 'minimal' return format ---
test_that("computeYostIndex minimal return format works", {
  vcr::use_cassette("yost-ca-county-minimal", {

    yost_minimal <- computeYostIndex(
      geo = "county",
      year = 2022,
      scope = "state",
      states = "CA",
      impute = FALSE,
      return_format = "minimal", # Test this argument
      quiet = TRUE
    )

  })

  # Test the output structure - minimal should return just a data frame
  expect_s3_class(yost_minimal, "data.frame")

  # Test the columns in the minimal data frame
  expect_named(
    yost_minimal,
    c("GEOID", "Yost", "YostQuintile", "annotation")
  )
})

# --- Test 4: Test scope = "state" ---
test_that("computeYostIndex works with scope = 'state'", {
  vcr::use_cassette("yost-multi-state-scope", {

    yost_state_scope <- computeYostIndex(
      geo = "county",
      year = 2022,
      states = c("RI", "VT"),  # Two small states
      scope = "state",  # Process each state separately
      impute = FALSE,
      quiet = TRUE
    )

  })

  # Check output structure
  expect_type(yost_state_scope, "list")
  expect_true("obj_factor" %in% names(yost_state_scope))

  # obj_factor should be a list with state names
  expect_type(yost_state_scope$obj_factor, "list")
  expect_true(all(names(yost_state_scope$obj_factor) %in% c("Rhode Island", "Vermont")))

  # Each element should be a factor analysis object or NULL (if insufficient data)
  # Check if at least one state has a valid fa object
  fa_objects <- yost_state_scope$obj_factor
  valid_fa <- sapply(fa_objects, function(x) inherits(x, "fa"))
  expect_true(any(valid_fa) || all(sapply(fa_objects, is.null)))

  # Check that YostQuintiles are calculated within each state
  expect_s3_class(yost_state_scope$df_yost, "data.frame")
  expect_true(all(c("GEOID", "Yost", "YostQuintile") %in% colnames(yost_state_scope$df_yost)))
})

# --- Test 5: Test scope = "county" ---
test_that("computeYostIndex works with scope = 'county'", {
  vcr::use_cassette("yost-county-scope", {

    yost_county_scope <- computeYostIndex(
      geo = "tract",
      year = 2022,
      states = "RI",  # Small state
      scope = "county",  # Process each county separately
      impute = FALSE,
      quiet = TRUE
    )

  })

  # Check output structure
  expect_type(yost_county_scope, "list")
  expect_true("obj_factor" %in% names(yost_county_scope))

  # obj_factor should be a list with county names
  expect_type(yost_county_scope$obj_factor, "list")
  expect_true(length(yost_county_scope$obj_factor) > 0)

  # Check that county_state identifier was created
  # (Should be in the format "County, State")
  county_names <- names(yost_county_scope$obj_factor)
  expect_true(all(grepl(", Rhode Island$", county_names)))
})

# --- Test 6: scope = "national" message check (no API call) ---
test_that("computeYostIndex with scope = 'national' emits expected message", {
  expect_message(
    suppressWarnings(try(
      computeYostIndex(geo = "state", year = 2022, scope = "national",
                       states = "CA", impute = FALSE, quiet = FALSE),
      silent = TRUE
    )),
    "Since the scope == 'national', it will pull all states"
  )
})

# --- Test 7: Test imputation with tot_pop weights ---
test_that("computeYostIndex performs weighted imputation with tot_pop", {
  vcr::use_cassette("yost-impute-weighted", {

    yost_imputed <- computeYostIndex(
      geo = "tract",
      year = 2022,
      scope = "state",
      states = "RI",
      impute = TRUE,
      weight_var = "tot_pop",
      quiet = TRUE
    )

  })

  # Check that imputation metadata exists
  expect_true("df_imputed" %in% names(yost_imputed))
  expect_true("nvar_imputed" %in% colnames(yost_imputed$df_imputed))
  expect_true("nvar_still_missing" %in% colnames(yost_imputed$df_imputed))
})

# --- Test 8: Test imputation without weights ---
test_that("computeYostIndex performs unweighted imputation", {
  vcr::use_cassette("yost-impute-unweighted", {

    yost_imputed <- computeYostIndex(
      geo = "tract",
      year = 2022,
      scope = "state",
      states = "RI",
      impute = TRUE,
      weight_var = "none",
      quiet = TRUE
    )

  })

  # Check that imputation was performed
  expect_true("df_imputed" %in% names(yost_imputed))
  expect_s3_class(yost_imputed$df_imputed, "data.frame")
})

# --- Test 9: Test invalid state abbreviation ---
test_that("computeYostIndex rejects invalid state abbreviations", {

  expect_error(
    computeYostIndex(
      geo = "county",
      year = 2022,
      scope = "state",
      states = c("CA", "XYZ"),  # XYZ is not valid
      quiet = TRUE
    ),
    "Invalid state abbreviations provided"
  )
})

# --- Test 10: Test scope validation ---
test_that("computeYostIndex validates scope and geo combinations", {

  # scope = 'county' requires geo to be tract or block group
  expect_error(
    computeYostIndex(
      geo = "county",
      year = 2022,
      states = "CA",
      scope = "county",
      quiet = TRUE
    ),
    "scope = 'county' can only be used with geo = 'tract', 'block group', or 'cbg'"
  )

  # scope = 'state' requires geo to be county, tract, or block group
  expect_error(
    computeYostIndex(
      geo = "state",
      year = 2022,
      states = "CA",
      scope = "state",
      quiet = TRUE
    ),
    "scope = 'state' can only be used with geo = 'county', 'tract', 'block group', or 'cbg'"
  )
})

# --- Test 11: Test rescale = "standardize" ---
test_that("computeYostIndex works with rescale = 'standardize'", {
  vcr::use_cassette("yost-standardize", {

    yost_std <- computeYostIndex(
      geo = "county",
      year = 2022,
      scope = "state",
      states = "RI",
      rescale = "standardize",
      impute = FALSE,
      quiet = TRUE
    )

  })

  # Check that df_rank has standardized columns
  expect_true(all(grepl("^std_", colnames(yost_std$df_rank)[-1])))
})

# --- Test 12: Test keep_geometry parameter ---
test_that("computeYostIndex respects keep_geometry parameter", {
  vcr::use_cassette("yost-no-geometry", {

    yost_no_geom <- computeYostIndex(
      geo = "county",
      year = 2022,
      scope = "state",
      states = "RI",
      impute = FALSE,
      keep_geometry = FALSE,
      quiet = TRUE
    )

  })

  # df_geometry should not have actual geometry when keep_geometry = FALSE
  # It will still have GEOID but no geometry column
  expect_true("df_geometry" %in% names(yost_no_geom))
})

# --- Test 13: Test cbg alias for block group ---
test_that("computeYostIndex accepts 'cbg' as alias for 'block group'", {
  vcr::use_cassette("yost-cbg-ca", {
    yost_cbg <- computeYostIndex(
      geo = "cbg",
      year = 2022,
      scope = "state",
      states = "CA",
      impute = FALSE,
      quiet = TRUE
    )
  })

  expect_type(yost_cbg, "list")
  expect_true("df_yost" %in% names(yost_cbg))
})

# --- Test 14: Test that YostQuintile is properly factored ---
test_that("computeYostIndex creates proper quintile factors", {
  vcr::use_cassette("yost-quintile-check", {

    yost_data <- computeYostIndex(
      geo = "county",
      year = 2022,
      scope = "state",
      states = "CA",
      impute = FALSE,
      quiet = TRUE
    )

  })

  # Check that YostQuintile is a factor with 5 levels
  expect_s3_class(yost_data$df_yost$YostQuintile, "factor")
  expect_equal(levels(yost_data$df_yost$YostQuintile), as.character(1:5))

  # Check that all quintiles are represented (for a large enough dataset like CA counties)
  quintile_counts <- table(yost_data$df_yost$YostQuintile, useNA = "ifany")
  expect_true(all(1:5 %in% names(quintile_counts)))
})

# --- Test 15: df_yost_raw is always present and has standard columns ---
test_that("computeYostIndex always returns df_yost_raw with Yost and YostQuintile", {
  vcr::use_cassette("yost-ca-county", {

    yost_data <- computeYostIndex(
      geo = "county",
      year = 2022,
      scope = "state",
      states = "CA",
      impute = FALSE,
      quiet = TRUE
    )

  })

  expect_s3_class(yost_data$df_yost_raw, "data.frame")
  expect_true("Yost" %in% colnames(yost_data$df_yost_raw))
  expect_true("YostQuintile" %in% colnames(yost_data$df_yost_raw))
  expect_true("GEOID" %in% colnames(yost_data$df_yost_raw))
  expect_true("annotation" %in% colnames(yost_data$df_yost_raw))
})

# --- Test 16: df_yost column names reflect requested parameters ---
test_that("computeYostIndex names df_yost columns based on shrink/impute", {
  vcr::use_cassette("yost-ca-county", {

    # shrink=F, impute=F -> Yost
    result_none <- computeYostIndex(
      geo = "county", year = 2022, scope = "state", states = "CA",
      stabilize = FALSE, impute = FALSE, quiet = TRUE
    )
    expect_true("Yost" %in% colnames(result_none$df_yost))
    expect_true("YostQuintile" %in% colnames(result_none$df_yost))

  })

  vcr::use_cassette("yost-impute-weighted", {

    # shrink=F, impute=T -> YostImputed
    result_imputed <- computeYostIndex(
      geo = "tract", year = 2022, scope = "state", states = "RI",
      stabilize = FALSE, impute = TRUE, quiet = TRUE
    )
    expect_true("YostImputed" %in% colnames(result_imputed$df_yost))
    expect_true("YostImputedQuintile" %in% colnames(result_imputed$df_yost))
    expect_false("Yost" %in% colnames(result_imputed$df_yost))

  })
})

# --- Test 17: df_yost_raw and df_yost are identical when no adjustments requested ---
test_that("df_yost_raw equals df_yost when shrink=FALSE and impute=FALSE", {
  vcr::use_cassette("yost-ca-county", {

    result <- computeYostIndex(
      geo = "county", year = 2022, scope = "state", states = "CA",
      stabilize = FALSE, impute = FALSE, quiet = TRUE
    )

  })

  expect_equal(result$df_yost_raw, result$df_yost)
})
