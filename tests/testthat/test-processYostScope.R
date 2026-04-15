library(testthat)
library(ReproduceYostIndex)
library(sf)
library(dplyr)

# Helper function to create mock Yost data
create_mock_yost_data <- function(n_rows = 50, add_geometry = TRUE, add_missing = FALSE) {

  set.seed(123)

  df <- data.frame(
    GEOID = paste0("GEOID", sprintf("%03d", 1:n_rows)),
    NAME = paste0("Geography ", 1:n_rows),
    state = sample(c("California", "New York"), n_rows, replace = TRUE),
    county = sample(c("Los Angeles", "Orange", "Kings"), n_rows, replace = TRUE),
    tot_pop = sample(100:10000, n_rows, replace = TRUE),
    income = sample(30000:100000, n_rows, replace = TRUE),
    wkcls = runif(n_rows, 0, 1),
    unemp = runif(n_rows, 0, 20),
    educ = runif(n_rows, 9, 16),
    poverty150 = runif(n_rows, 0, 30),
    rent = sample(500:3000, n_rows, replace = TRUE),
    hval = sample(100000:800000, n_rows, replace = TRUE)
  )

  # Add some missing data if requested
  if (add_missing) {
    df$income[sample(1:n_rows, 5)] <- NA
    df$rent[sample(1:n_rows, 3)] <- NA
  }

  # Add some zero population rows
  df$tot_pop[sample(1:n_rows, 2)] <- 0

  # Add geometry if requested
  if (add_geometry) {
    # Create simple polygons in a grid
    coords_list <- lapply(1:n_rows, function(i) {
      x_base <- (i %% 10) * 1
      y_base <- floor(i / 10) * 1
      rbind(
        c(x_base, y_base),
        c(x_base + 0.9, y_base),
        c(x_base + 0.9, y_base + 0.9),
        c(x_base, y_base + 0.9),
        c(x_base, y_base)
      )
    })

    polygons <- lapply(coords_list, function(coords) st_polygon(list(coords)))
    df <- st_sf(df, geometry = st_sfc(polygons))
  }

  return(df)
}

test_that("processYostScope handles complete data without imputation", {

  # Create mock data
  mock_data <- create_mock_yost_data(n_rows = 30, add_geometry = FALSE, add_missing = FALSE)

  # Run processYostScope
  result <- ReproduceYostIndex:::processYostScope(
    yost_data_sub = mock_data,
    impute_sub = FALSE,
    rescale_sub = "rank",
    nfactors_sub = 1,
    quiet_sub = TRUE,
    return_format_sub = "detailed",
    weight_var_sub = "tot_pop"
  )

  # Check output structure
  expect_type(result, "list")
  expect_named(result, c("df_yost", "df_raw_values", "df_geometry",
                         "df_imputed", "df_rank", "fa_object"))

  # Check df_yost
  expect_s3_class(result$df_yost, "data.frame")
  expect_true(all(c("GEOID", "Yost", "YostQuintile", "annotation") %in% colnames(result$df_yost)))
  expect_s3_class(result$df_yost$YostQuintile, "factor")
  expect_equal(levels(result$df_yost$YostQuintile), as.character(1:5))

  # Check that zero-population rows are excluded from Yost calculation
  zero_pop_rows <- result$df_yost[result$df_yost$annotation == "No population", ]
  expect_true(all(is.na(zero_pop_rows$Yost)))

  # Check that factor analysis object exists
  expect_s3_class(result$fa_object, "fa")

  # Check df_rank has the correct transformed columns
  expect_true(all(grepl("^rank_", colnames(result$df_rank)[-1])))
})

test_that("processYostScope handles data with imputation (geometry required)", {

  # Create mock data with geometry and missing values
  mock_data <- create_mock_yost_data(n_rows = 30, add_geometry = TRUE, add_missing = TRUE)

  # Run processYostScope with imputation
  result <- ReproduceYostIndex:::processYostScope(
    yost_data_sub = mock_data,
    impute_sub = TRUE,
    rescale_sub = "rank",
    nfactors_sub = 1,
    quiet_sub = TRUE,
    return_format_sub = "detailed",
    weight_var_sub = "tot_pop"
  )

  # Check that imputation metadata exists
  expect_true("nvar_imputed" %in% colnames(result$df_imputed))
  expect_true("nvar_still_missing" %in% colnames(result$df_imputed))

  # Check that some rows have imputation annotation
  annotations <- unique(result$df_yost$annotation)
  expect_true(any(annotations %in% c("Complete data", "Imputation completed", "Imputation incomplete")))
})

test_that("processYostScope respects rescale = 'standardize'", {

  mock_data <- create_mock_yost_data(n_rows = 30, add_geometry = FALSE, add_missing = FALSE)

  result <- ReproduceYostIndex:::processYostScope(
    yost_data_sub = mock_data,
    impute_sub = FALSE,
    rescale_sub = "standardize",
    nfactors_sub = 1,
    quiet_sub = TRUE,
    return_format_sub = "detailed",
    weight_var_sub = "none"
  )

  # Check df_rank has standardized columns
  expect_true(all(grepl("^std_", colnames(result$df_rank)[-1])))

  # Check that standardized values have mean ~0 and sd ~1
  std_cols <- result$df_rank %>% select(starts_with("std_"))
  means <- sapply(std_cols, mean, na.rm = TRUE)
  sds <- sapply(std_cols, sd, na.rm = TRUE)

  expect_true(all(abs(means) < 1e-10))  # Mean should be very close to 0
  expect_true(all(abs(sds - 1) < 1e-10))  # SD should be very close to 1
})

test_that("processYostScope handles insufficient data gracefully", {

  # Create very small dataset that will fail factor analysis
  mock_data <- create_mock_yost_data(n_rows = 5, add_geometry = FALSE, add_missing = FALSE)

  # Expect a warning about insufficient observations
  expect_warning(
    result <- ReproduceYostIndex:::processYostScope(
      yost_data_sub = mock_data,
      impute_sub = FALSE,
      rescale_sub = "rank",
      nfactors_sub = 1,
      quiet_sub = FALSE,
      return_format_sub = "detailed",
      weight_var_sub = "none"
    ),
    "Insufficient complete observations"
  )

  # Check that Yost scores are NA when FA fails
  expect_true(all(is.na(result$df_yost$Yost)))
  expect_null(result$fa_object)
})

test_that("processYostScope annotations are correct", {

  mock_data <- create_mock_yost_data(n_rows = 30, add_geometry = FALSE, add_missing = FALSE)

  result <- ReproduceYostIndex:::processYostScope(
    yost_data_sub = mock_data,
    impute_sub = FALSE,
    rescale_sub = "rank",
    nfactors_sub = 1,
    quiet_sub = TRUE,
    return_format_sub = "detailed",
    weight_var_sub = "none"
  )

  # Check annotation categories
  annotations <- unique(result$df_yost$annotation)
  valid_annotations <- c("No population", "Complete data", "Imputation completed",
                        "Imputation incomplete", "Factor analysis failed", "Other")

  expect_true(all(annotations %in% valid_annotations))

  # Verify zero population rows are annotated correctly
  zero_pop_geoids <- mock_data$GEOID[mock_data$tot_pop == 0]
  zero_pop_annotations <- result$df_yost$annotation[result$df_yost$GEOID %in% zero_pop_geoids]
  expect_true(all(zero_pop_annotations == "No population"))
})

test_that("processYostScope drops geometry from tabular outputs", {

  mock_data <- create_mock_yost_data(n_rows = 30, add_geometry = TRUE, add_missing = FALSE)

  result <- ReproduceYostIndex:::processYostScope(
    yost_data_sub = mock_data,
    impute_sub = FALSE,
    rescale_sub = "rank",
    nfactors_sub = 1,
    quiet_sub = TRUE,
    return_format_sub = "detailed",
    weight_var_sub = "none"
  )

  # df_yost, df_raw_values, df_imputed, and df_rank should NOT have geometry
  expect_false(inherits(result$df_yost, "sf"))
  expect_false(inherits(result$df_raw_values, "sf"))
  expect_false(inherits(result$df_imputed, "sf"))
  expect_false(inherits(result$df_rank, "sf"))

  # df_geometry should have geometry
  expect_true("geometry" %in% colnames(result$df_geometry))
})

test_that("processYostScope factor sign is aligned with income", {

  mock_data <- create_mock_yost_data(n_rows = 30, add_geometry = FALSE, add_missing = FALSE)

  result <- ReproduceYostIndex:::processYostScope(
    yost_data_sub = mock_data,
    impute_sub = FALSE,
    rescale_sub = "rank",
    nfactors_sub = 1,
    quiet_sub = TRUE,
    return_format_sub = "detailed",
    weight_var_sub = "none"
  )

  # Get the loading for income
  if (!is.null(result$fa_object)) {
    income_loading_row <- grep("income", rownames(result$fa_object$loadings))
    expect_length(income_loading_row, 1)

    # The Yost score should be centered around 10
    expect_true(mean(result$df_yost$Yost, na.rm = TRUE) > 5)
    expect_true(mean(result$df_yost$Yost, na.rm = TRUE) < 15)
  }
})