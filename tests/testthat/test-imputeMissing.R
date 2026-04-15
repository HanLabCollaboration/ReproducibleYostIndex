library(sf)

test_that("imputeMissing correctly imputes and handles edge cases", {

  # 1. Create a simple spatial grid
  # B C
  # A D (isolated)
  poly_A <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  poly_B <- st_polygon(list(rbind(c(0,1), c(1,1), c(1,2), c(0,2), c(0,1))))
  poly_C <- st_polygon(list(rbind(c(1,1), c(2,1), c(2,2), c(1,2), c(1,1))))
  poly_D <- st_polygon(list(rbind(c(5,5), c(6,5), c(6,6), c(5,6), c(5,5)))) # Isolated

  df_geo <- st_sf(
    GEOID = c("A", "B", "C", "D"),
    income = c(100, 200, NA, NA),
    unemp = c(10, NA, 30, NA),
    geometry = st_sfc(poly_A, poly_B, poly_C, poly_D)
  )

  # Run the imputation
  out_impute <- ReproduceYostIndex:::imputeMissing(df_geo, quiet = TRUE)

  # --- Check the results ---

  # Row A: No missing data
  out_A <- out_impute[out_impute$GEOID == "A", ]
  expect_equal(out_A$income, 100)
  expect_equal(out_A$unemp, 10)
  expect_equal(out_A$nvar_imputed, 0)

  # Row B: Missing 'unemp'. Neighbors are A (10) and C (30).
  # Queen contiguity includes C (touches at (1,1))
  # Mean(10, 30) = 20
  out_B <- out_impute[out_impute$GEOID == "B", ]
  expect_equal(out_B$income, 200)
  expect_equal(out_B$unemp, 20)
  expect_equal(out_B$nvar_imputed, 1)
  expect_equal(out_B$nvar_still_missing, 0)

  # Row C: Missing 'income'. Neighbors are A (100) and B (200).
  # Mean(100, 200) = 150
  out_C <- out_impute[out_impute$GEOID == "C", ]
  expect_equal(out_C$income, 150)
  expect_equal(out_C$unemp, 30)
  expect_equal(out_C$nvar_imputed, 1)

  # Row D: Isolated. No neighbors, imputation should fail.
  out_D <- out_impute[out_impute$GEOID == "D", ]
  expect_true(is.na(out_D$income))
  expect_true(is.na(out_D$unemp))
  expect_equal(out_D$nvar_imputed, 0)
  expect_equal(out_D$nvar_still_missing, 2)

})

test_that("imputeMissing performs weighted imputation correctly", {

  # Create a spatial grid: A - B - C
  poly_A <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  poly_B <- st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0))))
  poly_C <- st_polygon(list(rbind(c(2,0), c(3,0), c(3,1), c(2,1), c(2,0))))

  df_geo <- st_sf(
    GEOID = c("A", "B", "C"),
    income = c(100, NA, 200),
    tot_pop = c(1000, 500, 4000),  # Weight variable
    geometry = st_sfc(poly_A, poly_B, poly_C)
  )

  # Run weighted imputation
  out_weighted <- ReproduceYostIndex:::imputeMissing(df_geo, weight_var_sub = "tot_pop", quiet = TRUE)

  # B's income should be weighted mean of A and C
  # weighted.mean(c(100, 200), w = c(1000, 4000)) = (100*1000 + 200*4000) / 5000 = 180
  out_B <- out_weighted[out_weighted$GEOID == "B", ]
  expect_equal(out_B$income, 180)
  expect_equal(out_B$nvar_imputed, 1)
})

test_that("imputeMissing handles empty geometries gracefully", {

  poly_A <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  poly_B <- st_polygon() # Empty geometry

  df_geo <- st_sf(
    GEOID = c("A", "B"),
    income = c(100, NA),
    geometry = st_sfc(poly_A, poly_B)
  )

  # Should handle empty geometry without error
  out_impute <- suppressMessages(ReproduceYostIndex:::imputeMissing(df_geo, quiet = TRUE))

  # Row B should not be imputed (no valid geometry)
  out_B <- out_impute[out_impute$GEOID == "B", ]
  expect_true(is.na(out_B$income))
  expect_equal(out_B$nvar_imputed, 0)
})

test_that("imputeMissing returns data without geometry", {

  poly_A <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  poly_B <- st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0))))

  df_geo <- st_sf(
    GEOID = c("A", "B"),
    income = c(100, NA),
    geometry = st_sfc(poly_A, poly_B)
  )

  out_impute <- ReproduceYostIndex:::imputeMissing(df_geo, quiet = TRUE)

  # Output should NOT be an sf object
  expect_false(inherits(out_impute, "sf"))

  # But should have the data columns
  expect_true("GEOID" %in% colnames(out_impute))
  expect_true("income" %in% colnames(out_impute))
})

test_that("imputeMissing excludes metadata columns from imputation", {

  poly_A <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  poly_B <- st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0))))

  df_geo <- st_sf(
    GEOID = c("A", "B"),
    income = c(100, NA),
    county_state = c("Los Angeles, CA", "Los Angeles, CA"),  # Metadata column
    geometry = st_sfc(poly_A, poly_B)
  )

  out_impute <- ReproduceYostIndex:::imputeMissing(df_geo, quiet = TRUE)

  # county_state should be preserved and not treated as an imputation variable
  expect_true("county_state" %in% colnames(out_impute))
  expect_equal(out_impute$county_state, c("Los Angeles, CA", "Los Angeles, CA"))
})

test_that("imputeMissing errors on missing required columns", {

  # No GEOID column
  df_bad <- data.frame(
    ID = c("A", "B"),
    income = c(100, NA)
  )

  expect_error(
    ReproduceYostIndex:::imputeMissing(df_bad, quiet = TRUE),
    "must contain 'GEOID' and 'geometry'"
  )
})

test_that("imputeMissing handles all-NA neighbors", {

  # Create a grid where B's neighbors both have NA for income
  poly_A <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  poly_B <- st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0))))
  poly_C <- st_polygon(list(rbind(c(2,0), c(3,0), c(3,1), c(2,1), c(2,0))))

  df_geo <- st_sf(
    GEOID = c("A", "B", "C"),
    income = c(NA, NA, NA),
    unemp = c(10, NA, 20),
    geometry = st_sfc(poly_A, poly_B, poly_C)
  )

  out_impute <- ReproduceYostIndex:::imputeMissing(df_geo, quiet = TRUE)

  # B's income should still be NA (all neighbors are NA)
  out_B <- out_impute[out_impute$GEOID == "B", ]
  expect_true(is.na(out_B$income))

  # But unemp should be imputed
  expect_equal(out_B$unemp, 15)  # mean(10, 20)
})

test_that("imputeMissing handles weight_var = 'none'", {

  poly_A <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  poly_B <- st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0))))
  poly_C <- st_polygon(list(rbind(c(2,0), c(3,0), c(3,1), c(2,1), c(2,0))))

  df_geo <- st_sf(
    GEOID = c("A", "B", "C"),
    income = c(100, NA, 200),
    tot_pop = c(1000, 500, 4000),
    geometry = st_sfc(poly_A, poly_B, poly_C)
  )

  # Run with weight_var = "none"
  out_unweighted <- ReproduceYostIndex:::imputeMissing(df_geo, weight_var_sub = "none", quiet = TRUE)

  # B's income should be simple mean: (100 + 200) / 2 = 150
  out_B <- out_unweighted[out_unweighted$GEOID == "B", ]
  expect_equal(out_B$income, 150)
})