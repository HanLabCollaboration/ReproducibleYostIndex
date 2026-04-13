test_that("cleanAcsNames works for all geographies", {

  # Test data
  df_cbg <- data.frame(
    NAME = "Block Group 1, Census Tract 1.00, Los Angeles County, California"
  )
  df_tract <- data.frame(
    NAME = "Census Tract 2.00, Alameda County, California"
  )
  df_county <- data.frame(
    NAME = "Orange County, California"
  )
  df_state <- data.frame(
    NAME = "California"
  )

  # Test 'block group' (and 'cbg' alias)
  out_cbg <- ReproduceYost:::cleanAcsNames(df_cbg, geo = "block group")
  expect_named(out_cbg, c("NAME", "block_group", "tract", "county", "state"))
  expect_equal(out_cbg$block_group, "1")
  expect_equal(out_cbg$tract, "1.00")
  expect_equal(out_cbg$county, "Los Angeles")
  expect_equal(out_cbg$state, "California")

  # Test 'tract'
  out_tract <- ReproduceYost:::cleanAcsNames(df_tract, geo = "tract")
  expect_named(out_tract, c("NAME", "tract", "county", "state"))
  expect_equal(out_tract$tract, "2.00")
  expect_equal(out_tract$county, "Alameda")

  # Test 'county'
  out_county <- ReproduceYost:::cleanAcsNames(df_county, geo = "county")
  expect_named(out_county, c("NAME", "county", "state"))
  expect_equal(out_county$county, "Orange")
  expect_equal(out_county$state, "California")

  # Test 'state'
  out_state <- ReproduceYost:::cleanAcsNames(df_state, geo = "state")
  expect_named(out_state, c("NAME", "state"))
  expect_equal(out_state$state, "California")
})

test_that("cleanAcsNames handles different county types", {

  # Test Parish (Louisiana)
  df_parish <- data.frame(
    NAME = "Orleans Parish, Louisiana"
  )
  out_parish <- ReproduceYost:::cleanAcsNames(df_parish, geo = "county")
  expect_equal(out_parish$county, "Orleans")

  # Test Borough (Alaska)
  df_borough <- data.frame(
    NAME = "Anchorage Borough, Alaska"
  )
  out_borough <- ReproduceYost:::cleanAcsNames(df_borough, geo = "county")
  expect_equal(out_borough$county, "Anchorage")

  # Test Census Area (Alaska)
  df_census_area <- data.frame(
    NAME = "Bethel Census Area, Alaska"
  )
  out_census_area <- ReproduceYost:::cleanAcsNames(df_census_area, geo = "county")
  expect_equal(out_census_area$county, "Bethel")

  # Test Municipio (Puerto Rico)
  df_municipio <- data.frame(
    NAME = "San Juan Municipio, Puerto Rico"
  )
  out_municipio <- ReproduceYost:::cleanAcsNames(df_municipio, geo = "county")
  expect_equal(out_municipio$county, "San Juan")
})

test_that("cleanAcsNames handles 'cbg' alias", {

  df_cbg <- data.frame(
    NAME = "Block Group 2, Census Tract 3.01, Kings County, New York"
  )

  # Test using 'cbg' alias
  out_cbg <- ReproduceYost:::cleanAcsNames(df_cbg, geo = "cbg")
  expect_named(out_cbg, c("NAME", "block_group", "tract", "county", "state"))
  expect_equal(out_cbg$block_group, "2")
  expect_equal(out_cbg$tract, "3.01")
})

test_that("cleanAcsNames removes extra whitespace", {

  df_extra_space <- data.frame(
    NAME = "Census Tract  2.50,  Alameda County,  California"
  )

  out <- ReproduceYost:::cleanAcsNames(df_extra_space, geo = "tract")

  # All fields should have trimmed whitespace
  expect_equal(out$tract, "2.50")
  expect_equal(out$county, "Alameda")
  expect_equal(out$state, "California")
})

test_that("cleanAcsNames errors on unknown geography", {

  df <- data.frame(NAME = "Test")

  expect_error(
    ReproduceYost:::cleanAcsNames(df, geo = "zip_code"),
    "Unknown geo type"
  )
})
