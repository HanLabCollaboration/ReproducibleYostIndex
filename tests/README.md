# Testing Guide for ReproduceYostIndex

This document provides comprehensive guidance on testing the ReproduceYostIndex package.

## Overview

The ReproduceYostIndex package uses the `testthat` framework for unit testing and `vcr` for mocking API calls to the Census Bureau. This ensures tests are:
- **Fast**: No repeated API calls after initial cassette creation
- **Reliable**: Tests don't fail due to network issues
- **Reproducible**: Same results every time

## Test Structure

```
tests/
├── testthat/
│   ├── test-computeYostIndex.R       # Main function tests
│   ├── test-processYostScope.R       # Scope processing tests
│   ├── test-fetchYostAcs.R           # Data fetching tests
│   ├── test-cleanAcsNames.R          # Name parsing tests
│   ├── test-calculateYostVars.R      # Variable calculation tests
│   └── test-imputeMissing.R          # Spatial imputation tests
├── testthat.R                         # Test runner
└── README.md                          # This file
```

## Running Tests

### Run all tests
```r
# From R console
devtools::test()

# Or using testthat directly
testthat::test_dir("tests/testthat")
```

### Run specific test file
```r
testthat::test_file("tests/testthat/test-computeYostIndex.R")
```

### Run tests with coverage report
```r
covr::package_coverage()
covr::report()
```

## Test Categories

### 1. Input Validation Tests
These tests ensure that functions properly validate their inputs and provide clear error messages.

**Example:**
```r
test_that("computeYostIndex handles bad inputs", {
  expect_error(
    computeYostIndex(geo = "invalid", year = 2022),
    "should be one of"
  )
})
```

**Best Practices:**
- Test all invalid parameter combinations
- Verify error messages are informative
- Check boundary conditions (e.g., year = 2011 vs 2010)

### 2. Core Functionality Tests
These tests verify that functions produce correct outputs under normal conditions.

**Example:**
```r
test_that("calculateYostVars computes variables correctly", {
  # Create predictable test data
  raw_data <- data.frame(...)

  # Run function
  result <- calculateYostVars(raw_data, acs_vars)

  # Verify calculations
  expect_equal(result$unemp, (10 / 100) * 100)
})
```

**Best Practices:**
- Use small, predictable datasets
- Manually calculate expected results
- Test all major code paths

### 3. Edge Case Tests
These tests ensure functions handle unusual or extreme inputs gracefully.

**Example:**
```r
test_that("calculateYostVars handles NaN values correctly", {
  # Create data that produces division by zero
  raw_data <- data.frame(
    var = 100,
    denom = 0  # Will produce NaN
  )

  result <- calculateYostVars(raw_data, acs_vars)

  # NaN should be converted to NA
  expect_true(is.na(result$calculated_var))
})
```

**Common Edge Cases:**
- Empty datasets
- Missing values (NA)
- Zero denominators (NaN)
- Single-row datasets
- Isolated spatial features
- Empty geometries

### 4. API Mocking with VCR
The `vcr` package records API responses and replays them in subsequent test runs.

**Example:**
```r
test_that("fetchYostAcs returns correct structure", {
  vcr::use_cassette("fetch-yost-acs-county", {
    # This API call is recorded the first time, then replayed
    result <- fetchYostAcs(geo = "county", year = 2022, states = "CA")
  })

  expect_s3_class(result, "data.frame")
  expect_true("GEOID" %in% colnames(result))
})
```

**VCR Best Practices:**
- Use descriptive cassette names
- One cassette per test scenario
- Store cassettes in version control (tests/fixtures/)
- Re-record cassettes when API behavior changes:
  ```r
  # Delete old cassettes and re-run tests
  unlink("tests/fixtures/*.yml")
  ```

## Writing New Tests

### Step 1: Identify What to Test
Based on the function's documentation:
1. **Parameters**: Test each parameter's valid/invalid values
2. **Return values**: Verify structure and content
3. **Edge cases**: Empty inputs, NAs, extreme values
4. **Error handling**: Wrong types, missing required args

### Step 2: Create Test File
```r
# tests/testthat/test-myFunction.R
library(testthat)
library(ReproduceYostIndex)

test_that("myFunction does X correctly", {
  # Arrange: Set up test data
  test_data <- ...

  # Act: Run the function
  result <- myFunction(test_data)

  # Assert: Check the results
  expect_equal(result$value, expected_value)
})
```

### Step 3: Use Appropriate Expectations
- `expect_equal(a, b)` - Exact equality
- `expect_true(x)` / `expect_false(x)` - Boolean checks
- `expect_error(code, regexp)` - Error messages
- `expect_warning(code, regexp)` - Warnings
- `expect_s3_class(obj, "class")` - Object class
- `expect_length(x, n)` - Vector length
- `expect_named(x, names)` - Named elements

## Test Coverage Goals

### Current Coverage by Function

| Function | Tests | Coverage |
|----------|-------|----------|
| `computeYostIndex` | 14 tests | ✅ Comprehensive |
| `processYostScope` | 8 tests | ✅ Comprehensive |
| `fetchYostAcs` | 7 tests | ✅ Good |
| `cleanAcsNames` | 6 tests | ✅ Good |
| `calculateYostVars` | 4 tests | ✅ Good |
| `imputeMissing` | 8 tests | ✅ Comprehensive |

### Coverage Goals
- **Critical functions**: >95% line coverage
- **Utility functions**: >80% line coverage
- **Internal helpers**: >70% line coverage

## Common Testing Patterns

### Pattern 1: Testing Spatial Operations
```r
test_that("function handles spatial data correctly", {
  # Create simple geometries
  poly <- st_polygon(list(rbind(
    c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
  )))

  df_geo <- st_sf(
    GEOID = "test",
    value = 100,
    geometry = st_sfc(poly)
  )

  result <- myFunction(df_geo)

  # Check if geometry is preserved/dropped as expected
  expect_true(inherits(result, "sf"))  # or expect_false
})
```

### Pattern 2: Testing with Mock Data
```r
# Helper function to create consistent test data
create_mock_data <- function(n_rows = 10, add_missing = FALSE) {
  df <- data.frame(
    GEOID = paste0("ID", 1:n_rows),
    var1 = rnorm(n_rows),
    var2 = runif(n_rows)
  )

  if (add_missing) {
    df$var1[sample(1:n_rows, 2)] <- NA
  }

  return(df)
}

test_that("function handles complete data", {
  test_data <- create_mock_data(n_rows = 20, add_missing = FALSE)
  result <- myFunction(test_data)
  expect_equal(nrow(result), 20)
})
```

### Pattern 3: Testing Return Formats
```r
test_that("function respects return_format parameter", {
  # Detailed format
  result_detailed <- myFunction(data, return_format = "detailed")
  expect_type(result_detailed, "list")
  expect_length(result_detailed, 6)

  # Minimal format
  result_minimal <- myFunction(data, return_format = "minimal")
  expect_s3_class(result_minimal, "data.frame")
  expect_equal(ncol(result_minimal), 4)
})
```

## Debugging Failed Tests

### 1. Run test interactively
```r
devtools::load_all()  # Load package functions
source("tests/testthat/test-myFunction.R")  # Run test code line by line
```

### 2. Use browser() for debugging
```r
test_that("my failing test", {
  data <- create_test_data()
  browser()  # Execution will pause here
  result <- myFunction(data)
  expect_equal(result, expected)
})
```

### 3. Check test output
```r
# Run with detailed output
testthat::test_file("tests/testthat/test-myFunction.R", reporter = "location")
```

### 4. Verify test data
```r
# Print intermediate values
test_that("verify calculations", {
  data <- create_test_data()
  print(str(data))  # Inspect structure

  result <- myFunction(data)
  print(result)  # See actual output

  expect_equal(result$value, expected_value)
})
```

## Continuous Integration

Tests should run automatically on:
- Every commit (GitHub Actions)
- Pull requests
- Before package release

**Example .github/workflows/R-CMD-check.yaml:**
```yaml
on: [push, pull_request]

jobs:
  R-CMD-check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: r-lib/actions/setup-r@v2
      - name: Install dependencies
        run: |
          install.packages(c("devtools", "testthat", "vcr"))
          devtools::install_deps()
      - name: Run tests
        run: devtools::test()
```

## Tips for Maintaining Tests

1. **Keep tests fast**: Mock external dependencies, use small datasets
2. **Make tests independent**: Each test should run in isolation
3. **Use descriptive names**: `test_that("function handles empty input", ...)`
4. **Test one thing**: Each test should verify one specific behavior
5. **Update tests with code**: When fixing bugs, add tests to prevent regression
6. **Review coverage regularly**: Use `covr::package_coverage()` to find gaps

## Resources

- [testthat documentation](https://testthat.r-lib.org/)
- [vcr documentation](https://docs.ropensci.org/vcr/)
- [R Packages testing chapter](https://r-pkgs.org/testing-basics.html)
- [Testing best practices](https://rstudio.github.io/r-manuals/r-exts/writing-tests.html)