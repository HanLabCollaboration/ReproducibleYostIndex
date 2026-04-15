library(testthat)
library(ReproduceYostIndex)

varlist <- c("income", "wkcls", "unemp", "educ", "poverty150", "rent", "hval")

# Helper: build mock parent (tract) and child (block group) data
make_stabilize_data <- function() {
  set.seed(42)

  parent_data <- data.frame(
    GEOID      = c("06001", "06002", "06003", "06004"),
    income     = c(50000, 60000, 55000, 45000),
    wkcls      = c(0.60, 0.70, 0.65, 0.55),
    unemp      = c(5, 4, 6, 8),
    educ       = c(12, 14, 13, 11),
    poverty150 = c(15, 10, 12, 20),
    rent       = c(1200, 1500, 1300, 1000),
    hval       = c(300000, 400000, 350000, 250000)
  )

  n <- 20  # 5 block groups per tract
  child_data <- data.frame(
    GEOID      = paste0(rep(c("06001", "06002", "06003", "06004"), each = 5),
                        sprintf("%06d", 1:n)),
    income     = rnorm(n, 52000, 8000),
    wkcls      = runif(n, 0.5, 0.8),
    unemp      = runif(n, 3, 10),
    educ       = runif(n, 10, 16),
    poverty150 = runif(n, 5, 25),
    rent       = rnorm(n, 1300, 200),
    hval       = rnorm(n, 330000, 50000),
    income_moe     = abs(rnorm(n, 3000, 500)),
    wkcls_moe      = abs(rnorm(n, 0.05, 0.01)),
    unemp_moe      = abs(rnorm(n, 1, 0.2)),
    educ_moe       = abs(rnorm(n, 0.5, 0.1)),
    poverty150_moe = abs(rnorm(n, 2, 0.5)),
    rent_moe       = abs(rnorm(n, 100, 20)),
    hval_moe       = abs(rnorm(n, 15000, 3000))
  )

  list(parent = parent_data, child = child_data)
}

test_that("stabilizeYost returns a data frame with correct row count", {
  d <- make_stabilize_data()
  result <- ReproduceYostIndex:::stabilizeYost(d$child, d$parent, varlist, geo_parent_key_len = 5)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(d$child))
})

test_that("stabilizeYost produces shrunk, weight, and flag columns for each variable", {
  d <- make_stabilize_data()
  result <- ReproduceYostIndex:::stabilizeYost(d$child, d$parent, varlist, geo_parent_key_len = 5)

  expect_true(all(paste0(varlist, "_stabilized")    %in% colnames(result)))
  expect_true(all(paste0(varlist, "_wgt")       %in% colnames(result)))
  expect_true(all(paste0(varlist, "_s2")        %in% colnames(result)))
  expect_true(all(paste0("stabilizedflg_", varlist) %in% colnames(result)))
})

test_that("stabilizeYost flags are 0 or 1", {
  d <- make_stabilize_data()
  result <- ReproduceYostIndex:::stabilizeYost(d$child, d$parent, varlist, geo_parent_key_len = 5)

  for (col in paste0("stabilizedflg_", varlist)) {
    expect_true(all(result[[col]] %in% c(0L, 1L)))
  }
})

test_that("stabilizeYost weights are between 0 and 1", {
  d <- make_stabilize_data()
  result <- ReproduceYostIndex:::stabilizeYost(d$child, d$parent, varlist, geo_parent_key_len = 5)

  for (col in paste0(varlist, "_wgt")) {
    valid <- result[[col]][!is.na(result[[col]])]
    expect_true(all(valid >= 0 & valid <= 1))
  }
})

test_that("stabilizeYost shrunk values lie between original and parent", {
  d <- make_stabilize_data()
  result <- ReproduceYostIndex:::stabilizeYost(d$child, d$parent, varlist, geo_parent_key_len = 5)

  for (var in varlist) {
    shrunk_rows <- result[result[[paste0("stabilizedflg_", var)]] == 1L, ]
    if (nrow(shrunk_rows) == 0) next

    orig   <- shrunk_rows[[var]]
    parent <- shrunk_rows[[paste0(var, "_parent")]]
    shrunk <- shrunk_rows[[paste0(var, "_stabilized")]]

    lo <- pmin(orig, parent)
    hi <- pmax(orig, parent)
    expect_true(all(shrunk >= lo - 1e-10 & shrunk <= hi + 1e-10, na.rm = TRUE),
                info = paste("shrunk values out of range for variable:", var))
  }
})

test_that("stabilizeYost preserves original values when shrinkage is not applied", {
  d <- make_stabilize_data()
  result <- ReproduceYostIndex:::stabilizeYost(d$child, d$parent, varlist, geo_parent_key_len = 5)

  for (var in varlist) {
    no_shrink_rows <- result[result[[paste0("stabilizedflg_", var)]] == 0L, ]
    if (nrow(no_shrink_rows) == 0) next

    expect_equal(
      no_shrink_rows[[paste0(var, "_stabilized")]],
      no_shrink_rows[[var]],
      info = paste("original value not preserved for variable:", var)
    )
  }
})

test_that("stabilizeYost correctly links child units to parent via GEOID truncation", {
  d <- make_stabilize_data()
  result <- ReproduceYostIndex:::stabilizeYost(d$child, d$parent, varlist, geo_parent_key_len = 5)

  # Every child GEOID should have a matching parent value
  for (var in varlist) {
    expect_false(all(is.na(result[[paste0(var, "_parent")]])),
                 info = paste("all parent values are NA for variable:", var))
  }
})
