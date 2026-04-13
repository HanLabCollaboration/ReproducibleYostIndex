#' Fetch raw ACS data for Yost Index variables
#'
#' @description
#' This is an internal helper function that queries the Census API for all
#' variables required to compute the Yost Index.
#'
#' @param geo Geography level (e.g., "county", "tract").
#' @param year The ACS5 year.
#' @param states A vector of state abbreviations or 'all'.
#' @param get_geometry Boolean, whether to download geometry.
#'
#' @return A tidycensus data frame.

fetchYostAcs <- function(geo, year, states, get_geometry) {
  # --- Variable Definitions ---
  var_POPTOT <- 'B01003_001'
  var_MHHINC <- "B19013_001"
  var_WKCLS <- paste0("C24010_0", c(20,24,25,26,27,30,34,56,60,61,62,63,66,70))
  var_WKCLS_pop <- "C24010_001"
  var_UNEMP <- c("B23025_005")
  var_UNEMP_pop <- "B23025_003"
  var_Edu_denom <- 'B15002_001'
  var_Edu_P1 <- c(paste0("B15002_00", c(3:9)), "B15002_010", paste0("B15002_0", c(20:27)))
  var_Edu_P2 <- c("B15002_011", "B15002_028")
  var_Edu_P3 <- c(paste0("B15002_0", c(12:18)), paste0("B15002_0", c(29:35)))
  var_RATIOP <- c(paste0("C17002_00", c(2:5)))
  var_RATIOP_pop <- "C17002_001"
  var_MGRRNT <- "B25064_001"
  var_MVALUE <- "B25077_001"

  vars <- c(var_POPTOT, var_Edu_denom, var_WKCLS_pop, var_UNEMP_pop, var_RATIOP_pop,
            var_MHHINC, var_WKCLS, var_UNEMP, var_Edu_P1, var_Edu_P2, var_Edu_P3,
            var_RATIOP, var_MGRRNT, var_MVALUE)

  # Keep track of the original names for the calculation step
  acs_vars <- list(
    vars = paste(vars, "E", sep = ""),
    var_POPTOT = paste0(var_POPTOT, "E"),
    var_MHHINC = paste0(var_MHHINC, "E"),
    var_WKCLS = paste0(var_WKCLS, "E"),
    var_WKCLS_pop = paste0(var_WKCLS_pop, "E"),
    var_UNEMP = paste0(var_UNEMP, "E"),
    var_UNEMP_pop = paste0(var_UNEMP_pop, "E"),
    var_Edu_denom = paste0(var_Edu_denom, "E"),
    var_Edu_P1 = paste0(var_Edu_P1, "E"),
    var_Edu_P2 = paste0(var_Edu_P2, "E"),
    var_Edu_P3 = paste0(var_Edu_P3, "E"),
    var_RATIOP = paste0(var_RATIOP, "E"),
    var_RATIOP_pop = paste0(var_RATIOP_pop, "E"),
    var_MGRRNT = paste0(var_MGRRNT, "E"),
    var_MVALUE = paste0(var_MVALUE, "E")
  )

  # --- Data Fetching ---
  # tidycensus does not accept a `state` filter when geography = "state"
  fetch_args <- list(
    geography   = geo,
    year        = year,
    output      = "wide",
    variables   = acs_vars$vars,
    survey      = "acs5",
    geometry    = get_geometry,
    cache_table = TRUE
  )
  if (geo != "state") fetch_args$state <- states

  yost_data_raw <- do.call(tidycensus::get_acs, fetch_args)

  # Attach var names as an attribute for the next step
  yost_data_raw <- yost_data_raw |>
    dplyr::select(GEOID, NAME, dplyr::ends_with('E'), dplyr::ends_with('M'))
  attr(yost_data_raw, "acs_vars") <- acs_vars
  return(yost_data_raw)
}
