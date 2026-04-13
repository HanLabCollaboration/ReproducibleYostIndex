#' Compute the Yost Index
#'
#' @description
#' Fetches and processes ACS data to calculate the Yost Index, a composite
#' measure of neighborhood-level socioeconomic status (SES).
#'
#' The function automates: 1) data fetching from `tidycensus`, 2) calculation
#' of the 7 component variables (income, working class, unemployment, education,
#' poverty, rent, and home value), 3) optional shrinkage stabilization to reduce
#' small-area estimation noise by borrowing strength from parent geographies,
#' 4) optional spatial mean imputation for missing values using queen contiguity,
#' and 5) factor analysis to compute the final composite index.
#'
#' The output always includes a baseline Yost (`df_yost_raw`, no shrink, no
#' impute) alongside the requested version (`df_yost`), so results can be
#' directly compared. The requested Yost column is named dynamically:
#' `Yost`, `YostShrunk`, `YostImputed`, or `YostShrunkImputed`.
#'
#' @param geo The geography level to compute the index for. Must be one of
#'   `"state"`,`"county"`, `"tract"`, or `"block group"` (or `"cbg"`).
#' @param year The 5-year ACS survey year (e.g., 2022). Must be ≥ 2011, and
#'   must be ≥ 2013 for census block group
#' @param nfactors The number of factors to extract in the factor analysis.
#'   Defaults to 1.
#' @param states A vector of state abbreviations (e.g., `c("CA", "NY")`) or
#'   `'all'` to run for the entire US.
#' @param shrink Logical. If `TRUE`, applies a shrinkage estimator to stabilize
#'   the 7 component variables before factor analysis. Each lower-level estimate
#'   (e.g., tract) is pulled toward its parent geography (e.g., county) using a
#'   data-adaptive weight based on within-parent heterogeneity and sampling
#'   variance from the ACS margins of error. Defaults to `FALSE`.
#' @param impute Logical. If `TRUE`, performs a simple spatial mean imputation
#'   for missing data using 'queen' contiguity.
#' @param rescale Method for rescaling the 7 component variables before the
#'   factor analysis. Must be `"rank"` (default) or `"standardize"`.
#' @param quiet Logical. If `TRUE`, suppresses all messages and warnings.
#' @param return_format Return `"detailed"` (default) or `"minimal"`.
#'   `"minimal"` returns only `GEOID`, the requested Yost column, its quintile,
#'   and `annotation`.
#' @param scope Calculate the index using a `"national"` (default), `"state"`, or
#'   `"county"` scope. `"national"` ranks all geographies against each other
#'   (automatically sets `states = 'all'`). `"state"` ranks geographies within
#'   each state separately (only valid for `geo = "county"`, `"tract"`, or
#'   `"block group"`). `"county"` ranks geographies within each county separately
#'   (only valid for `geo = "tract"` or `"block group"`).
#' @param keep_geometry Logical. If `TRUE` (default), attaches geometry to
#'   `df_geometry` in the returned list. Geometry is always fetched when
#'   `impute = TRUE`.
#' @param weight_var The variable to use for weighted spatial imputation.
#'   Must be `"tot_pop"` (default) or `"none"`. Passed to `imputeMissing()` as
#'   `weight_var_sub`. Only relevant when `impute = TRUE`.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' A list containing the following elements:
#' \itemize{
#'   \item `df_yost_raw`: Baseline Yost (no shrink, no impute) with `Yost` and
#'     `YostQuintile`. Identical to `df_yost` when neither shrink nor impute is requested.
#'   \item `df_yost`: Requested Yost with dynamically named columns depending on
#'     parameters: `Yost`, `YostShrunk`, `YostImputed`, or `YostShrunkImputed`,
#'     plus the corresponding quintile column.
#'   \item `df_raw_values`: Raw input data without geometry.
#'   \item `df_geometry`: Data frame containing only GEOID and geometry.
#'   \item `df_imputed`: Imputed data without geometry.
#'   \item `df_rank`: Ranked/standardized variables used for factor analysis.
#'   \item `obj_factor`: The `psych::fa` object (or a list of objects if
#'     `scope = "state"` or `scope = "county"`).
#' }
#'
#' @importFrom glue glue
#' @importFrom dplyr group_split select any_of arrange rename
#' @importFrom purrr map map_dfr map_chr keep
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Requires a Census API Key
#'   # tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)
#'
#'   # Compute Yost Index for tracts in California for 2022
#'   yost_ca_tracts <- computeYostIndex(
#'     geo    = "tract",
#'     year   = 2022,
#'     states = "CA",
#'     shrink = TRUE,
#'     impute = TRUE,
#'     scope  = "state"
#'   )
#'
#'   # Compare raw vs requested side by side
#'   head(yost_ca_tracts$df_yost_raw)   # Yost, YostQuintile
#'   head(yost_ca_tracts$df_yost)       # YostShrunkImputed, YostShrunkImputedQuintile
#'
#'   # Compute index for all counties in New England (state scope)
#'   yost_ne <- computeYostIndex(
#'     geo    = "county",
#'     year   = 2022,
#'     states = c("ME", "NH", "VT", "MA", "RI", "CT"),
#'     scope  = "state",
#'     return_format = "minimal"
#'   )
#' }

computeYostIndex <- function(
    geo = "tract",
    year = 2022,
    nfactors = 1,
    states = 'CA',
    shrink = FALSE,
    impute = TRUE,
    rescale = 'rank',
    quiet = FALSE,
    return_format = 'detailed',
    scope = "state",
    keep_geometry = TRUE,
    weight_var = 'tot_pop',
    ...){

  # --- 1. Argument Checks ---
  geo           <- rlang::arg_match(geo, values = c("state", "county", "tract", "block group", "cbg"))
  rescale       <- rlang::arg_match(rescale, values = c("rank", "standardize"))
  return_format <- rlang::arg_match(return_format, values = c("minimal", "detailed"))
  scope         <- rlang::arg_match(scope, values = c("national", "state", "county"))
  weight_var    <- rlang::arg_match(weight_var, values = c("tot_pop", "none"))
  stopifnot(is.numeric(nfactors), nfactors == 1)
  stopifnot(is.numeric(year), year >= 2011)

  if (scope == "county" && !geo %in% c("tract", "block group", "cbg")) {
    stop("scope = 'county' can only be used with geo = 'tract', 'block group', or 'cbg'")
  }
  if (scope == "state" && !geo %in% c("county", "tract", "block group", "cbg")) {
    stop("scope = 'state' can only be used with geo = 'county', 'tract', 'block group', or 'cbg'")
  }
  if (scope == "national") {
    message(glue::glue("Since the scope == 'national', it will pull all states"))
    states <- 'all'
  }
  if (year <= 2012) {
    message(glue::glue('For years 2011 and 2012, data is only available for county, tract, and state level'))
    stopifnot(geo %in% c('county', 'tract'))
  }

  get_geometry <- impute || keep_geometry

  if (states[1] == 'all') {
    all_geos   <- unique(tidycensus::fips_codes$state)
    non_states <- c("DC", "PR", "AS", "GU", "MP", "VI", "UM")
    states     <- setdiff(all_geos, non_states)
  } else {
    all_geos       <- unique(tidycensus::fips_codes$state)
    non_states     <- c("DC", "PR", "AS", "GU", "MP", "VI", "UM")
    valid_states   <- setdiff(all_geos, non_states)
    invalid_states <- setdiff(states, valid_states)
    if (length(invalid_states) > 0) {
      invalid_states_str <- glue::glue_collapse(invalid_states, sep = ", ")
      stop(glue::glue("Invalid state abbreviations provided: {invalid_states_str}.
                      Please use standard 2-letter postal codes for the 50 US states."))
    }
  }

  # --- 2. Data Fetching ---
  if (!quiet) message("Fetching raw ACS data...")
  yost_data_raw <- fetchYostAcs(geo, year, states, get_geometry)
  acs_vars <- attr(yost_data_raw, "acs_vars")
  varlist  <- c("income", "wkcls", "unemp", "educ", "poverty150", "rent", "hval")

  yost_data_clean <- cleanAcsNames(dframe = yost_data_raw, geo = geo)

  if (!quiet) message("Calculating Yost component variables...")

  # Requested data (with shrink if asked)
  yost_data <- yost_data_clean |>
    calculateYostVars(acs_vars = acs_vars, shrink_sub = shrink) |>
    dplyr::select(
      GEOID, NAME, dplyr::any_of(c("block_group", "tract", "county", "state")),
      tot_pop, income, wkcls, unemp, educ, poverty150, rent, hval,
      dplyr::any_of(paste0(varlist, "_moe")),
      dplyr::any_of("geometry")
    )

  # --- 3. Determine output column names based on requested settings ---
  yost_col          <- dplyr::case_when(
    shrink && impute ~ "YostShrunkImputed",
    shrink           ~ "YostShrunk",
    impute           ~ "YostImputed",
    TRUE             ~ "Yost"
  )
  yost_quintile_col <- paste0(yost_col, "Quintile")

  # --- 4. Run REQUESTED pipeline (shrink/impute as asked) ---
  if (!quiet) message(switch(scope,
    "state"    = "Scope is 'state'. Processing each state separately",
    "county"   = "Scope is 'county'. Processing each county separately",
    "national" = "Scope is 'national'. Imputing, harmonizing and computing factor for all geoIDs."
  ))

  if (scope == "state") {

    list_of_state_data <- dplyr::group_split(yost_data, state)
    results_list <- purrr::map(
      list_of_state_data, ~processYostScope(
        yost_data_sub     = .x,
        shrink_sub        = shrink,
        geo               = geo,
        year              = year,
        states            = states,
        acs_vars          = acs_vars,
        varlist           = varlist,
        impute_sub        = impute,
        rescale_sub       = rescale,
        nfactors_sub      = nfactors,
        quiet_sub         = quiet,
        return_format_sub = return_format,
        weight_var_sub    = weight_var
      ))
    results_list  <- purrr::keep(results_list, ~!is.null(.x))
    df_yost       <- purrr::map_dfr(results_list, "df_yost")
    df_raw_values <- purrr::map_dfr(results_list, "df_raw_values")
    df_geometry   <- purrr::map_dfr(results_list, "df_geometry")
    df_imputed    <- purrr::map_dfr(results_list, "df_imputed")
    df_rank       <- purrr::map_dfr(results_list, "df_rank")
    list_of_fa_objects <- purrr::map(results_list, "fa_object")
    names(list_of_fa_objects) <- purrr::map_chr(results_list, ~unique(.x$df_raw_values$state))
    out_obj_factor <- list_of_fa_objects

  } else if (scope == "county") {

    yost_data <- yost_data |>
      dplyr::mutate(county_state = paste(county, state, sep = ", "))
    list_of_county_data <- dplyr::group_split(yost_data, county_state)
    results_list <- purrr::map(
      list_of_county_data, ~processYostScope(
        yost_data_sub     = .x,
        shrink_sub        = shrink,
        geo               = geo,
        year              = year,
        states            = states,
        acs_vars          = acs_vars,
        varlist           = varlist,
        impute_sub        = impute,
        rescale_sub       = rescale,
        nfactors_sub      = nfactors,
        quiet_sub         = quiet,
        return_format_sub = return_format,
        weight_var_sub    = weight_var
      ))
    results_list  <- purrr::keep(results_list, ~!is.null(.x))
    df_yost       <- purrr::map_dfr(results_list, "df_yost")
    df_raw_values <- purrr::map_dfr(results_list, "df_raw_values")
    df_geometry   <- purrr::map_dfr(results_list, "df_geometry")
    df_imputed    <- purrr::map_dfr(results_list, "df_imputed")
    df_rank       <- purrr::map_dfr(results_list, "df_rank")
    list_of_fa_objects <- purrr::map(results_list, "fa_object")
    names(list_of_fa_objects) <- purrr::map_chr(results_list, ~unique(.x$df_raw_values$county_state))
    out_obj_factor <- list_of_fa_objects

  } else {

    single_result <- processYostScope(
      yost_data_sub     = yost_data,
      shrink_sub        = shrink,
      geo               = geo,
      year              = year,
      states            = states,
      acs_vars          = acs_vars,
      varlist           = varlist,
      impute_sub        = impute,
      rescale_sub       = rescale,
      nfactors_sub      = nfactors,
      quiet_sub         = quiet,
      return_format_sub = return_format,
      weight_var_sub    = weight_var
    )
    df_yost        <- single_result$df_yost
    df_raw_values  <- single_result$df_raw_values
    df_geometry    <- single_result$df_geometry
    df_imputed     <- single_result$df_imputed
    df_rank        <- single_result$df_rank
    out_obj_factor <- single_result$fa_object

  }

  # Rename Yost columns to reflect what was applied
  df_yost <- df_yost |>
    dplyr::rename(!!yost_col := Yost, !!yost_quintile_col := YostQuintile)

  # --- 5. Run BASELINE pipeline (no shrink, no impute) when adjustments were requested ---
  if (shrink || impute) {
    if (!quiet) message("Computing baseline Yost (no shrink, no impute) for df_yost_raw...")

    yost_data_baseline <- yost_data_clean |>
      calculateYostVars(acs_vars = acs_vars, shrink_sub = FALSE) |>
      dplyr::select(
        GEOID, NAME, dplyr::any_of(c("block_group", "tract", "county", "state")),
        tot_pop, income, wkcls, unemp, educ, poverty150, rent, hval,
        dplyr::any_of("geometry")
      )

    if (scope == "state") {

      list_of_state_data <- dplyr::group_split(yost_data_baseline, state)
      results_list_raw <- purrr::map(
        list_of_state_data, ~processYostScope(
          yost_data_sub     = .x,
          shrink_sub        = FALSE,
          geo               = geo,
          year              = year,
          states            = states,
          acs_vars          = acs_vars,
          varlist           = varlist,
          impute_sub        = FALSE,
          rescale_sub       = rescale,
          nfactors_sub      = nfactors,
          quiet_sub         = TRUE,
          return_format_sub = return_format,
          weight_var_sub    = weight_var
        ))
      results_list_raw <- purrr::keep(results_list_raw, ~!is.null(.x))
      df_yost_raw <- purrr::map_dfr(results_list_raw, "df_yost")

    } else if (scope == "county") {

      yost_data_baseline <- yost_data_baseline |>
        dplyr::mutate(county_state = paste(county, state, sep = ", "))
      list_of_county_data <- dplyr::group_split(yost_data_baseline, county_state)
      results_list_raw <- purrr::map(
        list_of_county_data, ~processYostScope(
          yost_data_sub     = .x,
          shrink_sub        = FALSE,
          geo               = geo,
          year              = year,
          states            = states,
          acs_vars          = acs_vars,
          varlist           = varlist,
          impute_sub        = FALSE,
          rescale_sub       = rescale,
          nfactors_sub      = nfactors,
          quiet_sub         = TRUE,
          return_format_sub = return_format,
          weight_var_sub    = weight_var
        ))
      results_list_raw <- purrr::keep(results_list_raw, ~!is.null(.x))
      df_yost_raw <- purrr::map_dfr(results_list_raw, "df_yost")

    } else {

      baseline_result <- processYostScope(
        yost_data_sub     = yost_data_baseline,
        shrink_sub        = FALSE,
        geo               = geo,
        year              = year,
        states            = states,
        acs_vars          = acs_vars,
        varlist           = varlist,
        impute_sub        = FALSE,
        rescale_sub       = rescale,
        nfactors_sub      = nfactors,
        quiet_sub         = TRUE,
        return_format_sub = return_format,
        weight_var_sub    = weight_var
      )
      df_yost_raw <- baseline_result$df_yost

    }

  } else {
    # No adjustments requested: raw and requested are the same
    df_yost_raw <- df_yost
  }

  # --- 6. Final Output ---
  if (return_format == 'minimal') {
    return(df_yost |> dplyr::arrange(GEOID))
  }

  out <- list(
    df_yost_raw   = df_yost_raw   |> dplyr::arrange(GEOID),
    df_yost       = df_yost       |> dplyr::arrange(GEOID),
    df_raw_values = df_raw_values,
    df_geometry   = df_geometry,
    df_imputed    = df_imputed,
    df_rank       = df_rank,
    obj_factor    = out_obj_factor
  )

  return(out)
}
