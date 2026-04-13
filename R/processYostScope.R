#' Process a single scope for the Yost Index
#'
#' @description
#' This is the primary internal workhorse function for `computeYostIndex`. It
#' processes a single "scope" (e.g., all national data, or data for a
#' single state).
#'
#' It handles:
#' 1. Filtering geographies with no population.
#' 2. Optionally imputing missing data.
#' 3. Harmonizing/rescaling the 7 component variables.
#' 4. Running the factor analysis (`psych::fa`) with robust error handling.
#' 5. Calculating the final Yost score and quintiles.
#' 6. Annotating the final data based on data quality/imputation status.
#'
#' @param yost_data_sub An `sf` data frame subset for a single scope (e.g., one state).
#' @param shrink_sub Logical, whether to apply the shrinkage stabilization method.
#' @param geo The geography level string (e.g., `"tract"`, `"block group"`). Required when `shrink_sub = TRUE`.
#' @param year The ACS5 survey year. Required when `shrink_sub = TRUE`.
#' @param states A vector of state abbreviations. Required when `shrink_sub = TRUE`.
#' @param acs_vars The list of ACS variable names from the attribute. Required when `shrink_sub = TRUE`.
#' @param varlist A character vector of the 7 Yost variable names. Required when `shrink_sub = TRUE`.
#' @param impute_sub Logical, whether to perform spatial imputation.
#' @param rescale_sub The rescaling method ("rank" or "standardize").
#' @param nfactors_sub The number of factors to extract in `psych::fa`.
#' @param quiet_sub Logical, whether to suppress messages and warnings.
#' @param return_format_sub The desired return format (minimal or detailed).
#'   (Note: This is handled by the parent `computeYostIndex` function).
#' @param weight_var_sub A string. The name of the column to use for weighting.
#'
#' @return
#' A list containing six elements:
#' * `df_yost`: Data frame with GEOID, Yost, YostQuintile, and annotation (no geometry).
#' * `df_raw_values`: The raw input data without geometry.
#' * `df_geometry`: Data frame containing only GEOID and geometry.
#' * `df_imputed`: The imputed data without geometry.
#' * `df_rank`: The ranked/standardized variables used for factor analysis.
#' * `fa_object`: The `psych::fa` object, or `NULL` if the analysis failed.
#'
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr filter select any_of mutate across all_of starts_with
#' @importFrom dplyr left_join everything bind_rows ntile case_when
#' @importFrom glue glue
#' @importFrom psych fa

processYostScope <- function(
    yost_data_sub,
    shrink_sub = FALSE,
    geo = NULL,
    year = NULL,
    states = NULL,
    acs_vars = NULL,
    varlist = NULL,
    impute_sub,
    rescale_sub,
    nfactors_sub,
    quiet_sub,
    return_format_sub,
    weight_var_sub = NULL) {


  # Holding the input before manipulating
  raw_yost_data_sub <- yost_data_sub

  # Filter out no-population areas for this subset
  yost_data_sub <- yost_data_sub |> dplyr::filter(tot_pop > 0)

  rows_with_no_pop = nrow(raw_yost_data_sub) - nrow(yost_data_sub)
  if (!quiet_sub) message(glue::glue("... there are {rows_with_no_pop} geoids with no population."))
  if(nrow(yost_data_sub) == 0) return(NULL) # Skip if a state has no valid data

  # Step 2: Stabilize Yost variables (optional shrinkage)
  if (shrink_sub) {
    geo_hierarchy <- list(
      "block group" = list(parent = "tract",   key_len = 11),
      "tract"       = list(parent = "county",  key_len = 5),
      "county"      = list(parent = "state",   key_len = 2)
    )

    geo_parent         <- geo_hierarchy[[geo]]$parent
    geo_parent_key_len <- geo_hierarchy[[geo]]$key_len

    yost_data_parent       <- fetchYostAcs(geo = geo_parent, year = year, states = states, get_geometry = FALSE)
    yost_data_parent_clean <- cleanAcsNames(dframe = yost_data_parent, geo = geo_parent)
    yost_data_parent       <- calculateYostVars(data = yost_data_parent_clean, acs_vars = acs_vars, shrink_sub = FALSE)

    yost_data_shrunk_raw <- shrinkYost(yost_data = yost_data_sub, yost_data_parent = yost_data_parent,
                                       varlist = varlist, geo_parent_key_len = geo_parent_key_len)

    # Rename: original -> _raw, _moe -> _raw_moe, _shrunk -> original name
    colnames(yost_data_shrunk_raw)[colnames(yost_data_shrunk_raw) %in% varlist] <- paste0(varlist, "_raw")
    colnames(yost_data_shrunk_raw)[colnames(yost_data_shrunk_raw) %in% paste0(varlist, "_moe")] <- paste0(varlist, "_raw_moe")
    colnames(yost_data_shrunk_raw)[colnames(yost_data_shrunk_raw) %in% paste0(varlist, "_shrunk")] <- varlist

    # Restore zero-population rows
    raw_yost_data_sub <- dplyr::bind_rows(
      yost_data_shrunk_raw,
      raw_yost_data_sub |> dplyr::filter(tot_pop == 0)
    )

    yost_input_data <- yost_data_shrunk_raw |>
      dplyr::select(GEOID, tot_pop, income, wkcls, unemp, educ,
                    poverty150, rent, hval, dplyr::any_of("geometry"))
  } else {
    yost_input_data <- yost_data_sub |>
      dplyr::select(GEOID, tot_pop, income, wkcls, unemp, educ,
                    poverty150, rent, hval, dplyr::any_of("geometry"))
  }

  max_missing = yost_input_data %>% select(-GEOID) %>% sf::st_drop_geometry() %>% is.na() %>% rowSums() %>% max()

  if (impute_sub & max_missing>0) { ## Impute --------
    if (!quiet_sub) message(glue::glue("... imputing missing data."))

    if(weight_var_sub=='tot_pop') message(glue::glue("... imputation will be weighted by total population."))
    if(weight_var_sub=='none') message(glue::glue("... imputation will average the neighbors."))

    yost_input_data <- imputeMissing(
      dfGeo = yost_input_data,
      weight_var_sub = weight_var_sub,
      quiet = quiet_sub
    )

  }  else { ## Complete Cases only --------

    vars_yost <- setdiff(
      colnames(yost_input_data), c("GEOID", "impute","tot_pop", "has_geometry")
    )

    yost_input_data <- yost_input_data |>
      dplyr::mutate(
        nvar_still_missing = rowSums(
          is.na(dplyr::across(dplyr::all_of(vars_yost))), na.rm = FALSE
        ),
        nvar_imputed=0
      )
  }


  # Step 3: Harmonizing (Rescaling) and Filtering - Only keep complete rows from now on.
  yost_inp_data_fa <- yost_input_data |>
    dplyr::filter(nvar_still_missing==0) |>
    dplyr::select(GEOID, income, wkcls, unemp, educ, poverty150, rent, hval) %>% 
    sf::st_drop_geometry()

  if (rescale_sub == 'rank') {
    if (!quiet_sub) message(glue::glue("... harmonizing geoIDs by {rescale_sub}."))
    yost_inp_data_fa <- yost_inp_data_fa |>
      dplyr::mutate(dplyr::across(-GEOID, ~rank(.), .names = "rank_{.col}")) |>
      dplyr::select(GEOID, dplyr::starts_with("rank_"))
  }

  if (rescale_sub == 'standardize'){
    if (!quiet_sub) message(glue::glue("... harmonizing geoIDs by {rescale_sub}."))
    yost_inp_data_fa <- yost_inp_data_fa |>
      dplyr::mutate(dplyr::across(-GEOID, ~scale(.)[, 1], .names = "std_{.col}")) |>
      dplyr::select(GEOID, dplyr::starts_with("std_"))
  }


  # ====================================================================
  # === NEW SECTION: Step 4: Factor Analysis with Error Handling ===
  # ====================================================================

  # Get metadata for our warning message
  current_scope <- "Current Scope" # Default
  if ("state" %in% colnames(raw_yost_data_sub)) {
    # Get the state name from the *original* data for this scope
    current_scope <- unique(raw_yost_data_sub$state)[1]
  }

  n_obs <- nrow(yost_inp_data_fa)
  # -1 for GEOID column
  n_vars <- ncol(yost_inp_data_fa) - 1

  yost_fa <- NULL # Initialize as NULL

  # 1. Pre-check: Skip if not enough observations
  if (n_obs < n_vars + 2) { # Rule of thumb: need more obs than vars
    if (!quiet_sub) {
      warning(glue::glue(
        "Skipping factor analysis for '{current_scope}': ",
        "Insufficient complete observations ({n_obs}) for {n_vars} variables."
      ))
    }
  } else {
    # 2. tryCatch: Attempt the factor analysis
    set.seed(5757)
    yost_fa <- tryCatch({
      psych::fa(
        r = yost_inp_data_fa |> dplyr::select(-GEOID),
        rotate = "oblimin", fm = "ml", nfactors = nfactors_sub,
        n.obs = n_obs # Explicitly tell FA how many obs we have
      )
    }, error = function(e) {
      # This is your custom message!
      if (!quiet_sub) {
        warning(glue::glue(
          "Factor analysis failed for '{current_scope}' ",
          "which had {n_obs} complete observations. ",
          "Error: {e$message}"
        ))
      }
      return(NULL) # Return NULL on failure
    })
  }

  # 3. Post-check: Handle the result (success or failure)
  if (is.null(yost_fa)) {
    # FA failed or was skipped, so Yost score is NA
    yost_inp_data_fa$Yost <- NA_real_

  } else {
    # FA succeeded, calculate Yost score
    yost_inp_data_fa$Yost <- yost_fa$scores[, 1]

    ## making sure the factor aligns with the income.
    pos_income <- grepl('income', rownames(yost_fa$loadings)) |> which()
    factor_sign <- ifelse(yost_fa$loadings[pos_income] < 0, -1, 1)
    yost_inp_data_fa <- yost_inp_data_fa |>
      dplyr::mutate(Yost = 10 + Yost * factor_sign)
  }

  cols_from_raw = setdiff(colnames(raw_yost_data_sub),colnames(yost_input_data))

  # Step 5: Preparing output
  yost_output_data = raw_yost_data_sub |>
    dplyr::left_join(
      yost_inp_data_fa, by = "GEOID"
    ) |>
    dplyr::left_join(
      yost_input_data |>
        dplyr::select(GEOID, nvar_still_missing, nvar_imputed) |> sf::st_drop_geometry(), by = "GEOID"
    )

  # Quintiles
  yost_output_data <- yost_output_data |>
    dplyr::mutate(
      YostQuintile = dplyr::ntile(Yost, 5),
      YostQuintile = factor(YostQuintile, levels = 1:5)
    )

  ## Annotating GEOIDs
  yost_output_data = yost_output_data |>
    dplyr::mutate(
      annotation = dplyr::case_when(
        tot_pop == 0 ~ 'No population',
        !is.na(Yost) & nvar_imputed == 0 & nvar_still_missing == 0 ~ "Complete data",
        !is.na(Yost) & nvar_imputed >= 0 & nvar_still_missing == 0 ~ "Imputation completed",
        is.na(Yost) & nvar_imputed >= 0 & nvar_still_missing >  0 ~ "Imputation incomplete",
        is.na(Yost) & nvar_still_missing == 0 ~ "Factor analysis failed",
        TRUE ~ 'Other'
      )
    )

  # ====================================================================
  # === FINAL DATA FRAME EXTRACTIONS ===
  # ====================================================================

  # 1. df_yost (Drop geometry)
  df_yost <- yost_output_data |>
    dplyr::select(GEOID, Yost, YostQuintile, annotation)

  if (inherits(df_yost, "sf")) {
    df_yost <- sf::st_drop_geometry(df_yost)
  }

  # 2. df_raw_values (Drop geometry to keep it tabular)
  df_raw_values <- raw_yost_data_sub
  if (inherits(df_raw_values, "sf")) {
    df_raw_values <- sf::st_drop_geometry(df_raw_values)
  }

  # 3. df_geometry
  df_geometry <- raw_yost_data_sub |>
    dplyr::select(GEOID, dplyr::any_of("geometry"))

  # 4. df_imputed (Drop geometry if it carried over)
  df_imputed <- yost_input_data
  if (inherits(df_imputed, "sf")) {
    df_imputed <- sf::st_drop_geometry(df_imputed)
  }

  # 5. df_rank (Extract columns without the calculated Yost score, drop geometry)
  df_rank <- yost_inp_data_fa |>
    dplyr::select(-dplyr::any_of("Yost"))

  if (inherits(df_rank, "sf")) {
    df_rank <- sf::st_drop_geometry(df_rank)
  }

  return(list(
    df_yost = df_yost,
    df_raw_values = df_raw_values,
    df_geometry = df_geometry,
    df_imputed = df_imputed,
    df_rank = df_rank,
    fa_object = yost_fa
  ))
}
