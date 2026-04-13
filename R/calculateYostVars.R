#' Extract MOE for a Single ACS Variable
#'
#' @description
#' Internal helper to extract the margin of error (MOE) for a single ACS variable,
#' assuming the standard `"M"` suffix naming convention.
#'
#' @param data A data frame containing ACS variables.
#' @param var A character string of the variable name (without suffix).
#'
#' @return A numeric vector of MOE values.

moe1 = function(data, var) {
  var_base <- sub("E$", "", var)   # strip trailing E if already suffixed
  data[[paste0(var_base, 'M')]]
}


#' Compute MOE for Derived Ratios
#'
#' @description
#' Internal helper to approximate the margin of error (MOE) for ratio-based
#' variables (e.g., proportions) using ACS estimates and MOEs.
#'
#' @param data A data frame containing ACS variables.
#' @param num A character vector of numerator variable names (without suffix).
#' @param denom A character string of the denominator variable name (without suffix).
#'
#' @return A numeric vector of MOE values for the derived ratio.
#'
#' @details
#' When multiple numerator variables are provided, their MOEs are combined
#' assuming independence. If the denominator is zero, `NA` is returned.

moe3 <- function(data, num, denom) {
  num_base   <- sub("E$", "", num)    # strip trailing E if already suffixed
  denom_base <- sub("E$", "", denom)
  num_E   <- paste0(num_base, "E")
  num_M   <- paste0(num_base, "M")
  denom_E <- paste0(denom_base, "E")
  denom_M <- paste0(denom_base, "M")

  data_moe <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # numerator and denominator sums
      num_sum = sum(dplyr::c_across(all_of(num_E)), na.rm = TRUE),
      denom_sum = sum(dplyr::c_across(all_of(denom_E)), na.rm = TRUE),

      # ratio estimate
      ratio_est = ifelse(denom_sum != 0, num_sum / denom_sum, 0),

      n_moe = if(length(num) > 1) sqrt(sum((dplyr::c_across(all_of(num_M)))^2, na.rm = TRUE)) else dplyr::c_across(all_of(num_M)),
      d_moe = dplyr::c_across(all_of(denom_M)),

      # Refer to CDI guidelines - prevent negative variance (theoretically x_sub is correct)
      x_sub = n_moe^2 - ((ratio_est)^2 * d_moe^2),
      x_add = n_moe^2 + ((ratio_est)^2 * d_moe^2),

      out_moe = ifelse(
        denom_sum != 0,
        ifelse(x_sub > 0,
               sqrt(x_sub) / denom_sum,
               sqrt(x_add) / denom_sum),
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(GEOID, out_moe)

  return(data_moe$out_moe)
}


#' Calculate Yost Index component variables
#'
#' @description
#' This internal helper function calculates the 7 component variables
#' for the Yost Index from the raw ACS data.
#'
#' @param data The data frame from `clean_acs_names`.
#' @param acs_vars The list of ACS variable names from the attribute.
#' @param shrink_sub Logical. If `TRUE`, also compute MOE columns for each variable.
#'
#' @return A data frame with the new component variables.
#'
#' @importFrom dplyr mutate across all_of where select

calculateYostVars <- function(data, acs_vars, shrink_sub = FALSE) {
  data_calculated <- data |>
    dplyr::mutate(
      tot_pop = .data[[acs_vars$var_POPTOT]],
      income = .data[[acs_vars$var_MHHINC]],
      wkcls = rowSums(dplyr::across(dplyr::all_of(acs_vars$var_WKCLS))) / .data[[acs_vars$var_WKCLS_pop]],
      unemp = (.data[[acs_vars$var_UNEMP]] / .data[[acs_vars$var_UNEMP_pop]]) * 100,
      edu1 = (rowSums(dplyr::across(dplyr::all_of(acs_vars$var_Edu_P1))) / .data[[acs_vars$var_Edu_denom]]) * 100,
      edu2 = (rowSums(dplyr::across(dplyr::all_of(acs_vars$var_Edu_P2))) / .data[[acs_vars$var_Edu_denom]]) * 100,
      edu3 = (rowSums(dplyr::across(dplyr::all_of(acs_vars$var_Edu_P3))) / .data[[acs_vars$var_Edu_denom]]) * 100,
      educ = 16 * edu3 + 12 * edu2 + 9 * edu1,
      poverty150 = (rowSums(dplyr::across(dplyr::all_of(acs_vars$var_RATIOP))) / .data[[acs_vars$var_RATIOP_pop]]) * 100,
      rent = .data[[acs_vars$var_MGRRNT]],
      hval = .data[[acs_vars$var_MVALUE]]
    )

  # Conditionally add MOE variables
  if (shrink_sub) {
    data_calculated <- data_calculated |>
      dplyr::mutate(
        tot_pop_moe  = moe1(data = data, var = acs_vars$var_POPTOT),
        income_moe   = moe1(data = data, var = acs_vars$var_MHHINC),
        wkcls_moe    = moe3(data = data, num = acs_vars$var_WKCLS,    denom = acs_vars$var_WKCLS_pop),
        unemp_moe    = moe3(data = data, num = acs_vars$var_UNEMP,    denom = acs_vars$var_UNEMP_pop) * 100,
        edu1_moe     = moe3(data = data, num = acs_vars$var_Edu_P1,   denom = acs_vars$var_Edu_denom) * 100,
        edu2_moe     = moe3(data = data, num = acs_vars$var_Edu_P2,   denom = acs_vars$var_Edu_denom) * 100,
        edu3_moe     = moe3(data = data, num = acs_vars$var_Edu_P3,   denom = acs_vars$var_Edu_denom) * 100,
        educ_moe     = sqrt((16 * edu1_moe)^2 + (12 * edu2_moe)^2 + (9 * edu3_moe)^2),
        poverty150_moe = moe3(data = data, num = acs_vars$var_RATIOP, denom = acs_vars$var_RATIOP_pop) * 100,
        rent_moe     = moe1(data = data, var = acs_vars$var_MGRRNT),
        hval_moe     = moe1(data = data, var = acs_vars$var_MVALUE)
      )
  }

  data_calculated_out <- data_calculated |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.nan(.), NA, .))) |>
    dplyr::select(-edu1, -edu2, -edu3, dplyr::any_of(c("edu1_moe", "edu2_moe", "edu3_moe")))

  return(data_calculated_out)
}
