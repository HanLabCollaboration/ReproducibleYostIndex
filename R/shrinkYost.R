#' Shrink Yost Variables Using Parent-Level Information
#'
#' Applies a shrinkage estimator to Yost variables by borrowing strength from
#' higher-level (parent) geographic units. The method combines lower-level
#' estimates with parent-level estimates using a data-adaptive weight based on
#' within-parent heterogeneity and sampling variance derived from margins of
#' error (MOE).
#'
#' @param yost_data A data frame containing lower-level geographic units
#'   (e.g., block groups), including a `GEOID` column, Yost variables, and their
#'   corresponding MOEs (named as `var_moe`).
#' @param yost_data_parent A data frame containing parent-level geographic units
#'   (e.g., tracts), including a `GEOID` column and the same Yost variables
#'   specified in `varlist`.
#' @param varlist A character vector of variable names to be shrunk.
#' @param geo_parent_key_len Integer. Number of characters to truncate `GEOID`
#'   to when linking lower-level units to their parent geography.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Links lower-level units to their parent units via truncated `GEOID`.
#'   \item Computes within-parent heterogeneity:
#'   \deqn{t^2 = \frac{\sum (x_i - x_{\text{parent}})^2}{n - 1}}
#'   \item Converts margin of error (MOE) to variance:
#'   \deqn{s^2 = \left(\frac{\text{MOE}}{1.645}\right)^2}
#'   \item Computes shrinkage weight:
#'   \deqn{w = \frac{t^2}{s^2 + t^2}}
#'   \item Produces shrunk estimates:
#'   \deqn{x_{\text{shrunk}} = w \cdot x + (1 - w) \cdot x_{\text{parent}}}
#' }
#'
#' Shrinkage is applied only when both variance and heterogeneity are positive
#' and when multiple lower-level units exist within a parent unit. Otherwise,
#' the original estimate is retained.
#'
#' @return
#' A data frame with the original variables plus:
#' \itemize{
#'   \item `{var}_parent`: Parent-level values
#'   \item `{var}_sqdiff`: Sum of squared differences within parent
#'   \item `{var}_s2`: Estimated sampling variance from MOE
#'   \item `{var}_wgt`: Shrinkage weight
#'   \item `{var}_shrunk`: Shrunk estimate
#'   \item `shrunkflg_{var}`: Indicator (1 = shrinkage applied, 0 = not applied)
#' }
#'
#' @examples
#' \dontrun{
#' shrunk_data <- shrinkYost(
#'   yost_data = bg_data,
#'   yost_data_parent = tract_data,
#'   varlist = c("income", "education", "employment"),
#'   geo_parent_key_len = 11
#' )
#' }
#'
#' @seealso
#' Empirical Bayes and small area estimation methods for related shrinkage approaches.
#'
#' @export

shrinkYost <- function(yost_data, yost_data_parent, varlist, geo_parent_key_len) {

  # --- Joint dataset (lower + upper)
  yost_data_full <- yost_data |>
    dplyr::mutate(join_key = stringr::str_sub(GEOID, 1, geo_parent_key_len)) |>
    dplyr::left_join(
      yost_data_parent |>
        dplyr::select(GEOID, dplyr::all_of(varlist)) |>
        dplyr::rename_with(~paste0(.x, "_parent"), .cols = dplyr::all_of(varlist)),
      by = c("join_key" = "GEOID")
    )

  # --- Compute within-parent heterogeneity: squared differences and group counts
  yost_data_full_shrunk <- yost_data_full |>
    dplyr::group_by(join_key) |>
    dplyr::mutate(
      count = dplyr::n(),
      dplyr::across(
        .cols = dplyr::all_of(varlist),
        .fns = ~ sum((.x - get(paste0(dplyr::cur_column(), "_parent")))^2, na.rm = TRUE),
        .names = "{.col}_sqdiff"
      )
    ) |>
    dplyr::ungroup()

  # --- Shrinkage loop over each variable
  for (var in varlist) {
    moe_col    <- paste0(var, "_moe")
    parent_col <- paste0(var, "_parent")
    sqdiff_col <- paste0(var, "_sqdiff")
    s2_col     <- paste0(var, "_s2")
    wgt_col    <- paste0(var, "_wgt")
    shrunk_col <- paste0(var, "_shrunk")
    flag_col   <- paste0("shrunkflg_", var)

    yost_data_full_shrunk <- yost_data_full_shrunk |>
      dplyr::mutate(
        !!shrunk_col := .data[[var]],
        !!flag_col   := 0L,
        s2_tmp = (.data[[moe_col]] / 1.645)^2,      # s2 = (MOE / 1.645)^2
        t2_tmp = .data[[sqdiff_col]] / (count - 1), # t2 = within-parent heterogeneity
        !!s2_col  := s2_tmp,
        !!wgt_col := t2_tmp / (s2_tmp + t2_tmp)
      ) |>
      dplyr::mutate(
        !!shrunk_col := dplyr::case_when(
          is.na(.data[[var]]) | is.na(.data[[parent_col]]) ~ .data[[var]],
          is.na(.data[[moe_col]])                          ~ .data[[var]],
          count == 1                                       ~ .data[[var]],
          s2_tmp > 0 & t2_tmp > 0 ~ {
            w <- t2_tmp / (s2_tmp + t2_tmp)
            (w * .data[[var]]) + ((1 - w) * .data[[parent_col]])
          },
          TRUE ~ .data[[var]]
        ),
        !!flag_col := dplyr::if_else(s2_tmp > 0 & t2_tmp > 0 & count > 1, 1L, 0L)
      ) |>
      dplyr::select(-s2_tmp, -t2_tmp)
  }

  return(yost_data_full_shrunk)
}
