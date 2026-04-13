#' Clean ACS geographic names
#'
#' @description
#' This internal helper function splits the 'NAME' column from tidycensus
#' into clean columns for block group, tract, county, and state.
#'
#' @param dframe The raw data from `fetchYostAcs`.
#' @param geo The geography level.
#'
#' @return A data frame with cleaned name columns.
#'
#' @importFrom tidyr separate
#' @importFrom dplyr mutate across all_of any_of
#' @importFrom stringr str_squish str_extract str_remove

cleanAcsNames <- function(dframe, geo) {

  geo_norm <- ifelse(geo == 'cbg', 'block group', geo)

  # Added 'state' to support the geo options in computeYostIndex
  sep_cols <- switch(
    geo_norm,
    'block group' = c("block_group", "tract", "county", "state"),
    "tract"       = c("tract", "county", "state"),
    "county"      = c("county", "state"),
    "state"       = c("state"),
    stop("Unknown geo type: ", geo_norm)
  )

  # If the level is just 'state', we don't need to separate, just map NAME
  if (geo_norm == "state") {
    data_cleaned <- dframe |>
      dplyr::mutate(state = NAME)
  } else {
    data_cleaned <- dframe |>
      tidyr::separate(
        col    = NAME,
        into   = sep_cols,
        sep    = "; |, ",
        remove = FALSE,
        fill   = "right",
        extra  = "merge"
      )
  }

  data_cleaned <- data_cleaned |>
    dplyr::mutate(
      # Trim whitespace from all extracted pieces
      dplyr::across(
        .cols = dplyr::all_of(sep_cols),
        .fns  = stringr::str_squish
      ),
      # Pull out only digits (and decimal dot if needed) for block_group & tract
      dplyr::across(
        .cols = dplyr::any_of(c("block_group", "tract")),
        .fns  = ~ stringr::str_extract(.x, "\\d+\\.?\\d*")
      )
    )

  # Drop trailing administrative area words from county names if 'county' is present
  if ("county" %in% sep_cols) {
    data_cleaned <- data_cleaned |>
      dplyr::mutate(
        county = stringr::str_remove(county, "\\s*(County|Parish|Borough|Municipio|Census Area|Municipality)$")
      )
  }

  return(data_cleaned)
}
