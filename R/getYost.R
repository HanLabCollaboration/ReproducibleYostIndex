#' Get pre-computed Yost Index values
#'
#' @description
#' A fast, Census-API-free alternative to \code{\link{computeYostIndex}} that
#' retrieves pre-computed Yost Index values directly from the
#' \href{https://github.com/HanLabCollaboration/ReproduceYost-data}{ReproduceYost-data}
#' GitHub repository.
#'
#' Pre-computed files cover \strong{county}, \strong{tract}, and
#' \strong{block group} geographies for ACS years 2011–2023 (block groups
#' from 2013 onwards), under \strong{national} and \strong{state} scopes, for
#' all four shrink/impute configurations. Note: \code{scope = "county"} is
#' not available in the pre-computed files; use \code{\link{computeYostIndex}}
#' instead.
#'
#' Files are cached locally after the first download so subsequent calls with
#' the same arguments are instantaneous.
#'
#' @param geo Geography level. One of \code{"county"}, \code{"tract"},
#'   \code{"block group"} (or its alias \code{"cbg"}).
#' @param year ACS 5-year estimate year (2013–2023).
#' @param states Optional character vector of state abbreviations
#'   (e.g. \code{c("CA", "NY")}) to subset the results. \code{NULL} (default)
#'   returns all US states.
#' @param scope \code{"national"} (default) or \code{"state"}. Determines
#'   whether quintiles were computed across all US geographies
#'   (\code{"national"}) or within each state separately (\code{"state"}).
#' @param cache Logical. If \code{TRUE} (default), saves the downloaded file
#'   to the user-level R cache directory
#'   (\code{tools::R_user_dir("ReproduceYostIndex", "cache")}) so the download is
#'   skipped on subsequent calls.
#' @param quiet Logical. If \code{TRUE}, suppresses all messages.
#'   Defaults to \code{FALSE}.
#'
#' @return A tibble with columns \code{GEOID}, \code{year}, \code{geo},
#' \code{scope}, \code{Yost}, \code{YostQuintile}, \code{YostStabilized},
#' \code{YostStabilizedQuintile}, \code{YostImputed}, \code{YostImputedQuintile},
#' \code{YostStabilizedImputed}, and \code{YostStabilizedImputedQuintile}, sorted
#' by \code{GEOID}.
#'
#' @seealso \code{\link{computeYostIndex}} for the full pipeline when
#'   pre-computed data are unavailable (e.g. very recent ACS years, custom
#'   geographies, or \code{scope = "county"}).
#'
#' @importFrom dplyr filter pull arrange
#' @importFrom readr read_csv
#' @importFrom glue glue glue_collapse
#' @importFrom rlang arg_match
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # All Yost variants for US counties, 2022 (no Census API needed)
#'   county_yost <- getYost(geo = "county", year = 2022)
#'
#'   # State-scope tracts for California and New York
#'   tracts <- getYost(
#'     geo    = "tract",
#'     year   = 2022,
#'     states = c("CA", "NY"),
#'     scope  = "state"
#'   )
#'
#'   # Block groups, national scope
#'   bg <- getYost(geo = "block group", year = 2021)
#' }

getYostIndex <- function(
    geo    = "tract",
    year   = 2022,
    states = NULL,
    scope  = "national",
    cache  = TRUE,
    quiet  = FALSE
) {

  # --- 1. Argument validation --------------------------------------------------

  geo   <- rlang::arg_match(geo,   values = c("county", "tract", "block group", "cbg"))
  scope <- rlang::arg_match(scope, values = c("national", "state"))

  if (geo == "cbg") geo <- "block group"

  stopifnot(is.logical(cache), is.logical(quiet))
  stopifnot(is.numeric(year))

  year_min <- 2013L
  year_max <- 2023L

  if (year < year_min) {
    stop(glue::glue(
      "Pre-computed data starts at year {year_min} (got year = {year}). ",
      "Use computeYostIndex() for 2011\u20132012."
    ))
  }

  if (year > year_max) {
    stop(glue::glue(
      "Pre-computed data is available through year {year_max}. ",
      "Got year = {year}. Use computeYostIndex() to run the full pipeline, ",
      "or check https://github.com/HanLabCollaboration/ReproduceYost-data/releases ",
      "for a newer release."
    ))
  }

  if (!is.null(states)) {
    valid_states <- unique(tidycensus::fips_codes$state)
    invalid      <- setdiff(states, valid_states)
    if (length(invalid) > 0) {
      stop(glue::glue(
        "Invalid state abbreviation(s): {glue::glue_collapse(invalid, sep = ', ')}. ",
        "Please use standard 2-letter postal codes."
      ))
    }
  }

  # --- 2. Build URL and local cache path ---------------------------------------

  data_tag   <- "data-v2026.04"
  geo_tag    <- gsub(" ", "_", geo)
  filename   <- sprintf("yost_%s_%s_%d.csv.gz", geo_tag, scope, year)
  url        <- sprintf(
    "https://github.com/HanLabCollaboration/ReproduceYost-data/releases/download/%s/%s",
    data_tag, filename
  )
  cache_dir  <- tools::R_user_dir("ReproduceYostIndex", which = "cache")
  local_path <- file.path(cache_dir, data_tag, filename)

  # --- 3. Download (skip if cached) --------------------------------------------

  if (!file.exists(local_path) || !cache) {
    dir.create(dirname(local_path), showWarnings = FALSE, recursive = TRUE)
    if (!quiet) message(glue::glue("Downloading {filename}..."))
    tryCatch(
      utils::download.file(url, destfile = local_path, quiet = quiet, mode = "wb"),
      error = function(e) stop(glue::glue(
        "Failed to download '{filename}'. Check your internet connection or ",
        "visit https://github.com/HanLabCollaboration/ReproduceYost-data/releases ",
        "to verify the file exists."
      ))
    )
  } else {
    if (!quiet) message(glue::glue("Using cached {filename}."))
  }

  # --- 4. Read -----------------------------------------------------------------

  df <- readr::read_csv(local_path, show_col_types = FALSE, progress = FALSE)

  # --- 4b. Translate legacy "Shrunk" column names to "Stabilized" --------------
  col_map <- c(
    YostShrunk                = "YostStabilized",
    YostShrunkQuintile        = "YostStabilizedQuintile",
    YostShrunkImputed         = "YostStabilizedImputed",
    YostShrunkImputedQuintile = "YostStabilizedImputedQuintile"
  )
  to_rename <- intersect(names(col_map), names(df))
  if (length(to_rename) > 0) {
    names(df)[match(to_rename, names(df))] <- col_map[to_rename]
  }

  # --- 5. Filter by states (first 2 chars of GEOID = state FIPS) ---------------

  if (!is.null(states)) {
    state_fips <- tidycensus::fips_codes |>
      dplyr::filter(state %in% states) |>
      dplyr::pull(state_code) |>
      unique()
    df <- df |> dplyr::filter(substr(GEOID, 1, 2) %in% state_fips)
  }

  # --- 6. Return ---------------------------------------------------------------

  dplyr::arrange(df, GEOID)
}
