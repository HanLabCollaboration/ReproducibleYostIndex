#' Get pre-computed Yost Index values
#'
#' @description
#' A fast, Census-API-free alternative to \code{\link{computeYostIndex}} that
#' retrieves pre-computed Yost Index values directly from the
#' \href{https://github.com/HanLabCollaboration/ReproducibleYostIndex-data}{ReproducibleYostIndex-data}
#' GitHub repository.
#'
#' Pre-computed files cover \strong{county}, \strong{tract}, and
#' \strong{block group} geographies for ACS years 2011–2024 (block groups
#' from 2013 onwards), under \strong{national} and \strong{state} scopes, for
#' all four shrink/impute configurations. Note: \code{scope = "county"} is
#' not available in the pre-computed files; use \code{\link{computeYostIndex}}
#' instead.
#'
#' Files are cached locally after the first download so subsequent calls with
#' the same arguments are instantaneous.
#'
#' Coverage is updated annually. Data for a given year is added once the
#' US Census Bureau releases the corresponding ACS 5-year estimates
#' (typically in December of the following year); until then, the data
#' cannot be produced by any tool in this package.
#'
#' @param geo Geography level. One of \code{"county"}, \code{"tract"},
#'   \code{"block group"} (or its alias \code{"cbg"}).
#' @param year ACS 5-year estimate year. Available from 2011 for
#'   \code{"state"}, \code{"county"}, and \code{"tract"}, and from 2013 for
#'   \code{"block group"}, through 2024. Earlier years are not available
#'   because the underlying ACS 5-year estimates do not cover them at these
#'   geographies.
#' @param states Optional character vector of state abbreviations
#'   (e.g. \code{c("CA", "NY")}) to subset the results. \code{NULL} (default)
#'   returns all US states.
#' @param scope \code{"national"} (default) or \code{"state"}. Determines
#'   whether quintiles were computed across all US geographies
#'   (\code{"national"}) or within each state separately (\code{"state"}).
#' @param geometry Logical. If \code{TRUE}, downloads and attaches the
#'   pre-computed GeoPackage for the requested \code{geo} and \code{year},
#'   returning an \code{sf} object. The geometry file is cached independently
#'   of the CSV. Defaults to \code{FALSE}.
#' @param cache Logical. If \code{TRUE} (default), saves the downloaded file
#'   to the user-level R cache directory
#'   (\code{tools::R_user_dir("ReproducibleYostIndex", "cache")}) so the download is
#'   skipped on subsequent calls.
#' @param quiet Logical. If \code{TRUE}, suppresses all messages.
#'   Defaults to \code{FALSE}.
#'
#' @return When \code{geometry = FALSE} (default), a tibble with columns
#' \code{GEOID}, \code{year}, \code{geo}, \code{scope}, \code{Yost},
#' \code{YostQuintile}, \code{YostStabilized}, \code{YostStabilizedQuintile},
#' \code{YostImputed}, \code{YostImputedQuintile}, \code{YostStabilizedImputed},
#' and \code{YostStabilizedImputedQuintile}, sorted by \code{GEOID}.
#' When \code{geometry = TRUE}, an \code{sf} object with the same columns plus
#' a \code{geometry} column (WGS 84 / EPSG:4326).
#'
#' @seealso \code{\link{computeYostIndex}} for the full pipeline when you
#'   need a custom geography or \code{scope = "county"} (which is not
#'   included in the pre-computed files).
#'
#' @importFrom dplyr filter pull arrange left_join select
#' @importFrom readr read_csv
#' @importFrom glue glue glue_collapse
#' @importFrom rlang arg_match
#' @importFrom sf st_read
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # All Yost variants for US counties, 2022 (no Census API needed)
#'   county_yost <- getYostIndex(geo = "county", year = 2022)
#'
#'   # State-scope tracts for California and New York
#'   tracts <- getYostIndex(
#'     geo    = "tract",
#'     year   = 2022,
#'     states = c("CA", "NY"),
#'     scope  = "state"
#'   )
#'
#'   # Block groups, national scope, with geometry for mapping
#'   bg <- getYostIndex(geo = "block group", year = 2021, geometry = TRUE)
#'
#'   # State-level Yost
#'   states_yost <- getYostIndex(geo = "state", year = 2022)
#' }

getYostIndex <- function(
    geo      = "tract",
    year     = 2022,
    states   = NULL,
    scope    = "national",
    geometry = FALSE,
    cache    = TRUE,
    quiet    = FALSE
) {

  # --- 1. Argument validation --------------------------------------------------

  geo   <- rlang::arg_match(geo,   values = c("state", "county", "tract", "block group", "cbg"))
  scope <- rlang::arg_match(scope, values = c("national", "state"))

  if (geo == "cbg") geo <- "block group"

  stopifnot(is.logical(geometry), is.logical(cache), is.logical(quiet))
  stopifnot(is.numeric(year))

  year_min <- if (geo == "block group") 2013L else 2011L
  year_max <- 2024L

  if (year < year_min) {
    stop(glue::glue(
      "Yost Index data for '{geo}' is not available for year {year}. ",
      "The ACS 5-year estimates underlying the Yost Index only cover ",
      "2011 onwards for tracts (and state/county) and 2013 onwards for ",
      "block groups, so earlier years cannot be produced by either ",
      "getYostIndex() or computeYostIndex()."
    ))
  }

  if (year > year_max) {
    stop(glue::glue(
      "Pre-computed data is available through year {year_max} (got year = {year}). ",
      "This year may not yet be released. ",
      "Please check https://github.com/HanLabCollaboration/ReproducibleYostIndex-data/releases ",
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

  # --- 2. Build URLs and local cache paths -------------------------------------

  # Resolve the current "Latest" release tag from GitHub by following the
  # /releases/latest redirect. Used only to namespace the local cache so a
  # new release invalidates stale files. Falls back to "latest" if offline.
  resolve_latest_tag <- function() {
    url <- "https://github.com/HanLabCollaboration/ReproducibleYostIndex-data/releases/latest"
    resp <- tryCatch(
      httr2::request(url) |>
        httr2::req_method("HEAD") |>
        httr2::req_options(followlocation = FALSE) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform(),
      error = function(e) NULL
    )
    if (is.null(resp)) return("latest")
    loc <- httr2::resp_header(resp, "location")
    if (is.null(loc) || !nzchar(loc)) return("latest")
    tag <- sub(".*/tag/([^[:space:]\r\n]+).*", "\\1", loc)
    if (!nzchar(tag)) "latest" else tag
  }

  data_tag  <- resolve_latest_tag()
  geo_tag   <- gsub(" ", "_", geo)
  cache_dir <- tools::R_user_dir("ReproducibleYostIndex", which = "cache")

  # CSV — always pulled from /releases/latest/download/ so users get the
  # current release without bumping the package.
  csv_file   <- sprintf("yost_%s_%s_%d.csv.gz", geo_tag, scope, year)
  csv_url    <- sprintf(
    "https://github.com/HanLabCollaboration/ReproducibleYostIndex-data/releases/latest/download/%s",
    csv_file
  )
  csv_path   <- file.path(cache_dir, data_tag, csv_file)

  # GeoPackage (scope-independent: geometry is the same for national and state)
  gpkg_file  <- sprintf("yost_%s_%d.gpkg", geo_tag, year)
  gpkg_url   <- sprintf(
    "https://github.com/HanLabCollaboration/ReproducibleYostIndex-data/releases/latest/download/%s",
    gpkg_file
  )
  gpkg_path  <- file.path(cache_dir, data_tag, gpkg_file)

  # --- 3. Download helpers -----------------------------------------------------

  download_asset <- function(url, local_path, label) {
    if (!file.exists(local_path) || !cache) {
      dir.create(dirname(local_path), showWarnings = FALSE, recursive = TRUE)
      if (!quiet) message(glue::glue("Downloading {label}..."))
      tryCatch(
        utils::download.file(url, destfile = local_path, quiet = quiet, mode = "wb"),
        error = function(e) stop(glue::glue(
          "Failed to download '{label}'. Check your internet connection or ",
          "visit https://github.com/HanLabCollaboration/ReproducibleYostIndex-data/releases ",
          "to verify the file exists."
        ))
      )
    } else {
      if (!quiet) message(glue::glue("Using cached {label}."))
    }
  }

  # --- 4. Download and read CSV ------------------------------------------------

  download_asset(csv_url, csv_path, csv_file)
  df <- readr::read_csv(csv_path, show_col_types = FALSE, progress = FALSE)

  # Translate legacy "Shrunk" column names to "Stabilized"
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

  # --- 6. Attach geometry (optional) -------------------------------------------

  if (geometry) {
    download_asset(gpkg_url, gpkg_path, gpkg_file)
    geom_sf <- sf::st_read(gpkg_path, layer = "geometry", quiet = TRUE) |>
      dplyr::select(GEOID)

    # Filter geometry to match any state subset applied above
    if (!is.null(states)) {
      geom_sf <- geom_sf |> dplyr::filter(GEOID %in% df$GEOID)
    }

    df <- geom_sf |>
      dplyr::left_join(df, by = "GEOID")
  }

  # --- 7. Return ---------------------------------------------------------------

  quintile_cols <- grep("Quintile", names(df), value = TRUE)
  df <- dplyr::mutate(df, dplyr::across(
    dplyr::all_of(quintile_cols),
    ~ factor(.x, levels = 1:5, ordered = TRUE)
  ))

  dplyr::arrange(df, GEOID)
}
