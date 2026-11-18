#!/usr/bin/env Rscript
# =============================================================================
# run_yost_batch.R
#
# One SLURM array task = one (geo, scope, year). Runs 3 computeYostIndex()
# configs. The raw/base Yost (no stabilization, no imputation) is NOT run on
# its own: every non-base run already returns it as `df_yost_raw`.
#
#   imputed             stabilize = FALSE, impute = TRUE   -> YostImputed
#   stabilized          stabilize = TRUE,  impute = FALSE  -> YostStabilized
#   stabilized_imputed  stabilize = TRUE,  impute = TRUE   -> YostStabilizedImputed
#   (raw Yost / YostQuintile taken from imputed$df_yost_raw)
#
# Outputs (under $OUTDIR):
#   csvgz/yost_<geo>_<scope>_<year>.csv.gz   wide table (release asset)
#   gpkg/yost_<geo>_<year>.gpkg              GEOID + NAME + geometry (release asset;
#                                            written by the national task only)
#   rds/yost_<geo>_<scope>_<year>_<config>.rds  full detailed output per config
#
# Env vars: SLURM_ARRAY_TASK_ID, OUTDIR, YEAR_MAX, SAVE_GEOMETRY, SAVE_RDS,
#           OVERWRITE, CENSUS_API_KEY (or in ~/.Renviron), TIGRIS_CACHE_DIR
# =============================================================================

suppressPackageStartupMessages({
  library(ReproducibleYostIndex)
  library(dplyr)
  library(tidyr)
  library(sf)
})

options(tigris_use_cache = TRUE)

# --- Settings ----------------------------------------------------------------

OUTDIR        <- Sys.getenv("OUTDIR", unset = "/home/groups/manishad/reproducibleYostIndex-all-possible/output")
CSVGZDIR      <- file.path(OUTDIR, "csvgz")
GPKGDIR       <- file.path(OUTDIR, "gpkg")
RDSDIR        <- file.path(OUTDIR, "rds")
YEAR_MAX      <- as.integer(Sys.getenv("YEAR_MAX", unset = "2024"))
SAVE_GEOMETRY <- as.logical(Sys.getenv("SAVE_GEOMETRY", unset = "TRUE"))
SAVE_RDS      <- as.logical(Sys.getenv("SAVE_RDS", unset = "TRUE"))
OVERWRITE     <- as.logical(Sys.getenv("OVERWRITE", unset = "FALSE"))
API_KEY       <- Sys.getenv("CENSUS_API_KEY", unset = "")

if (nchar(API_KEY) > 0) {
  tidycensus::census_api_key(API_KEY)
} else {
  stop("CENSUS_API_KEY is not set. Add CENSUS_API_KEY=... to ~/.Renviron on the cluster.")
}

for (d in c(CSVGZDIR, GPKGDIR, RDSDIR)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# --- Parameter grid ----------------------------------------------------------
# 80 tasks at YEAR_MAX = 2024:
#   block group: 2013-YEAR_MAX x 2 scopes = 24  (tasks  1-24)
#   county:      2011-YEAR_MAX x 2 scopes = 28  (tasks 25-52)
#   tract:       2011-YEAR_MAX x 2 scopes = 28  (tasks 53-80)

params <- dplyr::bind_rows(
  tidyr::crossing(year = 2011:YEAR_MAX, geo = c("county", "tract"),
                  scope = c("national", "state")),
  tidyr::crossing(year = 2013:YEAR_MAX, geo = "block group",
                  scope = c("national", "state"))
) |>
  dplyr::arrange(geo, scope, year) |>
  dplyr::mutate(job_id = dplyr::row_number())

task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = "1"))

if (task_id == 1) {
  write.csv(params, file.path(OUTDIR, "params_grid.csv"), row.names = FALSE)
}

p <- params[params$job_id == task_id, ]
if (nrow(p) == 0) {
  stop(sprintf("No parameters for job_id = %d (max = %d).", task_id, nrow(params)))
}

geo_tag   <- gsub(" ", "_", p$geo)
file_stem <- sprintf("yost_%s_%s_%d", geo_tag, p$scope, p$year)
gz_path   <- file.path(CSVGZDIR, paste0(file_stem, ".csv.gz"))

cat(sprintf("\n[Job %d / %d] geo=%-12s | scope=%-10s | year=%d\n",
            task_id, nrow(params), p$geo, p$scope, p$year))
cat(sprintf("ReproducibleYostIndex %s (RemoteSha: %s)\n",
            as.character(utils::packageVersion("ReproducibleYostIndex")),
            utils::packageDescription("ReproducibleYostIndex")$RemoteSha %||% "NA"))

# Resume: a task whose CSV is already there is skipped (set OVERWRITE=TRUE to redo).
if (file.exists(gz_path) && !OVERWRITE) {
  cat(sprintf("  Output exists, skipping: %s\n", gz_path))
  quit(save = "no", status = 0)
}

# --- Helper: run one config, retrying on Census/TIGER API failures ----------

run_config <- function(stabilize, impute, max_tries = 4) {
  label <- sprintf("stabilize=%-5s impute=%-5s", stabilize, impute)
  for (attempt in seq_len(max_tries)) {
    cat(sprintf("  Running: %s (attempt %d) ...", label, attempt))
    t0 <- proc.time()
    result <- tryCatch(
      computeYostIndex(
        geo           = p$geo,
        year          = p$year,
        states        = "all",
        scope         = p$scope,
        stabilize     = stabilize,
        impute        = impute,
        keep_geometry = impute,   # geometry is fetched for imputation anyway
        return_format = "detailed",
        quiet         = TRUE
      ),
      error = function(e) e
    )
    if (!inherits(result, "error")) {
      cat(sprintf(" done (%.1fs)\n", (proc.time() - t0)[["elapsed"]]))
      return(result)
    }
    cat(sprintf(" FAILED: %s\n", conditionMessage(result)))
    if (attempt < max_tries) {
      wait <- 60 * 2^(attempt - 1) + stats::runif(1, 0, 60)  # 1, 2, 4 min + jitter
      cat(sprintf("  Retrying in %.0fs\n", wait))
      Sys.sleep(wait)
    }
  }
  stop(sprintf("computeYostIndex failed after %d attempts (%s)", max_tries, label))
}

# --- Run the 3 configs -------------------------------------------------------

r_imputed    <- run_config(stabilize = FALSE, impute = TRUE)   # also carries raw Yost + geometry
r_stabilized <- run_config(stabilize = TRUE,  impute = FALSE)
r_both       <- run_config(stabilize = TRUE,  impute = TRUE)

# --- Wide table --------------------------------------------------------------

df_out <- r_imputed$df_yost_raw |>
  dplyr::select(GEOID, Yost, YostQuintile) |>
  dplyr::left_join(r_imputed$df_raw_values |> dplyr::select(GEOID, NAME),
                   by = "GEOID") |>
  dplyr::left_join(r_stabilized$df_yost |>
                     dplyr::select(GEOID, YostStabilized, YostStabilizedQuintile),
                   by = "GEOID") |>
  dplyr::left_join(r_imputed$df_yost |>
                     dplyr::select(GEOID, YostImputed, YostImputedQuintile),
                   by = "GEOID") |>
  dplyr::left_join(r_both$df_yost |>
                     dplyr::select(GEOID, YostStabilizedImputed, YostStabilizedImputedQuintile),
                   by = "GEOID") |>
  dplyr::mutate(year = p$year, geo = p$geo, scope = p$scope) |>
  dplyr::select(
    GEOID, NAME, year, geo, scope,
    Yost, YostQuintile,
    YostStabilized, YostStabilizedQuintile,
    YostImputed, YostImputedQuintile,
    YostStabilizedImputed, YostStabilizedImputedQuintile
  ) |>
  dplyr::arrange(GEOID)

# Sanity check: the raw Yost from each run should agree.
chk <- dplyr::inner_join(
  r_imputed$df_yost_raw |> dplyr::select(GEOID, a = Yost),
  r_stabilized$df_yost_raw |> dplyr::select(GEOID, b = Yost),
  by = "GEOID"
)
max_diff <- max(abs(chk$a - chk$b), na.rm = TRUE)
cat(sprintf("  Raw Yost max |diff| imputed vs stabilized run: %.3g\n", max_diff))
if (is.finite(max_diff) && max_diff > 1e-8) warning("Raw Yost differs between runs.")

# --- Save --------------------------------------------------------------------

# Write to a temp name then rename, so a killed job never leaves a partial
# file that the resume check would treat as finished.
tmp_gz <- paste0(gz_path, ".tmp.gz")
data.table::fwrite(df_out, tmp_gz, sep = ",", scipen = 999, compress = "gzip")
file.rename(tmp_gz, gz_path)
cat(sprintf("  Saved CSV.GZ: %s  (%d rows)\n", gz_path, nrow(df_out)))

# GeoPackage is scope-agnostic; only the national task writes it, so the two
# scopes never write the same file at the same time.
if (SAVE_GEOMETRY && p$scope == "national") {
  gpkg_path <- file.path(GPKGDIR, sprintf("yost_%s_%d.gpkg", geo_tag, p$year))
  geom <- r_imputed$df_geometry
  if (!is.null(geom) && inherits(geom, "sf")) {
    sf_out <- geom |>
      dplyr::left_join(r_imputed$df_raw_values |> dplyr::select(GEOID, NAME),
                       by = "GEOID") |>
      dplyr::select(GEOID, NAME)
    if (file.exists(gpkg_path)) file.remove(gpkg_path)
    sf::st_write(sf_out, gpkg_path, layer = "geometry", quiet = TRUE)
    cat(sprintf("  Saved GPKG  : %s  (%.1f MB)\n", gpkg_path, file.size(gpkg_path) / 1024^2))
  } else {
    cat("  Note: geometry not available — skipping GPKG.\n")
  }
}

if (SAVE_RDS) {
  r_both$df_yost_combined <- df_out
  for (cfg in list(list(r = r_imputed,    s = "imputed"),
                   list(r = r_stabilized, s = "stabilized"),
                   list(r = r_both,       s = "stabilized_imputed"))) {
    rds_path <- file.path(RDSDIR, sprintf("%s_%s.rds", file_stem, cfg$s))
    saveRDS(cfg$r, rds_path)
    cat(sprintf("  Saved RDS   : %s\n", rds_path))
  }
}

cat(sprintf("[Job %d] Complete.\n\n", task_id))
