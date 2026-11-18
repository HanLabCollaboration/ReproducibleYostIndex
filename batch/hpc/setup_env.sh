#!/bin/bash
# =============================================================================
# setup_env.sh — one-time R environment for the batch run. Run on a compute
# node (not the login node):
#
#   srun -p normal -c 4 --mem=8G --time=2:00:00 --pty bash
#   cd /home/groups/manishad/reproducibleYostIndex-all-possible
#   bash setup_env.sh
#
# Re-run any time to pull the latest ReproducibleYostIndex from GitHub main.
# Packages go to your user library for R 4.4 (~/R/x86_64-pc-linux-gnu-library/4.4);
# the old R 4.2 library is not reused.
# =============================================================================
set -euo pipefail
BASE="${BASE:-/home/groups/manishad/reproducibleYostIndex-all-possible}"
cd "$BASE"
source "$BASE/modules.sh"

Rscript - <<'EOF'
lib <- Sys.getenv("R_LIBS_USER")
dir.create(lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(lib, .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"), Ncpus = 1)

# Old package name from before the rename; remove so it can't be loaded by mistake.
if ("ReproduceYostIndex" %in% rownames(installed.packages())) remove.packages("ReproduceYostIndex")

needed <- c("remotes", "dplyr", "tidyr", "sf", "data.table", "tidycensus", "tigris")
missing <- setdiff(needed, rownames(installed.packages()))
if (length(missing)) install.packages(missing)

missing <- setdiff(needed, rownames(installed.packages()))
if (length(missing)) stop("Failed to install: ", paste(missing, collapse = ", "),
                          ". Fix those (see errors above) before installing ReproducibleYostIndex.")

# The repo is public, so no token is needed. A stale GITHUB_PAT/GITHUB_TOKEN
# (e.g. in ~/.Renviron) makes GitHub answer 401 even for public repos, and
# remotes reads it from the environment, so drop it for the retry.
install_yost <- function() remotes::install_github(
  "HanLabCollaboration/ReproducibleYostIndex", ref = "main",
  upgrade = "never", force = TRUE)
ok <- tryCatch({ install_yost(); TRUE },
               error = function(e) { message("GitHub install failed: ", conditionMessage(e)); FALSE })
if (!ok) {
  message("Retrying without GITHUB_PAT / GITHUB_TOKEN ...")
  Sys.unsetenv(c("GITHUB_PAT", "GITHUB_TOKEN"))
  install_yost()
  message("NOTE: your GITHUB_PAT is invalid or expired; update or remove it in ~/.Renviron.")
}

library(ReproducibleYostIndex)
desc <- utils::packageDescription("ReproducibleYostIndex")
cat("\nInstalled ReproducibleYostIndex", desc$Version, "at commit", desc$RemoteSha, "\n")
writeLines(c(paste("ReproducibleYostIndex", desc$Version, desc$RemoteSha),
             paste("R", R.version.string), paste("date", Sys.time())),
           "package_version.txt")

if (!nzchar(Sys.getenv("CENSUS_API_KEY")))
  cat("WARNING: CENSUS_API_KEY not set. Add it to ~/.Renviron before submitting.\n")
EOF
