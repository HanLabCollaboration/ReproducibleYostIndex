#!/bin/bash
# =============================================================================
# submit_yost.sh — SLURM array over all (geo, scope, year) combinations.
#
#   cd /home/groups/manishad/reproducibleYostIndex-all-possible
#   sbatch submit_yost.sh                     # all 80 tasks, 8 at a time
#   sbatch --array=24 submit_yost.sh          # rerun one task
#   SLURM_ARRAY_TASK_ID=25 bash submit_yost.sh   # quick interactive test (county)
#
# Task map (YEAR_MAX=2024): 1-24 block group, 25-52 county, 53-80 tract.
# Finished tasks are skipped on resubmit (output csv.gz exists); set
# OVERWRITE=TRUE to redo them.
# =============================================================================

#SBATCH --job-name=ReproducibleYostIndex
#SBATCH --array=1-80%8
#SBATCH --time=24:00:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=2
#SBATCH --partition=normal,qsu,owners
#SBATCH --chdir=/home/groups/manishad/reproducibleYostIndex-all-possible
#SBATCH --output=/home/groups/manishad/reproducibleYostIndex-all-possible/output/logs/yost_%a.out
#SBATCH --error=/home/groups/manishad/reproducibleYostIndex-all-possible/output/logs/yost_%a.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=rcatoiap@stanford.edu

# %8 caps concurrent tasks: the last run lost task 24 to Census API rate
# limiting when all 80 tasks started at once.

BASE="${BASE:-/home/groups/manishad/reproducibleYostIndex-all-possible}"
export OUTDIR="$BASE/output"
export YEAR_MAX=2024
export SAVE_GEOMETRY=TRUE
export SAVE_RDS=TRUE
export OVERWRITE="${OVERWRITE:-FALSE}"
export TIGRIS_CACHE_DIR="$BASE/cache/tigris"   # keep the shapefile cache off $HOME (15 GB quota)

mkdir -p "$OUTDIR/logs" "$TIGRIS_CACHE_DIR"
cd "$BASE"

source "$BASE/modules.sh"

echo "========================================"
echo "Job:     ${SLURM_JOB_ID:-interactive}"
echo "Task:    $SLURM_ARRAY_TASK_ID"
echo "Node:    ${SLURMD_NODENAME:-$(hostname)}"
echo "Started: $(date)"
echo "========================================"

Rscript "$BASE/run_yost_batch.R"
EXIT_CODE=$?

echo "========================================"
echo "Finished: $(date)"
echo "Exit code: $EXIT_CODE"
echo "========================================"
exit $EXIT_CODE
