# Regenerating the ReproducibleYostIndex-data release

These scripts pre-compute the Yost Index for every supported combination and
produce the files published at
[ReproducibleYostIndex-data](https://github.com/HanLabCollaboration/ReproducibleYostIndex-data/releases),
which `getYostIndex()` downloads. They are not part of the R package
(`batch/` is listed in `.Rbuildignore`).

| File | Purpose |
|---|---|
| `setup_env.sh` | One-time R environment: installs dependencies and the latest `ReproducibleYostIndex` from GitHub `main`, and records the installed commit in `package_version.txt`. |
| `modules.sh` | Cluster modules (R 4.4.2 plus GDAL/GEOS/PROJ/udunits for `sf`), shared by setup and submit. |
| `submit_yost.sh` | SLURM array job, one task per (geo, scope, year), at most 8 running at a time. |
| `run_yost_batch.R` | The work for one task. |

## What gets computed

80 tasks with `YEAR_MAX = 2024`:

| Tasks | geo | years | scopes |
|---|---|---|---|
| 1–24 | block group | 2013–2024 | national, state |
| 25–52 | county | 2011–2024 | national, state |
| 53–80 | tract | 2011–2024 | national, state |

Each task runs three `computeYostIndex()` configurations: imputed, stabilized,
and stabilized + imputed. The unadjusted Yost does not need its own run because
every run already returns it as `df_yost_raw`.

## Outputs (under `$BASE/output`)

- `csvgz/yost_<geo>_<scope>_<year>.csv.gz`: release asset, a wide table with
  `GEOID, NAME, year, geo, scope`, plus `Yost`, `YostStabilized`, `YostImputed`
  and `YostStabilizedImputed`, each with its quintile.
- `gpkg/yost_<geo>_<year>.gpkg`: release asset, `GEOID + NAME + geometry`
  (layer `geometry`). These are the same for both scopes, so only the national
  task writes them.
- `rds/yost_<geo>_<scope>_<year>_<config>.rds`: the full detailed output of
  each configuration. These are kept for provenance and are not released.
- `logs/yost_<task>.out|.err`

## Running it (Stanford Sherlock)

The paths assume `BASE=/home/groups/manishad/reproducibleYostIndex-all-possible`.
On another cluster, change `BASE` and the `#SBATCH --chdir/--output/--error`
lines in `submit_yost.sh`.

```bash
# 1. Copy the scripts to the cluster
rclone copy batch/hpc sherlock:/home/groups/manishad/reproducibleYostIndex-all-possible --progress

# 2. On a compute node: set up R (re-run to pick up a newer package version)
srun -p normal -c 4 --mem=8G --time=2:00:00 --pty bash
cd /home/groups/manishad/reproducibleYostIndex-all-possible
bash setup_env.sh          # needs CENSUS_API_KEY=... in ~/.Renviron

# 3. Test one task interactively, then submit all of them
SLURM_ARRAY_TASK_ID=25 bash submit_yost.sh
sbatch submit_yost.sh
```

Resubmitting `sbatch submit_yost.sh` skips tasks whose `.csv.gz` already
exists, so it can be used to pick up failures. Use `sbatch --array=N
submit_yost.sh` for a single task, or `OVERWRITE=TRUE` to force a recompute.
Failed configurations are retried up to 4 times, waiting longer each time
(Census API rate limits).

## Publishing

Upload everything in `output/csvgz/` and `output/gpkg/` as assets of a new
release of `ReproducibleYostIndex-data`, and put the commit from
`package_version.txt` in the release notes. `getYostIndex()` always reads
`/releases/latest`.
