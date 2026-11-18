# Sourced by setup_env.sh and submit_yost.sh so both use the same toolchain.
# sf needs GDAL/GEOS/PROJ/udunits at install and run time; s2 (an sf
# dependency) needs cmake to build its bundled Abseil. On Sherlock they
# sit in the "physics" category; check names with `module spider gdal` if a
# load fails.
module purge
module load R/4.4.2
module load physics gdal geos proj udunits 2>/dev/null || \
  echo "WARNING: could not load gdal/geos/proj/udunits; run 'module spider gdal' and edit modules.sh"
module load cmake 2>/dev/null || \
  echo "WARNING: could not load cmake (needed to install s2/sf); run 'module spider cmake' and edit modules.sh"
