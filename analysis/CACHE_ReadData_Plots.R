# ADB
# Cache wrapper around 0_ReadData_Plots.r
#
# 0_ReadData_Plots.r is identical, species-agnostic setup sourced by
# DelfinesComunes.qmd, DelfinesOscuros.qmd, and VariablesAmbientales.qmd.
# Running it 3x per render repeats the same CSV/shapefile reads and the
# per-season st_join(st_nearest_feature)/st_intersection grid build.
# This wrapper runs it once and reuses the cached result across pages,
# rebuilding only when the script or the source data files change.
#
# On a cache hit, 0_ReadData_Plots.r's own library() calls never run, but
# 2_*_EDA.R/3_*_DetectionFunction.R/4_*_DSM.R (and the .qmd bodies themselves)
# call dplyr/ggplot2/sf/data.table/dsm/Distance/mrds functions unqualified.
# Attach them here unconditionally so every page gets them regardless of
# whether this call builds fresh or reads from cache.
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(data.table)
  library(dsm)
  library(Distance)
  library(mrds)
})

load_or_build_readdata <- function(
    script = file.path(here::here(), "analysis", "0_ReadData_Plots.r"),
    cache_file = file.path(here::here(), "analysis", ".cache", "ReadData_Plots.rds"),
    data_dirs = c(
      file.path(here::here(), "data", "DistanceData"),
      file.path(here::here(), "data", "shp")
    ),
    # 2_*.R/3_*.R/4_*.R/UTIL_*.R are loaded downstream via plain source(),
    # which always evaluates into .GlobalEnv regardless of caller - so the
    # objects built here must land there too, not in whatever (possibly
    # child) environment knitr/quarto is executing the current chunk in.
    envir = globalenv()
) {
  dep_files <- c(script, list.files(data_dirs, full.names = TRUE, recursive = TRUE))
  dep_mtime <- max(file.info(dep_files)$mtime)

  fresh <- file.exists(cache_file) && file.info(cache_file)$mtime > dep_mtime

  if (fresh) {
    objs <- readRDS(cache_file)
  } else {
    build_env <- new.env(parent = globalenv())
    sys.source(script, envir = build_env)
    objs <- as.list(build_env)
    dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(objs, cache_file)
  }

  # data.table's internal self-reference pointer doesn't survive
  # (de)serialization; reset it so a later `:=` on a cached table doesn't warn.
  # Use alloc.col(), not setDT(), because some cached objects (e.g.
  # segdata_traj_m) are data.table AND sf hybrids - setDT() overwrites the
  # class attribute wholesale and silently drops the "sf" class, which then
  # breaks geom_sf()/stat_sf() on those objects downstream.
  for (nm in names(objs)) {
    if (data.table::is.data.table(objs[[nm]])) {
      objs[[nm]] <- data.table::alloc.col(objs[[nm]])
    }
  }

  list2env(objs, envir = envir)
  invisible(NULL)
}
