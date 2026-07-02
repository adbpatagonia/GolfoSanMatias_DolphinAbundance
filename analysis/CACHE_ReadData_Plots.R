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


# Generic dependency-aware cache for an expensive analysis script -------------
#
# Same idea as load_or_build_readdata(), generalised for the model-fitting
# stages (3_*_DetectionFunction.R fits ~5 min of ds() models; 4_*_DSM.R ~3 min
# of dsm()/gam() fits). Runs `script` once, caches every object it creates to
# analysis/.cache/<script>.rds, and on later renders reloads instead of
# refitting - UNLESS any dependency is newer than the cache, in which case it
# rebuilds. This is what lets a prose-only edit render without refitting.
#
# Dependencies (mtime any-newer => rebuild):
#   - the script itself
#   - every upstream analysis script passed in `deps` (basenames under
#     analysis/). List the FULL transitive chain, not just the immediate
#     parent: 4_*_DSM.R must list BOTH 3_*_DetectionFunction.R AND 2_*_EDA.R,
#     because 2_*_EDA.R mutates the global distdata_dd that 3_* copies. A
#     missing edge here = silent stale output, the one failure mode to avoid.
#   - the ReadData cache (.rds) and this helper file, added automatically -
#     every compute stage depends on the shared data and on this code.
#
# capture_globals: names of PRE-EXISTING globals the script mutates in place by
#   reference (e.g. 4_*_DSM.R does `segdata[, year_fac := ...]`). Those don't
#   appear as new bindings in the build env, so name them here to fold them
#   into the cache; downstream (UTIL_Map_*) needs segdata$year_fac.
#
# Emits "[cache] <label>: rebuilt" or ": cached" on every call - a standing,
# glanceable signal of exactly what recomputed, the direct antidote to silent
# staleness.
cache_source <- function(script,
                         deps = character(),
                         capture_globals = character(),
                         label = tools::file_path_sans_ext(script),
                         envir = globalenv()) {
  analysis_dir <- file.path(here::here(), "analysis")
  cache_dir    <- file.path(analysis_dir, ".cache")
  script_path  <- file.path(analysis_dir, script)
  cache_file   <- file.path(cache_dir, paste0(tools::file_path_sans_ext(script), ".rds"))

  dep_files <- unique(c(
    script_path,
    file.path(analysis_dir, deps),
    file.path(cache_dir, "ReadData_Plots.rds"),   # shared data (via its cache)
    file.path(analysis_dir, "CACHE_ReadData_Plots.R")  # this helper
  ))
  dep_files <- dep_files[file.exists(dep_files)]
  dep_mtime <- if (length(dep_files)) max(file.info(dep_files)$mtime) else -Inf

  fresh <- file.exists(cache_file) && file.info(cache_file)$mtime > dep_mtime

  if (fresh) {
    objs <- readRDS(cache_file)
    cat(sprintf("[cache] %s: cached\n", label))
  } else {
    build_env <- new.env(parent = globalenv())
    sys.source(script_path, envir = build_env)
    objs <- as.list(build_env)
    # Fold in pre-existing globals mutated in place by reference. Skip any the
    # script rebound into build_env (that fully-mutated copy already won),
    # so build-env objects always take precedence over the global snapshot.
    for (g in capture_globals) {
      if (!(g %in% names(objs)) && exists(g, envir = globalenv(), inherits = FALSE)) {
        objs[[g]] <- get(g, envir = globalenv())
      }
    }
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    saveRDS(objs, cache_file)
    cat(sprintf("[cache] %s: rebuilt\n", label))
  }

  # reset data.table self-ref pointers without clobbering extra classes (sf)
  for (nm in names(objs)) {
    if (data.table::is.data.table(objs[[nm]])) {
      objs[[nm]] <- data.table::alloc.col(objs[[nm]])
    }
  }

  list2env(objs, envir = envir)
  invisible(names(objs))
}
