# ADB / Claude
# 2026-09-16
#
# Create the output/ directory skeleton.
#
# WHY THIS EXISTS. ggsave(), fwrite(), png() and writeLines() do NOT create
# parent directories -- they abort. The output/ tree is routinely deleted before
# a clean re-run (that is the point: one output directory, every file written by
# the current code), and before this helper the first script to reach a ggsave()
# would kill the driver several minutes in, leaving a half-written tree. Scripts
# 6_* and the UTIL_DSM_* studies already dir.create()d their own folders; the
# rest -- 0, 1, 2, 4, 5, the map scripts and the edge-effect scripts -- assumed
# the tree was already there.
#
# Sourced at the top of every script that writes to output/, not just from the
# drivers, so that any script still runs standalone against a deleted tree.
# Idempotent and cheap (recursive = TRUE, showWarnings = FALSE), so sourcing it
# a dozen times in one driver run costs nothing.
#
# The names are DOTTED on purpose. 1_*.R saves with save(list = ls(...)), which
# defaults to all.names = FALSE, so dotted names stay out of the .RData and
# cannot clobber anything in the script that load()s it next -- the same
# convention the drivers rely on for .SPP and the diagnostics config.

.ensure_output_dirs <- function() {
  .subs <- c("EDA", "DSM", "DSM/autocorrelation", "Abundance", "Nobs2Sensitivity")
  .spp  <- c("CommonDolphin", "DuskyDolphin")
  .dirs <- c(
    file.path(here::here(), "output"),
    file.path(here::here(), "output", "EnvVars"),
    file.path(here::here(), "output", rep(.spp, each = length(.subs)), .subs)
  )
  invisible(lapply(.dirs, dir.create, showWarnings = FALSE, recursive = TRUE))
}

.ensure_output_dirs()
