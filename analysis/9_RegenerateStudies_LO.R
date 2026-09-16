# ADB / Claude
# 2026-09-16
#
# DUSKY DOLPHIN -- regenerate the study outputs that are NOT part of the
# pipeline but CAN still be produced by current code.
#
# Run this AFTER 1_DuskyDolphin.R, from the project root:
#     source(file.path(here::here(), "analysis", "9_RegenerateStudies_LO.R"))
#
# See 9_RegenerateStudies_DD.R for why there is a second driver at all. The
# short version: these scripts refit models the pipeline has already fitted, or
# fit models it deliberately does not, so putting them in 1_DuskyDolphin.R would
# multiply its runtime for output not needed on every run -- but they are still
# wanted in a clean output directory, so they get a driver rather than being
# left to memory.
#
# UNLIKE THE DD SIDE, NOTHING HERE IS A DECISION RECORD.
# Three of the DD studies became un-runnable when the tuned soap configuration
# was adopted, because they hardcode the configuration they were measuring. Both
# LO studies survive that trap:
#
#   * UTIL_DSM_SoapTuning_LO.R reads simplify_tol / margin / knot_ngrid FROM THE
#     WORKSPACE rather than hardcoding them, so its "original" arm is whatever
#     the pipeline currently fits. It re-runs correctly no matter what the knobs
#     are set to.
#   * UTIL_DSM_CovariateK_LO.R works on the fs block, which the soap
#     configuration does not touch at all.
#
# Both are therefore genuinely regenerable, and after this driver runs every
# file under output/DuskyDolphin/ has been written by current code.
#
# COST. The LO fs fits in CovariateK_LO are the expensive part -- roughly 11
# minutes each, which is why that script caches aggressively and reads the k=10
# arm from the workspace instead of refitting it. SoapTuning_LO is much cheaper
# (soap fits run 20-250 s) but does ~45 of them. Budget an hour or two, and note
# that both resume from cache if interrupted.

library(here)

source(file.path(here::here(), "analysis", "UTIL_EnsureOutputDirs.R"))

if (!exists("lo.dsm.xy.fsyear.season"))
  stop("no fitted LO models in the workspace.\n",
       "  Run analysis/1_DuskyDolphin.R first, or load()\n",
       "  output/DuskyDolphin/lo_output.RData, then source this file.")

.t0 <- Sys.time()

# ---------------------------------------------------------------------------
# Was dropping the environmental covariates right within the fs block?
# The control case for the DD covariate-k study: LO's k = 10 diagnostics were
# already clean (s(depth) edf 3.66 of 9, no ceiling), so the expectation is that
# k = 20 changes little -- worth confirming rather than assuming, because it is
# what makes the DD result interpretable.
# ---------------------------------------------------------------------------
message("\n=== UTIL_DSM_CovariateK_LO.R ===")
source(file.path(here::here(), "analysis", "UTIL_DSM_CovariateK_LO.R"))

# ---------------------------------------------------------------------------
# Soap boundary and knot-grid tuning. Ran 2026-09-16 and adopted NOTHING -- the
# LO spatial basis is not binding (edf_frac 0.335 vs DD's 0.64), no covariate
# smooth is near its ceiling at k = 10, and the tighter boundary costs 7.32 AIC.
# Kept in the driver anyway: it is the evidence that the arm was checked and
# left alone deliberately, and it regenerates cleanly.
#
# Sourced with no overrides, so it runs the full sweep and then the tuned arm at
# whatever the sweep auto-picks. To reproduce the 2026-09-16 run exactly, set
# FORCE_CONFIG <- list(tol = 500, margin = 250, ngrid = c(10L, 8L)) first.
# ---------------------------------------------------------------------------
message("\n=== UTIL_DSM_SoapTuning_LO.R ===")
source(file.path(here::here(), "analysis", "UTIL_DSM_SoapTuning_LO.R"))

message(sprintf("\n9_RegenerateStudies_LO.R finished in %.1f min",
                as.numeric(difftime(Sys.time(), .t0, units = "mins"))))
