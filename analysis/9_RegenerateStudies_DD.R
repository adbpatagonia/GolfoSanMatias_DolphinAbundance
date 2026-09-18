# ADB / Claude
# 2026-09-16
#
# COMMON DOLPHIN -- regenerate the study outputs that are NOT part of the
# pipeline but CAN still be produced by current code.
#
# Run this AFTER 1_CommonDolphin.R, from the project root:
#     source(file.path(here::here(), "analysis", "9_RegenerateStudies_DD.R"))
#
# WHY THERE IS A SECOND DRIVER AT ALL
# 1_CommonDolphin.R is the publication pipeline: read data, EDA, detection
# function, DSM, abundance, sensitivity, maps, diagnostics, save. The scripts
# sourced here are not part of that chain -- they refit models the pipeline has
# already fitted, or fit models it deliberately does not, and adding them to the
# driver would multiply its runtime for output nobody needs on every run. They
# are still wanted in a clean output directory, so they get their own driver
# rather than being left to memory.
#
# WHAT IS DELIBERATELY NOT HERE
# Three studies cannot run against the current pipeline, because the
# configuration they exist to measure no longer exists -- 4_CommonDolphin_DSM_soap.R
# was retuned from 3000/2000/10x8/k=10 to 500/250/14x11/K_COV=20, and these are
# the evidence for that change:
#
#     UTIL_DSM_CovariateK_DD.R         guards on AIC 6100.528; its "stored"
#                                      arm reads the global knots, now 89
#     UTIL_DSM_SoapRevised_DD.R        its `original` arm reads the stored
#                                      fitted objects, which are now the tuned
#                                      models -- it would compare the tuned arm
#                                      against itself
#     UTIL_DSM_RootogramKnots_DD.R     pins AIC_STORED_ORIG <- 6100.53, and both
#                                      its arms now build from the same grid
#
#     UTIL_DSM_CovariateK_DD_Figure.R  plots the first one's CSVs; reproducible
#                                      from those stored CSVs, never from a
#                                      fresh run
#
#     UTIL_DSM_SoapTuning_DD.R         marked a decision record on 2026-09-16,
#                                      AFTER this header was first written. It
#                                      IS the study that chose 500/250/14x11,
#                                      so re-running it against the pipeline it
#                                      produced would compare that arm with
#                                      itself. Its four CSVs -- DD_soap_knot_sweep,
#                                      DD_soap_boundary_variants,
#                                      DD_soap_tuned_selection and
#                                      DD_soap_knot_correlogram -- restore from
#                                      quarantine like the three above.
#
# Each carries a DECISION RECORD header saying the same thing. Their CSVs under
# output/CommonDolphin/DSM/ are dated artefacts, not regenerable output: restore
# them from the quarantine rather than expecting this driver to rebuild them.
# The consequence for a publication audit is that every file in output/ is
# either (a) written by current code in this run, or (b) an explicitly dated
# decision record -- and the two are told apart by the header of the script that
# produced it, not by guesswork.

library(here)

source(file.path(here::here(), "analysis", "UTIL_EnsureOutputDirs.R"))

if (!exists("dd.dsm.soap.season.year"))
  stop("no fitted DD models in the workspace.\n",
       "  Run analysis/1_CommonDolphin.R first, or load()\n",
       "  output/CommonDolphin/dd_output.RData, then source this file.")

.t0 <- Sys.time()

# ===========================================================================
# ORDER IS LOAD-BEARING. All three scripts below consume
# output/CommonDolphin/DSM/tuned_models/dd_tuned_base.rds, and the FIRST one
# is the only script in the project that writes it. Reordering them breaks the
# run.
#
# This was wrong until 2026-09-18. TailFix_DD ran first and read a
# dd_tuned_base.rds that had been left on disk by a manual run of TunedArm on
# 2026-09-16, so the driver appeared to work while actually depending on output
# it had not produced. The first genuinely cold run -- the one this re-run
# exists to perform -- failed on it immediately:
#
#     cannot open compressed file '.../tuned_models/dd_tuned_base.rds'
#
# which is the bug doing exactly what it should. Worth keeping in view: a driver
# that passes only because the output directory is dirty is indistinguishable
# from a correct one until the directory is clean.
# ===========================================================================

# ---------------------------------------------------------------------------
# 1. Tuned-arm density maps and abundance series. Its guards (89 knots, AIC
# 6070.35 / 6034.93) now MATCH the pipeline, so it runs -- but it refits those
# two models rather than reading them, which is why it belongs here and not in
# the pipeline driver.
#
# FIRST because it WRITES tuned_models/dd_tuned_base.rds and
# Abundance/DD_abundance_tuned.csv, which 2 and 3 both read. It depends on
# nothing either of them produces -- only on the pipeline's own
# DD_abundance_season_year_soap.csv -- so it is safe here.
# ---------------------------------------------------------------------------
message("\n=== UTIL_DSM_TunedArm_MapsAbundance_DD.R ===")
source(file.path(here::here(), "analysis", "UTIL_DSM_TunedArm_MapsAbundance_DD.R"))

# ---------------------------------------------------------------------------
# 2. Rootogram tail: why DD over-predicts large counts, and the two routes out.
# Distributional, so it is unaffected by the soap retune -- the finding is that
# 92.7% of the count variance is school SIZE, which no spatial basis can fix.
#
# AFTER 1: readRDS()es dd_tuned_base.rds at its line 71 and takes the reported
# tw() model from it. Its other two readRDS() calls are self-contained -- both
# files are written inside its own cached_fit_row() blocks earlier in the same
# script -- so they are not an ordering constraint.
# ---------------------------------------------------------------------------
message("\n=== UTIL_DSM_TailFix_DD.R ===")
source(file.path(here::here(), "analysis", "UTIL_DSM_TailFix_DD.R"))

# ---------------------------------------------------------------------------
# 3. How much the rootogram tail misfit costs the abundance estimates.
# LAST, and it fails DIFFERENTLY from 2 if run early, which is the dangerous
# case: it readRDS()es dd_tuned_base.rds (hard error, like 2), but its two
# fread()s -- DD_abundance_tuned.csv from 1, and the pipeline's
# DD_abundance_season_year_soap.csv -- are wrapped in tryCatch() and degrade to
# NULL. Out of order it would not stop; it would write a test-C block with
# nothing in it.
# ---------------------------------------------------------------------------
message("\n=== UTIL_DSM_TailMisfit_Impact_DD.R ===")
source(file.path(here::here(), "analysis", "UTIL_DSM_TailMisfit_Impact_DD.R"))

message(sprintf("\n9_RegenerateStudies_DD.R finished in %.1f min",
                as.numeric(difftime(Sys.time(), .t0, units = "mins"))))
