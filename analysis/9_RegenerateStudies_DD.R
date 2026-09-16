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

# ---------------------------------------------------------------------------
# Rootogram tail: why DD over-predicts large counts, and the two routes out.
# Distributional, so it is unaffected by the soap retune -- the finding is that
# 92.7% of the count variance is school SIZE, which no spatial basis can fix.
# ---------------------------------------------------------------------------
message("\n=== UTIL_DSM_TailFix_DD.R ===")
source(file.path(here::here(), "analysis", "UTIL_DSM_TailFix_DD.R"))

# ---------------------------------------------------------------------------
# Tuned-arm density maps and abundance series. Its guards (89 knots, AIC 6070.35
# / 6034.93) now MATCH the pipeline, so it runs -- but it refits those two
# models rather than reading them, which is why it belongs here and not in the
# driver.
# ---------------------------------------------------------------------------
message("\n=== UTIL_DSM_TunedArm_MapsAbundance_DD.R ===")
source(file.path(here::here(), "analysis", "UTIL_DSM_TunedArm_MapsAbundance_DD.R"))

message(sprintf("\n9_RegenerateStudies_DD.R finished in %.1f min",
                as.numeric(difftime(Sys.time(), .t0, units = "mins"))))
