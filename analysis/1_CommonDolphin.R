# ADB
# 2026-04-20

# Estimate abundance of dusky and common dolphins
# in the San Matias Gulf, Argentina
# using density surface models as presented by
# Miller et al 2013
# Spatial models for distance sampling data: recent developments and future directions
# 10.1111/2041-210X.12105

# This is the MAIN file for common dolphins

# source data ----
# Output directories must exist before any ggsave()/fwrite() below.
source(file.path(here::here(), "analysis", "UTIL_EnsureOutputDirs.R"))

source(file.path(here::here(), "analysis", "0_ReadData_Plots.r"))

# EDA -----
source(file.path(here::here(), "analysis", "2_CommonDolphin_EDA.R"))

# Detection function -----
 source(file.path(here::here(), "analysis", "3_CommonDolphin_DetectionFunction.R"))

# DSM ----
source(file.path(here::here(), "analysis", "4_CommonDolphin_DSM.R"))

# DSM soap ----
source(file.path(here::here(), "analysis", "4_CommonDolphin_DSM_soap.R"))


# Create abundance estimates ----
source(file.path(here::here(), "analysis", "5_CommonDolphin_Abundance.R"))

# n_obs == 2 sensitivity analysis ----
source(file.path(here::here(), "analysis", "6_CommonDolphin_Nobs2SensitivityAnalysis.R"))

# Map expected densities -----
source(file.path(here::here(), "analysis", "UTIL_Map_DSM_output_DD.R"))

# Map CV -----
source(file.path(here::here(), "analysis", "UTIL_Map_DSM_output_CV_DD.R"))

# DSM diagnostics -----
# Sourced HERE, before the save, so that dd.diag (correlograms, k.check,
# rootograms and the AIC ranking) travels inside the workspace the Quarto
# report load()s. .SPP is dotted on purpose -- see the save below.
#
# UTIL_DSM_CovariateK_DD.R is deliberately NOT sourced here: it refits 16
# dsm() models and is a tuning study rather than a pipeline stage. Run it on
# its own when the covariate-k question comes up; the report can fread() the
# DD_covariate_k_comparison.csv it writes.
.SPP <- "CommonDolphin"
source(file.path(here::here(), "analysis", "UTIL_DSM_Diagnostics.R"))

# save image -----
# NOT save.image(): that writes all.names = TRUE, so every dotted config object
# created during the run (.cfg, .spp, .models, .diag_dir, .SPP ...) lands in the
# .RData and then clobbers the same names in whatever script load()s it next --
# which is how the diagnostics output once ended up in the Nobs2Sensitivity
# folder. ls() defaults to all.names = FALSE and keeps them out.
save(list = ls(envir = .GlobalEnv), envir = .GlobalEnv,
     file = "output/CommonDolphin/dd_output.RData")
