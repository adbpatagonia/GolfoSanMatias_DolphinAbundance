# ADB
# 2026-04-20

# Estimate abundance of dusky and common dolphins
# in the San Matias Gulf, Argentina
# using density surface models as presented by
# Miller et al 2013
# Spatial models for distance sampling data: recent developments and future directions
# 10.1111/2041-210X.12105

# This is the MAIN file for dusky dolphins

# source data ----
source(file.path(here::here(), "analysis", "0_ReadData_Plots.r"))

# EDA -----
source(file.path(here::here(), "analysis", "2_DuskyDolphin_EDA.R"))

# Detection function -----
source(file.path(here::here(), "analysis", "3_DuskyDolphin_DetectionFunction.R"))

# DSM ----
source(file.path(here::here(), "analysis", "4_DuskyDolphin_DSM.R"))

# Map expected densities -----
source(file.path(here::here(), "analysis", "UTIL_Map_DSM_output_LO.R"))
