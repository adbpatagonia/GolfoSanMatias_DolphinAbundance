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
source(file.path(here::here(), "analysis", "0_ReadData_Plots.r"))

# EDA -----
source(file.path(here::here(), "analysis", "2_CommonDolphin_EDA.R"))

# Detection function -----
 # source(file.path(here::here(), "analysis", "3_CommonDolphin_DetectionFunction.R"))

# DSM ----
# source(file.path(here::here(), "analysis", "4_CommonDolphin_DSM.R"))
