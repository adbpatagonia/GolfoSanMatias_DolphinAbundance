# ADB
# 2026-04-20

# Estimate abundance of dusky and common dolphins
# in the San Matias Gulf, Argentina
# using density surface models as presented by
# Miller et al 2013
# Spatial models for distance sampling data: recent developments and future directions
# 10.1111/2041-210X.12105

# This is the DSM file for common dolphins

# libraries ----
library(gratia)

# prepare data -----
obsdata_lo_mod <- copy(obsdata_lo)
obsdata_lo_mod <-   obsdata_lo_mod[distance <= trunc.dist_lo]


# esto esta en m2
segdata[, off.set_lo := Effort * trunc.dist_lo]

off.set_lo <- 800 * trunc.dist_lo

obsdata_lo_mod[, season := relevel(factor(season), ref = "Spring")]

# correlation among covariates ----

# Simple model ----

dsm.xy  <-  dsm(count~s(x,y),
                ddf.obj =  df.lo,
                segment.data = segdata,
                observation.data = obsdata_lo_mod,
                method="REML")

summary(dsm.xy)
appraise(dsm.xy)
# year ----
dsm.xy.year  <-  dsm(count ~ s(x,y) +
                       s(Ano),
                ddf.obj =  df.lo,
                segment.data = segdata,
                observation.data = obsdata_lo_mod,
                method="REML")

summary(dsm.xy.year)

appraise(dsm.xy.year)
draw(dsm.xy.year)

# season ----
dsm.xy.season  <-  dsm(count ~ s(x,y) +
                        s(season),
                     ddf.obj =  df.lo,
                     segment.data = segdata,
                     observation.data = obsdata_lo_mod,
                     # family = tweedie,
                     method="REML")

summary(dsm.xy.season)
appraise(dsm.xy.season)

draw(dsm.xy.season, residuals = FALSE)

# slope ----
dsm.xy.slope  <-  dsm(count ~ s(x,y) +
                         s(slope),
                       ddf.obj =  df.lo,
                       segment.data = segdata,
                       observation.data = obsdata_lo_mod,
                       # family = tweedie,
                       method="REML")

summary(dsm.xy.slope)
appraise(dsm.xy.slope)

draw(dsm.xy.slope, residuals = FALSE)

# depth ----
dsm.xy.depth  <-  dsm(count ~ s(x,y) +
                        s(depth),
                      ddf.obj =  df.lo,
                      segment.data = segdata,
                      observation.data = obsdata_lo_mod,
                      # family = tweedie,
                      method="REML")

summary(dsm.xy.depth)
appraise(dsm.xy.depth)

draw(dsm.xy.depth, residuals = FALSE)



# Smooth estimates
smooth_est <- smooth_estimates(dsm.xy.season) %>%
  add_confint() %>%
  rename(term = .smooth) %>%
  data.table()


draw(dsm.xy, residuals = FALSE)
