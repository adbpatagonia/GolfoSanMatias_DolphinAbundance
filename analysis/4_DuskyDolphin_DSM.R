# ADB
# 2026-04-20

# Estimate abundance of dusky and common dolphins
# in the San Matias Gulf, Argentina
# using density surface models as presented by
# Miller et al 2013
# Spatial models for distance sampling data: recent developments and future directions
# 10.1111/2041-210X.12105

# This is the DSM file for dusky dolphins

# Choice of value of p parameter for tweedie family
# analysis/UTIL_FindTweedieP_DuskyDolphin.R

# libraries ----
library(gratia)

# prepare data -----
obsdata_lo_mod <- copy(obsdata_lo)
obsdata_lo_mod <-   obsdata_lo_mod[distance <= trunc.dist_lo]


# esto esta en m2
segdata[, off.set_lo := Effort * trunc.dist_lo]

off.set_lo <- 800 * trunc.dist_lo

obsdata_lo_mod[, season := relevel(factor(season), ref = "Spring")]
obsdata_lo_mod[, year_fac := factor(Ano)]
segdata[, year_fac := factor(Ano)]

# correlation among covariates ----

# Simple model ----

dsm.xy  <-  dsm(count~s(x,y),
                ddf.obj =  df.lo,
                segment.data = segdata,
                observation.data = obsdata_lo_mod,
                method="REML")

summary(dsm.xy)
appraise(dsm.xy)

## Tweedie ----
dsm.xy.tw  <-  dsm(count~s(x,y),
                   ddf.obj =  df.lo,
                   segment.data = segdata,
                   observation.data = obsdata_lo_mod,
                   family = Tweedie(p = 1.33),
                   method="REML")

summary(dsm.xy.tw)
appraise(dsm.xy.tw)
draw(dsm.xy.tw)


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
dsm.xy.season.tw  <-  dsm(count ~ s(x,y) +
                            -1 + season +
                            s(year_fac, bs = "re"),
                          ddf.obj =  df.lo,
                          segment.data = segdata,
                          observation.data = obsdata_lo_mod,
                          family = Tweedie(p = 1.31),
                          method="REML")

summary(dsm.xy.season.tw)
appraise(dsm.xy.season.tw)

draw(dsm.xy.season.tw, residuals = FALSE)

# slope ----
dsm.xy.slope  <- dsm(count ~ s(x,y) +
                       -1 + season +
                       s(slope) +
                       s(year_fac, bs = "re"),
                     ddf.obj =  df.lo,
                     segment.data = segdata,
                     observation.data = obsdata_lo_mod,
                     family = Tweedie(p = 1.31),
                     method="REML")

summary(dsm.xy.slope)
appraise(dsm.xy.slope)

draw(dsm.xy.slope, residuals = FALSE)

# depth ----
dsm.xy.depthnull  <-  dsm(count ~ s(x,y) +
                        -1 + season +
                        # s(depth) +
                        s(year_fac, bs = "re"),
                      ddf.obj =  df.lo,
                      segment.data = segdata,
                      observation.data = obsdata_lo_mod,
                      family = Tweedie(p = 1.31),
                        method="REML")

dsm.xy.depth  <-  dsm(count ~ s(x,y) +
                        -1 + season +
                        s(depth) +
                        s(year_fac, bs = "re"),
                      ddf.obj =  df.lo,
                      segment.data = segdata,
                      observation.data = obsdata_lo_mod,
                      family = Tweedie(p = 1.31),
                                            method="REML")

summary(dsm.xy.depth)
appraise(dsm.xy.depth)

draw(dsm.xy.depth, residuals = FALSE)

anova(dsm.xy.depthnull, dsm.xy.depth,   test = "F")
AIC(dsm.xy.depth, dsm.xy.depthnull)

# Model selection -----

AIC(dsm.xy.tw,
    dsm.xy.season.tw,
    dsm.xy.slope,
    dsm.xy.depth
    ) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>%
  mutate(Dev = c(
    round(summary(dsm.xy.tw)$dev.expl, 2),
    round(summary(dsm.xy.season.tw)$dev.expl, 2),
    round(summary(dsm.xy.slope)$dev.expl, 2),
    round(summary(dsm.xy.depth)$dev.expl, 2)

  )) %>%
  arrange(AIC)


# partition of deviance ----
# p.var.part <- plot.gamhp(gam.hp(dsm.xy.depth$model), plot.perc = TRUE)



# Smooth estimates -----
smooth_est <- smooth_estimates(dsm.xy.season) %>%
  add_confint() %>%
  rename(term = .smooth) %>%
  data.table()


draw(dsm.xy, residuals = FALSE)
