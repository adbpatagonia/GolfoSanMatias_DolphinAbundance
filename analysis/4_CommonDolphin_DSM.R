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
obsdata_dd_mod <- copy(obsdata_dd)
obsdata_dd_mod <-   obsdata_dd_mod[distance <= trunc.dist_dd]


# esto esta en m2
segdata[, off.set_dd := Effort * trunc.dist_dd]

off.set_dd <- 800 * trunc.dist_dd

obsdata_dd_mod[, season := relevel(factor(season), ref = "Spring")]
obsdata_dd_mod[, year_fac := factor(Ano)]
segdata[, year_fac := factor(Ano)]

# Family and fitting engine ----
# FAMILY: tw(link = "log") estimates the Tweedie power p as part of each fit,
# replacing the fixed Tweedie(p = 1.58) that UTIL_FindTweedieP_CommonDolphin.R
# used to pick by AIC grid search. That grid search was a profile likelihood
# over p that paid NO degrees of freedom for the p it chose, and models fitted
# at different fixed p are not rankable against one another. tw() costs exactly
# 1 df (n.theta = 1), so the selection table is internally consistent.
#
# ENGINE: every model here is fitted with gam/REML, as before — common dolphins
# never needed bam. Dusky dolphins briefly did; that was reversed after a
# direct comparison showed bam fitting materially worse at this sample size
# (see the engine note in 4_DuskyDolphin_DSM.R). Both species are now gam/REML
# throughout, so AIC is comparable across every row of every selection table.

# correlation among covariates ----

# Simple model ----

dd.dsm.xy.ts  <-  dsm(count ~ s(x,y, bs = "ts"),
                ddf.obj =  df.dd,
                segment.data = segdata,
                observation.data = obsdata_dd_mod,
                method="REML")

dd.dsm.xy.te  <-  dsm(count ~ te(x,y, bs = "ts"),
                   ddf.obj =  df.dd,
                   segment.data = segdata,
                   observation.data = obsdata_dd_mod,
                   method="REML")


## Tweedie ----
dd.dsm.xy  <-  dsm(count~s(x,y),
                   ddf.obj =  df.dd,
                   segment.data = segdata,
                   observation.data = obsdata_dd_mod,
                   family = tw(link = "log"),
                   method="REML")

# summary(dd.dsm.xy)
# appraise(dd.dsm.xy)
# draw(dd.dsm.xy)

# year ----
dd.dsm.xy.year  <-  dsm(count ~ s(x,y) +
                        s(Ano),
                     ddf.obj =  df.dd,
                     segment.data = segdata,
                     observation.data = obsdata_dd_mod,
                     family = tw(link = "log"),
                     method="REML")

# summary(dd.dsm.xy.year)

# appraise(dd.dsm.xy.year)
# draw(dd.dsm.xy.year)

# season ----
dd.dsm.xy.season  <-  dsm(count ~ s(x,y) +
                             season ,
                          ddf.obj =  df.dd,
                          segment.data = segdata,
                          observation.data = obsdata_dd_mod,
                          family = tw(link = "log"),
                          method="REML")

# summary(dd.dsm.xy.season)
# appraise(dd.dsm.xy.season)

# draw(dd.dsm.xy.season, residuals = FALSE)
# anova(dd.dsm.xy.season)

# season.year ----
dd.dsm.xy.season.year  <-  dsm(count ~ s(x,y) +
                                  season + s(Ano),
                               ddf.obj =  df.dd,
                               segment.data = segdata,
                               observation.data = obsdata_dd_mod,
                               family = tw(link = "log"),
                               method="REML")

# summary(dd.dsm.xy.season.year)
# appraise(dd.dsm.xy.season.year)

# draw(dd.dsm.xy.season.year, residuals = FALSE)
# anova(dd.dsm.xy.season.year)

# Set: environmental variable + season + year ---------
## slope ----
dd.dsm.xy.year.season.slope  <- dsm(count ~ s(x,y) +
                        season + s(Ano) +
                       s(slope),
                     ddf.obj =  df.dd,
                     segment.data = segdata,
                     observation.data = obsdata_dd_mod,
                     family = tw(link = "log"),
                     method="REML")

# summary(dd.dsm.xy.year.season.slope)
# appraise(dd.dsm.xy.year.season.slope)

# draw(dd.dsm.xy.year.season.slope, residuals = FALSE)

## grad ----
dd.dsm.xy.year.season.grad  <- dsm(count ~ s(x,y) +
                       season + s(Ano) +
                      s(grad),
                    ddf.obj =  df.dd,
                    segment.data = segdata,
                    observation.data = obsdata_dd_mod,
                    family = tw(link = "log"),
                    method="REML")

# summary(dd.dsm.xy.year.season.grad)
# appraise(dd.dsm.xy.year.season.grad)

# draw(dd.dsm.xy.year.season.grad, residuals = FALSE)

## depth ----
dd.dsm.xy.year.season.depth  <-  dsm(count ~ s(x,y) +
                         season + s(Ano) +
                        s(depth),
                      ddf.obj =  df.dd,
                      segment.data = segdata,
                      observation.data = obsdata_dd_mod,
                      family = tw(link = "log"),
                      method="REML")

## VelVert ----
dd.dsm.xy.year.season.VelVert  <-  dsm(count ~ s(x,y) +
                                         season + s(Ano) +
                                         s(VelVert),
                                       ddf.obj =  df.dd,
                                       segment.data = segdata,
                                       observation.data = obsdata_dd_mod,
                                       family = tw(link = "log"),
                                       method="REML")

# summary(dd.dsm.xy.year.season.depth)
# appraise(dd.dsm.xy.year.season.depth)

# draw(dd.dsm.xy.year.season.depth, residuals = FALSE)


## sst ----
dd.dsm.xy.year.season.sst  <- dsm(count ~ s(x,y) +
                      season + s(Ano) +
                     s(sst),
                   ddf.obj =  df.dd,
                   segment.data = segdata,
                   observation.data = obsdata_dd_mod,
                   family = tw(link = "log"),
                   method="REML")

# summary(dd.dsm.xy.year.season.sst)
# appraise(dd.dsm.xy.year.season.sst)

# draw(dd.dsm.xy.year.season.sst, residuals = FALSE)


## clo ----
dd.dsm.xy.year.season.clo  <- dsm(count ~ s(x,y) +
                      season + s(Ano) +
                     s(clo),
                   ddf.obj =  df.dd,
                   segment.data = segdata,
                   observation.data = obsdata_dd_mod,
                   family = tw(link = "log"),
                   method="REML")

# summary(dd.dsm.xy.year.season.clo)
# appraise(dd.dsm.xy.year.season.clo)

# draw(dd.dsm.xy.year.season.clo, residuals = FALSE)

## dist.up ----
dd.dsm.xy.year.season.dist.up  <- dsm(count ~ s(x,y) +
                          season + s(Ano) +
                         s(dist.up),
                       ddf.obj =  df.dd,
                       segment.data = segdata,
                       observation.data = obsdata_dd_mod,
                       family = tw(link = "log"),
                       method="REML")

# summary(dd.dsm.xy.year.season.dist.up)
# appraise(dd.dsm.xy.year.season.dist.up)

# draw(dd.dsm.xy.year.season.dist.up, residuals = FALSE)

## dist.up.grad ----
dd.dsm.xy.year.season.dist.up.grad  <- dsm(count ~ s(x,y) +
                               season + s(Ano) +
                              s(grad) +
                              s(dist.up),
                            ddf.obj =  df.dd,
                            segment.data = segdata,
                            observation.data = obsdata_dd_mod,
                            family = tw(link = "log"),
                            method="REML")

# summary(dd.dsm.xy.year.season.dist.up.grad)
# appraise(dd.dsm.xy.year.season.dist.up.grad)

# draw(dd.dsm.xy.year.season.dist.up.grad, residuals = FALSE)


# Set: environmental variable + season (no year) ---------
#   count ~ s(x,y) +  season + s(env)
## season + slope ----
dd.dsm.xy.season.slope  <- dsm(count ~ s(x,y) +
                               season +
                              s(slope),
                            ddf.obj =  df.dd,
                            segment.data = segdata,
                            observation.data = obsdata_dd_mod,
                            family = tw(link = "log"),
                            method="REML")

# summary(dd.dsm.xy.season.slope)
# appraise(dd.dsm.xy.season.slope)
# draw(dd.dsm.xy.season.slope, residuals = FALSE)

## season + grad ----
dd.dsm.xy.season.grad  <- dsm(count ~ s(x,y) +
                              season +
                             s(grad),
                           ddf.obj =  df.dd,
                           segment.data = segdata,
                           observation.data = obsdata_dd_mod,
                           family = tw(link = "log"),
                           method="REML")

# summary(dd.dsm.xy.season.grad)
# appraise(dd.dsm.xy.season.grad)
# draw(dd.dsm.xy.season.grad, residuals = FALSE)

## season + sst ----
dd.dsm.xy.season.sst  <- dsm(count ~ s(x,y) +
                             season +
                            s(sst),
                          ddf.obj =  df.dd,
                          segment.data = segdata,
                          observation.data = obsdata_dd_mod,
                          family = tw(link = "log"),
                          method="REML")

# summary(dd.dsm.xy.season.sst)
# appraise(dd.dsm.xy.season.sst)
# draw(dd.dsm.xy.season.sst, residuals = FALSE)

## season + clo ----
dd.dsm.xy.season.clo  <- dsm(count ~ s(x,y) +
                             season +
                            s(clo),
                          ddf.obj =  df.dd,
                          segment.data = segdata,
                          observation.data = obsdata_dd_mod,
                          family = tw(link = "log"),
                          method="REML")

# summary(dd.dsm.xy.season.clo)
# appraise(dd.dsm.xy.season.clo)
# draw(dd.dsm.xy.season.clo, residuals = FALSE)

## season + dist.up ----
dd.dsm.xy.season.dist.up  <- dsm(count ~ s(x,y) +
                                 season +
                                s(dist.up),
                              ddf.obj =  df.dd,
                              segment.data = segdata,
                              observation.data = obsdata_dd_mod,
                              family = tw(link = "log"),
                              method="REML")

# summary(dd.dsm.xy.season.dist.up)
# appraise(dd.dsm.xy.season.dist.up)
# draw(dd.dsm.xy.season.dist.up, residuals = FALSE)

## season + depth ----
dd.dsm.xy.season.depth  <- dsm(count ~ s(x,y) +
                               season +
                              s(depth),
                            ddf.obj =  df.dd,
                            segment.data = segdata,
                            observation.data = obsdata_dd_mod,
                            family = tw(link = "log"),
                            method="REML")

## VelVert ----
dd.dsm.xy.season.VelVert  <- dsm(count ~ s(x,y) +
                                   season +
                                   s(VelVert),
                                 ddf.obj =  df.dd,
                                 segment.data = segdata,
                                 observation.data = obsdata_dd_mod,
                                 family = tw(link = "log"),
                                 method="REML")

# summary(dd.dsm.xy.season.depth)
# appraise(dd.dsm.xy.season.depth)
# draw(dd.dsm.xy.season.depth, residuals = FALSE)

# Set: environmental variable + s(Ano) (no season) ------
#   count ~ s(x,y) + s(Ano) + s(env)
## year + slope ----
dd.dsm.xy.year.slope  <- dsm(count ~ s(x,y) +
                           s(Ano) +
                           s(slope),
                         ddf.obj =  df.dd,
                         segment.data = segdata,
                         observation.data = obsdata_dd_mod,
                         family = tw(link = "log"),
                         method="REML")

# summary(dd.dsm.xy.year.slope)
# appraise(dd.dsm.xy.year.slope)
# draw(dd.dsm.xy.year.slope, residuals = FALSE)

## year + grad ----
dd.dsm.xy.year.grad  <- dsm(count ~ s(x,y) +
                          s(Ano) +
                          s(grad),
                        ddf.obj =  df.dd,
                        segment.data = segdata,
                        observation.data = obsdata_dd_mod,
                        family = tw(link = "log"),
                        method="REML")

# summary(dd.dsm.xy.year.grad)
# appraise(dd.dsm.xy.year.grad)
# draw(dd.dsm.xy.year.grad, residuals = FALSE)

## year + sst ----
dd.dsm.xy.year.sst  <- dsm(count ~ s(x,y) +
                         s(Ano) +
                         s(sst),
                       ddf.obj =  df.dd,
                       segment.data = segdata,
                       observation.data = obsdata_dd_mod,
                       family = tw(link = "log"),
                       method="REML")

# summary(dd.dsm.xy.year.sst)
# appraise(dd.dsm.xy.year.sst)
# draw(dd.dsm.xy.year.sst, residuals = FALSE)

## year + clo ----
dd.dsm.xy.year.clo  <- dsm(count ~ s(x,y) +
                         s(Ano) +
                         s(clo),
                       ddf.obj =  df.dd,
                       segment.data = segdata,
                       observation.data = obsdata_dd_mod,
                       family = tw(link = "log"),
                       method="REML")

# summary(dd.dsm.xy.year.clo)
# appraise(dd.dsm.xy.year.clo)
# draw(dd.dsm.xy.year.clo, residuals = FALSE)

## year + dist.up ----
dd.dsm.xy.year.dist.up  <- dsm(count ~ s(x,y) +
                             s(Ano) +
                             s(dist.up),
                           ddf.obj =  df.dd,
                           segment.data = segdata,
                           observation.data = obsdata_dd_mod,
                           family = tw(link = "log"),
                           method="REML")

# summary(dd.dsm.xy.year.dist.up)
# appraise(dd.dsm.xy.year.dist.up)
# draw(dd.dsm.xy.year.dist.up, residuals = FALSE)

## year + depth ----
dd.dsm.xy.year.depth  <- dsm(count ~ s(x,y) +
                           s(Ano) +
                           s(depth),
                         ddf.obj =  df.dd,
                         segment.data = segdata,
                         observation.data = obsdata_dd_mod,
                         family = tw(link = "log"),
                         method="REML")

## VelVert ----
dd.dsm.xy.year.VelVert  <- dsm(count ~ s(x,y) +
                                 s(Ano) +
                                 s(VelVert),
                               ddf.obj =  df.dd,
                               segment.data = segdata,
                               observation.data = obsdata_dd_mod,
                               family = tw(link = "log"),
                               method="REML")

# summary(dd.dsm.xy.year.depth)
# appraise(dd.dsm.xy.year.depth)
# draw(dd.dsm.xy.year.depth, residuals = FALSE)

# Annual (x, y) surface + season ----
# The fs (factor-smooth) basis uses a single shared smoothing parameter across years
# and includes per-year intercepts. It borrows strength across years, at the cost of assuming all years share the same smoothness.
dd.dsm.xy.fsyear.season <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                 season,
                               ddf.obj = df.dd,
                               segment.data = segdata,
                               observation.data = obsdata_dd_mod,
                               family = tw(link = "log"),
                               method = "REML")

# summary(dd.dsm.xy.fsyear.season)
# appraise(dd.dsm.xy.fsyear.season)

# this does not work with gratia::draw
# draw(dd.dsm.xy.fsyear.season, residuals = FALSE)
# here is the partial effects plot
m  <- dd.dsm.xy.fsyear.season
yf <- levels(m$model$year_fac)

# fine x,y grid over the surveyed extent, replicated per year
gr <- expand.grid(
  x = seq(min(m$model$x), max(m$model$x), length.out = 120),
  y = seq(min(m$model$y), max(m$model$y), length.out = 120)
)
grid_yr <- do.call(rbind, lapply(yf, function(yy)
  transform(gr,
            year_fac = factor(yy, levels = yf),
            season   = factor(levels(m$model$season)[1], levels = levels(m$model$season)))
))

# partial effect of the spatial smooth, per year (link scale, centred — as draw() shows)
tm <- predict(m, newdata = grid_yr, type = "terms", off.set = 1)   # off.set dummy; irrelevant to terms
grid_yr$s_xy <- tm[, "s(x,y,year_fac)"]

draw.fs <- ggplot(grid_yr, aes(x, y, fill = s_xy)) +
  geom_raster() +
  facet_wrap(~ year_fac) +
  scale_fill_viridis_c(option = "turbo", name = "s(x,y)") +
  coord_equal() +
  labs(title = 's(x, y, year_fac, bs = "fs") — partial spatial effect by year',
       x = "x", y = "y") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

draw.fs

gratia::parametric_effects(dd.dsm.xy.fsyear.season, term = "season")
gratia::draw(gratia::parametric_effects(dd.dsm.xy.fsyear.season, term = "season")) +
  theme_bw()

# Full fs model set (year-varying spatial, factor-smooth, shrunk) ----
# Same s(x, y, year_fac, bs = "fs") term as above, crossed with season and each
# environmental covariate (mirrors the by = year_fac set below).

## fsyear (no season) ----
dd.dsm.xy.fsyear <- dsm(count ~ s(x, y, year_fac, bs = "fs"),
                        ddf.obj = df.dd,
                        segment.data = segdata,
                        observation.data = obsdata_dd_mod,
                        family = tw(link = "log"),
                        method = "REML")

# summary(dd.dsm.xy.fsyear)
# appraise(dd.dsm.xy.fsyear)

# Set: fs year + season + environmental variable ----
## slope ----
dd.dsm.xy.fsyear.season.slope <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                       season +
                                       s(slope),
                                     ddf.obj = df.dd,
                                     segment.data = segdata,
                                     observation.data = obsdata_dd_mod,
                                     family = tw(link = "log"),
                                     method = "REML")

## grad ----
dd.dsm.xy.fsyear.season.grad <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                      season +
                                      s(grad),
                                    ddf.obj = df.dd,
                                    segment.data = segdata,
                                    observation.data = obsdata_dd_mod,
                                    family = tw(link = "log"),
                                    method = "REML")

## sst ----
dd.dsm.xy.fsyear.season.sst <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                     season +
                                     s(sst),
                                   ddf.obj = df.dd,
                                   segment.data = segdata,
                                   observation.data = obsdata_dd_mod,
                                   family = tw(link = "log"),
                                   method = "REML")

## clo ----
dd.dsm.xy.fsyear.season.clo <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                     season +
                                     s(clo),
                                   ddf.obj = df.dd,
                                   segment.data = segdata,
                                   observation.data = obsdata_dd_mod,
                                   family = tw(link = "log"),
                                   method = "REML")

## dist.up ----
dd.dsm.xy.fsyear.season.dist.up <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                         season +
                                         s(dist.up),
                                       ddf.obj = df.dd,
                                       segment.data = segdata,
                                       observation.data = obsdata_dd_mod,
                                       family = tw(link = "log"),
                                       method = "REML")

## depth ----
dd.dsm.xy.fsyear.season.depth <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                       season +
                                       s(depth),
                                     ddf.obj = df.dd,
                                     segment.data = segdata,
                                     observation.data = obsdata_dd_mod,
                                     family = tw(link = "log"),
                                     method = "REML")

## VelVert ----
dd.dsm.xy.fsyear.season.VelVert <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                         season +
                                         s(VelVert),
                                       ddf.obj = df.dd,
                                       segment.data = segdata,
                                       observation.data = obsdata_dd_mod,
                                       family = tw(link = "log"),
                                       method = "REML")

# Set: fs year + environmental variable (no season) ----
## slope ----
dd.dsm.xy.fsyear.slope <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                s(slope),
                              ddf.obj = df.dd,
                              segment.data = segdata,
                              observation.data = obsdata_dd_mod,
                              family = tw(link = "log"),
                              method = "REML")

## grad ----
dd.dsm.xy.fsyear.grad <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                               s(grad),
                             ddf.obj = df.dd,
                             segment.data = segdata,
                             observation.data = obsdata_dd_mod,
                             family = tw(link = "log"),
                             method = "REML")

## sst ----
dd.dsm.xy.fsyear.sst <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                              s(sst),
                            ddf.obj = df.dd,
                            segment.data = segdata,
                            observation.data = obsdata_dd_mod,
                            family = tw(link = "log"),
                            method = "REML")

## clo ----
dd.dsm.xy.fsyear.clo <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                              s(clo),
                            ddf.obj = df.dd,
                            segment.data = segdata,
                            observation.data = obsdata_dd_mod,
                            family = tw(link = "log"),
                            method = "REML")

## dist.up ----
dd.dsm.xy.fsyear.dist.up <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                  s(dist.up),
                                ddf.obj = df.dd,
                                segment.data = segdata,
                                observation.data = obsdata_dd_mod,
                                family = tw(link = "log"),
                                method = "REML")

## depth ----
dd.dsm.xy.fsyear.depth <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                s(depth),
                              ddf.obj = df.dd,
                              segment.data = segdata,
                              observation.data = obsdata_dd_mod,
                              family = tw(link = "log"),
                              method = "REML")

## VelVert ----
dd.dsm.xy.fsyear.VelVert <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                  s(VelVert),
                                ddf.obj = df.dd,
                                segment.data = segdata,
                                observation.data = obsdata_dd_mod,
                                family = tw(link = "log"),
                                method = "REML")

# Annual (x, y) surface via by = year_fac (fixed, unshrunk per-year effects) ----
# Unlike the fs basis above (year as a random effect, one shared smoothing
# parameter), s(x, y, by = year_fac) fits a SEPARATE, unshrunk 2-D surface for
# each year (its own smoothing parameter per year). A by-factor smooth is
# centred within each level, so year_fac must also enter as a parametric main
# effect to carry the per-year mean level.

## byyear (no season) ----
dd.dsm.xy.byyear  <- dsm(count ~ s(x, y, by = year_fac) +
                           year_fac,
                         ddf.obj = df.dd,
                         segment.data = segdata,
                         observation.data = obsdata_dd_mod,
                         family = tw(link = "log"),
                         method="REML")

# summary(dd.dsm.xy.byyear)
# appraise(dd.dsm.xy.byyear)

## byyear + season ----
dd.dsm.xy.byyear.season  <- dsm(count ~ s(x, y, by = year_fac) +
                                  year_fac +
                                  season,
                                ddf.obj = df.dd,
                                segment.data = segdata,
                                observation.data = obsdata_dd_mod,
                                family = tw(link = "log"),
                                method="REML")

# summary(dd.dsm.xy.byyear.season)
# appraise(dd.dsm.xy.byyear.season)

# Set: by = year_fac + season + environmental variable ----
## slope ----
dd.dsm.xy.byyear.season.slope <- dsm(count ~ s(x, y, by = year_fac) +
                                       year_fac +
                                       season +
                                       s(slope),
                                     ddf.obj = df.dd,
                                     segment.data = segdata,
                                     observation.data = obsdata_dd_mod,
                                     family = tw(link = "log"),
                                     method="REML")

## grad ----
dd.dsm.xy.byyear.season.grad <- dsm(count ~ s(x, y, by = year_fac) +
                                      year_fac +
                                      season +
                                      s(grad),
                                    ddf.obj = df.dd,
                                    segment.data = segdata,
                                    observation.data = obsdata_dd_mod,
                                    family = tw(link = "log"),
                                    method="REML")

## sst ----
dd.dsm.xy.byyear.season.sst <- dsm(count ~ s(x, y, by = year_fac) +
                                     year_fac +
                                     season +
                                     s(sst),
                                   ddf.obj = df.dd,
                                   segment.data = segdata,
                                   observation.data = obsdata_dd_mod,
                                   family = tw(link = "log"),
                                   method="REML")

## clo ----
dd.dsm.xy.byyear.season.clo <- dsm(count ~ s(x, y, by = year_fac) +
                                     year_fac +
                                     season +
                                     s(clo),
                                   ddf.obj = df.dd,
                                   segment.data = segdata,
                                   observation.data = obsdata_dd_mod,
                                   family = tw(link = "log"),
                                   method="REML")

## dist.up ----
dd.dsm.xy.byyear.season.dist.up <- dsm(count ~ s(x, y, by = year_fac) +
                                         year_fac +
                                         season +
                                         s(dist.up),
                                       ddf.obj = df.dd,
                                       segment.data = segdata,
                                       observation.data = obsdata_dd_mod,
                                       family = tw(link = "log"),
                                       method="REML")

## depth ----
dd.dsm.xy.byyear.season.depth <- dsm(count ~ s(x, y, by = year_fac) +
                                       year_fac +
                                       season +
                                       s(depth),
                                     ddf.obj = df.dd,
                                     segment.data = segdata,
                                     observation.data = obsdata_dd_mod,
                                     family = tw(link = "log"),
                                     method="REML")

## VelVert ----
dd.dsm.xy.byyear.season.VelVert <- dsm(count ~ s(x, y, by = year_fac) +
                                         year_fac +
                                         season +
                                         s(VelVert),
                                       ddf.obj = df.dd,
                                       segment.data = segdata,
                                       observation.data = obsdata_dd_mod,
                                       family = tw(link = "log"),
                                       method="REML")

# Set: by = year_fac + environmental variable (no season) ----
## slope ----
dd.dsm.xy.byyear.slope <- dsm(count ~ s(x, y, by = year_fac) +
                                year_fac +
                                s(slope),
                              ddf.obj = df.dd,
                              segment.data = segdata,
                              observation.data = obsdata_dd_mod,
                              family = tw(link = "log"),
                              method="REML")

## grad ----
dd.dsm.xy.byyear.grad <- dsm(count ~ s(x, y, by = year_fac) +
                               year_fac +
                               s(grad),
                             ddf.obj = df.dd,
                             segment.data = segdata,
                             observation.data = obsdata_dd_mod,
                             family = tw(link = "log"),
                             method="REML")

## sst ----
dd.dsm.xy.byyear.sst <- dsm(count ~ s(x, y, by = year_fac) +
                              year_fac +
                              s(sst),
                            ddf.obj = df.dd,
                            segment.data = segdata,
                            observation.data = obsdata_dd_mod,
                            family = tw(link = "log"),
                            method="REML")

## clo ----
dd.dsm.xy.byyear.clo <- dsm(count ~ s(x, y, by = year_fac) +
                              year_fac +
                              s(clo),
                            ddf.obj = df.dd,
                            segment.data = segdata,
                            observation.data = obsdata_dd_mod,
                            family = tw(link = "log"),
                            method="REML")

## dist.up ----
dd.dsm.xy.byyear.dist.up <- dsm(count ~ s(x, y, by = year_fac) +
                                  year_fac +
                                  s(dist.up),
                                ddf.obj = df.dd,
                                segment.data = segdata,
                                observation.data = obsdata_dd_mod,
                                family = tw(link = "log"),
                                method="REML")

## depth ----
dd.dsm.xy.byyear.depth <- dsm(count ~ s(x, y, by = year_fac) +
                                year_fac +
                                s(depth),
                              ddf.obj = df.dd,
                              segment.data = segdata,
                              observation.data = obsdata_dd_mod,
                              family = tw(link = "log"),
                              method="REML")

## VelVert ----
dd.dsm.xy.byyear.VelVert <- dsm(count ~ s(x, y, by = year_fac) +
                                  year_fac +
                                  s(VelVert),
                                ddf.obj = df.dd,
                                segment.data = segdata,
                                observation.data = obsdata_dd_mod,
                                family = tw(link = "log"),
                                method="REML")
# the seven environmental covariates, single-sourced so the model set and the
# selection-table labels cannot drift apart
.env7 <- c("slope", "grad", "sst", "clo", "dist.up", "depth", "VelVert")

# Model selection -----
# NAME-DRIVEN table. The previous version passed three parallel POSITIONAL
# vectors to AIC() / Dev = c(...) / model = c(...); with 57 models per species
# inserting one model silently shifted every label below it by a row. Here each
# row is looked up by object name, so a name that does not resolve is reported
# rather than quietly mislabelling the table.
#
# dd.dsm.xy.ts and dd.dsm.xy.te are deliberately left out, as before.

.lbl <- function(nm, lab) setNames(lab, nm)

# p_hat — the Tweedie power parameter p, as estimated by tw() for that model.
#
# WHAT IT IS. The Tweedie distributions used here are indexed by a power
# parameter p in (1, 2), which sets how the variance scales with the mean:
#   Var(y) = phi * mu^p
# p -> 1 behaves like a (quasi-)Poisson, p -> 2 like a Gamma. In between, the
# distribution has an atom at zero plus a continuous positive part, which is
# what makes it suit segment counts that are mostly zero with occasional large
# groups. Lower p puts relatively more mass on exact zeros; higher p spreads
# more of the variance into a heavy right tail.
#
# WHY IT IS IN THE TABLE. p used to be fixed per species by the AIC grid search
# in UTIL_FindTweedieP_*.R. It is now estimated inside each fit by tw(), which
# costs exactly 1 degree of freedom (family$n.theta == 1), so AIC stays
# comparable across rows even though each row has its own p.
#
# HOW TO READ IT. The point is the SPREAD down the column. If every model lands
# on a similar p, they are competing on the MEAN structure, which is what this
# table is meant to compare. A row whose p sits well away from the rest is
# partly winning by fitting the dispersion/tail differently rather than the
# spatial or temporal structure, and its AIC advantage should be read with that
# in mind. Common dolphins previously used a fixed p of 1.58.
#
# NA means that model was fitted with a fixed-p family rather than tw().
.p_hat <- function(n) {
  fam <- get(n)$family
  if (is.null(fam$getTheta)) NA_real_ else round(fam$getTheta(TRUE), 4)
}

.dd_labels <- c(
  # shared thin-plate surface
  .lbl("dd.dsm.xy",             "count ~ s(x,y)"),
  .lbl("dd.dsm.xy.season",      "count ~ s(x,y) + season "),
  .lbl("dd.dsm.xy.season.year", "count ~ s(x,y) + season + s(Ano)"),
  setNames(sprintf("count ~ s(x,y) + season + s(Ano) + s(%s)", .env7),
           sprintf("dd.dsm.xy.year.season.%s", .env7)),
  .lbl("dd.dsm.xy.year",        "count ~ s(x,y) + s(Ano)"),
  setNames(sprintf("count ~ s(x,y) + season + s(%s)", .env7),
           sprintf("dd.dsm.xy.season.%s", .env7)),
  setNames(sprintf("count ~ s(x,y) + s(Ano) + s(%s)", .env7),
           sprintf("dd.dsm.xy.year.%s", .env7)),
  # fs factor-smooth (year-varying, shrunk)
  .lbl("dd.dsm.xy.fsyear.season", "count ~ s(x,y,year_fac,bs=fs) + season"),
  .lbl("dd.dsm.xy.fsyear",        "count ~ s(x,y,year_fac,bs=fs)"),
  setNames(sprintf("count ~ s(x,y,year_fac,bs=fs) + season + s(%s)", .env7),
           sprintf("dd.dsm.xy.fsyear.season.%s", .env7)),
  setNames(sprintf("count ~ s(x,y,year_fac,bs=fs) + s(%s)", .env7),
           sprintf("dd.dsm.xy.fsyear.%s", .env7)),
  # by = year_fac (year-varying, unshrunk)
  .lbl("dd.dsm.xy.byyear",        "count ~ s(x,y,by=year_fac) + year_fac"),
  .lbl("dd.dsm.xy.byyear.season", "count ~ s(x,y,by=year_fac) + year_fac + season"),
  setNames(sprintf("count ~ s(x,y,by=year_fac) + year_fac + season + s(%s)", .env7),
           sprintf("dd.dsm.xy.byyear.season.%s", .env7)),
  setNames(sprintf("count ~ s(x,y,by=year_fac) + year_fac + s(%s)", .env7),
           sprintf("dd.dsm.xy.byyear.%s", .env7))
)

.ms_table <- function(labels) {
  present <- vapply(names(labels), exists, logical(1))

  # a missing model is a real problem (typo, or a fit that errored)
  gone <- names(labels)[!present]
  if (length(gone))
    warning("model selection: not found in the workspace -> ",
            paste(gone, collapse = ", "), call. = FALSE)

  nm <- names(labels)[present]
  if (!length(nm)) stop("model selection: no models found in the workspace")

  # AIC is only comparable across models fitted to the SAME segments. The old
  # AIC(m1, m2, ...) form checked this implicitly; check it explicitly now.
  # The likely trigger is an NA in a covariate silently dropping those rows.
  n_used <- vapply(nm, function(n) length(get(n)$y), integer(1))
  if (length(unique(n_used)) > 1L)
    warning("model selection: models were fitted to different numbers of ",
            "segments (", paste(sort(unique(n_used)), collapse = " / "),
            ") — AIC is NOT comparable across these rows.", call. = FALSE)

  aic <- vapply(nm, function(n) AIC(get(n)), numeric(1))
  out <- data.table(
    model    = unname(labels[nm]),
    df       = vapply(nm, function(n) round(attr(logLik(get(n)), "df"), 2), numeric(1)),
    AIC      = round(aic, 2),
    deltaAIC = round(aic - min(aic), 2),
    Dev      = vapply(nm, function(n) round(summary(get(n))$dev.expl, 2), numeric(1)),
    # with tw() every model estimates its own Tweedie p. AIC handles that (p
    # costs 1 df), but the spread should be VISIBLE rather than assumed: a wide
    # spread means models are competing partly on the tail/dispersion rather
    # than on the mean structure.
    p_hat    = vapply(nm, .p_hat, numeric(1))
  )
  out[order(deltaAIC)]
}

table_dd_modselection <- .ms_table(.dd_labels)
