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
                   family = Tweedie(p = 1.58),
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
                     family = Tweedie(p = 1.58),
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
                          family = Tweedie(p = 1.58),
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
                               family = Tweedie(p = 1.58),
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
                     family = Tweedie(p = 1.58),
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
                    family = Tweedie(p = 1.58),
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
                      family = Tweedie(p = 1.58),
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
                   family = Tweedie(p = 1.58),
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
                   family = Tweedie(p = 1.58),
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
                       family = Tweedie(p = 1.58),
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
                            family = Tweedie(p = 1.58),
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
                            family = Tweedie(p = 1.58),
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
                           family = Tweedie(p = 1.58),
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
                          family = Tweedie(p = 1.58),
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
                          family = Tweedie(p = 1.58),
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
                              family = Tweedie(p = 1.58),
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
                            family = Tweedie(p = 1.58),
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
                         family = Tweedie(p = 1.58),
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
                        family = Tweedie(p = 1.58),
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
                       family = Tweedie(p = 1.58),
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
                       family = Tweedie(p = 1.58),
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
                           family = Tweedie(p = 1.58),
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
                         family = Tweedie(p = 1.58),
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
                               family = Tweedie(p = 1.58),
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
                        family = Tweedie(p = 1.58),
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
                                     family = Tweedie(p = 1.58),
                                     method = "REML")

## grad ----
dd.dsm.xy.fsyear.season.grad <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                      season +
                                      s(grad),
                                    ddf.obj = df.dd,
                                    segment.data = segdata,
                                    observation.data = obsdata_dd_mod,
                                    family = Tweedie(p = 1.58),
                                    method = "REML")

## sst ----
dd.dsm.xy.fsyear.season.sst <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                     season +
                                     s(sst),
                                   ddf.obj = df.dd,
                                   segment.data = segdata,
                                   observation.data = obsdata_dd_mod,
                                   family = Tweedie(p = 1.58),
                                   method = "REML")

## clo ----
dd.dsm.xy.fsyear.season.clo <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                     season +
                                     s(clo),
                                   ddf.obj = df.dd,
                                   segment.data = segdata,
                                   observation.data = obsdata_dd_mod,
                                   family = Tweedie(p = 1.58),
                                   method = "REML")

## dist.up ----
dd.dsm.xy.fsyear.season.dist.up <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                         season +
                                         s(dist.up),
                                       ddf.obj = df.dd,
                                       segment.data = segdata,
                                       observation.data = obsdata_dd_mod,
                                       family = Tweedie(p = 1.58),
                                       method = "REML")

## depth ----
dd.dsm.xy.fsyear.season.depth <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                       season +
                                       s(depth),
                                     ddf.obj = df.dd,
                                     segment.data = segdata,
                                     observation.data = obsdata_dd_mod,
                                     family = Tweedie(p = 1.58),
                                     method = "REML")

# Set: fs year + environmental variable (no season) ----
## slope ----
dd.dsm.xy.fsyear.slope <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                s(slope),
                              ddf.obj = df.dd,
                              segment.data = segdata,
                              observation.data = obsdata_dd_mod,
                              family = Tweedie(p = 1.58),
                              method = "REML")

## grad ----
dd.dsm.xy.fsyear.grad <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                               s(grad),
                             ddf.obj = df.dd,
                             segment.data = segdata,
                             observation.data = obsdata_dd_mod,
                             family = Tweedie(p = 1.58),
                             method = "REML")

## sst ----
dd.dsm.xy.fsyear.sst <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                              s(sst),
                            ddf.obj = df.dd,
                            segment.data = segdata,
                            observation.data = obsdata_dd_mod,
                            family = Tweedie(p = 1.58),
                            method = "REML")

## clo ----
dd.dsm.xy.fsyear.clo <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                              s(clo),
                            ddf.obj = df.dd,
                            segment.data = segdata,
                            observation.data = obsdata_dd_mod,
                            family = Tweedie(p = 1.58),
                            method = "REML")

## dist.up ----
dd.dsm.xy.fsyear.dist.up <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                  s(dist.up),
                                ddf.obj = df.dd,
                                segment.data = segdata,
                                observation.data = obsdata_dd_mod,
                                family = Tweedie(p = 1.58),
                                method = "REML")

## depth ----
dd.dsm.xy.fsyear.depth <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                s(depth),
                              ddf.obj = df.dd,
                              segment.data = segdata,
                              observation.data = obsdata_dd_mod,
                              family = Tweedie(p = 1.58),
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
                         family = Tweedie(p = 1.58),
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
                                family = Tweedie(p = 1.58),
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
                                     family = Tweedie(p = 1.58),
                                     method="REML")

## grad ----
dd.dsm.xy.byyear.season.grad <- dsm(count ~ s(x, y, by = year_fac) +
                                      year_fac +
                                      season +
                                      s(grad),
                                    ddf.obj = df.dd,
                                    segment.data = segdata,
                                    observation.data = obsdata_dd_mod,
                                    family = Tweedie(p = 1.58),
                                    method="REML")

## sst ----
dd.dsm.xy.byyear.season.sst <- dsm(count ~ s(x, y, by = year_fac) +
                                     year_fac +
                                     season +
                                     s(sst),
                                   ddf.obj = df.dd,
                                   segment.data = segdata,
                                   observation.data = obsdata_dd_mod,
                                   family = Tweedie(p = 1.58),
                                   method="REML")

## clo ----
dd.dsm.xy.byyear.season.clo <- dsm(count ~ s(x, y, by = year_fac) +
                                     year_fac +
                                     season +
                                     s(clo),
                                   ddf.obj = df.dd,
                                   segment.data = segdata,
                                   observation.data = obsdata_dd_mod,
                                   family = Tweedie(p = 1.58),
                                   method="REML")

## dist.up ----
dd.dsm.xy.byyear.season.dist.up <- dsm(count ~ s(x, y, by = year_fac) +
                                         year_fac +
                                         season +
                                         s(dist.up),
                                       ddf.obj = df.dd,
                                       segment.data = segdata,
                                       observation.data = obsdata_dd_mod,
                                       family = Tweedie(p = 1.58),
                                       method="REML")

## depth ----
dd.dsm.xy.byyear.season.depth <- dsm(count ~ s(x, y, by = year_fac) +
                                       year_fac +
                                       season +
                                       s(depth),
                                     ddf.obj = df.dd,
                                     segment.data = segdata,
                                     observation.data = obsdata_dd_mod,
                                     family = Tweedie(p = 1.58),
                                     method="REML")

# Set: by = year_fac + environmental variable (no season) ----
## slope ----
dd.dsm.xy.byyear.slope <- dsm(count ~ s(x, y, by = year_fac) +
                                year_fac +
                                s(slope),
                              ddf.obj = df.dd,
                              segment.data = segdata,
                              observation.data = obsdata_dd_mod,
                              family = Tweedie(p = 1.58),
                              method="REML")

## grad ----
dd.dsm.xy.byyear.grad <- dsm(count ~ s(x, y, by = year_fac) +
                               year_fac +
                               s(grad),
                             ddf.obj = df.dd,
                             segment.data = segdata,
                             observation.data = obsdata_dd_mod,
                             family = Tweedie(p = 1.58),
                             method="REML")

## sst ----
dd.dsm.xy.byyear.sst <- dsm(count ~ s(x, y, by = year_fac) +
                              year_fac +
                              s(sst),
                            ddf.obj = df.dd,
                            segment.data = segdata,
                            observation.data = obsdata_dd_mod,
                            family = Tweedie(p = 1.58),
                            method="REML")

## clo ----
dd.dsm.xy.byyear.clo <- dsm(count ~ s(x, y, by = year_fac) +
                              year_fac +
                              s(clo),
                            ddf.obj = df.dd,
                            segment.data = segdata,
                            observation.data = obsdata_dd_mod,
                            family = Tweedie(p = 1.58),
                            method="REML")

## dist.up ----
dd.dsm.xy.byyear.dist.up <- dsm(count ~ s(x, y, by = year_fac) +
                                  year_fac +
                                  s(dist.up),
                                ddf.obj = df.dd,
                                segment.data = segdata,
                                observation.data = obsdata_dd_mod,
                                family = Tweedie(p = 1.58),
                                method="REML")

## depth ----
dd.dsm.xy.byyear.depth <- dsm(count ~ s(x, y, by = year_fac) +
                                year_fac +
                                s(depth),
                              ddf.obj = df.dd,
                              segment.data = segdata,
                              observation.data = obsdata_dd_mod,
                              family = Tweedie(p = 1.58),
                              method="REML")
# Model selection -----
table_dd_modselection <- AIC(dd.dsm.xy,
                             dd.dsm.xy.season,
                             dd.dsm.xy.season.year,
                             dd.dsm.xy.year.season.slope,
                             dd.dsm.xy.year.season.grad,
                             dd.dsm.xy.year.season.sst,
                             dd.dsm.xy.year.season.clo,
                             dd.dsm.xy.year.season.dist.up,
                             dd.dsm.xy.year.season.depth,
                             dd.dsm.xy.year,
                             dd.dsm.xy.season.slope,
                             dd.dsm.xy.season.grad,
                             dd.dsm.xy.season.sst,
                             dd.dsm.xy.season.clo,
                             dd.dsm.xy.season.dist.up,
                             dd.dsm.xy.season.depth,
                             dd.dsm.xy.year.slope,
                             dd.dsm.xy.year.grad,
                             dd.dsm.xy.year.sst,
                             dd.dsm.xy.year.clo,
                             dd.dsm.xy.year.dist.up,
                             dd.dsm.xy.year.depth,
                             dd.dsm.xy.fsyear.season,
                             dd.dsm.xy.fsyear,
                             dd.dsm.xy.fsyear.season.slope,
                             dd.dsm.xy.fsyear.season.grad,
                             dd.dsm.xy.fsyear.season.sst,
                             dd.dsm.xy.fsyear.season.clo,
                             dd.dsm.xy.fsyear.season.dist.up,
                             dd.dsm.xy.fsyear.season.depth,
                             dd.dsm.xy.fsyear.slope,
                             dd.dsm.xy.fsyear.grad,
                             dd.dsm.xy.fsyear.sst,
                             dd.dsm.xy.fsyear.clo,
                             dd.dsm.xy.fsyear.dist.up,
                             dd.dsm.xy.fsyear.depth,
                             dd.dsm.xy.byyear,
                             dd.dsm.xy.byyear.season,
                             dd.dsm.xy.byyear.season.slope,
                             dd.dsm.xy.byyear.season.grad,
                             dd.dsm.xy.byyear.season.sst,
                             dd.dsm.xy.byyear.season.clo,
                             dd.dsm.xy.byyear.season.dist.up,
                             dd.dsm.xy.byyear.season.depth,
                             dd.dsm.xy.byyear.slope,
                             dd.dsm.xy.byyear.grad,
                             dd.dsm.xy.byyear.sst,
                             dd.dsm.xy.byyear.clo,
                             dd.dsm.xy.byyear.dist.up,
                             dd.dsm.xy.byyear.depth

) %>%
  mutate(deltaAIC = round(AIC - min(AIC), 2)) %>%
  mutate(Dev = c(
    round(summary(dd.dsm.xy)$dev.expl, 2),
    round(summary(dd.dsm.xy.season)$dev.expl, 2),
    round(summary(dd.dsm.xy.season.year)$dev.expl, 2),
    round(summary(dd.dsm.xy.year.season.slope)$dev.expl, 2),
    round(summary(dd.dsm.xy.year.season.grad)$dev.expl, 2),
    round(summary(dd.dsm.xy.year.season.sst)$dev.expl, 2),
    round(summary(dd.dsm.xy.year.season.clo)$dev.expl, 2),
    round(summary(dd.dsm.xy.year.season.dist.up)$dev.expl, 2),
    round(summary(dd.dsm.xy.year.season.depth)$dev.expl, 2),
    round(summary(dd.dsm.xy.year)$dev.expl, 2),
    round(summary(dd.dsm.xy.season.slope)$dev.expl, 2),
    round(summary(dd.dsm.xy.season.grad)$dev.expl, 2),
    round(summary(dd.dsm.xy.season.sst)$dev.expl, 2),
    round(summary(dd.dsm.xy.season.clo)$dev.expl, 2),
    round(summary(dd.dsm.xy.season.dist.up)$dev.expl, 2),
    round(summary(dd.dsm.xy.season.depth)$dev.expl, 2),
    round(summary(dd.dsm.xy.year.slope)$dev.expl, 2),
    round(summary(dd.dsm.xy.year.grad)$dev.expl, 2),
    round(summary(dd.dsm.xy.year.sst)$dev.expl, 2),
    round(summary(dd.dsm.xy.year.clo)$dev.expl, 2),
    round(summary(dd.dsm.xy.year.dist.up)$dev.expl, 2),
    round(summary(dd.dsm.xy.year.depth)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear.season)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear.season.slope)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear.season.grad)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear.season.sst)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear.season.clo)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear.season.dist.up)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear.season.depth)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear.slope)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear.grad)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear.sst)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear.clo)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear.dist.up)$dev.expl, 2),
    round(summary(dd.dsm.xy.fsyear.depth)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear.season)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear.season.slope)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear.season.grad)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear.season.sst)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear.season.clo)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear.season.dist.up)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear.season.depth)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear.slope)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear.grad)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear.sst)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear.clo)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear.dist.up)$dev.expl, 2),
    round(summary(dd.dsm.xy.byyear.depth)$dev.expl, 2)

  )) %>%
  mutate(model = c("count ~ s(x,y)",
                   "count ~ s(x,y) + season ",
                   "count ~ s(x,y) + season + s(Ano)",
                   "count ~ s(x,y) + season + s(Ano) + s(slope)",
                   "count ~ s(x,y) + season + s(Ano) + s(grad)",
                   "count ~ s(x,y) + season + s(Ano) + s(sst)",
                   "count ~ s(x,y) + season + s(Ano) + s(clo)",
                   "count ~ s(x,y) + season + s(Ano) + s(dist.up)",
                   "count ~ s(x,y) + season + s(Ano) + s(depth)",
                   "count ~ s(x,y) + s(Ano)",
                   "count ~ s(x,y) + season + s(slope)",
                   "count ~ s(x,y) + season + s(grad)",
                   "count ~ s(x,y) + season + s(sst)",
                   "count ~ s(x,y) + season + s(clo)",
                   "count ~ s(x,y) + season + s(dist.up)",
                   "count ~ s(x,y) + season + s(depth)",
                   "count ~ s(x,y) + s(Ano) + s(slope)",
                   "count ~ s(x,y) + s(Ano) + s(grad)",
                   "count ~ s(x,y) + s(Ano) + s(sst)",
                   "count ~ s(x,y) + s(Ano) + s(clo)",
                   "count ~ s(x,y) + s(Ano) + s(dist.up)",
                   "count ~ s(x,y) + s(Ano) + s(depth)",
                   "count ~ s(x,y,year_fac,bs=fs) + season",
                   "count ~ s(x,y,year_fac,bs=fs)",
                   "count ~ s(x,y,year_fac,bs=fs) + season + s(slope)",
                   "count ~ s(x,y,year_fac,bs=fs) + season + s(grad)",
                   "count ~ s(x,y,year_fac,bs=fs) + season + s(sst)",
                   "count ~ s(x,y,year_fac,bs=fs) + season + s(clo)",
                   "count ~ s(x,y,year_fac,bs=fs) + season + s(dist.up)",
                   "count ~ s(x,y,year_fac,bs=fs) + season + s(depth)",
                   "count ~ s(x,y,year_fac,bs=fs) + s(slope)",
                   "count ~ s(x,y,year_fac,bs=fs) + s(grad)",
                   "count ~ s(x,y,year_fac,bs=fs) + s(sst)",
                   "count ~ s(x,y,year_fac,bs=fs) + s(clo)",
                   "count ~ s(x,y,year_fac,bs=fs) + s(dist.up)",
                   "count ~ s(x,y,year_fac,bs=fs) + s(depth)",
                   "count ~ s(x,y,by=year_fac) + year_fac",
                   "count ~ s(x,y,by=year_fac) + year_fac + season",
                   "count ~ s(x,y,by=year_fac) + year_fac + season + s(slope)",
                   "count ~ s(x,y,by=year_fac) + year_fac + season + s(grad)",
                   "count ~ s(x,y,by=year_fac) + year_fac + season + s(sst)",
                   "count ~ s(x,y,by=year_fac) + year_fac + season + s(clo)",
                   "count ~ s(x,y,by=year_fac) + year_fac + season + s(dist.up)",
                   "count ~ s(x,y,by=year_fac) + year_fac + season + s(depth)",
                   "count ~ s(x,y,by=year_fac) + year_fac + s(slope)",
                   "count ~ s(x,y,by=year_fac) + year_fac + s(grad)",
                   "count ~ s(x,y,by=year_fac) + year_fac + s(sst)",
                   "count ~ s(x,y,by=year_fac) + year_fac + s(clo)",
                   "count ~ s(x,y,by=year_fac) + year_fac + s(dist.up)",
                   "count ~ s(x,y,by=year_fac) + year_fac + s(depth)") ) %>%
  data.table() %>%
  mutate(df = round(df, 2)) %>%
  mutate(AIC = round(AIC, 2)) %>%
  select(model, df, AIC, deltaAIC, Dev) %>%
  arrange(deltaAIC)
