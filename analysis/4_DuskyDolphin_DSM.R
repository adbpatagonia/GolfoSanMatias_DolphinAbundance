# ADB
# 2026-04-20

# Estimate abundance of dusky and common dolphins
# in the San Matias Gulf, Argentina
# using density surface models as presented by
# Miller et al 2013
# Spatial models for distance sampling data: recent developments and future directions
# 10.1111/2041-210X.12105

# This is the DSM file for dusky dolphins
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

lo.dsm.xy.ts  <-  dsm(count ~ s(x,y, bs = "ts"),
                      ddf.obj =  df.lo,
                      segment.data = segdata,
                      observation.data = obsdata_lo_mod,
                      method="REML")

lo.dsm.xy.te  <-  dsm(count ~ te(x,y, bs = "ts"),
                      ddf.obj =  df.lo,
                      segment.data = segdata,
                      observation.data = obsdata_lo_mod,
                      method="REML")


## Tweedie ----
lo.dsm.xy  <-  dsm(count~s(x,y),
                   ddf.obj =  df.lo,
                   segment.data = segdata,
                   observation.data = obsdata_lo_mod,
                   family = Tweedie(p = 1.31),
                   method="REML")

# summary(lo.dsm.xy)
# appraise(lo.dsm.xy)
# draw(lo.dsm.xy)

# year ----
lo.dsm.xy.year  <-  dsm(count ~ s(x,y) +
                          s(Ano),
                        ddf.obj =  df.lo,
                        segment.data = segdata,
                        observation.data = obsdata_lo_mod,
                        family = Tweedie(p = 1.31),
                        method="REML")

# summary(lo.dsm.xy.year)

# appraise(lo.dsm.xy.year)
# draw(lo.dsm.xy.year)

# season ----
lo.dsm.xy.season  <-  dsm(count ~ s(x,y) +
                            season ,
                          ddf.obj =  df.lo,
                          segment.data = segdata,
                          observation.data = obsdata_lo_mod,
                          family = Tweedie(p = 1.31),
                          method="REML")

# summary(lo.dsm.xy.season)
# appraise(lo.dsm.xy.season)

# draw(lo.dsm.xy.season, residuals = FALSE)
# anova(lo.dsm.xy.season)

# season.year ----
lo.dsm.xy.season.year  <-  dsm(count ~ s(x,y) +
                                 season + s(Ano),
                               ddf.obj =  df.lo,
                               segment.data = segdata,
                               observation.data = obsdata_lo_mod,
                               family = Tweedie(p = 1.31),
                               method="REML")

# summary(lo.dsm.xy.season.year)
# appraise(lo.dsm.xy.season.year)

# draw(lo.dsm.xy.season.year, residuals = FALSE)
# anova(lo.dsm.xy.season.year)

# Set: environmental variable + season + year ---------
## slope ----
lo.dsm.xy.year.season.slope  <- dsm(count ~ s(x,y) +
                                      season + s(Ano) +
                                      s(slope),
                                    ddf.obj =  df.lo,
                                    segment.data = segdata,
                                    observation.data = obsdata_lo_mod,
                                    family = Tweedie(p = 1.31),
                                    method="REML")

# summary(lo.dsm.xy.year.season.slope)
# appraise(lo.dsm.xy.year.season.slope)

# draw(lo.dsm.xy.year.season.slope, residuals = FALSE)

## grad ----
lo.dsm.xy.year.season.grad  <- dsm(count ~ s(x,y) +
                                     season + s(Ano) +
                                     s(grad),
                                   ddf.obj =  df.lo,
                                   segment.data = segdata,
                                   observation.data = obsdata_lo_mod,
                                   family = Tweedie(p = 1.31),
                                   method="REML")

# summary(lo.dsm.xy.year.season.grad)
# appraise(lo.dsm.xy.year.season.grad)

# draw(lo.dsm.xy.year.season.grad, residuals = FALSE)

## depth ----
lo.dsm.xy.year.season.depth  <-  dsm(count ~ s(x,y) +
                                       season + s(Ano) +
                                       s(depth),
                                     ddf.obj =  df.lo,
                                     segment.data = segdata,
                                     observation.data = obsdata_lo_mod,
                                     family = Tweedie(p = 1.31),
                                     method="REML")

# summary(lo.dsm.xy.year.season.depth)
# appraise(lo.dsm.xy.year.season.depth)

# draw(lo.dsm.xy.year.season.depth, residuals = FALSE)


## sst ----
lo.dsm.xy.year.season.sst  <- dsm(count ~ s(x,y) +
                                    season + s(Ano) +
                                    s(sst),
                                  ddf.obj =  df.lo,
                                  segment.data = segdata,
                                  observation.data = obsdata_lo_mod,
                                  family = Tweedie(p = 1.31),
                                  method="REML")

# summary(lo.dsm.xy.year.season.sst)
# appraise(lo.dsm.xy.year.season.sst)

# draw(lo.dsm.xy.year.season.sst, residuals = FALSE)


## clo ----
lo.dsm.xy.year.season.clo  <- dsm(count ~ s(x,y) +
                                    season + s(Ano) +
                                    s(clo),
                                  ddf.obj =  df.lo,
                                  segment.data = segdata,
                                  observation.data = obsdata_lo_mod,
                                  family = Tweedie(p = 1.31),
                                  method="REML")

# summary(lo.dsm.xy.year.season.clo)
# appraise(lo.dsm.xy.year.season.clo)

# draw(lo.dsm.xy.year.season.clo, residuals = FALSE)

## dist.up ----
lo.dsm.xy.year.season.dist.up  <- dsm(count ~ s(x,y) +
                                        season + s(Ano) +
                                        s(dist.up),
                                      ddf.obj =  df.lo,
                                      segment.data = segdata,
                                      observation.data = obsdata_lo_mod,
                                      family = Tweedie(p = 1.31),
                                      method="REML")

# summary(lo.dsm.xy.year.season.dist.up)
# appraise(lo.dsm.xy.year.season.dist.up)

# draw(lo.dsm.xy.year.season.dist.up, residuals = FALSE)

## dist.up.grad ----
lo.dsm.xy.year.season.dist.up.grad  <- dsm(count ~ s(x,y) +
                                             season + s(Ano) +
                                             s(grad) +
                                             s(dist.up),
                                           ddf.obj =  df.lo,
                                           segment.data = segdata,
                                           observation.data = obsdata_lo_mod,
                                           family = Tweedie(p = 1.31),
                                           method="REML")

# summary(lo.dsm.xy.year.season.dist.up.grad)
# appraise(lo.dsm.xy.year.season.dist.up.grad)

# draw(lo.dsm.xy.year.season.dist.up.grad, residuals = FALSE)

print("Set: environmental variable + season (no year)")
s1 <- Sys.time()
# Set: environmental variable + season (no year) ---------
#   count ~ s(x,y) +season + s(env)
## season + slope ----
lo.dsm.xy.season.slope  <- dsm(count ~ s(x,y) +
                                 season +
                                 s(slope),
                               ddf.obj =  df.lo,
                               segment.data = segdata,
                               observation.data = obsdata_lo_mod,
                               family = Tweedie(p = 1.31),
                               method="REML")

# summary(lo.dsm.xy.season.slope)
# appraise(lo.dsm.xy.season.slope)
# draw(lo.dsm.xy.season.slope, residuals = FALSE)

## season + grad ----
lo.dsm.xy.season.grad  <- dsm(count ~ s(x,y) +
                                season +
                                s(grad),
                              ddf.obj =  df.lo,
                              segment.data = segdata,
                              observation.data = obsdata_lo_mod,
                              family = Tweedie(p = 1.31),
                              method="REML")

# summary(lo.dsm.xy.season.grad)
# appraise(lo.dsm.xy.season.grad)
# draw(lo.dsm.xy.season.grad, residuals = FALSE)

## season + sst ----
lo.dsm.xy.season.sst  <- dsm(count ~ s(x,y) +
                               season +
                               s(sst),
                             ddf.obj =  df.lo,
                             segment.data = segdata,
                             observation.data = obsdata_lo_mod,
                             family = Tweedie(p = 1.31),
                             method="REML")

# summary(lo.dsm.xy.season.sst)
# appraise(lo.dsm.xy.season.sst)
# draw(lo.dsm.xy.season.sst, residuals = FALSE)

## season + clo ----
lo.dsm.xy.season.clo  <- dsm(count ~ s(x,y) +
                               season +
                               s(clo),
                             ddf.obj =  df.lo,
                             segment.data = segdata,
                             observation.data = obsdata_lo_mod,
                             family = Tweedie(p = 1.31),
                             method="REML")

# summary(lo.dsm.xy.season.clo)
# appraise(lo.dsm.xy.season.clo)
# draw(lo.dsm.xy.season.clo, residuals = FALSE)

## season + dist.up ----
lo.dsm.xy.season.dist.up  <- dsm(count ~ s(x,y) +
                                   season +
                                   s(dist.up),
                                 ddf.obj =  df.lo,
                                 segment.data = segdata,
                                 observation.data = obsdata_lo_mod,
                                 family = Tweedie(p = 1.31),
                                 method="REML")

# summary(lo.dsm.xy.season.dist.up)
# appraise(lo.dsm.xy.season.dist.up)
# draw(lo.dsm.xy.season.dist.up, residuals = FALSE)

## season + depth ----
lo.dsm.xy.season.depth  <- dsm(count ~ s(x,y) +
                                 season +
                                 s(depth),
                               ddf.obj =  df.lo,
                               segment.data = segdata,
                               observation.data = obsdata_lo_mod,
                               family = Tweedie(p = 1.31),
                               method="REML")

# summary(lo.dsm.xy.season.depth)
# appraise(lo.dsm.xy.season.depth)
# draw(lo.dsm.xy.season.depth, residuals = FALSE)

print("Set: environmental variable + s(Ano) (no season)")
s2 <- Sys.time()
# Set: environmental variable + s(Ano) (no season) ------
#   count ~ s(x,y) + s(Ano) + s(env)
## year + slope ----
lo.dsm.xy.year.slope  <- dsm(count ~ s(x,y) +
                               s(Ano) +
                               s(slope),
                             ddf.obj =  df.lo,
                             segment.data = segdata,
                             observation.data = obsdata_lo_mod,
                             family = Tweedie(p = 1.31),
                             method="REML")

# summary(lo.dsm.xy.year.slope)
# appraise(lo.dsm.xy.year.slope)
# draw(lo.dsm.xy.year.slope, residuals = FALSE)

## year + grad ----
lo.dsm.xy.year.grad  <- dsm(count ~ s(x,y) +
                              s(Ano) +
                              s(grad),
                            ddf.obj =  df.lo,
                            segment.data = segdata,
                            observation.data = obsdata_lo_mod,
                            family = Tweedie(p = 1.31),
                            method="REML")

# summary(lo.dsm.xy.year.grad)
# appraise(lo.dsm.xy.year.grad)
# draw(lo.dsm.xy.year.grad, residuals = FALSE)

## year + sst ----
lo.dsm.xy.year.sst  <- dsm(count ~ s(x,y) +
                             s(Ano) +
                             s(sst),
                           ddf.obj =  df.lo,
                           segment.data = segdata,
                           observation.data = obsdata_lo_mod,
                           family = Tweedie(p = 1.31),
                           method="REML")

# summary(lo.dsm.xy.year.sst)
# appraise(lo.dsm.xy.year.sst)
# draw(lo.dsm.xy.year.sst, residuals = FALSE)

## year + clo ----
lo.dsm.xy.year.clo  <- dsm(count ~ s(x,y) +
                             s(Ano) +
                             s(clo),
                           ddf.obj =  df.lo,
                           segment.data = segdata,
                           observation.data = obsdata_lo_mod,
                           family = Tweedie(p = 1.31),
                           method="REML")

# summary(lo.dsm.xy.year.clo)
# appraise(lo.dsm.xy.year.clo)
# draw(lo.dsm.xy.year.clo, residuals = FALSE)

## year + dist.up ----
lo.dsm.xy.year.dist.up  <- dsm(count ~ s(x,y) +
                                 s(Ano) +
                                 s(dist.up),
                               ddf.obj =  df.lo,
                               segment.data = segdata,
                               observation.data = obsdata_lo_mod,
                               family = Tweedie(p = 1.31),
                               method="REML")

# summary(lo.dsm.xy.year.dist.up)
# appraise(lo.dsm.xy.year.dist.up)
# draw(lo.dsm.xy.year.dist.up, residuals = FALSE)

## year + depth ----
lo.dsm.xy.year.depth  <- dsm(count ~ s(x,y) +
                               s(Ano) +
                               s(depth),
                             ddf.obj =  df.lo,
                             segment.data = segdata,
                             observation.data = obsdata_lo_mod,
                             family = Tweedie(p = 1.31),
                             method="REML")

# summary(lo.dsm.xy.year.depth)
# appraise(lo.dsm.xy.year.depth)
# draw(lo.dsm.xy.year.depth, residuals = FALSE)

print("Annual (x, y) surface + season")
s3 <- Sys.time()
# Annual (x, y) surface + season ----
# The fs (factor-smooth) basis uses a single shared smoothing parameter across years
# and includes per-year intercepts. It borrows strength across years, at the cost of assuming all years share the same smoothness.
lo.dsm.xy.fsyear.season <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                 season,
                               ddf.obj = df.lo,
                               segment.data = segdata,
                               observation.data = obsdata_lo_mod,
                               family = Tweedie(p = 1.31),
                               method = "REML")

# summary(lo.dsm.xy.fsyear.season)
# appraise(lo.dsm.xy.fsyear.season)

# this does not work with gratia::draw
# draw(lo.dsm.xy.fsyear.season, residuals = FALSE)
# here is the patial effects plot
m  <- lo.dsm.xy.fsyear.season
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

gratia::parametric_effects(lo.dsm.xy.fsyear.season, term = "season")
gratia::draw(gratia::parametric_effects(lo.dsm.xy.fsyear.season, term = "season")) +
  theme_bw()

print("Full fs model set ")
s4 <- Sys.time()
# Full fs model set (year-varying spatial, factor-smooth, shrunk) ----
# Same s(x, y, year_fac, bs = "fs") term as above, crossed with season and each
# environmental covariate (mirrors the by = year_fac set below).

## fsyear (no season) ----
lo.dsm.xy.fsyear <- dsm(count ~ s(x, y, year_fac, bs = "fs"),
                        ddf.obj = df.lo,
                        segment.data = segdata,
                        observation.data = obsdata_lo_mod,
                        family = Tweedie(p = 1.31),
                        method = "REML")

# summary(lo.dsm.xy.fsyear)
# appraise(lo.dsm.xy.fsyear)

print("Set: fs year + season + environmental variable ")
s5 <- Sys.time()
# Set: fs year + season + environmental variable ----
## slope ----
lo.dsm.xy.fsyear.season.slope <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                       season +
                                       s(slope),
                                     ddf.obj = df.lo,
                                     segment.data = segdata,
                                     observation.data = obsdata_lo_mod,
                                     family = Tweedie(p = 1.31),
                                     method = "REML")

## grad ----
lo.dsm.xy.fsyear.season.grad <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                      season +
                                      s(grad),
                                    ddf.obj = df.lo,
                                    segment.data = segdata,
                                    observation.data = obsdata_lo_mod,
                                    family = Tweedie(p = 1.31),
                                    method = "REML")

## sst ----
lo.dsm.xy.fsyear.season.sst <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                     season +
                                     s(sst),
                                   ddf.obj = df.lo,
                                   segment.data = segdata,
                                   observation.data = obsdata_lo_mod,
                                   family = Tweedie(p = 1.31),
                                   method = "REML")

## clo ----
lo.dsm.xy.fsyear.season.clo <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                     season +
                                     s(clo),
                                   ddf.obj = df.lo,
                                   segment.data = segdata,
                                   observation.data = obsdata_lo_mod,
                                   family = Tweedie(p = 1.31),
                                   method = "REML")

## dist.up ----
lo.dsm.xy.fsyear.season.dist.up <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                         season +
                                         s(dist.up),
                                       ddf.obj = df.lo,
                                       segment.data = segdata,
                                       observation.data = obsdata_lo_mod,
                                       family = Tweedie(p = 1.31),
                                       method = "REML")

## depth ----
lo.dsm.xy.fsyear.season.depth <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                       season +
                                       s(depth),
                                     ddf.obj = df.lo,
                                     segment.data = segdata,
                                     observation.data = obsdata_lo_mod,
                                     family = Tweedie(p = 1.31),
                                     method = "REML")

# Set: fs year + environmental variable (no season) ----
## slope ----
lo.dsm.xy.fsyear.slope <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                s(slope),
                              ddf.obj = df.lo,
                              segment.data = segdata,
                              observation.data = obsdata_lo_mod,
                              family = Tweedie(p = 1.31),
                              method = "REML")

## grad ----
lo.dsm.xy.fsyear.grad <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                               s(grad),
                             ddf.obj = df.lo,
                             segment.data = segdata,
                             observation.data = obsdata_lo_mod,
                             family = Tweedie(p = 1.31),
                             method = "REML")

## sst ----
lo.dsm.xy.fsyear.sst <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                              s(sst),
                            ddf.obj = df.lo,
                            segment.data = segdata,
                            observation.data = obsdata_lo_mod,
                            family = Tweedie(p = 1.31),
                            method = "REML")

## clo ----
lo.dsm.xy.fsyear.clo <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                              s(clo),
                            ddf.obj = df.lo,
                            segment.data = segdata,
                            observation.data = obsdata_lo_mod,
                            family = Tweedie(p = 1.31),
                            method = "REML")

## dist.up ----
lo.dsm.xy.fsyear.dist.up <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                  s(dist.up),
                                ddf.obj = df.lo,
                                segment.data = segdata,
                                observation.data = obsdata_lo_mod,
                                family = Tweedie(p = 1.31),
                                method = "REML")

## depth ----
lo.dsm.xy.fsyear.depth <- dsm(count ~ s(x, y, year_fac, bs = "fs") +
                                s(depth),
                              ddf.obj = df.lo,
                              segment.data = segdata,
                              observation.data = obsdata_lo_mod,
                              family = Tweedie(p = 1.31),
                              method = "REML")

print("Annual (x, y) surface via by = year_fac")
s6 <- Sys.time()
# Annual (x, y) surface via by = year_fac (fixed, unshrunk per-year effects) ----
# Unlike the fs basis above (year as a random effect, one shared smoothing
# parameter), s(x, y, by = year_fac) fits a SEPARATE, unshrunk 2-D surface for
# each year (its own smoothing parameter per year). A by-factor smooth is
# centred within each level, so year_fac must also enter as a parametric main
# effect to carry the per-year mean level.

## byyear (no season) ----
lo.dsm.xy.byyear  <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                           year_fac,
                         ddf.obj = df.lo,
                         segment.data = segdata,
                         observation.data = obsdata_lo_mod,
                         family = Tweedie(p = 1.31),
                         method   = "fREML",
                         engine   = "bam")

# summary(lo.dsm.xy.byyear)
# appraise(lo.dsm.xy.byyear)

## byyear + season ----
lo.dsm.xy.byyear.season  <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                                  year_fac +
                                  season,
                                ddf.obj = df.lo,
                                segment.data = segdata,
                                observation.data = obsdata_lo_mod,
                                family = Tweedie(p = 1.31),
                                method   = "fREML",
                         engine   = "bam")

# summary(lo.dsm.xy.byyear.season)
# appraise(lo.dsm.xy.byyear.season)

print("Set: by = year_fac + season + environmental variable")
s7 <- Sys.time()
# Set: by = year_fac + season + environmental variable ----
## slope ----
lo.dsm.xy.byyear.season.slope <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                                       year_fac +
                                       season +
                                       s(slope),
                                     ddf.obj = df.lo,
                                     segment.data = segdata,
                                     observation.data = obsdata_lo_mod,
                                     family = Tweedie(p = 1.31),
                                     method   = "fREML",
                         engine   = "bam")

## grad ----
lo.dsm.xy.byyear.season.grad <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                                      year_fac +
                                      season +
                                      s(grad),
                                    ddf.obj = df.lo,
                                    segment.data = segdata,
                                    observation.data = obsdata_lo_mod,
                                    family = Tweedie(p = 1.31),
                                    method   = "fREML",
                         engine   = "bam")

## sst ----
lo.dsm.xy.byyear.season.sst <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                                     year_fac +
                                     season +
                                     s(sst),
                                   ddf.obj = df.lo,
                                   segment.data = segdata,
                                   observation.data = obsdata_lo_mod,
                                   family = Tweedie(p = 1.31),
                                   method   = "fREML",
                         engine   = "bam")

## clo ----
lo.dsm.xy.byyear.season.clo <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                                     year_fac +
                                     season +
                                     s(clo),
                                   ddf.obj = df.lo,
                                   segment.data = segdata,
                                   observation.data = obsdata_lo_mod,
                                   family = Tweedie(p = 1.31),
                                   method   = "fREML",
                         engine   = "bam")

## dist.up ----
lo.dsm.xy.byyear.season.dist.up <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                                         year_fac +
                                         season +
                                         s(dist.up),
                                       ddf.obj = df.lo,
                                       segment.data = segdata,
                                       observation.data = obsdata_lo_mod,
                                       family = Tweedie(p = 1.31),
                                       method   = "fREML",
                         engine   = "bam")

## depth ----
lo.dsm.xy.byyear.season.depth <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                                       year_fac +
                                       season +
                                       s(depth),
                                     ddf.obj = df.lo,
                                     segment.data = segdata,
                                     observation.data = obsdata_lo_mod,
                                     family = Tweedie(p = 1.31),
                                     method   = "fREML",
                         engine   = "bam")

print("Set: by = year_fac + environmental variable (no season)")
s8 <- Sys.time()
# Set: by = year_fac + environmental variable (no season) ----
## slope ----
lo.dsm.xy.byyear.slope <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                                year_fac +
                                s(slope),
                              ddf.obj = df.lo,
                              segment.data = segdata,
                              observation.data = obsdata_lo_mod,
                              family = Tweedie(p = 1.31),
                              method   = "fREML",
                         engine   = "bam")

## grad ----
lo.dsm.xy.byyear.grad <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                               year_fac +
                               s(grad),
                             ddf.obj = df.lo,
                             segment.data = segdata,
                             observation.data = obsdata_lo_mod,
                             family = Tweedie(p = 1.31),
                             method   = "fREML",
                         engine   = "bam")

## sst ----
lo.dsm.xy.byyear.sst <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                              year_fac +
                              s(sst),
                            ddf.obj = df.lo,
                            segment.data = segdata,
                            observation.data = obsdata_lo_mod,
                            family = Tweedie(p = 1.31),
                            method   = "fREML",
                         engine   = "bam")

## clo ----
lo.dsm.xy.byyear.clo <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                              year_fac +
                              s(clo),
                            ddf.obj = df.lo,
                            segment.data = segdata,
                            observation.data = obsdata_lo_mod,
                            family = Tweedie(p = 1.31),
                            method   = "fREML",
                         engine   = "bam")

## dist.up ----
lo.dsm.xy.byyear.dist.up <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                                  year_fac +
                                  s(dist.up),
                                ddf.obj = df.lo,
                                segment.data = segdata,
                                observation.data = obsdata_lo_mod,
                                family = Tweedie(p = 1.31),
                                method   = "fREML",
                         engine   = "bam")

## depth ----
lo.dsm.xy.byyear.depth <- dsm(count ~ s(x, y, by = year_fac, bs = "ts") +
                                year_fac +
                                s(depth),
                              ddf.obj = df.lo,
                              segment.data = segdata,
                              observation.data = obsdata_lo_mod,
                              family = Tweedie(p = 1.31),
                              method   = "fREML",
                         engine   = "bam")

print("Model selection")
s9 <- Sys.time()
# Model selection -----
table_lo_modselection <- AIC(lo.dsm.xy,
                             lo.dsm.xy.season,
                             lo.dsm.xy.season.year,
                             lo.dsm.xy.year.season.slope,
                             lo.dsm.xy.year.season.grad,
                             lo.dsm.xy.year.season.sst,
                             lo.dsm.xy.year.season.clo,
                             lo.dsm.xy.year.season.dist.up,
                             lo.dsm.xy.year.season.depth,
                             lo.dsm.xy.year,
                             lo.dsm.xy.season.slope,
                             lo.dsm.xy.season.grad,
                             lo.dsm.xy.season.sst,
                             lo.dsm.xy.season.clo,
                             lo.dsm.xy.season.dist.up,
                             lo.dsm.xy.season.depth,
                             lo.dsm.xy.year.slope,
                             lo.dsm.xy.year.grad,
                             lo.dsm.xy.year.sst,
                             lo.dsm.xy.year.clo,
                             lo.dsm.xy.year.dist.up,
                             lo.dsm.xy.year.depth,
                             lo.dsm.xy.fsyear.season,
                             lo.dsm.xy.fsyear,
                             lo.dsm.xy.fsyear.season.slope,
                             lo.dsm.xy.fsyear.season.grad,
                             lo.dsm.xy.fsyear.season.sst,
                             lo.dsm.xy.fsyear.season.clo,
                             lo.dsm.xy.fsyear.season.dist.up,
                             lo.dsm.xy.fsyear.season.depth,
                             lo.dsm.xy.fsyear.slope,
                             lo.dsm.xy.fsyear.grad,
                             lo.dsm.xy.fsyear.sst,
                             lo.dsm.xy.fsyear.clo,
                             lo.dsm.xy.fsyear.dist.up,
                             lo.dsm.xy.fsyear.depth,
                             lo.dsm.xy.byyear,
                             lo.dsm.xy.byyear.season,
                             lo.dsm.xy.byyear.season.slope,
                             lo.dsm.xy.byyear.season.grad,
                             lo.dsm.xy.byyear.season.sst,
                             lo.dsm.xy.byyear.season.clo,
                             lo.dsm.xy.byyear.season.dist.up,
                             lo.dsm.xy.byyear.season.depth,
                             lo.dsm.xy.byyear.slope,
                             lo.dsm.xy.byyear.grad,
                             lo.dsm.xy.byyear.sst,
                             lo.dsm.xy.byyear.clo,
                             lo.dsm.xy.byyear.dist.up,
                             lo.dsm.xy.byyear.depth

) %>%
  mutate(deltaAIC = round(AIC - min(AIC), 2)) %>%
  mutate(Dev = c(
    round(summary(lo.dsm.xy)$dev.expl, 2),
    round(summary(lo.dsm.xy.season)$dev.expl, 2),
    round(summary(lo.dsm.xy.season.year)$dev.expl, 2),
    round(summary(lo.dsm.xy.year.season.slope)$dev.expl, 2),
    round(summary(lo.dsm.xy.year.season.grad)$dev.expl, 2),
    round(summary(lo.dsm.xy.year.season.sst)$dev.expl, 2),
    round(summary(lo.dsm.xy.year.season.clo)$dev.expl, 2),
    round(summary(lo.dsm.xy.year.season.dist.up)$dev.expl, 2),
    round(summary(lo.dsm.xy.year.season.depth)$dev.expl, 2),
    round(summary(lo.dsm.xy.year)$dev.expl, 2),
    round(summary(lo.dsm.xy.season.slope)$dev.expl, 2),
    round(summary(lo.dsm.xy.season.grad)$dev.expl, 2),
    round(summary(lo.dsm.xy.season.sst)$dev.expl, 2),
    round(summary(lo.dsm.xy.season.clo)$dev.expl, 2),
    round(summary(lo.dsm.xy.season.dist.up)$dev.expl, 2),
    round(summary(lo.dsm.xy.season.depth)$dev.expl, 2),
    round(summary(lo.dsm.xy.year.slope)$dev.expl, 2),
    round(summary(lo.dsm.xy.year.grad)$dev.expl, 2),
    round(summary(lo.dsm.xy.year.sst)$dev.expl, 2),
    round(summary(lo.dsm.xy.year.clo)$dev.expl, 2),
    round(summary(lo.dsm.xy.year.dist.up)$dev.expl, 2),
    round(summary(lo.dsm.xy.year.depth)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear.season)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear.season.slope)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear.season.grad)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear.season.sst)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear.season.clo)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear.season.dist.up)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear.season.depth)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear.slope)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear.grad)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear.sst)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear.clo)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear.dist.up)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsyear.depth)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear.season)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear.season.slope)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear.season.grad)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear.season.sst)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear.season.clo)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear.season.dist.up)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear.season.depth)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear.slope)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear.grad)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear.sst)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear.clo)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear.dist.up)$dev.expl, 2),
    round(summary(lo.dsm.xy.byyear.depth)$dev.expl, 2)

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
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac",
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac + season",
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac + season + s(slope)",
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac + season + s(grad)",
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac + season + s(sst)",
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac + season + s(clo)",
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac + season + s(dist.up)",
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac + season + s(depth)",
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac + s(slope)",
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac + s(grad)",
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac + s(sst)",
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac + s(clo)",
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac + s(dist.up)",
                   "count ~ s(x,y,by=year_fac,bs=ts) + year_fac + s(depth)") ) %>%
  data.table() %>%
  mutate(df = round(df, 2)) %>%
  select(model, df , deltaAIC, Dev) %>%
  arrange(deltaAIC)

