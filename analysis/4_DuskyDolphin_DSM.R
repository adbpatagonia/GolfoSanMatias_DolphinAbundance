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
                            -1 + season ,
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
                                 -1 + season + s(Ano),
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
                       -1 + season + s(Ano) +
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
                      -1 + season + s(Ano) +
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
                        -1 + season + s(Ano) +
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
                     -1 + season + s(Ano) +
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
                     -1 + season + s(Ano) +
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
                         -1 + season + s(Ano) +
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
                              -1 + season + s(Ano) +
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


# Set: environmental variable + season (no year) ---------
#   count ~ s(x,y) + -1 + season + s(env)
## season + slope ----
lo.dsm.xy.season.slope  <- dsm(count ~ s(x,y) +
                              -1 + season +
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
                             -1 + season +
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
                            -1 + season +
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
                            -1 + season +
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
                                -1 + season +
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
                              -1 + season +
                              s(depth),
                            ddf.obj =  df.lo,
                            segment.data = segdata,
                            observation.data = obsdata_lo_mod,
                            family = Tweedie(p = 1.31),
                            method="REML")

# summary(lo.dsm.xy.season.depth)
# appraise(lo.dsm.xy.season.depth)
# draw(lo.dsm.xy.season.depth, residuals = FALSE)

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
                             lo.dsm.xy.year.depth

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
    round(summary(lo.dsm.xy.year.depth)$dev.expl, 2)

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
                   "count ~ s(x,y) + s(Ano) + s(depth)") ) %>%
  data.table() %>%
  mutate(df = round(df, 2)) %>%
  select(model, df , deltaAIC, Dev) %>%
  arrange(deltaAIC)
