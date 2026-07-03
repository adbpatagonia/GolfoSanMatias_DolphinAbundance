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
                             dd.dsm.xy.year.depth

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
    round(summary(dd.dsm.xy.year.depth)$dev.expl, 2)

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
