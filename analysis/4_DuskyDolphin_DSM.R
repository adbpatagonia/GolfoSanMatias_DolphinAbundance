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

dsm.xy  <-  dsm(count ~ s(x,y, bs = "ts"),
                ddf.obj =  df.lo,
                segment.data = segdata,
                observation.data = obsdata_lo_mod,
                method="REML")

dsm.xy.te  <-  dsm(count ~ te(x,y, bs = "ts"),
                   ddf.obj =  df.lo,
                   segment.data = segdata,
                   observation.data = obsdata_lo_mod,
                   method="REML")


## Tweedie ----
dsm.xy.tw  <-  dsm(count~s(x,y),
                   ddf.obj =  df.lo,
                   segment.data = segdata,
                   observation.data = obsdata_lo_mod,
                   family = Tweedie(p = 1.31),
                   method="REML")

summary(dsm.xy.tw)
appraise(dsm.xy.tw)
draw(dsm.xy.tw)

# year ----
dsm.xy.year  <-  dsm(count ~ s(x,y) +
                       year_fac,
                     # s(Ano),
                     ddf.obj =  df.lo,
                     segment.data = segdata,
                     observation.data = obsdata_lo_mod,
                     family = Tweedie(p = 1.31),
                     method="REML")

summary(dsm.xy.year)

appraise(dsm.xy.year)
draw(dsm.xy.year)

# season ----
dsm.xy.season.tw  <-  dsm(count ~ s(x,y) +
                            -1 + season ,
                          ddf.obj =  df.lo,
                          segment.data = segdata,
                          observation.data = obsdata_lo_mod,
                          family = Tweedie(p = 1.31),
                          method="REML")

summary(dsm.xy.season.tw)
appraise(dsm.xy.season.tw)

draw(dsm.xy.season.tw, residuals = FALSE)
anova(dsm.xy.season.tw)

# season.year ----
dsm.xy.season.year.tw  <-  dsm(count ~ s(x,y) +
                                 -1 + season + s(year_fac, bs = "re"),
                               ddf.obj =  df.lo,
                               segment.data = segdata,
                               observation.data = obsdata_lo_mod,
                               family = Tweedie(p = 1.31),
                               method="REML")

summary(dsm.xy.season.year.tw)
appraise(dsm.xy.season.year.tw)

draw(dsm.xy.season.year.tw, residuals = FALSE)
anova(dsm.xy.season.year.tw)

# slope ----
dsm.xy.slope  <- dsm(count ~ s(x,y) +
                       -1 + season + s(year_fac, bs = "re") +
                       s(slope),
                     ddf.obj =  df.lo,
                     segment.data = segdata,
                     observation.data = obsdata_lo_mod,
                     family = Tweedie(p = 1.31),
                     method="REML")

summary(dsm.xy.slope)
appraise(dsm.xy.slope)

draw(dsm.xy.slope, residuals = FALSE)

# grad ----
dsm.xy.grad  <- dsm(count ~ s(x,y) +
                      -1 + season + s(year_fac, bs = "re") +
                      s(grad),
                    ddf.obj =  df.lo,
                    segment.data = segdata,
                    observation.data = obsdata_lo_mod,
                    family = Tweedie(p = 1.31),
                    method="REML")

summary(dsm.xy.grad)
appraise(dsm.xy.grad)

draw(dsm.xy.grad, residuals = FALSE)

# depth ----
# dsm.xy.depthnull  <-  dsm(count ~ s(x,y) +
#                             -1 + season +
#                             # s(depth) +
#                             s(year_fac, bs = "re"),
#                           ddf.obj =  df.lo,
#                           segment.data = segdata,
#                           observation.data = obsdata_lo_mod,
#                             family = Tweedie(p = 1.31),
#                           method="REML")

dsm.xy.depth  <-  dsm(count ~ s(x,y) +
                        -1 + season + s(year_fac, bs = "re") +
                        s(depth),
                      ddf.obj =  df.lo,
                      segment.data = segdata,
                      observation.data = obsdata_lo_mod,
                      family = Tweedie(p = 1.31),
                      method="REML")

summary(dsm.xy.depth)
appraise(dsm.xy.depth)

draw(dsm.xy.depth, residuals = FALSE)

# anova(dsm.xy.depthnull, dsm.xy.depth,   test = "F")
# AIC(dsm.xy.depth, dsm.xy.depthnull)

# sst ----
dsm.xy.sst  <- dsm(count ~ s(x,y) +
                     -1 + season + s(year_fac, bs = "re") +
                     s(sst),
                   ddf.obj =  df.lo,
                   segment.data = segdata,
                   observation.data = obsdata_lo_mod,
                   family = Tweedie(p = 1.31),
                   method="REML")

summary(dsm.xy.sst)
appraise(dsm.xy.sst)

draw(dsm.xy.sst, residuals = FALSE)


# clo ----
dsm.xy.clo  <- dsm(count ~ s(x,y) +
                     -1 + season + s(year_fac, bs = "re") +
                     s(clo),
                   ddf.obj =  df.lo,
                   segment.data = segdata,
                   observation.data = obsdata_lo_mod,
                   family = Tweedie(p = 1.31),
                   method="REML")

summary(dsm.xy.clo)
appraise(dsm.xy.clo)

draw(dsm.xy.clo, residuals = FALSE)

# dist.up ----
dsm.xy.dist.up  <- dsm(count ~ s(x,y) +
                         -1 + season + s(year_fac, bs = "re") +
                         s(dist.up),
                       ddf.obj =  df.lo,
                       segment.data = segdata,
                       observation.data = obsdata_lo_mod,
                       family = Tweedie(p = 1.31),
                       method="REML")

summary(dsm.xy.dist.up)
appraise(dsm.xy.dist.up)

draw(dsm.xy.dist.up, residuals = FALSE)

# dist.up.grad ----
dsm.xy.dist.up.grad  <- dsm(count ~ s(x,y) +
                              -1 + season + s(year_fac, bs = "re") +
                              s(grad) +
                              s(dist.up),
                            ddf.obj =  df.lo,
                            segment.data = segdata,
                            observation.data = obsdata_lo_mod,
                            family = Tweedie(p = 1.31),
                            method="REML")

summary(dsm.xy.dist.up.grad)
appraise(dsm.xy.dist.up.grad)

draw(dsm.xy.dist.up.grad, residuals = FALSE)

# Model selection -----
table_lo_modselection <- AIC(dsm.xy.tw,
                             dsm.xy.season.tw,
                             dsm.xy.season.year.tw,
                             dsm.xy.slope,
                             dsm.xy.grad,
                             dsm.xy.sst,
                             dsm.xy.clo,
                             dsm.xy.dist.up,
                             dsm.xy.depth

) %>%
  mutate(deltaAIC = round(AIC - min(AIC), 2)) %>%
  mutate(Dev = c(
    round(summary(dsm.xy.tw)$dev.expl, 2),
    round(summary(dsm.xy.season.tw)$dev.expl, 2),
    round(summary(dsm.xy.season.year.tw)$dev.expl, 2),
    round(summary(dsm.xy.slope)$dev.expl, 2),
    round(summary(dsm.xy.grad)$dev.expl, 2),
    round(summary(dsm.xy.sst)$dev.expl, 2),
    round(summary(dsm.xy.clo)$dev.expl, 2),
    round(summary(dsm.xy.dist.up)$dev.expl, 2),
    round(summary(dsm.xy.depth)$dev.expl, 2)

  )) %>%
  mutate(model = c("count ~ s(x,y)",
                   "count ~ s(x,y) + season ",
                   "count ~ s(x,y) + season + s(year, bs = 're')",
                   "count ~ s(x,y) + season + s(year, bs = 're') + s(slope)",
                   "count ~ s(x,y) + season + s(year, bs = 're') + s(grad)",
                   "count ~ s(x,y) + season + s(year, bs = 're') + s(sst)",
                   "count ~ s(x,y) + season + s(year, bs = 're') + s(clo)",
                   "count ~ s(x,y) + season + s(year, bs = 're') + s(dist.up)",
                   "count ~ s(x,y) + season + s(year, bs = 're') + s(depth)") ) %>%
  data.table() %>%
  mutate(df = round(df, 2)) %>%
  select(model, df , deltaAIC, Dev) %>%
  arrange(deltaAIC) %>%
  kable(
    align = c("l", "c","c", "c")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    position = "center",
    full_width = FALSE
  )
