# RETIRED 2026-08-24 - kept for provenance only, not part of the pipeline.
#
# This grid search chose a single Tweedie p per species by AIC. It has been
# superseded by family = tw(link = "log") in 4_DuskyDolphin_DSM.R, which
# estimates p inside each fit at a cost of 1 df. The grid search was a profile
# likelihood over p that paid NO df for the p it selected, and it forced every
# model in the table to share one hand-picked p.
#
p_grid <- seq(1.1, 1.9, by = 0.2)

# p_grid <- c(1.0001, 1.9999, p_grid,  1.225, 1.250, 1.275, 1.325, 1.350, 1.375, 1.31, 1.32, 1.33, 1.34)
p_grid <- c( p_grid,  1.225, 1.250, 1.275, 1.325, 1.350, 1.375, 1.31, 1.32, 1.33, 1.34)

tw_profile <- data.table(
  p     = p_grid,
  AIC   = NA_real_,
  REML  = NA_real_
)

for (i in seq_along(p_grid)) {
  fit <- tryCatch(
    dsm(count ~ s(x, y) + season,
        ddf.obj          = df.lo,
        segment.data     = segdata,
        observation.data = obsdata_lo_mod,
        family           = Tweedie(p = p_grid[i]),
        method           = "REML"),
    error = function(e) NULL
  )

  if (!is.null(fit)) {
    tw_profile[i, AIC  := AIC(fit)]
    tw_profile[i, REML := fit$gcv.ubre]  # REML score stored here in mgcv/dsm
  }
}

tw_profile$deltaAIC <- tw_profile$AIC - min(tw_profile$AIC)

# Inspect
tw_profile[order(AIC)]


ggplot(tw_profile, aes(x = p, y = deltaAIC)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = tw_profile[which.min(deltaAIC), p], lty = 2, col = "red") +
  scale_x_continuous(breaks = seq(1, 2, .1))

ggplot(tw_profile[data.table::between(x = p, lower = 1.05, upper = 1.95)], aes(x = p, y = deltaAIC)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = tw_profile[which.min(deltaAIC), p], lty = 2, col = "red") +
  scale_x_continuous(breaks = seq(1.1, 1.9, .1))

