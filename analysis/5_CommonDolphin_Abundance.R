# ADB
# 2026-07-06
#
# Abundance and density estimates (with 95% CI) for common dolphins
# by season × year. Three density surface models are compared against the same
# design-based (Horvitz–Thompson) estimate:
#   dd.dsm.xy.season.year   — count ~ s(x,y) + season + s(Ano)                         [primary]
#   dd.dsm.xy.fsyear.season — count ~ s(x, y, year_fac, bs = "fs") + season            [year-varying spatial, shrunk]
#   dd.dsm.xy.byyear.season — count ~ s(x, y, by = year_fac) + year_fac + season       [year-varying spatial, unshrunk]
#
# Assumes workspace contains:
#   dd.dsm.xy.season.year, dd.dsm.xy.fsyear.season, dd.dsm.xy.byyear.season,
#   pred.polys_m, survey.area_m, segdata, obsdata_dd_mod, trunc.dist_dd, target_crs

# libraries -----
library(plotly)
# functions ----
source(file.path(here::here(), "R", "lnorm_ci.R"))
source(file.path(here::here(), "R", "year_partial_effect.R"))

#  helpers ----
# total survey area (km²)
survey_area_km2 <- as.numeric(st_area(survey.area_m)) / 1e6

# x,y centroids on prediction grid (needed by the spatial smooth)
if (!all(c("x", "y") %in% names(pred.polys_m))) {
  pred.polys_m <- pred.polys_m %>%
    mutate(
      x = st_coordinates(st_centroid(geometry))[, 1],
      y = st_coordinates(st_centroid(geometry))[, 2]
    )
}

# per-cell prediction offset = ground area of each grid cell (m²).
# Cells are NOT equal-area (range ~1.10–1.17 km²), so a constant offset would
# bias abundance. Row order matches st_drop_geometry(pred.polys_m) in the loop.
cell_area_m2 <- as.numeric(st_area(pred.polys_m))

# season factor levels from training data
season_levels <- levels(obsdata_dd_mod$season)

# actual (season, year) combinations surveyed
sy_combos <- unique(segdata[, .(season, Ano)])
setorder(sy_combos, Ano, season)


# season.year ----
# dd.dsm.xy.season.year : count ~ s(x,y) + season + s(Ano)

#  abundance + density by season × year -----
results_dd <- lapply(seq_len(nrow(sy_combos)),
                     function(i) {

                       s <- sy_combos$season[i]
                       a <- sy_combos$Ano[i]

                       # drop geometry for predict() — keep as plain data.frame
                       pred_grid <- pred.polys_m %>%
                         st_drop_geometry() %>%
                         mutate(
                           season = factor(s, levels = season_levels),
                           Ano    = a
                         )

                       # variance from the GAM + detection function (delta
                       # method). dsm_var_gam is the correct estimator here:
                       # df.dd is a plain hazard-rate with NO covariates (a
                       # nobs_grp covariate was investigated and deliberately
                       # NOT used as the final detection function — see
                       # NOTE_nobsgrp_detection_function_issue.R for why, and
                       # 6_CommonDolphin_Nobs2SensitivityAnalysis.R for how the
                       # underlying question — whether the dd time trend is a
                       # changing-observer-effort artefact — is addressed
                       # instead). pred.data must be a data.frame and off.set a
                       # per-cell-area vector (m²) — the list() form errors in
                       # dsm 2.3.3.
                       vp <- dsm_var_gam(
                         dsm.obj   = dd.dsm.xy.season.year,
                         pred.data = pred_grid,
                         off.set   = cell_area_m2
                       )

                       sm       <- summary(vp)
                       # summary(vp)$cv is a 1x1 matrix (variance is a quadratic
                       # form); coerce to numeric so downstream columns stay
                       # vectors and don't pick up matrix (.V1) names.
                       N_hat    <- as.numeric(sm$pred.est)
                       cv_hat   <- as.numeric(sm$cv)
                       ci       <- lnorm_ci(N_hat, cv_hat)
                       dens     <- N_hat / survey_area_km2
                       dens_lo  <- ci$lo  / survey_area_km2
                       dens_hi  <- ci$hi  / survey_area_km2

                       data.table(
                         species  = "Common dolphin",
                         season   = s,
                         year     = a,
                         N_hat    = round(N_hat),
                         N_lo95   = round(ci$lo),
                         N_hi95   = round(ci$hi),
                         CV       = round(cv_hat, 3),
                         density  = round(dens,    4),
                         dens_lo  = round(dens_lo, 4),
                         dens_hi  = round(dens_hi, 4)
                       )
                     })

dd_abund <- rbindlist(results_dd)

# plots -----
dd_abund <- dd_abund %>%
  mutate(seas = as.numeric(case_when(
    season == "Summer" ~ "0.1",
    season == "Fall" ~ "0.35",
    season == "Winter" ~ "0.60",
    season == "Spring" ~ "0.85"
  )) )%>%
  mutate(sy = year + seas) %>%
  data.table()

# design-based (Horvitz–Thompson) estimate + CI per season × year -----
# Classical distance sampling, no spatial model. Point estimate =
# detection-corrected count / effective area, scaled to the survey region;
# uses the model's OWN per-segment counts and effective area (exp(offset),
# which folds in detection probability) so it lands on the model's scale.
# Uncertainty (Buckland et al. 2001, delta method):
#   CV(N)^2 = CV(encounter rate)^2 + CV(p-hat)^2
#     encounter-rate CV : ratio (Fewster et al. 2009 R2-type) estimator, with
#                         effective area as exposure and transects (traj_id) as
#                         the replicate unit
#     detection CV      : from the fitted detection function (constant here —
#                         df.dd has no covariates)
# NOTE: this estimate is MODEL-INDEPENDENT (raw counts + detection offset), so
# the fs section below reuses obs_strata rather than recomputing it.
seg_obs <- data.table(
  count  = as.numeric(dd.dsm.xy.season.year$y),
  earea  = exp(as.numeric(dd.dsm.xy.season.year$offset)),
  x      = dd.dsm.xy.season.year$model$x,
  y      = dd.dsm.xy.season.year$model$y,
  season = as.character(dd.dsm.xy.season.year$model$season),
  year   = as.numeric(as.character(dd.dsm.xy.season.year$model$Ano))
)

# attach transect id (the replicate unit) by matching segment centroids; x/y are
# copied verbatim from segdata into the model frame, so the match is exact.
seg_obs <- merge(seg_obs, unique(segdata[, .(x, y, traj_id)]),
                 by = c("x", "y"), all.x = TRUE, sort = FALSE)
if (anyNA(seg_obs$traj_id))
  stop("Some model segments did not match a segdata traj_id (x/y key).")

# sum counts and effective area to the transect within each stratum
tran_obs <- seg_obs[, .(count = sum(count), earea = sum(earea)),
                    by = .(season, year, traj_id)]

# detection-function CV (global; constant because df.dd has no covariates)
ddf_sm <- summary(dd.dsm.xy.season.year$ddf)
cv_p   <- as.numeric(ddf_sm$average.p.se / ddf_sm$average.p)

obs_strata <- tran_obs[, {
  k <- .N                                               # number of transects
  A <- sum(earea)
  D <- sum(count) / A                                   # density, per m^2
  v_er <- if (k > 1 && D > 0)                           # encounter-rate variance
            (k / (k - 1)) * sum((count - D * earea)^2) / A^2 else NA_real_
  .(density_obs = D * 1e6,                              # dolphins km^-2
    cv_obs      = sqrt((sqrt(v_er) / D)^2 + cv_p^2))
}, by = .(season, year)]

obs_strata[, N_obs := density_obs * survey_area_km2]    # survey-area abundance
obs_strata[is.finite(cv_obs), `:=`(                     # lognormal CI (skip NA CV)
  density_obs_lo = lnorm_ci(density_obs, cv_obs)$lo,
  density_obs_hi = lnorm_ci(density_obs, cv_obs)$hi,
  N_obs_lo       = lnorm_ci(N_obs,       cv_obs)$lo,
  N_obs_hi       = lnorm_ci(N_obs,       cv_obs)$hi
)]

dd_abund[, `:=`(season = as.character(season), year = as.numeric(year))]
dd_abund <- merge(dd_abund, obs_strata, by = c("season", "year"), all.x = TRUE)
dd_abund[, season := factor(season, levels = season_levels)]
setorder(dd_abund, sy)

# colours for the model-fit legend
fit_cols <- c("Density surface model"                      = "black",
              "design-based Horvitz–Thompson estimate" = "firebrick")

##  abundance ----
### facets -----
p.dd.N <- ggplot(dd_abund, aes(x = year)) +
  geom_ribbon(aes(ymin = N_lo95, ymax = N_hi95), alpha = 0.2) +
  geom_line(aes(y = N_hat)) +
  geom_point(aes(y = N_hat, colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = N_obs_lo, ymax = N_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = N_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  facet_wrap(~ season, scales = "free_y") +
  labs(
    title    = "Common dolphin abundance by season and year",
    subtitle = "count ~ s(x,y) + season + s(Ano)  |  ribbon = 95% CI (lognormal)",
    x        = "Year",
    y        = expression(hat(N))
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

p.dd.N

### continuous ----
p.dd.N.sy <- ggplot(dd_abund, aes(x = sy)) +
  geom_ribbon(aes(ymin = N_lo95, ymax = N_hi95), alpha = 0.2) +
  geom_line(aes(y = N_hat)) +
  geom_point(aes(y = N_hat, colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = N_obs_lo, ymax = N_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = N_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  scale_x_continuous(breaks = 2006:2018,
    minor_breaks = seq(2006.1, 2018.1, 0.25),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(
    # title    = "Common dolphin abundance — model fit",
    subtitle = "count ~ s(x,y) + season + s(Ano)  |  ribbon = 95% CI (lognormal)",
    x        = "",
    y        = expression(hat(N))
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.minor.ticks.length.x = rel(0.65))

p.dd.N.sy.noci <- ggplot(dd_abund, aes(x = sy)) +
  geom_ribbon(aes(ymin = N_lo95, ymax = N_hi95), alpha = 0.2) +
  geom_line(aes(y = N_hat)) +
  geom_point(aes(y = N_hat, colour = "Density surface model"), size = 2) +
  # geom_linerange(aes(ymin = N_obs_lo, ymax = N_obs_hi,
  #                    colour = "design-based Horvitz–Thompson estimate"),
  #                alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = N_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  scale_x_continuous(breaks = 2006:2018,
                     minor_breaks = seq(2006.1, 2018.1, 0.25),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(
    # title    = "Common dolphin abundance — model fit",
    subtitle = "count ~ s(x,y) + season + s(Ano)  |  ribbon = 95% CI (lognormal)",
    x        = "",
    y        = expression(hat(N))
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.minor.ticks.length.x = rel(0.65))


ply.dd.N.sy <- plot_ly() %>%
  # 95% CI ribbon (lognormal)
  add_ribbons(data = dd_abund,
              x = ~sy, ymin = ~N_lo95, ymax = ~N_hi95,
              color = I("black"), opacity = 0.2,
              line = list(color = 'transparent'),
              showlegend = FALSE) %>%
  # DSM line + dots
  add_trace(data = dd_abund, x = ~sy, y = ~N_hat,
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'black', width = 1.5),
            marker = list(color = 'black', size = 6),
            name = 'Density surface model') %>%
  # HT linerange (invisible markers anchor error_y)
  add_trace(data = dd_abund %>% filter(!is.na(N_obs)),
            x = ~sy, y = ~N_obs,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'firebrick', size = 0, opacity = 0),
            error_y = list(type = "data", symmetric = FALSE,
                           arrayminus = ~(N_obs - N_obs_lo),
                           array = ~(N_obs_hi - N_obs),
                           color = 'firebrick', width = 0, thickness = 1.5,
                           opacity = 0.6),
            showlegend = FALSE, hoverinfo = 'skip') %>%
  # HT triangles
  add_trace(data = dd_abund %>% filter(!is.na(N_obs)),
            x = ~sy, y = ~N_obs,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'firebrick', size = 8,
                          symbol = 'triangle-down'),
            name = 'design-based Horvitz–Thompson estimate') %>%
  layout(
    title = list(
      text = 'count ~ s(x,y) + season + s(Ano)  |  ribbon = 95% CI (lognormal)',
      font = list(size = 11), xref = 'paper', x = 0.05
    ),
    xaxis = list(
      title = '',
      tickvals = 2006:2018,
      minor = list(tickvals = seq(2006.1, 2018.1, 0.25),
                   ticklen = 4, tickcolor = '#333'),
      showminor = TRUE, showgrid = TRUE, gridcolor = '#eee'
    ),
    yaxis = list(title = list(text = 'N̂')),
    legend = list(orientation = 'h', xanchor = 'center',
                  x = 0.5, y = -0.15),
    margin = list(b = 80, t = 40),
    font = list(size = 13)
  )

## density ----
### facets -----
p.dd.D <- ggplot(dd_abund, aes(x = year)) +
  geom_ribbon(aes(ymin = dens_lo, ymax = dens_hi), alpha = 0.2) +
  geom_line(aes(y = density)) +
  geom_point(aes(y = density,     colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = density_obs_lo, ymax = density_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = density_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  facet_wrap(~ season, scales = "free_y") +
  labs(
    title    = "Common dolphin density by season and year",
    subtitle = "count ~ s(x,y) + season + s(Ano)  |  ribbon = 95% CI (lognormal)",
    x        = "Year",
    y        = expression("Dolphins km"^{-2})
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

p.dd.D

### continuous ----
p.dd.D.sy <- ggplot(dd_abund, aes(x = sy)) +
  geom_ribbon(aes(ymin = dens_lo, ymax = dens_hi), alpha = 0.2) +
  geom_line(aes(y = density)) +
  geom_point(aes(y = density,     colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = density_obs_lo, ymax = density_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = density_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +

  scale_x_continuous(breaks = 2006:2018,
                     minor_breaks = seq(2006.1, 2018.1, 0.25),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(
    # title    = "Common dolphin abundance — model fit",
    subtitle = "count ~ s(x,y) + season + s(Ano)  |  ribbon = 95% CI (lognormal)",
    x        = "",
    y        = expression("Dolphins km"^{-2})
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.minor.ticks.length.x = rel(0.65))

p.dd.D.sy

# output ----

fwrite(dd_abund, "output/CommonDolphin/Abundance/DD_abundance_season_year.csv")
ggsave(
  "output/CommonDolphin/Abundance/DD_abundance_season_year.png",
  plot   = p.dd.N,
  width  = 13,
  height = 8
)
ggsave(
  "output/CommonDolphin/Abundance/DD_density_season_year.png",
  plot   = p.dd.D,
  width  = 13,
  height = 8
)
ggsave(
  "output/CommonDolphin/Abundance/DD_abundance_modelfit.png",
  plot   = p.dd.N.sy,
  width  = 13,
  height = 8
)
ggsave(
  "output/CommonDolphin/Abundance/DD_density_modelfit.png",
  plot   = p.dd.D.sy,
  width  = 13,
  height = 8
)

# Annual (x, y) surface + season ----
# dd.dsm.xy.fsyear.season
#   count ~ s(x, y, year_fac, bs = "fs") + season
#   A separate spatial surface is estimated per year (factor-smooth basis).
#   Same pipeline as the model above; the design-based estimate
#   (obs_strata) is MODEL-INDEPENDENT and is reused, not recomputed.

# year_fac levels from the fs model; prediction grids must use these levels
yf_levels <- levels(dd.dsm.xy.fsyear.season$model$year_fac)

#  abundance + density by season × year (fs) -----
results_dd_fs <- lapply(seq_len(nrow(sy_combos)),
                        function(i) {

                          s <- sy_combos$season[i]
                          a <- sy_combos$Ano[i]

                          pred_grid <- pred.polys_m %>%
                            st_drop_geometry() %>%
                            mutate(
                              season   = factor(s, levels = season_levels),
                              year_fac = factor(as.character(a), levels = yf_levels)
                            )

                          vp <- dsm_var_gam(
                            dsm.obj   = dd.dsm.xy.fsyear.season,
                            pred.data = pred_grid,
                            off.set   = cell_area_m2
                          )

                          sm       <- summary(vp)
                          N_hat    <- as.numeric(sm$pred.est)
                          cv_hat   <- as.numeric(sm$cv)
                          ci       <- lnorm_ci(N_hat, cv_hat)
                          dens     <- N_hat / survey_area_km2
                          dens_lo  <- ci$lo  / survey_area_km2
                          dens_hi  <- ci$hi  / survey_area_km2

                          data.table(
                            species  = "Common dolphin",
                            season   = s,
                            year     = a,
                            N_hat    = round(N_hat),
                            N_lo95   = round(ci$lo),
                            N_hi95   = round(ci$hi),
                            CV       = round(cv_hat, 3),
                            density  = round(dens,    4),
                            dens_lo  = round(dens_lo, 4),
                            dens_hi  = round(dens_hi, 4)
                          )
                        })

dd_abund_fs <- rbindlist(results_dd_fs) %>%
  mutate(seas = as.numeric(case_when(
    season == "Summer" ~ "0.1",
    season == "Fall" ~ "0.35",
    season == "Winter" ~ "0.60",
    season == "Spring" ~ "0.85"
  )) ) %>%
  mutate(sy = year + seas) %>%
  data.table()

# reuse the model-independent design-based estimate (obs_strata) from above
dd_abund_fs[, `:=`(season = as.character(season), year = as.numeric(year))]
dd_abund_fs <- merge(dd_abund_fs, obs_strata, by = c("season", "year"), all.x = TRUE)
dd_abund_fs[, season := factor(season, levels = season_levels)]
setorder(dd_abund_fs, sy)

##  abundance (fs) ----
### facets -----
p.dd.N.fs <- ggplot(dd_abund_fs, aes(x = year)) +
  geom_ribbon(aes(ymin = N_lo95, ymax = N_hi95), alpha = 0.2) +
  geom_line(aes(y = N_hat)) +
  geom_point(aes(y = N_hat, colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = N_obs_lo, ymax = N_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = N_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  facet_wrap(~ season, scales = "free_y") +
  labs(
    title    = "Common dolphin abundance by season and year (year-varying spatial)",
    subtitle = "count ~ s(x, y, year_fac, bs = \"fs\") + season  |  ribbon = 95% CI (lognormal)",
    x        = "Year",
    y        = expression(hat(N))
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

p.dd.N.fs

### continuous ----
p.dd.N.sy.fs <- ggplot(dd_abund_fs, aes(x = sy)) +
  geom_ribbon(aes(ymin = N_lo95, ymax = N_hi95), alpha = 0.2) +
  geom_line(aes(y = N_hat)) +
  geom_point(aes(y = N_hat, colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = N_obs_lo, ymax = N_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = N_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  scale_x_continuous(breaks = 2006:2018,
                     minor_breaks = seq(2006.1, 2018.1, 0.25),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(
    subtitle = "count ~ s(x, y, year_fac, bs = \"fs\") + season  |  ribbon = 95% CI (lognormal)",
    x        = "",
    y        = expression(hat(N))
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.minor.ticks.length.x = rel(0.65))

p.dd.N.sy.fs

## density (fs) ----
### facets -----
p.dd.D.fs <- ggplot(dd_abund_fs, aes(x = year)) +
  geom_ribbon(aes(ymin = dens_lo, ymax = dens_hi), alpha = 0.2) +
  geom_line(aes(y = density)) +
  geom_point(aes(y = density,     colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = density_obs_lo, ymax = density_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = density_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  facet_wrap(~ season, scales = "free_y") +
  labs(
    title    = "Common dolphin density by season and year (year-varying spatial)",
    subtitle = "count ~ s(x, y, year_fac, bs = \"fs\") + season  |  ribbon = 95% CI (lognormal)",
    x        = "Year",
    y        = expression("Dolphins km"^{-2})
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

p.dd.D.fs

### continuous ----
p.dd.D.sy.fs <- ggplot(dd_abund_fs, aes(x = sy)) +
  geom_ribbon(aes(ymin = dens_lo, ymax = dens_hi), alpha = 0.2) +
  geom_line(aes(y = density)) +
  geom_point(aes(y = density,     colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = density_obs_lo, ymax = density_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = density_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  scale_x_continuous(breaks = 2006:2018,
                     minor_breaks = seq(2006.1, 2018.1, 0.25),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(
    subtitle = "count ~ s(x, y, year_fac, bs = \"fs\") + season  |  ribbon = 95% CI (lognormal)",
    x        = "",
    y        = expression("Dolphins km"^{-2})
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.minor.ticks.length.x = rel(0.65))

p.dd.D.sy.fs

ply.dd.N.sy.fs <- plot_ly() %>%
  # 95% CI ribbon (lognormal)
  add_ribbons(data = dd_abund_fs,
              x = ~sy, ymin = ~N_lo95, ymax = ~N_hi95,
              color = I("black"), opacity = 0.2,
              line = list(color = 'transparent'),
              showlegend = FALSE) %>%
  # DSM line + dots
  add_trace(data = dd_abund_fs, x = ~sy, y = ~N_hat,
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'black', width = 1.5),
            marker = list(color = 'black', size = 6),
            name = 'Density surface model') %>%
  # HT linerange (invisible markers anchor error_y)
  add_trace(data = dd_abund_fs %>% filter(!is.na(N_obs)),
            x = ~sy, y = ~N_obs,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'firebrick', size = 0, opacity = 0),
            error_y = list(type = "data", symmetric = FALSE,
                           arrayminus = ~(N_obs - N_obs_lo),
                           array = ~(N_obs_hi - N_obs),
                           color = 'firebrick', width = 0, thickness = 1.5,
                           opacity = 0.6),
            showlegend = FALSE, hoverinfo = 'skip') %>%
  # HT triangles
  add_trace(data = dd_abund_fs %>% filter(!is.na(N_obs)),
            x = ~sy, y = ~N_obs,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'firebrick', size = 8,
                          symbol = 'triangle-down'),
            name = 'design-based Horvitz–Thompson estimate') %>%
  layout(
    title = list(
      text = 'count ~ s(x, y, year_fac, bs = "fs") + season  |  ribbon = 95% CI (lognormal)',
      font = list(size = 11), xref = 'paper', x = 0.05
    ),
    xaxis = list(
      title = '',
      tickvals = 2006:2018,
      minor = list(tickvals = seq(2006.1, 2018.1, 0.25),
                   ticklen = 4, tickcolor = '#333'),
      showminor = TRUE, showgrid = TRUE, gridcolor = '#eee'
    ),
    yaxis = list(title = list(text = 'N̂')),
    legend = list(orientation = 'h', xanchor = 'center',
                  x = 0.5, y = -0.15),
    margin = list(b = 80, t = 40),
    font = list(size = 13)
  )

ply.dd.N.sy.fs
## output (fs) ----
fwrite(dd_abund_fs, "output/CommonDolphin/Abundance/DD_abundance_season_year_fsyear.csv")
ggsave(
  "output/CommonDolphin/Abundance/DD_abundance_season_year_fsyear.png",
  plot   = p.dd.N.fs,
  width  = 13,
  height = 8
)
ggsave(
  "output/CommonDolphin/Abundance/DD_density_season_year_fsyear.png",
  plot   = p.dd.D.fs,
  width  = 13,
  height = 8
)
ggsave(
  "output/CommonDolphin/Abundance/DD_abundance_modelfit_fsyear.png",
  plot   = p.dd.N.sy.fs,
  width  = 13,
  height = 8
)
ggsave(
  "output/CommonDolphin/Abundance/DD_density_modelfit_fsyear.png",
  plot   = p.dd.D.sy.fs,
  width  = 13,
  height = 8
)

# partial effect of YEAR (fs model) -----
# year lives INSIDE s(x,y,year_fac,bs="fs"), so there is no standalone year term.
# year_partial_effect() (in R/year_partial_effect.R) returns that spatial smooth
# area-averaged over the grid per year (log scale, 95% CI): a shrinkage estimate
# of the year-level shift, with a CI from the smooth's covariance.
dd_year_pe <- year_partial_effect(
  dd.dsm.xy.fsyear.season,
  data    = st_drop_geometry(pred.polys_m)[, c("x", "y")],
  weights = cell_area_m2
)
print(dd_year_pe)

p.dd.year.pe <- ggplot(dd_year_pe, aes(year, partial)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey60") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2006:2018,
                     minor_breaks = seq(2006.1, 2018.1, 0.25),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(
    title    = "Common dolphin — partial effect of year (fs model)",
    subtitle = "s(x, y, year_fac, bs = \"fs\")  |  area-averaged, log scale, 95% CI",
    x = "Year",
    y = "Partial effect of year (log scale)"
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        axis.minor.ticks.length.x = rel(0.65))

p.dd.year.pe

fwrite(dd_year_pe, "output/CommonDolphin/Abundance/DD_year_partial_effect_fsyear.csv")
ggsave(
  "output/CommonDolphin/Abundance/DD_year_partial_effect_fsyear.png",
  plot   = p.dd.year.pe,
  width  = 10,
  height = 6
)

# Annual (x, y) surface via by = year_fac ----
# dd.dsm.xy.byyear.season
#   count ~ s(x, y, by = year_fac) + year_fac + season
#   A SEPARATE, unshrunk spatial surface is estimated per year (unlike the fs
#   model above, which shares one smoothing parameter and shrinks years toward
#   the mean). Same pipeline as the sections above; the design-based estimate
#   (obs_strata) is MODEL-INDEPENDENT and is reused, not recomputed.

# year_fac levels from the byyear model; prediction grids must use these levels
yf_levels_byyear <- levels(dd.dsm.xy.byyear.season$model$year_fac)

#  abundance + density by season × year (byyear) -----
results_dd_byyear <- lapply(seq_len(nrow(sy_combos)),
                            function(i) {

                              s <- sy_combos$season[i]
                              a <- sy_combos$Ano[i]

                              pred_grid <- pred.polys_m %>%
                                st_drop_geometry() %>%
                                mutate(
                                  season   = factor(s, levels = season_levels),
                                  year_fac = factor(as.character(a), levels = yf_levels_byyear)
                                )

                              vp <- dsm_var_gam(
                                dsm.obj   = dd.dsm.xy.byyear.season,
                                pred.data = pred_grid,
                                off.set   = cell_area_m2
                              )

                              sm       <- summary(vp)
                              N_hat    <- as.numeric(sm$pred.est)
                              cv_hat   <- as.numeric(sm$cv)
                              ci       <- lnorm_ci(N_hat, cv_hat)
                              dens     <- N_hat / survey_area_km2
                              dens_lo  <- ci$lo  / survey_area_km2
                              dens_hi  <- ci$hi  / survey_area_km2

                              data.table(
                                species  = "Common dolphin",
                                season   = s,
                                year     = a,
                                N_hat    = round(N_hat),
                                N_lo95   = round(ci$lo),
                                N_hi95   = round(ci$hi),
                                CV       = round(cv_hat, 3),
                                density  = round(dens,    4),
                                dens_lo  = round(dens_lo, 4),
                                dens_hi  = round(dens_hi, 4)
                              )
                            })

dd_abund_byyear <- rbindlist(results_dd_byyear) %>%
  mutate(seas = as.numeric(case_when(
    season == "Summer" ~ "0.1",
    season == "Fall" ~ "0.35",
    season == "Winter" ~ "0.60",
    season == "Spring" ~ "0.85"
  )) ) %>%
  mutate(sy = year + seas) %>%
  data.table()

# reuse the model-independent design-based estimate (obs_strata) from above
dd_abund_byyear[, `:=`(season = as.character(season), year = as.numeric(year))]
dd_abund_byyear <- merge(dd_abund_byyear, obs_strata, by = c("season", "year"), all.x = TRUE)
dd_abund_byyear[, season := factor(season, levels = season_levels)]
setorder(dd_abund_byyear, sy)

##  abundance (byyear) ----
### facets -----
p.dd.N.byyear <- ggplot(dd_abund_byyear, aes(x = year)) +
  geom_ribbon(aes(ymin = N_lo95, ymax = N_hi95), alpha = 0.2) +
  geom_line(aes(y = N_hat)) +
  geom_point(aes(y = N_hat, colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = N_obs_lo, ymax = N_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = N_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  facet_wrap(~ season, scales = "free_y") +
  labs(
    title    = "Common dolphin abundance by season and year (by-year spatial)",
    subtitle = "count ~ s(x, y, by = year_fac) + year_fac + season  |  ribbon = 95% CI (lognormal)",
    x        = "Year",
    y        = expression(hat(N))
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

p.dd.N.byyear

### continuous ----
p.dd.N.sy.byyear <- ggplot(dd_abund_byyear, aes(x = sy)) +
  geom_ribbon(aes(ymin = N_lo95, ymax = N_hi95), alpha = 0.2) +
  geom_line(aes(y = N_hat)) +
  geom_point(aes(y = N_hat, colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = N_obs_lo, ymax = N_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = N_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  scale_x_continuous(breaks = 2006:2018,
                     minor_breaks = seq(2006.1, 2018.1, 0.25),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(
    subtitle = "count ~ s(x, y, by = year_fac) + year_fac + season  |  ribbon = 95% CI (lognormal)",
    x        = "",
    y        = expression(hat(N))
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.minor.ticks.length.x = rel(0.65))

p.dd.N.sy.byyear

## density (byyear) ----
### facets -----
p.dd.D.byyear <- ggplot(dd_abund_byyear, aes(x = year)) +
  geom_ribbon(aes(ymin = dens_lo, ymax = dens_hi), alpha = 0.2) +
  geom_line(aes(y = density)) +
  geom_point(aes(y = density,     colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = density_obs_lo, ymax = density_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = density_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  facet_wrap(~ season, scales = "free_y") +
  labs(
    title    = "Common dolphin density by season and year (by-year spatial)",
    subtitle = "count ~ s(x, y, by = year_fac) + year_fac + season  |  ribbon = 95% CI (lognormal)",
    x        = "Year",
    y        = expression("Dolphins km"^{-2})
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

p.dd.D.byyear

### continuous ----
p.dd.D.sy.byyear <- ggplot(dd_abund_byyear, aes(x = sy)) +
  geom_ribbon(aes(ymin = dens_lo, ymax = dens_hi), alpha = 0.2) +
  geom_line(aes(y = density)) +
  geom_point(aes(y = density,     colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = density_obs_lo, ymax = density_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = density_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  scale_x_continuous(breaks = 2006:2018,
                     minor_breaks = seq(2006.1, 2018.1, 0.25),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(
    subtitle = "count ~ s(x, y, by = year_fac) + year_fac + season  |  ribbon = 95% CI (lognormal)",
    x        = "",
    y        = expression("Dolphins km"^{-2})
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.minor.ticks.length.x = rel(0.65))

p.dd.D.sy.byyear

ply.dd.N.sy.byyear <- plot_ly() %>%
  # 95% CI ribbon (lognormal)
  add_ribbons(data = dd_abund_byyear,
              x = ~sy, ymin = ~N_lo95, ymax = ~N_hi95,
              color = I("black"), opacity = 0.2,
              line = list(color = 'transparent'),
              showlegend = FALSE) %>%
  # DSM line + dots
  add_trace(data = dd_abund_byyear, x = ~sy, y = ~N_hat,
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'black', width = 1.5),
            marker = list(color = 'black', size = 6),
            name = 'Density surface model') %>%
  # HT linerange (invisible markers anchor error_y)
  add_trace(data = dd_abund_byyear %>% filter(!is.na(N_obs)),
            x = ~sy, y = ~N_obs,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'firebrick', size = 0, opacity = 0),
            error_y = list(type = "data", symmetric = FALSE,
                           arrayminus = ~(N_obs - N_obs_lo),
                           array = ~(N_obs_hi - N_obs),
                           color = 'firebrick', width = 0, thickness = 1.5,
                           opacity = 0.6),
            showlegend = FALSE, hoverinfo = 'skip') %>%
  # HT triangles
  add_trace(data = dd_abund_byyear %>% filter(!is.na(N_obs)),
            x = ~sy, y = ~N_obs,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'firebrick', size = 8,
                          symbol = 'triangle-down'),
            name = 'design-based Horvitz–Thompson estimate') %>%
  layout(
    title = list(
      text = 'count ~ s(x, y, by = year_fac) + year_fac + season  |  ribbon = 95% CI (lognormal)',
      font = list(size = 11), xref = 'paper', x = 0.05
    ),
    xaxis = list(
      title = '',
      tickvals = 2006:2018,
      minor = list(tickvals = seq(2006.1, 2018.1, 0.25),
                   ticklen = 4, tickcolor = '#333'),
      showminor = TRUE, showgrid = TRUE, gridcolor = '#eee'
    ),
    yaxis = list(title = list(text = 'N̂')),
    legend = list(orientation = 'h', xanchor = 'center',
                  x = 0.5, y = -0.15),
    margin = list(b = 80, t = 40),
    font = list(size = 13)
  )

ply.dd.N.sy.byyear

## output (byyear) ----
fwrite(dd_abund_byyear, "output/CommonDolphin/Abundance/DD_abundance_season_year_byyear.csv")
ggsave(
  "output/CommonDolphin/Abundance/DD_abundance_season_year_byyear.png",
  plot   = p.dd.N.byyear,
  width  = 13,
  height = 8
)
ggsave(
  "output/CommonDolphin/Abundance/DD_density_season_year_byyear.png",
  plot   = p.dd.D.byyear,
  width  = 13,
  height = 8
)
ggsave(
  "output/CommonDolphin/Abundance/DD_abundance_modelfit_byyear.png",
  plot   = p.dd.N.sy.byyear,
  width  = 13,
  height = 8
)
ggsave(
  "output/CommonDolphin/Abundance/DD_density_modelfit_byyear.png",
  plot   = p.dd.D.sy.byyear,
  width  = 13,
  height = 8
)

# Soap-film shared surface ----
# dd.dsm.soap.season.year : count ~ s(x, y, bs = "so") + season + s(Ano)
#   A single spatial surface, edge-effect controlled (respects the gulf
#   boundary — see UTIL_CommonDolphin_EdgeEffects.R / 4_CommonDolphin_DSM_soap.R
#   for how the boundary/knots were built). Uses season + Ano like the primary
#   model above (NOT year_fac). predict() auto-returns NA for grid cells
#   outside the fitted (buffered) boundary; pred.polys_m sits inside that
#   boundary by construction, but we check rather than assume.

#  abundance + density by season × year (soap) -----
results_dd_soap <- lapply(seq_len(nrow(sy_combos)),
                          function(i) {

                            s <- sy_combos$season[i]
                            a <- sy_combos$Ano[i]

                            pred_grid <- pred.polys_m %>%
                              st_drop_geometry() %>%
                              mutate(
                                season = factor(s, levels = season_levels),
                                Ano    = a
                              )

                            # sanity check: soap predictions are NA outside the
                            # fitted boundary; pred.polys_m should be fully inside
                            chk <- predict(dd.dsm.soap.season.year, newdata = pred_grid,
                                          off.set = cell_area_m2, type = "response")
                            if (anyNA(chk))
                              warning(sprintf(
                                paste0("season=%s, year=%d: %d/%d prediction grid cells fall ",
                                      "outside the soap boundary (NA) — widen `margin` in ",
                                      "4_CommonDolphin_DSM_soap.R and refit."),
                                s, a, sum(is.na(chk)), length(chk)))

                            vp <- dsm_var_gam(
                              dsm.obj   = dd.dsm.soap.season.year,
                              pred.data = pred_grid,
                              off.set   = cell_area_m2
                            )

                            sm       <- summary(vp)
                            N_hat    <- as.numeric(sm$pred.est)
                            cv_hat   <- as.numeric(sm$cv)
                            ci       <- lnorm_ci(N_hat, cv_hat)
                            dens     <- N_hat / survey_area_km2
                            dens_lo  <- ci$lo  / survey_area_km2
                            dens_hi  <- ci$hi  / survey_area_km2

                            data.table(
                              species  = "Common dolphin",
                              season   = s,
                              year     = a,
                              N_hat    = round(N_hat),
                              N_lo95   = round(ci$lo),
                              N_hi95   = round(ci$hi),
                              CV       = round(cv_hat, 3),
                              density  = round(dens,    4),
                              dens_lo  = round(dens_lo, 4),
                              dens_hi  = round(dens_hi, 4)
                            )
                          })

dd_abund_soap <- rbindlist(results_dd_soap) %>%
  mutate(seas = as.numeric(case_when(
    season == "Summer" ~ "0.1",
    season == "Fall" ~ "0.35",
    season == "Winter" ~ "0.60",
    season == "Spring" ~ "0.85"
  )) ) %>%
  mutate(sy = year + seas) %>%
  data.table()

# reuse the model-independent design-based estimate (obs_strata) from above
dd_abund_soap[, `:=`(season = as.character(season), year = as.numeric(year))]
dd_abund_soap <- merge(dd_abund_soap, obs_strata, by = c("season", "year"), all.x = TRUE)
dd_abund_soap[, season := factor(season, levels = season_levels)]
setorder(dd_abund_soap, sy)

##  abundance (soap) ----
### facets -----
p.dd.N.soap <- ggplot(dd_abund_soap, aes(x = year)) +
  geom_ribbon(aes(ymin = N_lo95, ymax = N_hi95), alpha = 0.2) +
  geom_line(aes(y = N_hat)) +
  geom_point(aes(y = N_hat, colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = N_obs_lo, ymax = N_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = N_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  facet_wrap(~ season, scales = "free_y") +
  labs(
    title    = "Common dolphin abundance by season and year (soap-film surface)",
    subtitle = "count ~ s(x, y, bs = \"so\") + season + s(Ano)  |  ribbon = 95% CI (lognormal)",
    x        = "Year",
    y        = expression(hat(N))
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

p.dd.N.soap

### continuous ----
p.dd.N.sy.soap <- ggplot(dd_abund_soap, aes(x = sy)) +
  geom_ribbon(aes(ymin = N_lo95, ymax = N_hi95), alpha = 0.2) +
  geom_line(aes(y = N_hat)) +
  geom_point(aes(y = N_hat, colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = N_obs_lo, ymax = N_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = N_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  scale_x_continuous(breaks = 2006:2018,
                     minor_breaks = seq(2006.1, 2018.1, 0.25),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(
    subtitle = "count ~ s(x, y, bs = \"so\") + season + s(Ano)  |  ribbon = 95% CI (lognormal)",
    x        = "",
    y        = expression(hat(N))
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.minor.ticks.length.x = rel(0.65))

p.dd.N.sy.soap

## density (soap) ----
### facets -----
p.dd.D.soap <- ggplot(dd_abund_soap, aes(x = year)) +
  geom_ribbon(aes(ymin = dens_lo, ymax = dens_hi), alpha = 0.2) +
  geom_line(aes(y = density)) +
  geom_point(aes(y = density,     colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = density_obs_lo, ymax = density_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = density_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  facet_wrap(~ season, scales = "free_y") +
  labs(
    title    = "Common dolphin density by season and year (soap-film surface)",
    subtitle = "count ~ s(x, y, bs = \"so\") + season + s(Ano)  |  ribbon = 95% CI (lognormal)",
    x        = "Year",
    y        = expression("Dolphins km"^{-2})
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

p.dd.D.soap

### continuous ----
p.dd.D.sy.soap <- ggplot(dd_abund_soap, aes(x = sy)) +
  geom_ribbon(aes(ymin = dens_lo, ymax = dens_hi), alpha = 0.2) +
  geom_line(aes(y = density)) +
  geom_point(aes(y = density,     colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = density_obs_lo, ymax = density_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = density_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  scale_x_continuous(breaks = 2006:2018,
                     minor_breaks = seq(2006.1, 2018.1, 0.25),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(
    subtitle = "count ~ s(x, y, bs = \"so\") + season + s(Ano)  |  ribbon = 95% CI (lognormal)",
    x        = "",
    y        = expression("Dolphins km"^{-2})
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.minor.ticks.length.x = rel(0.65))

p.dd.D.sy.soap

ply.dd.N.sy.soap <- plot_ly() %>%
  # 95% CI ribbon (lognormal)
  add_ribbons(data = dd_abund_soap,
              x = ~sy, ymin = ~N_lo95, ymax = ~N_hi95,
              color = I("black"), opacity = 0.2,
              line = list(color = 'transparent'),
              showlegend = FALSE) %>%
  # DSM line + dots
  add_trace(data = dd_abund_soap, x = ~sy, y = ~N_hat,
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'black', width = 1.5),
            marker = list(color = 'black', size = 6),
            name = 'Density surface model') %>%
  # HT linerange (invisible markers anchor error_y)
  add_trace(data = dd_abund_soap %>% filter(!is.na(N_obs)),
            x = ~sy, y = ~N_obs,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'firebrick', size = 0, opacity = 0),
            error_y = list(type = "data", symmetric = FALSE,
                           arrayminus = ~(N_obs - N_obs_lo),
                           array = ~(N_obs_hi - N_obs),
                           color = 'firebrick', width = 0, thickness = 1.5,
                           opacity = 0.6),
            showlegend = FALSE, hoverinfo = 'skip') %>%
  # HT triangles
  add_trace(data = dd_abund_soap %>% filter(!is.na(N_obs)),
            x = ~sy, y = ~N_obs,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'firebrick', size = 8,
                          symbol = 'triangle-down'),
            name = 'design-based Horvitz–Thompson estimate') %>%
  layout(
    title = list(
      text = 'count ~ s(x, y, bs = "so") + season + s(Ano)  |  ribbon = 95% CI (lognormal)',
      font = list(size = 11), xref = 'paper', x = 0.05
    ),
    xaxis = list(
      title = '',
      tickvals = 2006:2018,
      minor = list(tickvals = seq(2006.1, 2018.1, 0.25),
                   ticklen = 4, tickcolor = '#333'),
      showminor = TRUE, showgrid = TRUE, gridcolor = '#eee'
    ),
    yaxis = list(title = list(text = 'N̂')),
    legend = list(orientation = 'h', xanchor = 'center',
                  x = 0.5, y = -0.15),
    margin = list(b = 80, t = 40),
    font = list(size = 13)
  )

ply.dd.N.sy.soap

## partial effects (soap) ------
### season -----
p.dd.soap.seasonyear.season <- gratia::draw(gratia::parametric_effects(dd_soap_models$dd.dsm.soap.season.year, term = "season")) +
  theme_bw()

### ano -----
p.dd.soap.seasonyear.year<-gratia::draw(dd_soap_models$dd.dsm.soap.season.year, select = "s(Ano)") +
  theme_bw()

## output (soap) ----
ggsave(plot = p.dd.soap.seasonyear.season,
       file = "output/CommonDolphin/Abundance/DD_season_partial_effect_soap.png",
       width  = 10,
       height = 6)
ggsave(plot = p.dd.soap.seasonyear.year,
       file = "output/CommonDolphin/Abundance/DD_year_partial_effect_soap.png",
       width  = 10,
       height = 6)
fwrite(dd_abund_soap, "output/CommonDolphin/Abundance/DD_abundance_season_year_soap.csv")
ggsave(
  "output/CommonDolphin/Abundance/DD_abundance_season_year_soap.png",
  plot   = p.dd.N.soap,
  width  = 13,
  height = 8
)
ggsave(
  "output/CommonDolphin/Abundance/DD_density_season_year_soap.png",
  plot   = p.dd.D.soap,
  width  = 13,
  height = 8
)
ggsave(
  "output/CommonDolphin/Abundance/DD_abundance_modelfit_soap.png",
  plot   = p.dd.N.sy.soap,
  width  = 13,
  height = 8
)
ggsave(
  "output/CommonDolphin/Abundance/DD_density_modelfit_soap.png",
  plot   = p.dd.D.sy.soap,
  width  = 13,
  height = 8
)
