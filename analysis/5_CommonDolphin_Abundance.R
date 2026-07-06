# ADB
# 2026-07-06
#
# Abundance and density estimates (with 95% CI) for common dolphins
# by season × year, using:
#   dd.dsm.xy.season.year  — count ~ s(x,y) + season + s(Ano)
#
# Assumes workspace contains:
#   dd.dsm.xy.season.year, pred.polys_m, survey.area_m,
#   segdata, obsdata_dd_mod, trunc.dist_dd, target_crs

# functions ----
source(file.path(here::here(), "R", "lnorm_ci.R"))

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
                       # method). dsm_var_gam is the correct estimator when the
                       # detection function has NO covariates (df.dd is a plain
                       # hazard-rate); dsm_var_prop refits the model and fails
                       # here. pred.data must be a data.frame and off.set a
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
print(dd_abund)

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

p.dd.N.sy



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
