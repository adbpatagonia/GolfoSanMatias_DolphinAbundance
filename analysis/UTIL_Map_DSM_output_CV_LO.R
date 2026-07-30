# ADB / Claude
# 2026-07-28
#
# Coefficient of variation (CV) map for dusky dolphins, following Figure 5 of
# Miller et al. (2013) — "Spatial models for distance sampling data: recent
# developments and future directions" (assets/Methods Ecol Evol - 2013 -
# Miller...pdf). Their Fig. 5 caption: "Map of the coefficients of variation
# for the model with smooths of both depth and location. Uncertainty was
# estimated using the variance propagation method of Williams et al. (2011)."
# Earlier in the same paper: "DSM uncertainty can be visualized via a plot of
# per-cell coefficient of variation obtained by dividing the standard error
# for each cell by its predicted abundance." The paper also notes: "there is
# high uncertainty where there is low sampling effort" — exactly the pattern
# this map is meant to expose across years.
#
# Model: lo.dsm.xy.fsyear.season
#        count ~ s(x, y, year_fac, bs = "fs") + season
# (fitted in 4_DuskyDolphin_DSM.R)
#
# CV COMPUTATION
# dsm_var_gam() gives per-cell variance ASSUMING INDEPENDENCE of the spatial
# model and the detection function, with squared CVs adding:
#   CV_total^2 = CV_spatial^2 + CV_detection^2
# (dsm_var_gam's own help text; matches Williams et al. 2011 / this figure).
#
# For a log-link model, dsm_var_gam's own per-cell formula reduces to an
# identity: SE(cell abundance)/cell abundance == SE of the LINEAR PREDICTOR at
# that cell (the offset and exp() terms cancel in the delta method). This lets
# CV_spatial be computed with ONE predict(type="lpmatrix") call over the whole
# grid instead of looping dsm_var_gam over hundreds of single-cell "regions"
# (which calls predict() separately per region — slow for large grids).
# Verified numerically identical to the per-region dsm_var_gam loop before use.
#
# The fs spatial term VARIES by year, so this CV map facets by YEAR (season
# fixed at Spring) — this is where uneven year-to-year survey coverage shows
# up most directly as elevated CV, and directly extends this session's
# edge-effect diagnostics (e.g. the 2009/2015 low-effort artefacts). The
# overlay shows ALL seasons of that year (not just Spring): the fs spatial
# term does not vary by season within a year, so every season's effort that
# year legitimately informs (and helps explain) the precision shown here.
#
# Assumes in the workspace:
#   lo.dsm.xy.fsyear.season, pred.polys_m, survey.area_m, patagonia_m,
#   segdata, segdata_traj_m, obsdata_lo_mod, distdata_lo_sf_m, target_crs,
#   trunc.dist_lo

# library(dsm)
# library(sf)
# library(dplyr)
# library(ggplot2)
# library(viridis)

# Helpers -----

m <- lo.dsm.xy.fsyear.season

off.set <- 800 * trunc.dist_lo

bb   <- st_bbox(survey.area_m)
xpad <- 3000
ypad <- 3000

# Ensure x,y centroids exist on prediction grid
if (!all(c("x", "y") %in% names(pred.polys_m))) {
  pred.polys_m <- pred.polys_m %>%
    mutate(
      x = st_coordinates(st_centroid(geometry))[, 1],
      y = st_coordinates(st_centroid(geometry))[, 2]
    )
}

# year_fac levels from the fitted fs model
yf_levels <- levels(m$model$year_fac)
years_fs  <- sort(as.numeric(yf_levels))

# detection-function CV (global; constant because df.lo has no covariates) ----
ddf_sm <- summary(m$ddf)
cv_p   <- as.numeric(ddf_sm$average.p.se / ddf_sm$average.p)

# CV map — lo.dsm.xy.fsyear.season, one panel per YEAR (season = Spring) ----
# count ~ s(x, y, year_fac, bs = "fs") + season

pred.polys_cv_m <- bind_rows(
  lapply(years_fs, function(a) pred.polys_m %>%
           mutate(Ano      = a,
                  year_fac = factor(as.character(a), levels = yf_levels)))
) %>%
  mutate(season = factor("Spring", levels = levels(obsdata_lo_mod$season)))

pred_grid <- st_drop_geometry(pred.polys_cv_m)

# lpmatrix built with off.set = 0, matching dsm_var_gam's own convention: the
# offset is applied multiplicatively afterward, not as a design-matrix column.
lpmat <- predict(m, newdata = pred_grid, type = "lpmatrix", off.set = 0)
Vb    <- vcov(m)

cv_spatial <- sqrt(rowSums((lpmat %*% Vb) * lpmat))   # == CV(cell) for log link
pred.polys_cv_m$cv <- sqrt(cv_spatial^2 + cv_p^2)      # combine with detection CV

lo.map.cv.year.fs <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_cv_m,
          aes(fill = cv),
          color = NA) +
  geom_sf(data      = survey.area_m,
          fill      = NA,
          color     = "black",
          linewidth = 0.6) +
  # overlay intentionally shows ALL seasons of that year — the fs spatial term
  # does not vary by season, so every season's effort that year is relevant
  # context for the precision of the year-specific surface shown here.
  geom_sf(data  = segdata_traj_m %>% filter(Ano > 2006),
          size  = 0.6,
          alpha = 0.25) +
  geom_sf(data  = distdata_lo_sf_m %>% filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option   = "turbo",
    name     = "CV",
    na.value = "grey95"
  ) +
  labs(
    title    = "Coefficient of variation — dusky dolphins (year-varying spatial model)",
    subtitle = "per-cell CV = variance-propagation SE / predicted abundance (Miller et al. 2013, Fig. 5)",
    caption  = "model: count ~ s(x, y, year_fac, bs = \"fs\") + season  |  season = Spring",
    x = "Easting (Mm)",
    y = "Northing (Mm)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "right",
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(labels = \(x) x / 1e6) +
  scale_y_continuous(labels = \(x) x / 1e6) +
  coord_sf(
    xlim        = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim        = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum       = target_crs,
    expand      = FALSE
  ) +
  facet_wrap(. ~ Ano)

lo.map.cv.year.fs

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_CV_Year_fsyear.png",
  plot     = lo.map.cv.year.fs,
  width    = 13,
  height   = 13
)
