# ADB / Claude
# 2026-07-28
#
# Coefficient of variation (CV) map for common dolphins, following Figure 5 of
# Miller et al. (2013) — "Spatial models for distance sampling data: recent
# developments and future directions" (assets/Methods Ecol Evol - 2013 -
# Miller...pdf). Their Fig. 5 caption: "Map of the coefficients of variation
# for the model with smooths of both depth and location. Uncertainty was
# estimated using the variance propagation method of Williams et al. (2011)."
# Earlier in the same paper: "DSM uncertainty can be visualized via a plot of
# per-cell coefficient of variation obtained by dividing the standard error
# for each cell by its predicted abundance."
#
# Model: dd_soap_models$dd.dsm.soap.season.year
#        count ~ s(x, y, bs = "so") + season + s(Ano)
# (fitted in 4_CommonDolphin_DSM_soap.R)
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
# The soap-film spatial term does NOT vary by year (only s(Ano) shifts the
# level), so — as established for the density maps — this CV map facets by
# SEASON at the reference year, and the overlay legitimately shows ALL years'
# survey effort (it all informs the one shared spatial surface).
#
# Assumes in the workspace:
#   dd_soap_models (with dd.dsm.soap.season.year), pred.polys_m, survey.area_m,
#   patagonia_m, segdata, segdata_traj_m, obsdata_dd_mod, distdata_dd_sf_m,
#   target_crs, trunc.dist_dd

# library(dsm)
# library(sf)
# library(dplyr)
# library(ggplot2)
# library(viridis)

# Helpers -----

m <- dd_soap_models$dd.dsm.soap.season.year

off.set <- 800 * trunc.dist_dd

# Reference year — evaluated at the median survey year
ref_ano <- as.integer(round(median(segdata$Ano)))

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

# detection-function CV (global; constant because df.dd has no covariates) ----
ddf_sm <- summary(m$ddf)
cv_p   <- as.numeric(ddf_sm$average.p.se / ddf_sm$average.p)

# CV map — dd.dsm.soap.season.year, season facets at reference year ----
# count ~ s(x, y, bs = "so") + season + s(Ano)

pred.polys_cv_m <- bind_rows(
  pred.polys_m %>% mutate(season = "Summer"),
  pred.polys_m %>% mutate(season = "Spring"),
  pred.polys_m %>% mutate(season = "Winter"),
  pred.polys_m %>% mutate(season = "Fall")
) %>%
  mutate(
    season = factor(season, levels = levels(obsdata_dd_mod$season)),
    Ano    = ref_ano
  )

pred_grid <- st_drop_geometry(pred.polys_cv_m)

# lpmatrix built with off.set = 0, matching dsm_var_gam's own convention: the
# offset is applied multiplicatively afterward, not as a design-matrix column.
# predict() automatically returns NA rows for cells outside the soap boundary
# (verified), which then propagate as NA through the matrix algebra below.
lpmat <- predict(m, newdata = pred_grid, type = "lpmatrix", off.set = 0)
Vb    <- vcov(m)

cv_spatial <- sqrt(rowSums((lpmat %*% Vb) * lpmat))   # == CV(cell) for log link
pred.polys_cv_m$cv <- sqrt(cv_spatial^2 + cv_p^2)      # combine with detection CV

dd.map.cv.season.soap <- ggplot() +
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
  # overlay shows ALL years' effort — the soap spatial term is shared across
  # years, so every year's data informs (and helps explain the precision of)
  # the surface shown here (see the density map, Map 9, for the same logic).
  geom_sf(data  = segdata_traj_m %>% filter(Ano > 2006),
          size  = 0.6,
          alpha = 0.25) +
  geom_sf(data  = distdata_dd_sf_m %>% filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option   = "turbo",
    name     = "CV",
    na.value = "grey95"
  ) +
  labs(
    title    = "Coefficient of variation — common dolphins (soap-film model)",
    subtitle = "per-cell CV = variance-propagation SE / predicted abundance (Miller et al. 2013, Fig. 5)",
    caption  = "model: count ~ s(x, y, bs = \"so\") + season + s(Ano)",
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
  facet_wrap(. ~ season)

dd.map.cv.season.soap

ggsave(
  filename = "output/CommonDolphin/DSM/DD_DSM_CV_Season_soap.png",
  plot     = dd.map.cv.season.soap,
  width    = 13,
  height   = 13
)
