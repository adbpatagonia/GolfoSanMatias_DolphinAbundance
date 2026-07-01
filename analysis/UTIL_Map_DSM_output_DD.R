# ADB
# 2026-06-30
#
# Predicted density maps for common dolphins (Delphinus delphis)
# Models: dsm.xy.season.year.tw  (count ~ s(x,y) + -1 + season + s(year, bs = 're'))
#         dsm.xy.clo        (count ~ s(x,y) + -1 + season + s(year, bs = 're') + s(clo))
#
# Assumes the following objects are already in the workspace:
#   pred.polys_m, survey.area_m, patagonia_m, segdata, segdata_traj_m
#   obsdata_dd_mod, distdata_dd_sf_m, target_crs
#   trunc.dist_dd, dsm.xy.season.year.tw, dsm.xy.clo

library(dsm)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

#----------------------------------------------------------
# Helpers
#----------------------------------------------------------

off.set <- 800 * trunc.dist_dd

# Reference year level for s(year_fac, bs = "re") — excluded from prediction
ref_year_fac <- factor(levels(segdata$year_fac)[8], levels = levels(segdata$year_fac))

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

#----------------------------------------------------------
# Build season prediction grid (shared by both models)
#----------------------------------------------------------

pred.polys_season_m <- bind_rows(
  pred.polys_m %>% mutate(season = "Summer"),
  pred.polys_m %>% mutate(season = "Spring"),
  pred.polys_m %>% mutate(season = "Winter"),
  pred.polys_m %>% mutate(season = "Fall")
) %>%
  mutate(
    season   = factor(season, levels = levels(obsdata_dd_mod$season)),
    year_fac = ref_year_fac
  )

#----------------------------------------------------------
# Map 1 — dsm.xy.season.year.tw
# count ~ s(x, y) + -1 + season + s(Ano)
#----------------------------------------------------------

pred.polys_season_m$Nhat <- predict(
  dsm.xy.season.year.tw,
  newdata = pred.polys_season_m,
  off.set = off.set,
  exclude = "s(year_fac)",
  type    = "response"
)

pred.polys_season_m$area_m2  <- as.numeric(st_area(pred.polys_season_m))
pred.polys_season_m$density  <- pred.polys_season_m$Nhat / (pred.polys_season_m$area_m2 / 1e6)
pred.polys_season_m$ldensity <- log10(pred.polys_season_m$density + 0.001)

dd.map.density.season <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_season_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data      = survey.area_m,
          fill      = NA,
          color     = "black",
          linewidth = 0.6) +
  geom_sf(data  = segdata_traj_m %>% filter(Ano > 2006),
          size  = 0.6,
          alpha = 0.25) +
  geom_sf(data  = distdata_dd_sf_m %>% filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option = "turbo",
    name   = expression("Dolphins km"^-2)
  ) +
  labs(
    title   = "Predicted spatial density of common dolphins",
    caption = "model: count ~ s(x, y) + season + s(Ano)",
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

dd.map.density.season

ggsave(
  filename = "output/CommonDolphin/DSM/DD_DSM_Season.png",
  plot     = dd.map.density.season,
  width    = 13,
  height   = 13
)

#----------------------------------------------------------
# Map 2 — dsm.xy.clo
# count ~ s(x, y) + -1 + season + s(Ano) + s(clo)
#
# clo is available at every prediction cell in pred.polys_m
#----------------------------------------------------------

segdata_sf <- st_as_sf(segdata, coords = c("x", "y"),
                       crs = st_crs(pred.polys_m), remove = FALSE)

pred.polys_season_clo_m <- bind_rows(
  lapply(levels(obsdata_dd_mod$season), function(s) {
    seg_s <- segdata_sf[segdata_sf$season == s, ]
    idx   <- st_nearest_feature(pred.polys_m, seg_s)
    pred.polys_m %>%
      mutate(season   = factor(s, levels = levels(obsdata_dd_mod$season)),
             year_fac = ref_year_fac,
             clo      = seg_s$clo[idx])
  })
)

# cap clo at 95th percentile of training data to avoid the high-uncertainty tail
clo_cap <- quantile(segdata$clo, 0.95)
pred.polys_season_clo_m <- pred.polys_season_clo_m %>%
  mutate(clo = pmin(clo, clo_cap))

pred.polys_season_clo_m$Nhat <- predict(
  dsm.xy.clo,
  newdata = pred.polys_season_clo_m,
  off.set = off.set,
  exclude = "s(year_fac)",
  type    = "response"
)

pred.polys_season_clo_m$area_m2  <- as.numeric(st_area(pred.polys_season_clo_m))
pred.polys_season_clo_m$density  <- pred.polys_season_clo_m$Nhat / (pred.polys_season_clo_m$area_m2 / 1e6)
pred.polys_season_clo_m$ldensity <- log10(pred.polys_season_clo_m$density + 0.001)

dd.map.density.season.clo <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_season_clo_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data      = survey.area_m,
          fill      = NA,
          color     = "black",
          linewidth = 0.6) +
  geom_sf(data  = segdata_traj_m %>% filter(Ano > 2006),
          size  = 0.6,
          alpha = 0.25) +
  geom_sf(data  = distdata_dd_sf_m %>% filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option = "turbo",
    name   = expression("Dolphins km"^-2)
  ) +
  labs(
    title   = "Predicted spatial density of common dolphins",
    caption = "model: count ~ s(x, y) + season + s(Ano) + s(clo)",
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

dd.map.density.season.clo

ggsave(
  filename = "output/CommonDolphin/DSM/DD_DSM_Season_clo.png",
  plot     = dd.map.density.season.clo,
  width    = 13,
  height   = 13
)
