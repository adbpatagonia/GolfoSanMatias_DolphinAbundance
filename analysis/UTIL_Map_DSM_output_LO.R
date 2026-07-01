# ADB
# 2026-06-30
#
# Predicted density maps for dusky dolphins (Lagenorhynchus obscurus)
# Models: dsm.xy.season.year.tw  (count ~ s(x,y) + -1 + season + s(year_fac, bs="re"))
#         dsm.xy.depth       (count ~ s(x,y) + -1 + season + s(depth) + s(year_fac, bs="re"))
#
# Assumes the following objects are already in the workspace:
#   pred.polys_m, survey.area_m, patagonia_m, segdata, segdata_traj_m
#   obsdata_lo_mod, distdata_lo_sf_m, target_crs
#   trunc.dist_lo, dsm.xy.season.year.tw, dsm.xy.depth

library(dsm)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

#----------------------------------------------------------
# Helpers
#----------------------------------------------------------

off.set <- 800 * trunc.dist_lo

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
# Map 1 — dsm.xy.season.year.tw
# count ~ s(x, y) + -1 + season + s(year_fac, bs = "re")
#----------------------------------------------------------

pred.polys_season_m <- bind_rows(
  pred.polys_m %>% mutate(season = "Summer"),
  pred.polys_m %>% mutate(season = "Spring"),
  pred.polys_m %>% mutate(season = "Winter"),
  pred.polys_m %>% mutate(season = "Fall")
) %>%
  mutate(
    season   = factor(season, levels = levels(obsdata_lo_mod$season)),
    year_fac = ref_year_fac
  )

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

lo.map.density.season <- ggplot() +
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
  geom_sf(data  = distdata_lo_sf_m %>% filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option = "turbo",
    name   = expression("Dolphins km"^-2)
  ) +
  labs(
    title   = "Predicted spatial density of dusky dolphins",
    caption = "model: count ~ s(x, y) + season + s(year_fac, bs='re')",
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

lo.map.density.season

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_Season.png",
  plot     = lo.map.density.season,
  width    = 13,
  height   = 13
)

#----------------------------------------------------------
# Map 2 — dsm.xy.depth
# count ~ s(x, y) + -1 + season + s(depth) + s(year_fac, bs = "re")
#
# depth has no seasonal variation: assign from nearest segdata point,
# then stack by season
#----------------------------------------------------------

segdata_sf <- st_as_sf(segdata, coords = c("x", "y"),
                       crs    = st_crs(pred.polys_m),
                       remove = FALSE)

idx_depth <- st_nearest_feature(pred.polys_m, segdata_sf)

pred.polys_season_depth_m <- bind_rows(
  pred.polys_m %>% mutate(season = "Summer"),
  pred.polys_m %>% mutate(season = "Spring"),
  pred.polys_m %>% mutate(season = "Winter"),
  pred.polys_m %>% mutate(season = "Fall")
) %>%
  mutate(
    season   = factor(season, levels = levels(obsdata_lo_mod$season)),
    year_fac = ref_year_fac,
    depth    = segdata_sf$depth[rep(idx_depth, 4)]
  )

pred.polys_season_depth_m$Nhat <- predict(
  dsm.xy.depth,
  newdata = pred.polys_season_depth_m,
  off.set = off.set,
  exclude = "s(year_fac)",
  type    = "response"
)

pred.polys_season_depth_m$area_m2  <- as.numeric(st_area(pred.polys_season_depth_m))
pred.polys_season_depth_m$density  <- pred.polys_season_depth_m$Nhat / (pred.polys_season_depth_m$area_m2 / 1e6)
pred.polys_season_depth_m$ldensity <- log10(pred.polys_season_depth_m$density + 0.001)

lo.map.density.season.depth <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_season_depth_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data      = survey.area_m,
          fill      = NA,
          color     = "black",
          linewidth = 0.6) +
  geom_sf(data  = segdata_traj_m %>% filter(Ano > 2006),
          size  = 0.6,
          alpha = 0.25) +
  geom_sf(data  = distdata_lo_sf_m %>% filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option = "turbo",
    name   = expression("Dolphins km"^-2)
  ) +
  labs(
    title   = "Predicted spatial density of dusky dolphins",
    caption = "model: count ~ s(x, y) + season + s(depth) + s(year_fac, bs='re')",
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

lo.map.density.season.depth

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_Season_depth.png",
  plot     = lo.map.density.season.depth,
  width    = 13,
  height   = 13
)
