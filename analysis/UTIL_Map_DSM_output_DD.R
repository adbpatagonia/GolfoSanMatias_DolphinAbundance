#----------------------------------------------------------
# Predict spatial density from fitted DSM
#----------------------------------------------------------

library(dsm)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

# pred.polys_m = prediction polygons/grid already in projected CRS
# must contain x,y centroids used by model

# If x,y do not exist:
pred.polys_m <- pred.polys_m %>%
  mutate(
    x = st_coordinates(st_centroid(geometry))[,1],
    y = st_coordinates(st_centroid(geometry))[,2]
  )


pred.polys_season_m <- copy(pred.polys_m)
pred.polys_season_m <- bind_rows(pred.polys_season_m %>%
                                   mutate(season = "Summer"),
                                 pred.polys_season_m %>%
                                   mutate(season = "Spring"),
                                 pred.polys_season_m %>%
                                   mutate(season = "Winter"),
                                 pred.polys_season_m %>%
                                   mutate(season = "Fall")
)
pred.polys_season_m <- pred.polys_season_m %>%
  mutate(
    year_fac = factor(
      levels(segdata$year_fac)[8],
      levels = levels(segdata$year_fac)
    ),
    season = factor(season, levels = levels(obsdata_dd_mod$season))
  )
#----------------------------------------------------------
# Predict abundance per cell
#----------------------------------------------------------

off.set <- 800 * trunc.dist_dd

pred.polys_m$Nhat <- predict(
  dsm.xy.tw,
  newdata = pred.polys_m,
  off.set
)


pred.polys_season_m$Nhat <- predict(
  dsm.xy.season.tw,
  newdata = pred.polys_season_m,
  off.set = off.set,
  exclude   = "s(year_fac)",
  type      = "response"
)

#----------------------------------------------------------
# Convert to density
# (animals / km2)
#----------------------------------------------------------

pred.polys_m$area_m2 <- as.numeric(st_area(pred.polys_m))
pred.polys_m$density <- pred.polys_m$Nhat / (pred.polys_m$area_m2 / 1e6)

# optional log density for smoother map
pred.polys_m$ldensity <- log10(pred.polys_m$density + 0.001)

pred.polys_season_m$area_m2 <- as.numeric(st_area(pred.polys_season_m))
pred.polys_season_m$density <- pred.polys_season_m$Nhat / (pred.polys_season_m$area_m2 / 1e6)

# optional log density for smoother map
pred.polys_season_m$ldensity <- log10(pred.polys_season_m$density + 0.001)

#----------------------------------------------------------
# Spatial density map
#----------------------------------------------------------

bb <- st_bbox(survey.area_m)

xpad <- 3000
ypad <- 3000


map.density <- ggplot() +
  geom_sf(data = patagonia_m,
          fill = "grey85",
          color = "grey40") +
  geom_sf(data = pred.polys_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data = survey.area_m,
          fill = NA,
          color = "black",
          linewidth = 0.6) +
  geom_sf(data = segdata_traj_m %>%
            filter(Ano > 2006),
          # aes(color = factor(season)),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m %>%
            filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7
  ) +
  scale_fill_viridis_c(
    option = "turbo",
    # trans = "sqrt",
    name = expression("Dolphins km"^-2)
  ) +
  labs(
    title = "Predicted spatial density of dusky dolphins",
    caption = "model: count ~ s(x, y)",
    x = "Easting (Mm)",
    y = "Northing (Mm)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(labels = \(x) x / 1e6) +
  scale_y_continuous(labels = \(x) x / 1e6) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = FALSE
  )


map.density

## season ----

map.density.season <- ggplot() +
  geom_sf(data = patagonia_m,
          fill = "grey85",
          color = "grey40") +
  geom_sf(data = pred.polys_season_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data = survey.area_m,
          fill = NA,
          color = "black",
          linewidth = 0.6) +
  geom_sf(data = segdata_traj_m %>%
            filter(Ano > 2006),
          # aes(color = factor(season)),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m %>%
            filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7
  ) +
  scale_fill_viridis_c(
    option = "turbo",
    # trans = "sqrt",
    name = expression("Dolphins km"^-2)
  ) +
  labs(
    title = "Predicted spatial density of dusky dolphins",
    caption = 'model: count ~ s(x, y) + season + s(year, bs = "re" ) ',
    x = "Easting (Mm)",
    y = "Northing (Mm)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(labels = \(x) x / 1e6) +
  scale_y_continuous(labels = \(x) x / 1e6) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = FALSE
  ) +
  facet_wrap(. ~ season)

map.density.season


# model with depth -----
# convert segdata to sf points
segdata_sf <- st_as_sf(
  segdata,
  coords = c("x", "y"),
  crs = st_crs(pred.polys_m),
  remove = FALSE
)

# nearest segdata row for each prediction polygon
idx <- st_nearest_feature(pred.polys_m, segdata_sf)

# copy depth
#' @ADB this may fail - I thikn pred.polys_m was modified above
#' double chekc
pred.polys_season_d_m <- copy(pred.polys_m)
pred.polys_season_d_m$depth <- segdata_sf$depth[idx]

pred.polys_season_d_m <- bind_rows(pred.polys_season_d_m %>%
                                     mutate(season = "Summer"),
                                   pred.polys_season_d_m %>%
                                     mutate(season = "Spring"),
                                   pred.polys_season_d_m %>%
                                     mutate(season = "Winter"),
                                   pred.polys_season_d_m %>%
                                     mutate(season = "Fall")
)
pred.polys_season_d_m <- pred.polys_season_d_m %>%
  mutate(
    year_fac = factor(
      levels(segdata$year_fac)[8],
      levels = levels(segdata$year_fac)
    ),
    season = factor(season, levels = levels(obsdata_dd_mod$season))
  )


pred.polys_season_d_m$Nhat <- predict(
  dsm.xy.season.tw,
  newdata = pred.polys_season_d_m,
  off.set = off.set,
  exclude   = "s(year_fac)",
  type      = "response"
)

pred.polys_season_d_m$area_m2 <- as.numeric(st_area(pred.polys_season_d_m))
pred.polys_season_d_m$density <- pred.polys_season_d_m$Nhat / (pred.polys_season_d_m$area_m2 / 1e6)

# optional log density for smoother map
pred.polys_season_d_m$ldensity <- log10(pred.polys_season_d_m$density + 0.001)




map.density.season.depth <- ggplot() +
  geom_sf(data = patagonia_m,
          fill = "grey85",
          color = "grey40") +
  geom_sf(data = pred.polys_season_d_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data = survey.area_m,
          fill = NA,
          color = "black",
          linewidth = 0.6) +
  geom_sf(data = segdata_traj_m %>%
            filter(Ano > 2006),
          # aes(color = factor(season)),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m %>%
            filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7
  ) +
  scale_fill_viridis_c(
    option = "turbo",
    # trans = "sqrt",
    name = expression("Dolphins km"^-2)
  ) +
  labs(
    title = "Predicted spatial density of dusky dolphins",
    caption = 'model: count ~ s(x, y) + season + s(depth) + s(year, bs = "re" ) ',
    x = "Easting (Mm)",
    y = "Northing (Mm)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(labels = \(x) x / 1e6) +
  scale_y_continuous(labels = \(x) x / 1e6) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = FALSE
  ) +
  facet_wrap(. ~ season)

map.density.season.depth



ggsave(filename = "output/DuskyDolphin/DSM/DD_DSM_Season_depth.png",
       width = 13, height = 13)
