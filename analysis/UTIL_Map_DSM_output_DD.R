# ADB
# 2026-06-30
#
# Predicted density maps for common dolphins (Delphinus delphis)
# Models: dd.dsm.xy.season.year        (count ~ s(x,y) + -1 + season + s(Ano))
#         dd.dsm.xy.year.season.clo    (count ~ s(x,y) + -1 + season + s(Ano) + s(clo))
#
# Assumes the following objects are already in the workspace:
#   pred.polys_m, survey.area_m, patagonia_m, segdata, segdata_traj_m
#   obsdata_dd_mod, distdata_dd_sf_m, target_crs
#   trunc.dist_dd, dd.dsm.xy.season.year, dd.dsm.xy.year.season.clo

library(dsm)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

#----------------------------------------------------------
# Helpers
#----------------------------------------------------------

off.set <- 800 * trunc.dist_dd

# Reference year for s(Ano) — evaluated at the median survey year
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
    season = factor(season, levels = levels(obsdata_dd_mod$season)),
    Ano    = ref_ano
  )

#----------------------------------------------------------
# Map 1 — dd.dsm.xy.season.year
# count ~ s(x, y) + -1 + season + s(Ano)
#----------------------------------------------------------

pred.polys_season_m$Nhat <- predict(
  dd.dsm.xy.season.year,
  newdata = pred.polys_season_m,
  off.set = off.set,
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
# Map 2 — dd.dsm.xy.year.season.clo
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
      mutate(season = factor(s, levels = levels(obsdata_dd_mod$season)),
             Ano    = ref_ano,
             clo    = seg_s$clo[idx])
  })
)

# cap clo at 95th percentile of training data to avoid the high-uncertainty tail
clo_cap <- quantile(segdata$clo, 0.95)
pred.polys_season_clo_m <- pred.polys_season_clo_m %>%
  mutate(clo = pmin(clo, clo_cap))

pred.polys_season_clo_m$Nhat <- predict(
  dd.dsm.xy.year.season.clo,
  newdata = pred.polys_season_clo_m,
  off.set = off.set,
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

#----------------------------------------------------------
# Map 3 — dd.dsm.xy.season.year, one panel per YEAR (season = Spring)
# count ~ s(x, y) + season + s(Ano)
#
# Season fixed at the reference level (Spring); Ano varies across panels.
# NOTE: s(Ano) is additive (no x,y-by-year interaction), so every panel
# shares the SAME spatial pattern and differs only in overall level
# (the s(Ano) effect) plus the per-year survey overlays.
#----------------------------------------------------------

years <- sort(unique(segdata$Ano))

pred.polys_year_m <- bind_rows(
  lapply(years, function(a) pred.polys_m %>% mutate(Ano = a))
) %>%
  mutate(season = factor("Spring", levels = levels(obsdata_dd_mod$season)))

pred.polys_year_m$Nhat <- predict(
  dd.dsm.xy.season.year,
  newdata = pred.polys_year_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_year_m$area_m2  <- as.numeric(st_area(pred.polys_year_m))
pred.polys_year_m$density  <- pred.polys_year_m$Nhat / (pred.polys_year_m$area_m2 / 1e6)
pred.polys_year_m$ldensity <- log10(pred.polys_year_m$density + 0.001)

dd.map.density.year <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_year_m,
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
    title   = "Predicted spatial density of common dolphins (Spring)",
    caption = "model: count ~ s(x, y) + season + s(Ano)  |  season = Spring",
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

dd.map.density.year

ggsave(
  filename = "output/CommonDolphin/DSM/DD_DSM_Year.png",
  plot     = dd.map.density.year,
  width    = 13,
  height   = 13
)

#----------------------------------------------------------
# Map 4 — dd.dsm.xy.year.season.clo, one panel per YEAR (season = Spring)
# count ~ s(x, y) + season + s(Ano) + s(clo)
#
# Season fixed at Spring; clo held at the Spring spatial field (nearest
# Spring segment per cell, pooled over years) and constant across panels,
# so only s(Ano) drives the year-to-year change. clo is a seasonal
# covariate, so with season fixed it cannot vary across year panels.
# (segdata_sf and clo_cap are defined above in Map 2.)
#----------------------------------------------------------

seg_spring <- segdata_sf[segdata_sf$season == "Spring", ]
idx_spring <- st_nearest_feature(pred.polys_m, seg_spring)

pred.polys_year_clo_m <- bind_rows(
  lapply(years, function(a) {
    pred.polys_m %>%
      mutate(Ano = a,
             clo = seg_spring$clo[idx_spring])
  })
) %>%
  mutate(season = factor("Spring", levels = levels(obsdata_dd_mod$season)),
         clo    = pmin(clo, clo_cap))

pred.polys_year_clo_m$Nhat <- predict(
  dd.dsm.xy.year.season.clo,
  newdata = pred.polys_year_clo_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_year_clo_m$area_m2  <- as.numeric(st_area(pred.polys_year_clo_m))
pred.polys_year_clo_m$density  <- pred.polys_year_clo_m$Nhat / (pred.polys_year_clo_m$area_m2 / 1e6)
pred.polys_year_clo_m$ldensity <- log10(pred.polys_year_clo_m$density + 0.001)

dd.map.density.year.clo <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_year_clo_m,
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
    title   = "Predicted spatial density of common dolphins (Spring)",
    caption = "model: count ~ s(x, y) + season + s(Ano) + s(clo)  |  season = Spring",
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

dd.map.density.year.clo

ggsave(
  filename = "output/CommonDolphin/DSM/DD_DSM_Year_clo.png",
  plot     = dd.map.density.year.clo,
  width    = 13,
  height   = 13
)
