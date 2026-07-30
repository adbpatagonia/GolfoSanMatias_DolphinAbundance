# ADB
# 2026-06-30
#
# Predicted density maps for dusky dolphins (Lagenorhynchus obscurus)
# Models: lo.dsm.xy.season.year    (count ~ s(x,y) + -1 + season + s(Ano))
#         lo.dsm.xy.year.season.depth  (count ~ s(x,y) + -1 + season + s(Ano) + s(depth))
#         lo.dsm.xy.fsyear.season  (count ~ s(x, y, year_fac, bs = "fs") + season)  [year-varying spatial, shrunk]
#         lo.dsm.xy.byyear.season  (count ~ s(x, y, by = year_fac) + year_fac + season)  [year-varying spatial, unshrunk]
#         lo.dsm.soap.season.year  (count ~ s(x, y, bs = "so") + season + s(Ano))  [soap-film, edge-effect controlled]
#
# Assumes the following objects are already in the workspace:
#   pred.polys_m, survey.area_m, patagonia_m, segdata, segdata_traj_m
#   obsdata_lo_mod, distdata_lo_sf_m, target_crs
#   trunc.dist_lo, lo.dsm.xy.season.year, lo.dsm.xy.year.season.depth,
#   lo.dsm.xy.fsyear.season, lo.dsm.xy.byyear.season, lo.dsm.soap.season.year

library(dsm)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)


# Helpers -----

off.set <- 800 * trunc.dist_lo

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

# Map 1 — lo.dsm.xy.season.year ----
# count ~ s(x, y) + -1 + season + s(Ano)

pred.polys_season_m <- bind_rows(
  pred.polys_m %>% mutate(season = "Summer"),
  pred.polys_m %>% mutate(season = "Spring"),
  pred.polys_m %>% mutate(season = "Winter"),
  pred.polys_m %>% mutate(season = "Fall")
) %>%
  mutate(
    season = factor(season, levels = levels(obsdata_lo_mod$season)),
    Ano    = ref_ano
  )

pred.polys_season_m$Nhat <- predict(
  lo.dsm.xy.season.year,
  newdata = pred.polys_season_m,
  off.set = off.set,
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

lo.map.density.season

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_Season.png",
  plot     = lo.map.density.season,
  width    = 13,
  height   = 13
)

# Map 2 — lo.dsm.xy.year.season.depth -----
# count ~ s(x, y) + -1 + season + s(Ano) + s(depth)
#
# depth has no seasonal variation: assign from nearest segdata point,
# then stack by season

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
    season = factor(season, levels = levels(obsdata_lo_mod$season)),
    Ano    = ref_ano,
    depth  = segdata_sf$depth[rep(idx_depth, 4)]
  )

pred.polys_season_depth_m$Nhat <- predict(
  lo.dsm.xy.year.season.depth,
  newdata = pred.polys_season_depth_m,
  off.set = off.set,
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
    caption = "model: count ~ s(x, y) + season + s(Ano) + s(depth)",
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

# Map 3 — lo.dsm.xy.season.year, one panel per YEAR (season = Spring) ----
# count ~ s(x, y) + season + s(Ano)
#
# Season fixed at the reference level (Spring); Ano varies across panels.
# NOTE: s(Ano) is additive (no x,y-by-year interaction), so every panel
# shares the SAME spatial pattern and differs only in overall level
# (the s(Ano) effect) plus the per-year survey overlays.

years <- sort(unique(segdata$Ano))

pred.polys_year_m <- bind_rows(
  lapply(years, function(a) pred.polys_m %>% mutate(Ano = a))
) %>%
  mutate(season = factor("Spring", levels = levels(obsdata_lo_mod$season)))

pred.polys_year_m$Nhat <- predict(
  lo.dsm.xy.season.year,
  newdata = pred.polys_year_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_year_m$area_m2  <- as.numeric(st_area(pred.polys_year_m))
pred.polys_year_m$density  <- pred.polys_year_m$Nhat / (pred.polys_year_m$area_m2 / 1e6)
pred.polys_year_m$ldensity <- log10(pred.polys_year_m$density + 0.001)

lo.map.density.year <- ggplot() +
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
  # overlay intentionally shows ALL seasons of that year, not just Spring: the
  # spatial term (s(x,y) here) does not vary by season, so every season's
  # survey effort that year informs the SAME year-level spatial pattern shown
  # in this panel — season only shifts the predicted level, not the shape.
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
    title   = "Predicted spatial density of dusky dolphins (Spring)",
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

lo.map.density.year

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_Year.png",
  plot     = lo.map.density.year,
  width    = 13,
  height   = 13
)

# Map 4 — lo.dsm.xy.year.season.depth, one panel per YEAR (season = Spring) ----
# count ~ s(x, y) + season + s(Ano) + s(depth)
#
# Season fixed at Spring; depth is a non-seasonal spatial field (nearest
# segment per cell) held constant across panels, so only s(Ano) drives
# the year-to-year change.
# (segdata_sf and idx_depth are defined above in Map 2.)

pred.polys_year_depth_m <- bind_rows(
  lapply(years, function(a) {
    pred.polys_m %>%
      mutate(Ano   = a,
             depth = segdata_sf$depth[idx_depth])
  })
) %>%
  mutate(season = factor("Spring", levels = levels(obsdata_lo_mod$season)))

pred.polys_year_depth_m$Nhat <- predict(
  lo.dsm.xy.year.season.depth,
  newdata = pred.polys_year_depth_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_year_depth_m$area_m2  <- as.numeric(st_area(pred.polys_year_depth_m))
pred.polys_year_depth_m$density  <- pred.polys_year_depth_m$Nhat / (pred.polys_year_depth_m$area_m2 / 1e6)
pred.polys_year_depth_m$ldensity <- log10(pred.polys_year_depth_m$density + 0.001)

lo.map.density.year.depth <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_year_depth_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data      = survey.area_m,
          fill      = NA,
          color     = "black",
          linewidth = 0.6) +
  # overlay intentionally shows ALL seasons of that year — see Map 3 (the
  # spatial term does not vary by season, only shifts the predicted level).
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
    title   = "Predicted spatial density of dusky dolphins (Spring)",
    caption = "model: count ~ s(x, y) + season + s(Ano) + s(depth)  |  season = Spring",
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

lo.map.density.year.depth

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_Year_depth.png",
  plot     = lo.map.density.year.depth,
  width    = 13,
  height   = 13
)

# Map 5 — lo.dsm.xy.fsyear.season -------
# (season facets, at reference year ref_yf_fs)
# count ~ s(x, y, year_fac, bs = "fs") + season
#
# Year-varying spatial surface. Season panels shown for one reference year
# (fitted year nearest the median); season is additive, so the 4 panels share
# that year's surface, scaled by the season effect.

yf_levels  <- levels(lo.dsm.xy.fsyear.season$model$year_fac)
ref_yf_fs  <- yf_levels[which.min(abs(as.numeric(yf_levels) - ref_ano))]
ref_yf_fs <- 2017

pred.polys_season_fs_m <- bind_rows(
  pred.polys_m %>% mutate(season = "Summer"),
  pred.polys_m %>% mutate(season = "Spring"),
  pred.polys_m %>% mutate(season = "Winter"),
  pred.polys_m %>% mutate(season = "Fall")
) %>%
  mutate(
    season   = factor(season, levels = levels(obsdata_lo_mod$season)),
    year_fac = factor(ref_yf_fs, levels = yf_levels)
  )

pred.polys_season_fs_m$Nhat <- predict(
  lo.dsm.xy.fsyear.season,
  newdata = pred.polys_season_fs_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_season_fs_m$area_m2  <- as.numeric(st_area(pred.polys_season_fs_m))
pred.polys_season_fs_m$density  <- pred.polys_season_fs_m$Nhat / (pred.polys_season_fs_m$area_m2 / 1e6)
pred.polys_season_fs_m$ldensity <- log10(pred.polys_season_fs_m$density + 0.001)

lo.map.density.season.fs <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_season_fs_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data      = survey.area_m,
          fill      = NA,
          color     = "black",
          linewidth = 0.6) +
  # overlay restricted to Ano == ref_yf_fs: unlike the thin-plate/soap season
  # maps, the fs spatial term VARIES by year, so only ref_yf_fs's own segments
  # informed the surface shown here — other years' tracks are irrelevant (and
  # would be misleading) context for this specific year's spatial pattern.
  geom_sf(data  = segdata_traj_m %>% filter(Ano == as.numeric(ref_yf_fs)),
          size  = 0.6,
          alpha = 0.25) +
  geom_sf(data  = distdata_lo_sf_m %>% filter(Ano == as.numeric(ref_yf_fs)),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option = "turbo",
    name   = expression("Dolphins km"^-2)
  ) +
  labs(
    title   = "Predicted spatial density of dusky dolphins (year-varying spatial)",
    caption = "model: count ~ s(x, y, year_fac, bs = \"fs\") + season. Ref year: 2017",
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

lo.map.density.season.fs

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_Season_fsyear.png",
  plot     = lo.map.density.season.fs,
  width    = 13,
  height   = 13
)

# Map 6 — lo.dsm.xy.fsyear.season, one panel per YEAR (season = Spring) ----
# count ~ s(x, y, year_fac, bs = "fs") + season
#
# The spatial smooth is estimated separately per year, so each panel shows a
# genuinely DIFFERENT spatial surface — the payoff of the year-varying model.
# Ano is carried alongside year_fac only to drive faceting + the per-year
# survey overlays.

years_fs <- sort(as.numeric(yf_levels))

pred.polys_year_fs_m <- bind_rows(
  lapply(years_fs, function(a) pred.polys_m %>%
           mutate(Ano      = a,
                  year_fac = factor(as.character(a), levels = yf_levels)))
) %>%
  mutate(season = factor("Spring", levels = levels(obsdata_lo_mod$season)))

pred.polys_year_fs_m$Nhat <- predict(
  lo.dsm.xy.fsyear.season,
  newdata = pred.polys_year_fs_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_year_fs_m$area_m2  <- as.numeric(st_area(pred.polys_year_fs_m))
pred.polys_year_fs_m$density  <- pred.polys_year_fs_m$Nhat / (pred.polys_year_fs_m$area_m2 / 1e6)
pred.polys_year_fs_m$ldensity <- log10(pred.polys_year_fs_m$density + 0.001)

lo.map.density.year.fs <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_year_fs_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data      = survey.area_m,
          fill      = NA,
          color     = "black",
          linewidth = 0.6) +
  # overlay intentionally shows ALL seasons of that year, not just Spring: the
  # fs spatial term s(x,y,year_fac) does not vary by season within a year, so
  # a high count from ANY season legitimately informs (and helps explain) the
  # year-level spatial pattern shown here, e.g. the 2009 west blob.
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
    title   = "Predicted spatial density of dusky dolphins (Spring, year-varying spatial)",
    caption = "model: count ~ s(x, y, year_fac, bs = \"fs\") + season  |  season = Spring",
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

lo.map.density.year.fs

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_Year_fsyear.png",
  plot     = lo.map.density.year.fs,
  width    = 13,
  height   = 13
)

# Map 7 — lo.dsm.xy.byyear.season -------
# (season facets, at reference year ref_yf_byyear)
# count ~ s(x, y, by = year_fac) + year_fac + season
#
# Year-varying, UNSHRUNK spatial surface (a separate surface per year, each
# with its own smoothing parameter — unlike the fs model, which shares one
# smoothing parameter and shrinks years toward the mean). Season panels shown
# for one reference year (fitted year nearest the median); season is additive,
# so the 4 panels share that year's surface, scaled by the season effect.

yf_levels_byyear <- levels(lo.dsm.xy.byyear.season$model$year_fac)
ref_yf_byyear     <- yf_levels_byyear[which.min(abs(as.numeric(yf_levels_byyear) - ref_ano))]

pred.polys_season_byyear_m <- bind_rows(
  pred.polys_m %>% mutate(season = "Summer"),
  pred.polys_m %>% mutate(season = "Spring"),
  pred.polys_m %>% mutate(season = "Winter"),
  pred.polys_m %>% mutate(season = "Fall")
) %>%
  mutate(
    season   = factor(season, levels = levels(obsdata_lo_mod$season)),
    year_fac = factor(ref_yf_byyear, levels = yf_levels_byyear)
  )

pred.polys_season_byyear_m$Nhat <- predict(
  lo.dsm.xy.byyear.season,
  newdata = pred.polys_season_byyear_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_season_byyear_m$area_m2  <- as.numeric(st_area(pred.polys_season_byyear_m))
pred.polys_season_byyear_m$density  <- pred.polys_season_byyear_m$Nhat / (pred.polys_season_byyear_m$area_m2 / 1e6)
pred.polys_season_byyear_m$ldensity <- log10(pred.polys_season_byyear_m$density + 0.001)

lo.map.density.season.byyear <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_season_byyear_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data      = survey.area_m,
          fill      = NA,
          color     = "black",
          linewidth = 0.6) +
  # overlay restricted to Ano == ref_yf_byyear — see Map 5 (the by=year_fac
  # spatial term varies by year, so only that year's segments are relevant).
  geom_sf(data  = segdata_traj_m %>% filter(Ano == as.numeric(ref_yf_byyear)),
          size  = 0.6,
          alpha = 0.25) +
  geom_sf(data  = distdata_lo_sf_m %>% filter(Ano == as.numeric(ref_yf_byyear)),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option = "turbo",
    name   = expression("Dolphins km"^-2)
  ) +
  labs(
    title   = "Predicted spatial density of dusky dolphins (year-varying spatial, unshrunk)",
    caption = "model: count ~ s(x, y, by = year_fac) + year_fac + season",
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

lo.map.density.season.byyear

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_Season_byyear.png",
  plot     = lo.map.density.season.byyear,
  width    = 13,
  height   = 13
)

# Map 8 — lo.dsm.xy.byyear.season, one panel per YEAR (season = Spring) ----
# count ~ s(x, y, by = year_fac) + year_fac + season
#
# Each panel shows a genuinely DIFFERENT, UNSHRUNK spatial surface — a separate
# fit per year (vs. the fs model's shared-smoothing-parameter, shrunk surfaces).
# Ano is carried alongside year_fac only to drive faceting + the per-year
# survey overlays.

years_byyear <- sort(as.numeric(yf_levels_byyear))

pred.polys_year_byyear_m <- bind_rows(
  lapply(years_byyear, function(a) pred.polys_m %>%
           mutate(Ano      = a,
                  year_fac = factor(as.character(a), levels = yf_levels_byyear)))
) %>%
  mutate(season = factor("Spring", levels = levels(obsdata_lo_mod$season)))

pred.polys_year_byyear_m$Nhat <- predict(
  lo.dsm.xy.byyear.season,
  newdata = pred.polys_year_byyear_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_year_byyear_m$area_m2  <- as.numeric(st_area(pred.polys_year_byyear_m))
pred.polys_year_byyear_m$density  <- pred.polys_year_byyear_m$Nhat / (pred.polys_year_byyear_m$area_m2 / 1e6)
pred.polys_year_byyear_m$ldensity <- log10(pred.polys_year_byyear_m$density + 0.001)

lo.map.density.year.byyear <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_year_byyear_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data      = survey.area_m,
          fill      = NA,
          color     = "black",
          linewidth = 0.6) +
  # overlay intentionally shows ALL seasons of that year — see Map 3 (the
  # by=year_fac spatial term does not vary by season within a year).
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
    title   = "Predicted spatial density of dusky dolphins (Spring, year-varying spatial, unshrunk)",
    caption = "model: count ~ s(x, y, by = year_fac) + year_fac + season  |  season = Spring",
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

lo.map.density.year.byyear

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_Year_byyear.png",
  plot     = lo.map.density.year.byyear,
  width    = 13,
  height   = 13
)

# Map 9 — lo.dsm.soap.season.year -------
# (season facets, at reference year ref_ano)
# count ~ s(x, y, bs = "so") + season + s(Ano)
#
# Soap-film shared spatial surface (edge-effect controlled, respects the gulf
# boundary — see UTIL_DuskyDolphin_EdgeEffects.R for how the boundary/knots
# were built). Like the thin-plate season.year model, the spatial term is
# SHARED across years (only s(Ano) shifts the level), so these 4 panels differ
# only in the season effect, at the fitted year nearest the median.
#
# predict() on a soap-film model automatically returns NA for grid cells
# outside the fitted boundary (no manual masking needed) — those cells are
# rendered as the na.value grey below.

pred.polys_season_soap_m <- bind_rows(
  pred.polys_m %>% mutate(season = "Summer"),
  pred.polys_m %>% mutate(season = "Spring"),
  pred.polys_m %>% mutate(season = "Winter"),
  pred.polys_m %>% mutate(season = "Fall")
) %>%
  mutate(
    season = factor(season, levels = levels(obsdata_lo_mod$season)),
    Ano    = ref_ano
  )

pred.polys_season_soap_m$Nhat <- predict(
  lo.dsm.soap.season.year,
  newdata = pred.polys_season_soap_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_season_soap_m$area_m2  <- as.numeric(st_area(pred.polys_season_soap_m))
pred.polys_season_soap_m$density  <- pred.polys_season_soap_m$Nhat / (pred.polys_season_soap_m$area_m2 / 1e6)
pred.polys_season_soap_m$ldensity <- log10(pred.polys_season_soap_m$density + 0.001)

lo.map.density.season.soap <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_season_soap_m,
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
    option   = "turbo",
    name     = expression("Dolphins km"^-2),
    na.value = "grey95"
  ) +
  labs(
    title   = "Predicted spatial density of dusky dolphins (soap-film)",
    caption = "model: count ~ s(x, y, bs = \"so\") + season + s(Ano)",
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

lo.map.density.season.soap

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_Season_soap.png",
  plot     = lo.map.density.season.soap,
  width    = 13,
  height   = 13
)

# Map 10 — lo.dsm.soap.season.year, one panel per YEAR (season = Spring) ----
# count ~ s(x, y, bs = "so") + season + s(Ano)
#
# Season fixed at Spring; Ano varies across panels. NOTE: the soap term is a
# single SHARED surface (no x,y-by-year interaction), so — like Map 3 for the
# thin-plate season.year model — every panel shows the SAME spatial pattern
# and differs only in overall level (the s(Ano) effect) plus the per-year
# survey overlays. Cells outside the fitted soap boundary predict as NA
# (rendered grey) automatically.

pred.polys_year_soap_m <- bind_rows(
  lapply(years, function(a) pred.polys_m %>% mutate(Ano = a))
) %>%
  mutate(season = factor("Spring", levels = levels(obsdata_lo_mod$season)))

pred.polys_year_soap_m$Nhat <- predict(
  lo.dsm.soap.season.year,
  newdata = pred.polys_year_soap_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_year_soap_m$area_m2  <- as.numeric(st_area(pred.polys_year_soap_m))
pred.polys_year_soap_m$density  <- pred.polys_year_soap_m$Nhat / (pred.polys_year_soap_m$area_m2 / 1e6)
pred.polys_year_soap_m$ldensity <- log10(pred.polys_year_soap_m$density + 0.001)

lo.map.density.year.soap <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_year_soap_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data      = survey.area_m,
          fill      = NA,
          color     = "black",
          linewidth = 0.6) +
  # overlay intentionally shows ALL seasons of that year — see Map 3 (the soap
  # spatial term is a single shared surface, invariant across seasons).
  geom_sf(data  = segdata_traj_m %>% filter(Ano > 2006),
          size  = 0.6,
          alpha = 0.25) +
  geom_sf(data  = distdata_lo_sf_m %>% filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option   = "turbo",
    name     = expression("Dolphins km"^-2),
    na.value = "grey95"
  ) +
  labs(
    title   = "Predicted spatial density of dusky dolphins (Spring, soap-film)",
    caption = "model: count ~ s(x, y, bs = \"so\") + season + s(Ano)  |  season = Spring",
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

lo.map.density.year.soap

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_Year_soap.png",
  plot     = lo.map.density.year.soap,
  width    = 13,
  height   = 13
)

# Map 11 — lo.dsm.xy.fsyear.season, one panel per (season, year) combination ----
# count ~ s(x, y, year_fac, bs = "fs") + season
#
# Every combination of year_fac (from the fitted model) x season is its own
# facet, ordered chronologically: earliest year first, cycling
# Summer -> Fall -> Winter -> Spring within each year (e.g. 2007-Summer,
# 2007-Fall, 2007-Winter, 2007-Spring, 2008-Summer, ...). This is the FULL
# cross of year x season, not filtered to combinations actually surveyed —
# season enters the fs model additively (no season x year interaction), so any
# season can be predicted for any fitted year.

season_order_sy <- c("Summer", "Fall", "Winter", "Spring")

pred.polys_yearseason_fs_m <- bind_rows(
  lapply(yf_levels, function(yy) {
    bind_rows(lapply(season_order_sy, function(ss) {
      pred.polys_m %>%
        mutate(
          year_fac = factor(yy, levels = yf_levels),
          season   = factor(ss, levels = levels(obsdata_lo_mod$season))
        )
    }))
  })
)

pred.polys_yearseason_fs_m$Nhat <- predict(
  lo.dsm.xy.fsyear.season,
  newdata = pred.polys_yearseason_fs_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_yearseason_fs_m$area_m2 <- as.numeric(st_area(pred.polys_yearseason_fs_m))
pred.polys_yearseason_fs_m$density <- pred.polys_yearseason_fs_m$Nhat / (pred.polys_yearseason_fs_m$area_m2 / 1e6)

# facet label, ordered chronologically: year ascending, season cycling
# Summer -> Fall -> Winter -> Spring within each year
pred.polys_yearseason_fs_m <- pred.polys_yearseason_fs_m %>%
  mutate(
    year_num    = as.numeric(as.character(year_fac)),
    year_season = factor(
      paste(year_num, season, sep = "-"),
      levels = paste(rep(sort(as.numeric(yf_levels)), each = 4),
                     rep(season_order_sy, times = length(yf_levels)),
                     sep = "-")
    )
  )

# tag the overlay layers with the SAME year_season factor (identical labels
# and level order) as the raster layer, so ggplot's per-layer facet subsetting
# shows each panel only the tracks/sightings from that season-year combination
# — otherwise segdata_traj_m/distdata_lo_sf_m (which only carry Ano and season
# separately) are repeated in full in every panel.
segdata_traj_yearseason_m <- segdata_traj_m %>%
  filter(Ano %in% as.numeric(yf_levels)) %>%
  mutate(year_season = factor(paste(Ano, season, sep = "-"),
                              levels = levels(pred.polys_yearseason_fs_m$year_season)))

distdata_yearseason_lo_sf_m <- distdata_lo_sf_m %>%
  filter(Ano %in% as.numeric(yf_levels)) %>%
  mutate(year_season = factor(paste(Ano, season, sep = "-"),
                              levels = levels(pred.polys_yearseason_fs_m$year_season)))

lo.map.density.yearseason.fs <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_yearseason_fs_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data      = survey.area_m,
          fill      = NA,
          color     = "black",
          linewidth = 0.6) +
  geom_sf(data  = segdata_traj_yearseason_m,
          size  = 0.6,
          alpha = 0.25) +
  geom_sf(data  = distdata_yearseason_lo_sf_m,
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option = "turbo",
    name   = expression("Dolphins km"^-2)
  ) +
  labs(
    title   = "Predicted spatial density of dusky dolphins by season and year",
    caption = "model: count ~ s(x, y, year_fac, bs = \"fs\") + season",
    x = "Easting (Mm)",
    y = "Northing (Mm)"
  ) +
  theme_minimal(base_size = 11) +
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
  facet_wrap(~ year_season, ncol = 4)

lo.map.density.yearseason.fs

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_YearSeason_fsyear.png",
  plot     = lo.map.density.yearseason.fs,
  width    = 14,
  height   = 3.2 * length(yf_levels)
)

# Map 12 — lo.dsm.xy.fsyear.season, blank panels for un-surveyed season x year ----
# Same grid as Map 11 (reused, no re-prediction needed), but panels for a
# season x year combination that was NEVER actually surveyed are blanked out
# (density set to NA -> rendered grey) rather than showing a prediction.
#
# This directly addresses the risk illustrated by the 2015 south-edge blob: a
# year-varying spatial model (fs or by=year_fac) still has to produce SOME
# value at every grid cell for every fitted year, including season x year
# combinations with ZERO survey effort anywhere in the study area that period.
# Away from any data, that value is pure basis-function extrapolation and can
# be misleadingly large. Blanking by (season, year) combination is a blunter
# but more certain safeguard than distance-based (exclude.too.far) masking: it
# does not depend on a distance threshold, only on whether that combination
# was surveyed AT ALL.

surveyed_combos <- unique(segdata[, .(season = as.character(season), Ano)])
surveyed_key     <- paste(surveyed_combos$Ano, surveyed_combos$season, sep = "-")

pred.polys_yearseason_fs_m <- pred.polys_yearseason_fs_m %>%
  mutate(
    surveyed        = paste(year_num, as.character(season), sep = "-") %in% surveyed_key,
    density_surveyed = ifelse(surveyed, density, NA_real_)
  )

lo.map.density.yearseason.fs.blank <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_yearseason_fs_m,
          aes(fill = density_surveyed),
          color = NA) +
  geom_sf(data      = survey.area_m,
          fill      = NA,
          color     = "black",
          linewidth = 0.6) +
  geom_sf(data  = segdata_traj_yearseason_m,
          size  = 0.6,
          alpha = 0.25) +
  geom_sf(data  = distdata_yearseason_lo_sf_m,
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option   = "turbo",
    name     = expression("Dolphins km"^-2),
    na.value = "grey98"
  ) +
  labs(
    title    = "Predicted spatial density of dusky dolphins by season and year",
    subtitle = "blank panels = no survey conducted in that season-year combination",
    caption  = "model: count ~ s(x, y, year_fac, bs = \"fs\") + season",
    x = "Easting (Mm)",
    y = "Northing (Mm)"
  ) +
  theme_minimal(base_size = 11) +
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
  facet_wrap(~ year_season, ncol = 4)

lo.map.density.yearseason.fs.blank

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_YearSeason_fsyear_surveyedonly.png",
  plot     = lo.map.density.yearseason.fs.blank,
  width    = 14,
  height   = 3.2 * length(yf_levels)
)
