# ADB
# 2026-06-30
#
# Predicted density maps for common dolphins (Delphinus delphis)
# Models: dd.dsm.xy.season.year    (count ~ s(x,y) + -1 + season + s(Ano))
#         dd.dsm.xy.year.season.clo  (count ~ s(x,y) + -1 + season + s(Ano) + s(clo))
#         dd.dsm.xy.fsyear.season  (count ~ s(x, y, year_fac, bs = "fs") + season)  [year-varying spatial, shrunk]
#         dd.dsm.xy.byyear.season  (count ~ s(x, y, by = year_fac) + year_fac + season)  [year-varying spatial, unshrunk]
#
# Assumes the following objects are already in the workspace:
#   pred.polys_m, survey.area_m, patagonia_m, segdata, segdata_traj_m
#   obsdata_dd_mod, distdata_dd_sf_m, target_crs
#   trunc.dist_dd, dd.dsm.xy.season.year, dd.dsm.xy.year.season.clo,
#   dd.dsm.xy.fsyear.season, dd.dsm.xy.byyear.season

# Output directories must exist before any ggsave()/fwrite() below.
source(file.path(here::here(), "analysis", "UTIL_EnsureOutputDirs.R"))

library(dsm)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)


# Helpers -----

# ---------------------------------------------------------------------------
# DENSITY OFFSET -- CORRECTED 2026-09-16. READ THIS BEFORE CHANGING IT BACK.
#
# Every map here used to predict with a CONSTANT offset,
#     off.set <- 800 * trunc.dist_dd          # = 260000 m2
# and then divide the result by the cell area to get density. That does not
# convert anything: a prediction made with a constant off.set does not depend on
# cell area at all, so dividing by it only rescales. The maps were therefore
# 4.49x too LOW (the factor is median(cell_area)/260000 = 4.495, and it varies
# only 4.222-4.508 across cells, which is why the spatial PATTERN still looked
# right while the legend did not).
#
# Verified against the project's own abundance output rather than by derivation:
# predicting the stored soap model with off.set = cell area and summing over the
# grid reproduces DD_abundance_season_year_soap.csv exactly --
#     Fall 2006  4200.7 vs 4201 stored     Winter 2006  1628.4 vs 1628
#     Spring 2006 3919.5 vs 3920           Summer 2007  3704.7 vs 3705
# (ratios 0.9999-1.0002). So 5_CommonDolphin_Abundance.R, which already used
# off.set = cell_area_m2, was right and these maps were wrong.
#
# The offset is now taken PER GRID rather than as one shared vector, because the
# prediction frames here are stacks of the grid (4 seasons, or one panel per
# year). A single 1408-long vector would be silently recycled against a
# 5632-row frame; it happens to recycle correctly when the stack is grid-major,
# but that is a property of how the frames are built, not something to rely on.
.cell_off <- function(x) as.numeric(st_area(x))
# ---------------------------------------------------------------------------

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

# Map 1 — dd.dsm.xy.season.year ----
# count ~ s(x, y) + -1 + season + s(Ano)

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

pred.polys_season_m$Nhat <- predict(
  dd.dsm.xy.season.year,
  newdata = pred.polys_season_m,
  off.set = .cell_off(pred.polys_season_m),
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

# Map 2 — dd.dsm.xy.year.season.clo -----
# count ~ s(x, y) + -1 + season + s(Ano) + s(clo)
#
# clo (chlorophyll) varies by season: assign from the nearest segdata point
# WITHIN that season, per prediction cell

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
  off.set = .cell_off(pred.polys_season_clo_m),
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

# Map 3 — dd.dsm.xy.season.year, one panel per YEAR (season = Spring) ----
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
  mutate(season = factor("Spring", levels = levels(obsdata_dd_mod$season)))

pred.polys_year_m$Nhat <- predict(
  dd.dsm.xy.season.year,
  newdata = pred.polys_year_m,
  off.set = .cell_off(pred.polys_year_m),
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
  # overlay intentionally shows ALL seasons of that year, not just Spring: the
  # spatial term (s(x,y) here) does not vary by season, so every season's
  # survey effort that year informs the SAME year-level spatial pattern shown
  # in this panel — season only shifts the predicted level, not the shape.
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

# Map 4 — dd.dsm.xy.year.season.clo, one panel per YEAR (season = Spring) ----
# count ~ s(x, y) + season + s(Ano) + s(clo)
#
# Season fixed at Spring; clo held at the Spring spatial field (nearest
# Spring segment per cell, pooled over years) and constant across panels,
# so only s(Ano) drives the year-to-year change. clo is a seasonal
# covariate, so with season fixed it cannot vary across year panels.
# (segdata_sf and clo_cap are defined above in Map 2.)

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
  off.set = .cell_off(pred.polys_year_clo_m),
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
  # overlay intentionally shows ALL seasons of that year — see Map 3 (the
  # spatial term does not vary by season, only shifts the predicted level).
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

# Map 5 — dd.dsm.xy.fsyear.season -------
# (season facets, at reference year ref_yf_fs)
# count ~ s(x, y, year_fac, bs = "fs") + season
#
# Year-varying spatial surface. Season panels shown for one reference year
# (fitted year nearest the median); season is additive, so the 4 panels share
# that year's surface, scaled by the season effect.

yf_levels  <- levels(dd.dsm.xy.fsyear.season$model$year_fac)
ref_yf_fs  <- yf_levels[which.min(abs(as.numeric(yf_levels) - ref_ano))]

pred.polys_season_fs_m <- bind_rows(
  pred.polys_m %>% mutate(season = "Summer"),
  pred.polys_m %>% mutate(season = "Spring"),
  pred.polys_m %>% mutate(season = "Winter"),
  pred.polys_m %>% mutate(season = "Fall")
) %>%
  mutate(
    season   = factor(season, levels = levels(obsdata_dd_mod$season)),
    year_fac = factor(ref_yf_fs, levels = yf_levels)
  )

pred.polys_season_fs_m$Nhat <- predict(
  dd.dsm.xy.fsyear.season,
  newdata = pred.polys_season_fs_m,
  off.set = .cell_off(pred.polys_season_fs_m),
  type    = "response"
)

pred.polys_season_fs_m$area_m2  <- as.numeric(st_area(pred.polys_season_fs_m))
pred.polys_season_fs_m$density  <- pred.polys_season_fs_m$Nhat / (pred.polys_season_fs_m$area_m2 / 1e6)
pred.polys_season_fs_m$ldensity <- log10(pred.polys_season_fs_m$density + 0.001)

dd.map.density.season.fs <- ggplot() +
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
  geom_sf(data  = distdata_dd_sf_m %>% filter(Ano == as.numeric(ref_yf_fs)),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option = "turbo",
    name   = expression("Dolphins km"^-2)
  ) +
  labs(
    title   = "Predicted spatial density of common dolphins (year-varying spatial)",
    caption = "model: count ~ s(x, y, year_fac, bs = \"fs\") + season",
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

dd.map.density.season.fs

ggsave(
  filename = "output/CommonDolphin/DSM/DD_DSM_Season_fsyear.png",
  plot     = dd.map.density.season.fs,
  width    = 13,
  height   = 13
)

# Map 6 — dd.dsm.xy.fsyear.season, one panel per YEAR (season = Spring) ----
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
  mutate(season = factor("Spring", levels = levels(obsdata_dd_mod$season)))

pred.polys_year_fs_m$Nhat <- predict(
  dd.dsm.xy.fsyear.season,
  newdata = pred.polys_year_fs_m,
  off.set = .cell_off(pred.polys_year_fs_m),
  type    = "response"
)

pred.polys_year_fs_m$area_m2  <- as.numeric(st_area(pred.polys_year_fs_m))
pred.polys_year_fs_m$density  <- pred.polys_year_fs_m$Nhat / (pred.polys_year_fs_m$area_m2 / 1e6)
pred.polys_year_fs_m$ldensity <- log10(pred.polys_year_fs_m$density + 0.001)

dd.map.density.year.fs <- ggplot() +
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
  geom_sf(data  = distdata_dd_sf_m %>% filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option = "turbo",
    name   = expression("Dolphins km"^-2)
  ) +
  labs(
    title   = "Predicted spatial density of common dolphins (Spring, year-varying spatial)",
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

dd.map.density.year.fs

ggsave(
  filename = "output/CommonDolphin/DSM/DD_DSM_Year_fsyear.png",
  plot     = dd.map.density.year.fs,
  width    = 13,
  height   = 13
)

# Map 7 — dd.dsm.xy.byyear.season -------
# (season facets, at reference year ref_yf_byyear)
# count ~ s(x, y, by = year_fac) + year_fac + season
#
# Year-varying, UNSHRUNK spatial surface (a separate surface per year, each
# with its own smoothing parameter — unlike the fs model, which shares one
# smoothing parameter and shrinks years toward the mean). Season panels shown
# for one reference year (fitted year nearest the median); season is additive,
# so the 4 panels share that year's surface, scaled by the season effect.

yf_levels_byyear <- levels(dd.dsm.xy.byyear.season$model$year_fac)
ref_yf_byyear     <- yf_levels_byyear[which.min(abs(as.numeric(yf_levels_byyear) - ref_ano))]

pred.polys_season_byyear_m <- bind_rows(
  pred.polys_m %>% mutate(season = "Summer"),
  pred.polys_m %>% mutate(season = "Spring"),
  pred.polys_m %>% mutate(season = "Winter"),
  pred.polys_m %>% mutate(season = "Fall")
) %>%
  mutate(
    season   = factor(season, levels = levels(obsdata_dd_mod$season)),
    year_fac = factor(ref_yf_byyear, levels = yf_levels_byyear)
  )

pred.polys_season_byyear_m$Nhat <- predict(
  dd.dsm.xy.byyear.season,
  newdata = pred.polys_season_byyear_m,
  off.set = .cell_off(pred.polys_season_byyear_m),
  type    = "response"
)

pred.polys_season_byyear_m$area_m2  <- as.numeric(st_area(pred.polys_season_byyear_m))
pred.polys_season_byyear_m$density  <- pred.polys_season_byyear_m$Nhat / (pred.polys_season_byyear_m$area_m2 / 1e6)
pred.polys_season_byyear_m$ldensity <- log10(pred.polys_season_byyear_m$density + 0.001)

dd.map.density.season.byyear <- ggplot() +
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
  geom_sf(data  = distdata_dd_sf_m %>% filter(Ano == as.numeric(ref_yf_byyear)),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option = "turbo",
    name   = expression("Dolphins km"^-2)
  ) +
  labs(
    title   = "Predicted spatial density of common dolphins (year-varying spatial, unshrunk)",
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

dd.map.density.season.byyear

ggsave(
  filename = "output/CommonDolphin/DSM/DD_DSM_Season_byyear.png",
  plot     = dd.map.density.season.byyear,
  width    = 13,
  height   = 13
)

# Map 8 — dd.dsm.xy.byyear.season, one panel per YEAR (season = Spring) ----
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
  mutate(season = factor("Spring", levels = levels(obsdata_dd_mod$season)))

pred.polys_year_byyear_m$Nhat <- predict(
  dd.dsm.xy.byyear.season,
  newdata = pred.polys_year_byyear_m,
  off.set = .cell_off(pred.polys_year_byyear_m),
  type    = "response"
)

pred.polys_year_byyear_m$area_m2  <- as.numeric(st_area(pred.polys_year_byyear_m))
pred.polys_year_byyear_m$density  <- pred.polys_year_byyear_m$Nhat / (pred.polys_year_byyear_m$area_m2 / 1e6)
pred.polys_year_byyear_m$ldensity <- log10(pred.polys_year_byyear_m$density + 0.001)

dd.map.density.year.byyear <- ggplot() +
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
  geom_sf(data  = distdata_dd_sf_m %>% filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option = "turbo",
    name   = expression("Dolphins km"^-2)
  ) +
  labs(
    title   = "Predicted spatial density of common dolphins (Spring, year-varying spatial, unshrunk)",
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

dd.map.density.year.byyear

ggsave(
  filename = "output/CommonDolphin/DSM/DD_DSM_Year_byyear.png",
  plot     = dd.map.density.year.byyear,
  width    = 13,
  height   = 13
)

# Map 9 — dd.dsm.soap.season.year -------
# (season facets, at reference year ref_ano)
# count ~ s(x, y, bs = "so") + season + s(Ano)
#
# Soap-film shared spatial surface (edge-effect controlled, respects the gulf
# boundary — see UTIL_CommonDolphin_EdgeEffects.R for how the boundary/knots
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
    season = factor(season, levels = levels(obsdata_dd_mod$season)),
    Ano    = ref_ano
  )

pred.polys_season_soap_m$Nhat <- predict(
  dd.dsm.soap.season.year,
  newdata = pred.polys_season_soap_m,
  off.set = .cell_off(pred.polys_season_soap_m),
  type    = "response"
)

pred.polys_season_soap_m$area_m2  <- as.numeric(st_area(pred.polys_season_soap_m))
pred.polys_season_soap_m$density  <- pred.polys_season_soap_m$Nhat / (pred.polys_season_soap_m$area_m2 / 1e6)
pred.polys_season_soap_m$ldensity <- log10(pred.polys_season_soap_m$density + 0.001)

dd.map.density.season.soap <- ggplot() +
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
  geom_sf(data  = distdata_dd_sf_m %>% filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option   = "turbo",
    name     = expression("Dolphins km"^-2),
    na.value = "grey95"
  ) +
  labs(
    title   = "Predicted spatial density of common dolphins (soap-film)",
    caption = "model: count ~ s(x, y, bs = \"so\") + season + s(Ano). Ref year: 2015",
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

dd.map.density.season.soap

ggsave(
  filename = "output/CommonDolphin/DSM/DD_DSM_Season_soap.png",
  plot     = dd.map.density.season.soap,
  width    = 13,
  height   = 13
)

# Map 10 — dd.dsm.soap.season.year, one panel per YEAR (season = Spring) ----
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
  mutate(season = factor("Spring", levels = levels(obsdata_dd_mod$season)))

pred.polys_year_soap_m$Nhat <- predict(
  dd.dsm.soap.season.year,
  newdata = pred.polys_year_soap_m,
  off.set = .cell_off(pred.polys_year_soap_m),
  type    = "response"
)

pred.polys_year_soap_m$area_m2  <- as.numeric(st_area(pred.polys_year_soap_m))
pred.polys_year_soap_m$density  <- pred.polys_year_soap_m$Nhat / (pred.polys_year_soap_m$area_m2 / 1e6)
pred.polys_year_soap_m$ldensity <- log10(pred.polys_year_soap_m$density + 0.001)

dd.map.density.year.soap <- ggplot() +
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
  geom_sf(data  = distdata_dd_sf_m %>% filter(Ano > 2006),
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option   = "turbo",
    name     = expression("Dolphins km"^-2),
    na.value = "grey95"
  ) +
  labs(
    title   = "Predicted spatial density of common dolphins (Spring, soap-film)",
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

dd.map.density.year.soap

ggsave(
  filename = "output/CommonDolphin/DSM/DD_DSM_Year_soap.png",
  plot     = dd.map.density.year.soap,
  width    = 13,
  height   = 13
)

# Map 11 — dd.dsm.xy.fsyear.season, one panel per (season, year) combination ----
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
          season   = factor(ss, levels = levels(obsdata_dd_mod$season))
        )
    }))
  })
)

pred.polys_yearseason_fs_m$Nhat <- predict(
  dd.dsm.xy.fsyear.season,
  newdata = pred.polys_yearseason_fs_m,
  off.set = .cell_off(pred.polys_yearseason_fs_m),
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
# — otherwise segdata_traj_m/distdata_dd_sf_m (which only carry Ano and season
# separately) are repeated in full in every panel.
segdata_traj_yearseason_m <- segdata_traj_m %>%
  filter(Ano %in% as.numeric(yf_levels)) %>%
  mutate(year_season = factor(paste(Ano, season, sep = "-"),
                              levels = levels(pred.polys_yearseason_fs_m$year_season)))

distdata_yearseason_dd_sf_m <- distdata_dd_sf_m %>%
  filter(Ano %in% as.numeric(yf_levels)) %>%
  mutate(year_season = factor(paste(Ano, season, sep = "-"),
                              levels = levels(pred.polys_yearseason_fs_m$year_season)))

dd.map.density.yearseason.fs <- ggplot() +
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
  geom_sf(data  = distdata_yearseason_dd_sf_m,
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option = "turbo",
    name   = expression("Dolphins km"^-2)
  ) +
  labs(
    title   = "Predicted spatial density of common dolphins by season and year",
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

dd.map.density.yearseason.fs

ggsave(
  filename = "output/CommonDolphin/DSM/DD_DSM_YearSeason_fsyear.png",
  plot     = dd.map.density.yearseason.fs,
  width    = 14,
  height   = 3.2 * length(yf_levels)
)

# Map 12 — dd.dsm.xy.fsyear.season, blank panels for un-surveyed season x year ----
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

dd.map.density.yearseason.fs.blank <- ggplot() +
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
  geom_sf(data  = distdata_yearseason_dd_sf_m,
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option   = "turbo",
    name     = expression("Dolphins km"^-2),
    na.value = "grey98"
  ) +
  labs(
    title    = "Predicted spatial density of common dolphins by season and year",
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

# dd.map.density.yearseason.fs.blank

ggsave(
  filename = "output/CommonDolphin/DSM/DD_DSM_YearSeason_fsyear_surveyedonly.png",
  plot     = dd.map.density.yearseason.fs.blank,
  width    = 14,
  height   = 3.2 * length(yf_levels)
)


# ===========================================================================
# Map 12 - TUNED SOAP ARM, 4 x 2 tile: season (columns) x model (rows)
#          rows = base  count ~ s(x,y,so) + season + s(Ano)
#                 +sst  count ~ s(x,y,so) + season + s(Ano) + s(sst, k = 20)
#
# Both models come straight out of 4_CommonDolphin_DSM_soap.R at the tuned
# configuration (boundary tol500/margin250, 14x11 -> 89 knots, K_COV = 20), so
# this panel shows what the environmental covariate actually buys in space.
#
# TWO THINGS THAT ARE NOT OPTIONAL HERE
#
# 1. sst IS A MONTHLY CLIMATOLOGY. preddata carries one value per cell per
#    calendar month (Mes_n 1-12) and has no Ano. Averaging sst over the three
#    months of a season and predicting once would be wrong wherever s(sst) is
#    curved, because mean(f(sst)) != f(mean(sst)), and the within-season monthly
#    spread PER CELL is large: median range 4.29 degC in Spring and 3.62 in
#    Fall. So each season is predicted at EACH of its three months and the
#    resulting densities are averaged -- mean(f(sst)), the quantity wanted.
#
# 2. THE sst TERM IS IDENTICAL IN EVERY YEAR, by construction, because the
#    climatology has no year dimension. All interannual variation still comes
#    from s(Ano). Do not read these panels as sst-driven change.
#
# Only the 1353 of 1408 grid cells that carry covariates can be shown for the
# +sst row (the other 55 are 3.9% of grid area and 0.35% of predicted N), so
# BOTH rows are drawn on that common footprint to keep them comparable.
#
# Both a linear and a log10 colour version are written. The density field is
# strongly skewed -- two small hotspots reach ~55 km^-2 against a median near 2
# -- so on a linear scale the gulf renders as one flat dark field. The linear
# version is the honest depiction of magnitude; read spatial structure off the
# log one.
# ===========================================================================
if (exists("dd.dsm.soap.season.year") && exists("dd.dsm.soap.year.season.sst") &&
    exists("preddata")) {

  .tuned_models <- list(base = dd.dsm.soap.season.year,
                        sst  = dd.dsm.soap.year.season.sst)

  .pd_sst <- as.data.frame(preddata)[, c("id", "Mes_n", "season", "sst")]
  .grid0  <- pred.polys_m
  .grid0$area_m2 <- as.numeric(st_area(.grid0))
  .grid0$id_int  <- suppressWarnings(as.integer(as.character(.grid0$id)))
  .ids_cov <- sort(unique(.pd_sst$id))
  .grid_cov <- .grid0[.grid0$id_int %in% .ids_cov, ]
  message(sprintf("tuned 4x2 map: %d/%d cells carry covariates (%.1f%% of area)",
                  nrow(.grid_cov), nrow(.grid0),
                  100 * sum(.grid_cov$area_m2) / sum(.grid0$area_m2)))

  # NOTE: pred.polys_m has columns `x` and `y`. Never name a loop variable after
  # a grid column and then reference it inside mutate()/transform() -- data
  # masking resolves the COLUMN, so `Ano = y` silently binds the year to the
  # northing (~5e6), s(Ano) extrapolates, and every prediction becomes Inf.
  .tuned_density <- function(model, .ssn) {
    .mo <- sort(unique(.pd_sst$Mes_n[.pd_sst$season == .ssn]))
    stopifnot(length(.mo) == 3L)
    .acc <- rep(0, nrow(.grid_cov))
    for (.m in .mo) {
      .g <- st_drop_geometry(.grid_cov)
      .g$season <- factor(.ssn, levels = levels(segdata$season))
      .g$Ano    <- ref_ano
      .this_mo  <- .pd_sst[.pd_sst$Mes_n == .m, ]
      .g$sst    <- .this_mo$sst[match(.g$id_int, .this_mo$id)]
      stopifnot(!anyNA(.g$sst))
      .p <- predict(model, newdata = .g,
                    off.set = .grid_cov$area_m2 / 3, type = "response")
      stopifnot(all(is.finite(.p)))
      .acc <- .acc + .p
    }
    .acc / (.grid_cov$area_m2 / 1e6)          # dolphins km^-2
  }

  .tuned_tiles <- do.call(rbind, lapply(names(.tuned_models), function(.mk) {
    do.call(rbind, lapply(levels(segdata$season), function(.ssn) {
      out <- .grid_cov["geometry"]
      out$density <- .tuned_density(.tuned_models[[.mk]], .ssn)
      out$season  <- factor(.ssn, levels = levels(segdata$season))
      out$model   <- factor(.mk, levels = c("base", "sst"),
                            labels = c("base: season + s(Ano)",
                                       "+ s(sst, k = 20)"))
      out
    }))
  }))

  .tuned_map <- function(dat, ttl, cap, log_scale = FALSE) {
    d <- if (log_scale) dat[dat$density > 0, ] else dat
    ggplot() +
      geom_sf(data = patagonia_m, fill = "grey85", color = "grey40") +
      geom_sf(data = d, aes(fill = density), color = NA) +
      geom_sf(data = survey.area_m, fill = NA, color = "black", linewidth = 0.5) +
      geom_sf(data = segdata_traj_m %>% filter(Ano > 2006), size = 0.5, alpha = 0.15) +
      (if (log_scale)
         scale_fill_viridis_c(option = "turbo", trans = "log10",
                              name = expression("Dolphins km"^-2),
                              labels = \(v) format(v, drop0trailing = TRUE,
                                                   scientific = FALSE))
       else
         scale_fill_viridis_c(option = "turbo",
                              name = expression("Dolphins km"^-2))) +
      labs(title = ttl, caption = cap, x = "Easting (Mm)", y = "Northing (Mm)") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "right", panel.grid.minor = element_blank()) +
      scale_x_continuous(labels = \(x) x / 1e6) +
      scale_y_continuous(labels = \(x) x / 1e6) +
      coord_sf(xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
               ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
               default_crs = st_crs(target_crs), datum = target_crs,
               expand = FALSE) +
      facet_grid(model ~ season)
  }

  .cap_tuned <- paste0(
    "tuned soap: boundary tol500/margin250, 89 knots | s(Ano) evaluated at ",
    ref_ano, "\n",
    "sst is a monthly CLIMATOLOGY averaged over each season's 3 months - ",
    "the sst term is identical in every year, so all interannual change is s(Ano)")

  dd.map.tuned.season.model <- .tuned_map(
    .tuned_tiles, "Common dolphin density - tuned soap film: base vs + s(sst)",
    .cap_tuned)

  dd.map.tuned.season.model.log <- .tuned_map(
    .tuned_tiles,
    "Common dolphin density - tuned soap film: base vs + s(sst) (log scale)",
    paste0(.cap_tuned, " | log10 colour scale"), log_scale = TRUE)

  ggsave("output/CommonDolphin/DSM/DD_DSM_Tuned_SeasonByModel.png",
         dd.map.tuned.season.model, width = 15, height = 9)
  ggsave("output/CommonDolphin/DSM/DD_DSM_Tuned_SeasonByModel_log.png",
         dd.map.tuned.season.model.log, width = 15, height = 9)

} else {
  warning("Map 12 skipped: needs dd.dsm.soap.season.year, ",
          "dd.dsm.soap.year.season.sst and preddata in the workspace ",
          "(run 4_CommonDolphin_DSM_soap.R first).", call. = FALSE)
}
