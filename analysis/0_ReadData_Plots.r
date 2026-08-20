# ADB
# 2026-04-20

# Estimate abundance of dusky and common dolphins
# in the San Matias Gulf, Argentina
# using density surface models as presented by
# Miller et al 2013
# Spatial models for distance sampling data: recent developments and future directions
# 10.1111/2041-210X.12105


# variable Effort es el largo del segmento

# libraries -----
library(dsm)
library(Distance)
library(mrds)
library(scales)
library(terra)
library(sf)
library(data.table)
library(tidyverse)
library(tweedie)
library(patchwork)
library(here)
library(kableExtra)
library(gam.hp)

# plotting options
ggplot2::theme_set(theme_bw())
gg.opts  <-  theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank())

# parameters -----
# Apply CRS
crs_m <- "EPSG:22193"
crs_latlon <- "EPSG:4326"

# read data -----
## Distance data ----
### Common dolphin ----
distdata_dd <- fread(paste0(here::here(), "/data/DistanceData/distdata_ddwholesample.csv"))

#' @ADB: ask Silvana about this
#' Silvana confirmed this file should not be used  for analysis
#' NOT USED
distdata_ddoption2 <- fread(paste0(here::here(), "/data/DistanceData/distdata_dd_todos2.csv"))


obsdata_dd <- fread(paste0(here::here(), "/data/DistanceData/obsdata_dd.csv"))

### Dusky dolphin ----
distdata_lo <- fread(paste0(here::here(), "/data/DistanceData/distdata_lowholesample.csv"))
obsdata_lo <- fread(paste0(here::here(), "/data/DistanceData/obsdata_lo.csv"))

### segment and prediction grid ----
segdata <-  fread(paste0(here::here(), "/data/DistanceData/segdata.csv"))
preddata <-  fread(paste0(here::here(), "/data/DistanceData/preddata.csv"))
preddata <-  fread(paste0(here::here(), "/data/DistanceData/preddataVV.csv"))

setnames(preddata, old = c("dist_coast", "dist_up"), new = c("dist.coast", "dist.up"))


## map data ----
patagonia  <-  st_read(paste0(here::here(), "/data/shp/Patagonia_Completa.shp"), quiet = TRUE)
patagonia <- st_make_valid(patagonia)

## survey area -----
survey.area  <-  st_read(paste0(here::here(), "/data/shp/survey.area.shp"), quiet = TRUE)
pred.polys <- st_read(paste0(here::here(), "/data/shp/gridproy41.1.shp"))


# wrangle data ----
segdata <- segdata %>%
  mutate(season = case_when(
    between(Mes_n, 1, 3) ~ "Summer",
    between(Mes_n, 4, 6) ~ "Fall",
    between(Mes_n, 6, 9) ~ "Winter",
    between(Mes_n, 9, 12) ~ "Spring"
  ))

preddata <- preddata %>%
  mutate(season = case_when(
    between(Mes_n, 1, 3) ~ "Summer",
    between(Mes_n, 4, 6) ~ "Fall",
    between(Mes_n, 6, 9) ~ "Winter",
    between(Mes_n, 9, 12) ~ "Spring"
  ))

distdata_dd <- distdata_dd %>%
  left_join(unique(segdata[,.(Mes, Mes_n)])) %>%
  mutate(date = lubridate::ymd(paste(Ano, Mes_n, Dia, sep = "/"))) %>%
  mutate(season = case_when(
    Mes %in% c("Enero", "Febrero", "Marzo") ~ "Summer",
    Mes %in% c("Abril", "Mayo", "Junio") ~ "Fall",
    Mes %in% c("Julio", "Agosto", "Septiembre") ~ "Winter",
    Mes %in% c("Octubre", "Noviembre", "Diciembre") ~ "Spring"))

distdata_lo <- distdata_lo %>%
  left_join(unique(segdata[,.(Mes, Mes_n)])) %>%
  mutate(date = lubridate::ymd(paste(Ano, Mes_n, Dia, sep = "/"))) %>%
  mutate(season = case_when(
    Mes %in% c("Enero", "Febrero", "Marzo") ~ "Summer",
    Mes %in% c("Abril", "Mayo", "Junio") ~ "Fall",
    Mes %in% c("Julio", "Agosto", "Septiembre") ~ "Winter",
    Mes %in% c("Octubre", "Noviembre", "Diciembre") ~ "Spring"))

obsdata_dd[, Mes := as.integer(substr(Sample.Label, 5, 6))]
obsdata_dd <-obsdata_dd %>%
  mutate(season = case_when(
    Mes < 4 ~ "Summer",
    Mes %in% c(4, 5, 6) ~ "Fall",
    Mes %in% c(7, 8, 9) ~ "Winter",
    Mes > 9 ~ "Spring"))

obsdata_lo[, Mes := as.integer(substr(Sample.Label, 5, 6))]
obsdata_lo <-obsdata_lo %>%
  mutate(season = case_when(
    Mes < 4 ~ "Summer",
    Mes %in% c(4, 5, 6) ~ "Fall",
    Mes %in% c(7, 8, 9) ~ "Winter",
    Mes > 9 ~ "Spring"))


segdata[
  ,
  season := factor(
    season,
    levels = c("Spring", "Summer", "Fall", "Winter")
  )
]
preddata[
  ,
  season := factor(
    season,
    levels = c("Spring", "Summer", "Fall", "Winter")
  )
]
distdata_dd[
  ,
  season := factor(
    season,
    levels = c("Spring", "Summer", "Fall", "Winter")
  )
]
distdata_lo[
  ,
  season := factor(
    season,
    levels = c("Spring", "Summer", "Fall", "Winter")
  )
]
obsdata_dd[
  ,
  season := factor(
    season,
    levels = c("Spring", "Summer", "Fall", "Winter")
  )
]
obsdata_lo[
  ,
  season := factor(
    season,
    levels = c("Spring", "Summer", "Fall", "Winter")
  )
]


segdata_sf_m  <-  st_as_sf(segdata,
                           coords = c("x","y"),
                           crs = crs_m)

target_crs <- st_crs(segdata_sf_m)


distdata_dd_sf_m <-  st_as_sf(distdata_dd,
                              coords = c("x","y"),
                              crs = target_crs)
distdata_lo_sf_m <-  st_as_sf(distdata_lo,
                              coords = c("x","y"),
                              crs = target_crs)
obsdata_dd_sf_m <-  st_as_sf(obsdata_dd,
                             coords = c("x","y"),
                             crs = target_crs)

obsdata_lo_sf_m <-  st_as_sf(obsdata_lo,
                             coords = c("x","y"),
                             crs = target_crs)

preddata_sf_m <-  st_as_sf(preddata,
                           coords = c("x","y"),
                           crs = target_crs)

patagonia_m <- st_transform(patagonia, target_crs)

pred.polys_m <- st_transform(pred.polys, target_crs)

survey.area_m <- st_transform(survey.area, target_crs)



## create trajectories ----
setDT(segdata)
segdata[, traj_id := sub("_.*", "", Transect.Label)]
setorder(segdata, traj_id)
segdata_traj_m <- segdata[
  ,
  {
    coords <- as.matrix(.SD[, .(x, y)])

    if (nrow(coords) < 2) {
      geom <- st_sfc(st_linestring(), crs = st_crs(target_crs))
    } else {
      geom <- st_sfc(st_linestring(coords), crs = st_crs(target_crs))
    }

    list(geometry = geom)
  },
  by = .(traj_id,  Ano, season)
] %>%
  st_as_sf()

n.surveys <- segdata %>%
  distinct(Ano, season, traj_id) %>%
  group_by(Ano, season) %>%
  tally() %>%
  pivot_wider(names_from = season, values_from = n)

setnafill(n.surveys, "const", fill = 0)

# plots -----
## area map ----
# Original lon/lat bbox (WGS84)
bbox_ll <- st_bbox(
  c(xmin = -65.5,
    xmax = -63.4,
    ymin = -42.5,
    ymax = -40.7),
  crs = 4326
)

# Convert bbox to polygon (required for transformation)
bbox_poly_ll <- st_as_sfc(bbox_ll)

# Transform to UTM 20S
bbox_poly_utm <- st_transform(bbox_poly_ll, crs_m)

# Extract transformed bbox
bbox_utm <- st_bbox(bbox_poly_utm)

bbox_utm

map.area.m <- ggplot() +
  geom_sf(data = patagonia_m) +
  geom_sf(data = pred.polys_m, color = "lightblue", fill = NA) +
  geom_sf(data = survey.area_m, color = "red", fill = NA) +
  geom_sf(data = segdata_sf_m,
          aes(color = factor(Ano)),
          size = 0.6,
          alpha = 0.8) +
  labs(
    x = "Easting (Mm)",
    y = "Northing (Mm)"
  ) +
  theme_minimal() +
  coord_sf(
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(
    color = guide_legend(nrow = 1,
                         override.aes = list(size = 2, alpha = 1)
    )
  ) +
  scale_x_continuous(labels = \(x) x / 1000000) +
  scale_y_continuous(labels = \(x) x / 1000000)

bb <- st_bbox(survey.area_m)

xpad <- 3000
ypad <- 3000

map.area.m.zoom <- map.area.m +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  )



## species maps ----
### Common dolphin ----
p.dd <- ggplot() +
  geom_sf(data = patagonia_m) +
  geom_sf(data = segdata_traj_m %>%
            filter(Ano > 2006),
          aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  # geom_sf(data = pred.polys_m, color = "lightblue", fill = "transparent", alpha = 0.1) +
  geom_sf(data = survey.area_m, color = "red", fill = "transparent") +
  geom_sf(data = distdata_dd_sf_m %>%
            filter(Ano > 2006),
          aes(size = size,
              color = season),
          alpha = 0.5
  ) +
  labs(title = "Common Dolphin",
       x = "Easting (Mm)", y = "Northing (Mm)") +
  theme_minimal() +
  facet_wrap(.~Ano) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 6)) +
  guides(
    color = guide_legend(nrow = 1,
                         override.aes = list(size = 2, alpha = 1)
    )
  ) +
  scale_x_continuous(labels = \(x) x / 1000000) +
  scale_y_continuous(labels = \(x) x / 1000000)


### Dusky dolphin -----
p.lo <- ggplot() +
  geom_sf(data = patagonia_m) +
  geom_sf(data = segdata_traj_m %>%
            filter(Ano > 2006),
          aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  # geom_sf(data = pred.polys_m, color = "lightblue", fill = "transparent", alpha = 0.1) +
  geom_sf(data = survey.area_m, color = "red", fill = "transparent") +
  geom_sf(data = distdata_lo_sf_m %>%
            filter(Ano > 2006),
          aes(size = size,
              color = season),
          alpha = 0.5
  ) +
  labs(title = "Dusky Dolphin",
       x = "Easting (Mm)", y = "Northing (Mm)") +
  theme_minimal() +
  facet_wrap(.~Ano) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 6)) +
  guides(
    color = guide_legend(nrow = 1,
                         override.aes = list(size = 2, alpha = 1)
    )
  ) +
  scale_x_continuous(labels = \(x) x / 1000000) +
  scale_y_continuous(labels = \(x) x / 1000000)

### combined ----
p.lo.c <- p.lo + ylab("")
p.sp <- (p.dd + p.lo.c) +
  plot_layout(
    ncol = 2,
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )

## variables maps ----

# need to produce prediction grid
prediction_grid <- st_make_grid(survey.area, cellsize = c(1500,1500))
prediction_grid_sf_m <- st_sf(geometry = prediction_grid)
prediction_grid_sf_m <- st_transform(prediction_grid_sf_m, target_crs)



# create grid
seasons <- unique(preddata_sf_m$season)

# base grid
grid0 <- st_make_grid(survey.area_m, cellsize = c(1500,1500))
grid0 <- st_make_grid(survey.area_m, cellsize = c(800,800))
grid0 <- st_sf(geometry = grid0)

# build a grid, and join with preddata (by season), then bind them
out <- lapply(seasons, function(ss){

  # copy the grid
  g <- copy(grid0)

  # subset preddata for the season
  dat_ss <- preddata_sf_m[preddata_sf_m$season == ss, ]

  # join the grid with the seasonal preddata
  g2 <- st_join(
    g,
    dat_ss,
    join = st_nearest_feature
  )

  g2
})

# bind the seasonal grids
cropped_grid <- do.call(rbind, out)

# crop the seasonal grids to the survey area
cropped_grid <- st_intersection(cropped_grid, survey.area_m)



# sst, clo, dist.up vary with time
# slope, depth, grad are time invariant

vars_with_season <- c("dist.up", "sst", "clo")

## depth ----
var <- "depth"

cols <- c({{var}}, "season", "geometry")
dat <- cropped_grid[, c(cols), with = FALSE]
setDT(dat)
setnames(dat, {{var}}, "value");  dat <- st_as_sf(dat)

# the cropped grid is repeated by season
# data is identical for the 4 seasons
# subset only one season
if(!var %in% vars_with_season){
  dat <- dat %>%
    filter(season == "Summer") %>%
    select(-season)
}

p.depth <- ggplot() +
  geom_sf(data = patagonia_m) +
  geom_sf(data = dat,
          aes(color = -value, fill = -value), col = NA, alpha = 0.5) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  scale_fill_viridis_c() +
  # scale_color_viridis_c() +
  scale_x_continuous(labels = \(x) x / 1000000) +
  scale_y_continuous(labels = \(x) x / 1000000) +
  labs(title = {{var}},
       x = "Easting (Mm)", y = "Northing (Mm)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

## slope -----
var <- "slope"

cols <- c({{var}}, "season", "geometry")
dat <- cropped_grid[, c(cols), with = FALSE]
setDT(dat)
setnames(dat, {{var}}, "value");  dat <- st_as_sf(dat)

# the cropped grid is repeated by season
# data is identical for the 4 seasons
# subset only one season
if(!var %in% vars_with_season){
  dat <- dat %>%
    filter(season == "Summer") %>%
    select(-season)
}

p.slope <- ggplot() +
  geom_sf(data = patagonia_m) +
  geom_sf(data = dat,
          aes(color = value, fill = value), col = NA, alpha = 0.5) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  scale_fill_viridis_c() +
  # scale_color_viridis_c() +
  scale_x_continuous(labels = \(x) x / 1000000) +
  scale_y_continuous(labels = \(x) x / 1000000) +
  labs(title = {{var}},
       x = "Easting (Mm)", y = "Northing (Mm)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))


## grad -----
var <- "grad"

cols <- c({{var}}, "season", "geometry")
dat <- cropped_grid[, c(cols), with = FALSE]
setDT(dat)
setnames(dat, {{var}}, "value");  dat <- st_as_sf(dat)

# the cropped grid is repeated by season
# data is identical for the 4 seasons
# subset only one season
if(!var %in% vars_with_season){
  dat <- dat %>%
    filter(season == "Summer") %>%
    select(-season)
}

p.grad <- ggplot() +
  geom_sf(data = patagonia_m) +
  geom_sf(data = dat,
          aes(color = value, fill = value), col = NA, alpha = 0.5) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  scale_fill_viridis_c() +
  # scale_color_viridis_c() +
  scale_x_continuous(labels = \(x) x / 1000000) +
  scale_y_continuous(labels = \(x) x / 1000000) +
  labs(title = {{var}},
       x = "Easting (Mm)", y = "Northing (Mm)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))



## sst ----
var <- "sst"
cols <- c({{var}}, "season", "geometry")
dat <- cropped_grid[, c(cols), with = FALSE]
setDT(dat)
setnames(dat, {{var}}, "value")
dat <- st_as_sf(dat)

# the cropped grid is repeated by season
# data is identical for the 4 seasons
# subset only one season
if(!var %in% vars_with_season){
  dat <- dat %>%
    filter(season == "Summer") %>%
    select(-season)
}

p.sst <- ggplot() +
  geom_sf(data = patagonia_m) +
  geom_sf(data = dat,
          aes(color = value, fill = value), col = NA, alpha = 0.5) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  scale_fill_viridis_c() +
  # scale_color_viridis_c() +
  scale_x_continuous(labels = \(x) x / 1000000) +
  scale_y_continuous(labels = \(x) x / 1000000) +
  labs(title = {{var}},
       x = "Easting (Mm)", y = "Northing (Mm)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))  +
  facet_wrap(. ~ season)

## Clorophyll ----
var <- "clo"

cols <- c({{var}}, "season", "geometry")
dat <- cropped_grid[, c(cols), with = FALSE]
setDT(dat)
setnames(dat, {{var}}, "value")
dat <- st_as_sf(dat)

p.clo <- ggplot() +
  geom_sf(data = patagonia_m) +
  geom_sf(data = dat,
          aes(color = value, fill = value), col = NA, alpha = 0.5) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  scale_fill_viridis_c() +
  # scale_color_viridis_c() +
  scale_x_continuous(labels = \(x) x / 1000000) +
  scale_y_continuous(labels = \(x) x / 1000000) +
  labs(title = {{var}},
       x = "Easting (Mm)", y = "Northing (Mm)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))  +
  facet_wrap(. ~ season)

## dist.up ----
var <- "dist.up"

cols <- c({{var}}, "season", "geometry")
dat <- cropped_grid[, c(cols), with = FALSE]
setDT(dat)
setnames(dat, {{var}}, "value")
dat <- st_as_sf(dat)

p.distup <- ggplot() +
  geom_sf(data = patagonia_m) +
  geom_sf(data = dat,
          aes(color = value, fill = value), col = NA, alpha = 0.5) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  scale_fill_viridis_c() +
  # scale_color_viridis_c() +
  scale_x_continuous(labels = \(x) x / 1000000) +
  scale_y_continuous(labels = \(x) x / 1000000) +
  labs(title = {{var}},
       x = "Easting (Mm)", y = "Northing (Mm)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))  +
  facet_wrap(. ~ season)

## VelVert ----
### monthly grid ----
# VelVert was provided at a monthly scale
# create grid
months <- unique(preddata_sf_m$Mes_n)

# base grid
grid0 <- st_make_grid(survey.area_m, cellsize = c(800,  800))
grid0 <- st_sf(geometry = grid0)

# build a grid, and join with preddata (by season), then bind them
out <- lapply(months, function(mh){

  # copy the grid
  g <- copy(grid0)

  # subset preddata for the season
  dat_ss <- preddata_sf_m[preddata_sf_m$Mes_n == mh, ]

  # join the grid with the seasonal preddata
  g2 <- st_join(
    g,
    dat_ss,
    join = st_nearest_feature
  )

  g2
})

# bind the monthly grids
cropped_grid <- do.call(rbind, out)

# crop the monthly grids to the survey area
cropped_grid <- st_intersection(cropped_grid, survey.area_m)

### plot ----

var <- "VelVert"

cols <- c({{var}}, "Mes_n", "geometry")
dat <- cropped_grid[, c(cols), with = FALSE]
setDT(dat)
setnames(dat, {{var}}, "value")
dat <- st_as_sf(dat)

p.velvert <- ggplot() +
  geom_sf(data = patagonia_m) +
  geom_sf(data = dat,
          aes(color = value, fill = value), col = NA, alpha = 0.5) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  scale_fill_viridis_c() +
  # scale_color_viridis_c() +
  scale_x_continuous(labels = \(x) x / 1000000) +
  scale_y_continuous(labels = \(x) x / 1000000) +
  labs(title = {{var}},
       x = "Easting (Mm)", y = "Northing (Mm)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))  +
  facet_wrap(. ~ Mes_n)

# output -----
ggsave(plot = p.sp,
       filename = paste0(here::here(), '/output/SpeciesPlots.png'),
       width = 13,
       height = 5.5)

ggsave(plot = p.depth,
       filename = paste0(here::here(), '/output/EnvVars/Depth.png'),
       width = 8,
       height = 8)
ggsave(plot = p.slope,
       filename = paste0(here::here(), '/output/EnvVars/Slope.png'),
       width = 8,
       height = 8)
ggsave(plot = p.grad,
       filename = paste0(here::here(), '/output/EnvVars/grad.png'),
       width = 8,
       height = 8)
ggsave(plot = p.sst,
       filename = paste0(here::here(), '/output/EnvVars/SST.png'),
       width = 13,
       height = 13)
ggsave(plot= p.clo,
       filename = paste0(here::here(), '/output/EnvVars/Clorophyll.png'),
       width = 13,
       height = 13)
ggsave(plot = p.distup,
       filename = paste0(here::here(), '/output/EnvVars/dist.up.png'),
       width = 13,
       height = 13)

ggsave(plot = p.velvert,
       filename = paste0(here::here(), '/output/EnvVars/VelVert.png'),
       width = 16,
       height = 16)
