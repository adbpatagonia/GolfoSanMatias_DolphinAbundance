# ADB
# 2026-04-20

# Estimate abundance of dusky and common dolphins
# in the San Matias Gulf, Argentina
# using density surface models as presented by
# Miller et al 2013
# Spatial models for distance sampling data: recent developments and future directions
# 10.1111/2041-210X.12105

# This is the EDA file for common dolphins

distdata_lo$ship <- as.factor(distdata_lo$ship)


# Effect of ship ----
## Distance ----
# there in an effect of ship on the distance at which dolphins are detected
# consider including in detection function
# it may be observer rather than ship but we can't tell as the data does not have observer ID
p.dist.ship.lo <- ggplot(distdata_lo) +
  geom_histogram(aes(x = distance, fill = ship, color = ship), alpha = 0.5, position = "stack")

p.dist.ship.dens.lo <- ggplot(distdata_lo) +
  geom_density(aes(x = distance, fill = ship, color = ship), alpha = 0.5)

## Group size ----
# it does not seem to affect the group sizes detected
p.size.ship.lo <- ggplot(distdata_lo) +
  geom_histogram(aes(x = size, fill = ship, color = ship), alpha = 0.5, position = "stack")

p.size.ship.dens.lo <-ggplot(distdata_lo) +
  geom_density(aes(x = size, fill = ship, color = ship), alpha = 0.5)

# Distance ----
## Group size ----
p.size.dist.ship.lo <- ggplot(distdata_lo) +
  geom_point(aes(x = distance, y = size, fill = ship, color = ship), alpha = 0.5) +
  geom_smooth(aes(x = distance, y = size, group = ship, color = ship, fill = ship),
              method = "lm")

## Beaufort ----
p.beauf.dist.ship.lo <- ggplot(distdata_lo) +
  geom_point(aes(x = distance, y = beaufort, fill = ship, color = ship), alpha = 0.5) +
  geom_smooth(aes(x = distance, y = beaufort, group = ship, color = ship, fill = ship),
              method = "lm")

p.size.ship.lo <- ggplot(distdata_lo) +
  geom_histogram(aes(x = beaufort, fill = ship, color = ship), alpha = 0.5)
p.size.ship.dens.lo <- ggplot(distdata_lo) +
  geom_density(aes(x = beaufort, fill = ship, color = ship), alpha = 0.5)

# Spatial covariates -----
## Depth ----
p.depth.lo <- p.depth +
  geom_sf(data = segdata_traj_m %>%
            filter(Ano > 2006),
          aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_lo_sf_m %>%
            filter(Ano > 2006),
          aes(size = size,
              color = season),
          alpha = 0.9
  ) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  facet_wrap(.~Ano)

## Slope ----
p.slope.lo <- p.slope +
  geom_sf(data = segdata_traj_m %>%
            filter(Ano > 2006),
          aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_lo_sf_m %>%
            filter(Ano > 2006),
          aes(size = size,
              color = season),
          alpha = 0.9
  ) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  facet_wrap(.~Ano)

## grad ----
p.grad.lo <- p.grad +
  geom_sf(data = segdata_traj_m %>%
            filter(Ano > 2006),
          aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_lo_sf_m %>%
            filter(Ano > 2006),
          aes(size = size,
              color = season),
          alpha = 0.9
  ) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  facet_wrap(.~Ano)


## sst ----
p.sst.lo <-  p.sst +
  geom_sf(data = segdata_traj_m %>%
            filter(Ano > 2006),
          # aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_lo_sf_m %>%
            filter(Ano > 2006),
          aes(size = size),
          alpha = 0.9
  ) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  facet_grid(Ano ~ season)

## clorophyll ----
p.clo.lo <-  p.clo +
  geom_sf(data = segdata_traj_m %>%
            filter(Ano > 2006),
          # aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_lo_sf_m %>%
            filter(Ano > 2006),
          aes(size = size),
          alpha = 0.9
  ) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  facet_grid(Ano ~ season)

## dist.up ----
p.distup.lo <-  p.distup +
  geom_sf(data = segdata_traj_m %>%
            filter(Ano > 2006),
          # aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_lo_sf_m %>%
            filter(Ano > 2006),
          aes(size = size),
          alpha = 0.9
  ) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs),
    datum = target_crs,
    expand = TRUE
  ) +
  facet_grid(Ano ~ season)

# output ----
ggsave(plot = p.distup.lo,
       filename = paste0(here::here(), '/output/DuskyDolphin/EDA/Survey_Distup.png'),
       width = 10,
       height = 13)

ggsave(plot = p.clo.lo,
       filename = paste0(here::here(), '/output/DuskyDolphin/EDA/Survey_Clorophyll.png'),
       width = 10,
       height = 13)

ggsave(plot = p.sst.lo,
       filename = paste0(here::here(), '/output/DuskyDolphin/EDA/Survey_SST.png'),
       width = 10,
       height = 13)

ggsave(plot = p.depth.lo,
       filename = paste0(here::here(), '/output/DuskyDolphin/EDA/Survey_Depth.png'),
       width = 13,
       height = 10)

ggsave(plot = p.slope.lo,
       filename = paste0(here::here(), '/output/DuskyDolphin/EDA/Survey_Slope.png'),
       width = 13,
       height = 10)

ggsave(plot = p.grad.lo,
       filename = paste0(here::here(), '/output/DuskyDolphin/EDA/Survey_Grad.png'),
       width = 13,
       height = 10)
# -----

titlab <- "EDA - Dusky Dolphin"

png(
  filename = paste0(here::here(), "/output/DuskyDolphin/EDA/EDA_Dusky_Dolphin.png"),
  width = 1600,
  height = 1200,
  res = 150
)

par(
  mfrow = c(2, 2),
  oma = c(0, 0, 3, 0)   # outer margin for global title
)

# Histograms
hist(
  distdata_lo$distance,
  main = "",
  xlab = "Distance (m)",
  breaks = seq(0, max(distdata_lo$distance, na.rm = TRUE), len = 20)
)

hist(
  distdata_lo$size,
  main = "",
  xlab = "Cluster size",
  breaks = seq(0, max(distdata_lo$size, na.rm = TRUE), len = 20)
)

# Distance vs cluster size
plot(
  distdata_lo$distance,
  distdata_lo$size,
  main = "",
  xlab = "Distance (m)",
  ylab = "Group size",
  pch = 19,
  col = rgb(0.74, 0.74, 0.74, 0.7)
)

l.dat <- data.frame(
  distance = seq(0, max(distdata_lo$distance, na.rm = TRUE), len = 1000)
)

lo <- lm(size ~ distance, data = distdata_lo)

lines(
  l.dat$distance,
  predict(lo, newdata = l.dat),
  lwd = 2
)

# Distance vs Beaufort
plot(
  distdata_lo$distance,
  distdata_lo$beaufort,
  main = "",
  xlab = "Distance (m)",
  ylab = "Beaufort sea state",
  pch = 19,
  col = rgb(0.74, 0.74, 0.74, 0.7)
)

# Global title
mtext(titlab, outer = TRUE, cex = 1.6, font = 2)

dev.off()
