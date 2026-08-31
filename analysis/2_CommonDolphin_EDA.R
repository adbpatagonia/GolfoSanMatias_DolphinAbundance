# ADB
# 2026-04-20

# Estimate abundance of dusky and common dolphins
# in the San Matias Gulf, Argentina
# using density surface models as presented by
# Miller et al 2013
# Spatial models for distance sampling data: recent developments and future directions
# 10.1111/2041-210X.12105

# This is the EDA file for common dolphins

distdata_dd$ship <- as.factor(distdata_dd$ship)


# Effect of ship ----
## Distance ----
# there seems to be some effect of ship on the distance at which dolphins are detected
# consider including in detection function
p.dist.ship.dd <- ggplot(distdata_dd) +
  geom_histogram(aes(x = distance, fill = ship, color = ship), alpha = 0.5, position = "stack")

p.dist.ship.dens.dd <- ggplot(distdata_dd) +
  geom_density(aes(x = distance, fill = ship, color = ship), alpha = 0.5)

## Group size ----
# it does not seem to affect the group sizes detected
p.size.ship.dd <- ggplot(distdata_dd) +
  geom_histogram(aes(x = size, fill = ship, color = ship), alpha = 0.5, position = "stack")

p.size.ship.dens.dd <-ggplot(distdata_dd) +
  geom_density(aes(x = size, fill = ship, color = ship), alpha = 0.5)

# Distance ----
## Group size ----
p.size.dist.ship.dd <- ggplot(distdata_dd) +
  geom_point(aes(x = distance, y = size, fill = ship, color = ship), alpha = 0.5) +
  geom_smooth(aes(x = distance, y = size, group = ship, color = ship, fill = ship),
              method = "lm")

## Beaufort ----
 p.beauf.dist.ship.dd <-
  ggplot(distdata_dd) +
  geom_point(aes(y = distance, x = beaufort, fill = ship, color = ship),
             alpha = 0.5,
             position = position_jitterdodge(jitter.width = 0.15,
                                              dodge.width = 0.3)) +
  geom_smooth(aes(y = distance, x = beaufort, group = ship, color = ship, fill = ship),
              method = "lm",
              position = position_dodge(width = 0.3))

p.size.ship.dd <- ggplot(distdata_dd) +
  geom_histogram(aes(x = beaufort, fill = ship, color = ship), alpha = 0.5)

# Spatial covariates -----
## Depth ----
p.depth.dd <- p.depth +
  geom_sf(data = segdata_sf_m ,
          aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m ,
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
p.slope.dd <- p.slope +
  geom_sf(data = segdata_sf_m ,
          # aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m %>%
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
p.grad.dd <- p.grad +
  geom_sf(data = segdata_traj_m_month ,
          # aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m ,
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
  facet_grid(Mes_n ~ Ano)


## sst ----
p.sst.dd <- p.sst +
  geom_sf(data = segdata_traj_m_month ,
          # aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m ,
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
  facet_grid(Mes_n ~ Ano)

## clorophyll ----
p.clo.dd <- p.clo +
  geom_sf(data = segdata_traj_m_month ,
          # aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m ,
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
  facet_grid(Mes_n ~ Ano)

## dist.up ----
p.distup.dd <- p.distup +
  geom_sf(data = segdata_traj_m_month ,
          # aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m ,
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
  facet_grid(Mes_n ~ Ano)

## VelVert ----
p.velvert.dd <- p.velvert +
  geom_sf(data = segdata_traj_m_month ,
          # aes(color = season),
          size = 0.6,
          alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m ,
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
  facet_grid(Mes_n ~ Ano)


# output ----
ggsave(plot = p.velvert.dd,
       filename = paste0(here::here(), '/output/CommonDolphin/EDA/Survey_VelVert.png'),
       width = 10,
       height = 13)

ggsave(plot = p.distup.dd,
       filename = paste0(here::here(), '/output/CommonDolphin/EDA/Survey_Distup.png'),
       width = 10,
       height = 13)

ggsave(plot = p.clo.dd,
       filename = paste0(here::here(), '/output/CommonDolphin/EDA/Survey_Clorophyll.png'),
       width = 10,
       height = 13)

ggsave(plot = p.sst.dd,
       filename = paste0(here::here(), '/output/CommonDolphin/EDA/Survey_SST.png'),
       width = 10,
       height = 13)

ggsave(plot = p.depth.dd,
       filename = paste0(here::here(), '/output/CommonDolphin/EDA/Survey_Depth.png'),
       width = 13,
       height = 10)

ggsave(plot = p.slope.dd,
       filename = paste0(here::here(), '/output/CommonDolphin/EDA/Survey_Slope.png'),
       width = 13,
       height = 10)

ggsave(plot = p.grad.dd,
       filename = paste0(here::here(), '/output/CommonDolphin/EDA/Survey_Grad.png'),
       width = 13,
       height = 10)
# -----

titlab <- "EDA - Common Dolphin"

png(
  filename = paste0(here::here(), "/output/CommonDolphin/EDA/EDA_Common_Dolphin.png"),
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
  distdata_dd$distance,
  main = "",
  xlab = "Distance (m)",
  breaks = seq(0, max(distdata_dd$distance, na.rm = TRUE), len = 20)
)

hist(
  distdata_dd$size,
  main = "",
  xlab = "Cluster size",
  breaks = seq(0, max(distdata_dd$size, na.rm = TRUE), len = 20)
)

# Distance vs cluster size
plot(
  distdata_dd$distance,
  distdata_dd$size,
  main = "",
  xlab = "Distance (m)",
  ylab = "Group size",
  pch = 19,
  col = rgb(0.74, 0.74, 0.74, 0.7)
)

l.dat <- data.frame(
  distance = seq(0, max(distdata_dd$distance, na.rm = TRUE), len = 1000)
)

lo <- lm(size ~ distance, data = distdata_dd)

lines(
  l.dat$distance,
  predict(lo, newdata = l.dat),
  lwd = 2
)

# Distance vs Beaufort
plot(
  distdata_dd$distance,
  distdata_dd$beaufort,
  main = "",
  xlab = "Distance (m)",
  ylab = "Beaufort sea state",
  pch = 19,
  col = rgb(0.74, 0.74, 0.74, 0.7)
)

# Global title
mtext(titlab, outer = TRUE, cex = 1.6, font = 2)

dev.off()
