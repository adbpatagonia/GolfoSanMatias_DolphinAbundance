# ADB / Claude
# 2026-07-07
#
# Edge-effect handling for common-dolphin density surface models.
# The fs per-year surfaces (dd.dsm.xy.fsyear.season) show year-specific boundary
# artefacts where a year's transects don't reach the gulf edge (extrapolation).
# Two remedies, per Miller et al. (2013):
#
#   PART 1  exclude.too.far() masking of the fs maps — blank grid cells farther
#           than `dist` from THAT year's transects (a plotting/prediction fix;
#           does not change the fitted model).
#
#   PART 2  Soap-film shared-surface model — s(x, y, bs = "so") + s(Ano) + season,
#           with the boundary taken from survey.area_m and interior knots from
#           dsm::make.soapgrid(). The soap film respects the gulf boundary and
#           does not smooth across it, controlling the boundary behaviour of the
#           (single, pooled) spatial surface.
#
# Assumes in the workspace:
#   dd.dsm.xy.fsyear.season, pred.polys_m, survey.area_m, patagonia_m,
#   segdata, obsdata_dd_mod, df.dd, trunc.dist_dd, distdata_dd_sf_m,
#   segdata_traj_m, target_crs
#
# NOTE ON PART 2: soap films are notoriously fiddly to set up. Every data point
# and every knot must lie STRICTLY inside the boundary, and the boundary must be
# a clean, non-self-intersecting loop. The parameters flagged  ## TUNE  below
# (simplification tolerance, knot grid, boundary buffer, dist) will almost
# certainly need adjusting for your geometry. This section is a working scaffold,
# not a guaranteed one-shot fit.

library(dsm)
library(mgcv)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

# common helpers ----
off.set   <- 800 * trunc.dist_dd
bb        <- st_bbox(survey.area_m)
xpad <- 3000; ypad <- 3000

if (!all(c("x", "y") %in% names(pred.polys_m))) {
  pred.polys_m <- pred.polys_m %>%
    mutate(x = st_coordinates(st_centroid(geometry))[, 1],
           y = st_coordinates(st_centroid(geometry))[, 2])
}
yf_levels <- levels(dd.dsm.xy.fsyear.season$model$year_fac)
years     <- sort(as.numeric(yf_levels))
segd      <- st_drop_geometry(segdata)   # needs columns x, y, Ano

# ============================================================
# PART 1 — exclude.too.far() masking of the fs per-year maps
# ============================================================

too_far_dist <- 0.1        ## TUNE  distance in unit-square scaling (0.05–0.15)

# per-year prediction grid (fs model uses year_fac; Ano carried for faceting)
pred.polys_year_fs_m <- bind_rows(
  lapply(years, function(a) pred.polys_m %>%
           mutate(Ano      = a,
                  year_fac = factor(as.character(a), levels = yf_levels)))
) %>%
  mutate(season = factor("Spring", levels = levels(obsdata_dd_mod$season)))

pred.polys_year_fs_m$Nhat <- predict(
  dd.dsm.xy.fsyear.season,
  newdata = pred.polys_year_fs_m,
  off.set = off.set,
  type    = "response"
)
pred.polys_year_fs_m$area_m2 <- as.numeric(st_area(pred.polys_year_fs_m))
pred.polys_year_fs_m$density <- pred.polys_year_fs_m$Nhat /
  (pred.polys_year_fs_m$area_m2 / 1e6)

# blank cells too far from THAT year's transects (index-based, order-safe)
pred.polys_year_fs_m$too_far <- FALSE
for (a in years) {
  idx   <- which(pred.polys_year_fs_m$Ano == a)
  seg_a <- segd[segd$Ano == a, ]
  cc    <- st_drop_geometry(pred.polys_year_fs_m)[idx, ]
  pred.polys_year_fs_m$too_far[idx] <-
    exclude.too.far(cc$x, cc$y, seg_a$x, seg_a$y, dist = too_far_dist)
}
pred.polys_year_fs_m$density_masked <- pred.polys_year_fs_m$density
pred.polys_year_fs_m$density_masked[pred.polys_year_fs_m$too_far] <- NA

dd.map.density.year.fs.masked <- ggplot() +
  geom_sf(data = patagonia_m, fill = "grey85", color = "grey40") +
  geom_sf(data = pred.polys_year_fs_m, aes(fill = density_masked), color = NA) +
  geom_sf(data = survey.area_m, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = segdata_traj_m %>% filter(Ano > 2006), size = 0.6, alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m %>% filter(Ano > 2006), aes(size = size), alpha = 0.7) +
  scale_fill_viridis_c(option = "turbo", name = expression("Dolphins km"^-2),
                       na.value = "grey95") +
  labs(title   = "Common dolphin (Spring) — fs per-year surface, edges masked",
       caption = paste0("cells > ", too_far_dist,
                        " (unit-square) from that year's transects are blanked"),
       x = "Easting (Mm)", y = "Northing (Mm)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right", panel.grid.minor = element_blank()) +
  scale_x_continuous(labels = \(x) x / 1e6) +
  scale_y_continuous(labels = \(x) x / 1e6) +
  coord_sf(xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
           ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
           default_crs = st_crs(target_crs), datum = target_crs, expand = FALSE) +
  facet_wrap(. ~ Ano)

dd.map.density.year.fs.masked

ggsave("output/CommonDolphin/DSM/DD_DSM_Year_fsyear_masked.png",
       plot = dd.map.density.year.fs.masked, width = 13, height = 13)

# ============================================================
# PART 2 — Soap-film shared-surface model
#          count ~ s(x, y, bs = "so") + s(Ano) + season
# ============================================================

## --- 2a. boundary from the survey polygon, buffered OUT so ALL segments -------
##          sit strictly inside (soap errors on ANY data point outside the loop;
##          transects here run right up to / just past the survey edge)
simplify_tol <- 3000        ## TUNE  metres; larger = simpler (safer) boundary
margin       <- 2000        ## TUNE  metres of clearance to leave inside the edge

gulf0 <- survey.area_m %>%
  st_geometry() %>% st_union() %>%
  st_cast("MULTIPOLYGON") %>% st_cast("POLYGON")
if (length(gulf0) > 1)                             # keep the largest polygon only
  gulf0 <- gulf0[which.max(as.numeric(st_area(gulf0)))]

# distance of each segment to the raw polygon: 0 if inside/on, > 0 if outside.
# Buffer the boundary outward by (max outside distance + margin) so every
# segment ends up strictly interior — keeps all data, no distortion.
seg_sf <- st_as_sf(segdata, coords = c("x", "y"),
                   crs = st_crs(survey.area_m), remove = FALSE)
d_out  <- as.numeric(st_distance(seg_sf, gulf0))
buffer_out <- if (any(d_out > 0)) max(d_out) + margin else margin
message(sprintf("segments outside raw polygon: %d (max %.0f m) -> buffering out %.0f m",
                sum(d_out > 0), max(d_out), buffer_out))

gulf <- gulf0 %>%
  st_buffer(buffer_out) %>%
  st_simplify(dTolerance = simplify_tol, preserveTopology = TRUE) %>%
  st_cast("POLYGON")
if (length(gulf) > 1)                              # keep the largest ring only
  gulf <- gulf[which.max(as.numeric(st_area(gulf)))]

ring <- st_coordinates(gulf)[, c("X", "Y")]        # exterior ring (closed)
bnd_loop <- list(x = ring[, 1], y = ring[, 2])
if (bnd_loop$x[1] != tail(bnd_loop$x, 1)) {        # ensure closed
  bnd_loop$x <- c(bnd_loop$x, bnd_loop$x[1])
  bnd_loop$y <- c(bnd_loop$y, bnd_loop$y[1])
}
bnd_soap <- list(bnd_loop)                         # xt$bnd is a LIST OF LOOPS
bmat     <- cbind(bnd_loop$x, bnd_loop$y)

# distance-to-boundary helper (min distance to boundary vertices)
bnd_dist <- function(px, py)
  vapply(seq_along(px),
         function(i) min(sqrt((bnd_loop$x - px[i])^2 + (bnd_loop$y - py[i])^2)),
         numeric(1))

## --- 2b. interior knots (dsm::make.soapgrid), strictly inside + buffered ------
knot_ngrid <- c(10, 8)     ## TUNE  knot grid density (start coarse!)
knot_buffer <- 1000         ## TUNE  metres; knots this close to the edge are dropped
kn   <- make.soapgrid(bnd_loop, n.grid = knot_ngrid)
keep <- as.logical(in.out(bmat, cbind(kn$x, kn$y))) & bnd_dist(kn$x, kn$y) > knot_buffer
knots <- data.frame(x = kn$x[keep], y = kn$y[keep])
message(sprintf("soap knots: %d generated, %d kept (inside + %gm buffer)",
                length(kn$x), nrow(knots), knot_buffer))
# FALLBACK if the fit below dies with 'NA/NaN/Inf in soap.basis': make.soapgrid
# placed a knot the PDE grid can't handle. Coarsen knot_ngrid, raise knot_buffer,
# or replace `knots` with a hand-placed regular grid known to sit well inside.

## --- 2c. all segments must be STRICTLY inside the boundary --------------------
seg_in <- as.logical(in.out(bmat, cbind(segdata$x, segdata$y))) &
  bnd_dist(segdata$x, segdata$y) > 1
if (!all(seg_in)) {
  warning(sprintf(paste0("%d of %d segments fall on/outside the boundary — soap ",
                         "will error. Increase simplify_tol, or buffer the boundary ",
                         "outward (st_buffer) so all segments are inside."),
                  sum(!seg_in), length(seg_in)))
}
# soap requires the SAME strict-inside condition on the observation/segment data
# used to fit; if the warning fires, fix the boundary before fitting.

## --- 2d. fit the soap-film shared-surface DSM --------------------------------
# knots are forwarded to gam() via dsm()'s `...`; k sets the boundary-loop basis.
dd.dsm.soap.season.year <- dsm(
  count ~ s(x, y, bs = "so", xt = list(bnd = bnd_soap), k = 10) +
    s(Ano) + season,
  ddf.obj          = df.dd,
  segment.data     = segdata,
  observation.data = obsdata_dd_mod,
  family           = tw(link = "log"),
  method           = "REML",
  knots            = knots
)

# summary(dd.dsm.soap.season.year)
# appraise(dd.dsm.soap.season.year)

## --- 2e. predict (masked to inside the boundary) and map ---------------------
ref_ano <- as.integer(round(median(segdata$Ano)))

pred.polys_soap_m <- bind_rows(
  pred.polys_m %>% mutate(season = "Summer"),
  pred.polys_m %>% mutate(season = "Spring"),
  pred.polys_m %>% mutate(season = "Winter"),
  pred.polys_m %>% mutate(season = "Fall")
) %>%
  mutate(season = factor(season, levels = levels(obsdata_dd_mod$season)),
         Ano    = ref_ano)

# soap predictions are only valid inside the boundary
pred.polys_soap_m$inside <- as.logical(
  in.out(bmat, cbind(pred.polys_soap_m$x, pred.polys_soap_m$y)))

pred.polys_soap_m$Nhat <- NA_real_
pred.polys_soap_m$Nhat[pred.polys_soap_m$inside] <- predict(
  dd.dsm.soap.season.year,
  newdata = pred.polys_soap_m[pred.polys_soap_m$inside, ],
  off.set = off.set,
  type    = "response"
)
pred.polys_soap_m$area_m2 <- as.numeric(st_area(pred.polys_soap_m))
pred.polys_soap_m$density <- pred.polys_soap_m$Nhat / (pred.polys_soap_m$area_m2 / 1e6)

dd.map.density.soap <- ggplot() +
  geom_sf(data = patagonia_m, fill = "grey85", color = "grey40") +
  geom_sf(data = pred.polys_soap_m, aes(fill = density), color = NA) +
  geom_sf(data = survey.area_m, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = segdata_traj_m %>% filter(Ano > 2006), size = 0.6, alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m %>% filter(Ano > 2006), aes(size = size), alpha = 0.7) +
  scale_fill_viridis_c(option = "turbo", name = expression("Dolphins km"^-2),
                       na.value = "grey95") +
  labs(title   = "Common dolphin — soap-film shared surface",
       caption = "model: count ~ s(x, y, bs = \"so\") + s(Ano) + season",
       x = "Easting (Mm)", y = "Northing (Mm)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right", panel.grid.minor = element_blank()) +
  scale_x_continuous(labels = \(x) x / 1e6) +
  scale_y_continuous(labels = \(x) x / 1e6) +
  coord_sf(xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
           ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
           default_crs = st_crs(target_crs), datum = target_crs, expand = FALSE) +
  facet_wrap(. ~ season)

dd.map.density.soap

ggsave("output/CommonDolphin/DSM/DD_DSM_Soap_Season.png",
       plot = dd.map.density.soap, width = 13, height = 13)
