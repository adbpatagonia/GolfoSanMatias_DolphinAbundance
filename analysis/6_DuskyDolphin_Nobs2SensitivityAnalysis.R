# ADB / Claude
# 2026-09-01
#
# n_obs == 2 sensitivity analysis — DUSKY dolphins.
#
# ============================================================================
# RATIONALE
# ============================================================================
#
# Using a detection function with NO covariates, ADB found a clear increasing
# trend in common-dolphin abundance over time. The data providers raised the
# possibility that this is an artefact of the number of observers on effort
# (n_obs) changing over the study period, rather than a real change in
# abundance -- more observers on watch plausibly means higher detection
# probability, which alone could produce an apparent increasing trend even if
# true abundance were flat. This script applies the same check to dusky
# dolphins for consistency, even though the original trend that prompted the
# investigation was seen in common dolphins.
#
# ADB investigated adding n_obs (grouped as nobs_grp: "1" vs "> 1") as a
# COVARIATE in the detection function to correct for this (see
# 3_CommonDolphin_DetectionFunction.R / 3_DuskyDolphin_DetectionFunction.R).
# That approach was abandoned -- see NOTE_nobsgrp_detection_function_issue.R
# for the full writeup. In short: for dusky dolphins the nobsgrp coefficient's
# Hessian was numerically singular (only 9 of 108 detections had
# nobs_grp == "1"), producing an unusable SE (1e5) that inflated every
# downstream abundance CV to nonsense (CV ~ 64000, identical on every row of
# every table). For common dolphins nobsgrp itself was numerically fine, but
# the year x n_obs cross-tab in segdata showed n_obs is nearly a STEP
# FUNCTION of year (n_obs == 1 only occurs 2006-2010; n_obs is almost always
# 2 from 2015 on, and this is shared survey-effort data, so it applies to
# dusky dolphins too) -- meaning nobs_grp/n_obs and any year term are too
# close to collinear for a covariate-based correction to cleanly separate
# "detectability changed" from "abundance changed". Whatever a joint model
# returns mostly reflects which term the fit happens to favour, not evidence
# about the real question.
#
# WHY NOT "add n_obs to the dsm() count-model formula" (the other option
# considered instead of a detection-function covariate): rejected for the
# same collinearity reason, plus n_obs is a DETECTABILITY covariate and so is
# the wrong kind of term to add to a density surface in the first place. See
# NOTE_nobsgrp_detection_function_issue.R section 4 for the full argument.
#
# THIS SCRIPT tests the confound directly instead, by holding n_obs constant
# rather than modelling it: it refits the best dusky-dolphin model
# (lo.dsm.xy.fsyear.season, from 4_DuskyDolphin_DSM.R -- count ~
# s(x,y,year_fac,bs="fs") + season) on the SUBSET of segments/detections
# where n_obs == 2 (the modal, most consistently represented level across
# the full 2006-2018 range -- see the coverage check below), using the SAME
# covariate-free detection function as the rest of the pipeline (df.lo). If
# the year effect survives in this observer-effort-constant subset, that is
# evidence any trend is real rather than an artefact of changing observer
# numbers.
#
# CAVEAT: segdata's 2013 segments are ALL n_obs == 1 (39 segments, zero with
# n_obs == 2) -- the n_obs == 2 subset therefore has NO 2013 data at all, and
# year_fac for 2013 is dropped from the subset fit entirely (an "fs" factor
# smooth cannot be estimated for a level with zero rows). Every other year
# (2006-2010, 2014-2018) has usable n_obs == 2 coverage (40-100% of that
# year's segments; see the printed coverage table below). Unlike a smooth on
# a continuous year variable, the fs subset model simply has no per-year
# effect at all for 2013 -- it is not interpolated, it is absent.
#
# Assumes in the workspace (i.e. run after 4_DuskyDolphin_DSM.R, which
# 1_DuskyDolphin.R does):
#   df.lo, segdata, obsdata_lo_mod, lo.dsm.xy.fsyear.season

library(dsm)
library(mgcv)
library(data.table)
library(ggplot2)

source(file.path(here::here(), "R", "year_partial_effect.R"))

out_dir <- "output/DuskyDolphin/Nobs2Sensitivity"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# n_obs == 2 coverage by year
# ============================================================================
cov_tab <- segdata[, .N, by = .(Ano, n_obs)]
setorder(cov_tab, Ano, n_obs)
cov_wide <- dcast(cov_tab, Ano ~ n_obs, value.var = "N", fill = 0)
print(cov_wide)

nobs2_by_year <- segdata[, .(n_total = .N,
                             n_obs2  = sum(n_obs == 2, na.rm = TRUE)),
                         by = Ano]
nobs2_by_year[, pct_obs2 := round(100 * n_obs2 / n_total, 1)]
setorder(nobs2_by_year, Ano)
print(nobs2_by_year)

if (any(nobs2_by_year$n_obs2 == 0))
  message("n_obs == 2 subset has ZERO segments in: ",
          paste(nobs2_by_year$Ano[nobs2_by_year$n_obs2 == 0], collapse = ", "),
          " -- those year_fac level(s) are absent from the subset fit ",
          "entirely (an fs factor-smooth cannot be estimated for a level ",
          "with zero rows).")

fwrite(nobs2_by_year, file.path(out_dir, "LO_nobs2_coverage_by_year.csv"))

# ============================================================================
# subset to n_obs == 2 and refit the best model
# ============================================================================
segdata_nobs2 <- segdata[n_obs == 2]
segdata_nobs2[, year_fac := droplevels(factor(Ano))]   # drop absent levels (2013)
obsdata_lo_mod_nobs2 <- obsdata_lo_mod[Sample.Label %in% segdata_nobs2$Sample.Label]
obsdata_lo_mod_nobs2[, year_fac := droplevels(factor(Ano))]

stopifnot(nrow(segdata_nobs2) > 0)

message(sprintf(
  "n_obs == 2 subset: %d/%d segments (%.1f%%), %d/%d detections, %d/%d years",
  nrow(segdata_nobs2), nrow(segdata),
  100 * nrow(segdata_nobs2) / nrow(segdata),
  nrow(obsdata_lo_mod_nobs2), nrow(obsdata_lo_mod),
  nlevels(segdata_nobs2$year_fac), nlevels(segdata$year_fac)))

lo.dsm.xy.fsyear.season.nobs2 <- dsm(
  count ~ s(x, y, year_fac, bs = "fs") + season,
  ddf.obj          = df.lo,
  segment.data     = segdata_nobs2,
  observation.data = obsdata_lo_mod_nobs2,
  family           = tw(link = "log"),
  method           = "REML"
)

print(summary(lo.dsm.xy.fsyear.season.nobs2))

# ============================================================================
# compare the per-year partial effect: full data vs n_obs == 2 subset
# ============================================================================
# year enters through the factor-smooth s(x,y,year_fac,bs="fs"), bundled with
# the spatial term, so (unlike dd's plain s(Ano)) it cannot be read off at a
# single point -- year_partial_effect() area-averages the smooth's
# contribution over a grid of locations (see R/year_partial_effect.R for the
# method). The same grid, covering the full survey extent, is used for both
# models so the comparison is apples-to-apples.
grid_xy <- expand.grid(
  x = seq(min(segdata$x), max(segdata$x), length.out = 40),
  y = seq(min(segdata$y), max(segdata$y), length.out = 40)
)

yr_full  <- year_partial_effect(lo.dsm.xy.fsyear.season,       grid_xy)
yr_nobs2 <- year_partial_effect(lo.dsm.xy.fsyear.season.nobs2, grid_xy)
yr_full$data  <- "All data (n_obs pooled)"
yr_nobs2$data <- "n_obs == 2 only"
yr_compare <- rbind(yr_full, yr_nobs2)

p.lo.nobs2.year <- ggplot(yr_compare, aes(x = year, y = partial, colour = data, fill = data)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Dusky dolphin: per-year partial effect, full data vs n_obs == 2 subset",
       subtitle = "count ~ s(x,y,year_fac,bs=\"fs\") + season, area-weighted mean over the survey extent",
       x = "Ano", y = "partial effect (log scale)", colour = NULL, fill = NULL) +
  theme_minimal(base_size = 13)

print(p.lo.nobs2.year)

ggsave(p.lo.nobs2.year,
       filename = file.path(out_dir, "LO_year_partial_effect_compare.png"),
       width = 9, height = 6)
fwrite(yr_compare, file.path(out_dir, "LO_year_partial_effect_compare.csv"))

# ============================================================================
# side-by-side fit summary
# ============================================================================
table_lo_nobs2_compare <- data.frame(
  model   = c("full data", "n_obs == 2 subset"),
  n_seg   = c(nrow(segdata), nrow(segdata_nobs2)),
  n_obs   = c(nrow(obsdata_lo_mod), nrow(obsdata_lo_mod_nobs2)),
  n_years = c(nlevels(segdata$year_fac), nlevels(segdata_nobs2$year_fac)),
  AIC     = round(c(AIC(lo.dsm.xy.fsyear.season), AIC(lo.dsm.xy.fsyear.season.nobs2)), 2),
  Dev     = round(c(summary(lo.dsm.xy.fsyear.season)$dev.expl,
                     summary(lo.dsm.xy.fsyear.season.nobs2)$dev.expl), 3)
)
print(table_lo_nobs2_compare)
fwrite(table_lo_nobs2_compare, file.path(out_dir, "LO_nobs2_fit_compare.csv"))

# ============================================================================
# map the predicted density from the n_obs == 2 subset model
# ============================================================================
# Mirrors Maps 5-6 of UTIL_Map_DSM_output_LO.R (lo.dsm.xy.fsyear.season), but
# for lo.dsm.xy.fsyear.season.nobs2. Track/sighting overlays are restricted to
# n_obs == 2 segments and detections, so each map only shows the data that
# actually trained it. Assumes patagonia_m, segdata_traj_m, distdata_lo_sf_m
# (from 0_ReadData_Plots.r) and pred.polys_m/survey.area_m/target_crs are in
# the workspace, same as the main map scripts.

library(sf)
library(dplyr)
library(viridis)

off.set <- 800 * trunc.dist_lo
ref_ano <- as.integer(round(median(segdata$Ano)))
bb      <- st_bbox(survey.area_m)
xpad    <- 3000
ypad    <- 3000

# segdata_traj_m is aggregated to one row per (traj_id, Ano, season) -- a
# whole survey leg, built by grouping segdata (see 0_ReadData_Plots.r) -- so
# it has no per-segment n_obs column to filter on directly. Instead, keep the
# (traj_id, Ano, season) legs that have ANY n_obs == 2 segment in the subset
# (in practice n_obs is constant within a single day's leg, so this is
# equivalent to "all segments in that leg have n_obs == 2").
traj_nobs2_keys <- unique(segdata_nobs2[, .(traj_id, Ano, season)])
segdata_traj_m_nobs2 <- segdata_traj_m %>%
  semi_join(traj_nobs2_keys, by = c("traj_id", "Ano", "season"))

distdata_lo_sf_m_nobs2 <- distdata_lo_sf_m %>% filter(n_obs == 2)

## Map A — season facets, at one reference fitted year ----
# UTIL_Map_DSM_output_LO.R hardcodes 2017 as the reference year for the
# full-data fs model (better spatial coverage than the median-year pick); use
# the same year here if the subset fit still has it, otherwise fall back to
# the nearest available year_fac level.
yf_levels_nobs2 <- levels(lo.dsm.xy.fsyear.season.nobs2$model$year_fac)
ref_yf_fs_nobs2 <- if ("2017" %in% yf_levels_nobs2) "2017" else
  yf_levels_nobs2[which.min(abs(as.numeric(yf_levels_nobs2) - ref_ano))]

pred.polys_season_nobs2_m <- bind_rows(
  pred.polys_m %>% mutate(season = "Summer"),
  pred.polys_m %>% mutate(season = "Spring"),
  pred.polys_m %>% mutate(season = "Winter"),
  pred.polys_m %>% mutate(season = "Fall")
) %>%
  mutate(
    season   = factor(season, levels = levels(obsdata_lo_mod$season)),
    year_fac = factor(ref_yf_fs_nobs2, levels = yf_levels_nobs2)
  )

pred.polys_season_nobs2_m$Nhat <- predict(
  lo.dsm.xy.fsyear.season.nobs2,
  newdata = pred.polys_season_nobs2_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_season_nobs2_m$area_m2 <- as.numeric(st_area(pred.polys_season_nobs2_m))
pred.polys_season_nobs2_m$density <- pred.polys_season_nobs2_m$Nhat /
  (pred.polys_season_nobs2_m$area_m2 / 1e6)

lo.map.density.season.nobs2 <- ggplot() +
  geom_sf(data = patagonia_m, fill = "grey85", color = "grey40") +
  geom_sf(data = pred.polys_season_nobs2_m, aes(fill = density), color = NA) +
  geom_sf(data = survey.area_m, fill = NA, color = "black", linewidth = 0.6) +
  # restricted to the reference year: the fs spatial term varies by year, so
  # only that year's n_obs == 2 segments informed the surface shown here.
  geom_sf(data = segdata_traj_m_nobs2 %>% filter(Ano == as.numeric(ref_yf_fs_nobs2)),
          size = 0.6, alpha = 0.25) +
  geom_sf(data = distdata_lo_sf_m_nobs2 %>% filter(Ano == as.numeric(ref_yf_fs_nobs2)),
          aes(size = size), alpha = 0.7) +
  scale_fill_viridis_c(option = "turbo", name = expression("Dolphins km"^-2)) +
  labs(
    title   = "Predicted spatial density of dusky dolphins (n_obs == 2 subset, year-varying spatial)",
    caption = sprintf(
      "model: count ~ s(x, y, year_fac, bs = \"fs\") + season  |  n_obs == 2 only. Ref year: %s",
      ref_yf_fs_nobs2),
    x = "Easting (Mm)", y = "Northing (Mm)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right", panel.grid.minor = element_blank()) +
  scale_x_continuous(labels = \(x) x / 1e6) +
  scale_y_continuous(labels = \(x) x / 1e6) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs), datum = target_crs, expand = FALSE
  ) +
  facet_wrap(. ~ season)

print(lo.map.density.season.nobs2)
ggsave(lo.map.density.season.nobs2,
       filename = file.path(out_dir, "LO_DSM_Season_fsyear_nobs2.png"),
       width = 13, height = 13)

## Map B — year facets, at season = Spring ----
# years_fs_nobs2 comes from the model's own fitted year_fac levels, so 2013
# (zero n_obs == 2 segments -- see the coverage check above) is automatically
# absent, not interpolated.
years_fs_nobs2 <- sort(as.numeric(yf_levels_nobs2))

pred.polys_year_nobs2_m <- bind_rows(
  lapply(years_fs_nobs2, function(a) pred.polys_m %>%
           mutate(Ano      = a,
                  year_fac = factor(as.character(a), levels = yf_levels_nobs2)))
) %>%
  mutate(season = factor("Spring", levels = levels(obsdata_lo_mod$season)))

pred.polys_year_nobs2_m$Nhat <- predict(
  lo.dsm.xy.fsyear.season.nobs2,
  newdata = pred.polys_year_nobs2_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_year_nobs2_m$area_m2 <- as.numeric(st_area(pred.polys_year_nobs2_m))
pred.polys_year_nobs2_m$density <- pred.polys_year_nobs2_m$Nhat /
  (pred.polys_year_nobs2_m$area_m2 / 1e6)

lo.map.density.year.nobs2 <- ggplot() +
  geom_sf(data = patagonia_m, fill = "grey85", color = "grey40") +
  geom_sf(data = pred.polys_year_nobs2_m, aes(fill = density), color = NA) +
  geom_sf(data = survey.area_m, fill = NA, color = "black", linewidth = 0.6) +
  # overlay intentionally shows ALL seasons of that year, not just Spring —
  # the fs spatial term does not vary by season within a year.
  geom_sf(data = segdata_traj_m_nobs2, size = 0.6, alpha = 0.25) +
  geom_sf(data = distdata_lo_sf_m_nobs2, aes(size = size), alpha = 0.7) +
  scale_fill_viridis_c(option = "turbo", name = expression("Dolphins km"^-2)) +
  labs(
    title   = "Predicted spatial density of dusky dolphins (Spring, n_obs == 2 subset, year-varying spatial)",
    caption = "model: count ~ s(x, y, year_fac, bs = \"fs\") + season  |  season = Spring, n_obs == 2 only",
    x = "Easting (Mm)", y = "Northing (Mm)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right", panel.grid.minor = element_blank()) +
  scale_x_continuous(labels = \(x) x / 1e6) +
  scale_y_continuous(labels = \(x) x / 1e6) +
  coord_sf(
    xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
    ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
    default_crs = st_crs(target_crs), datum = target_crs, expand = FALSE
  ) +
  facet_wrap(. ~ Ano)

print(lo.map.density.year.nobs2)
ggsave(lo.map.density.year.nobs2,
       filename = file.path(out_dir, "LO_DSM_Year_fsyear_nobs2.png"),
       width = 13, height = 13)
