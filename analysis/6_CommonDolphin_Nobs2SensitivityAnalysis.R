# ADB / Claude
# 2026-09-01
#
# n_obs == 2 sensitivity analysis — COMMON dolphins.
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
# true abundance were flat.
#
# ADB investigated adding n_obs (grouped as nobs_grp: "1" vs "> 1") as a
# COVARIATE in the detection function to correct for this (see
# 3_CommonDolphin_DetectionFunction.R / 3_DuskyDolphin_DetectionFunction.R).
# That approach was abandoned -- see NOTE_nobsgrp_detection_function_issue.R
# for the full writeup. In short: for dusky dolphins the nobsgrp coefficient's
# Hessian was numerically singular (only 9 of 108 detections had
# nobs_grp == "1"), producing an unusable SE that inflated every downstream
# abundance CV to nonsense (CV ~ 64000, identical on every row of every
# table). For common dolphins nobsgrp itself was numerically fine, but the
# year x n_obs cross-tab in segdata showed n_obs is nearly a STEP FUNCTION of
# year (n_obs == 1 only occurs 2006-2010; n_obs is almost always 2 from 2015
# on) -- meaning nobs_grp/n_obs and s(Ano)/year are too close to collinear
# for a covariate-based correction to cleanly separate "detectability
# changed" from "abundance changed". Whatever a joint model returns mostly
# reflects which term the fit happens to favour, not evidence about the real
# question.
#
# WHY NOT "add n_obs to the dsm() count-model formula" (the other option
# considered instead of a detection-function covariate): rejected for the
# same collinearity reason, plus n_obs is a DETECTABILITY covariate and so is
# the wrong kind of term to add to a density surface in the first place. See
# NOTE_nobsgrp_detection_function_issue.R section 4 for the full argument.
#
# THIS SCRIPT tests the confound directly instead, by holding n_obs constant
# rather than modelling it: it refits the best common-dolphin model
# (dd.dsm.soap.season.year, from 4_CommonDolphin_DSM_soap.R -- count ~
# s(x,y,bs="so") + season + s(Ano)) on the SUBSET of segments/detections
# where n_obs == 2 (the modal, most consistently represented level across
# the full 2006-2018 range -- see the coverage check below), using the SAME
# covariate-free detection function as the rest of the pipeline (df.dd). If
# the year trend survives in this observer-effort-constant subset, that is
# evidence the trend is real rather than an artefact of changing observer
# numbers. If it disappears or reverses, that is evidence it was (at least
# partly) an observer-effort artefact.
#
# CAVEAT: segdata's 2013 segments are ALL n_obs == 1 (39 segments, zero with
# n_obs == 2) -- the n_obs == 2 subset therefore has NO 2013 data at all.
# Every other year (2006-2010, 2014-2018) has usable n_obs == 2 coverage
# (40-100% of that year's segments; see the printed coverage table below).
# The s(Ano) smooth will interpolate through 2013 with no way to check that
# year specifically within this subset.
#
# Assumes in the workspace (i.e. run after 4_CommonDolphin_DSM_soap.R, which
# 1_CommonDolphin.R does):
#   df.dd, segdata, obsdata_dd_mod, bnd_soap, knots, soap_term,
#   dd.dsm.soap.season.year

library(dsm)
library(mgcv)
library(data.table)
library(ggplot2)

out_dir <- "output/CommonDolphin/Nobs2Sensitivity"
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
          " -- s(Ano) will interpolate through these years with no data",
          " there to check them.")

fwrite(nobs2_by_year, file.path(out_dir, "DD_nobs2_coverage_by_year.csv"))

# ============================================================================
# subset to n_obs == 2 and refit the best model
# ============================================================================
segdata_nobs2        <- segdata[n_obs == 2]
obsdata_dd_mod_nobs2 <- obsdata_dd_mod[Sample.Label %in% segdata_nobs2$Sample.Label]

stopifnot(nrow(segdata_nobs2) > 0)

message(sprintf(
  "n_obs == 2 subset: %d/%d segments (%.1f%%), %d/%d detections",
  nrow(segdata_nobs2), nrow(segdata),
  100 * nrow(segdata_nobs2) / nrow(segdata),
  nrow(obsdata_dd_mod_nobs2), nrow(obsdata_dd_mod)))

# same spatial basis (bnd_soap / knots / soap_term) as the full-data model in
# 4_CommonDolphin_DSM_soap.R, so the two fits are directly comparable and
# differ only in which rows trained them.
dd_soap_form <- as.formula(paste("count ~", soap_term, "+ season + s(Ano)"))
environment(dd_soap_form) <- globalenv()

dd.dsm.soap.season.year.nobs2 <- dsm(
  dd_soap_form,
  ddf.obj          = df.dd,
  segment.data     = segdata_nobs2,
  observation.data = obsdata_dd_mod_nobs2,
  family           = tw(link = "log"),
  method           = "REML",
  knots            = knots
)

print(summary(dd.dsm.soap.season.year.nobs2))

# ============================================================================
# compare the s(Ano) partial effect: full data vs n_obs == 2 subset
# ============================================================================
# s(Ano) enters additively (not interacted with the spatial term or season),
# so its partial effect does not depend on which valid x/y/season is held
# fixed for prediction -- a single representative point suffices.
extract_sAno <- function(model, ano_grid, label) {
  nd <- data.frame(
    Ano    = ano_grid,
    x      = mean(model$model$x),
    y      = mean(model$model$y),
    season = factor(levels(model$model$season)[1],
                    levels = levels(model$model$season))
  )
  pr <- predict(model, newdata = nd, type = "terms", se.fit = TRUE, off.set = 1)
  term_col <- grep("^s\\(Ano\\)$", colnames(pr$fit))
  if (length(term_col) != 1)
    stop("Could not find a unique s(Ano) term column in predict(type='terms') ",
         "output -- columns were: ", paste(colnames(pr$fit), collapse = ", "))
  data.frame(Ano      = ano_grid,
             estimate = as.numeric(pr$fit[, term_col]),
             se       = as.numeric(pr$se.fit[, term_col]),
             data     = label)
}

ano_grid <- seq(min(segdata$Ano), max(segdata$Ano), length.out = 100)

sAno_full  <- extract_sAno(dd.dsm.soap.season.year,       ano_grid, "All data (n_obs pooled)")
sAno_nobs2 <- extract_sAno(dd.dsm.soap.season.year.nobs2, ano_grid, "n_obs == 2 only")
sAno_compare <- rbind(sAno_full, sAno_nobs2)

p.dd.nobs2.sAno <- ggplot(sAno_compare, aes(x = Ano, y = estimate, colour = data, fill = data)) +
  geom_ribbon(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
              alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1) +
  labs(title = "Common dolphin: s(Ano) partial effect, full data vs n_obs == 2 subset",
       subtitle = "count ~ s(x,y,bs=\"so\") + season + s(Ano), same spatial basis both fits",
       x = "Ano", y = "partial effect (log scale)", colour = NULL, fill = NULL) +
  theme_bw() +
  theme(base.size = 13,
        legend.position = "bottom")

print(p.dd.nobs2.sAno)

ggsave(p.dd.nobs2.sAno,
       filename = file.path(out_dir, "DD_sAno_partial_effect_compare.png"),
       width = 10, height = 6)
fwrite(sAno_compare, file.path(out_dir, "DD_sAno_partial_effect_compare.csv"))


m <- dd.dsm.soap.season.year.nobs2
class(m) <- setdiff(class(m), "dsm")

p.dd.soap.seasonyear.year.nobs2 <- gratia::draw(m,
                                                select = "s(Ano)",
                                                residuals = TRUE,
                                                rug = TRUE) +
  theme_bw() +
  geom_hline(yintercept = 0, col = "gray30")

p.dd.soap.seasonyear.season.nobs2 <- gratia::draw(gratia::parametric_effects(m,
                                                                             term = "season")) +
  theme_bw()

# ============================================================================
# side-by-side fit summary
# ============================================================================
table_dd_nobs2_compare <- data.frame(
  model   = c("full data", "n_obs == 2 subset"),
  n_seg   = c(nrow(segdata), nrow(segdata_nobs2)),
  n_obs   = c(nrow(obsdata_dd_mod), nrow(obsdata_dd_mod_nobs2)),
  AIC     = round(c(AIC(dd.dsm.soap.season.year), AIC(dd.dsm.soap.season.year.nobs2)), 2),
  Dev     = round(c(summary(dd.dsm.soap.season.year)$dev.expl,
                    summary(dd.dsm.soap.season.year.nobs2)$dev.expl), 3)
)
print(table_dd_nobs2_compare)
fwrite(table_dd_nobs2_compare, file.path(out_dir, "DD_nobs2_fit_compare.csv"))

# ============================================================================
# map the predicted density from the n_obs == 2 subset model
# ============================================================================
# Mirrors Maps 9-10 of UTIL_Map_DSM_output_DD.R (dd.dsm.soap.season.year), but
# for dd.dsm.soap.season.year.nobs2. Track/sighting overlays are restricted to
# n_obs == 2 segments and detections, so each map only shows the data that
# actually trained it. Assumes patagonia_m, segdata_traj_m, distdata_dd_sf_m
# (from 0_ReadData_Plots.r) and pred.polys_m/survey.area_m/target_crs are in
# the workspace, same as the main map scripts.

library(sf)
library(dplyr)
library(viridis)

off.set <- 800 * trunc.dist_dd
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

distdata_dd_sf_m_nobs2 <- distdata_dd_sf_m %>% filter(n_obs == 2)

## Map A — season facets, at reference year ref_ano ----
pred.polys_season_nobs2_m <- bind_rows(
  pred.polys_m %>% mutate(season = "Summer"),
  pred.polys_m %>% mutate(season = "Spring"),
  pred.polys_m %>% mutate(season = "Winter"),
  pred.polys_m %>% mutate(season = "Fall")
) %>%
  mutate(
    season = factor(season, levels = levels(obsdata_dd_mod$season)),
    Ano    = ref_ano
  )

pred.polys_season_nobs2_m$Nhat <- predict(
  dd.dsm.soap.season.year.nobs2,
  newdata = pred.polys_season_nobs2_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_season_nobs2_m$area_m2 <- as.numeric(st_area(pred.polys_season_nobs2_m))
pred.polys_season_nobs2_m$density <- pred.polys_season_nobs2_m$Nhat /
  (pred.polys_season_nobs2_m$area_m2 / 1e6)

dd.map.density.season.nobs2 <- ggplot() +
  geom_sf(data = patagonia_m, fill = "grey85", color = "grey40") +
  geom_sf(data = pred.polys_season_nobs2_m, aes(fill = density), color = NA) +
  geom_sf(data = survey.area_m, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = segdata_traj_m_nobs2, size = 0.6, alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m_nobs2, aes(size = size), alpha = 0.7) +
  scale_fill_viridis_c(option = "turbo", name = expression("Dolphins km"^-2),
                       na.value = "grey95") +
  labs(
    title   = "Predicted spatial density of common dolphins (n_obs == 2 subset, soap-film)",
    caption = sprintf(
      "model: count ~ s(x, y, bs = \"so\") + season + s(Ano)  |  n_obs == 2 only. Ref year: %d",
      ref_ano),
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

print(dd.map.density.season.nobs2)
ggsave(dd.map.density.season.nobs2,
       filename = file.path(out_dir, "DD_DSM_Season_soap_nobs2.png"),
       width = 13, height = 13)

## Map B — year facets, at season = Spring ----
# 2013 has zero n_obs == 2 segments (see the coverage check above), so it is
# absent from segdata_nobs2$Ano and this loop naturally skips it — no special
# handling needed.
years_nobs2 <- sort(unique(segdata_nobs2$Ano))

pred.polys_year_nobs2_m <- bind_rows(
  lapply(years_nobs2, function(a) pred.polys_m %>% mutate(Ano = a))
) %>%
  mutate(season = factor("Spring", levels = levels(obsdata_dd_mod$season)))

pred.polys_year_nobs2_m$Nhat <- predict(
  dd.dsm.soap.season.year.nobs2,
  newdata = pred.polys_year_nobs2_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_year_nobs2_m$area_m2 <- as.numeric(st_area(pred.polys_year_nobs2_m))
pred.polys_year_nobs2_m$density <- pred.polys_year_nobs2_m$Nhat /
  (pred.polys_year_nobs2_m$area_m2 / 1e6)

dd.map.density.year.nobs2 <- ggplot() +
  geom_sf(data = patagonia_m, fill = "grey85", color = "grey40") +
  geom_sf(data = pred.polys_year_nobs2_m, aes(fill = density), color = NA) +
  geom_sf(data = survey.area_m, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = segdata_traj_m_nobs2, size = 0.6, alpha = 0.25) +
  geom_sf(data = distdata_dd_sf_m_nobs2, aes(size = size), alpha = 0.7) +
  scale_fill_viridis_c(option = "turbo", name = expression("Dolphins km"^-2),
                       na.value = "grey95") +
  labs(
    title   = "Predicted spatial density of common dolphins (Spring, n_obs == 2 subset, soap-film)",
    caption = "model: count ~ s(x, y, bs = \"so\") + season + s(Ano)  |  season = Spring, n_obs == 2 only",
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

print(dd.map.density.year.nobs2)
ggsave(dd.map.density.year.nobs2,
       filename = file.path(out_dir, "DD_DSM_Year_soap_nobs2.png"),
       width = 13, height = 13)


# output -----
ggsave(plot = p.dd.soap.seasonyear.season.nobs2,
       file = file.path(out_dir, "DD_season_partial_effect_soap_nobs2.png"),
       width  = 10,
       height = 6)
ggsave(plot = p.dd.soap.seasonyear.year.nobs2,
       file = file.path(out_dir, "DD_year_partial_effect_soap_nobs2.png"),
       width  = 10,
       height = 6)
