# ADB / Claude
# 2026-08-02
#
# Dusky dolphin factor-smooth ("fs") model set using a BIOLOGICAL YEAR grouping
# instead of the calendar year (Ano). A biological year runs Spring (Oct-Dec of
# year Y) through Summer/Fall/Winter (Jan-Sep of year Y+1), mimicking the
# species' natural annual cycle instead of splitting that cycle across two
# calendar years.
#
# season -> Mes_n mapping (see 0_ReadData_Plots.r, Southern Hemisphere):
#   Summer = Jan-Mar, Fall = Apr-Jun, Winter = Jul-Sep, Spring = Oct-Dec.
# biological_year is therefore the SPRING calendar year: Spring keeps Ano;
# Summer/Fall/Winter are relabelled to Ano - 1 (the Spring that started their
# cycle), so e.g. Spring 2007 + Summer/Fall/Winter 2008 all become
# biological_year 2007.
#
# This script:
#   1. load()s the full dusky-dolphin workspace (output/DuskyDolphin/lo_output.RData)
#   2. defines biological_year / bioyear_fac on segdata and obsdata_lo_mod
#   3. re-profiles the Tweedie p parameter (logic of UTIL_FindTweedieP_DuskyDolphin.R)
#   4. fits the full fs model set (14 models, mirroring the fsyear block of
#      4_DuskyDolphin_DSM.R) using bioyear_fac in place of year_fac
#   5. builds a model-selection table (table_lo_bioyear_modselection)
#   6. appends those candidates to table_lo_combined_modselection
#   7. maps the season model (lo.dsm.xy.fsbioyear.season, no environmental
#      covariates), faceted by biological_year (mirrors Map 6 of
#      UTIL_Map_DSM_output_LO.R)
#   8. abundance/density estimates (dsm_var_gam) for that model, mirroring the
#      fs section of 5_DuskyDolphin_Abundance.R
#
# Assumes output/DuskyDolphin/lo_output.RData already exists (produced by
# running 1_DuskyDolphin.R once).

# 1. Load workspace ------------------------------------------------------------
load("output/DuskyDolphin/lo_output.RData")

library(dsm)
library(mgcv)
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(viridis)

source(file.path(here::here(), "R", "lnorm_ci.R"))

# 2. Biological year ------------------------------------------------------------
segdata[, biological_year := ifelse(season == "Spring", Ano, Ano - 1)]
obsdata_lo_mod[, biological_year := ifelse(season == "Spring", Ano, Ano - 1)]

segdata[, bioyear_fac := factor(biological_year)]
obsdata_lo_mod[, bioyear_fac := factor(biological_year)]

# 3. Tweedie p profiling (adapted from UTIL_FindTweedieP_DuskyDolphin.R) --------
# The profiled formula (count ~ s(x,y) + season) does not involve year at all,
# so switching from calendar to biological year cannot change the optimal p —
# this re-run exists to CONFIRM that empirically, not because a different
# answer is expected.
p_grid <- seq(1.1, 1.9, by = 0.2)
p_grid <- c(p_grid, 1.225, 1.250, 1.275, 1.325, 1.350, 1.375, 1.31, 1.32, 1.33, 1.34)

tw_profile_bioyear <- data.table(
  p    = p_grid,
  AIC  = NA_real_,
  REML = NA_real_
)

for (i in seq_along(p_grid)) {
  fit <- tryCatch(
    dsm(count ~ s(x, y) + season,
        ddf.obj          = df.lo,
        segment.data     = segdata,
        observation.data = obsdata_lo_mod,
        family           = Tweedie(p = p_grid[i]),
        method           = "REML"),
    error = function(e) NULL
  )

  if (!is.null(fit)) {
    tw_profile_bioyear[i, AIC  := AIC(fit)]
    tw_profile_bioyear[i, REML := fit$gcv.ubre]
  }
}

tw_profile_bioyear$deltaAIC <- tw_profile_bioyear$AIC - min(tw_profile_bioyear$AIC, na.rm = TRUE)
print(tw_profile_bioyear[order(AIC)])

best_p_bioyear <- tw_profile_bioyear[which.min(AIC), p]
cat(sprintf("Optimal Tweedie p (biological-year model set): %.4f\n", best_p_bioyear))

# 4. Fit the fs model set (bioyear_fac in place of year_fac) --------------------
lo.dsm.xy.fsbioyear.season <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs") +
                                    season,
                                  ddf.obj = df.lo,
                                  segment.data = segdata,
                                  observation.data = obsdata_lo_mod,
                                  family = Tweedie(p = best_p_bioyear),
                                  method = "REML")

lo.dsm.xy.fsbioyear <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs"),
                          ddf.obj = df.lo,
                          segment.data = segdata,
                          observation.data = obsdata_lo_mod,
                          family = Tweedie(p = best_p_bioyear),
                          method = "REML")

lo.dsm.xy.fsbioyear.season.slope <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs") +
                                          season + s(slope),
                                        ddf.obj = df.lo,
                                        segment.data = segdata,
                                        observation.data = obsdata_lo_mod,
                                        family = Tweedie(p = best_p_bioyear),
                                        method = "REML")

lo.dsm.xy.fsbioyear.season.grad <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs") +
                                         season + s(grad),
                                       ddf.obj = df.lo,
                                       segment.data = segdata,
                                       observation.data = obsdata_lo_mod,
                                       family = Tweedie(p = best_p_bioyear),
                                       method = "REML")

lo.dsm.xy.fsbioyear.season.sst <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs") +
                                        season + s(sst),
                                      ddf.obj = df.lo,
                                      segment.data = segdata,
                                      observation.data = obsdata_lo_mod,
                                      family = Tweedie(p = best_p_bioyear),
                                      method = "REML")

lo.dsm.xy.fsbioyear.season.clo <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs") +
                                        season + s(clo),
                                      ddf.obj = df.lo,
                                      segment.data = segdata,
                                      observation.data = obsdata_lo_mod,
                                      family = Tweedie(p = best_p_bioyear),
                                      method = "REML")

lo.dsm.xy.fsbioyear.season.dist.up <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs") +
                                            season + s(dist.up),
                                          ddf.obj = df.lo,
                                          segment.data = segdata,
                                          observation.data = obsdata_lo_mod,
                                          family = Tweedie(p = best_p_bioyear),
                                          method = "REML")

lo.dsm.xy.fsbioyear.season.depth <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs") +
                                          season + s(depth),
                                        ddf.obj = df.lo,
                                        segment.data = segdata,
                                        observation.data = obsdata_lo_mod,
                                        family = Tweedie(p = best_p_bioyear),
                                        method = "REML")

lo.dsm.xy.fsbioyear.slope <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs") +
                                   s(slope),
                                 ddf.obj = df.lo,
                                 segment.data = segdata,
                                 observation.data = obsdata_lo_mod,
                                 family = Tweedie(p = best_p_bioyear),
                                 method = "REML")

lo.dsm.xy.fsbioyear.grad <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs") +
                                  s(grad),
                                ddf.obj = df.lo,
                                segment.data = segdata,
                                observation.data = obsdata_lo_mod,
                                family = Tweedie(p = best_p_bioyear),
                                method = "REML")

lo.dsm.xy.fsbioyear.sst <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs") +
                                 s(sst),
                               ddf.obj = df.lo,
                               segment.data = segdata,
                               observation.data = obsdata_lo_mod,
                               family = Tweedie(p = best_p_bioyear),
                               method = "REML")

lo.dsm.xy.fsbioyear.clo <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs") +
                                 s(clo),
                               ddf.obj = df.lo,
                               segment.data = segdata,
                               observation.data = obsdata_lo_mod,
                               family = Tweedie(p = best_p_bioyear),
                               method = "REML")

lo.dsm.xy.fsbioyear.dist.up <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs") +
                                     s(dist.up),
                                   ddf.obj = df.lo,
                                   segment.data = segdata,
                                   observation.data = obsdata_lo_mod,
                                   family = Tweedie(p = best_p_bioyear),
                                   method = "REML")

lo.dsm.xy.fsbioyear.depth <- dsm(count ~ s(x, y, bioyear_fac, bs = "fs") +
                                   s(depth),
                                 ddf.obj = df.lo,
                                 segment.data = segdata,
                                 observation.data = obsdata_lo_mod,
                                 family = Tweedie(p = best_p_bioyear),
                                 method = "REML")

# 5. Model-selection table -------------------------------------------------------
table_lo_bioyear_modselection <- AIC(
  lo.dsm.xy.fsbioyear.season,
  lo.dsm.xy.fsbioyear,
  lo.dsm.xy.fsbioyear.season.slope,
  lo.dsm.xy.fsbioyear.season.grad,
  lo.dsm.xy.fsbioyear.season.sst,
  lo.dsm.xy.fsbioyear.season.clo,
  lo.dsm.xy.fsbioyear.season.dist.up,
  lo.dsm.xy.fsbioyear.season.depth,
  lo.dsm.xy.fsbioyear.slope,
  lo.dsm.xy.fsbioyear.grad,
  lo.dsm.xy.fsbioyear.sst,
  lo.dsm.xy.fsbioyear.clo,
  lo.dsm.xy.fsbioyear.dist.up,
  lo.dsm.xy.fsbioyear.depth
) %>%
  mutate(deltaAIC = round(AIC - min(AIC), 2)) %>%
  mutate(Dev = c(
    round(summary(lo.dsm.xy.fsbioyear.season)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsbioyear)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsbioyear.season.slope)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsbioyear.season.grad)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsbioyear.season.sst)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsbioyear.season.clo)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsbioyear.season.dist.up)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsbioyear.season.depth)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsbioyear.slope)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsbioyear.grad)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsbioyear.sst)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsbioyear.clo)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsbioyear.dist.up)$dev.expl, 2),
    round(summary(lo.dsm.xy.fsbioyear.depth)$dev.expl, 2)
  )) %>%
  mutate(model = c(
    "count ~ s(x,y,bioyear_fac,bs=fs) + season",
    "count ~ s(x,y,bioyear_fac,bs=fs)",
    "count ~ s(x,y,bioyear_fac,bs=fs) + season + s(slope)",
    "count ~ s(x,y,bioyear_fac,bs=fs) + season + s(grad)",
    "count ~ s(x,y,bioyear_fac,bs=fs) + season + s(sst)",
    "count ~ s(x,y,bioyear_fac,bs=fs) + season + s(clo)",
    "count ~ s(x,y,bioyear_fac,bs=fs) + season + s(dist.up)",
    "count ~ s(x,y,bioyear_fac,bs=fs) + season + s(depth)",
    "count ~ s(x,y,bioyear_fac,bs=fs) + s(slope)",
    "count ~ s(x,y,bioyear_fac,bs=fs) + s(grad)",
    "count ~ s(x,y,bioyear_fac,bs=fs) + s(sst)",
    "count ~ s(x,y,bioyear_fac,bs=fs) + s(clo)",
    "count ~ s(x,y,bioyear_fac,bs=fs) + s(dist.up)",
    "count ~ s(x,y,bioyear_fac,bs=fs) + s(depth)"
  )) %>%
  data.table() %>%
  mutate(df = round(df, 2), AIC = round(AIC, 2)) %>%
  select(model, df, AIC, deltaAIC, Dev) %>%
  arrange(deltaAIC)

print(table_lo_bioyear_modselection)

# 6. Append to the combined selection table --------------------------------------
# table_lo_combined_modselection (columns: basis, model, df, AIC, deltaAIC, Dev)
# already exists in the workspace (built by 4_DuskyDolphin_DSM_soap.R). Older
# saved workspaces may predate the AIC column (only deltaAIC was kept) — if so,
# recover the absolute scale using lo.dsm.xy.fsyear.season as an anchor (it is
# guaranteed to be both in the workspace and a row of that table).
if (!"AIC" %in% names(table_lo_combined_modselection)) {
  .anchor_label <- paste(deparse(formula(lo.dsm.xy.fsyear.season), width.cutoff = 200),
                         collapse = " ")
  .anchor_row   <- table_lo_combined_modselection[
    table_lo_combined_modselection$model == .anchor_label, ]
  stopifnot(nrow(.anchor_row) == 1)
  .aic_min_old  <- AIC(lo.dsm.xy.fsyear.season) - .anchor_row$deltaAIC

  table_lo_combined_modselection$AIC <-
    table_lo_combined_modselection$deltaAIC + .aic_min_old
}

.bioyear_models <- list(
  lo.dsm.xy.fsbioyear.season, lo.dsm.xy.fsbioyear,
  lo.dsm.xy.fsbioyear.season.slope, lo.dsm.xy.fsbioyear.season.grad,
  lo.dsm.xy.fsbioyear.season.sst, lo.dsm.xy.fsbioyear.season.clo,
  lo.dsm.xy.fsbioyear.season.dist.up, lo.dsm.xy.fsbioyear.season.depth,
  lo.dsm.xy.fsbioyear.slope, lo.dsm.xy.fsbioyear.grad, lo.dsm.xy.fsbioyear.sst,
  lo.dsm.xy.fsbioyear.clo, lo.dsm.xy.fsbioyear.dist.up, lo.dsm.xy.fsbioyear.depth
)
.bioyear_labels <- c(
  "count ~ s(x,y,bioyear_fac,bs=fs) + season",
  "count ~ s(x,y,bioyear_fac,bs=fs)",
  "count ~ s(x,y,bioyear_fac,bs=fs) + season + s(slope)",
  "count ~ s(x,y,bioyear_fac,bs=fs) + season + s(grad)",
  "count ~ s(x,y,bioyear_fac,bs=fs) + season + s(sst)",
  "count ~ s(x,y,bioyear_fac,bs=fs) + season + s(clo)",
  "count ~ s(x,y,bioyear_fac,bs=fs) + season + s(dist.up)",
  "count ~ s(x,y,bioyear_fac,bs=fs) + season + s(depth)",
  "count ~ s(x,y,bioyear_fac,bs=fs) + s(slope)",
  "count ~ s(x,y,bioyear_fac,bs=fs) + s(grad)",
  "count ~ s(x,y,bioyear_fac,bs=fs) + s(sst)",
  "count ~ s(x,y,bioyear_fac,bs=fs) + s(clo)",
  "count ~ s(x,y,bioyear_fac,bs=fs) + s(dist.up)",
  "count ~ s(x,y,bioyear_fac,bs=fs) + s(depth)"
)

.bioyear_rows <- do.call(rbind, Map(function(m, lab)
  data.frame(basis = "thin-plate", model = lab,
             df    = round(attr(logLik(m), "df"), 2),
             AIC   = round(AIC(m), 2),
             Dev   = round(summary(m)$dev.expl, 2),
             stringsAsFactors = FALSE),
  .bioyear_models, .bioyear_labels))

table_lo_combined_modselection <- rbind(
  table_lo_combined_modselection[, c("basis", "model", "df", "AIC", "Dev")],
  .bioyear_rows
)
table_lo_combined_modselection$deltaAIC <-
  round(table_lo_combined_modselection$AIC -
          min(table_lo_combined_modselection$AIC), 2)
table_lo_combined_modselection <-
  table_lo_combined_modselection[order(table_lo_combined_modselection$deltaAIC),
                                 c("basis", "model", "df", "AIC", "deltaAIC", "Dev")]
rownames(table_lo_combined_modselection) <- NULL

print(table_lo_combined_modselection)

# 7. Map — lo.dsm.xy.fsbioyear.season, one panel per BIOLOGICAL YEAR ------------
# count ~ s(x, y, bioyear_fac, bs = "fs") + season
#
# Mirrors Map 6 of UTIL_Map_DSM_output_LO.R (fs model, season fixed at Spring,
# faceted by year) but with biological_year as the facetting variable in place
# of the calendar year Ano.

off.set <- 800 * trunc.dist_lo
bb      <- st_bbox(survey.area_m)
xpad    <- 3000
ypad    <- 3000

if (!all(c("x", "y") %in% names(pred.polys_m))) {
  pred.polys_m <- pred.polys_m %>%
    mutate(
      x = st_coordinates(st_centroid(geometry))[, 1],
      y = st_coordinates(st_centroid(geometry))[, 2]
    )
}

yf_levels_bio <- levels(lo.dsm.xy.fsbioyear.season$model$bioyear_fac)
years_bio     <- sort(as.numeric(yf_levels_bio))

pred.polys_bioyear_fs_m <- bind_rows(
  lapply(years_bio, function(a) pred.polys_m %>%
           mutate(biological_year = a,
                  bioyear_fac     = factor(as.character(a), levels = yf_levels_bio)))
) %>%
  mutate(season = factor("Spring", levels = levels(obsdata_lo_mod$season)))

pred.polys_bioyear_fs_m$Nhat <- predict(
  lo.dsm.xy.fsbioyear.season,
  newdata = pred.polys_bioyear_fs_m,
  off.set = off.set,
  type    = "response"
)

pred.polys_bioyear_fs_m$area_m2  <- as.numeric(st_area(pred.polys_bioyear_fs_m))
pred.polys_bioyear_fs_m$density  <- pred.polys_bioyear_fs_m$Nhat / (pred.polys_bioyear_fs_m$area_m2 / 1e6)
pred.polys_bioyear_fs_m$ldensity <- log10(pred.polys_bioyear_fs_m$density + 0.001)

# overlay: tag segdata_traj_m / distdata_lo_sf_m with the SAME biological_year
# definition and show ALL seasons within that biological year — the fs spatial
# term s(x,y,bioyear_fac) does not vary by season, so a high count from ANY
# season legitimately informs the biological-year-level spatial pattern shown
# here (same logic as Map 6's "all seasons" overlay for calendar year).
segdata_traj_bio_m <- segdata_traj_m %>%
  filter(Ano > 2006) %>%
  mutate(biological_year = ifelse(season == "Spring", Ano, Ano - 1))

distdata_bio_lo_sf_m <- distdata_lo_sf_m %>%
  filter(Ano > 2006) %>%
  mutate(biological_year = ifelse(season == "Spring", Ano, Ano - 1))

lo.map.density.bioyear.fs <- ggplot() +
  geom_sf(data = patagonia_m,
          fill  = "grey85",
          color = "grey40") +
  geom_sf(data  = pred.polys_bioyear_fs_m,
          aes(fill = density),
          color = NA) +
  geom_sf(data      = survey.area_m,
          fill      = NA,
          color     = "black",
          linewidth = 0.6) +
  geom_sf(data  = segdata_traj_bio_m,
          size  = 0.6,
          alpha = 0.25) +
  geom_sf(data  = distdata_bio_lo_sf_m,
          aes(size = size),
          alpha = 0.7) +
  scale_fill_viridis_c(
    option = "turbo",
    name   = expression("Dolphins km"^-2)
  ) +
  labs(
    title   = "Predicted spatial density of dusky dolphins (Spring, biological-year-varying spatial)",
    caption = "model: count ~ s(x, y, bioyear_fac, bs = \"fs\") + season  |  season = Spring  |  biological year = Spring(Y)→Winter(Y+1)",
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
  facet_wrap(. ~ biological_year)

lo.map.density.bioyear.fs

ggsave(
  filename = "output/DuskyDolphin/DSM/LO_DSM_BiologicalYear_fsbioyear.png",
  plot     = lo.map.density.bioyear.fs,
  width    = 13,
  height   = 13
)

# 8. Abundance / density estimates (dsm_var_gam) ---------------------------------
# Mirrors the fs section of 5_DuskyDolphin_Abundance.R: one estimate per
# (season, calendar year) combination actually surveyed, predicted from
# lo.dsm.xy.fsbioyear.season, then plotted against biological_year instead of
# Ano. obs_strata (the model-independent Horvitz-Thompson design-based
# estimate, built in 5_DuskyDolphin_Abundance.R) is reused, not recomputed.

results_lo_bio <- lapply(seq_len(nrow(sy_combos)),
                         function(i) {

                           s   <- sy_combos$season[i]
                           a   <- sy_combos$Ano[i]
                           by_ <- ifelse(s == "Spring", a, a - 1)

                           pred_grid <- pred.polys_m %>%
                             st_drop_geometry() %>%
                             mutate(
                               season      = factor(s, levels = season_levels),
                               bioyear_fac = factor(as.character(by_), levels = yf_levels_bio)
                             )

                           vp <- dsm_var_gam(
                             dsm.obj   = lo.dsm.xy.fsbioyear.season,
                             pred.data = pred_grid,
                             off.set   = cell_area_m2
                           )

                           sm      <- summary(vp)
                           N_hat   <- as.numeric(sm$pred.est)
                           cv_hat  <- as.numeric(sm$cv)
                           ci      <- lnorm_ci(N_hat, cv_hat)
                           dens    <- N_hat / survey_area_km2
                           dens_lo <- ci$lo  / survey_area_km2
                           dens_hi <- ci$hi  / survey_area_km2

                           data.table(
                             species         = "Dusky dolphin",
                             season          = s,
                             year            = a,
                             biological_year = by_,
                             N_hat           = round(N_hat),
                             N_lo95          = round(ci$lo),
                             N_hi95          = round(ci$hi),
                             CV              = round(cv_hat, 3),
                             density         = round(dens,    4),
                             dens_lo         = round(dens_lo, 4),
                             dens_hi         = round(dens_hi, 4)
                           )
                         })

lo_abund_bioyear <- rbindlist(results_lo_bio) %>%
  # offsets ordered chronologically WITHIN a biological year: Spring first
  # (it starts the cycle), then Summer, Fall, Winter of the following
  # calendar year — unlike the calendar-year "sy" offsets used elsewhere.
  mutate(seas = as.numeric(case_when(
    season == "Spring" ~ "0.10",
    season == "Summer" ~ "0.35",
    season == "Fall"   ~ "0.60",
    season == "Winter" ~ "0.85"
  ))) %>%
  mutate(sy_bio = biological_year + seas) %>%
  data.table()

# reuse the model-independent design-based estimate (obs_strata), keyed by
# (season, calendar year) as it was originally built
lo_abund_bioyear[, `:=`(season = as.character(season), year = as.numeric(year))]
lo_abund_bioyear <- merge(lo_abund_bioyear, obs_strata, by = c("season", "year"), all.x = TRUE)
lo_abund_bioyear[, season := factor(season, levels = season_levels)]
setorder(lo_abund_bioyear, sy_bio)

## abundance (biological year, fs) ----
### facets -----
p.lo.N.bioyear <- ggplot(lo_abund_bioyear, aes(x = biological_year)) +
  geom_ribbon(aes(ymin = N_lo95, ymax = N_hi95), alpha = 0.2) +
  geom_line(aes(y = N_hat)) +
  geom_point(aes(y = N_hat, colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = N_obs_lo, ymax = N_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = N_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  facet_wrap(~ season, scales = "free_y") +
  labs(
    title    = "Dusky dolphin abundance by season and biological year (year-varying spatial)",
    subtitle = "count ~ s(x, y, bioyear_fac, bs = \"fs\") + season  |  ribbon = 95% CI (lognormal)",
    x        = "Biological year (Spring start)",
    y        = expression(hat(N))
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

p.lo.N.bioyear

### continuous ----
p.lo.N.sy.bioyear <- ggplot(lo_abund_bioyear, aes(x = sy_bio)) +
  geom_ribbon(aes(ymin = N_lo95, ymax = N_hi95), alpha = 0.2) +
  geom_line(aes(y = N_hat)) +
  geom_point(aes(y = N_hat, colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = N_obs_lo, ymax = N_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = N_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  scale_x_continuous(breaks = 2006:2018,
                     minor_breaks = seq(2006.1, 2018.1, 0.25),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(
    subtitle = "count ~ s(x, y, bioyear_fac, bs = \"fs\") + season  |  ribbon = 95% CI (lognormal)",
    x        = "",
    y        = expression(hat(N))
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.minor.ticks.length.x = rel(0.65))

p.lo.N.sy.bioyear

## density (biological year, fs) ----
### facets -----
p.lo.D.bioyear <- ggplot(lo_abund_bioyear, aes(x = biological_year)) +
  geom_ribbon(aes(ymin = dens_lo, ymax = dens_hi), alpha = 0.2) +
  geom_line(aes(y = density)) +
  geom_point(aes(y = density,     colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = density_obs_lo, ymax = density_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = density_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  facet_wrap(~ season, scales = "free_y") +
  labs(
    title    = "Dusky dolphin density by season and biological year (year-varying spatial)",
    subtitle = "count ~ s(x, y, bioyear_fac, bs = \"fs\") + season  |  ribbon = 95% CI (lognormal)",
    x        = "Biological year (Spring start)",
    y        = expression("Dolphins km"^{-2})
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

p.lo.D.bioyear

### continuous ----
p.lo.D.sy.bioyear <- ggplot(lo_abund_bioyear, aes(x = sy_bio)) +
  geom_ribbon(aes(ymin = dens_lo, ymax = dens_hi), alpha = 0.2) +
  geom_line(aes(y = density)) +
  geom_point(aes(y = density,     colour = "Density surface model"), size = 2) +
  geom_linerange(aes(ymin = density_obs_lo, ymax = density_obs_hi,
                     colour = "design-based Horvitz–Thompson estimate"),
                 alpha = 0.6, na.rm = TRUE) +
  geom_point(aes(y = density_obs, colour = "design-based Horvitz–Thompson estimate"),
             size = 2.6, shape = 17, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = fit_cols) +
  scale_x_continuous(breaks = 2006:2018,
                     minor_breaks = seq(2006.1, 2018.1, 0.25),
                     guide = guide_axis(minor.ticks = TRUE)) +
  labs(
    subtitle = "count ~ s(x, y, bioyear_fac, bs = \"fs\") + season  |  ribbon = 95% CI (lognormal)",
    x        = "",
    y        = expression("Dolphins km"^{-2})
  ) +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.minor.ticks.length.x = rel(0.65))

p.lo.D.sy.bioyear

## output -----
fwrite(lo_abund_bioyear, "output/DuskyDolphin/Abundance/LO_abundance_season_bioyear_fsbioyear.csv")

ggsave(
  "output/DuskyDolphin/Abundance/LO_abundance_season_bioyear_fsbioyear.png",
  plot   = p.lo.N.bioyear,
  width  = 13,
  height = 8
)
ggsave(
  "output/DuskyDolphin/Abundance/LO_density_season_bioyear_fsbioyear.png",
  plot   = p.lo.D.bioyear,
  width  = 13,
  height = 8
)
ggsave(
  "output/DuskyDolphin/Abundance/LO_abundance_modelfit_fsbioyear.png",
  plot   = p.lo.N.sy.bioyear,
  width  = 13,
  height = 8
)
ggsave(
  "output/DuskyDolphin/Abundance/LO_density_modelfit_fsbioyear.png",
  plot   = p.lo.D.sy.bioyear,
  width  = 13,
  height = 8
)
