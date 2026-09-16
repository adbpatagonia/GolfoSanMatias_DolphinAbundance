# ADB / Claude
# 2026-09-15
#
# COMMON DOLPHIN -- density maps and abundance series for the TUNED soap arm.
#
# Two models, both at the tuned configuration settled in UTIL_DSM_SoapTuning_DD.R
# (boundary tol500/margin250, 14x11 grid -> 89 knots, K_BND = 10, K_COV = 20):
#
#   base :  count ~ s(x, y, bs = "so") + season + s(Ano)              AIC 6070.35
#   sst  :  count ~ s(x, y, bs = "so") + season + s(Ano) + s(sst, k=20)  AIC 6034.93
#
# The tuned models are NOT stored anywhere -- .cache_soaptune_dd/ holds one-row
# summary tables, not gam objects -- so this script refits them once and caches
# the fitted objects in output/CommonDolphin/DSM/tuned_models/.
#
# ---------------------------------------------------------------------------
# THREE THINGS THAT WERE CHECKED BEFORE ANY OF THIS WAS WRITTEN
# ---------------------------------------------------------------------------
#
# 1. PREDICTION OUTSIDE THE TUNED BOUNDARY -- a non-issue, verified.
#    The tuned boundary (2012.0 km2) is tighter than the stored one (2184.6),
#    so it was not obvious that the prediction grid still fits inside it. It
#    does: all 1408 cells of pred.polys_m pass in.out() against BOTH boundaries,
#    and predict() returns 1408/1408 finite values for the tuned base model. No
#    masking is needed.
#
# 2. THE off.set INCONSISTENCY -- real, and it is the MAP script that is wrong.
#    UTIL_Map_DSM_output_DD.R predicts with off.set = 800 * trunc.dist_dd
#    (260000 m2, a constant) and then divides by the cell area to get density.
#    But a prediction made with a constant off.set does not depend on cell area
#    at all, so dividing by it does not convert anything -- it just rescales.
#    Measured against the stored soap model:
#
#       off.set = cell_area_m2  reproduces the stored N_hat exactly
#                               (4200.7 vs 4201; ratio 0.9999-1.0002 over 4 combos)
#       the map recipe          understates density by a factor of 4.49
#                               (4.222-4.508 across cells)
#
#    So the published maps are ~4.5x too low on the legend, though the spatial
#    PATTERN survives nearly intact (only 6.8% spread in the factor). The
#    abundance numbers in 5_CommonDolphin_Abundance.R are unaffected and correct.
#    THIS SCRIPT USES off.set = cell_area_m2 THROUGHOUT.
#    UTIL_Map_DSM_output_DD.R is deliberately NOT edited here.
#
# 3. sst ON THE PREDICTION GRID -- climatological, and the season mean is NOT safe.
#    preddata carries one value per cell per calendar month (Mes_n 1-12) with no
#    Ano, i.e. a monthly climatology. Season maps to months exactly as in
#    segdata (Spring 10-12, Summer 1-3, Fall 4-6, Winter 7-9). The naive move is
#    to average sst over the three months of a season and predict once. That is
#    wrong where the smooth is curved, because mean(f(sst)) != f(mean(sst)), and
#    the within-season monthly spread PER CELL is large in two seasons:
#
#       season   median range   p90     max
#       Summer      1.06       1.64    7.19
#       Fall        3.62       4.70    7.08
#       Winter      0.54       1.25    3.62
#       Spring      4.29       4.89    7.82
#
#    So instead of predicting at the mean sst, this script predicts at EACH of
#    the three months and averages the resulting densities -- mean(f(sst)), the
#    quantity actually wanted. It is implemented by stacking the grid three deep
#    with off.set = cell_area/3, which makes dsm_var_gam sum to the month-averaged
#    abundance and propagate variance correctly in a single call.
#
#    CONSEQUENCE TO STATE IN ANY CAPTION: because sst is climatological and has
#    no year dimension, the sst term contributes the SAME thing in every year by
#    construction. All interannual variation in the +sst abundance series still
#    comes from s(Ano). The series is not evidence of sst-driven change.
#
# FOOTPRINT. preddata covers 1353 of the 1408 prediction cells; 55 cells (64.2
# km2, 3.9% of grid area, carrying 0.35% of predicted N) have no covariates and
# therefore no sst. Base and +sst are compared on the COMMON 1353-cell footprint
# so the difference between them is the model, not the domain. The base model is
# additionally reported on the full 1408 cells to bridge to existing figures.
#
# COST. Two fits (~95 s each, cached to .rds) plus 2 models x 33 season-year
# combos of dsm_var_gam. Every abundance row is cached the moment it is produced.
# Delete .cache_tunedmaps_dd/ to force a clean run.
#
# OUTPUT  output/CommonDolphin/DSM/tuned_models/dd_tuned_{base,sst}.rds
#         output/CommonDolphin/DSM/DD_DSM_Tuned_{Base,SST}_Season.png
#         output/CommonDolphin/DSM/DD_DSM_Tuned_BaseVsSST_Season.png
#         output/CommonDolphin/DSM/DD_tuned_density_grid.csv
#         output/CommonDolphin/Abundance/DD_abundance_tuned.csv
#         output/CommonDolphin/Abundance/DD_abundance_tuned.png
#         output/CommonDolphin/Abundance/DD_density_tuned.png
#         output/CommonDolphin/Abundance/DD_abundance_tuned_vs_original.png

library(dsm)
library(mgcv)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(data.table)

source(file.path(here::here(), "R", "dsm_correlogram.R"))
source(file.path(here::here(), "R", "cached_fit_row.R"))
source(file.path(here::here(), "R", "lnorm_ci.R"))

load("output/CommonDolphin/dd_output.RData")

.dsm_dir  <- file.path("output", "CommonDolphin", "DSM")
.ab_dir   <- file.path("output", "CommonDolphin", "Abundance")
.mod_dir  <- file.path(.dsm_dir, "tuned_models")
.cache    <- file.path(.dsm_dir, ".cache_tunedmaps_dd")
for (d in c(.dsm_dir, .ab_dir, .mod_dir, .cache))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

TOL <- 500; MARGIN <- 250; NGRID <- c(14L, 11L); K_BND <- 10L; K_COV <- 20L
REFIT <- FALSE                 # TRUE to force a refit even if the .rds exist

.season_levels <- levels(segdata$season)
.ref_ano       <- as.integer(round(median(segdata$Ano)))

.write <- function(x, file, dir = .dsm_dir) {
  p <- file.path(dir, file)
  tryCatch(fwrite(x, p), error = function(e) {
    alt <- sub("[.]csv$", "_new.csv", p)
    warning(sprintf("could not write %s (locked? Excel?) -- writing %s",
                    basename(p), basename(alt)), call. = FALSE)
    fwrite(x, alt)
  })
}

# ---------------------------------------------------------------------------
# 1. The two tuned models
#
# Same construction as UTIL_DSM_SoapTuning_DD.R. dsm() resolves `knots` by NAME
# and the formula names the boundary, so both are staged in globalenv() under
# distinct names so the workspace's own bnd_soap / knots survive.
#
# Knot clearance is measured to the boundary EDGES (MULTILINESTRING), not its
# vertices. bnd_dist() in 4_CommonDolphin_DSM_soap.R uses vertices, which lets a
# knot sit on a long edge and makes mgcv fail in crunch.knots().
# ---------------------------------------------------------------------------
.gulf0 <- survey.area_m %>% st_geometry() %>% st_union() %>%
  st_cast("MULTIPOLYGON") %>% st_cast("POLYGON")
if (length(.gulf0) > 1) .gulf0 <- .gulf0[which.max(as.numeric(st_area(.gulf0)))]
.survey_area_km2 <- as.numeric(st_area(.gulf0)) / 1e6

.seg_sf <- st_as_sf(segdata, coords = c("x", "y"),
                    crs = st_crs(survey.area_m), remove = FALSE)
.d_out  <- as.numeric(st_distance(.seg_sf, .gulf0))

.build_boundary <- function(tol, margin) {
  bo <- if (any(.d_out > 0)) max(.d_out) + margin else margin
  g <- .gulf0 %>% st_buffer(bo) %>%
    st_simplify(dTolerance = tol, preserveTopology = TRUE) %>% st_cast("POLYGON")
  if (length(g) > 1) g <- g[which.max(as.numeric(st_area(g)))]
  r <- st_coordinates(g)[, c("X", "Y")]
  lp <- list(x = r[, 1], y = r[, 2])
  if (lp$x[1] != tail(lp$x, 1)) { lp$x <- c(lp$x, lp$x[1]); lp$y <- c(lp$y, lp$y[1]) }
  lp
}

.make_knots <- function(loop, ngrid) {
  bm   <- cbind(loop$x, loop$y)
  line <- st_cast(st_sfc(st_polygon(list(bm)), crs = st_crs(survey.area_m)),
                  "MULTILINESTRING")
  kn   <- make.soapgrid(loop, n.grid = ngrid)
  pts  <- st_as_sf(data.frame(x = kn$x, y = kn$y), coords = c("x", "y"),
                   crs = st_crs(survey.area_m))
  keep <- as.logical(in.out(bm, cbind(kn$x, kn$y))) &
          as.numeric(st_distance(pts, line)) > knot_buffer
  data.frame(x = kn$x[keep], y = kn$y[keep])
}

.fit_tuned <- function(rhs, loop, knots_use) {
  assign(".bnd_active",   list(loop), envir = globalenv())
  assign(".knots_active", knots_use,  envir = globalenv())
  rhs <- if (nzchar(rhs)) paste("+", rhs) else ""
  f <- as.formula(sprintf(
    'count ~ s(x, y, bs = "so", xt = list(bnd = .bnd_active), k = %d) %s', K_BND, rhs))
  environment(f) <- globalenv()
  dsm(f, ddf.obj = df.dd, segment.data = segdata, observation.data = obsdata_dd_mod,
      family = tw(link = "log"), method = "REML", knots = .knots_active)
}

.loop  <- .build_boundary(TOL, MARGIN)
.knots <- .make_knots(.loop, NGRID)
cat(sprintf("tuned boundary: %d vertices, %.1f km2 | knots kept: %d\n",
            length(.loop$x), as.numeric(st_area(st_sfc(
              st_polygon(list(cbind(.loop$x, .loop$y))),
              crs = st_crs(survey.area_m)))) / 1e6, nrow(.knots)))
stopifnot(nrow(.knots) == 89L)

.get_model <- function(tag, rhs) {
  f <- file.path(.mod_dir, sprintf("dd_tuned_%s.rds", tag))
  if (!REFIT && file.exists(f)) {
    cat("loading cached fit:", basename(f), "\n"); return(readRDS(f)$model)
  }
  cat("fitting", tag, "...\n"); t0 <- Sys.time()
  m <- .fit_tuned(rhs, .loop, .knots)
  cat("  done in", round(difftime(Sys.time(), t0, units = "secs")), "s\n")
  saveRDS(list(model = m, loop = .loop, knots = .knots), f)
  m
}

m_base <- .get_model("base", "season + s(Ano)")
m_sst  <- .get_model("sst",  sprintf("season + s(Ano) + s(sst, k = %d)", K_COV))

# Refit sanity check -- these are the numbers UTIL_DSM_SoapTuning_DD.R produced.
.d  <- dd.dsm.soap.season.year$data
.sn <- dsm_seg_num(.d$Sample.Label)
.chk <- rbindlist(list(
  data.table(model = "base", AIC = round(AIC(m_base), 2),
             Dev = round(summary(m_base)$dev.expl, 3),
             lag1 = round(dsm_correlogram(m_base, .d$Transect.Label, .sn,
                                          max.lag = 2)[lag == 1, cor], 4),
             AIC_exp = 6070.35, Dev_exp = 0.258, lag1_exp = 0.0247),
  data.table(model = "sst", AIC = round(AIC(m_sst), 2),
             Dev = round(summary(m_sst)$dev.expl, 3),
             lag1 = round(dsm_correlogram(m_sst, .d$Transect.Label, .sn,
                                          max.lag = 2)[lag == 1, cor], 4),
             AIC_exp = 6034.93, Dev_exp = 0.290, lag1_exp = 0.0278)))
cat("\n=== refit sanity check ===\n"); print(.chk)
if (!isTRUE(all.equal(.chk$AIC, .chk$AIC_exp, tolerance = 1e-4)))
  warning("tuned refit does NOT reproduce the stored AIC -- investigate before using.")

# ---------------------------------------------------------------------------
# 2. Prediction grid + the sst climatology
#
# NOTE ON NAMING. pred.polys_m has columns `x` and `y`. Never use `x`, `y`, `a`
# or any other grid column name as a loop variable that is then referenced
# inside mutate()/data.table -- data masking resolves the COLUMN, not the loop
# variable, silently. Setting Ano from a loop variable called `y` binds Ano to
# the northing (~5e6), s(Ano) extrapolates, and every prediction becomes Inf.
# The season/year loop below uses .ssn / .yr for exactly this reason.
# ---------------------------------------------------------------------------
if (!all(c("x", "y") %in% names(pred.polys_m)))
  pred.polys_m <- pred.polys_m %>%
    mutate(x = st_coordinates(st_centroid(geometry))[, 1],
           y = st_coordinates(st_centroid(geometry))[, 2])

.cell_area_m2 <- as.numeric(st_area(pred.polys_m))
.grid <- as.data.table(st_drop_geometry(pred.polys_m))
.grid[, area_m2 := .cell_area_m2]
.grid[, id_int  := suppressWarnings(as.integer(id))]   # id is character here
stopifnot(!anyNA(.grid$id_int))

.pd <- as.data.table(preddata)[, .(id, Mes_n, season, sst)]
stopifnot(is.integer(.pd$id))

.ids_cov <- sort(unique(.pd$id))
.grid[, has_cov := id_int %in% .ids_cov]
cat(sprintf("\nprediction grid: %d cells, %.1f km2\n",
            nrow(.grid), sum(.grid$area_m2) / 1e6))
cat(sprintf("cells WITH covariates: %d (%.1f km2); WITHOUT: %d (%.1f km2, %.1f%%)\n",
            sum(.grid$has_cov),  sum(.grid[has_cov == TRUE,  area_m2]) / 1e6,
            sum(!.grid$has_cov), sum(.grid[has_cov == FALSE, area_m2]) / 1e6,
            100 * sum(.grid[has_cov == FALSE, area_m2]) / sum(.grid$area_m2)))

# every cell is inside the tuned boundary -- assert rather than assume
.bm <- cbind(.loop$x, .loop$y)
stopifnot(all(as.logical(in.out(.bm, cbind(.grid$x, .grid$y)))))

# Month-stacked grid: 3 months per season, off.set = cell_area / 3, so that a
# sum over the stack is the month-AVERAGED abundance, i.e. mean(f(sst)).
.month_grid <- function(ssn, ano, covars_only = TRUE) {
  g <- if (covars_only) .grid[has_cov == TRUE] else .grid
  mo <- .pd[season == ssn, sort(unique(Mes_n))]
  stopifnot(length(mo) == 3L)
  out <- rbindlist(lapply(mo, function(m) {
    gg <- copy(g)
    gg[, Mes_n := m]
    gg[.pd[Mes_n == m], on = .(id_int = id), sst := i.sst]
    gg
  }))
  out[, season := factor(ssn, levels = .season_levels)]
  out[, Ano := as.integer(ano)]
  out[, off := area_m2 / 3]
  if (covars_only) stopifnot(!anyNA(out$sst))
  out[]
}

# ---------------------------------------------------------------------------
# 3. Density maps at the reference year
# ---------------------------------------------------------------------------
.density_by_season <- function(model, use_sst, tag) {
  cached_fit_row(sprintf("map__%s__%s", tag, if (use_sst) "common_1353" else "all_1408"),
                 .cache, {
    rbindlist(lapply(.season_levels, function(ssn) {
      g <- .month_grid(ssn, .ref_ano, covars_only = use_sst)
      p <- predict(model, newdata = as.data.frame(g), off.set = g$off,
                   type = "response")
      stopifnot(all(is.finite(p)))
      g[, Nhat := p]
      # sum the 3 monthly (area/3)-weighted predictions back to one value per cell
      g[, .(Nhat = sum(Nhat), area_m2 = area_m2[1]), by = .(id, x, y)][
        , `:=`(season = factor(ssn, levels = .season_levels),
               density = Nhat / (area_m2 / 1e6))][]
    }))
  })
}

cat("\npredicting density maps at Ano =", .ref_ano, "...\n")
dens_base_full <- .density_by_season(m_base, use_sst = FALSE, tag = "base")  # all 1408
dens_base      <- .density_by_season(m_base, use_sst = TRUE,  tag = "base")  # common 1353
dens_sst       <- .density_by_season(m_sst,  use_sst = TRUE,  tag = "sst")   # common 1353

dens_base_full[, model := "base"]; dens_base[, model := "base"]
dens_sst[, model := "sst"]

.map_dt <- rbindlist(list(dens_base, dens_sst))
.map_sf <- pred.polys_m %>% select(id, geometry) %>%
  left_join(as.data.frame(.map_dt), by = "id") %>%
  filter(!is.na(density))

.bb <- st_bbox(survey.area_m); .xpad <- 3000; .ypad <- 3000
.tuned_poly <- st_sfc(st_polygon(list(cbind(.loop$x, .loop$y))),
                      crs = st_crs(survey.area_m))

#
# TWO COLOUR SCALES ARE PRODUCED FOR EVERY MAP.
# The density field is extremely skewed -- two small hotspots reach ~55 km^-2
# against a median near 2 -- so on a linear scale the whole gulf renders as one
# flat dark field and only the hotspots are legible. The linear version is kept
# because it is the honest depiction of magnitude; the log10 version is the one
# to read spatial structure off. Neither is a substitute for the other.
.map <- function(dat, ttl, cap, fill_lim = NULL, log_scale = FALSE) {
  fill_scale <- if (log_scale) {
    scale_fill_viridis_c(option = "turbo", trans = "log10",
                         name = expression("Dolphins km"^-2),
                         limits = fill_lim,
                         labels = \(v) format(v, drop0trailing = TRUE,
                                              scientific = FALSE))
  } else {
    scale_fill_viridis_c(option = "turbo", name = expression("Dolphins km"^-2),
                         limits = fill_lim)
  }
  ggplot() +
    geom_sf(data = patagonia_m, fill = "grey85", color = "grey40") +
    geom_sf(data = dat, aes(fill = density), color = NA) +
    geom_sf(data = .tuned_poly, fill = NA, color = "black",
            linewidth = 0.6, linetype = "solid") +
    geom_sf(data = survey.area_m, fill = NA, color = "grey30",
            linewidth = 0.4, linetype = "22") +
    geom_sf(data = segdata_traj_m %>% filter(Ano > 2006), size = 0.6, alpha = 0.18) +
    fill_scale +
    labs(title = ttl, caption = cap, x = "Easting (Mm)", y = "Northing (Mm)") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "right", panel.grid.minor = element_blank()) +
    scale_x_continuous(labels = \(v) v / 1e6) +
    scale_y_continuous(labels = \(v) v / 1e6) +
    coord_sf(xlim = c(.bb["xmin"] - .xpad, .bb["xmax"] + .xpad),
             ylim = c(.bb["ymin"] - .ypad, .bb["ymax"] + .ypad),
             default_crs = st_crs(target_crs), datum = target_crs, expand = FALSE) +
    facet_wrap(. ~ season)
}

.lim <- range(.map_sf$density, na.rm = TRUE)   # shared scale: the two are comparable

p.base <- .map(.map_sf %>% filter(model == "base"),
  "Predicted common dolphin density - tuned soap film",
  sprintf(paste0("count ~ s(x,y,bs=\"so\") + season + s(Ano) | tuned boundary ",
                 "tol%g/margin%g, 89 knots | s(Ano) evaluated at %d\n",
                 "solid line = soap boundary, dashed = survey polygon"),
          TOL, MARGIN, .ref_ano), .lim)

p.sst <- .map(.map_sf %>% filter(model == "sst"),
  "Predicted common dolphin density - tuned soap film + s(sst)",
  sprintf(paste0("count ~ s(x,y,bs=\"so\") + season + s(Ano) + s(sst, k=%d) | ",
                 "s(Ano) at %d; density averaged over the 3 months of each season\n",
                 "sst is a monthly CLIMATOLOGY (no year), so the sst term is identical ",
                 "in every year - all interannual change comes from s(Ano)"),
          K_COV, .ref_ano), .lim)

p.both <- .map(.map_sf, "Common dolphin density - tuned soap film: base vs +s(sst)",
  sprintf("rows: base (top) vs +s(sst) | s(Ano) at %d | shared colour scale", .ref_ano),
  .lim) + facet_grid(model ~ season)

ggsave(file.path(.dsm_dir, "DD_DSM_Tuned_Base_Season.png"), p.base, width = 13, height = 13)
ggsave(file.path(.dsm_dir, "DD_DSM_Tuned_SST_Season.png"),  p.sst,  width = 13, height = 13)
ggsave(file.path(.dsm_dir, "DD_DSM_Tuned_BaseVsSST_Season.png"), p.both,
       width = 15, height = 9)

# log10 companions -- same data, readable spatial structure
.loglim <- range(.map_sf$density[.map_sf$density > 0], na.rm = TRUE)
ggsave(file.path(.dsm_dir, "DD_DSM_Tuned_Base_Season_log.png"),
       .map(.map_sf %>% filter(model == "base", density > 0),
            "Predicted common dolphin density - tuned soap film (log scale)",
            sprintf(paste0("count ~ s(x,y,bs=\"so\") + season + s(Ano) | 89 knots | ",
                           "s(Ano) at %d | log10 colour scale"), .ref_ano),
            .loglim, log_scale = TRUE), width = 13, height = 13)
ggsave(file.path(.dsm_dir, "DD_DSM_Tuned_SST_Season_log.png"),
       .map(.map_sf %>% filter(model == "sst", density > 0),
            "Predicted common dolphin density - tuned soap film + s(sst) (log scale)",
            sprintf(paste0("count ~ s(x,y,bs=\"so\") + season + s(Ano) + s(sst, k=%d) | ",
                           "s(Ano) at %d | log10 colour scale\nsst is a monthly ",
                           "CLIMATOLOGY - the sst term is identical in every year"),
                    K_COV, .ref_ano),
            .loglim, log_scale = TRUE), width = 13, height = 13)
ggsave(file.path(.dsm_dir, "DD_DSM_Tuned_BaseVsSST_Season_log.png"),
       .map(.map_sf %>% filter(density > 0),
            "Common dolphin density - tuned soap film: base vs +s(sst) (log scale)",
            sprintf("rows: base (top) vs +s(sst) | s(Ano) at %d | shared log10 scale",
                    .ref_ano),
            .loglim, log_scale = TRUE) + facet_grid(model ~ season),
       width = 15, height = 9)

.write(rbindlist(list(dens_base_full[, footprint := "all_1408"],
                      dens_base[,      footprint := "common_1353"],
                      dens_sst[,       footprint := "common_1353"]), fill = TRUE),
       "DD_tuned_density_grid.csv")

cat("\n=== map density summary (dolphins km^-2, Ano =", .ref_ano, ") ===\n")
print(.map_dt[, .(min = round(min(density), 3), median = round(median(density), 3),
                  max = round(max(density), 3)), by = .(model, season)])

# ---------------------------------------------------------------------------
# 4. Abundance by season x year
#
# dsm_var_gam (NOT dsm_varprop): the models carry no detection covariates, so
# the variance of the detection function is already folded in by the GAM route.
# Every row is cached, keyed on model x footprint x season x year.
# ---------------------------------------------------------------------------
.sy <- unique(as.data.table(segdata)[, .(season, Ano)])
setorder(.sy, Ano, season)
cat(sprintf("\n=== abundance: %d season-year combos x 2 models ===\n", nrow(.sy)))

.abund_row <- function(model, tag, ssn, ano, use_sst, fp) {
  key <- sprintf("ab__%s__%s__%s_%d", tag, fp, ssn, ano)
  cached_fit_row(key, .cache, {
    g  <- .month_grid(ssn, ano, covars_only = use_sst)
    vp <- dsm_var_gam(dsm.obj = model, pred.data = as.data.frame(g),
                      off.set = g$off)
    sm <- summary(vp)
    N  <- as.numeric(sm$pred.est); cv <- as.numeric(sm$cv)
    ci <- lnorm_ci(N, cv)
    data.table(model = tag, footprint = fp, season = ssn, year = ano,
               N_hat = round(N), N_lo95 = round(ci$lo), N_hi95 = round(ci$hi),
               CV = round(cv, 3),
               density    = round(N / .survey_area_km2, 4),
               dens_lo    = round(ci$lo / .survey_area_km2, 4),
               dens_hi    = round(ci$hi / .survey_area_km2, 4))
  })
}

ab <- rbindlist(lapply(seq_len(nrow(.sy)), function(i) {
  .ssn <- as.character(.sy$season[i]); .yr <- as.integer(.sy$Ano[i])
  cat(sprintf("  [%2d/%2d] %-7s %d\n", i, nrow(.sy), .ssn, .yr))
  rbindlist(list(
    .abund_row(m_base, "base", .ssn, .yr, use_sst = FALSE, fp = "all_1408"),
    .abund_row(m_base, "base", .ssn, .yr, use_sst = TRUE,  fp = "common_1353"),
    .abund_row(m_sst,  "sst",  .ssn, .yr, use_sst = TRUE,  fp = "common_1353")))
}))

ab[, season := factor(season, levels = .season_levels)]
ab[, seas := c(Spring = 0.85, Summer = 0.1, Fall = 0.35, Winter = 0.6)[as.character(season)]]
ab[, sy := year + seas]
setorder(ab, model, footprint, sy)
.write(ab, "DD_abundance_tuned.csv", dir = .ab_dir)

# bridge to the stored ORIGINAL soap series
.ab_orig <- tryCatch(
  fread(file.path(.ab_dir, "DD_abundance_season_year_soap.csv")),
  error = function(e) NULL)

# ---------------------------------------------------------------------------
# 5. Abundance plots
# ---------------------------------------------------------------------------
.ab_plot <- as.data.table(ab)[footprint == "common_1353"]
.ab_plot[, model_lab := factor(model, levels = c("base", "sst"),
                               labels = c("base: s(x,y,so)+season+s(Ano)",
                                          "+ s(sst, k=20)"))]

# DO NOT DRAW A LINE ACROSS THE SURVEY GAP.
# There are no surveys in 2011 or 2012 (and 2013 is Summer only), so a
# continuous line from Winter 2010 to Summer 2013 is s(Ano) interpolating over
# three unsampled years -- it reads as a measured trend and is not one. Split
# the series into segments either side of any gap longer than one year and let
# ggplot break the line there.
.gap_after <- 2010L
.ab_plot[, era := ifelse(year <= .gap_after, "2006-2010", "2013-2018")]
.ab_plot[, grp := paste(model_lab, era)]
.surveyed <- sort(unique(.ab_plot$year))
cat("\nsurveyed years:", paste(.surveyed, collapse = ", "),
    "| unsurveyed in range:",
    paste(setdiff(min(.surveyed):max(.surveyed), .surveyed), collapse = ", "), "\n")

.gap_note <- "grey band = no surveys (2011-2012); lines are broken across it"
.gap_rect <- annotate("rect", xmin = 2010.75, xmax = 2013.0, ymin = -Inf, ymax = Inf,
                      fill = "grey80", alpha = 0.35)

p.N <- ggplot(.ab_plot, aes(x = sy, colour = model_lab, fill = model_lab)) +
  .gap_rect +
  geom_ribbon(aes(ymin = N_lo95, ymax = N_hi95, group = grp), alpha = 0.18, colour = NA) +
  geom_line(aes(y = N_hat, group = grp), linewidth = 0.7) +
  geom_point(aes(y = N_hat), size = 1.6) +
  scale_colour_manual(values = c("grey25", "#D55E00"), name = NULL) +
  scale_fill_manual(values   = c("grey25", "#D55E00"), name = NULL) +
  labs(title = "Common dolphin abundance - tuned soap arm",
       subtitle = paste0("ribbon = 95% CI (lognormal) | common 1353-cell footprint | ",
                         "sst is climatological, so all interannual change is s(Ano)"),
       caption = .gap_note,
       x = "Year (offset by season)", y = "Abundance") +
  theme_minimal(base_size = 13) + theme(legend.position = "top")

p.D <- ggplot(.ab_plot, aes(x = sy, colour = model_lab, fill = model_lab)) +
  .gap_rect +
  geom_ribbon(aes(ymin = dens_lo, ymax = dens_hi, group = grp), alpha = 0.18, colour = NA) +
  geom_line(aes(y = density, group = grp), linewidth = 0.7) +
  geom_point(aes(y = density), size = 1.6) +
  scale_colour_manual(values = c("grey25", "#D55E00"), name = NULL) +
  scale_fill_manual(values   = c("grey25", "#D55E00"), name = NULL) +
  labs(title = "Common dolphin density - tuned soap arm",
       subtitle = sprintf("N / %.1f km2 survey area | ribbon = 95%% CI (lognormal)",
                          .survey_area_km2),
       caption = .gap_note,
       x = "Year (offset by season)", y = expression("Dolphins km"^-2)) +
  theme_minimal(base_size = 13) + theme(legend.position = "top")

p.N.season <- p.N + facet_wrap(. ~ season) + aes(x = year) +
  labs(x = "Year", subtitle = "by season | ribbon = 95% CI (lognormal)")

ggsave(file.path(.ab_dir, "DD_abundance_tuned.png"), p.N, width = 13, height = 7)
ggsave(file.path(.ab_dir, "DD_density_tuned.png"),   p.D, width = 13, height = 7)
ggsave(file.path(.ab_dir, "DD_abundance_tuned_byseason.png"), p.N.season,
       width = 13, height = 9)

if (!is.null(.ab_orig)) {
  .cmp <- rbindlist(list(
    .ab_plot[model == "base", .(arm = "tuned base", sy, year, season, N_hat, N_lo95, N_hi95)],
    .ab_plot[model == "sst",  .(arm = "tuned +sst", sy, year, season, N_hat, N_lo95, N_hi95)],
    as.data.table(.ab_orig)[, .(arm = "original base", sy, year,
                                season = factor(season, levels = .season_levels),
                                N_hat, N_lo95, N_hi95)]))
  .cmp[, grp := paste(arm, ifelse(year <= .gap_after, "a", "b"))]
  p.cmp <- ggplot(.cmp, aes(x = sy, colour = arm, fill = arm)) +
    .gap_rect +
    geom_ribbon(aes(ymin = N_lo95, ymax = N_hi95, group = grp), alpha = 0.12, colour = NA) +
    geom_line(aes(y = N_hat, group = grp), linewidth = 0.7) +
    scale_colour_manual(values = c("original base" = "grey45",
                                   "tuned base"    = "#0072B2",
                                   "tuned +sst"    = "#D55E00"), name = NULL) +
    scale_fill_manual(values   = c("original base" = "grey45",
                                   "tuned base"    = "#0072B2",
                                   "tuned +sst"    = "#D55E00"), name = NULL) +
    labs(title = "Common dolphin abundance: original vs tuned soap arm",
         subtitle = "ribbon = 95% CI (lognormal)", caption = .gap_note,
         x = "Year (offset by season)", y = "Abundance") +
    theme_minimal(base_size = 13) + theme(legend.position = "top")
  ggsave(file.path(.ab_dir, "DD_abundance_tuned_vs_original.png"), p.cmp,
         width = 13, height = 7)

  cat("\n=== abundance by arm (mean over the 33 season-year combos) ===\n")
  print(.cmp[, .(mean_N = round(mean(N_hat)), min_N = min(N_hat),
                 max_N = max(N_hat)), by = arm])
}

cat("\n=== footprint effect on the base model ===\n")
print(dcast(ab[model == "base"], season + year ~ footprint, value.var = "N_hat")[
  , .(mean_all_1408 = round(mean(all_1408)),
      mean_common_1353 = round(mean(common_1353)),
      pct_diff = round(100 * (mean(common_1353) / mean(all_1408) - 1), 2))])

cat("\n=== tuned base vs tuned +sst (common footprint) ===\n")
print(dcast(.ab_plot, season + year ~ model, value.var = "N_hat")[
  , .(mean_base = round(mean(base)), mean_sst = round(mean(sst)),
      pct_diff = round(100 * (mean(sst) / mean(base) - 1), 2))])

cat("\ndone.\n")
