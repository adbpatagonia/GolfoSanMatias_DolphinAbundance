# ADB / Claude
# 2026-09-16
#
# DUSKY DOLPHIN soap film: tune the BOUNDARY and the INTERIOR KNOT GRID, then
# refit the candidate set at the tuned configuration and rank it alongside the
# original arm already in lo_output.RData.
#
# Twin of UTIL_DSM_SoapTuning_DD.R. Read that script's header for the reasoning;
# what follows is only what differs for dusky.
#
# WHY THIS IS WORTH RUNNING EVEN THOUGH SOAP IS NOT THE REPORTED LO MODEL
# The reported dusky model is the factor-smooth arm, lo.dsm.xy.fsyear.season --
# soap is the considered block, not the decision model (that asymmetry with
# common dolphins is deliberate). So nothing here is expected to move a
# published number. It is worth doing anyway for two reasons. First, the DD
# soap arm has just been retuned and adopted; leaving the LO soap arm at the
# untuned 3000/2000/10x8 while presenting both in the same report means the two
# species' soap blocks are no longer comparable, and a reader cannot tell
# whether a DD-LO difference is biology or basis resolution. Second, the
# deltaAIC between the fs arm and the soap arm is part of why fs was chosen for
# dusky; if tuning moves the soap arm materially, that choice deserves
# re-examination rather than inheritance.
#
# PREREQUISITE, ALREADY DONE. bnd_dist() in 4_DuskyDolphin_DSM_soap.R measured
# knot clearance to the boundary VERTICES rather than its edges. That is latent
# at 10x8 but fires as soon as the grid is refined, so it was fixed first, in
# its own commit. .make_knots() below uses the corrected edge distance.
#
# WHAT IS DIFFERENT FROM THE DD SCRIPT
#   * lag-1 residual autocorrelation is computed AT EVERY SWEEP ROW, not only in
#     the tuned arm. For DD the knot decision turned on autocorrelation rather
#     than AIC -- AIC kept falling to 485 knots while lag-1 rose past its band --
#     and that only became visible after a separate pass. Computing it inside
#     the sweep makes the criterion available when the configuration is picked
#     instead of after. LO's band is its own; do not assume DD's 0.026.
#   * There is no LO_soap_revised_selection.csv to append to, because no
#     "revised" arm was ever built for dusky. The ORIGINAL arm is taken from the
#     25 lo.dsm.soap.* objects already in lo_output.RData -- not refitted --
#     with a guard that a refit of one of them reproduces its stored AIC. If
#     that guard trips, the workspace and this script disagree about the
#     boundary and nothing below is interpretable.
#
# HOW TO READ THE SWEEP -- use the PLATEAU, not the minimum. The criterion is
# the smallest grid at which edf_xy stops rising, subject to lag-1 staying
# inside its band. Chasing the AIC minimum keeps adding knots for diminishing
# return, degrades the independence assumption that the AIC itself rests on,
# and eventually trips the soap PDE solver.
#
# HONEST CAVEAT, same as DD. Tuning the grid on AIC and then quoting that AIC is
# a mild selection bias. This is the right way to FIND a basis defect; a
# headline number should come from the model you would have specified knowing
# the basis was adequate, not from the winner of a sweep.
#
# COST. About 24 base fits for the sweep plus 25 for the tuned arm. Every row is
# cached the moment it is produced, so an interrupted run resumes. Delete
# .cache_soaptune_lo/ to force a clean run.
#
# RUNNING IT
#   STOP_AFTER_SWEEP <- TRUE   # sweep only, then stop and look
#   FORCE_CONFIG <- list(tol = 500, margin = 250, ngrid = c(14L, 11L))
#   LO_WORKSPACE <- "path/to/lo_output.RData"   # default: output/DuskyDolphin/
#   source("analysis/UTIL_DSM_SoapTuning_LO.R")
#
# OUTPUT  output/DuskyDolphin/DSM/LO_soap_boundary_variants.csv
#                                 LO_soap_knot_sweep.csv
#                                 LO_soap_tuned_selection.csv   (both arms)
#
# ===========================================================================
# RESULT, 2026-09-16: NOTHING WAS ADOPTED. THE LO SOAP ARM NEEDS NO RETUNE.
#
# Ran in full (20-configuration sweep, then 25 models at tol500/margin250/10x8
# with K_COV = 20 against the 25 stored models). All three knobs came back
# negative, for reasons that are specific and checkable rather than a shrug:
#
#  KNOT GRID -- the LO spatial basis was NEVER BINDING. edf_frac_xy is 0.335 at
#     the stored configuration (16.06 of 48), against 0.64 for the DD arm that
#     did need fixing. Across the whole sweep edf_xy climbs only 15.9 -> 29.3
#     while k_prime goes 49 -> 369: the penalty is doing the work, not the basis
#     ceiling. There is no defect here to repair.
#     Supporting: the entire 20-configuration sweep spans 15.8 AIC (DD's knot
#     refinement ALONE was worth 24), and AIC is NON-MONOTONE in knots -- 12x9
#     is worse than 10x8 in all three boundaries, by 3.4 / 9.2 / 12.8. A finer
#     penalized basis cannot genuinely fit worse, so that spread is REML landing
#     differently and it sets the noise floor for everything else in the table.
#     The stored 40-knot configuration ranks 4th of 20, within 3.06 of the best.
#
#  COVARIATE BASIS -- k = 10 was never the constraint either, and the edf say so
#     directly. Doubling k to 20 moved every environmental smooth by essentially
#     nothing:
#         depth 3.09 -> 3.15   grad 2.06 -> 2.15   slope 2.23 -> 2.36
#         VelVert 1.71 -> 1.71   sst 1.00 -> 1.00   clo 1.00 -> 1.00
#     (DD's s(clo), by contrast, was pinned at 8.25-8.43 of 9.) The apparent
#     within-arm "gains" at k = 20 -- depth +4.25, grad +2.69 -- are NOT the
#     basis: depth swings 6 AIC units while its edf moves 0.06. That is noise of
#     the size the knot sweep already measured. At the stored configuration no
#     covariate beats the base at all; the best is sst at +0.21.
#
#  BOUNDARY -- tol500/margin250 costs 7.32 AIC on the reported model
#     (1005.19 -> 1012.51). AIC cannot really adjudicate this, since 7.32 sits
#     inside the 12.8-unit wobble, but it is consistently negative across grids
#     (3.7-7.4), and the only argument the other way is geometric tidiness.
#     Not worth paying for. NOTE this is the opposite of the DD result, where
#     the tighter boundary was worth ~6 AIC -- so the two species' soap arms now
#     sit at different boundaries ON PURPOSE, each on its own evidence.
#
# SO 4_DuskyDolphin_DSM_soap.R KEEPS 3000/2000/10x8 AND k = 10. The only change
# that arm received is the vertex-vs-edge bnd_dist bug fix, which is a
# correctness fix worth 0.08 AIC and changes the kept-knot count from 41 to 40.
#
# SIDE FINDING, worth more than the tuning was: EVERY model that drops `season`
# has SIGNIFICANT residual autocorrelation (lag-1 0.030-0.081 against a band of
# 0.026), and every model that keeps it is clean (0.010-0.026). That includes
# the nominal AIC winner of the whole table, count ~ s(x,y,so) + s(Ano) + s(sst)
# at 1003.92 -- which is therefore NOT a defensible model, because the residual
# independence its AIC assumes does not hold. Season is doing real work here,
# and the ranking must be read with lag1_sig, not on AIC alone.
#
# This script remains RUNNABLE, unlike the three DD decision records: its guard
# reads simplify_tol / margin / knot_ngrid from the workspace rather than
# hardcoding them, so it re-runs correctly whatever the pipeline is set to. It
# is sourced by 9_RegenerateStudies_LO.R.
# ===========================================================================

library(dsm)
library(mgcv)
library(sf)
library(dplyr)
library(data.table)

source(file.path(here::here(), "R", "dsm_correlogram.R"))
source(file.path(here::here(), "R", "cached_fit_row.R"))

# CAPTURE THE CALLER'S OVERRIDES FIRST, for the same reason UTIL_DSM_Diagnostics.R
# does: anything assigned below would clobber a value the caller set before
# source()ing this file, and load() below can clobber it too.
.stop_after_sweep <- if (exists("STOP_AFTER_SWEEP")) isTRUE(STOP_AFTER_SWEEP) else FALSE
.force_config     <- if (exists("FORCE_CONFIG")) FORCE_CONFIG else NULL
.ws               <- if (exists("LO_WORKSPACE")) LO_WORKSPACE else
                       file.path("output", "DuskyDolphin", "lo_output.RData")

# SKIP THE LOAD IF THE WORKSPACE IS ALREADY IN MEMORY. When this runs standalone
# it needs the .RData; when 9_RegenerateStudies_LO.R sources it the objects are
# already there, and lo_output.RData is 2.1 GB -- re-reading it would add
# minutes and, worse, would restore stale copies over anything the driver had
# already recomputed in this session.
.have_ws <- all(vapply(c("segdata", "obsdata_lo_mod", "df.lo", "survey.area_m",
                         "lo.dsm.soap.season.year"), exists, logical(1)))

if (.have_ws) {
  message("LO workspace already in memory -- skipping load(", .ws, ")")
} else {

if (!file.exists(.ws))
  stop("LO workspace not found: ", .ws,
       "\n  Run analysis/1_DuskyDolphin.R first, or set LO_WORKSPACE.")
message("loading ", .ws, " ...")

# Stash the captured overrides across load(). Capturing before load() is not
# enough on its own: lo_output.RData was written by save.image() (all.names =
# TRUE), so it can itself contain objects named .stop_after_sweep / .force_config
# and load() would silently restore them over what the caller just set. That is
# the same class of bug the drivers fixed by switching to save(list = ls(...)).
# Belt and braces -- re-assert after the load.
.keep <- list(sas = .stop_after_sweep, fc = .force_config, ws = .ws)
load(.ws)
.stop_after_sweep <- .keep$sas
.force_config     <- .keep$fc
.ws               <- .keep$ws

}  # end if (!.have_ws)

source(file.path(here::here(), "analysis", "UTIL_EnsureOutputDirs.R"))

.diag_dir <- file.path("output", "DuskyDolphin", "DSM")
.cache    <- file.path(.diag_dir, ".cache_soaptune_lo")

ENV7  <- c("slope", "grad", "sst", "clo", "dist.up", "depth", "VelVert")
K_COV <- 20L                 # covariate basis for the tuned arm
K_BND <- 10L                 # boundary-film dimension; never was the constraint

# The knot_buffer the pipeline script uses; .make_knots() needs it and load()
# above supplies it, but be explicit rather than depending on the workspace.
if (!exists("knot_buffer")) knot_buffer <- 1000

TOLS    <- c(3000, 500)                # st_simplify dTolerance, m
MARGINS <- c(2000, 250)                # clearance beyond the outermost segment, m
NGRIDS  <- list(c(10L, 8L), c(12L, 9L), c(14L, 11L),
                c(16L, 13L), c(18L, 14L), c(20L, 16L))
# Swept on the best boundary only, once the boundary comparison has settled.
NGRIDS_EXTRA <- list(c(23L, 18L), c(26L, 21L))

.write <- function(x, file) {
  p <- file.path(.diag_dir, file)
  tryCatch(fwrite(x, p), error = function(e) {
    alt <- sub("[.]csv$", "_new.csv", p)
    warning(sprintf("could not write %s (locked? Excel?) -- writing %s",
                    basename(p), basename(alt)), call. = FALSE)
    fwrite(x, alt)
  })
}

# ---------------------------------------------------------------------------
# 1. Boundary variants
# ---------------------------------------------------------------------------
.gulf0 <- survey.area_m %>% st_geometry() %>% st_union() %>%
  st_cast("MULTIPOLYGON") %>% st_cast("POLYGON")
if (length(.gulf0) > 1)
  .gulf0 <- .gulf0[which.max(as.numeric(st_area(.gulf0)))]

.seg_sf <- st_as_sf(segdata, coords = c("x", "y"),
                    crs = st_crs(survey.area_m), remove = FALSE)
.d_out  <- as.numeric(st_distance(.seg_sf, .gulf0))   # 0 inside, >0 outside
cat(sprintf("segments outside the raw survey polygon: %d (worst %.0f m)\n",
            sum(.d_out > 0), max(.d_out)))

# mirrors the construction in 4_DuskyDolphin_DSM_soap.R, parameterised
.build_boundary <- function(tol, margin) {
  buffer_out <- if (any(.d_out > 0)) max(.d_out) + margin else margin
  g <- .gulf0 %>% st_buffer(buffer_out) %>%
    st_simplify(dTolerance = tol, preserveTopology = TRUE) %>%
    st_cast("POLYGON")
  if (length(g) > 1) g <- g[which.max(as.numeric(st_area(g)))]
  ring <- st_coordinates(g)[, c("X", "Y")]
  loop <- list(x = ring[, 1], y = ring[, 2])
  if (loop$x[1] != tail(loop$x, 1)) {
    loop$x <- c(loop$x, loop$x[1]); loop$y <- c(loop$y, loop$y[1])
  }
  list(loop = loop, poly = g, buffer_out = buffer_out)
}

.area_km2   <- function(g) as.numeric(st_area(g)) / 1e6
.survey_km2 <- .area_km2(.gulf0)

bnd_tab <- rbindlist(lapply(seq_len(nrow(CJ(tol = TOLS, margin = MARGINS))), function(i) {
  cc <- CJ(tol = TOLS, margin = MARGINS, sorted = FALSE)[i]
  b  <- .build_boundary(cc$tol, cc$margin)
  bm <- cbind(b$loop$x, b$loop$y)
  inside <- as.logical(in.out(bm, cbind(segdata$x, segdata$y)))
  dline  <- as.numeric(st_distance(
    .seg_sf, st_cast(st_sfc(b$poly, crs = st_crs(survey.area_m)), "MULTILINESTRING")))
  data.table(
    tol = cc$tol, margin = cc$margin,
    stored = cc$tol == 3000 & cc$margin == 2000,
    buffer_out = round(b$buffer_out),
    vertices = length(b$loop$x),
    area_km2 = round(.area_km2(b$poly), 1),
    pct_larger = round(100 * (.area_km2(b$poly) / .survey_km2 - 1), 1),
    seg_outside = sum(!inside),
    seg_within_2km = sum(dline < 2000),
    min_dist_m = round(min(dline)),
    usable = all(inside)
  )
}))
cat(sprintf("\nsurvey polygon: %.1f km2, %d vertices\n",
            .survey_km2, nrow(st_coordinates(.gulf0))))
cat("=== boundary variants ===\n"); print(bnd_tab)
.write(bnd_tab, "LO_soap_boundary_variants.csv")

if (!any(bnd_tab$usable))
  stop("no boundary variant contains every segment; widen MARGINS.")
if (any(!bnd_tab$usable))
  cat("\ndropping unusable variants (segments outside -> soap would error):",
      bnd_tab[usable == FALSE, paste0("tol", tol, "/margin", margin, collapse = ", ")], "\n")
bnd_ok <- bnd_tab[usable == TRUE]

# ---------------------------------------------------------------------------
# 2. Fit helpers
#
# dsm() resolves `knots` by NAME, and the formula string refers to the boundary
# by name too, so BOTH have to be staged in the global environment. Distinct
# names (.bnd_active / .knots_active) so the workspace's own bnd_soap and knots
# are never overwritten -- other scripts in the same session still need those.
#
# KNOT CLEARANCE IS MEASURED TO THE BOUNDARY EDGES, NOT ITS VERTICES -- see the
# prerequisite note in the header.
# ---------------------------------------------------------------------------
.make_knots <- function(loop, ngrid) {
  bm   <- cbind(loop$x, loop$y)
  line <- st_cast(st_sfc(st_polygon(list(bm)), crs = st_crs(survey.area_m)),
                  "MULTILINESTRING")
  kn   <- make.soapgrid(loop, n.grid = ngrid)
  pts  <- st_as_sf(data.frame(x = kn$x, y = kn$y), coords = c("x", "y"),
                   crs = st_crs(survey.area_m))
  d    <- as.numeric(st_distance(pts, line))
  keep <- as.logical(in.out(bm, cbind(kn$x, kn$y))) & d > knot_buffer
  data.frame(x = kn$x[keep], y = kn$y[keep])
}

.fit <- function(rhs, loop, knots_use) {
  assign(".bnd_active",   list(loop),  envir = globalenv())
  assign(".knots_active", knots_use,   envir = globalenv())
  rhs <- if (nzchar(rhs)) paste("+", rhs) else ""
  f <- as.formula(sprintf(
    'count ~ s(x, y, bs = "so", xt = list(bnd = .bnd_active), k = %d) %s',
    K_BND, rhs))
  environment(f) <- globalenv()
  dsm(f, ddf.obj = df.lo, segment.data = segdata,
      observation.data = obsdata_lo_mod,
      family = tw(link = "log"), method = "REML", knots = .knots_active)
}

.xy_edf <- function(m) {
  s <- m$smooth[[1]]
  c(edf = sum(m$edf[s$first.para:s$last.para]), dim = s$bs.dim)
}

# Segment ordering for the correlogram. NEVER sort Sample.Label as character:
# the labels are zero-padded to 2 digits against 166 segments, so "10" sorts
# before "9". dsm_seg_num() extracts the numeric part.
.d  <- lo.dsm.soap.season.year$data
.sn <- dsm_seg_num(.d$Sample.Label)

.lag1 <- function(m) {
  ct <- dsm_correlogram(m, .d$Transect.Label, .sn, max.lag = 1)
  list(cor = round(ct$cor, 4), band = round(ct$band, 4), sig = ct$sig)
}

# ---------------------------------------------------------------------------
# 2b. GUARD: does this script's reconstruction reproduce the stored fit?
#     The original arm below is NOT refitted -- it is read out of the workspace
#     -- so the two must agree about what "original" means before anything is
#     compared. Refit one model at the stored configuration and check.
# ---------------------------------------------------------------------------
#     The stored configuration is read FROM THE WORKSPACE (simplify_tol, margin
#     and knot_ngrid are plain globals assigned by 4_DuskyDolphin_DSM_soap.R and
#     so travel inside lo_output.RData) rather than hardcoded. That matters: the
#     moment any of those knobs is adopted into the pipeline, a hardcoded
#     3000/2000/c(10,8) would reconstruct a configuration the workspace no
#     longer uses, the guard would trip, and this script would become
#     un-runnable -- which is exactly what happened to the three DD studies.
#     Reading the knobs keeps it a live study instead of a decision record.
.AIC_STORED <- round(AIC(lo.dsm.soap.season.year), 2)
.tol_st     <- if (exists("simplify_tol")) simplify_tol else 3000
.mar_st     <- if (exists("margin"))       margin       else 2000
.ng_st      <- if (exists("knot_ngrid"))   as.integer(knot_ngrid) else c(10L, 8L)
cat(sprintf("\nguard: workspace was fitted at tol %g / margin %g / grid %dx%d\n",
            .tol_st, .mar_st, .ng_st[1], .ng_st[2]))
.loop_st    <- .build_boundary(.tol_st, .mar_st)$loop
.knots_st   <- .make_knots(.loop_st, .ng_st)
cat(sprintf("\nguard: stored config reconstructs to %d knots (workspace has %s)\n",
            nrow(.knots_st),
            if (exists("knots")) as.character(nrow(knots)) else "unknown"))
.m_guard   <- .fit("season + s(Ano)", .loop_st, .knots_st)
.AIC_GUARD <- round(AIC(.m_guard), 2)
cat(sprintf("guard: refit AIC %.2f vs stored %.2f (diff %+.2f)\n",
            .AIC_GUARD, .AIC_STORED, .AIC_GUARD - .AIC_STORED))
if (abs(.AIC_GUARD - .AIC_STORED) > 1)
  stop(sprintf(paste0("reconstruction does NOT reproduce the stored soap fit ",
                      "(%.2f vs %.2f). The workspace was fitted at a different ",
                      "boundary/knot configuration than this script rebuilds, so ",
                      "the original arm below is not comparable. Investigate ",
                      "before trusting anything downstream."),
               .AIC_GUARD, .AIC_STORED))

# ---------------------------------------------------------------------------
# 3. Knot-grid sweep, within each usable boundary
# ---------------------------------------------------------------------------
.sweep_row <- function(tol, margin, ng) {
  key <- sprintf("sweep__tol%g_m%g_g%dx%d", tol, margin, ng[1], ng[2])
  cached_fit_row(key, .cache, {
    loop <- .build_boundary(tol, margin)$loop
    kn   <- .make_knots(loop, ng)
    m    <- .fit("season + s(Ano)", loop, kn)
    e    <- .xy_edf(m)
    kc   <- dsm_kcheck(m, n.rep = 200)[smooth == "s(x,y)"]
    ct   <- .lag1(m)
    data.table(tol = tol, margin = margin,
               ngrid = sprintf("%dx%d", ng[1], ng[2]),
               n_knots = nrow(kn), AIC = round(AIC(m), 2),
               df = round(attr(logLik(m), "df"), 2),
               edf_xy = round(unname(e["edf"]), 2),
               k_prime_xy = kc$k_prime, edf_frac_xy = kc$edf_frac,
               lag1 = ct$cor, lag1_band = ct$band, lag1_sig = ct$sig,
               Dev = round(summary(m)$dev.expl, 3))
  })
}

sweep_grid <- CJ(i = seq_len(nrow(bnd_ok)), j = seq_along(NGRIDS), sorted = FALSE)
cat("\n=== knot sweep:", nrow(sweep_grid), "base fits ===\n")
sweep <- rbindlist(lapply(seq_len(nrow(sweep_grid)), function(r) {
  bi <- bnd_ok[sweep_grid$i[r]]
  .sweep_row(bi$tol, bi$margin, NGRIDS[[sweep_grid$j[r]]])
}))
setorder(sweep, tol, margin, n_knots)

# Extend on the best boundary, compared AT THE SAME GRID -- not by max(n_knots),
# which silently selects whichever boundary happened to keep more knots.
.lastgrid <- sprintf("%dx%d", NGRIDS[[length(NGRIDS)]][1], NGRIDS[[length(NGRIDS)]][2])
.bl <- sweep[ngrid == .lastgrid][which.min(AIC)]
cat(sprintf("\nbest boundary at grid %s: tol %g / margin %g (AIC %.2f)\n",
            .lastgrid, .bl$tol, .bl$margin, .bl$AIC))
if (length(NGRIDS_EXTRA)) {
  cat("=== extending the sweep on that boundary:", length(NGRIDS_EXTRA), "more fits ===\n")
  sweep <- rbind(sweep, rbindlist(lapply(NGRIDS_EXTRA, function(ng)
    .sweep_row(.bl$tol, .bl$margin, ng))))
  setorder(sweep, tol, margin, n_knots)
}

# PLATEAU. edf_frac is NOT the criterion -- it falls as the grid grows simply
# because k_prime grows faster than edf. The signal is edf_xy itself flattening.
sweep[, edf_gain  := round(edf_xy - shift(edf_xy), 2), by = .(tol, margin)]
sweep[, knot_gain := n_knots - shift(n_knots), by = .(tol, margin)]
sweep[, edf_per_10knots := round(10 * edf_gain / knot_gain, 2)]
sweep[, lag1_ok := lag1 < lag1_band]
cat("\n=== knot sweep ===\n")
print(sweep[, .(tol, margin, ngrid, n_knots, AIC, edf_xy, k_prime_xy,
                edf_frac_xy, edf_per_10knots, lag1, lag1_band, lag1_ok, Dev)])

.plateau <- sweep[tol == .bl$tol & margin == .bl$margin &
                    !is.na(edf_per_10knots) & edf_per_10knots < 1][1]
if (nrow(.plateau) && !is.na(.plateau$ngrid)) {
  cat(sprintf("\nplateau suggestion (first grid gaining < 1 edf per 10 knots): %s\n",
              .plateau$ngrid))
} else {
  cat("\nNO PLATEAU REACHED -- edf_xy is still climbing at the largest grid swept.\n",
      "  Extend NGRIDS_EXTRA, and read the lag1 column before going further: a\n",
      "  surface this flexible is exactly where residual autocorrelation makes\n",
      "  AIC over-select, which is what capped the DD grid at 89 knots.\n")
}
.finest_ok <- sweep[tol == .bl$tol & margin == .bl$margin & lag1_ok == TRUE][
  which.max(n_knots)]
if (nrow(.finest_ok))
  cat(sprintf("finest grid with lag-1 still inside its band: %s (%d knots, lag1 %.4f / %.4f)\n",
              .finest_ok$ngrid, .finest_ok$n_knots, .finest_ok$lag1, .finest_ok$lag1_band))
.write(sweep, "LO_soap_knot_sweep.csv")

# ---------------------------------------------------------------------------
# 4. Pick a configuration
# ---------------------------------------------------------------------------
if (is.null(.force_config)) {
  best <- sweep[which.min(AIC)]
  cat(sprintf("\nauto-picked lowest-AIC configuration: tol %g / margin %g / grid %s (%d knots)\n",
              best$tol, best$margin, best$ngrid, best$n_knots))
  cat("  -- this is a STARTING POINT, not the decision. If edf_xy had already\n")
  cat("     plateaued at a coarser grid, or lag1_ok is FALSE here, prefer the\n")
  cat("     coarser configuration and set FORCE_CONFIG.\n")
} else {
  best <- sweep[tol == .force_config$tol & margin == .force_config$margin &
                  ngrid == sprintf("%dx%d", .force_config$ngrid[1], .force_config$ngrid[2])]
  if (!nrow(best)) stop("FORCE_CONFIG does not match any swept configuration")
  cat("\nusing FORCE_CONFIG:", best$ngrid, "at tol", best$tol, "margin", best$margin, "\n")
}
.loop_best  <- .build_boundary(best$tol, best$margin)$loop
.ngrid_best <- as.integer(strsplit(best$ngrid, "x")[[1]])
.knots_best <- .make_knots(.loop_best, .ngrid_best)

# ---------------------------------------------------------------------------
# 5. The 25-model candidate set, generated exactly as 4_DuskyDolphin_DSM_soap.R
# ---------------------------------------------------------------------------
.sp <- function(name, extra, label)
  data.frame(name = name, extra = extra, label = label, stringsAsFactors = FALSE)
spec <- do.call(rbind, c(
  list(
    .sp("lo.dsm.soap",             "",                "count ~ s(x,y,so)"),
    .sp("lo.dsm.soap.season",      "season",          "count ~ s(x,y,so) + season"),
    .sp("lo.dsm.soap.season.year", "season + s(Ano)", "count ~ s(x,y,so) + season + s(Ano)")
  ),
  lapply(ENV7, \(e) .sp(sprintf("lo.dsm.soap.year.season.%s", e),
                        sprintf("season + s(Ano) + s(%s)", e),
                        sprintf("count ~ s(x,y,so) + season + s(Ano) + s(%s)", e))),
  list(.sp("lo.dsm.soap.year", "s(Ano)", "count ~ s(x,y,so) + s(Ano)")),
  lapply(ENV7, \(e) .sp(sprintf("lo.dsm.soap.season.%s", e),
                        sprintf("season + s(%s)", e),
                        sprintf("count ~ s(x,y,so) + season + s(%s)", e))),
  lapply(ENV7, \(e) .sp(sprintf("lo.dsm.soap.year.%s", e),
                        sprintf("s(Ano) + s(%s)", e),
                        sprintf("count ~ s(x,y,so) + s(Ano) + s(%s)", e)))
))
setDT(spec)
spec[, env := {
  hit <- vapply(ENV7, function(e) grepl(sprintf("s(%s)", e), extra, fixed = TRUE),
                logical(1))
  if (any(hit)) ENV7[which(hit)[1]] else NA_character_
}, by = seq_len(nrow(spec))]

# raise k on the seven environmental smooths only, never on s(Ano)
.bump_k <- function(extra, k) {
  for (e in ENV7)
    extra <- gsub(sprintf("s(%s)", e), sprintf("s(%s, k = %d)", e, k), extra, fixed = TRUE)
  extra
}

.row_from_model <- function(m, arm, s) {
  kc  <- dsm_kcheck(m, n.rep = 400)
  rxy <- kc[smooth == "s(x,y)"]
  rcv <- if (is.na(s$env)) NULL else kc[smooth == sprintf("s(%s)", s$env)]
  ct  <- .lag1(m)
  data.table(
    arm = arm, name = s$name, model = s$label, env = s$env,
    n = length(m$y), df = round(attr(logLik(m), "df"), 2),
    AIC = round(AIC(m), 2), Dev = round(summary(m)$dev.expl, 3),
    p_hat = if (is.null(m$family$getTheta)) NA_real_
            else round(m$family$getTheta(TRUE), 4),
    edf_xy = round(rxy$edf, 2), k_prime_xy = rxy$k_prime,
    edf_frac_xy = rxy$edf_frac,
    edf_env = if (is.null(rcv)) NA_real_ else round(rcv$edf, 2),
    k_prime_env = if (is.null(rcv)) NA_real_ else rcv$k_prime,
    edf_frac_env = if (is.null(rcv)) NA_real_ else rcv$edf_frac,
    lag1 = ct$cor, lag1_band = ct$band, lag1_sig = ct$sig)
}

if (.stop_after_sweep)
  cat("\nSTOP_AFTER_SWEEP is TRUE -- stopping before the 25-model tuned arm.\n")

if (!.stop_after_sweep) {

# ORIGINAL arm: read from the workspace, NOT refitted. The guard in 2b has
# already established that this script's reconstruction of the stored
# configuration reproduces the stored AIC, so the two arms are comparable.
cat("\n=== original arm: reading", nrow(spec), "stored models from the workspace ===\n")
rows_orig <- rbindlist(lapply(seq_len(nrow(spec)), function(i) {
  s <- spec[i]
  if (!exists(s$name)) {
    warning("stored model missing from the workspace: ", s$name, call. = FALSE)
    return(NULL)
  }
  cached_fit_row(sprintf("orig__%s", s$name), .cache,
                 .row_from_model(get(s$name), "original", s))
}), fill = TRUE)

# TUNED arm. The cache key carries the CONFIGURATION, not just the model name:
# without it a row fitted at one boundary/grid would be silently served for a
# different one the next time the pick moved.
.cfgkey <- sprintf("tol%g_m%g_g%s", best$tol, best$margin, best$ngrid)
cat("\n=== tuned arm:", nrow(spec), "models at", best$ngrid, "/",
    nrow(.knots_best), "knots, covariates k =", K_COV,
    "\n    cache key prefix:", .cfgkey, "===\n")
rows_tuned <- rbindlist(lapply(seq_len(nrow(spec)), function(i) {
  s <- spec[i]
  cached_fit_row(sprintf("tuned__%s__%s", .cfgkey, s$name), .cache,
                 .row_from_model(.fit(.bump_k(s$extra, K_COV),
                                      .loop_best, .knots_best), "tuned", s))
}), fill = TRUE)

# ---------------------------------------------------------------------------
# 6. One ranking: original + tuned
# ---------------------------------------------------------------------------
sel <- rbind(rows_orig, rows_tuned, fill = TRUE)
if (uniqueN(sel$n) > 1L)
  stop("models fitted to different numbers of segments (",
       paste(sort(unique(sel$n)), collapse = " / "), ") -- AIC not comparable.")
if (any(!is.finite(sel$AIC)))
  stop("non-finite AIC in: ", paste(sel[!is.finite(AIC), name], collapse = ", "))

sel[, deltaAIC      := round(AIC - min(AIC), 2)]
sel[, deltaAIC_spec := round(AIC - min(AIC), 2), by = arm]
setorder(sel, deltaAIC)
setcolorder(sel, c("arm", "model", "name", "env", "df", "AIC",
                   "deltaAIC", "deltaAIC_spec", "Dev"))
cat("\n=== combined selection table: top 15 of", nrow(sel), "===\n")
print(sel[1:min(15, nrow(sel)),
          .(arm, model, df, AIC, deltaAIC, Dev, edf_xy, edf_env, lag1, lag1_sig)])
cat("\nbest per arm:\n")
print(sel[, .SD[which.min(AIC)], by = arm,
          .SDcols = c("model", "df", "AIC", "deltaAIC", "Dev", "lag1", "lag1_sig")])

.rep <- sel[name == "lo.dsm.soap.season.year"]
if (nrow(.rep) == 2L)
  cat(sprintf("\nreported soap model, original -> tuned: AIC %.2f -> %.2f (%+.2f), Dev %.3f -> %.3f\n",
              .rep[arm == "original", AIC], .rep[arm == "tuned", AIC],
              .rep[arm == "tuned", AIC] - .rep[arm == "original", AIC],
              .rep[arm == "original", Dev], .rep[arm == "tuned", Dev]))

.write(sel, "LO_soap_tuned_selection.csv")
cat("\nwrote three CSVs to", .diag_dir, "\n")

}  # end if (!.stop_after_sweep)
