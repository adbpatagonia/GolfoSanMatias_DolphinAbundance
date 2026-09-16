# ADB / Claude
# 2026-09-10
# =========================================================================
# STATUS: DECISION RECORD -- NOT RE-RUN BY THE PIPELINE  (marked 2026-09-16)
#
# This is the study that produced the tuned common-dolphin configuration now
# live in 4_CommonDolphin_DSM_soap.R (tol500/margin250, 14x11 -> 89 knots,
# K_COV = 20). It is a decision record for two independent reasons:
#
#   * step 6 stops unless DD_soap_revised_selection.csv exists, and that file is
#     the output of UTIL_DSM_SoapRevised_DD.R, itself a decision record whose
#     `original` arm can no longer be rebuilt;
#   * re-running it would re-derive a conclusion the repo has already adopted,
#     at the cost of a knot sweep that reaches 485 knots.
#
# Its CSVs -- DD_soap_boundary_variants.csv, DD_soap_knot_sweep.csv,
# DD_soap_knot_correlogram.csv, DD_soap_tuned_selection.csv -- are dated
# artefacts of the 2026-09-10 run. Restore them from the quarantine rather than
# expecting 9_RegenerateStudies_DD.R to rebuild them; it deliberately does not
# source this file.
#
# NOTE the LO twin, UTIL_DSM_SoapTuning_LO.R, is NOT a decision record: it reads
# the stored configuration from the workspace instead of hardcoding it, and it
# builds its own `original` arm rather than depending on a prior CSV. That is
# the pattern to copy if this script is ever revived.
# =========================================================================
#
#
# COMMON DOLPHIN soap film: tune the BOUNDARY and the INTERIOR KNOT GRID, then
# refit the candidate set at the tuned configuration and rank it alongside the
# original and revised arms from UTIL_DSM_SoapRevised_DD.R.
#
# WHY THE BOUNDARY COMES FIRST
# The stored boundary is a 9-vertex polygon of 2184.6 km2 against a survey
# polygon of 1811.0 km2 (21 vertices) -- 20.6% larger in area, with every
# vertex sitting ~2.91 km OUTSIDE the survey area. The consequence is that only
# 44 of 6288 segments lie within 2 km of the soap boundary, where 577 lie
# within 2 km of the real survey-polygon edge. A soap film's whole purpose is
# to impose sensible behaviour AT the boundary, so a boundary placed ~3 km
# beyond the data is barely exercising that.
#
# It got there for a good reason: 4_CommonDolphin_DSM_soap.R buffers outward by
# max(distance of any segment outside the polygon) + margin, because a soap
# film errors on data outside its boundary. Inflating the boundary was the safe
# fix. This script asks what a tighter one costs or buys.
#
# The knot grid has to be tuned AFTER the boundary, not before, because
# make.soapgrid() fills the boundary and knot_buffer drops knots near it -- so
# the grid is defined relative to whichever boundary is in force.
#
# WHAT THIS SCRIPT DOES
#   1. Builds a 2x2 of boundary variants: simplify_tol {3000, 500} x
#      margin {2000, 250}, the stored setting being 3000/2000. For each it
#      reports area, vertices, distance from the survey polygon, how many
#      segments sit near the boundary, and -- decisively -- whether every
#      segment is still strictly inside. A variant that leaves any segment
#      outside is DROPPED, because soap would error on it.
#   2. Sweeps the interior knot grid within each surviving boundary, fitting
#      the covariate-free base each time.
#   3. Picks a configuration and refits the full 25-model candidate set there.
#   4. Appends all of it to the selection table from UTIL_DSM_SoapRevised_DD.R
#      so original / revised / tuned rank in ONE ranking.
#
# HOW TO READ THE SWEEP -- use the PLATEAU, not the minimum
# The criterion is the smallest grid at which edf_xy stops rising: that is the
# point where the basis has stopped being the binding constraint. Chasing the
# AIC minimum instead keeps adding knots for diminishing return and eventually
# trips the soap PDE solver. The script auto-picks the lowest-AIC surviving
# configuration but prints the whole sweep so that choice can be overridden --
# set FORCE_CONFIG to a row of the sweep table.
#
# HONEST CAVEAT. Tuning the grid on AIC and then quoting that AIC is a mild
# selection bias. This is the right way to FIND the defect; a headline number
# for a paper should come from the model you would have specified knowing the
# basis was adequate, not from the winner of a sweep.
#
# COST. About 24 base fits for the sweep plus 25 for the tuned arm, at roughly
# 35-120 s each, so on the order of an hour. Every row is cached the moment it
# is produced -- an interrupted run resumes. Delete .cache_soaptune_dd/ to
# force a clean run.
#
# OUTPUT  output/CommonDolphin/DSM/DD_soap_boundary_variants.csv
#                                  DD_soap_knot_sweep.csv
#                                  DD_soap_tuned_selection.csv   (all arms)

library(dsm)
library(mgcv)
library(sf)
library(dplyr)
library(data.table)

source(file.path(here::here(), "R", "dsm_correlogram.R"))
source(file.path(here::here(), "R", "cached_fit_row.R"))

# CAPTURE THE CALLER'S OVERRIDES FIRST, for the same reason UTIL_DSM_Diagnostics.R
# does: anything assigned below would clobber a value the caller set before
# source()ing this file, and load() below can clobber it too. Setting
# STOP_AFTER_SWEEP <- TRUE in the runner and then having the config block below
# reset it to FALSE is exactly how a "sweep only" run spent an hour starting the
# 25-model arm at 485 knots.
.stop_after_sweep <- if (exists("STOP_AFTER_SWEEP")) isTRUE(STOP_AFTER_SWEEP) else FALSE
.force_config     <- if (exists("FORCE_CONFIG")) FORCE_CONFIG else NULL

load("output/CommonDolphin/dd_output.RData")

.diag_dir <- file.path("output", "CommonDolphin", "DSM")
dir.create(.diag_dir, showWarnings = FALSE, recursive = TRUE)
.cache <- file.path(.diag_dir, ".cache_soaptune_dd")

ENV7  <- c("slope", "grad", "sst", "clo", "dist.up", "depth", "VelVert")
K_COV <- 20L                 # covariate basis for the tuned arm
K_BND <- 10L                 # boundary-film dimension; never was the constraint

# boundary 2x2 and the grids to sweep
TOLS    <- c(3000, 500)                # st_simplify dTolerance, m
MARGINS <- c(2000, 250)                # clearance beyond the outermost segment, m
NGRIDS  <- list(c(10L, 8L), c(12L, 9L), c(14L, 11L),
                c(16L, 13L), c(18L, 14L), c(20L, 16L))
# The first sweep never plateaued -- edf_xy was still climbing by 6.2 going into
# 20x16 -- so these are swept on the BEST boundary only, to find where it stops.
# Restricted to one boundary because the boundary comparison is already settled:
# tol500/margin250 wins at every grid, on geometry and on AIC.
NGRIDS_EXTRA <- list(c(23L, 18L), c(26L, 21L), c(30L, 24L))
# NOTE: set FORCE_CONFIG / STOP_AFTER_SWEEP in the CALLING script before
# source()ing this file; they are captured into .force_config /
# .stop_after_sweep above. Do not assign them here -- that is what broke the
# sweep-only run.
#   FORCE_CONFIG e.g. list(tol = 500, margin = 250, ngrid = c(16L, 13L))

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

# mirrors the construction in 4_CommonDolphin_DSM_soap.R, parameterised
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

.area_km2 <- function(g) as.numeric(st_area(g)) / 1e6
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
.write(bnd_tab, "DD_soap_boundary_variants.csv")

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
# by name too, so BOTH have to be staged in the global environment. They are
# given distinct names (.bnd_active / .knots_active) so the workspace's own
# `bnd_soap` and `knots` are never overwritten -- other scripts in the same
# session still need those.
# ---------------------------------------------------------------------------
# KNOT CLEARANCE MUST BE MEASURED TO THE BOUNDARY EDGES, NOT ITS VERTICES.
# bnd_dist() in 4_CommonDolphin_DSM_soap.R takes the minimum distance from a
# candidate knot to the boundary VERTICES. With a 9-vertex boundary the edges
# are tens of km long, so a knot can be far from every vertex and yet sit right
# on -- or just outside -- an edge. That filter passes it, and mgcv then fails
# with "knot <n> is on or outside boundary" inside crunch.knots(). It survived
# in the stored run only because the 10x8 grid happened to miss the edges.
# Distance to the boundary as a LINESTRING is the clearance that was intended.
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
  dsm(f, ddf.obj = df.dd, segment.data = segdata,
      observation.data = obsdata_dd_mod,
      family = tw(link = "log"), method = "REML", knots = .knots_active)
}

.xy_edf <- function(m) {
  s <- m$smooth[[1]]
  c(edf = sum(m$edf[s$first.para:s$last.para]), dim = s$bs.dim)
}

# ---------------------------------------------------------------------------
# 3. Knot-grid sweep, within each usable boundary
# ---------------------------------------------------------------------------
sweep_grid <- CJ(i = seq_len(nrow(bnd_ok)), j = seq_along(NGRIDS), sorted = FALSE)
cat("\n=== knot sweep:", nrow(sweep_grid), "base fits ===\n")

sweep <- rbindlist(lapply(seq_len(nrow(sweep_grid)), function(r) {
  bi <- bnd_ok[sweep_grid$i[r]]; ng <- NGRIDS[[sweep_grid$j[r]]]
  key <- sprintf("sweep__tol%g_m%g_g%dx%d", bi$tol, bi$margin, ng[1], ng[2])
  cached_fit_row(key, .cache, {
    loop <- .build_boundary(bi$tol, bi$margin)$loop
    kn   <- .make_knots(loop, ng)
    m    <- .fit("season + s(Ano)", loop, kn)
    e    <- .xy_edf(m)
    kc   <- dsm_kcheck(m, n.rep = 200)[smooth == "s(x,y)"]
    data.table(tol = bi$tol, margin = bi$margin,
               ngrid = sprintf("%dx%d", ng[1], ng[2]),
               n_knots = nrow(kn), AIC = round(AIC(m), 2),
               df = round(attr(logLik(m), "df"), 2),
               edf_xy = round(unname(e["edf"]), 2),
               k_prime_xy = kc$k_prime, edf_frac_xy = kc$edf_frac,
               Dev = round(summary(m)$dev.expl, 3))
  })
}))
setorder(sweep, tol, margin, n_knots)

# Extend the sweep on the best boundary, compared AT THE SAME GRID.
# Not by max(n_knots): a given n.grid yields a different number of surviving
# knots under each boundary (204 / 206 / 200 at 20x16, since the boundaries
# differ in area and the knot_buffer filter bites differently), so picking the
# row with the most knots silently selects one boundary rather than comparing
# them. It chose tol500/margin2000 -- the boundary with only 2 segments within
# 2 km of it -- purely because its larger area kept two extra knots.
.lastgrid <- sprintf("%dx%d", NGRIDS[[length(NGRIDS)]][1], NGRIDS[[length(NGRIDS)]][2])
.bl <- sweep[ngrid == .lastgrid][which.min(AIC)]
cat(sprintf("\nbest boundary at grid %s: tol %g / margin %g (AIC %.2f)\n",
            .lastgrid, .bl$tol, .bl$margin, .bl$AIC))
if (length(NGRIDS_EXTRA)) {
  cat("=== extending the sweep on that boundary:", length(NGRIDS_EXTRA), "more fits ===\n")
  extra <- rbindlist(lapply(NGRIDS_EXTRA, function(ng) {
    key <- sprintf("sweep__tol%g_m%g_g%dx%d", .bl$tol, .bl$margin, ng[1], ng[2])
    cached_fit_row(key, .cache, {
      loop <- .build_boundary(.bl$tol, .bl$margin)$loop
      kn   <- .make_knots(loop, ng)
      m    <- .fit("season + s(Ano)", loop, kn)
      e    <- .xy_edf(m)
      kc   <- dsm_kcheck(m, n.rep = 200)[smooth == "s(x,y)"]
      data.table(tol = .bl$tol, margin = .bl$margin,
                 ngrid = sprintf("%dx%d", ng[1], ng[2]),
                 n_knots = nrow(kn), AIC = round(AIC(m), 2),
                 df = round(attr(logLik(m), "df"), 2),
                 edf_xy = round(unname(e["edf"]), 2),
                 k_prime_xy = kc$k_prime, edf_frac_xy = kc$edf_frac,
                 Dev = round(summary(m)$dev.expl, 3))
    })
  }))
  sweep <- rbind(sweep, extra)
  setorder(sweep, tol, margin, n_knots)
}

# PLATEAU. edf_frac is NOT the criterion -- it falls as the grid grows simply
# because k' grows faster than edf, so it drops even while the basis is still
# binding. The signal is edf_xy itself flattening: edf_gain per 10 extra knots.
sweep[, edf_gain := round(edf_xy - shift(edf_xy), 2), by = .(tol, margin)]
sweep[, knot_gain := n_knots - shift(n_knots), by = .(tol, margin)]
sweep[, edf_per_10knots := round(10 * edf_gain / knot_gain, 2)]
cat("\n=== knot sweep ===\n")
print(sweep[, .(tol, margin, ngrid, n_knots, AIC, edf_xy, k_prime_xy,
                edf_frac_xy, edf_gain, edf_per_10knots, Dev)])

.plateau <- sweep[tol == .bl$tol & margin == .bl$margin &
                    !is.na(edf_per_10knots) & edf_per_10knots < 1][1]
if (nrow(.plateau) && !is.na(.plateau$ngrid)) {
  cat(sprintf("\nplateau suggestion (first grid gaining < 1 edf per 10 knots): %s\n",
              .plateau$ngrid))
} else {
  cat("\nNO PLATEAU REACHED -- edf_xy is still climbing at the largest grid swept.\n",
      "  The spatial term wants more resolution than any grid tested. Extend\n",
      "  NGRIDS_EXTRA, and re-check the correlogram at whatever grid is chosen:\n",
      "  a surface this flexible is exactly where residual autocorrelation makes\n",
      "  AIC over-select.\n")
}
.write(sweep, "DD_soap_knot_sweep.csv")

# ---------------------------------------------------------------------------
# 4. Pick a configuration
# ---------------------------------------------------------------------------
if (is.null(.force_config)) {
  best <- sweep[which.min(AIC)]
  cat(sprintf("\nauto-picked lowest-AIC configuration: tol %g / margin %g / grid %s (%d knots)\n",
              best$tol, best$margin, best$ngrid, best$n_knots))
  cat("  -- check the sweep above: if edf_xy had already plateaued at a coarser\n")
  cat("     grid, prefer that one and set FORCE_CONFIG.\n")
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
# 5. The 25-model candidate set at the tuned configuration
#    (generated exactly as 4_CommonDolphin_DSM_soap.R does)
# ---------------------------------------------------------------------------
.sp <- function(name, extra, label)
  data.frame(name = name, extra = extra, label = label, stringsAsFactors = FALSE)
spec <- do.call(rbind, c(
  list(
    .sp("dd.dsm.soap",             "",                "count ~ s(x,y,so)"),
    .sp("dd.dsm.soap.season",      "season",          "count ~ s(x,y,so) + season"),
    .sp("dd.dsm.soap.season.year", "season + s(Ano)", "count ~ s(x,y,so) + season + s(Ano)")
  ),
  lapply(ENV7, \(e) .sp(sprintf("dd.dsm.soap.year.season.%s", e),
                        sprintf("season + s(Ano) + s(%s)", e),
                        sprintf("count ~ s(x,y,so) + season + s(Ano) + s(%s)", e))),
  list(.sp("dd.dsm.soap.year", "s(Ano)", "count ~ s(x,y,so) + s(Ano)")),
  lapply(ENV7, \(e) .sp(sprintf("dd.dsm.soap.season.%s", e),
                        sprintf("season + s(%s)", e),
                        sprintf("count ~ s(x,y,so) + season + s(%s)", e))),
  lapply(ENV7, \(e) .sp(sprintf("dd.dsm.soap.year.%s", e),
                        sprintf("s(Ano) + s(%s)", e),
                        sprintf("count ~ s(x,y,so) + s(Ano) + s(%s)", e)))
))
setDT(spec)
spec[, env := {
  hit <- vapply(ENV7, function(e) grepl(sprintf("s(%s)", e), extra, fixed = TRUE),
                logical(1))
  if (any(hit)) ENV7[which(hit)[1]] else NA_character_
}, by = seq_len(nrow(spec))]

# raise k on the seven environmental smooths only, never on s(Ano) (edf 1.00)
.bump_k <- function(extra, k) {
  for (e in ENV7)
    extra <- gsub(sprintf("s(%s)", e), sprintf("s(%s, k = %d)", e, k), extra, fixed = TRUE)
  extra
}

.d  <- dd.dsm.soap.season.year$data
.sn <- dsm_seg_num(.d$Sample.Label)

if (.stop_after_sweep)
  cat("\nSTOP_AFTER_SWEEP is TRUE -- stopping before the 25-model tuned arm.\n")

if (!.stop_after_sweep) {

# The cache key carries the CONFIGURATION, not just the model name. Without it a
# row fitted at one boundary/grid would be silently served for a different one
# the next time the auto-pick moved -- which is exactly what happens when the
# sweep is extended.
.cfgkey <- sprintf("tol%g_m%g_g%s", best$tol, best$margin, best$ngrid)
cat("\n=== tuned arm:", nrow(spec), "models at",
    best$ngrid, "/", nrow(.knots_best), "knots, covariates k =", K_COV,
    "\n    cache key prefix:", .cfgkey, "===\n")
rows_tuned <- rbindlist(lapply(seq_len(nrow(spec)), function(i) {
  s <- spec[i]
  cached_fit_row(sprintf("tuned__%s__%s", .cfgkey, s$name), .cache, {
    m   <- .fit(.bump_k(s$extra, K_COV), .loop_best, .knots_best)
    kc  <- dsm_kcheck(m, n.rep = 400)
    rxy <- kc[smooth == "s(x,y)"]
    rcv <- if (is.na(s$env)) NULL else kc[smooth == sprintf("s(%s)", s$env)]
    ct  <- dsm_correlogram(m, .d$Transect.Label, .sn, max.lag = 1)
    data.table(
      arm = "tuned", name = s$name, model = s$label, env = s$env,
      n = length(m$y), df = round(attr(logLik(m), "df"), 2),
      AIC = round(AIC(m), 2), Dev = round(summary(m)$dev.expl, 3),
      p_hat = if (is.null(m$family$getTheta)) NA_real_
              else round(m$family$getTheta(TRUE), 4),
      edf_xy = round(rxy$edf, 2), k_prime_xy = rxy$k_prime,
      edf_frac_xy = rxy$edf_frac,
      edf_env = if (is.null(rcv)) NA_real_ else round(rcv$edf, 2),
      k_prime_env = if (is.null(rcv)) NA_real_ else rcv$k_prime,
      edf_frac_env = if (is.null(rcv)) NA_real_ else rcv$edf_frac,
      lag1 = round(ct$cor, 4), lag1_band = round(ct$band, 4), lag1_sig = ct$sig)
  })
}))

# ---------------------------------------------------------------------------
# 6. One ranking: original + revised + tuned
# ---------------------------------------------------------------------------
.prev_f <- file.path(.diag_dir, "DD_soap_revised_selection.csv")
if (!file.exists(.prev_f))
  stop("run UTIL_DSM_SoapRevised_DD.R first -- ", basename(.prev_f), " is missing.")
prev <- fread(.prev_f)

sel <- rbind(prev, rows_tuned, fill = TRUE)
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
print(sel[1:15, .(arm, model, df, AIC, deltaAIC, Dev, edf_xy, edf_env, lag1_sig)])
cat("\nbest per arm:\n")
print(sel[, .SD[which.min(AIC)], by = arm,
          .SDcols = c("model", "df", "AIC", "deltaAIC", "Dev")])
.write(sel, "DD_soap_tuned_selection.csv")

cat("\nwrote three CSVs to", .diag_dir, "\n")

}  # end if (!.stop_after_sweep)
