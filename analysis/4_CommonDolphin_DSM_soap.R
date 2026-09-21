# ADB / Claude
# 2026-07-07
#
# Soap-film DSM candidate set + model-selection table — COMMON dolphins.
# Mirrors the candidate set in 4_CommonDolphin_DSM.R, but the spatial term is a
# SOAP-FILM smoother  s(x, y, bs = "so")  (edge-effect control) instead of the
# thin-plate s(x, y).  Temporal term = s(Ano); covariates = the 7 environmental
# smooths.  The fs year-varying model is NOT included — soap films cannot be used
# as a factor-smooth ("fs") marginal.
#
# WARNING: this fits 25 soap-film GAMs over all segments. Soap films are SLOW;
# expect minutes to tens of minutes. Confirm the base soap fit works (no
# 'NA/NaN/Inf in soap.basis') before running the whole set — if a knot trips the
# PDE grid, coarsen knot_ngrid / raise knot_buffer / hand-place knots.
#
# Assumes in the workspace:
#   df.dd, segdata (x, y, Ano + env covars depth/slope/grad/sst/clo/dist.up/VelVert),
#   obsdata_dd_mod, trunc.dist_dd, survey.area_m, target_crs

library(dsm)
library(mgcv)
library(sf)
library(dplyr)

# lag-1 residual autocorrelation, appended to both selection tables below.
source(file.path(here::here(), "R", "dsm_correlogram.R"))

# ============================================================
# Soap boundary (buffered so ALL segments sit inside) + interior knots
# ============================================================
# TUNED CONFIGURATION (set 2026-09-16; see UTIL_DSM_SoapTuning_DD.R for the
# evidence and analysis/TODO_DD_TunedArm_MapsAbundance.md for the summary).
# The previous values were simplify_tol = 3000, margin = 2000, knot_ngrid =
# c(10, 8) -> 41 knots, and the seven environmental smooths at mgcv's default
# k = 10. Three things changed, and they are independent:
#
#  KNOB 1  boundary: 3000/2000 -> 500/250.
#     The old boundary was a 9-vertex polygon of 2184.6 km2 against a survey
#     polygon of 1811.0 km2, every vertex ~2.91 km OUTSIDE the survey area, so
#     only 44 of 6288 segments sat within 2 km of it. A soap film exists to
#     impose behaviour AT its boundary, so one placed ~3 km beyond the data was
#     barely doing its job. The new boundary is 19 vertices / 2012.0 km2 with
#     247 segments within 2 km, and still contains every segment (checked below).
#     Worth ~6 AIC on the reported model.
#
#  KNOB 2  interior knots: c(10, 8) -> c(14, 11), i.e. 41 -> 89 knots.
#     Worth ~24 AIC on the reported model, the largest of the three gains.
#     DO NOT REFINE FURTHER. AIC keeps falling all the way to 485 knots (-132
#     in total) but residual lag-1 autocorrelation rises monotonically with it
#     (0.0215 -> 0.0441 against a band of 0.026): the independence assumption
#     underwriting those AIC gains degrades exactly as the claimed gain grows.
#     89 knots is the most refined grid still inside the band (lag-1 0.0247, at
#     95% of it). See UTIL_DSM_SoapTuning_DD.R / DD_soap_knot_sweep.csv.
#
#  KNOB 3  covariate basis: the seven environmental smooths go to k = 20 (K_COV
#     below). s(Ano) is DELIBERATELY left at the default -- it comes out at edf
#     1.00 of 9, so raising it is meaningless. k = 20 is ample: s(sst) in the
#     tuned fit uses edf 10.04 of k_prime = 19.
#
# Reference values for the reported model count ~ s(x,y,so) + season + s(Ano):
#     AIC 6070.35, Dev 0.258, lag-1 0.0247   (was AIC 6100.53, Dev 0.218)
simplify_tol <- 500         ## TUNE  metres; larger = simpler (safer) boundary
margin       <- 250         ## TUNE  metres clearance to leave inside the edge
knot_ngrid   <- c(14, 11)   ## TUNE  interior-knot grid density -> 89 knots
knot_buffer  <- 1000        ## TUNE  metres; knots this close to the edge are dropped
K_COV        <- 20          ## TUNE  basis for the 7 environmental smooths (knob 3)
K_BND        <- 10          ## boundary-film dimension; never was the constraint

.n_knots_expected <- 89L    # assertion below; update if any knob above changes

gulf0 <- survey.area_m %>%
  st_geometry() %>% st_union() %>%
  st_cast("MULTIPOLYGON") %>% st_cast("POLYGON")
if (length(gulf0) > 1) gulf0 <- gulf0[which.max(as.numeric(st_area(gulf0)))]

seg_sf <- st_as_sf(segdata, coords = c("x", "y"),
                   crs = st_crs(survey.area_m), remove = FALSE)
d_out  <- as.numeric(st_distance(seg_sf, gulf0))       # 0 if inside/on, >0 if out
buffer_out <- if (any(d_out > 0)) max(d_out) + margin else margin
message(sprintf("segments outside raw polygon: %d (max %.0f m) -> buffering out %.0f m",
                sum(d_out > 0), max(d_out), buffer_out))

gulf <- gulf0 %>%
  st_buffer(buffer_out) %>%
  st_simplify(dTolerance = simplify_tol, preserveTopology = TRUE) %>%
  st_cast("POLYGON")
if (length(gulf) > 1) gulf <- gulf[which.max(as.numeric(st_area(gulf)))]

ring <- st_coordinates(gulf)[, c("X", "Y")]
bnd_loop <- list(x = ring[, 1], y = ring[, 2])
if (bnd_loop$x[1] != tail(bnd_loop$x, 1)) {
  bnd_loop$x <- c(bnd_loop$x, bnd_loop$x[1])
  bnd_loop$y <- c(bnd_loop$y, bnd_loop$y[1])
}
bnd_soap <- list(bnd_loop)                             # xt$bnd is a LIST OF LOOPS
bmat     <- cbind(bnd_loop$x, bnd_loop$y)

# KNOT CLEARANCE IS MEASURED TO THE BOUNDARY EDGES, NOT ITS VERTICES.
# The previous bnd_dist() took the minimum distance from a candidate knot to the
# boundary VERTICES. With a 9-vertex boundary the edges are tens of km long, so a
# knot could be far from every vertex and still sit on -- or just outside -- an
# edge; the filter passed it and mgcv then failed inside crunch.knots() with
# "knot <n> is on or outside boundary". It survived only because the old 10x8
# grid happened to miss the edges. The 14x11 grid does not, so this had to be
# fixed for knob 2 to run at all. Distance to the boundary as a LINESTRING is the
# clearance that was intended all along.
bnd_line <- st_cast(st_sfc(st_polygon(list(bmat)), crs = st_crs(survey.area_m)),
                    "MULTILINESTRING")

bnd_dist <- function(px, py) {
  pts <- st_as_sf(data.frame(x = px, y = py), coords = c("x", "y"),
                  crs = st_crs(survey.area_m))
  as.numeric(st_distance(pts, bnd_line))
}

# interior knots, strictly inside + buffered off the edge
kn   <- make.soapgrid(bnd_loop, n.grid = knot_ngrid)
keep <- as.logical(in.out(bmat, cbind(kn$x, kn$y))) & bnd_dist(kn$x, kn$y) > knot_buffer
knots <- data.frame(x = kn$x[keep], y = kn$y[keep])
message(sprintf("soap knots: %d generated, %d kept", length(kn$x), nrow(knots)))

# Fail loudly rather than silently fitting a differently-resolved surface: the
# whole tuned configuration is calibrated to this knot count.
if (nrow(knots) != .n_knots_expected)
  stop(sprintf(paste0("soap knots: expected %d, got %d. The boundary/knot knobs ",
                      "at the top of this script have changed, so the stored AIC ",
                      "reference values no longer apply -- re-run ",
                      "UTIL_DSM_SoapTuning_DD.R before trusting any output."),
               .n_knots_expected, nrow(knots)))

# sanity: every segment must be strictly inside (soap errors otherwise)
seg_in <- as.logical(in.out(bmat, cbind(segdata$x, segdata$y)))
if (!all(seg_in))
  warning(sprintf("%d/%d segments still outside boundary — raise `margin`.",
                  sum(!seg_in), length(seg_in)))

# ============================================================
# Fit helper + candidate set (soap spatial term shared by all)
# ============================================================
soap_term <- sprintf('s(x, y, bs = "so", xt = list(bnd = bnd_soap), k = %d)', K_BND)

# KNOB 3. The seven environmental smooths are built at k = K_COV; s(Ano) and the
# parametric season term are untouched. Kept as a helper so the basis appears in
# exactly one place and cannot drift between the 21 covariate models.
.s_cov <- function(v) sprintf("s(%s, k = %d)", v, K_COV)

fit_soap <- function(extra = "") {
  rhs  <- if (nzchar(extra)) paste(soap_term, "+", extra) else soap_term
  form <- as.formula(paste("count ~", rhs))
  environment(form) <- globalenv()                     # so bnd_soap resolves
  dsm(form,
      ddf.obj          = df.dd,
      segment.data     = segdata,
      observation.data = obsdata_dd_mod,
      family           = tw(link = "log"),
      method           = "REML",
      knots            = knots)
}

# name / extra terms / label — mirrors the 4_CommonDolphin_DSM.R set, soap spatial.
# GENERATED rather than three hand-maintained parallel vectors: previously a
# covariate could only be added by editing name/extra/label in three places, and
# a single omission silently paired one model's formula with another's label.
.env7 <- c("slope", "grad", "sst", "clo", "dist.up", "depth", "VelVert")

.sp <- function(name, extra, label)
  data.frame(name = name, extra = extra, label = label, stringsAsFactors = FALSE)

spec <- do.call(rbind, c(
  list(
    .sp("dd.dsm.soap",             "",                "count ~ s(x,y,so)"),
    .sp("dd.dsm.soap.season",      "season",          "count ~ s(x,y,so) + season"),
    .sp("dd.dsm.soap.season.year", "season + s(Ano)", "count ~ s(x,y,so) + season + s(Ano)")
  ),
  lapply(.env7, \(e) .sp(sprintf("dd.dsm.soap.year.season.%s", e),
                         sprintf("season + s(Ano) + %s", .s_cov(e)),
                         sprintf("count ~ s(x,y,so) + season + s(Ano) + s(%s)", e))),
  list(
    .sp("dd.dsm.soap.year", "s(Ano)", "count ~ s(x,y,so) + s(Ano)")
  ),
  lapply(.env7, \(e) .sp(sprintf("dd.dsm.soap.season.%s", e),
                         sprintf("season + %s", .s_cov(e)),
                         sprintf("count ~ s(x,y,so) + season + s(%s)", e))),
  lapply(.env7, \(e) .sp(sprintf("dd.dsm.soap.year.%s", e),
                         sprintf("s(Ano) + %s", .s_cov(e)),
                         sprintf("count ~ s(x,y,so) + s(Ano) + s(%s)", e)))
))

# fit all (slow!) and expose each as a named object
dd_soap_models <- lapply(spec$extra, fit_soap)
names(dd_soap_models) <- spec$name
list2env(dd_soap_models, envir = .GlobalEnv)

# Reference check against the tuned run of 2026-09-16. Not an error -- REML can
# land marginally differently across mgcv versions -- but a large discrepancy
# means a knob moved or the data changed, and every downstream number would then
# be built on a surface nobody has diagnosed.
.aic_ref <- c(dd.dsm.soap.season.year = 6070.35,
              dd.dsm.soap.year.season.sst = 6034.93)
for (.nm in names(.aic_ref)) {
  if (!is.null(dd_soap_models[[.nm]])) {
    .got <- round(AIC(dd_soap_models[[.nm]]), 2)
    message(sprintf("AIC check %-28s got %8.2f  expected %8.2f  (diff %+.2f)",
                    .nm, .got, .aic_ref[.nm], .got - .aic_ref[.nm]))
    if (abs(.got - .aic_ref[.nm]) > 1)
      warning(sprintf(paste0("%s AIC differs from the tuned reference by %+.2f ",
                             "-- check the knobs at the top of this script."),
                      .nm, .got - .aic_ref[.nm]), call. = FALSE)
  }
}

# ============================================================
# Model-selection table (analogous to table_dd_modselection)
# ============================================================
# Tweedie p estimated by tw(); NA for any model fitted with a fixed-p family
.p_hat_m <- function(m)
  if (is.null(m$family$getTheta)) NA_real_ else round(m$family$getTheta(TRUE), 4)

table_dd_soap_modselection <- data.frame(stringsAsFactors = FALSE,
  model = spec$label,
  df    = vapply(dd_soap_models, function(m) round(attr(logLik(m), "df"), 2), numeric(1)),
  AIC   = round(vapply(dd_soap_models, AIC, numeric(1)), 2),
  Dev   = vapply(dd_soap_models, function(m) round(summary(m)$dev.expl, 2), numeric(1)),
  p_hat = vapply(dd_soap_models, .p_hat_m, numeric(1)),
  # AIC assumes independent segments; these are contiguous pieces of one
  # track. lag1_sig = TRUE means the row breaks the independence its own AIC
  # assumes -- do not select it on AIC. Travels with the ranking on purpose.
  lag1     = vapply(dd_soap_models, function(m) dsm_lag1(m)$lag1, numeric(1)),
  lag1_sig = vapply(dd_soap_models, function(m) isTRUE(dsm_lag1(m)$lag1_sig), logical(1))
)
table_dd_soap_modselection$deltaAIC <-
  round(table_dd_soap_modselection$AIC - min(table_dd_soap_modselection$AIC), 2)
table_dd_soap_modselection <-
  table_dd_soap_modselection[order(table_dd_soap_modselection$deltaAIC),
                             c("model", "df", "AIC", "deltaAIC", "Dev", "p_hat",
                               "lag1", "lag1_sig")]
rownames(table_dd_soap_modselection) <- NULL

dsm_lag1_note(transform(table_dd_soap_modselection, lag1_band = dsm_lag1(dd_soap_models[[1]])$lag1_band),
              "table_dd_soap_modselection")
print(table_dd_soap_modselection)

# ============================================================
# Combined selection table — thin-plate (4_CommonDolphin_DSM.R) vs soap
# AIC is comparable across bases: same response, data and Tweedie p within a
# species. Requires the thin-plate models to already be in the workspace.
# ============================================================
.msrow <- function(m, label, basis)
  data.frame(basis = basis, model = label,
             df    = round(attr(logLik(m), "df"), 2),
             AIC   = round(AIC(m), 2),
             Dev   = round(summary(m)$dev.expl, 2),
             p_hat = .p_hat_m(m),
             lag1     = dsm_lag1(m)$lag1,
             lag1_sig = isTRUE(dsm_lag1(m)$lag1_sig),
             stringsAsFactors = FALSE)

# soap candidates (fitted above; concise labels from `spec`)
.soap_rows <- do.call(rbind, Map(function(m, lab) .msrow(m, lab, "soap"),
                                 dd_soap_models, spec$label))

# thin-plate candidates (from 4_CommonDolphin_DSM.R). The name list and the pretty
# labels are both taken from .dd_labels, built there, so this table cannot
# disagree with table_dd_modselection and picks up the VelVert models
# automatically instead of needing a second hand-maintained name list.
.tp_names <- character(0)
if (exists(".dd_labels"))
  .tp_names <- names(.dd_labels)[vapply(names(.dd_labels), exists, logical(1))]

if (length(.tp_names) == 0) {
  warning("No thin-plate models in the workspace — run 4_CommonDolphin_DSM.R ",
          "first; showing the soap table only.")
  table_dd_combined_modselection <- table_dd_soap_modselection
} else {
  .tp_rows <- do.call(rbind, lapply(.tp_names, function(nm)
    .msrow(get(nm), unname(.dd_labels[nm]), "thin-plate")))
  table_dd_combined_modselection <- rbind(.tp_rows, .soap_rows)
  table_dd_combined_modselection$deltaAIC <-
    round(table_dd_combined_modselection$AIC -
            min(table_dd_combined_modselection$AIC), 2)
  table_dd_combined_modselection <-
    table_dd_combined_modselection[order(table_dd_combined_modselection$deltaAIC),
                                   c("basis", "model", "df", "AIC", "deltaAIC", "Dev",
                                     "p_hat", "lag1", "lag1_sig")]
  rownames(table_dd_combined_modselection) <- NULL
}

dsm_lag1_note(table_dd_combined_modselection, "table_dd_combined_modselection")
print(table_dd_combined_modselection)


