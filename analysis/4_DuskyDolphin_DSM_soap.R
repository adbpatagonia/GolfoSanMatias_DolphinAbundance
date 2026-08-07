# ADB / Claude
# 2026-07-07
#
# Soap-film DSM candidate set + model-selection table — DUSKY dolphins.
# Mirrors the candidate set in 4_DuskyDolphin_DSM.R, but the spatial term is a
# SOAP-FILM smoother  s(x, y, bs = "so")  (edge-effect control) instead of the
# thin-plate s(x, y).  Temporal term = s(Ano); covariates = the 6 environmental
# smooths.  The fs year-varying model is NOT included — soap films cannot be used
# as a factor-smooth ("fs") marginal.
#
# WARNING: this fits ~22 soap-film GAMs over all segments. Soap films are SLOW;
# expect minutes to tens of minutes. Confirm the base soap fit works (no
# 'NA/NaN/Inf in soap.basis') before running the whole set — if a knot trips the
# PDE grid, coarsen knot_ngrid / raise knot_buffer / hand-place knots.
#
# Assumes in the workspace:
#   df.lo, segdata (x, y, Ano + env covars depth/slope/grad/sst/clo/dist.up),
#   obsdata_lo_mod, trunc.dist_lo, survey.area_m, target_crs

library(dsm)
library(mgcv)
library(sf)
library(dplyr)

# ============================================================
# Soap boundary (buffered so ALL segments sit inside) + interior knots
# ============================================================
simplify_tol <- 3000        ## TUNE  metres; larger = simpler (safer) boundary
margin       <- 2000        ## TUNE  metres clearance to leave inside the edge
knot_ngrid   <- c(10, 8)    ## TUNE  interior-knot grid density (start coarse)
knot_buffer  <- 1000        ## TUNE  metres; knots this close to the edge are dropped

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

bnd_dist <- function(px, py)
  vapply(seq_along(px),
         function(i) min(sqrt((bnd_loop$x - px[i])^2 + (bnd_loop$y - py[i])^2)),
         numeric(1))

# interior knots, strictly inside + buffered off the edge
kn   <- make.soapgrid(bnd_loop, n.grid = knot_ngrid)
keep <- as.logical(in.out(bmat, cbind(kn$x, kn$y))) & bnd_dist(kn$x, kn$y) > knot_buffer
knots <- data.frame(x = kn$x[keep], y = kn$y[keep])
message(sprintf("soap knots: %d generated, %d kept", length(kn$x), nrow(knots)))

# sanity: every segment must be strictly inside (soap errors otherwise)
seg_in <- as.logical(in.out(bmat, cbind(segdata$x, segdata$y)))
if (!all(seg_in))
  warning(sprintf("%d/%d segments still outside boundary — raise `margin`.",
                  sum(!seg_in), length(seg_in)))

# ============================================================
# Fit helper + candidate set (soap spatial term shared by all)
# ============================================================
soap_term <- 's(x, y, bs = "so", xt = list(bnd = bnd_soap), k = 10)'

fit_soap <- function(extra = "") {
  rhs  <- if (nzchar(extra)) paste(soap_term, "+", extra) else soap_term
  form <- as.formula(paste("count ~", rhs))
  environment(form) <- globalenv()                     # so bnd_soap resolves
  dsm(form,
      ddf.obj          = df.lo,
      segment.data     = segdata,
      observation.data = obsdata_lo_mod,
      family           = Tweedie(p = 1.31),
      method           = "REML",
      knots            = knots)
}

# name / extra terms / label — mirrors the 4_DuskyDolphin_DSM.R set, soap spatial
spec <- data.frame(stringsAsFactors = FALSE,
  name = c(
    "lo.dsm.soap",
    "lo.dsm.soap.season",
    "lo.dsm.soap.season.year",
    "lo.dsm.soap.year.season.slope",
    "lo.dsm.soap.year.season.grad",
    "lo.dsm.soap.year.season.sst",
    "lo.dsm.soap.year.season.clo",
    "lo.dsm.soap.year.season.dist.up",
    "lo.dsm.soap.year.season.depth",
    "lo.dsm.soap.year",
    "lo.dsm.soap.season.slope",
    "lo.dsm.soap.season.grad",
    "lo.dsm.soap.season.sst",
    "lo.dsm.soap.season.clo",
    "lo.dsm.soap.season.dist.up",
    "lo.dsm.soap.season.depth",
    "lo.dsm.soap.year.slope",
    "lo.dsm.soap.year.grad",
    "lo.dsm.soap.year.sst",
    "lo.dsm.soap.year.clo",
    "lo.dsm.soap.year.dist.up",
    "lo.dsm.soap.year.depth"),
  extra = c(
    "",
    "season",
    "season + s(Ano)",
    "season + s(Ano) + s(slope)",
    "season + s(Ano) + s(grad)",
    "season + s(Ano) + s(sst)",
    "season + s(Ano) + s(clo)",
    "season + s(Ano) + s(dist.up)",
    "season + s(Ano) + s(depth)",
    "s(Ano)",
    "season + s(slope)",
    "season + s(grad)",
    "season + s(sst)",
    "season + s(clo)",
    "season + s(dist.up)",
    "season + s(depth)",
    "s(Ano) + s(slope)",
    "s(Ano) + s(grad)",
    "s(Ano) + s(sst)",
    "s(Ano) + s(clo)",
    "s(Ano) + s(dist.up)",
    "s(Ano) + s(depth)"),
  label = c(
    "count ~ s(x,y,so)",
    "count ~ s(x,y,so) + season",
    "count ~ s(x,y,so) + season + s(Ano)",
    "count ~ s(x,y,so) + season + s(Ano) + s(slope)",
    "count ~ s(x,y,so) + season + s(Ano) + s(grad)",
    "count ~ s(x,y,so) + season + s(Ano) + s(sst)",
    "count ~ s(x,y,so) + season + s(Ano) + s(clo)",
    "count ~ s(x,y,so) + season + s(Ano) + s(dist.up)",
    "count ~ s(x,y,so) + season + s(Ano) + s(depth)",
    "count ~ s(x,y,so) + s(Ano)",
    "count ~ s(x,y,so) + season + s(slope)",
    "count ~ s(x,y,so) + season + s(grad)",
    "count ~ s(x,y,so) + season + s(sst)",
    "count ~ s(x,y,so) + season + s(clo)",
    "count ~ s(x,y,so) + season + s(dist.up)",
    "count ~ s(x,y,so) + season + s(depth)",
    "count ~ s(x,y,so) + s(Ano) + s(slope)",
    "count ~ s(x,y,so) + s(Ano) + s(grad)",
    "count ~ s(x,y,so) + s(Ano) + s(sst)",
    "count ~ s(x,y,so) + s(Ano) + s(clo)",
    "count ~ s(x,y,so) + s(Ano) + s(dist.up)",
    "count ~ s(x,y,so) + s(Ano) + s(depth)"))

# fit all (slow!) and expose each as a named object
lo_soap_models <- lapply(spec$extra, fit_soap)
names(lo_soap_models) <- spec$name
list2env(lo_soap_models, envir = .GlobalEnv)

# ============================================================
# Model-selection table (analogous to table_lo_modselection)
# ============================================================
table_lo_soap_modselection <- data.frame(stringsAsFactors = FALSE,
  model = spec$label,
  df    = vapply(lo_soap_models, function(m) round(attr(logLik(m), "df"), 2), numeric(1)),
  AIC   = round(vapply(lo_soap_models, AIC, numeric(1)), 2),
  Dev   = vapply(lo_soap_models, function(m) round(summary(m)$dev.expl, 2), numeric(1))
)
table_lo_soap_modselection$deltaAIC <-
  round(table_lo_soap_modselection$AIC - min(table_lo_soap_modselection$AIC), 2)
table_lo_soap_modselection <-
  table_lo_soap_modselection[order(table_lo_soap_modselection$deltaAIC),
                             c("model", "df", "AIC", "deltaAIC", "Dev")]
rownames(table_lo_soap_modselection) <- NULL

print(table_lo_soap_modselection)

# ============================================================
# Combined selection table — thin-plate (4_DuskyDolphin_DSM.R) vs soap
# AIC is comparable across bases: same response, data and Tweedie p within a
# species. Requires the thin-plate models to already be in the workspace.
# ============================================================
.msrow <- function(m, label, basis)
  data.frame(basis = basis, model = label,
             df    = round(attr(logLik(m), "df"), 2),
             AIC   = round(AIC(m), 2),
             Dev   = round(summary(m)$dev.expl, 2),
             stringsAsFactors = FALSE)

# soap candidates (fitted above; concise labels from `spec`)
.soap_rows <- do.call(rbind, Map(function(m, lab) .msrow(m, lab, "soap"),
                                 lo_soap_models, spec$label))

# thin-plate candidates (from 4_DuskyDolphin_DSM.R); labels from the formulas
.tp_names <- c(
  "lo.dsm.xy", "lo.dsm.xy.season", "lo.dsm.xy.season.year",
  "lo.dsm.xy.year.season.slope", "lo.dsm.xy.year.season.grad",
  "lo.dsm.xy.year.season.sst", "lo.dsm.xy.year.season.clo",
  "lo.dsm.xy.year.season.dist.up", "lo.dsm.xy.year.season.depth",
  "lo.dsm.xy.year", "lo.dsm.xy.season.slope", "lo.dsm.xy.season.grad",
  "lo.dsm.xy.season.sst", "lo.dsm.xy.season.clo", "lo.dsm.xy.season.dist.up",
  "lo.dsm.xy.season.depth", "lo.dsm.xy.year.slope", "lo.dsm.xy.year.grad",
  "lo.dsm.xy.year.sst", "lo.dsm.xy.year.clo", "lo.dsm.xy.year.dist.up",
  "lo.dsm.xy.year.depth", "lo.dsm.xy.fsyear.season",
  "lo.dsm.xy.fsyear",
  "lo.dsm.xy.fsyear.season.slope", "lo.dsm.xy.fsyear.season.grad",
  "lo.dsm.xy.fsyear.season.sst", "lo.dsm.xy.fsyear.season.clo",
  "lo.dsm.xy.fsyear.season.dist.up", "lo.dsm.xy.fsyear.season.depth",
  "lo.dsm.xy.fsyear.slope", "lo.dsm.xy.fsyear.grad", "lo.dsm.xy.fsyear.sst",
  "lo.dsm.xy.fsyear.clo", "lo.dsm.xy.fsyear.dist.up", "lo.dsm.xy.fsyear.depth",
  "lo.dsm.xy.byyear", "lo.dsm.xy.byyear.season",
  "lo.dsm.xy.byyear.season.slope", "lo.dsm.xy.byyear.season.grad",
  "lo.dsm.xy.byyear.season.sst", "lo.dsm.xy.byyear.season.clo",
  "lo.dsm.xy.byyear.season.dist.up", "lo.dsm.xy.byyear.season.depth",
  "lo.dsm.xy.byyear.slope", "lo.dsm.xy.byyear.grad", "lo.dsm.xy.byyear.sst",
  "lo.dsm.xy.byyear.clo", "lo.dsm.xy.byyear.dist.up", "lo.dsm.xy.byyear.depth")
.tp_names <- .tp_names[vapply(.tp_names, exists, logical(1))]

if (length(.tp_names) == 0) {
  warning("No thin-plate models in the workspace — run 4_DuskyDolphin_DSM.R ",
          "first; showing the soap table only.")
  table_lo_combined_modselection <- table_lo_soap_modselection
} else {
  .tp_rows <- do.call(rbind, lapply(.tp_names, function(nm) {
    m <- get(nm)
    .msrow(m, paste(deparse(formula(m), width.cutoff = 200), collapse = " "),
           "thin-plate")
  }))
  table_lo_combined_modselection <- rbind(.tp_rows, .soap_rows)
  table_lo_combined_modselection$deltaAIC <-
    round(table_lo_combined_modselection$AIC -
            min(table_lo_combined_modselection$AIC), 2)
  table_lo_combined_modselection <-
    table_lo_combined_modselection[order(table_lo_combined_modselection$deltaAIC),
                                   c("basis", "model", "df", "AIC", "deltaAIC", "Dev")]
  rownames(table_lo_combined_modselection) <- NULL
}

print(table_lo_combined_modselection)
