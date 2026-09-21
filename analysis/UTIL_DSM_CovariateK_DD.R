# ADB / Claude
# 2026-09-10
# =========================================================================
# STATUS: DECISION RECORD -- NOT RE-RUN BY THE PIPELINE  (marked 2026-09-16)
#
# This script measured the ORIGINAL common-dolphin soap configuration:
#     simplify_tol 3000 / margin 2000 / knot_ngrid c(10, 8) -> 41 knots,
#     boundary-film k = 10, all seven environmental smooths at mgcv k = 10.
# That configuration no longer exists. 4_CommonDolphin_DSM_soap.R was retuned
# to tol500 / margin250 / c(14, 11) -> 89 knots with K_COV = 20, and the
# evidence for doing so is exactly what this script produced.
#
# SO IT CANNOT BE RE-RUN AGAINST THE CURRENT PIPELINE:
#   * it guards on the base model returning AIC 6100.528, which was the
#     stored value at 41 knots; the pipeline now produces 6070.35, and
#   * its "stored (41 knots, k=10)" arm reads the global `knots`, which the
#     pipeline now sets to 89. That arm would silently stop being `stored`.
#
# It is preserved as the evidence behind that change, not as a regenerable
# output. Its CSVs under output/CommonDolphin/DSM/ are dated artefacts of the
# run described above; 9_RegenerateStudies_DD.R deliberately does NOT source
# it. To re-derive the comparison you would first have to rebuild the
# abandoned configuration, which is the thing the repo decided against.
# =========================================================================
#
#
# Was dropping the environmental covariates the right call WITHIN the soap-film
# block for COMMON dolphins?
#
# THE PROBLEM WITH THE EXISTING ANSWER
# The report drops the environmental covariates because they add little
# deviance (Dev 0.26 vs 0.22). But within the soap block the covariate models
# beat the reported dd.dsm.soap.season.year by a lot -- s(clo) by 47.5 AIC
# units -- and UTIL_DSM_Diagnostics.R showed every s(clo) fit pinned at its
# basis ceiling: edf 8.25-8.43 of k_prime = 9. A smooth that has run out of basis is
# not "as wiggly as the data asked for", so neither its deviance nor its AIC
# is a fair measure of what that covariate is worth. The same k = 10 default
# applies to all seven covariates, so all seven need re-testing, not just clo.
#
# WHAT THIS SCRIPT DOES
#   1. Reproduces the stored soap fit exactly, as a guard: if the reconstructed
#      boundary and knots do not return AIC 6100.528, everything below is void.
#   2. Checks whether the SPATIAL term is basis-limited, which for a soap film
#      means two separate controls -- the interior knot grid (41 knots here)
#      and the boundary-film dimension k (10, giving 8 coefficients; 41 + 8 =
#      the k' = 49 that k.check reports). Refits the base at a denser interior
#      grid and at a larger boundary k to see whether either moves the fit.
#   3. Refits base + s(covariate) for all seven covariates at k = 10 (the
#      original) and k = 20, and compares.
#   4. Re-runs both diagnostics -- correlogram and k.check -- on the k = 20
#      fits, because the k = 10 s(clo) models also carried about twice the
#      lag-1 residual correlation of the reported model.
#
# HOW TO READ THE OUTPUT
#   If a covariate's AIC advantage over the base SURVIVES at k = 20 and its
#   edf settles well below 20, the k = 10 fit was basis-limited and the
#   covariate is doing real work that the original comparison understated.
#   If the advantage largely disappears, the k = 10 advantage was an artefact.
#   If edf climbs to near 20 as well, k is still binding and the covariate is
#   standing in for something the model has not represented -- treat a large
#   AIC gain there as a symptom, not a result.
#
# WHAT THE RUN ACTUALLY FOUND -- two corrections to the premise above
#   a) The s(clo) ceiling that motivated this was nearly harmless: freeing it
#      from k = 10 to k = 20 moved its advantage only -47.46 -> -48.72. The
#      covariates that WERE materially basis-limited are s(sst) (-27.80 ->
#      -39.08) and s(VelVert) (-2.97 -> -11.11), and NEITHER was at its
#      ceiling (edf_frac 0.73 and 0.63). So `edf_frac > 0.8` is not a
#      sufficient screen for "k is binding" -- which is why this script tests
#      k on all seven covariates rather than only the flagged one.
#   b) The larger defect is not a covariate at all. The interior knot grid was
#      under-resolved: 41 -> 92 knots is worth 24.19 AIC units and +3.5
#      deviance points with no covariate in the model. Note that 92 knots is
#      one step up, chosen to show 41 was binding (edf_frac 0.63 -> 0.45); it
#      is not a converged choice, and knot_ngrid in
#      4_CommonDolphin_DSM_soap.R needs tuning rather than replacing with
#      14x11 on this evidence.
#
# OUTPUT  output/CommonDolphin/DSM/DD_covariate_k_comparison.csv
#                                  DD_soap_spatial_basis_check.csv
#                                  DD_covariate_k20_diagnostics.csv
#                                  DD_covariate_k20_denseknots.csv

library(dsm)
library(mgcv)
library(data.table)

source(file.path(here::here(), "R", "dsm_correlogram.R"))
source(file.path(here::here(), "R", "cached_fit_row.R"))

load("output/CommonDolphin/dd_output.RData")
.diag_dir <- file.path("output", "CommonDolphin", "DSM")
dir.create(.diag_dir, showWarnings = FALSE, recursive = TRUE)
.cache <- file.path(.diag_dir, ".cache_covk_dd")

# WRITING OVER A LOCKED FILE. These CSVs get opened in Excel, which takes an
# exclusive lock on Windows, and fwrite() then aborts the whole script -- losing
# the run rather than just the write. Warn and divert to a *_new.csv instead.
.write <- function(x, file) {
  p <- file.path(.diag_dir, file)
  tryCatch(fwrite(x, p), error = function(e) {
    alt <- sub("[.]csv$", "_new.csv", p)
    warning(sprintf("could not write %s (locked? Excel?) -- writing %s instead",
                    basename(p), basename(alt)), call. = FALSE)
    fwrite(x, alt)
  })
}

ENV7 <- c("slope", "grad", "sst", "clo", "dist.up", "depth", "VelVert")
K_NEW <- 20L
# mgcv's default for a 1-D thin-plate s() is k = 10, which k.check reports as
# k' = 9 because the centring constraint removes one degree of freedom. So the
# original fits are k = 10, NOT k = 9 -- fitting k = 9 gives a SMALLER basis
# than the original and is not the right comparison. (Done wrong once: k = 9
# put s(clo) 32.8 AIC units below the base where the original k = 10 fit is
# 47.5 below, which reads as though the covariate were weaker rather than the
# basis being smaller.)
K_OLD <- 10L

# NOTE ON `knots`. dsm() resolves that argument by NAME, not by value --
# internally it does get(as.character(match.call()$knots)) -- so a local
# variable cannot be passed through: dsm() would look for an object of that
# name and fail. The knot set therefore has to be staged in the global
# environment under the exact name used in the call.
.fit_soap <- function(rhs, knots_use = knots, k_bnd = 10) {
  assign(".knots_active", knots_use, envir = globalenv())
  f <- as.formula(sprintf(
    'count ~ s(x, y, bs = "so", xt = list(bnd = bnd_soap), k = %d) + %s', k_bnd, rhs))
  environment(f) <- globalenv()
  dsm(f, ddf.obj = df.dd, segment.data = segdata,
      observation.data = obsdata_dd_mod,
      family = tw(link = "log"), method = "REML", knots = .knots_active)
}

# ---------------------------------------------------------------------------
# 1. Guard: reproduce the stored fit
# ---------------------------------------------------------------------------
base <- .fit_soap("season + s(Ano)")
.stored <- AIC(dd.dsm.soap.season.year)
cat(sprintf("reproduction guard: refit AIC %.4f vs stored %.4f -> %s\n",
            AIC(base), .stored,
            if (isTRUE(all.equal(AIC(base), .stored, tolerance = 1e-5))) "OK" else "MISMATCH"))
if (!isTRUE(all.equal(AIC(base), .stored, tolerance = 1e-5)))
  stop("reconstructed soap boundary/knots do not reproduce the stored fit; ",
       "the comparisons below would not be valid.")

# ---------------------------------------------------------------------------
# 2. Is the SPATIAL term basis-limited? Two controls, tested separately.
# ---------------------------------------------------------------------------
.bnd_loop <- bnd_soap[[1]]
.bmat     <- cbind(.bnd_loop$x, .bnd_loop$y)
.bnd_dist <- function(px, py)
  vapply(seq_along(px), function(i)
    min(sqrt((.bnd_loop$x - px[i])^2 + (.bnd_loop$y - py[i])^2)), numeric(1))
.make_knots <- function(ngrid) {
  kn <- make.soapgrid(.bnd_loop, n.grid = ngrid)
  keep <- as.logical(in.out(.bmat, cbind(kn$x, kn$y))) &
    .bnd_dist(kn$x, kn$y) > knot_buffer
  data.frame(x = kn$x[keep], y = kn$y[keep])
}

.knots_dense <- .make_knots(c(14L, 11L))
cat(sprintf("\ninterior knots: stored grid %s -> %d knots | denser grid 14x11 -> %d knots\n",
            paste(knot_ngrid, collapse = "x"), nrow(knots), nrow(.knots_dense)))
cat("boundary polygon vertices:", length(.bnd_loop$x), "\n")

.spatial_variants <- list(
  "stored (41 knots, k=10)"      = list(kn = knots,         k = 10),
  "denser interior (k=10)"       = list(kn = .knots_dense,  k = 10),
  "larger boundary k=16"         = list(kn = knots,         k = 16),
  "denser interior + k=16"       = list(kn = .knots_dense,  k = 16)
)
# STAGE GUARD. Each stage below re-reads its own CSV if that file is already
# on disk, so the script is idempotent and a re-run after an interruption only
# redoes what is missing. `.knots_dense` above is deliberately computed outside
# any guard -- it involves no fitting and stage 5 needs it.
.stage <- function(file, expr) {
  p <- file.path(.diag_dir, file)
  if (file.exists(p)) { cat(sprintf("[stage cached] %s\n", file)); return(fread(p)) }
  v <- eval.parent(substitute(expr))
  tryCatch(fwrite(v, p), error = function(e)
    warning("could not write ", file, ": ", conditionMessage(e), call. = FALSE))
  v
}

basis_check <- .stage("DD_soap_spatial_basis_check.csv",
  rbindlist(lapply(names(.spatial_variants), function(v) {
    s <- .spatial_variants[[v]]
    m <- .fit_soap("season + s(Ano)", knots_use = s$kn, k_bnd = s$k)
    kc <- dsm_kcheck(m, n.rep = 200)
    data.table(variant = v, n_knots = nrow(s$kn), k_bnd = s$k,
               AIC = round(AIC(m), 2), df = round(attr(logLik(m), "df"), 2),
               edf_xy = round(kc[smooth == "s(x,y)", edf], 2),
               k_prime_xy = kc[smooth == "s(x,y)", k_prime],
               edf_frac_xy = kc[smooth == "s(x,y)", edf_frac])
  })))
cat("\n=== spatial basis check (soap: interior knots vs boundary k) ===\n")
print(basis_check)

# ---------------------------------------------------------------------------
# 3. Covariate refits at k = 10 (original) and k = 20
# ---------------------------------------------------------------------------
.grid <- CJ(cov = ENV7, k = c(K_OLD, K_NEW), sorted = FALSE)
.aic_base <- AIC(base)
.d  <- base$data
.sn <- dsm_seg_num(.d$Sample.Label)

# one cached row per (covariate, k), carrying both the basis check and the
# correlogram so the fitted object never has to be kept or refitted
.row <- function(m, cv, kk) {
  kc <- dsm_kcheck(m, n.rep = 400)
  r  <- kc[smooth == sprintf("s(%s)", cv)]
  ct <- dsm_correlogram(m, .d$Transect.Label, .sn, max.lag = 1)
  cd <- dsm_correlogram(m, .d$traj_id,        .sn, max.lag = 1)
  data.table(covariate = cv, k = kk,
             AIC = round(AIC(m), 2), df = round(attr(logLik(m), "df"), 2),
             edf = round(r$edf, 2), k_prime = r$k_prime, edf_frac = r$edf_frac,
             k_index = round(r$k_index, 3), p_value = round(r$p_value, 3),
             at_ceiling = r$edf_frac > 0.80,
             Dev = round(summary(m)$dev.expl, 3),
             p_hat = round(m$family$getTheta(TRUE), 4),
             lag1_transect = round(ct$cor, 4), band_transect = round(ct$band, 4),
             sig_transect = ct$sig,
             lag1_day = round(cd$cor, 4), band_day = round(cd$band, 4),
             sig_day = cd$sig)
}

rows <- rbindlist(lapply(seq_len(nrow(.grid)), function(i) {
  cv <- .grid$cov[i]; kk <- .grid$k[i]
  cached_fit_row(sprintf("%s_k%d", cv, kk), .cache, {
    .row(.fit_soap(sprintf("season + s(Ano) + s(%s, k = %d)", cv, kk)), cv, kk)
  })
}))
rows[, dAIC_vs_base := round(AIC - .aic_base, 2)]

cmp <- dcast(rows, covariate ~ k,
             value.var = c("AIC", "dAIC_vs_base", "edf", "edf_frac",
                           "k_index", "p_value", "at_ceiling", "Dev"))
setorderv(cmp, paste0("dAIC_vs_base_", K_NEW))

cat(sprintf("\n=== base (no covariate) dd.dsm.soap.season.year: AIC %.2f, Dev %.3f ===\n",
            .aic_base, summary(base)$dev.expl))
cat(sprintf("=== covariate refits: k = %d (original) vs k = %d ===\n", K_OLD, K_NEW))
# column names come out of dcast as <stat>_<k>, so build them from K_OLD/K_NEW
# rather than hard-coding a suffix that goes stale when K_OLD changes
.c <- function(stat, k) paste0(stat, "_", k)
print(cmp[, c("covariate",
              .c(c("AIC", "dAIC_vs_base", "edf", "edf_frac", "Dev"), K_OLD),
              .c(c("AIC", "dAIC_vs_base", "edf", "edf_frac", "Dev"), K_NEW),
              .c("at_ceiling", K_NEW)), with = FALSE])
.write(cmp, "DD_covariate_k_comparison.csv")

# ---------------------------------------------------------------------------
# 4. Diagnostics on the k = 20 fits (computed with each row, above)
# ---------------------------------------------------------------------------
base_diag <- cached_fit_row("BASE_diag", .cache, {
  ct <- dsm_correlogram(base, .d$Transect.Label, .sn, max.lag = 1)
  cd <- dsm_correlogram(base, .d$traj_id,        .sn, max.lag = 1)
  data.table(covariate = "(base, no covariate)", k = NA_integer_,
             lag1_transect = round(ct$cor, 4), band_transect = round(ct$band, 4),
             sig_transect = ct$sig,
             lag1_day = round(cd$cor, 4), band_day = round(cd$band, 4),
             sig_day = cd$sig)
})
diag20 <- rbind(base_diag,
                rows[k == K_NEW, .(covariate, k, lag1_transect, band_transect,
                                   sig_transect, lag1_day, band_day, sig_day)],
                fill = TRUE)
cat("\n=== lag-1 residual correlation, k = 20 fits ===\n")
print(diag20)
.write(diag20, "DD_covariate_k20_diagnostics.csv")

# ---------------------------------------------------------------------------
# 5. The same comparison at a spatial basis that is NOT limiting
#
# Section 2 found the interior knot grid was the binding constraint on the
# spatial term: 41 -> 92 knots gains 24.2 AIC units on the base model, while
# raising the boundary dimension k does nothing. That matters here, because a
# covariate can win AIC by standing in for spatial structure the knot grid
# cannot resolve -- chlorophyll and sea-surface temperature are themselves
# spatially structured fields. Repeating the covariate comparison on the denser
# grid separates "this covariate carries information" from "this covariate is
# proxying for an under-resolved surface". A covariate whose advantage survives
# at 92 knots is doing its own work.
# ---------------------------------------------------------------------------
# the dense base needs its deviance and its correlogram too, not just its AIC:
# the covariate rows below are only interpretable against a same-grid baseline
.base_dense_row <- cached_fit_row("BASE_dense_row", .cache, {
  m  <- .fit_soap("season + s(Ano)", knots_use = .knots_dense)
  ct <- dsm_correlogram(m, .d$Transect.Label, .sn, max.lag = 1)
  data.table(covariate = "(dense base, no covariate)", AIC = round(AIC(m), 2),
             edf = round(sum(m$edf[m$smooth[[1]]$first.para:m$smooth[[1]]$last.para]), 2),
             edf_frac = NA_real_, Dev = round(summary(m)$dev.expl, 3),
             lag1_transect = round(ct$cor, 4), band = round(ct$band, 4),
             sig = ct$sig)
})
.aic_dense <- .base_dense_row$AIC
cat(sprintf("\n=== dense-knot arm: base AIC %.2f (Dev %.3f, lag-1 %.4f) vs %.2f at 41 knots ===\n",
            .aic_dense, .base_dense_row$Dev, .base_dense_row$lag1_transect, .aic_base))

dense <- rbindlist(lapply(ENV7, function(cv)
  cached_fit_row(sprintf("dense_%s_k%d", cv, K_NEW), .cache, {
    m  <- .fit_soap(sprintf("season + s(Ano) + s(%s, k = %d)", cv, K_NEW),
                    knots_use = .knots_dense)
    kc <- dsm_kcheck(m, n.rep = 400)
    r  <- kc[smooth == sprintf("s(%s)", cv)]
    ct <- dsm_correlogram(m, .d$Transect.Label, .sn, max.lag = 1)
    data.table(covariate = cv, AIC = round(AIC(m), 2),
               edf = round(r$edf, 2), edf_frac = r$edf_frac,
               Dev = round(summary(m)$dev.expl, 3),
               lag1_transect = round(ct$cor, 4), band = round(ct$band, 4),
               sig = ct$sig)
  })))
dense <- rbind(.base_dense_row, dense, fill = TRUE)
dense[, dAIC_vs_dense_base := round(AIC - .aic_dense, 2)]
setcolorder(dense, c("covariate", "AIC", "dAIC_vs_dense_base"))
setorder(dense, dAIC_vs_dense_base)
cat("\n=== covariates at k = 20 on the DENSER knot grid (92 knots) ===\n")
print(dense)
.write(dense, "DD_covariate_k20_denseknots.csv")

# ---------------------------------------------------------------------------
# 6. The missing cell, so the two defects can be read SEPARATELY
#
# Sections 3 and 5 give three of the four combinations of (interior knot grid)
# x (covariate basis k). Fitting the fourth -- 92 knots with the covariate left
# at the original k = 10 -- completes a 2x2 for every covariate, which is what
# lets the two issues be attributed rather than confounded:
#
#   vertical gap between the two grids at fixed k .... the SPATIAL defect
#   slope from k = 10 to k = 20 at fixed grid ........ the COVARIATE-BASIS defect
#   difference of those slopes ....................... interaction, i.e. whether
#       a covariate needed the extra basis only because the surface was coarse
# ---------------------------------------------------------------------------
dense_k10 <- rbindlist(lapply(ENV7, function(cv)
  cached_fit_row(sprintf("dense_%s_k%d", cv, K_OLD), .cache, {
    m <- .fit_soap(sprintf("season + s(Ano) + s(%s, k = %d)", cv, K_OLD),
                   knots_use = .knots_dense)
    r <- dsm_kcheck(m, n.rep = 400)[smooth == sprintf("s(%s)", cv)]
    data.table(covariate = cv, AIC = round(AIC(m), 2),
               edf = round(r$edf, 2), Dev = round(summary(m)$dev.expl, 3))
  })))

grid2x2 <- rbindlist(list(
  rows[k == K_OLD, .(covariate, knots = 41L, k = K_OLD, AIC, edf, Dev)],
  rows[k == K_NEW, .(covariate, knots = 41L, k = K_NEW, AIC, edf, Dev)],
  dense_k10[,      .(covariate, knots = 92L, k = K_OLD, AIC, edf, Dev)],
  dense[covariate %in% ENV7,
                   .(covariate, knots = 92L, k = K_NEW, AIC, edf, Dev)]
))
# everything measured against the model the report presents
grid2x2[, dAIC_vs_reported := round(AIC - .aic_base, 2)]
setorder(grid2x2, covariate, knots, k)
.write(grid2x2, "DD_covariate_2x2_grid.csv")

# attribution per covariate, at the k / grid the other factor is held at
attrib <- dcast(grid2x2, covariate ~ knots + k, value.var = "dAIC_vs_reported")
setnames(attrib, c("covariate", "g41_k10", "g41_k20", "g92_k10", "g92_k20"))
attrib[, `:=`(
  covariate_k_effect_at41 = round(g41_k20 - g41_k10, 2),
  covariate_k_effect_at92 = round(g92_k20 - g92_k10, 2),
  grid_effect_at_k10      = round(g92_k10 - g41_k10, 2),
  grid_effect_at_k20      = round(g92_k20 - g41_k20, 2)
)]
setorder(attrib, g92_k20)
cat("\n=== 2x2 attribution: dAIC vs the reported model (6100.53) ===\n")
print(attrib)
cat(sprintf("\nreference: base with NO covariate -- 41 knots %.2f (0.00), 92 knots %.2f (%.2f)\n",
            .aic_base, .aic_dense, .aic_dense - .aic_base))
.write(attrib, "DD_covariate_2x2_attribution.csv")

cat("\nwrote six CSVs to", .diag_dir, "\n")
