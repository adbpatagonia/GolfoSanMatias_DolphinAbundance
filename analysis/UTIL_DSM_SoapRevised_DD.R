# ADB / Claude
# 2026-09-10
#
# COMMON DOLPHIN soap-film block, fitted two ways and ranked in ONE table.
#
#   original : exactly as in 4_CommonDolphin_DSM_soap.R -- interior knot grid
#              10x8 (41 knots kept), boundary-film k = 10, every environmental
#              smooth at mgcv's k = 10 default. These are NOT refitted; the 25
#              fitted objects already in dd_output.RData are used, with a guard
#              that a refit of one of them reproduces its stored AIC.
#   revised  : the same 25 formulas with the two defects found by
#              UTIL_DSM_CovariateK_DD.R fixed -- interior knot grid 14x11
#              (92 knots kept) and every environmental smooth at k = 20.
#
# WHY THESE TWO CHANGES
#   * Interior knot grid. Refining it moved the covariate-free base by 24.19
#     AIC units and its s(x,y) edf from 30.93/49 to 45.01/100. Raising the
#     BOUNDARY dimension k instead did nothing (+0.51), so the interior grid
#     was the binding control, not k. See DD_soap_spatial_basis_check.csv.
#   * Covariate basis. Every environmental smooth was at mgcv's k = 10
#     default. s(sst) gained 11.28 AIC units and s(VelVert) 8.14 when freed to
#     k = 20 -- and NEITHER was at its basis ceiling, so the usual
#     edf/k' screen would have missed both.
#
# s(Ano) IS DELIBERATELY LEFT AT THE DEFAULT. It comes out at edf 1.00 of 9 --
# a straight line, penalised to the edge of the basis from the other side --
# so raising its k cannot help. Only the seven environmental smooths are bumped.
#
# COMPARABILITY. All 50 models are fitted to the same 6288 segments with the
# same response, offset and detection function (df.dd), and every one estimates
# its own Tweedie p via tw() at a cost of 1 df, so AIC ranks them all against
# each other. The script asserts the common n rather than assuming it -- a
# covariate NA silently dropping rows would otherwise produce a table whose
# ranking means nothing. Two deltaAIC columns are reported:
#   deltaAIC       across all 50 rows (the combined ranking)
#   deltaAIC_spec  within that model's own arm (so each arm reads on its own)
#
# CAVEAT ON 14x11. That grid was chosen to show 41 knots was binding
# (edf_frac 0.63 -> 0.45), not by tuning. It is a better grid, not the right
# one. If the revised arm is adopted, tune knot_ngrid properly first.
#
# COST. 25 soap fits, roughly 45-120 s each, so about 30-45 minutes. Every row
# is cached by cached_fit_row() as soon as it is produced, so an interrupted
# run resumes instead of restarting. Delete .cache_soaprev_dd/ to force a
# clean run.
#
# OUTPUT  output/CommonDolphin/DSM/DD_soap_revised_selection.csv   (all 50 rows)
#                                  DD_soap_revised_paired.csv      (one row per
#                                     formula: original vs revised, side by side)

library(dsm)
library(mgcv)
library(data.table)

source(file.path(here::here(), "R", "dsm_correlogram.R"))
source(file.path(here::here(), "R", "cached_fit_row.R"))

load("output/CommonDolphin/dd_output.RData")

.diag_dir <- file.path("output", "CommonDolphin", "DSM")
dir.create(.diag_dir, showWarnings = FALSE, recursive = TRUE)
.cache <- file.path(.diag_dir, ".cache_soaprev_dd")

ENV7        <- c("slope", "grad", "sst", "clo", "dist.up", "depth", "VelVert")
K_COV       <- 20L          # revised covariate basis
KNOT_NGRID  <- c(14L, 11L)  # revised interior knot grid (stored run used 10x8)
K_BND       <- 10L          # boundary film: unchanged, it was never binding

# Excel locks these CSVs and fwrite() would abort the whole run over a write.
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
# 1. The candidate set -- generated exactly as 4_CommonDolphin_DSM_soap.R does,
#    so names and labels cannot drift from the stored objects.
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
  list(
    .sp("dd.dsm.soap.year", "s(Ano)", "count ~ s(x,y,so) + s(Ano)")
  ),
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
cat("candidate set:", nrow(spec), "soap models,",
    sum(!is.na(spec$env)), "of them carrying an environmental covariate\n")

# ---------------------------------------------------------------------------
# 2. Revised interior knots
# ---------------------------------------------------------------------------
.bnd_loop <- bnd_soap[[1]]
.bmat     <- cbind(.bnd_loop$x, .bnd_loop$y)
.bnd_dist <- function(px, py)
  vapply(seq_along(px), function(i)
    min(sqrt((.bnd_loop$x - px[i])^2 + (.bnd_loop$y - py[i])^2)), numeric(1))
.kn   <- make.soapgrid(.bnd_loop, n.grid = KNOT_NGRID)
.keep <- as.logical(in.out(.bmat, cbind(.kn$x, .kn$y))) &
  .bnd_dist(.kn$x, .kn$y) > knot_buffer
knots_revised <- data.frame(x = .kn$x[.keep], y = .kn$y[.keep])
cat(sprintf("interior knots: original %s -> %d | revised %s -> %d\n",
            paste(knot_ngrid, collapse = "x"), nrow(knots),
            paste(KNOT_NGRID, collapse = "x"), nrow(knots_revised)))

# dsm() resolves `knots` by NAME (get(as.character(match.call()$knots))), so the
# knot set has to be staged in the global environment under the name used here.
.fit_soap <- function(rhs, knots_use) {
  assign(".knots_active", knots_use, envir = globalenv())
  rhs <- if (nzchar(rhs)) paste("+", rhs) else ""
  f <- as.formula(sprintf(
    'count ~ s(x, y, bs = "so", xt = list(bnd = bnd_soap), k = %d) %s', K_BND, rhs))
  environment(f) <- globalenv()
  dsm(f, ddf.obj = df.dd, segment.data = segdata,
      observation.data = obsdata_dd_mod,
      family = tw(link = "log"), method = "REML", knots = .knots_active)
}

# raise k on the SEVEN environmental smooths only, never on s(Ano)
.bump_k <- function(extra, k) {
  for (e in ENV7)
    extra <- gsub(sprintf("s(%s)", e), sprintf("s(%s, k = %d)", e, k),
                  extra, fixed = TRUE)
  extra
}

# ---------------------------------------------------------------------------
# 3. Guard: the reconstruction must reproduce a stored fit exactly
# ---------------------------------------------------------------------------
.missing <- spec$name[!vapply(spec$name, exists, logical(1))]
if (length(.missing))
  stop("stored soap models absent from the workspace: ",
       paste(.missing, collapse = ", "),
       ". Run 4_CommonDolphin_DSM_soap.R first.")

.guard <- cached_fit_row("GUARD_orig_refit", .cache, {
  m <- .fit_soap("season + s(Ano)", knots_use = knots)
  data.table(refit = AIC(m), stored = AIC(dd.dsm.soap.season.year),
             ok = isTRUE(all.equal(AIC(m), AIC(dd.dsm.soap.season.year),
                                   tolerance = 1e-5)))
})
cat(sprintf("reproduction guard: refit %.4f vs stored %.4f -> %s\n",
            .guard$refit, .guard$stored, if (.guard$ok) "OK" else "MISMATCH"))
if (!.guard$ok)
  stop("the reconstructed boundary/knots do not reproduce the stored fit, so ",
       "the revised arm would not be comparable with the original arm.")

# ---------------------------------------------------------------------------
# 4. One cached row per (arm, model)
# ---------------------------------------------------------------------------
.d  <- dd.dsm.soap.season.year$data
.sn <- dsm_seg_num(.d$Sample.Label)

.row <- function(m, s, arm) {
  kc  <- dsm_kcheck(m, n.rep = 400)
  rxy <- kc[smooth == "s(x,y)"]
  rcv <- if (is.na(s$env)) NULL else kc[smooth == sprintf("s(%s)", s$env)]
  ct  <- dsm_correlogram(m, .d$Transect.Label, .sn, max.lag = 1)
  data.table(
    arm = arm, name = s$name, model = s$label, env = s$env,
    n = length(m$y),
    df  = round(attr(logLik(m), "df"), 2),
    AIC = round(AIC(m), 2),
    Dev = round(summary(m)$dev.expl, 3),
    p_hat = if (is.null(m$family$getTheta)) NA_real_
            else round(m$family$getTheta(TRUE), 4),
    edf_xy = round(rxy$edf, 2), k_prime_xy = rxy$k_prime,
    edf_frac_xy = rxy$edf_frac,
    edf_env = if (is.null(rcv)) NA_real_ else round(rcv$edf, 2),
    k_prime_env = if (is.null(rcv)) NA_real_ else rcv$k_prime,
    edf_frac_env = if (is.null(rcv)) NA_real_ else rcv$edf_frac,
    lag1 = round(ct$cor, 4), lag1_band = round(ct$band, 4), lag1_sig = ct$sig
  )
}

cat("\n--- original arm (stored objects, no refitting) ---\n")
rows_orig <- rbindlist(lapply(seq_len(nrow(spec)), function(i) {
  s <- spec[i]
  cached_fit_row(sprintf("orig__%s", s$name), .cache,
                 .row(get(s$name), s, "original"))
}))

cat("\n--- revised arm (", nrow(knots_revised), " knots, covariates at k = ",
    K_COV, ") ---\n", sep = "")
rows_rev <- rbindlist(lapply(seq_len(nrow(spec)), function(i) {
  s <- spec[i]
  cached_fit_row(sprintf("rev__%s", s$name), .cache,
                 .row(.fit_soap(.bump_k(s$extra, K_COV), knots_revised),
                      s, "revised"))
}))

# ---------------------------------------------------------------------------
# 5. Combined selection table
# ---------------------------------------------------------------------------
sel <- rbind(rows_orig, rows_rev)

# AIC only ranks models fitted to the same segments -- assert, do not assume
if (uniqueN(sel$n) > 1L)
  stop("models were fitted to different numbers of segments (",
       paste(sort(unique(sel$n)), collapse = " / "),
       ") -- AIC is NOT comparable across these rows.")
if (any(!is.finite(sel$AIC)))
  stop("non-finite AIC (a fixed-p Tweedie family?) in: ",
       paste(sel[!is.finite(AIC), name], collapse = ", "))

sel[, deltaAIC      := round(AIC - min(AIC), 2)]
sel[, deltaAIC_spec := round(AIC - min(AIC), 2), by = arm]
setorder(sel, deltaAIC)
setcolorder(sel, c("arm", "model", "name", "env", "df", "AIC",
                   "deltaAIC", "deltaAIC_spec", "Dev", "p_hat"))

cat("\n=== combined soap selection table: top 15 of", nrow(sel), "===\n")
print(sel[1:15, .(arm, model, df, AIC, deltaAIC, deltaAIC_spec, Dev,
                  edf_xy, edf_env, lag1_sig)])
cat("\nbest in each arm:\n")
print(sel[, .SD[which.min(AIC)], by = arm,
          .SDcols = c("model", "df", "AIC", "deltaAIC", "Dev")])
.write(sel, "DD_soap_revised_selection.csv")

# ---------------------------------------------------------------------------
# 6. Paired view: what the revision is worth, formula by formula
# ---------------------------------------------------------------------------
paired <- dcast(sel, name + model + env ~ arm,
                value.var = c("AIC", "df", "Dev", "edf_xy", "edf_env", "lag1"))
paired[, gain := round(AIC_revised - AIC_original, 2)]   # negative = revision better
setorder(paired, gain)
cat("\n=== paired: AIC_revised - AIC_original (negative = revision better) ===\n")
print(paired[, .(model, env, AIC_original, AIC_revised, gain,
                 Dev_original, Dev_revised,
                 edf_xy_original, edf_xy_revised,
                 edf_env_original, edf_env_revised)])
cat(sprintf("\nrevision improves %d of %d formulas; median gain %.2f AIC\n",
            paired[gain < 0, .N], nrow(paired), median(paired$gain)))
.write(paired, "DD_soap_revised_paired.csv")

cat("\nwrote two CSVs to", .diag_dir, "\n")

