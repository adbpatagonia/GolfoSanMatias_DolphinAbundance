# ADB / Claude
# 2026-09-10
#
# Was dropping the environmental covariates the right call WITHIN the fs
# (factor-smooth) block for DUSKY dolphins?
#
# Companion to UTIL_DSM_CovariateK_DD.R -- same question, the other species and
# the other considered block. See that script's header for the reasoning; the
# short version is that every environmental smooth in this project was fitted
# at mgcv's k = 10 default (which k.check reports as k' = 9, the centring
# constraint costing one df), so a covariate whose smooth ran out of basis
# would have its contribution understated in both deviance and AIC. For dusky
# the k = 10 diagnostics were already clean -- s(depth) at edf 3.66 of 9, no
# ceiling -- so the expectation is that k = 20 changes little. That is worth
# confirming rather than assuming, because it is the control case that makes
# the common dolphin result interpretable.
#
# The base model is the one the report presents:
#   lo.dsm.xy.fsyear.season = count ~ s(x, y, year_fac, bs = "fs") + season
#
# COST AND CRASH-RESILIENCE
# One fs fit takes about 11 MINUTES here -- the s(x,y,year_fac) basis is
# 330-dimensional. Two consequences shape this script:
#   * The k = 10 arm is NOT refitted. lo.dsm.xy.fsyear.season.<cov> already is
#     base + s(<cov>) at the default k, so those models are read from the
#     workspace. Only the seven k = 20 fits are run.
#   * Every row is cached to disk as soon as it is produced, via
#     cached_fit_row(). A machine crash during the first attempt at this
#     analysis destroyed about forty minutes of fitting because results were
#     assembled in memory and written only at the end. Re-running now resumes
#     from whatever is already cached; delete .cache_covk_lo/ to force a
#     clean run.
#
# OUTPUT  output/DuskyDolphin/DSM/LO_covariate_k_comparison.csv
#                                 LO_covariate_k20_diagnostics.csv

library(dsm)
library(mgcv)
library(data.table)

source(file.path(here::here(), "R", "dsm_correlogram.R"))
source(file.path(here::here(), "R", "cached_fit_row.R"))

load("output/DuskyDolphin/lo_output.RData")
.diag_dir <- file.path("output", "DuskyDolphin", "DSM")
dir.create(.diag_dir, showWarnings = FALSE, recursive = TRUE)
.cache <- file.path(.diag_dir, ".cache_covk_lo")

ENV7  <- c("slope", "grad", "sst", "clo", "dist.up", "depth", "VelVert")
K_NEW <- 20L
K_OLD <- 10L      # mgcv's default for a 1-D s(); k.check calls it k' = 9

.fit_fs <- function(rhs) {
  f <- as.formula(sprintf('count ~ s(x, y, year_fac, bs = "fs") + %s', rhs))
  environment(f) <- globalenv()
  dsm(f, ddf.obj = df.lo, segment.data = segdata,
      observation.data = obsdata_lo_mod,
      family = tw(link = "log"), method = "REML")
}

# ---------------------------------------------------------------------------
# 1. Base model, and a one-off guard that refitting reproduces it
#
# The base IS a stored object, so use it directly rather than spending 11
# minutes recreating it. The guard refits it once and caches the comparison;
# it exists to prove .fit_fs() specifies the same model the workspace holds,
# which is what licenses reading the stored k = 10 fits as the k = 10 arm.
# ---------------------------------------------------------------------------
base      <- lo.dsm.xy.fsyear.season
.aic_base <- AIC(base)

.guard <- cached_fit_row("GUARD_base_refit", .cache, {
  m <- .fit_fs("season")
  data.table(refit = AIC(m), stored = .aic_base,
             ok = isTRUE(all.equal(AIC(m), .aic_base, tolerance = 1e-5)))
})
cat(sprintf("reproduction guard: refit AIC %.4f vs stored %.4f -> %s\n",
            .guard$refit, .guard$stored, if (.guard$ok) "OK" else "MISMATCH"))
if (!.guard$ok)
  stop("refit does not reproduce the stored fs fit; comparisons would not be valid.")

# ---------------------------------------------------------------------------
# 2. One cached row per (covariate, k): AIC, basis check and correlogram
# ---------------------------------------------------------------------------
.d  <- base$data
.sn <- dsm_seg_num(.d$Sample.Label)

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

.grid <- CJ(cov = ENV7, k = c(K_OLD, K_NEW), sorted = FALSE)
rows <- rbindlist(lapply(seq_len(nrow(.grid)), function(i) {
  cv <- .grid$cov[i]; kk <- .grid$k[i]
  cached_fit_row(sprintf("%s_k%d", cv, kk), .cache, {
    m <- if (kk == K_OLD) {
      nm <- sprintf("lo.dsm.xy.fsyear.season.%s", cv)
      if (!exists(nm)) stop("stored k = 10 model not in the workspace: ", nm)
      get(nm)
    } else {
      .fit_fs(sprintf("season + s(%s, k = %d)", cv, kk))
    }
    .row(m, cv, kk)
  })
}))
rows[, dAIC_vs_base := round(AIC - .aic_base, 2)]

# base's own correlogram, for the diagnostics table
base_diag <- cached_fit_row("BASE_diag", .cache, {
  ct <- dsm_correlogram(base, .d$Transect.Label, .sn, max.lag = 1)
  cd <- dsm_correlogram(base, .d$traj_id,        .sn, max.lag = 1)
  data.table(covariate = "(base, no covariate)", k = NA_integer_,
             lag1_transect = round(ct$cor, 4), band_transect = round(ct$band, 4),
             sig_transect = ct$sig,
             lag1_day = round(cd$cor, 4), band_day = round(cd$band, 4),
             sig_day = cd$sig)
})

# ---------------------------------------------------------------------------
# 3. Tables
# ---------------------------------------------------------------------------
cmp <- dcast(rows, covariate ~ k,
             value.var = c("AIC", "dAIC_vs_base", "edf", "edf_frac",
                           "k_index", "p_value", "at_ceiling", "Dev"))
.c <- function(stat, k) paste0(stat, "_", k)
setorderv(cmp, .c("dAIC_vs_base", K_NEW))

cat(sprintf("\n=== base lo.dsm.xy.fsyear.season: AIC %.2f, Dev %.3f ===\n",
            .aic_base, summary(base)$dev.expl))
cat(sprintf("=== covariate refits: k = %d (original, from workspace) vs k = %d ===\n",
            K_OLD, K_NEW))
print(cmp[, c("covariate",
              .c(c("AIC", "dAIC_vs_base", "edf", "edf_frac", "Dev"), K_OLD),
              .c(c("AIC", "dAIC_vs_base", "edf", "edf_frac", "Dev"), K_NEW),
              .c("at_ceiling", K_NEW)), with = FALSE])
fwrite(cmp, file.path(.diag_dir, "LO_covariate_k_comparison.csv"))

diag20 <- rbind(base_diag,
                rows[k == K_NEW, .(covariate, k, lag1_transect, band_transect,
                                   sig_transect, lag1_day, band_day, sig_day)],
                fill = TRUE)
cat("\n=== lag-1 residual correlation, k = 20 fits ===\n")
print(diag20)
fwrite(diag20, file.path(.diag_dir, "LO_covariate_k20_diagnostics.csv"))

cat("\nwrote two CSVs to", .diag_dir, "\n")
