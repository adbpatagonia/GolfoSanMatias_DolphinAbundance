# ADB / Claude
# 2026-09-09
#
# DSM diagnostics: (1) along-track residual autocorrelation, (2) basis
# dimension, (3) rootograms. Runs for one species per invocation; set SPP
# below, or set it in the calling environment before source()ing this file.
#
# WHY THESE THREE CHECKS
# The model-selection tables rank candidates by AIC, and mgcv computes that AIC
# with the smoothing-parameter-uncertainty correction of Wood, Pya & Safken
# (2016) -- sum(edf2) plus one degree of freedom for the Tweedie power p that
# tw() estimates. That correction is the right one, but it does not rescue two
# assumptions the ranking still rests on:
#
#   AUTOCORRELATION. Both the AIC and the REML smoothing-parameter selection
#   assume the segments are independent. Segments are contiguous pieces of one
#   survey track, so they need not be. If the residuals stay correlated along
#   the track, the effective sample size is below the nominal one and AIC
#   over-selects complexity -- which, with ~57 candidates per species, is
#   exactly what would make small deltaAIC gaps unreadable.
#
#   BASIS DIMENSION. Every smooth is fitted inside a basis of size k. If the
#   fit runs up against that ceiling, the term is not "as wiggly as the
#   penalty wanted", it is "as wiggly as k allowed", and its deviance and edf
#   -- hence its AIC -- reflect the ceiling rather than the data. k.check
#   flags that.
#
#   GOODNESS OF FIT. Neither of the above asks whether the fitted distribution
#   actually produces the counts that were seen. A rootogram (Kleiber &
#   Zeileis 2016) does: it compares the observed number of segments in each
#   count bin with the number the fitted model expects, on the square-root
#   scale. That is the scale on which the standard error of sqrt(observed) is
#   roughly constant, so a deviation of a given size means the same thing in
#   the zero bin as in the tail -- which is what makes the two failure modes
#   of segment-level count data readable at all: overdispersion, and excess
#   zeros. See R/dsm_rootogram.R for why the expected counts have to be built
#   by differencing the per-segment Tweedie CDF over the bins rather than by
#   the discrete P(Y = y) construction of the published rootogram.
#
# WHAT COUNTS AS A PROBLEM
#   Correlogram: a RUN of positive correlations starting at lag 1 and decaying.
#   Isolated exceedances at scattered lags are noise -- across several models
#   and a dozen lags a few are expected. Note also that a flat correlogram
#   means no UNMODELLED correlation, not no correlation: a flexible
#   year-varying spatial term can absorb along-track structure into the fitted
#   surface, which is the same mechanism as the over-fitting being tested for.
#   That is why a no-year-term model is always included below as a positive
#   control -- it shows what the correlation looks like before it is absorbed.
#
#   k.check: edf close to k_prime AND a low k_index with a small p_value. Either
#   alone is weak evidence; k_index below ~1 with p below ~0.05 and edf near
#   the ceiling together mean raise k and refit.
#
#   Rootogram: a bar foot displaced from zero by more than its band, and more
#   importantly a PATTERN across bins. One foot high in the zero bin with the
#   rest roughly flat is excess zeros; feet high at both ends and low in the
#   middle is overdispersion; scattered single exceedances are noise. The band
#   assumes independent segments and ignores parameter uncertainty, so it is
#   optimistic for the same reason the correlogram's is.
#
# SURVEY BLOCKS. Rootograms are computed at two scales: the whole survey area,
# and survey blocks. This survey has no design strata -- segdata carries no
# block column and survey.area.shp is one polygon -- so "survey block" here is
# a regular grid over the gulf, defined by dsm_spatial_blocks(). The grid is
# arbitrary but model-independent, so the same blocks compare across candidate
# models and across species; partitioning by nearest soap knot would tie more
# directly to the knot question but the blocks would move with each variant's
# knot set. The block scale exists to localise a misfit the pooled rootogram
# averages away, which is what makes it useful for judging soap-film knot
# number and placement.
#
# OUTPUT  output/<Species>/DSM/autocorrelation/{LO,DD}_dsm_correlogram.{csv,png}
#                              autocorrelation/{LO,DD}_autocorrelation_summary.csv
#                              autocorrelation/README_{LO,DD}_autocorrelation.txt
#                              {LO,DD}_dsm_kcheck.csv
#                              {LO,DD}_dsm_rootogram.{csv,png}
#                              {LO,DD}_dsm_rootogram_block.{csv,png}
#
# Assumes the species workspace exists (produced by 1_DuskyDolphin.R /
# 1_CommonDolphin.R, plus the soap and biological-year scripts if those
# candidates are to be diagnosed). If the models are already in the workspace
# it uses them; otherwise it load()s the .RData.

library(dsm)
library(mgcv)
library(data.table)
library(ggplot2)
library(patchwork)

source(file.path(here::here(), "R", "dsm_correlogram.R"))
source(file.path(here::here(), "R", "dsm_rootogram.R"))

# ---------------------------------------------------------------------------
# 1. Load the species workspace, THEN configure
#
# Order matters. The saved .RData carries its own `out_dir`, `MODELS` and
# friends from whichever script wrote it, and load() drops them straight into
# the global environment. Anything configured before the load is silently
# overwritten -- the first run of this script wrote its output to
# output/DuskyDolphin/Nobs2Sensitivity for exactly that reason. So: capture the
# caller's overrides into dotted names, load, then set everything else.
# ---------------------------------------------------------------------------
# .SPP is the preferred override; SPP is kept for interactive use. The
# drivers set the dotted one because they save with ls() (all.names = FALSE),
# which keeps it out of the .RData -- a saved SPP would otherwise be pulled in
# by the OTHER species' report and quietly name the wrong species.
.spp    <- if (exists(".SPP")) .SPP else if (exists("SPP")) SPP else "DuskyDolphin"
.models <- if (exists("MODELS")) MODELS else NULL
stopifnot(.spp %in% c("DuskyDolphin", "CommonDolphin"))

# `reported` is the model whose results the Quarto report actually presents,
# and `block` is the family of candidates that was considered for that species.
# Both come from docs/Delfines{Oscuros,Comunes}.html, not from the AIC ranking:
#
#   DUSKY  -- time-varying surfaces were judged the right description (Garaffo
#     et al.: dusky dolphins shift their aggregation areas between years), so
#     the fs block was considered. Section 6.1: "En lo que continua, presentare
#     los resultados del modelo que excluye variables ambientales", i.e.
#     count ~ s(x,y,year_fac,bs="fs") + season. Section 7.1.3 refits that same
#     formula for the n_obs sensitivity analysis.
#   COMMON -- the static density foci sat on the southern and eastern
#     boundaries, so the soap film was judged the right description of an edge
#     effect: "el modelo 1.b Superficie estatica soap film parece ser el que se
#     ajusta mas a la situacion". Section 7.1 calls it "el mejor modelo (DSM
#     soap model, con anio y estacion como covariables)".
#
# Note that 5_<Species>_Abundance.R runs dsm_var_gam() on the soap variant for
# BOTH species; for dusky that is not the model the report presents. The report
# is what settles it.
#
# This matters for how deltaAIC is read. The global winner for dusky is a
# by-year model 56 units ahead of the reported fs model, but the by-year block
# was set aside on the grounds that it estimates unrealistic surfaces in years
# with few data -- so that 56 is not a decision-relevant number. The gap that
# IS decision-relevant is the one WITHIN the considered block, which the table
# below reports as deltaAIC_block.
.cfg <- list(
  DuskyDolphin  = list(prefix = "lo", tag = "LO",
                       rdata = "output/DuskyDolphin/lo_output.RData",
                       reported = "lo.dsm.xy.fsyear.season",
                       block = "fs", block_label = "factor-smooth (fs)"),
  CommonDolphin = list(prefix = "dd", tag = "DD",
                       rdata = "output/CommonDolphin/dd_output.RData",
                       reported = "dd.dsm.soap.season.year",
                       block = "soap", block_label = "soap film")
)[[.spp]]

if (!any(grepl(sprintf("^%s\\.dsm", .cfg$prefix), ls(envir = .GlobalEnv)))) {
  message("no ", .cfg$prefix, ".dsm* objects in the workspace - loading ", .cfg$rdata)
  # Load into a throwaway environment and copy over ONLY the fitted models.
  # load(envir = .GlobalEnv) drops every object the saving script held on top of
  # the config set above -- including the dotted ones, because save.image()
  # writes all.names = TRUE. That makes .cfg, .spp and .models clobberable, so
  # capturing the caller's overrides into dotted names (see the header note) is
  # not on its own enough. Everything downstream reads each model's own $data,
  # so no other workspace object is needed here.
  .tmp_env  <- new.env(parent = emptyenv())
  load(.cfg$rdata, envir = .tmp_env)
  .keep_obj <- ls(.tmp_env, all.names = TRUE)
  .keep_obj <- .keep_obj[vapply(.keep_obj, function(n)
    inherits(tryCatch(get(n, envir = .tmp_env), error = function(e) NULL), "dsm"),
    logical(1))]
  if (!length(.keep_obj)) stop("no dsm objects in ", .cfg$rdata)
  for (.n in .keep_obj) assign(.n, get(.n, envir = .tmp_env), envir = .GlobalEnv)
  message("  copied ", length(.keep_obj), " dsm objects: ",
          paste(.keep_obj, collapse = ", "))
  rm(.tmp_env, .keep_obj, .n)
}

.max_lag_transect <- 12L   # Transect.Label series are short (median 15 segments)
.max_lag_day      <- 25L   # traj_id series are ~5x longer
.n_rep_kcheck     <- 400L
.rg_max_count     <- 20L               # largest count with its own bin
.rg_tail_mult     <- 2                 # tail bins double: 20, 40, 80, ...
.rg_block_grid    <- c(3L, 3L)         # survey-block grid over the gulf
.rg_block_min_n   <- 60L               # cells with fewer segments are dropped
.rg_block_breaks  <- c(0, 1, 2, 4, 8, Inf)   # coarser: blocks hold few segments

.diag_dir <- file.path("output", .spp, "DSM")
dir.create(.diag_dir, showWarnings = FALSE, recursive = TRUE)

# Residual-autocorrelation output gets its own folder so a reader can assess
# that question without picking it out of the rest of the diagnostics.
.acf_dir <- file.path(.diag_dir, "autocorrelation")
dir.create(.acf_dir, showWarnings = FALSE, recursive = TRUE)

.all <- ls(envir = .GlobalEnv)
.all <- .all[vapply(.all, function(n)
  inherits(tryCatch(get(n, envir = .GlobalEnv), error = function(e) NULL), "dsm"),
  logical(1))]
if (!length(.all)) stop("no dsm objects found for ", .spp)

.info <- data.table(
  name = .all,
  n    = vapply(.all, function(n) length(get(n)$y), integer(1)),
  df   = vapply(.all, function(n) round(attr(logLik(get(n)), "df"), 2), numeric(1)),
  AIC  = vapply(.all, function(n) round(AIC(get(n)), 2), numeric(1)),
  soap = vapply(.all, function(n)
           any(vapply(get(n)$smooth, inherits, logical(1), "soap.film")), logical(1)),
  fs   = vapply(.all, function(n)
           any(vapply(get(n)$smooth, inherits, logical(1), "fs.interaction")), logical(1)),
  year = vapply(.all, function(n)
           grepl("Ano|year_fac", paste(deparse(formula(get(n))), collapse = " ")), logical(1))
)

# AIC only ranks models fitted to the SAME segments, and a fixed-p Tweedie()
# family returns AIC = NA. Restrict to the modal n with a finite AIC.
.n_modal <- .info[, .N, by = n][which.max(N), n]
.rank <- .info[is.finite(AIC) & n == .n_modal][order(AIC)]
if (nrow(.info[n != .n_modal]))
  message("excluded from ranking (different n, AIC not comparable): ",
          paste(.info[n != .n_modal, name], collapse = ", "))

# the considered block, and deltaAIC measured within it
.rank[, in_block := get(.cfg$block)]
.rank[, deltaAIC       := round(AIC - min(AIC), 2)]
.rank[in_block == TRUE, deltaAIC_block := round(AIC - min(AIC), 2)]

cat("\n=== ", .spp, ": ", .cfg$block_label, " block, ranked within block ===\n", sep = "")
print(head(.rank[in_block == TRUE,
                 .(name, df, AIC, deltaAIC_block, deltaAIC_global = deltaAIC)], 10))
cat("reported model:", .cfg$reported, " -- within-block deltaAIC:",
    .rank[name == .cfg$reported, deltaAIC_block],
    "| global deltaAIC:", .rank[name == .cfg$reported, deltaAIC], "\n")

if (is.null(.models)) {
  .models <- unique(stats::na.omit(c(
    .cfg$reported,                                  # what the report presents
    .rank[in_block == TRUE][1]$name,                # best in the considered block
    .rank[in_block == TRUE & year == FALSE][1]$name,# control: no year term
    .rank[1]$name                                   # global winner, for context
  )))
}
.models <- .models[vapply(.models, exists, logical(1))]

# A model set that has quietly lost its reported member is the failure worth
# catching here: the block rootogram falls back to .models[1] and the bundle
# below then describes models the report does not present. That is a live risk
# for the LO soap arm. Warn rather than stop -- the drivers source this script
# before the save, so stopping would discard the whole pipeline run -- and
# record it in the bundle so it cannot be read past.
.reported_ok <- .cfg$reported %in% .models
if (!.reported_ok) {
  cat("\n", strrep("!", 72), "\n", sep = "")
  cat("WARNING: the reported model (", .cfg$reported, ") was not fitted in this\n",
      "run, so everything below describes OTHER models. Check that the ",
      .cfg$block_label, " block for ", .spp, " actually converged.\n", sep = "")
  cat(strrep("!", 72), "\n", sep = "")
  warning("reported model ", .cfg$reported, " missing from ", .spp,
          " diagnostics")
}
cat("\n=== ", .spp, ": diagnosing ", length(.models), " models ===\n", sep = "")
print(.rank[name %in% .models])

# ---------------------------------------------------------------------------
# 2. Along-track residual autocorrelation
# ---------------------------------------------------------------------------
.d       <- get(.models[1])$data
.seg_num <- dsm_seg_num(.d$Sample.Label)

.scales <- list(
  "transect leg (Transect.Label)" = list(g = .d$Transect.Label, lag = .max_lag_transect),
  "survey day (traj_id)"          = list(g = .d$traj_id,        lag = .max_lag_day)
)
for (s in names(.scales))
  cat(sprintf("%-32s %4d series, median %.0f segments\n", s,
              uniqueN(.scales[[s]]$g), median(table(.scales[[s]]$g))))

cor_tab <- rbindlist(lapply(names(.scales), function(s) {
  rbindlist(lapply(.models, function(m) {
    r <- dsm_correlogram(get(m), .scales[[s]]$g, .seg_num, max.lag = .scales[[s]]$lag)
    cbind(model = m, scale = s, r)
  }))
}))
cor_tab[, `:=`(cor = round(cor, 4), band = round(band, 4))]

cat("\n--- lag 1-4, by scale ---\n")
print(dcast(cor_tab[lag <= 4], model + lag ~ scale, value.var = "cor"))
cat("\n--- exceedances of +/-2/sqrt(n_pairs) (flag, not a test) ---\n")
print(cor_tab[sig == TRUE, .(model, scale, lag, cor, band)])
cat("\nAR(1)-equivalent effective sample size from lag 1:\n")
print(cor_tab[lag == 1, .(model, scale, lag1 = cor,
                          n_eff = round(.n_modal * (1 - cor) / (1 + cor)),
                          of = .n_modal)])

fwrite(cor_tab, file.path(.acf_dir, sprintf("%s_dsm_correlogram.csv", .cfg$tag)))

# ---- reader-facing summary: one row per model x scale, verdict spelled out ----
# The full correlogram is 12-25 lags per model per scale, which is the right
# object for checking but the wrong one for reading. This collapses it to the
# question actually being asked: does this model's residual series look
# independent along the track?
acf_summary <- cor_tab[, {
  l1 <- cor[lag == 1]; b1 <- band[lag == 1]
  .(lag1            = l1,
    band            = b1,
    pct_of_band     = round(100 * abs(l1) / b1),
    lag1_within     = abs(l1) < b1,
    n_lags_checked  = .N,
    n_lags_outside  = sum(sig),
    lags_outside    = if (any(sig)) paste(lag[sig], collapse = ",") else "",
    max_abs_cor     = max(abs(cor)),
    n_eff_lag1      = round(.n_modal * (1 - l1) / (1 + l1)),
    n_segments      = .n_modal)
}, by = .(model, scale)]
acf_summary[, verdict := fifelse(
  lag1_within & n_lags_outside == 0, "pass",
  fifelse(lag1_within, "pass at lag 1; isolated higher-lag spike(s)",
          "FAIL: lag-1 outside band"))]
setorder(acf_summary, scale, model)
cat("\n=== autocorrelation summary (written to", .acf_dir, ") ===\n")
print(acf_summary[, .(model, scale, lag1, band, pct_of_band, verdict)])
fwrite(acf_summary, file.path(.acf_dir, sprintf("%s_autocorrelation_summary.csv", .cfg$tag)))

# Provenance, so the folder cannot be read out of context. For the soap species
# the knot count and boundary ARE the thing that changed during tuning, so they
# are recorded: a file saying 41 knots predates the tuned configuration, one
# saying 89 knots is current.
.prov <- c(sprintf("  species/model set : %s", .spp),
           sprintf("  models diagnosed  : %s", paste(.models, collapse = ", ")),
           sprintf("  segments (n)      : %d", .n_modal))
if (exists("knots", envir = .GlobalEnv) && is.data.frame(get("knots", envir = .GlobalEnv)))
  .prov <- c(.prov, sprintf("  soap interior knots: %d",
                            nrow(get("knots", envir = .GlobalEnv))))
if (exists("simplify_tol", envir = .GlobalEnv) && exists("margin", envir = .GlobalEnv))
  .prov <- c(.prov, sprintf("  soap boundary      : simplify_tol %s / margin %s",
                            get("simplify_tol", envir = .GlobalEnv),
                            get("margin", envir = .GlobalEnv)))

writeLines(c(
  sprintf("%s -- residual autocorrelation: how to read these files", .cfg$tag),
  sprintf("generated %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "PROVENANCE (regenerate this folder whenever the models are refitted)",
  .prov,
  "",
  "FILES",
  sprintf("  %s_autocorrelation_summary.csv  one row per model x scale -- start here",
          .cfg$tag),
  sprintf("  %s_dsm_correlogram.csv          every lag, the underlying numbers",
          .cfg$tag),
  sprintf("  %s_dsm_correlogram.png          the same thing as a figure", .cfg$tag),
  "",
  "WHAT IS BEING TESTED",
  strwrap(paste(
    "A density surface model assumes the segment residuals are independent.",
    "They are not, if animals seen on one segment make animals on the next",
    "segment more likely than the model expects. That would not bias the",
    "abundance estimate much, but it deflates the standard errors and inflates",
    "AIC differences -- so both the confidence intervals and the model",
    "selection would be over-confident. These files check that assumption by",
    "correlating each segment's residual with the residual k segments later",
    "along the same track."), width = 78),
  "",
  "HOW TO READ IT",
  strwrap(paste(
    "lag1 is the correlation between neighbouring segments -- the one that",
    "matters. band is +/- 2/sqrt(n_pairs), a rough envelope for 'indistinguishable",
    "from zero'. pct_of_band says how much of that envelope the model uses: 87",
    "means lag1 is at 87% of the band, i.e. inside but not comfortably.",
    "verdict collapses it. A single spike at a high lag with lag 1 clean is",
    "noise -- with a dozen lags per model, a few exceedances are expected.",
    "The signal to worry about is a RUN of positive correlations starting at",
    "lag 1."), width = 78),
  "",
  strwrap(paste(
    "n_eff_lag1 translates lag-1 into an AR(1)-equivalent effective sample",
    "size: if it is much below n_segments, the model has fewer independent",
    "observations than it thinks, and that is the practical cost."), width = 78),
  "",
  "TWO SCALES",
  strwrap(paste(
    "'transect leg' lags within a transect -- the fine scale, a few km.",
    "'survey day' lags within a day's track -- the coarse scale. A model can",
    "pass one and fail the other; both are reported."), width = 78),
  "",
  "CAVEAT",
  strwrap(paste(
    "The band is optimistic. It treats the n_pairs residual pairs as",
    "independent when they overlap by construction, so a correlation just",
    "inside the band is weaker evidence of independence than it looks.",
    "Treat 'inside the band' as 'not obviously violated', not as 'verified'."),
    width = 78)),
  file.path(.acf_dir, sprintf("README_%s_autocorrelation.txt", .cfg$tag)))

# short labels: the object name minus the species prefix
cor_tab[, model_f := factor(sub(sprintf("^%s\\.dsm\\.", .cfg$prefix), "", model),
                            levels = sub(sprintf("^%s\\.dsm\\.", .cfg$prefix), "", .models))]

p_cor <- ggplot(cor_tab, aes(lag, cor)) +
  geom_ribbon(aes(ymin = -band, ymax = band), fill = "grey85") +
  geom_hline(yintercept = 0, linewidth = .3) +
  geom_segment(aes(xend = lag, yend = 0, colour = sig), linewidth = .9) +
  geom_point(aes(colour = sig), size = 1.4) +
  scale_colour_manual(values = c(`FALSE` = "grey25", `TRUE` = "#c0392b"),
                      labels = c("within band", "outside band"), name = NULL) +
  facet_grid(model_f ~ scale, scales = "free_x") +
  labs(title = sprintf("%s: along-track residual autocorrelation", .spp),
       subtitle = paste("scaled Pearson residuals; shaded band = +/-2/sqrt(n_pairs).",
                        "A run of positive lags starting at 1 is the signal;",
                        "\nisolated spikes are noise. The no-year-term model is the",
                        "positive control."),
       x = "Lag (segments)", y = "Residual correlation") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text.y = element_text(angle = 0, hjust = 0, size = 7),
        strip.text.x = element_text(face = "bold"))

ggsave(file.path(.acf_dir, sprintf("%s_dsm_correlogram.png", .cfg$tag)),
       p_cor, width = 11, height = 2 + 2 * length(.models), dpi = 150, limitsize = FALSE)

# ---------------------------------------------------------------------------
# 3. Basis dimension
# ---------------------------------------------------------------------------
set.seed(42)   # k.check randomises; fix it so the table is reproducible
k_tab <- rbindlist(lapply(.models, function(m)
  cbind(model = m, dsm_kcheck(get(m), n.rep = .n_rep_kcheck))))

# FLAGGING. Only edf sitting near k_prime is evidence that the BASIS is the binding
# constraint. A low k_index with a small p_value on its own is not: it says
# residual variance is higher between neighbouring covariate values than
# expected, and raising k is only one of the things that can cause. The
# discriminating check is whether the k_index is depressed for this smooth
# specifically or for every smooth in the model. Common dolphin shows
# k_index 0.53-0.68 on ALL terms including s(Ano) at 11% of its basis -- a
# smooth using an eighth of its basis cannot be basis-limited, so that pattern
# points at the mean-variance relationship or at spatial structure the 1-D
# randomisation is picking up, not at k. Dusky sits at 0.70-0.84 throughout.
# So: `refit at higher k` is driven by edf_frac, and the k_index columns are
# reported for context rather than converted into a verdict.
k_tab[, near_ceiling := edf_frac > 0.80]
k_tab[, shrunk       := edf < 0.5]
k_tab[, low_k_index  := k_index < 1 & p_value < 0.05]
k_tab[, flag := fifelse(shrunk, "shrunk to ~0",
                 fifelse(near_ceiling, "at ceiling - refit at higher k",
                  fifelse(low_k_index, "low k_index (see model-wide pattern)", "")))]
# is the low k_index specific to a term, or model-wide?
k_tab[, k_index_model_wide := mean(low_k_index) > 0.8, by = model]

cat("\n=== basis dimension (k.check, n.rep =", .n_rep_kcheck, ") ===\n")
print(k_tab[, .(model, smooth, k_prime, edf = round(edf, 2), edf_frac,
                k_index = round(k_index, 3), p_value = round(p_value, 3), flag)])
cat("\nsmooths at their basis ceiling (refit at higher k):",
    k_tab[near_ceiling == TRUE, .N], "\n")
if (any(k_tab$k_index_model_wide))
  cat("NOTE: k_index is low for essentially EVERY smooth in:",
      paste(unique(k_tab[k_index_model_wide == TRUE, model]), collapse = ", "),
      "\n  A model-wide depression is not per-smooth basis shortage -- look at the\n",
      " mean-variance relationship (Tweedie p, zero inflation) and at spatial\n",
      " structure the along-track randomisation may be detecting.\n")

fwrite(k_tab, file.path(.diag_dir, sprintf("%s_dsm_kcheck.csv", .cfg$tag)))

# ---------------------------------------------------------------------------
# 4. Rootogram: whole survey area
# ---------------------------------------------------------------------------
# Bins are taken from the FIRST model and reused for the rest. All candidates
# are fitted to the same segments so the observed counts are identical; fixing
# the bins keeps the facets on a common x axis and makes the expected curves
# comparable panel to panel.
rg_breaks <- dsm_rootogram(get(.models[1]), max_count = .rg_max_count)$upper

# SPLIT THE LUMPED TAIL. Left alone, the last bin is (cutoff, Inf), which for
# common dolphin covers counts 21 to 462 in one bar. A deficit there is the
# largest deviation in the table, but it cannot be read: "too much predicted
# mass above 20" and "about the right mass, spread over a far wider range than
# was observed" produce the same bar. Doubling bins across the observed tail
# separate them, at no cost when the tail is short (dusky's max is 20, so this
# adds a single bin).
.tail_cut <- max(rg_breaks[is.finite(rg_breaks)])
.tail_max <- max(get(.models[1])$y)
.tail     <- .tail_cut * .rg_tail_mult^seq_len(20)
.tail     <- .tail[.tail < .tail_max]
rg_breaks <- c(rg_breaks[is.finite(rg_breaks)], .tail, Inf)

rg_tab <- rbindlist(lapply(.models, function(m) {
  pars <- dsm_tweedie_pars(get(m))
  cbind(model = m, p = round(pars$p, 4), phi = round(pars$phi, 4),
        dsm_rootogram(get(m), breaks = rg_breaks))
}))

.y1 <- get(.models[1])$y
cat("\n=== rootogram, whole survey area ===\n")
cat("counts: n =", length(.y1), "| zeros =", sum(.y1 == 0),
    sprintf("(%.1f%%)", 100 * mean(.y1 == 0)), "| max =", max(.y1),
    "\nbins:", paste(levels(rg_tab$bin), collapse = " "), "\n\n")
print(rg_tab[, .(model, bin, observed, expected = round(expected, 1),
                 resid = round(resid, 2), band = round(band, 2), sig)])

# The zero bin is the headline: the paragraph this implements names excess
# zeros explicitly, and a Tweedie with 1 < p < 2 has an atom there whose size
# is a function of mu, p and phi rather than a free parameter.
cat("\n--- zero bin (excess zeros?) ---\n")
print(rg_tab[bin == "0", .(model, p, phi, observed, expected = round(expected, 1),
                           resid = round(resid, 2), band = round(band, 2), sig)])
cat("\n--- upper tail bin ---\n")
print(rg_tab[bin == levels(bin)[length(levels(bin))],
             .(model, bin, observed, expected = round(expected, 1),
               resid = round(resid, 2), band = round(band, 2), sig)])
cat("\nbins outside the band, per model:\n")
print(rg_tab[, .(n_bins = .N, outside = sum(sig),
                 worst_bin = as.character(bin[which.max(abs(resid))]),
                 worst_resid = round(resid[which.max(abs(resid))], 2)),
             by = model])

fwrite(rg_tab, file.path(.diag_dir, sprintf("%s_dsm_rootogram.csv", .cfg$tag)))

rg_tab[, model_f := factor(sub(sprintf("^%s\\.dsm\\.", .cfg$prefix), "", model),
                           levels = sub(sprintf("^%s\\.dsm\\.", .cfg$prefix), "",
                                        .models))]
rg_tab[, bin_i := as.integer(bin)]

# TWO STYLES, SIDE BY SIDE. Kleiber & Zeileis describe both, and on this
# response both are needed. With 93% of segments empty the zero bar is ~76 on
# the sqrt scale and every other bin is a sliver, so the HANGING rootogram --
# the canonical one -- shows the dominant feature (the atom at zero, matched
# almost exactly) and nothing else. The SUSPENDED version plots the deviations
# themselves on their own scale, which is where the fit in the positive bins
# can actually be read. Neither is a summary of the other, so both are drawn.
.rg_fill <- scale_fill_manual(values = c(`FALSE` = "grey85", `TRUE` = "#f0b27a"),
                              labels = c("within band", "outside band"),
                              name = NULL, drop = FALSE)
.rg_x    <- scale_x_continuous(breaks = unique(rg_tab$bin_i),
                               labels = levels(rg_tab$bin))
.rg_thm  <- theme_bw(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 7),
        # the split tail bins carry wide labels ("161-320") that collide when
        # horizontal
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

p_hang <- ggplot(rg_tab, aes(bin_i)) +
  geom_rect(aes(xmin = bin_i - 0.4, xmax = bin_i + 0.4, ymin = ymin, ymax = ymax,
                fill = sig), colour = "grey30", linewidth = .2) +
  geom_hline(yintercept = 0, linewidth = .3) +
  geom_line(aes(y = sqrt_exp), colour = "#c0392b", linewidth = .6) +
  geom_point(aes(y = sqrt_exp), colour = "#c0392b", size = 1.1) +
  .rg_fill + .rg_x +
  facet_wrap(~ model_f, ncol = 1, scales = "free_y") +
  labs(subtitle = paste("HANGING: bars hang from sqrt(expected) (red curve).",
                        "A foot above the\nzero line means fewer segments were",
                        "expected in that bin than were seen."),
       x = "Count per segment", y = "sqrt(frequency)") +
  .rg_thm

p_susp <- ggplot(rg_tab, aes(bin_i)) +
  geom_ribbon(aes(ymin = -band, ymax = band), fill = "grey88") +
  geom_col(aes(y = resid, fill = sig), width = .8, colour = "grey30",
           linewidth = .2) +
  geom_hline(yintercept = 0, linewidth = .3) +
  .rg_fill + .rg_x +
  facet_wrap(~ model_f, ncol = 1, scales = "free_y") +
  labs(subtitle = paste("SUSPENDED: the deviations themselves,",
                        "sqrt(obs) - sqrt(exp), with the\n+/-2 SE band. Optimistic:",
                        "it assumes independent segments."),
       x = "Count per segment", y = "sqrt(obs) - sqrt(exp)") +
  .rg_thm

p_rg <- (p_hang | p_susp) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = sprintf("%s: rootogram, whole survey area", .spp),
    subtitle = paste("Expected counts from the fitted Tweedie CDF differenced",
                     "over the bins; the atom at zero is P(Y = 0).")) &
  theme(legend.position = "bottom")

ggsave(file.path(.diag_dir, sprintf("%s_dsm_rootogram.png", .cfg$tag)),
       p_rg, width = 13, height = 2.2 + 1.9 * length(.models), dpi = 150,
       limitsize = FALSE)

# ---------------------------------------------------------------------------
# 5. Rootogram: survey-block scale
#
# ONE model only -- the reported one. Four models by nine blocks is 36 panels
# and unreadable, and the block scale is a tool for locating a misfit rather
# than a figure to present.
# ---------------------------------------------------------------------------
.rg_block_model <- if (.cfg$reported %in% .models) .cfg$reported else .models[1]
.mb  <- get(.rg_block_model)
.blk <- dsm_spatial_blocks(.mb$data$x, .mb$data$y,
                           n = .rg_block_grid, min_n = .rg_block_min_n)

cat("\n=== rootogram, survey-block scale:", .rg_block_model, "===\n")
cat(sprintf("%dx%d grid over the gulf, cells with < %d segments dropped\n",
            .rg_block_grid[1], .rg_block_grid[2], .rg_block_min_n))
print(.blk[!is.na(block), .N, by = block][order(block)])
if (anyNA(.blk$block))
  cat("dropped:", sum(is.na(.blk$block)), "segments in under-sampled cells\n")

# NA blocks are dropped rather than pooled: a cell with a handful of segments
# gives a rootogram that is all noise, and lumping the leftovers into one
# pseudo-block would mix opposite corners of the gulf.
.keep  <- !is.na(.blk$block)
rg_blk <- dsm_rootogram(.mb, breaks = .rg_block_breaks,
                        group = ifelse(.keep, as.character(.blk$block), NA))
rg_blk <- rg_blk[!is.na(group)]
rg_blk[, group := droplevels(group)]

cat("\n--- per block: zero bin, and worst bin ---\n")
print(rg_blk[, .(n_seg = sum(observed),
                 zero_obs = observed[bin == "0"],
                 zero_exp = round(expected[bin == "0"], 1),
                 zero_resid = round(resid[bin == "0"], 2),
                 zero_sig = sig[bin == "0"],
                 worst_bin = as.character(bin[which.max(abs(resid))]),
                 worst_resid = round(resid[which.max(abs(resid))], 2),
                 outside = sum(sig)), by = group][order(group)])

fwrite(rg_blk, file.path(.diag_dir,
                         sprintf("%s_dsm_rootogram_block.csv", .cfg$tag)))

rg_blk[, bin_i := as.integer(bin)]
# panels laid out as the map is: block C<col>R<row>, column across, row up
rg_blk[, `:=`(b_col = as.integer(sub("^C([0-9]+)R.*$", "\\1", group)),
              b_row = as.integer(sub("^.*R([0-9]+)$", "\\1", group)))]
rg_blk[, row_f := factor(b_row, levels = rev(sort(unique(b_row))))]

# SUSPENDED only here. The point of the block scale is to locate a misfit, and
# a hanging bar in a block whose segments are ~93% empty is the zero bar and
# nothing else; the deviations are the whole content.
p_rgb <- ggplot(rg_blk, aes(bin_i)) +
  geom_ribbon(aes(ymin = -band, ymax = band), fill = "grey88") +
  geom_col(aes(y = resid, fill = sig), width = .8, colour = "grey30",
           linewidth = .2) +
  geom_hline(yintercept = 0, linewidth = .3) +
  geom_text(data = rg_blk[, .(lab = sprintf("n = %d", sum(observed))),
                          by = .(row_f, b_col)],
            aes(x = Inf, y = Inf, label = lab), hjust = 1.1, vjust = 1.4,
            size = 2.4, colour = "grey40", inherit.aes = FALSE) +
  scale_fill_manual(values = c(`FALSE` = "grey85", `TRUE` = "#f0b27a"),
                    labels = c("within band", "outside band"), name = NULL,
                    drop = FALSE) +
  scale_x_continuous(breaks = unique(rg_blk$bin_i), labels = levels(rg_blk$bin)) +
  facet_grid(row_f ~ b_col, labeller = label_both) +
  labs(title = sprintf("%s: rootogram by survey block -- %s", .spp,
                       .rg_block_model),
       subtitle = paste0(sprintf("%dx%d grid over the gulf, laid out as the map ",
                                 .rg_block_grid[1], .rg_block_grid[2]),
                         "(b_col across, row_f up); empty panels are cells with ",
                         sprintf("< %d segments.", .rg_block_min_n),
                         "\nSuspended style: bars are sqrt(obs) - sqrt(exp) with the",
                         " +/-2 SE band. Coarser bins than the pooled rootogram,",
                         "\nsince blocks hold few segments. Used to locate a misfit,",
                         " not to present."),
       x = "Count per segment", y = "sqrt(obs) - sqrt(exp)") +
  theme_bw(base_size = 9) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

ggsave(file.path(.diag_dir, sprintf("%s_dsm_rootogram_block.png", .cfg$tag)),
       p_rgb, width = 3 + 2.4 * .rg_block_grid[1],
       height = 2 + 2.2 * .rg_block_grid[2], dpi = 150, limitsize = FALSE)

# ---------------------------------------------------------------------------
# 6. Bundle the results under a species-prefixed name
#
# The Quarto reports load() the species workspace and nothing else, so the
# diagnostics have to travel inside it. Two constraints shape the object:
#
#   - ONE object, species-prefixed. The tables built above are cor_tab, k_tab,
#     rg_tab and friends -- all species-agnostic -- so a DD run after an LO run
#     in the same session overwrites the LO results and the save keeps whichever
#     ran last. lo.diag and dd.diag cannot collide.
#   - TABLES ONLY, no ggplot objects. A stored ggplot captures its enclosing
#     environment, and with dsm objects in scope that pulls copies of the fitted
#     models into the list and inflates the .RData. Reports either
#     include_graphics() the PNGs written above or rebuild from these tables.
# ---------------------------------------------------------------------------
assign(sprintf("%s.diag", .cfg$prefix),
       list(correlogram     = copy(cor_tab),
            acf_summary     = copy(acf_summary),
            kcheck          = copy(k_tab),
            rootogram       = copy(rg_tab),
            rootogram_block = copy(rg_blk),
            rank            = copy(.rank),
            models          = .models,
            reported        = .cfg$reported,
            block           = .cfg$block,
            block_label     = .cfg$block_label,
            spp             = .spp,
            reported_ok     = .reported_ok,
            run_at          = Sys.time()),
       envir = .GlobalEnv)

# The plots are dropped for the environment-capture reason above; the PNGs on
# disk are the artefact. The tables are left in place for interactive use.
rm(p_cor, p_hang, p_susp, p_rg, p_rgb)

cat("\nwrote diagnostics to", .diag_dir, "\n")
cat("results bundled as ", sprintf("%s.diag", .cfg$prefix), " -- fields: ",
    paste(names(get(sprintf("%s.diag", .cfg$prefix))), collapse = ", "),
    "\n", sep = "")
