# ADB / Claude
# 2026-09-16
#
# COMMON DOLPHIN: why the rootogram tail misfits, and two routes out.
#
# THE PROBLEM. The DD rootogram over-predicts large counts -- observed 213 vs
# expected 257 in bins >= 5, ~17% too many -- identically in all four DD models
# including both clo variants. So it is a distributional problem, not something
# a covariate or the spatial basis can fix. LO has no such problem.
#
# THE CAUSE, as a variance decomposition rather than a heuristic. For a compound
# process (N schools per segment, each of size X),
#     Var(count) = E[N]*Var(X) + Var(N)*E[X]^2
# and for DD that is
#     1.458*1104 + 0.908*11.82^2  =  1611 + 127  =  1738   vs 1630 observed.
# 92.7% of the count variance is SCHOOL SIZE, not school number. School number is
# nearly Poisson (Var 0.908, mean 1.458); school size is median 4 with a second
# cluster at 150-450 and a maximum of 450.
#
# A Tweedie with 1<p<2 IS a compound Poisson-gamma, so it is structurally the
# right model -- but the gamma cannot simultaneously concentrate near 4 and reach
# 450. The fitted p = 1.576, phi = 29.7 is a compromise that puts too much mass
# in the tail. LO's maximum school is 11, and its rootogram is clean: that
# contrast is the evidence.
#
# ROUTE 2 (quick): let the dispersion vary. mgcv's twlss() is a Tweedie
#   location-scale family with three linear predictors, so phi -- and p -- can
#   depend on covariates instead of being one global power law.
#
# ROUTE 1 (prototype): model GROUP counts, then multiply by mean group size.
#   Removes school-size variance from the spatial model entirely. This is the
#   standard distance-sampling answer to this exact symptom.
#
# WHAT THIS SCRIPT IS NOT. A replacement for the pipeline. It fits nothing that
# 4_CommonDolphin_DSM_soap.R depends on and writes nothing that script reads.
# It is evidence for a decision about the count model, not the decision.
#
# twlss PREDICTOR ORDER -- verified here, not assumed. twlss(link = list("log",
# "identity", "identity")) gives lp1 = log(mu), lp2 = a logistic transform of p
# onto (a, b) = (1.01, 1.99), lp3 = log(phi). Checked by fitting the constant
# model and recovering tw()'s p and phi: 1.562 vs 1.576 and 27.0 vs 29.7, with
# mu correlating 0.99984. NOTE that twlss and tw AIC are NOT comparable -- they
# count the dispersion parameters differently -- so AIC is used only WITHIN the
# twlss ladder.
#
# OUTPUT  output/CommonDolphin/DSM/tail/DD_tail_variance_decomposition.csv
#                                       DD_tail_route2_twlss.csv
#                                       DD_tail_route1_groups.csv
#                                       DD_tail_rootogram_compare.{csv,png}
#                                       DD_tail_groupsize.csv
#                                       README_DD_tail.txt

library(dsm)
library(mgcv)
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)

source(file.path(here::here(), "R", "dsm_rootogram.R"))
source(file.path(here::here(), "R", "cached_fit_row.R"))

load("output/CommonDolphin/dd_output.RData")

.out   <- file.path("output", "CommonDolphin", "DSM", "tail")
.cache <- file.path(.out, ".cache_tailfix_dd")
dir.create(.out,   showWarnings = FALSE, recursive = TRUE)
dir.create(.cache, showWarnings = FALSE, recursive = TRUE)
.mod_dir <- file.path("output", "CommonDolphin", "DSM", "tuned_models")

.tb    <- readRDS(file.path(.mod_dir, "dd_tuned_base.rds"))
m_tw   <- .tb$model                       # the tuned reported model, tw()
assign(".bnd_active",   list(.tb$loop), envir = globalenv())
assign(".knots_active", .tb$knots,      envir = globalenv())

.write <- function(x, f) {
  p <- file.path(.out, f)
  tryCatch(fwrite(x, p), error = function(e) {
    alt <- sub("[.]csv$", "_new.csv", p)
    warning(sprintf("could not write %s -- writing %s", basename(p), basename(alt)),
            call. = FALSE)
    fwrite(x, alt)
  })
}

# ---------------------------------------------------------------------------
# 0. The diagnosis, as a decomposition
# ---------------------------------------------------------------------------
.o   <- as.data.table(obsdata_dd_mod)
.seg <- .o[, .(count = sum(size), ngroup = .N), by = Sample.Label]
.EN <- mean(.seg$ngroup); .VN <- var(.seg$ngroup)
.EX <- mean(.o$size);     .VX <- var(.o$size)

decomp <- data.table(
  quantity = c("E[N] schools per occupied segment", "Var[N]",
               "E[X] school size", "Var[X]",
               "E[N]*Var[X]  (school-SIZE component)",
               "Var[N]*E[X]^2 (school-NUMBER component)",
               "predicted Var[count]", "observed Var[count]",
               "school-size share of variance (%)"),
  value = round(c(.EN, .VN, .EX, .VX, .EN * .VX, .VN * .EX^2,
                  .EN * .VX + .VN * .EX^2, var(.seg$count),
                  100 * .EN * .VX / (.EN * .VX + .VN * .EX^2)), 3))
cat("\n=== variance decomposition ===\n"); print(decomp)
.write(decomp, "DD_tail_variance_decomposition.csv")

gs <- data.table(
  stat = c("n detections", "min", "median", "mean", "p90", "p99", "max",
           "sd", "CV of a single school", "CV of the MEAN school size"),
  value = round(c(nrow(.o), min(.o$size), median(.o$size), mean(.o$size),
                  quantile(.o$size, .9), quantile(.o$size, .99), max(.o$size),
                  sd(.o$size), sd(.o$size) / mean(.o$size),
                  sd(.o$size) / mean(.o$size) / sqrt(nrow(.o))), 4))
cat("\n=== group sizes ===\n"); print(gs)

# SIZE-BIAS CHECK. Both detection functions are ~1, so p_hat is CONSTANT and the
# Horvitz-Thompson mean school size is algebraically identical to the naive mean
# -- there is no size-bias correction in the current fit, and none can come from
# it. The only way to see size bias is to ask whether bigger schools were
# detected further out.
.dd <- as.data.table(distdata_dd_sf_m)[!is.na(distance) & distance <= trunc.dist_dd]
.sb <- lm(log(size) ~ distance, data = .dd[size > 0])
.sbc <- summary(.sb)$coefficients
cat(sprintf("\n=== size bias: log(size) ~ distance, n = %d ===\n", nrow(.dd[size > 0])))
cat(sprintf("  slope %+.5f per m (SE %.5f), p = %.4f\n",
            .sbc[2, 1], .sbc[2, 2], .sbc[2, 4]))
cat(sprintf("  implied size ratio across the truncation distance: %.2f\n",
            exp(.sbc[2, 1] * trunc.dist_dd)))

# SIZE-BIAS CORRECTION. If bigger schools are detected further out, the detected
# sample over-represents them and the naive mean OVERSTATES the population mean
# school size. The standard correction (Buckland et al. 2001, sec 3.5.4) is to
# take the regression's prediction at distance 0, where detection is certain.
# Back-transformed from the log scale with the smearing term exp(sigma^2 / 2).
.s2      <- summary(.sb)$sigma^2
.Sbar_sb <- exp(.sbc[1, 1] + .s2 / 2)
.Sbar_nv <- mean(.o$size)
.sb_sig  <- .sbc[2, 4] < 0.05
cat(sprintf("  naive mean school size            : %.2f\n", .Sbar_nv))
cat(sprintf("  size-bias-corrected (at distance 0): %.2f  (%.0f%% of naive)\n",
            .Sbar_sb, 100 * .Sbar_sb / .Sbar_nv))
if (.sb_sig)
  cat("  *** SIZE BIAS IS SIGNIFICANT -- the naive mean is NOT usable for route 1 ***\n")

gs <- rbind(gs, data.table(
  stat  = c("size-bias slope log(size)~distance", "size-bias p", "size ratio over w",
            "naive mean school size", "size-bias-corrected mean school size",
            "corrected / naive"),
  value = round(c(.sbc[2, 1], .sbc[2, 4], exp(.sbc[2, 1] * trunc.dist_dd),
                  .Sbar_nv, .Sbar_sb, .Sbar_sb / .Sbar_nv), 5)))
.write(gs, "DD_tail_groupsize.csv")

# ---------------------------------------------------------------------------
# Rootogram machinery that accepts PER-OBSERVATION p and phi
#
# R/dsm_rootogram.R is built for one scalar p and phi and refuses a twlss fit
# (dsm_tweedie_pars() requires family ~ "^Tweedie"). That helper is on the
# pipeline path and is deliberately NOT modified here; this is a local version.
# ---------------------------------------------------------------------------
# tweedie::ptweedie takes ONE power per call -- it vectorises over q, mu and phi
# but not over the index. When p varies by observation the observations are
# therefore binned by p (at most .np_bin groups, group mean used) and one
# vectorised call is made per bin. The Tweedie CDF is smooth in p and the bins
# are narrow, so this is a rounding of p, not an approximation of the model.
.np_bin <- 200L

.roots <- function(y, mu, p, phi, breaks, label) {
  stopifnot(length(mu) == length(y))
  n <- length(y)
  p <- rep_len(p, n); phi <- rep_len(phi, n)
  upper <- as.numeric(breaks)

  grp <- if (length(unique(round(p, 8))) == 1L) rep(1L, n) else
    as.integer(cut(p, breaks = unique(quantile(p, seq(0, 1, length.out = .np_bin + 1L))),
                   include.lowest = TRUE))
  # ptweedie() cannot evaluate q = Inf -- it fails inside the C inversion rather
  # than returning 1 -- and the last break IS Inf, so that edge is set directly.
  .fin <- is.finite(upper)
  cdf <- matrix(1, n, length(upper))
  for (g in sort(unique(grp))) {
    ix <- which(grp == g)
    pg <- mean(p[ix])
    cdf[ix, .fin] <- vapply(upper[.fin], function(q)
      tweedie::ptweedie(q = rep(q, length(ix)), mu = mu[ix], phi = phi[ix], power = pg),
      numeric(length(ix)))
  }
  pi <- cdf
  if (ncol(cdf) > 1L) pi[, -1L] <- cdf[, -1L, drop = FALSE] - cdf[, -ncol(cdf), drop = FALSE]
  expected <- colSums(pi)
  lower <- c(0, upper[-length(upper)])
  lab   <- c("0", vapply(seq_along(upper)[-1], function(j)
    if (is.infinite(upper[j])) sprintf(">%g", lower[j])
    else if (upper[j] - lower[j] == 1) sprintf("%g", upper[j])
    else sprintf("%g-%g", lower[j] + 1, upper[j]), character(1)))
  obs <- as.numeric(table(cut(y, breaks = c(-Inf, upper), labels = FALSE)))
  obs <- c(obs, rep(0, length(upper) - length(obs)))[seq_along(upper)]
  data.table(fit = label, bin = lab, lower = lower, upper = upper,
             observed = obs, expected = expected)
}

# `from` is the smallest COUNT in the tail, so the bins wanted are those whose
# lower edge is >= from - 1 (bin "5" spans lower 4 to upper 5), plus any open
# ">n" bin. Getting this off by one silently shifts the whole tail comparison.
.tail_stat <- function(rt, from = 5) {
  z <- rt[lower >= from - 1 | grepl("^>", bin)]
  data.table(tail_obs = sum(z$observed), tail_exp = round(sum(z$expected), 1),
             tail_pct_over = round(100 * (sum(z$expected) - sum(z$observed)) /
                                     sum(z$observed), 1),
             zero_obs = rt[bin == "0", observed],
             zero_exp = round(rt[bin == "0", expected], 1))
}

.breaks_ind <- dsm_rootogram(m_tw, max_count = 20L)$upper   # the pipeline's bins

# ---------------------------------------------------------------------------
# ROUTE 2. twlss: let phi (and p) vary
# ---------------------------------------------------------------------------
.dat <- m_tw$data
.dat$logoff <- m_tw$offset

.f_mu <- count ~ s(x, y, bs = "so", xt = list(bnd = .bnd_active), k = 10) +
                 season + s(Ano) + offset(logoff)
environment(.f_mu) <- globalenv()

.twlss_specs <- list(
  list(key = "A_constant",   p = ~ 1,        phi = ~ 1,
       label = "phi ~ 1, p ~ 1  (matches tw)"),
  list(key = "B_phi_season", p = ~ 1,        phi = ~ season,
       label = "phi ~ season"),
  list(key = "C_phi_xy",     p = ~ 1,        phi = ~ s(x, y),
       label = "phi ~ s(x,y)"),
  list(key = "D_phi_p_xy",   p = ~ s(x, y),  phi = ~ s(x, y),
       label = "phi ~ s(x,y), p ~ s(x,y)"))

.fit_twlss <- function(sp) {
  f2 <- sp$p; f3 <- sp$phi
  environment(f2) <- environment(f3) <- globalenv()
  gam(list(.f_mu, f2, f3), family = twlss(), data = .dat,
      knots = .knots_active, method = "REML")
}

# p is on a logistic transform onto (a, b); phi is on the log scale
.twlss_pars <- function(g, a = 1.01, b = 1.99) {
  pr <- predict(g, type = "response")
  pr <- as.matrix(pr)
  list(mu = pr[, 1], p = a + (b - a) * plogis(pr[, 2]), phi = exp(pr[, 3]))
}

cat("\n=== ROUTE 2: twlss ladder ===\n")
route2 <- rbindlist(lapply(.twlss_specs, function(sp) {
  cached_fit_row(sprintf("twlss__%s", sp$key), .cache, {
    t0 <- Sys.time()
    g  <- .fit_twlss(sp)
    saveRDS(g, file.path(.cache, sprintf("model_twlss_%s.rds", sp$key)))
    pars <- .twlss_pars(g)
    rt   <- .roots(as.numeric(g$y), pars$mu, pars$p, pars$phi, .breaks_ind, sp$label)
    cbind(data.table(spec = sp$key, label = sp$label,
                     AIC_twlss = round(AIC(g), 2),
                     edf = round(sum(g$edf), 1),
                     p_med = round(median(pars$p), 3),
                     p_range = sprintf("%.2f-%.2f", min(pars$p), max(pars$p)),
                     phi_med = round(median(pars$phi), 2),
                     phi_range = sprintf("%.1f-%.1f", min(pars$phi), max(pars$phi)),
                     secs = round(as.numeric(difftime(Sys.time(), t0, units = "secs")))),
          .tail_stat(rt))
  })
}))
# the tw() reference row, same bins, scalar p/phi
.pars_tw <- dsm_tweedie_pars(m_tw)
.rt_tw   <- .roots(as.numeric(m_tw$y), fitted(m_tw), .pars_tw$p, .pars_tw$phi,
                   .breaks_ind, "tw() reference")
route2 <- rbind(
  cbind(data.table(spec = "tw_reference", label = "tw() -- the current model",
                   AIC_twlss = NA_real_, edf = round(sum(m_tw$edf), 1),
                   p_med = round(.pars_tw$p, 3), p_range = "constant",
                   phi_med = round(.pars_tw$phi, 2), phi_range = "constant",
                   secs = NA_real_), .tail_stat(.rt_tw)),
  route2, fill = TRUE)
print(route2[, .(spec, label, p_med, phi_med, phi_range,
                 tail_obs, tail_exp, tail_pct_over, AIC_twlss)])
.write(route2, "DD_tail_route2_twlss.csv")

# ---------------------------------------------------------------------------
# ROUTE 1. Group counts x mean group size
#
# Setting size = 1 makes dsm count SCHOOLS rather than individuals. The
# detection function is ~1, so p_hat is constant and sits in the offset; the
# response stays an integer count (verified: max 10 schools per segment).
# ---------------------------------------------------------------------------
cat("\n=== ROUTE 1: group-count model ===\n")
.obs_grp <- copy(as.data.table(obsdata_dd_mod))
.obs_grp[, size := 1]

m_grp <- cached_fit_row("route1__group_model_marker", .cache, {
  g <- dsm(count ~ s(x, y, bs = "so", xt = list(bnd = .bnd_active), k = 10) +
             season + s(Ano),
           ddf.obj = df.dd, segment.data = segdata,
           observation.data = as.data.frame(.obs_grp),
           family = tw(link = "log"), method = "REML", knots = .knots_active)
  saveRDS(g, file.path(.cache, "model_route1_groups.rds"))
  data.table(done = TRUE)
})
m_grp <- readRDS(file.path(.cache, "model_route1_groups.rds"))

cat(sprintf("  group counts: integer %s, max %g, zeros %d/%d\n",
            all(abs(m_grp$y - round(m_grp$y)) < 1e-8), max(m_grp$y),
            sum(m_grp$y == 0), length(m_grp$y)))
.pars_grp <- dsm_tweedie_pars(m_grp)
cat(sprintf("  individual-count model: p = %.3f, phi = %.2f\n",
            .pars_tw$p, .pars_tw$phi))
cat(sprintf("  GROUP-count model     : p = %.3f, phi = %.2f  <- the diagnosis\n",
            .pars_grp$p, .pars_grp$phi))

.breaks_grp <- dsm_rootogram(m_grp, max_count = 10L)$upper
rt_grp <- .roots(as.numeric(m_grp$y), fitted(m_grp), .pars_grp$p, .pars_grp$phi,
                 .breaks_grp, "route 1: group counts")
cat("\n  group-count rootogram:\n"); print(rt_grp[, .(bin, observed, expected = round(expected, 1))])

route1 <- rbind(
  cbind(data.table(model = "individuals (current)", p = round(.pars_tw$p, 3),
                   phi = round(.pars_tw$phi, 2), Dev = round(summary(m_tw)$dev.expl, 3)),
        .tail_stat(.rt_tw)),
  cbind(data.table(model = "groups (route 1)", p = round(.pars_grp$p, 3),
                   phi = round(.pars_grp$phi, 2), Dev = round(summary(m_grp)$dev.expl, 3)),
        .tail_stat(rt_grp, from = 3)), fill = TRUE)
print(route1)

# ---- abundance reconciliation: N_groups * mean size vs the existing estimate --
if (!all(c("x", "y") %in% names(pred.polys_m)))
  pred.polys_m <- pred.polys_m %>%
    mutate(x = st_coordinates(st_centroid(geometry))[, 1],
           y = st_coordinates(st_centroid(geometry))[, 2])
.cell <- as.numeric(st_area(pred.polys_m))
.sy   <- unique(as.data.table(segdata)[, .(season, Ano)]); setorder(.sy, Ano, season)

.Sbar   <- .Sbar_nv                       # naive, for the reconciliation gate
.cv_S   <- sd(.o$size) / mean(.o$size) / sqrt(nrow(.o))

recon <- rbindlist(lapply(seq_len(nrow(.sy)), function(i) {
  .ssn <- as.character(.sy$season[i]); .yr <- as.integer(.sy$Ano[i])
  nd <- st_drop_geometry(pred.polys_m) %>%
    mutate(season = factor(.ssn, levels = levels(segdata$season)), Ano = .yr)
  Ng <- sum(predict(m_grp, newdata = nd, off.set = .cell, type = "response"))
  Ni <- sum(predict(m_tw,  newdata = nd, off.set = .cell, type = "response"))
  data.table(season = .ssn, year = .yr, N_groups = Ng,
             N_route1_naive     = Ng * .Sbar_nv,
             N_route1_corrected = Ng * .Sbar_sb,
             N_individuals = Ni)
}))
recon[, `:=`(ratio_naive     = round(N_route1_naive / N_individuals, 3),
             ratio_corrected = round(N_route1_corrected / N_individuals, 3))]
cat(sprintf("\n=== reconciliation gate: N_groups x S_bar vs the individual-count model ===\n"))
cat(sprintf("  S_bar naive = %.2f | S_bar size-bias corrected = %.2f\n",
            .Sbar_nv, .Sbar_sb))
print(recon[, .(mean_N_groups      = round(mean(N_groups)),
                mean_route1_naive  = round(mean(N_route1_naive)),
                mean_route1_corr   = round(mean(N_route1_corrected)),
                mean_individuals   = round(mean(N_individuals)),
                ratio_naive        = round(mean(ratio_naive), 3),
                ratio_corrected    = round(mean(ratio_corrected), 3))])
cat("  The GATE is ratio_naive ~ 1: it checks the offset and S_bar, not the biology.\n")
cat("  ratio_corrected < 1 is the size-bias correction doing its job.\n")
.write(recon, "DD_tail_route1_groups.csv")
.write(route1, "DD_tail_route1_summary.csv")

cat(sprintf("\n  CV of mean school size = %.3f -- this is a FLOOR on route 1's CV.\n", .cv_S))
cat("  It enters in quadrature with the spatial CV, so route 1 cannot produce a\n")
cat("  SMALLER interval than the current one; its payoff is a correctly specified\n")
cat("  count model and an honest split of the variance, not a tighter CI.\n")

# ---------------------------------------------------------------------------
# Figure: hanging rootograms, current vs the two routes
# ---------------------------------------------------------------------------
.best2 <- route2[spec != "tw_reference"][which.min(tail_pct_over)]
.g_best <- readRDS(file.path(.cache, sprintf("model_twlss_%s.rds", .best2$spec)))
.pb <- .twlss_pars(.g_best)
rt_best <- .roots(as.numeric(.g_best$y), .pb$mu, .pb$p, .pb$phi, .breaks_ind,
                  sprintf("route 2: %s", .best2$label))

rg <- rbindlist(list(.rt_tw[, fit := "current: tw(), individuals"],
                     rt_best,
                     rt_grp[, fit := "route 1: group counts (own bins)"]), fill = TRUE)
rg[, `:=`(sqrt_obs = sqrt(observed), sqrt_exp = sqrt(expected))]
rg[, bin_f := factor(bin, levels = unique(bin))]

p_rg <- ggplot(rg, aes(x = bin_f)) +
  geom_col(aes(y = sqrt_exp), fill = "grey80", width = .85) +
  geom_segment(aes(xend = bin_f, y = sqrt_exp, yend = sqrt_exp - sqrt_obs),
               colour = "#c0392b", linewidth = .8) +
  geom_point(aes(y = sqrt_exp - sqrt_obs), colour = "#c0392b", size = 1.3) +
  geom_hline(yintercept = 0, linewidth = .3) +
  facet_wrap(~ fit, scales = "free", ncol = 1) +
  labs(title = "Common dolphin: rootogram, current model vs the two tail routes",
       subtitle = paste("bars = sqrt(expected); red hangs to sqrt(expected) - sqrt(observed).",
                        "Bars hanging ABOVE zero = the model expects more than was seen."),
       x = "Count bin", y = "sqrt(frequency)") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        strip.text = element_text(face = "bold"))
ggsave(file.path(.out, "DD_tail_rootogram_compare.png"), p_rg,
       width = 11, height = 9, dpi = 150)
.write(rg, "DD_tail_rootogram_compare.csv")

writeLines(c(
  "DD tail misfit -- diagnosis and two routes out",
  sprintf("generated %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  strwrap(paste(
    "PROBLEM. The common-dolphin rootogram over-predicts large counts by ~17%",
    "(observed 213 vs expected 257 in bins >= 5), identically in every DD model,",
    "so it is the count distribution and not the covariates or the spatial basis."),
    width = 78),
  "",
  strwrap(paste(
    "CAUSE. Var(count) = E[N]*Var(X) + Var(N)*E[X]^2 for a compound process of N",
    "schools of size X. For DD that is 1611 + 127 = 1738 against 1630 observed:",
    "92.7% of the variance is SCHOOL SIZE. School number is nearly Poisson;",
    "school size is median 4 with a cluster at 150-450 and a maximum of 450. A",
    "Tweedie with 1<p<2 is a compound Poisson-gamma and so is structurally right,",
    "but one gamma cannot concentrate near 4 and also reach 450. LO's largest",
    "school is 11 and its rootogram is clean -- that contrast is the evidence."),
    width = 78),
  "",
  "FILES",
  "  DD_tail_variance_decomposition.csv  the decomposition above",
  "  DD_tail_groupsize.csv               school-size summary + size-bias test",
  "  DD_tail_route2_twlss.csv            route 2 ladder, tail fit per spec",
  "  DD_tail_route1_summary.csv          route 1 vs current, p / phi / tail",
  "  DD_tail_route1_groups.csv           N_groups x S_bar per season-year",
  "  DD_tail_rootogram_compare.{csv,png} the three rootograms side by side",
  "",
  strwrap(paste(
    "CAVEAT ON ROUTE 1. The detection function is ~1, so p_hat is constant and",
    "the Horvitz-Thompson mean school size is algebraically identical to the",
    "naive mean -- the current fit contains no size-bias correction and none can",
    "be extracted from it. DD_tail_groupsize.csv reports an explicit",
    "log(size) ~ distance regression instead; read it before trusting S_bar."),
    width = 78),
  "",
  strwrap(paste(
    "CAVEAT ON INTERVALS. The CV of the mean school size is a floor on route 1's",
    "CV and enters in quadrature with the spatial CV, so route 1 will not produce",
    "a smaller interval than the current model. Its payoff is a correctly",
    "specified count model and an honest attribution of the variance."), width = 78),
  "",
  strwrap(paste(
    "CAVEAT ON AIC. twlss and tw count the dispersion parameters differently, so",
    "their AICs are not comparable. AIC is used only within the twlss ladder."),
    width = 78)),
  file.path(.out, "README_DD_tail.txt"))

cat("\nwrote to", .out, "\n")
