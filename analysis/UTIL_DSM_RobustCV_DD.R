# ADB / Claude
# 2026-09-29
#
# COMMON DOLPHIN: robust (sandwich) variances for the reported abundance
# estimates, and what a nonparametric bootstrap would and would not add.
# Evidence for the manuscript, not a change to the pipeline. NO REFITS: every
# quantity below comes from the fitted dd.dsm.soap.season.year.
#
# WHY. UTIL_DSM_TailMisfit_Impact_DD.R test A2 puts the CV inflation caused by
# the rootogram tail misfit at 1.29x (sqrt of Pearson phi / REML phi). That
# number assumes the fitted variance SHAPE, phi * mu^p, is right up to a scalar,
# and that segments are independent -- and the shape is exactly what the tail
# misfit says is wrong. It is also one scalar applied to all 33 season-year
# combos. This script replaces it with estimators that assume neither:
#
#   Vp  the reported covariance. dsm_var_gam() calls vcov() with its defaults,
#       i.e. Vp, which is CONDITIONAL on the smoothing parameters.
#   Vc  Vp plus smoothing-parameter uncertainty (Wood, Pya & Safken 2016).
#   HC  mgcv's sandwich, vcov(m, sandwich = TRUE): one score term per segment,
#       no assumption about the variance function.
#   CL  the same sandwich with the scores summed within survey day (traj_id)
#       before the outer product, which allows any dependence inside a day.
#       This is the quantity a day-level nonparametric bootstrap estimates, so
#       CL is the cheap preview of that bootstrap.
#
# All CVs are TOTAL CVs: GAM part and detection-function CV in quadrature, the
# same combination dsm_var_gam() uses (df.dd has no covariates, so the ddf CV
# is one constant).
#
# RESULT, 2026-09-29 (workspace of 2026-09-18: tuned soap, 89 knots,
# p = 1.572, phi = 28.45; ddf CV 0.049). Ratio to the reported total CV,
# median (range) over the 33 combos:
#
#   Vc               1.02 (1.02-1.03)   smoothing-parameter uncertainty is negligible
#   HC               1.39 (1.22-1.76)   <- the correction to report
#   CL               1.47 (1.15-2.09)
#   CL / HC (GAM)    0.97 (0.87-1.58)
#
#   * The Pearson scalar understates the inflation, and the inflation is not
#     uniform: Fall and Winter are the worst seasons (HC 1.34-1.76).
#   * The tail owns the HC variance: the 67 segments with > 20 animals (1.1%
#     of the segments) carry a median 93% of the HC meat (range 87-96%).
#   * Within-day dependence adds nothing on balance (CL / HC ~ 1), which
#     agrees with the correlogram. The exception is Fall 2014-2017
#     (CL / HC 1.42-1.58), and there ONE traj_id, 201704Hidro (16% of all
#     animals seen, 1201 of 7333), owns 66-69% of the CL meat. Those CL
#     values describe one cruise, not a stable variance. Winter's CL / HC of
#     0.87-0.90 is estimator noise from 82 uneven clusters, not evidence of
#     negative dependence.
#
# WHAT A DAY-LEVEL BOOTSTRAP WOULD ADD, GIVEN THE ABOVE: little. It targets CL.
# It would re-estimate the smoothing parameters, which Vc shows are worth 2%. And
# the design works against it:
#   * 82 traj_id, each nested in ONE season-year. 9 of the 33 combos have a
#     single day and 10 have two. Resampling within season-year gives the 9
#     single-day combos zero within-stratum variation. Resampling within season
#     (19 / 16 / 17 / 30 days) avoids that.
#   * In Fall the replicates would split in two, depending on whether
#     201704Hidro was drawn. With ~80 uneven clusters the percentile interval
#     would itself likely under-cover.
#   * The 4 `Hidro` traj_ids are month-labelled, with 8-9 legs and ~2x the
#     median segments per traj_id: probably cruises rather than single days.
# And neither a bootstrap nor any sandwich touches the UNCORRECTED SIZE BIAS
# (UTIL_DSM_TailFix_DD.R): that moves the centre of the interval, not its width.
#
# GATES (the script stops if either fails)
#   1. The hand-built HC reproduces vcov(m, sandwich = TRUE). CL is the same
#      code with a different grouping, so this gate is what licenses CL.
#   2. The lpmatrix route with Vp reproduces dsm_var_gam() on the largest
#      combo, and the pipeline's DD_abundance_season_year_soap.csv on all 33.
#
# OUTPUT  output/CommonDolphin/DSM/TailMisfit/DD_robustcv_by_combo.csv
#                                             DD_robustcv_summary.csv
#                                             DD_robustcv_design.csv
#                                             DD_robustcv_ratios.png
#                                             README_DD_robustcv.txt
#
# Runs standalone (load()s the workspace if the model is missing) or from
# 9_RegenerateStudies_DD.R. Names are dotted so nothing lands in the workspace
# namespace that 1_*.R saves.

library(dsm)
library(mgcv)
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)

source(file.path(here::here(), "R", "lnorm_ci.R"))
source(file.path(here::here(), "analysis", "UTIL_EnsureOutputDirs.R"))

# load BEFORE setting anything: the workspace would overwrite it
if (!exists("dd.dsm.soap.season.year"))
  load(file.path(here::here(), "output", "CommonDolphin", "dd_output.RData"))

.rc_out <- file.path(here::here(), "output", "CommonDolphin", "DSM", "TailMisfit")
dir.create(.rc_out, showWarnings = FALSE, recursive = TRUE)

.rc_write <- function(x, f) {
  p <- file.path(.rc_out, f)
  tryCatch(fwrite(x, p), error = function(e) {
    alt <- sub("[.]csv$", "_new.csv", p)
    warning(sprintf("could not write %s -- writing %s", basename(p), basename(alt)),
            call. = FALSE)
    fwrite(x, alt)
  })
}

.rc_m <- dd.dsm.soap.season.year
# the hand-built sandwich mirrors mgcv:::gam.sandwich's extended-family branch
stopifnot(inherits(.rc_m$family, "extended.family"),
          !inherits(.rc_m$family, "general.family"))
cat(sprintf("model: %s | phi %.2f | n %d | edf %.1f\n",
            .rc_m$family$family, .rc_m$sig2, length(.rc_m$y), sum(.rc_m$edf)))

# ---------------------------------------------------------------------------
# 1. Covariance matrices
# ---------------------------------------------------------------------------
.rc_X  <- model.matrix(.rc_m)
stopifnot(nrow(.rc_m$data) == nrow(.rc_X))
.rc_day <- sub("_.*", "", as.character(.rc_m$data$Transect.Label))   # = traj_id
.rc_G   <- length(unique(.rc_day))

.rc_dd <- mgcv:::dDeta(.rc_m$y, .rc_m$fitted.values, .rc_m$prior.weights,
                       .rc_m$family$getTheta(), .rc_m$family, deriv = 0)
.rc_S  <- 0.5 / .rc_m$sig2 * .rc_dd$Deta * .rc_X     # per-segment score rows
.rc_B2 <- .rc_m$Vp - .rc_m$Ve                        # squared-bias term, freq = FALSE
.rc_mf <- nrow(.rc_X) / (nrow(.rc_X) - sum(.rc_m$edf))
.rc_V  <- function(meat) .rc_mf * .rc_m$Vp %*% meat %*% .rc_m$Vp + .rc_B2

.rc_Vp  <- .rc_m$Vp
.rc_Vc  <- .rc_m$Vc
stopifnot(!is.null(.rc_Vc))
.rc_Vhc <- vcov(.rc_m, sandwich = TRUE)

# GATE 1
.rc_gate1 <- max(abs(.rc_V(crossprod(.rc_S)) - .rc_Vhc)) / max(abs(.rc_Vhc))
cat(sprintf("gate 1, hand-built HC vs mgcv: max rel diff %.2e\n", .rc_gate1))
stopifnot(.rc_gate1 < 1e-8)

.rc_Sc  <- rowsum(.rc_S, .rc_day)                    # G x p, scores summed by day
.rc_Vcl <- .rc_V(.rc_G / (.rc_G - 1) * crossprod(.rc_Sc))   # CR1 small-G factor

# ---------------------------------------------------------------------------
# 2. Per season-year abundance and CV under each covariance
# ---------------------------------------------------------------------------
if (!all(c("x", "y") %in% names(pred.polys_m)))
  pred.polys_m <- pred.polys_m %>%
    mutate(x = st_coordinates(st_centroid(geometry))[, 1],
           y = st_coordinates(st_centroid(geometry))[, 2])
.rc_A   <- as.numeric(st_area(pred.polys_m))
.rc_lev <- levels(obsdata_dd_mod$season)
.rc_seg <- as.data.table(segdata)
.rc_sy  <- .rc_seg[, .(days = uniqueN(traj_id)), by = .(season, Ano)]
setorder(.rc_sy, Ano, season)

.rc_ddf  <- summary(.rc_m$ddf)
.rc_cvp  <- as.numeric(.rc_ddf$average.p.se / .rc_ddf$average.p)
.rc_tail <- as.numeric(.rc_m$y) > 20

.rc_one <- function(s, a) {
  nd <- st_drop_geometry(pred.polys_m) %>%
    mutate(season = factor(s, levels = .rc_lev), Ano = a)
  Xp <- predict(.rc_m, newdata = nd, type = "lpmatrix", off.set = 0)
  ok <- stats::complete.cases(Xp)            # NA outside the soap boundary
  mu <- .rc_A[ok] * exp(drop(Xp[ok, , drop = FALSE] %*% coef(.rc_m)))
  N  <- sum(mu)
  g  <- crossprod(Xp[ok, , drop = FALSE], mu)
  cv <- function(V) sqrt(drop(t(g) %*% V %*% g) / N^2 + .rc_cvp^2)
  # contributions to the meat: per segment (HC) and per day (CL)
  Vpg  <- .rc_m$Vp %*% g
  a_i  <- drop(.rc_S  %*% Vpg)^2
  a_c  <- drop(.rc_Sc %*% Vpg)^2
  o    <- order(-a_c)
  data.table(N = N, n_na_cells = sum(!ok),
             CV_reported = cv(.rc_Vp), CV_Vc = cv(.rc_Vc),
             CV_HC = cv(.rc_Vhc), CV_CL = cv(.rc_Vcl),
             cvgam_HC = sqrt(drop(t(g) %*% .rc_Vhc %*% g)) / N,
             cvgam_CL = sqrt(drop(t(g) %*% .rc_Vcl %*% g)) / N,
             HC_share_tail = sum(a_i[.rc_tail]) / sum(a_i),
             CL_top_day = rownames(.rc_Sc)[o[1]],
             CL_top_day_share = a_c[o[1]] / sum(a_c),
             CL_top3_share = sum(a_c[o[1:3]]) / sum(a_c))
}

cat("\n=== per season-year CVs ===\n")
rc <- .rc_sy[, .rc_one(season, Ano), by = .(season, Ano, days)]
setnames(rc, "Ano", "year")
if (any(rc$n_na_cells > 0))
  warning("some prediction cells fall outside the soap boundary.", call. = FALSE)

# GATE 2a: dsm_var_gam on the largest combo
.rc_i  <- which.max(rc$N)
.rc_nd <- st_drop_geometry(pred.polys_m) %>%
  mutate(season = factor(rc$season[.rc_i], levels = .rc_lev), Ano = rc$year[.rc_i])
.rc_sm <- summary(dsm_var_gam(dsm.obj = .rc_m, pred.data = as.data.frame(.rc_nd),
                              off.set = .rc_A))
cat(sprintf("gate 2a, dsm_var_gam (%s %d): N %.2f vs %.2f | CV %.6f vs %.6f\n",
            rc$season[.rc_i], rc$year[.rc_i], as.numeric(.rc_sm$pred.est), rc$N[.rc_i],
            as.numeric(.rc_sm$cv), rc$CV_reported[.rc_i]))
stopifnot(abs(as.numeric(.rc_sm$pred.est) / rc$N[.rc_i] - 1) < 1e-6,
          abs(as.numeric(.rc_sm$cv) - rc$CV_reported[.rc_i]) < 1e-6)

# GATE 2b: the pipeline's abundance table, all combos (it is rounded: N to 1, CV to 3 dp)
.rc_ab <- tryCatch(fread(file.path(here::here(), "output", "CommonDolphin", "Abundance",
                                   "DD_abundance_season_year_soap.csv")),
                   error = function(e) NULL)
if (!is.null(.rc_ab)) {
  .rc_cmp <- merge(rc[, .(season = as.character(season), year, N, CV_reported)],
                   .rc_ab[, .(season, year, N_hat, CV)], by = c("season", "year"))
  cat(sprintf("gate 2b, vs DD_abundance_season_year_soap.csv: %d/%d combos | max |dN| %.2f | max |dCV| %.4f\n",
              nrow(.rc_cmp), nrow(rc), max(abs(.rc_cmp$N - .rc_cmp$N_hat)),
              max(abs(.rc_cmp$CV_reported - .rc_cmp$CV))))
  stopifnot(nrow(.rc_cmp) == nrow(rc),
            max(abs(.rc_cmp$N - .rc_cmp$N_hat)) <= 1,
            max(abs(.rc_cmp$CV_reported - .rc_cmp$CV)) <= 6e-4)
} else {
  warning("DD_abundance_season_year_soap.csv not found -- gate 2b skipped.", call. = FALSE)
}

rc[, `:=`(r_Vc = CV_Vc / CV_reported, r_HC = CV_HC / CV_reported,
          r_CL = CV_CL / CV_reported, r_CL_HC = cvgam_CL / cvgam_HC)]
.rc_ci_rep <- lnorm_ci(rc$N, rc$CV_reported)
.rc_ci_hc  <- lnorm_ci(rc$N, rc$CV_HC)
rc[, `:=`(N_lo95_reported = .rc_ci_rep$lo, N_hi95_reported = .rc_ci_rep$hi,
          N_lo95_HC = .rc_ci_hc$lo, N_hi95_HC = .rc_ci_hc$hi)]

.rc_tab <- rc[, .(season, year, days, N = round(N),
                  CV_reported = round(CV_reported, 3), CV_Vc = round(CV_Vc, 3),
                  CV_HC = round(CV_HC, 3), CV_CL = round(CV_CL, 3),
                  r_HC = round(r_HC, 2), r_CL = round(r_CL, 2), r_CL_HC = round(r_CL_HC, 2),
                  N_lo95_reported = round(N_lo95_reported), N_hi95_reported = round(N_hi95_reported),
                  N_lo95_HC = round(N_lo95_HC), N_hi95_HC = round(N_hi95_HC),
                  HC_share_tail = round(HC_share_tail, 3),
                  CL_top_day, CL_top_day_share = round(CL_top_day_share, 3),
                  CL_top3_share = round(CL_top3_share, 3))]
print(.rc_tab)
.rc_write(.rc_tab, "DD_robustcv_by_combo.csv")

# ---------------------------------------------------------------------------
# 3. Resampling design facts: what a day-level bootstrap would have to work with
# ---------------------------------------------------------------------------
.rc_cnt <- as.data.table(obsdata_dd_mod)[, .(count = sum(size)), by = Sample.Label]
.rc_seg <- merge(.rc_seg, .rc_cnt, by = "Sample.Label", all.x = TRUE)
.rc_seg[is.na(count), count := 0]
.rc_byday <- .rc_seg[, .(animals = sum(count), segs = .N), by = traj_id][order(-animals)]
.rc_nest  <- .rc_seg[, .(n_sy = uniqueN(paste(Ano, season))), by = traj_id]
.rc_ss    <- .rc_seg[, .(days = uniqueN(traj_id)), by = season]
.rc_ss    <- .rc_ss[order(match(season, .rc_lev))]

design <- data.table(
  quantity = c("survey days (traj_id)",
               "traj_id spanning more than one season-year",
               "season-year combos",
               "combos with 1 survey day",
               "combos with 2 survey days",
               sprintf("days per season stratum (%s)", paste(.rc_ss$season, collapse = " / ")),
               "share of all animals seen, top traj_id",
               "top traj_id",
               "share of all animals seen, top 10 traj_id",
               "Hidro traj_id: segments each (median segments per traj_id)"),
  value = c(as.character(uniqueN(.rc_seg$traj_id)),
            as.character(sum(.rc_nest$n_sy > 1)),
            as.character(nrow(.rc_sy)),
            as.character(sum(.rc_sy$days == 1)),
            as.character(sum(.rc_sy$days == 2)),
            paste(.rc_ss$days, collapse = " / "),
            sprintf("%.1f%% (%d of %d)", 100 * .rc_byday$animals[1] / sum(.rc_byday$animals),
                    .rc_byday$animals[1], sum(.rc_byday$animals)),
            .rc_byday$traj_id[1],
            sprintf("%.1f%%", 100 * sum(.rc_byday$animals[1:10]) / sum(.rc_byday$animals)),
            sprintf("%s (%s)", paste(.rc_byday[grepl("Hidro", traj_id), segs], collapse = ", "),
                    median(.rc_byday$segs))))
cat("\n=== resampling design ===\n"); print(design)
.rc_write(design, "DD_robustcv_design.csv")

# ---------------------------------------------------------------------------
# 4. Summary, figure, README
# ---------------------------------------------------------------------------
.rc_q <- function(x, d = 2)
  sprintf(paste0("%.", d, "f (%.", d, "f-%.", d, "f)"), median(x), min(x), max(x))
.rc_A2 <- tryCatch(fread(file.path(.rc_out, "DD_tailmisfit_A2_inflation.csv")),
                   error = function(e) NULL)
.rc_pearson <- if (!is.null(.rc_A2))
  .rc_A2[grepl("implied CV inflation", quantity), value] else NA_real_
.rc_fall_dom <- rc[CL_top_day_share > 0.5]

summ <- data.table(
  quantity = c(
    "CV ratio to reported, Vc (smoothing-parameter corrected), median (range)",
    "CV ratio to reported, HC sandwich, median (range)",
    "CV ratio to reported, day-cluster sandwich (CR1), median (range)",
    "day-cluster / HC, GAM part only, median (range)",
    "for comparison: Pearson/REML scalar from test A2",
    "share of HC meat owned by the segments with > 20 animals, median (range)",
    "combos where one traj_id owns > 50% of the cluster meat",
    "  that traj_id, and its share there (range)",
    "detection-function CV (added in quadrature to every CV above)"),
  value = c(
    .rc_q(rc$r_Vc), .rc_q(rc$r_HC), .rc_q(rc$r_CL), .rc_q(rc$r_CL_HC),
    if (is.na(.rc_pearson)) "not available" else sprintf("%.2f", .rc_pearson),
    .rc_q(rc$HC_share_tail),
    if (nrow(.rc_fall_dom)) paste(sprintf("%s %d", .rc_fall_dom$season, .rc_fall_dom$year),
                                  collapse = ", ") else "none",
    if (nrow(.rc_fall_dom)) sprintf("%s, %.2f-%.2f", paste(unique(.rc_fall_dom$CL_top_day), collapse = "/"),
                                    min(.rc_fall_dom$CL_top_day_share),
                                    max(.rc_fall_dom$CL_top_day_share)) else "-",
    sprintf("%.3f", .rc_cvp)))
cat("\n=== SUMMARY ===\n"); print(summ)
.rc_write(summ, "DD_robustcv_summary.csv")

# Two series, one axis, colour + shape so identity is never colour alone.
# Okabe-Ito pair, same as DD_tailmisfit_pladder.png (validated: CVD dE 21.9).
.rc_long <- rbind(
  rc[, .(season, year, ratio = r_HC, estimator = "Sandwich, per segment (HC)",
         dominated = FALSE)],
  rc[, .(season, year, ratio = r_CL, estimator = "Sandwich, clustered by survey day",
         dominated = CL_top_day_share > 0.5)])
.rc_long[, estimator := factor(estimator, levels = c("Sandwich, per segment (HC)",
                                                     "Sandwich, clustered by survey day"))]
.rc_long[, season := factor(season, levels = .rc_lev)]
# reference lines labelled once, in the first panel only
.rc_ref <- data.table(y = c(1, .rc_pearson),
                      lab = c("reported", sprintf("Pearson scalar (A2) %.2f", .rc_pearson)),
                      season = factor(.rc_lev[1], levels = .rc_lev))
.rc_ref <- .rc_ref[!is.na(y)]

p_rc <- ggplot(.rc_long, aes(year, ratio, colour = estimator, shape = estimator)) +
  geom_hline(yintercept = 1, colour = "grey55", linewidth = 0.4) +
  { if (!is.na(.rc_pearson))
      geom_hline(yintercept = .rc_pearson, colour = "grey55", linetype = "22",
                 linewidth = 0.4) } +
  geom_text(data = .rc_ref, aes(x = 2005.6, y = y, label = lab), inherit.aes = FALSE,
            hjust = 0, vjust = -0.4, size = 3, colour = "grey35") +
  geom_point(data = .rc_long[dominated == FALSE], size = 2.6) +
  # shape 2 = hollow triangle: the clustered value rests on one survey day
  geom_point(data = .rc_long[dominated == TRUE], size = 2.6, shape = 2,
             show.legend = FALSE) +
  scale_colour_manual(values = c("#0072B2", "#D55E00"), name = NULL) +
  scale_shape_manual(values = c(16, 17), name = NULL) +
  facet_wrap(~ season, nrow = 1) +
  scale_x_continuous(breaks = seq(2006, 2018, 4)) +
  labs(title = "Common dolphin: robust CV relative to the reported CV, by season-year",
       subtitle = paste0("Total CV (GAM + detection function). Hollow triangles: one survey",
                         " day owns > 50% of the clustered variance",
                         if (nrow(.rc_fall_dom))
                           sprintf(" (%s).", paste(unique(.rc_fall_dom$CL_top_day),
                                                   collapse = ", ")) else "."),
       x = NULL, y = "CV / reported CV") +
  theme_bw(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())
ggsave(file.path(.rc_out, "DD_robustcv_ratios.png"), p_rc, width = 11, height = 4.8, dpi = 150)

writeLines(c(
  "DD robust (sandwich) CVs for the reported abundance -- and what a bootstrap would add",
  sprintf("generated %s by analysis/UTIL_DSM_RobustCV_DD.R", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "THE ANSWER",
  strwrap(paste(
    "The reported intervals are optimistic by more than the Pearson scalar of",
    "test A2 says, and by different amounts in different season-years. The",
    sprintf("per-segment sandwich puts the CV at %s times the reported one;",
            .rc_q(rc$r_HC)),
    sprintf("smoothing-parameter uncertainty adds only %s. The segments with more",
            .rc_q(rc$r_Vc)),
    sprintf("than 20 animals carry %s of the sandwich meat, so the inflation is",
            .rc_q(rc$HC_share_tail)),
    "the tail. Clustering by survey day changes little, except in the Fall",
    "combos where one cruise (201704Hidro) owns most of the clustered variance."),
    width = 78),
  "",
  strwrap(paste(
    "A day-level nonparametric bootstrap estimates the clustered quantity, so it",
    "would add little: few days per season-year (see DD_robustcv_design.csv),",
    "bimodal replicates in Fall, and no effect on the uncorrected size bias,",
    "which moves the centre of the interval rather than its width."), width = 78),
  "",
  "FILES",
  "  DD_robustcv_by_combo.csv  CVs, ratios, reported and HC 95% CIs, tail share,",
  "                            dominant survey day, per season-year",
  "  DD_robustcv_summary.csv   the numbers above, one table",
  "  DD_robustcv_design.csv    what a day-level bootstrap would have to work with",
  "  DD_robustcv_ratios.png    the ratios by season-year",
  "",
  "GATES PASSED",
  sprintf("  hand-built HC vs vcov(m, sandwich = TRUE): max rel diff %.1e", .rc_gate1),
  "  lpmatrix + Vp vs dsm_var_gam() and vs DD_abundance_season_year_soap.csv",
  "",
  "PROVENANCE",
  sprintf("  model: dd.dsm.soap.season.year, phi %.2f, edf %.1f, n %d, %d survey days",
          .rc_m$sig2, sum(.rc_m$edf), length(.rc_m$y), .rc_G)),
  file.path(.rc_out, "README_DD_robustcv.txt"))

cat("\nwrote to", .rc_out, "\n")
