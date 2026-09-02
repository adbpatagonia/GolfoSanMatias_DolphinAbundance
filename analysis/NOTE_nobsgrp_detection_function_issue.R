# ADB
# 2026-09-01
#
# THIS FILE IS A WRITTEN RECORD, NOT A SCRIPT TO RUN. It is not sourced by
# 1_CommonDolphin.R / 1_DuskyDolphin.R or anything else in the pipeline.
# It documents why nobs_grp (number of observers, grouped) was investigated
# as a detection-function covariate and then dropped, so the reasoning is not
# lost the next time someone is tempted to re-add it. Referenced from the
# "Final Detection Function" section of 3_CommonDolphin_DetectionFunction.R
# and 3_DuskyDolphin_DetectionFunction.R.
#
# ============================================================================
# 1. WHY nobs_grp WAS TRIED AT ALL
# ============================================================================
#
# Using detection functions with NO covariates, ADB found a clear increasing
# trend in common-dolphin (dd) abundance over time. The data providers raised
# the possibility that this is an artefact: if the number of observers on
# effort changed systematically over the study period, and more observers
# means higher detection probability, that alone could produce an apparent
# increasing trend even if true abundance were flat.
#
# ADB's first response was to add n_obs -- grouped as nobs_grp, "1" observer
# vs "> 1" -- as a covariate in the detection function (Distance::ds(...,
# formula = ~nobs_grp)) for both species, intending to correct for this by
# letting detection probability depend on effort. That produced
# dd.df.hr.trun.cp.nobsgrp and lo.df.hn.trun.cp.nobsgrp.
#
# ============================================================================
# 2. WHAT WENT WRONG: dusky dolphins (numerically degenerate)
# ============================================================================
#
# The abundance pipeline (5_DuskyDolphin_Abundance.R) computes variance with
# dsm_var_gam(), which combines the DSM's own GAM uncertainty with the
# detection function's marginal average-p uncertainty
# (summary(ddf)$average.p.se / summary(ddf)$average.p) in quadrature. With
# lo.df.hn.trun.cp.nobsgrp as the detection function, EVERY row (33 of 33) in
# EVERY one of the four dusky abundance tables (season_year, byyear, fsyear,
# soap) came back with the identical CV = 63990.286 (i.e. ~6.4 million %),
# regardless of season, year, or model structure -- clearly systemic, not
# sparse-stratum noise.
#
# Traced directly (see the ddf summary):
#
#   average.p     = 0.1775
#   average.p.se  = 11357.01
#   cv_p (se/p)   = 63990.29
#
#   Detection function parameters
#   Scale coefficient(s):
#                estimate           se
#   (Intercept)  5.094764 8.034332e-02
#   nobs_grp1   -2.978671 1.000000e+05      <- SE = 100,000, suspiciously round
#
#   hessian:
#              [,1]         [,2]
#   [1,] 1.549175e+02 3.567116e-14
#   [2,] 3.567116e-14 3.566837e-14          <- curvature for nobs_grp1 ~ 0
#   eigenvalues: 154.917495, 0
#
# The second eigenvalue is zero to machine precision: the likelihood surface
# is essentially flat along the nobs_grp1 direction at the fitted value.
# nlminb still reports "successful convergence" (no warning was thrown), but
# the parameter isn't actually identified -- inverting that near-zero
# curvature is what produces SE = 1e5 and the resulting CV = 63990 that then
# poisons every downstream abundance estimate's CV, regardless of which dsm
# variance function is used (dsm_var_gam and dsm_var_prop both rely on
# summary(ddf)$average.p.se).
#
# Why: of the 108 detections used in this fit (post-truncation), only 9 have
# nobs_grp == "1" -- far too few to pin down a coefficient and its curvature.
#
# This also undermines the AIC-based justification for picking the nobsgrp
# model in the first place: lo.df.hn.trun.cp (no covariate) AIC = 299.24 vs
# lo.df.hn.trun.cp.nobsgrp AIC = 276.17, a seemingly convincing ~23-point
# drop. An AIC "improvement" driven by a coefficient sitting on a degenerate,
# flat part of the likelihood is not trustworthy -- it can look like a large
# fit improvement while really reflecting an unidentified boundary solution.
#
# ============================================================================
# 3. COMMON DOLPHINS: the nobsgrp fit itself is fine, but the covariate is
#    still not usable
# ============================================================================
#
# The dd.df.hr.trun.cp.nobsgrp fit does NOT have the dusky problem. It is
# numerically healthy:
#
#   632 detections used (35 with nobs_grp == "1", vs dusky's 9 of 108)
#   hessian eigenvalues: 382.97, 65.92, 8.64      -- nothing near zero
#   nobs_grp1: estimate = -1.773, se = 0.329        -- sane SE
#   Average p CV = 0.087 (8.7%)                     -- sane
#   AIC: 2273.32 (no covariate) -> 2248.87 (nobs_grp), a credible ~24-point
#     drop, not built on a degenerate Hessian this time
#
# So the numerical failure mode from section 2 is dusky-specific (small
# sample). But a second, independent problem applies to BOTH species, because
# segdata (survey effort) is shared between them. The year x n_obs cross-tab
# (segments, from segdata):
#
#           n_obs=1  n_obs=2  n_obs=3  n_obs=4
#   2006       31       49       25       0
#   2007      196      387       77       0
#   2008      293      260       91       0
#   2009       22      265       69       0
#   2010        0      129      150       0
#   2013       39        0        0       0
#   2014        0      235      130     124
#   2015        0      764      295       0
#   2016        0     1088      188       0
#   2017        0     1075      113       0
#   2018        0      144        0       0
#
# n_obs == 1 occurs ONLY in 2006-2010 and never again; n_obs is almost always
# 2 from 2015 onward. Observer count is close to a step function of year, not
# merely "correlated" with it. This cuts two ways:
#
#   - It means the data providers' concern is legitimate: effort genuinely
#     shifted over the study period in a way that plausibly tracks
#     detectability.
#   - It also means a covariate-based correction is fighting near-total
#     collinearity with the year trend. Even where nobs_grp is well-estimated
#     (dd), its coefficient is substantially standing in for "early period vs
#     late period" -- which is exactly what s(Ano) is also trying to
#     estimate. A regression can't cleanly separate "detectability changed"
#     from "abundance changed" when the covariate's distribution barely
#     overlaps across time.
#
# This is why nobs_grp was dropped for BOTH species, not just dusky: for
# dusky it is numerically unusable; for common dolphins it is numerically
# fine but not statistically able to answer the question it was added for.
#
# ============================================================================
# 4. WHY "add n_obs to the dsm() COUNT-MODEL formula" WAS ALSO REJECTED
# ============================================================================
#
# An alternative considered (and rejected by ADB before this was written up
# further): put n_obs directly into the dsm() spatial/temporal GAM formula
# (count ~ s(x,y) + s(Ano) + n_obs + ...) instead of the detection function.
#
# This is the wrong tool for two reasons:
#   1. n_obs is a DETECTABILITY covariate (it affects whether an animal
#      present is recorded), not a density covariate. Its methodologically
#      correct home, if used at all, is the detection function/offset, not
#      an additive term competing with the spatial/temporal density surface.
#   2. Per section 3, n_obs is close to collinear with year/Ano by
#      construction in this data. Putting both s(Ano) and n_obs in the same
#      count-model formula gives the GAM no principled way to divide the
#      variance between them -- whatever comes out reflects which term the
#      fit happens to favour (basis choice, penalty, convergence path), not
#      evidence about whether the trend is real. It cannot answer the
#      question the data providers raised.
#
# See 6_CommonDolphin_Nobs2SensitivityAnalysis.R /
# 6_DuskyDolphin_Nobs2SensitivityAnalysis.R for the rationale behind the
# approach actually used instead: holding n_obs constant by subsetting to
# n_obs == 2, rather than modelling it as a covariate anywhere.
#
# ============================================================================
# 5. A DETOUR: could dsm_var_prop() work around the dusky CV problem?
# ============================================================================
#
# Before the decision in section 3-4 was reached, dsm_var_prop() (dsm's
# variance-PROPAGATION estimator, meant for covariate detection functions)
# was investigated as a possible fix for the dusky CV blowup, since it
# doesn't rely on the same average.p/average.p.se computation dsm_var_gam
# does. Two independent problems were found; neither is a fix.
#
# 5a. A real, reproducible bug for a single (non-list) ddf object.
# dsm_var_prop() unconditionally builds its result as:
#   result <- list(..., model.check = varprop_check(varp), ...)
# varprop_check() assumes dsm.obj$ddf is a LIST of ddf objects:
#   for (i in seq_along(object$old_model$ddf))
#     parskel[[i]] <- object$old_model$ddf[[i]]$par
# Our ddf.obj (from Distance::ds()) is a single ds/ddf object, not a list, so
# `object$old_model$ddf[[i]]` walks the ddf object's OWN internal fields
# instead (e.g. its own $par, a plain numeric vector), and `[[i]]$par` on
# that atomic vector errors: "$ operator is invalid for atomic vectors". The
# crash is entirely inside this diagnostic sub-step -- the actual
# variance-propagation numbers (dsm:::dsm_varprop()) compute fine before it,
# but because model.check is built unconditionally in the same list() call,
# the whole dsm_var_prop() call fails and nothing is returned.
#
# Passing ddf.obj = list(df.dd)/list(df.lo) to dsm() does NOT fix this: dsm()
# collapses any length-1 list straight back to the bare object before it
# would strip the Distance::ds() wrapper (see dsm:::dsm(), the
# `if (length(ddf.obj) == 1) ddf.obj <- ddf.obj[[1]]` line), so the fitted
# model ends up with $ddf as an un-unwrapped "dsmodel" object -- worse, not
# better (verified: predictions from a model fitted this way don't even match
# the model's own point estimate from a bare ddf.obj fit).
#
# A wrapper (dsm_var_prop_safe(), tested but not committed anywhere in this
# repo) that reuses dsm_var_prop()'s exact logic but wraps just the
# varprop_check() diagnostic in tryCatch avoids the crash with NO refitting
# of the 4_*.R models required. That surfaced problem 5b.
#
# 5b. The refit itself changes the point estimate -- not just the variance.
# dsm_var_prop's core machinery (dsm:::dsm_varprop()) refits the whole GAM
# via gam.fixed.priors() with an added fixed-prior "XX" term representing
# detection-function parameter uncertainty. dsm's own documentation states
# this added random effect "has zero mean and hence no effect on point
# estimates". That was not what was observed. Tested on lo.dsm.xy.season.year
# (dusky), evaluated at the term's zero mean, across 5 season x year strata:
#
#   stratum          n_seg   plain predict()   dsm_var_prop refit   diff
#   Spring 2006        37         978.3               265.5        -72.9%
#   Fall   2006        49          48.0                20.8        -56.8%
#   Fall   2010        63         430.0               219.9        -48.9%
#   Winter 2017       237          17.7                 9.2        -48.2%
#   Summer 2018       144          55.0                35.1        -36.2%
#
# Confirmed two independent ways (dsm's own predict.dsm response path, and a
# manual lpmatrix reconstruction) -- both agree the refit really does predict
# lower abundance, not a computation mistake on our side. Directly comparing
# the original model to the refit (both at XX = 0) shows WHY: the refit's
# smoothing parameters differ materially --
#
#   sp   orig : 0.0945 (s(x,y)), 0.0878 (s(Ano))
#   sp   refit: 0.1461 (s(x,y)), 0.0956 (s(Ano))     -- +55% on s(x,y)
#
# -- and dozens of spline basis coefficients shift by up to ~17 units on the
# log-link scale. REML re-selects smoothing parameters jointly with the new
# fixed-prior term present, and that shift is large enough to move the point
# estimate substantially, contradicting the "no effect on point estimates"
# claim.
#
# 5c. Corroborating context (not proof, but consistent).
#   - dsm 2.3.4 (installed) is the current CRAN release (published
#     2025-09-02) -- there is no newer version to try.
#   - GitHub issue #29 on DistanceDevelopment/dsm ("Variance results
#     contrast") reports var.prop() giving a CV ~55x larger than var.gam(),
#     with implausible confidence intervals, on unrelated data -- left open
#     without a documented fix. This corroborates that dsm_varprop/var.prop
#     is a recognised soft spot in the package generally, not unique to this
#     dataset.
#   - The changelog shows real point-estimate-affecting bugs fixed in this
#     exact function before: v2.3.0 ("fixed dsm_varprop (gam.fixed.priors)
#     for response distributions with fixed scale parameters. Negative
#     binomial results before now may have been incorrect!") and v2.3.2
#     ("fixed bug in offset calculation in dsm_varprop se estimation where
#     offsets were logged twice"). That's a track record of exactly this bug
#     class, with no guarantee every case is caught -- the fix history only
#     names negative binomial explicitly, not the Tweedie family used here.
#   - An attempt to isolate the mechanism further, by forcing the refit's
#     smoothing parameters to match the original model's (rather than
#     letting REML re-select them), required hand-reproducing dsm's internal
#     machinery (gam.fixed.priors() is unexported) and produced its own bugs
#     (Inf results; the forced sp vector was silently rejected by mgcv as
#     "too short"). That attempt was not reliable enough to draw further
#     conclusions from, and a trustworthy version would mean forking and
#     testing a patched copy of dsm's internals -- judged out of proportion
#     to this task.
#
# Decision: do not use dsm_var_prop anywhere in this pipeline; do not open an
# issue on the dsm GitHub repo (explicit instruction from ADB). Keep
# dsm_var_gam, and keep detection functions covariate-free so the CV = 63990
# problem (section 2) never arises in the first place.
#
# ============================================================================
# 6. BOTTOM LINE
# ============================================================================
#
# df.dd <- dd.df.hr.trun.cp   (3_CommonDolphin_DetectionFunction.R)
# df.lo <- lo.df.hn.trun.cp   (3_DuskyDolphin_DetectionFunction.R)
#
# Both covariate-free. The observer-effort concern that motivated nobs_grp is
# real and still needs answering -- just not by putting a confounded
# covariate into either the detection function or the count model. It is
# addressed by 6_CommonDolphin_Nobs2SensitivityAnalysis.R and
# 6_DuskyDolphin_Nobs2SensitivityAnalysis.R instead: refit the best model per
# species on the subset of segments/detections where n_obs == 2 (observer
# effort held constant), and check whether the year trend survives.
