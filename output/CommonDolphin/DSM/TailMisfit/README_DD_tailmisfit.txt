DD rootogram tail misfit -- how much does it affect the abundance estimates?
generated 2026-09-18 12:27

THE ANSWER
The tail misfit is a misspecification of the VARIANCE FUNCTION, not of the
mean. Under quasi-likelihood a misspecified variance function leaves the mean
estimator consistent and affects only its precision, and that is what the
tests find: fixing the Tweedie power anywhere in [1.50, 1.80], a range
bracketing the estimated 1.572, moves the abundance by 5.7% (15.4% over the
wider [1.30, 1.80]). The interval is a different matter. The residuals imply
a dispersion 1.68 times the REML estimate, i.e. a CV about 1.29 times the
reported one. That inflation is entirely the tail: excluding the 67 segments
holding more than 20 animals the Pearson phi is 10.6, BELOW the REML 28.5 --
so without those segments the residuals imply less dispersion than the model
assumes, not more.

So: report the abundance point estimates as they stand; treat the confidence
intervals as optimistic, and say by roughly how much.

FILES
  DD_tailmisfit_summary.csv       the numbers above, one table
  DD_tailmisfit_A_bins.csv        the rootogram in the currency of animals
  DD_tailmisfit_A2_inflation.csv  the variance inflation, and the tail's share
  DD_tailmisfit_B_pladder.csv     the variance-function ladder
  DD_tailmisfit_C_designcheck.csv model vs design-based CV
  DD_tailmisfit_pladder.png       the ladder as a figure

HOW TO READ TEST A -- AND A TRAP THAT WAS FALLEN INTO FIRST
Do NOT compare the fitted mean against the observed count WITHIN bins defined
by the observed count. That conditions on the outcome: a segment that
recorded 450 animals is 'under-predicted' and a segment that recorded 0 is
'over-predicted' by shrinkage alone, whatever the fit. An earlier version of
this analysis did that and reported a -98% error in the >100 bin, which
measured conditioning rather than misfit.

The table instead compares, per bin, how many SEGMENTS the fitted
distribution expects there -- a sum of per-segment bin probabilities, which
never looks at that segment's own count -- against how many landed there.
Multiplying the segment discrepancy by the bin's mean observed count
expresses the misfit in animals.

CAVEAT ON TEST B -- THE CV COLUMN IS CONFOUNDED
Fixed-p Tweedie() re-estimates phi and the smoothing parameters, so mu-hat is
free to move between rungs; that is what makes flat abundance informative.
Convergence and edf_xy are recorded per rung because a rung that failed to
converge would look like insensitivity. Fixed-p fits return AIC = NA by
design and are not comparable to the reported model on AIC.

But the CV column is NOT a sensitivity to p. tw() estimates phi by REML;
fixed-p Tweedie() uses a Pearson-type scale estimator. At p = 1.60 --
essentially the estimated 1.572 -- phi comes back 67.6 against tw()'s 28.45,
and the CV 0.295 against 0.194. That gap is the ESTIMATOR, and the tail is
what drives a residual-based estimator up. Read the ladder's CV column as
'what the interval becomes under a residual-based scale estimate', and read
test A2 for the attribution.

CAVEAT ON TEST C
The design-based CV is a stratum-level encounter-rate quantity from a survey
with 2011-2012 unsurveyed and 2013 at one season only, and several
season-year combos carry no usable value. It is an order-of-magnitude
comparator, not a calibration standard. Do not quote it as a validation.

WHAT WAS NOT DONE
A nonparametric bootstrap would give a genuinely distribution-free interval,
but it is ~100 soap refits (~2.5 h) for a quantity the ladder already bounds.
If it is ever run, the resampling unit must be the SURVEY DAY (traj_id)
rather than the transect leg: that is the scale at which the correlogram
found residual structure, so leg-level resampling would break the dependence
it is meant to preserve.

PROVENANCE
  model      : tuned soap, 89 knots, boundary tol500/margin250
  estimated p: 1.572   phi: 28.45
  reference  : 4 seasons at Ano = 2015, summed over the 1408-cell grid
  diagnosis  : see output/CommonDolphin/DSM/tail/ (UTIL_DSM_TailFix_DD.R)
