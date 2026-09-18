DD tail misfit -- diagnosis and two routes out
generated 2026-09-18 12:15

PROBLEM. The common-dolphin rootogram over-predicts large counts by ~17%
(observed 213 vs expected 257 in bins >= 5), identically in every DD model,
so it is the count distribution and not the covariates or the spatial basis.

CAUSE. Var(count) = E[N]*Var(X) + Var(N)*E[X]^2 for a compound process of N
schools of size X. For DD that is 1611 + 127 = 1738 against 1630 observed:
92.7% of the variance is SCHOOL SIZE. School number is nearly Poisson; school
size is median 4 with a cluster at 150-450 and a maximum of 450. A Tweedie
with 1<p<2 is a compound Poisson-gamma and so is structurally right, but one
gamma cannot concentrate near 4 and also reach 450. LO's largest school is 11
and its rootogram is clean -- that contrast is the evidence.

FILES
  DD_tail_variance_decomposition.csv  the decomposition above
  DD_tail_groupsize.csv               school-size summary + size-bias test
  DD_tail_route2_twlss.csv            route 2 ladder, tail fit per spec
  DD_tail_route1_summary.csv          route 1 vs current, p / phi / tail
  DD_tail_route1_groups.csv           N_groups x S_bar per season-year
  DD_tail_rootogram_compare.{csv,png} the three rootograms side by side

CAVEAT ON ROUTE 1. The detection function is ~1, so p_hat is constant and the
Horvitz-Thompson mean school size is algebraically identical to the naive
mean -- the current fit contains no size-bias correction and none can be
extracted from it. DD_tail_groupsize.csv reports an explicit log(size) ~
distance regression instead; read it before trusting S_bar.

CAVEAT ON INTERVALS. The CV of the mean school size is a floor on route 1's
CV and enters in quadrature with the spatial CV, so route 1 will not produce
a smaller interval than the current model. Its payoff is a correctly
specified count model and an honest attribution of the variance.

CAVEAT ON AIC. twlss and tw count the dispersion parameters differently, so
their AICs are not comparable. AIC is used only within the twlss ladder.
