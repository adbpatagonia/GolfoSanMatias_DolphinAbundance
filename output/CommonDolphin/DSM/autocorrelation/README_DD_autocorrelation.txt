DD -- residual autocorrelation: how to read these files
generated 2026-09-18 02:42

PROVENANCE (regenerate this folder whenever the models are refitted)
  species/model set : CommonDolphin
  models diagnosed  : dd.dsm.soap.season.year, dd.dsm.soap.year.season.sst, dd.dsm.soap.season.clo, dd.dsm.xy.byyear.season.clo
  segments (n)      : 6288
  soap interior knots: 89
  soap boundary      : simplify_tol 500 / margin 250

FILES
  DD_autocorrelation_summary.csv  one row per model x scale -- start here
  DD_dsm_correlogram.csv          every lag, the underlying numbers
  DD_dsm_correlogram.png          the same thing as a figure

WHAT IS BEING TESTED
A density surface model assumes the segment residuals are independent. They
are not, if animals seen on one segment make animals on the next segment more
likely than the model expects. That would not bias the abundance estimate
much, but it deflates the standard errors and inflates AIC differences -- so
both the confidence intervals and the model selection would be
over-confident. These files check that assumption by correlating each
segment's residual with the residual k segments later along the same track.

HOW TO READ IT
lag1 is the correlation between neighbouring segments -- the one that
matters. band is +/- 2/sqrt(n_pairs), a rough envelope for 'indistinguishable
from zero'. pct_of_band says how much of that envelope the model uses: 87
means lag1 is at 87% of the band, i.e. inside but not comfortably. verdict
collapses it. A single spike at a high lag with lag 1 clean is noise -- with
a dozen lags per model, a few exceedances are expected. The signal to worry
about is a RUN of positive correlations starting at lag 1.

n_eff_lag1 translates lag-1 into an AR(1)-equivalent effective sample size:
if it is much below n_segments, the model has fewer independent observations
than it thinks, and that is the practical cost.

TWO SCALES
'transect leg' lags within a transect -- the fine scale, a few km. 'survey
day' lags within a day's track -- the coarse scale. A model can pass one and
fail the other; both are reported.

CAVEAT
The band is optimistic. It treats the n_pairs residual pairs as independent
when they overlap by construction, so a correlation just inside the band is
weaker evidence of independence than it looks. Treat 'inside the band' as
'not obviously violated', not as 'verified'.
