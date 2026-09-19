LO_nobs2_fit_compare.csv -- how to read it

AIC is comparable only WITHIN a row's own data set: the full-data model is
fitted on all segments and the n_obs == 2 model on the subset, so their
log-likelihoods are sums over different numbers of rows (and for the fs model
the subset has one fewer year_fac level). Do NOT difference the two AIC
values. Dev (a proportion) is comparable; the year partial-effect overlay is
the intended comparison.

Context: n_obs is the number of observers on watch. It is almost a step
function of year (n_obs == 1 occurs only 2006-2009 and 2013; n_obs == 2
dominates from 2014 on), so it cannot be separated from the year term by
putting it in the detection function as a covariate. This script instead
holds it constant by subsetting to n_obs == 2 and asks whether the year
pattern survives. Note that 2013 drops out of the subset entirely -- all 39
of its segments are n_obs == 1 -- so the subset fs model has one fewer year
level.
