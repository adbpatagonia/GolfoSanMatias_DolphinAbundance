DD robust (sandwich) CVs for the reported abundance -- and what a bootstrap would add
generated 2026-09-29 19:50 by analysis/UTIL_DSM_RobustCV_DD.R

THE ANSWER
The reported intervals are optimistic by more than the Pearson scalar of test
A2 says, and by different amounts in different season-years. The per-segment
sandwich puts the CV at 1.39 (1.22-1.76) times the reported one;
smoothing-parameter uncertainty adds only 1.02 (1.02-1.03). The segments with
more than 20 animals carry 0.93 (0.87-0.96) of the sandwich meat, so the
inflation is the tail. Clustering by survey day changes little, except in the
Fall combos where one cruise (201704Hidro) owns most of the clustered
variance.

A day-level nonparametric bootstrap estimates the clustered quantity, so it
would add little: few days per season-year (see DD_robustcv_design.csv),
bimodal replicates in Fall, and no effect on the uncorrected size bias, which
moves the centre of the interval rather than its width.

FILES
  DD_robustcv_by_combo.csv  CVs, ratios, reported and HC 95% CIs, tail share,
                            dominant survey day, per season-year
  DD_robustcv_summary.csv   the numbers above, one table
  DD_robustcv_design.csv    what a day-level bootstrap would have to work with
  DD_robustcv_ratios.png    the ratios by season-year

GATES PASSED
  hand-built HC vs vcov(m, sandwich = TRUE): max rel diff 0.0e+00
  lpmatrix + Vp vs dsm_var_gam() and vs DD_abundance_season_year_soap.csv

PROVENANCE
  model: dd.dsm.soap.season.year, phi 28.45, edf 51.7, n 6288, 82 survey days
