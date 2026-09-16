# DONE - Density maps + abundance plots for the DD tuned soap arm

Requested 2026-09-15, implemented 2026-09-16 in
`analysis/UTIL_DSM_TunedArm_MapsAbundance_DD.R`. This file is kept as the record
of what was checked and what the checks found.

## Refit reproduced exactly

Both tuned models were refit (the cache held summary rows, not `gam` objects)
and saved to `output/CommonDolphin/DSM/tuned_models/`:

| model | AIC | Dev | lag-1 | expected |
|---|---|---|---|---|
| base | 6070.35 | 0.258 | 0.0247 | 6070.35 / 0.258 / 0.0247 |
| + s(sst) | 6034.93 | 0.290 | 0.0278 | 6034.93 / 0.290 / 0.0278 |

89 knots on the tol500/margin250 boundary, as specified. `s(sst)` uses edf 10.04
of k' = 19, so k = 20 was more than adequate.

## The three flagged risks, resolved

1. **Prediction outside the tuned boundary - NOT an issue.** All 1408 cells of
   `pred.polys_m` pass `in.out()` against both the stored and the tuned
   boundary, and `predict()` returns 1408/1408 finite values. No masking needed.

2. **`off.set` - the MAP script is the wrong one.** Verified against the stored
   soap model: `off.set = cell_area_m2` reproduces the published abundance
   exactly (4200.7 vs 4201, ratio 0.9999-1.0002 over four season-year combos).
   The recipe in `UTIL_Map_DSM_output_DD.R` - constant `off.set = 800 *
   trunc.dist_dd`, then divide by cell area - **understates density by 4.49x**
   (4.222-4.508 across cells). The published maps are therefore ~4.5x too low on
   the legend, but because the factor varies only 6.8% across cells the spatial
   PATTERN is essentially intact. Abundance numbers are unaffected.
   `UTIL_Map_DSM_output_DD.R` was NOT edited - that is a separate decision.

3. **`sst` on the grid - the season mean is not safe.** Within-season monthly
   spread per cell is large in Spring (median 4.29 degC) and Fall (3.62), so
   `mean(f(sst)) != f(mean(sst))`. The script predicts at each of the three
   months and averages the resulting densities, implemented by stacking the grid
   three deep with `off.set = cell_area/3` so `dsm_var_gam` propagates variance
   correctly in one call.

## Additional findings during implementation

- **55 of 1408 cells have no covariates** (64.2 km2, 3.9% of grid area, 0.35% of
  predicted N). Base and +sst are compared on the common 1353-cell footprint;
  base is additionally reported on all 1408. Footprint effect: -0.35%.
- **`pred.polys_m` has columns `x` and `y`.** Using `y` as a season/year loop
  variable inside `mutate()` silently binds `Ano` to the northing (~5e6), which
  extrapolates `s(Ano)` and turns every prediction into `Inf`. The project's own
  `5_CommonDolphin_Abundance.R` uses `a` and is safe. The new script uses
  `.ssn` / `.yr` and documents the trap.
- **No surveys in 2011-2012** (2013 is Summer only). Abundance lines are broken
  across the gap and the span is shaded, so `s(Ano)` interpolation is not read
  as a measured trend.
- **`seasonFall` = 0.0002**, so Fall and Spring are identical in the base model
  by fit, not by error.

## Results

| arm | mean N over 33 season-year combos | range |
|---|---|---|
| original base | 4907 | 1628-8652 |
| tuned base | 4541 | 1362-7797 |
| tuned + s(sst) | 4241 | 1144-8243 |

Tuning lowers the abundance series ~7% relative to the original; adding `s(sst)`
lowers it a further 6.6%, concentrated in Fall (-13.9%). Mean CV rises from
0.215 (base) to 0.240 (+sst).

## Caveat that must travel with the +s(sst) figures

`sst` is a monthly climatology with no year dimension, so the sst term
contributes the same thing in every year by construction - all interannual
variation still comes from `s(Ano)`. The series is not evidence of sst-driven
change. And `+ s(sst)` breaches the residual independence band (lag-1 0.0278 vs
0.026) while the base does not (0.0247), so it is a sensitivity, not the
preferred model.
