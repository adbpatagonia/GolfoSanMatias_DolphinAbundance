# Dolphin Abundance in the San Matías Gulf, Argentina

Distance sampling and density surface models (DSMs) for two dolphin species in the San Matías Gulf, Patagonia, Argentina.

**Species:**
- *Delphinus delphis* — Common dolphin (`dd`)
- *Lagenorhynchus obscurus* — Dusky dolphin (`lo`)

- **Survey period:** 2006–2018 (11 years; no survey in 2011 or 2012), 6288 transect segments
- **Website:** <https://adbpatagonia.github.io/GolfoSanMatias_DolphinAbundance/>

**Two questions:**
1. Does dolphin density vary by season?
2. Did dolphin density change across survey years?

---

## Quick start (RStudio)

**Just want to read the results?** Open the [website](https://adbpatagonia.github.io/GolfoSanMatias_DolphinAbundance/). Nothing to install.

**Want to re-run the analysis?**

1. Open `GolfoSanMatias_DolphinAbundance.Rproj`. This sets the working directory — every path in the code uses `here::here()`, so nothing else needs configuring.
2. Put the input data in `data/DistanceData/` (see [The data](#the-data) — **it is not in this repository**).
3. Install the packages (see [The packages](#the-packages)).
4. Run one species in the Console:

   ```r
   source(here::here("analysis", "1_CommonDolphin.R"))   # common dolphin
   source(here::here("analysis", "1_DuskyDolphin.R"))    # dusky dolphin
   ```

   Each one runs that species' whole pipeline, start to finish. **Expect hours** — almost all of it is model fitting.

5. Optionally regenerate that species' side studies (minutes) — **in the same R session**, right after its master script:

   ```r
   source(here::here("analysis", "9_RegenerateStudies_DD.R"))   # after 1_CommonDolphin.R
   source(here::here("analysis", "9_RegenerateStudies_LO.R"))   # after 1_DuskyDolphin.R
   ```

6. Rebuild the reports: **Build** pane → *Render Website*. Output goes to `docs/`.

---

## What you need

### The data

The CSV files in `data/DistanceData/` are **not in this repository** — `.gitignore` excludes the whole folder. Ask the project owner for them and copy them in before you start. The shapefiles in `data/shp/` *are* tracked, so you already have those. (`data-raw/` is currently empty.)

This mirrors how the `.RData` workspaces are handled: too big or too private for git, regenerated or supplied locally.

### The packages

These are the packages the code actually loads:

```r
install.packages(c(
  "dsm", "Distance", "mrds", "mgcv",      # distance sampling and GAMs
  "sf", "terra",                          # spatial data
  "data.table", "tidyverse",              # data wrangling
  "ggplot2", "plotly", "patchwork", "viridis",  # plots
  "gratia", "tweedie", "gam.hp",          # GAM diagnostics and helpers
  "here", "knitr", "kableExtra"           # project paths and reports
))
```

If `dsm` is not on CRAN when you try, install it from GitHub instead:
`remotes::install_github("DistanceDevelopment/dsm")`.

`DESCRIPTION` only declares the handful of packages the `R/` functions need, so `devtools::install_deps()` will not cover the analysis scripts. Use the call above.

---

## Running the analysis

Each species has **one master script** that sources everything else in order:

| | |
|---|---|
| `analysis/1_CommonDolphin.R` | common dolphin, whole pipeline |
| `analysis/1_DuskyDolphin.R` | dusky dolphin, whole pipeline |

They run these steps, in this order:

| Step | Script | What it does |
|------|--------|--------------|
| — | `UTIL_EnsureOutputDirs.R` | Creates the `output/` folder tree. Must run first — `ggsave()` and `fwrite()` do not create parent folders, they abort |
| 0 | `0_ReadData_Plots.r` | Reads all data, projects to EPSG:22193, maps the environmental covariates |
| 2 | `2_*_EDA.R` | Exploratory analysis: ship effect, Beaufort filtering, covariate overlays |
| 3 | `3_*_DetectionFunction.R` | Fits and selects the detection function; sets the truncation distance |
| 4 | `4_*_DSM.R` | Fits the 60 thin-plate / factor-smooth / by-year candidates; builds the selection table |
| 4 | `4_*_DSM_soap.R` | Fits the 25 soap-film candidates; builds the soap and the combined selection tables |
| 5 | `5_*_Abundance.R` | Abundance and density with 95% CIs (see below) |
| 6 | `6_*_Nobs2SensitivityAnalysis.R` | Re-runs the trend on segments with 2 observers only, to test whether the time trend is an observer artefact |
| — | `UTIL_Map_DSM_output_*.R` | Density maps (see *Maps*) |
| — | `UTIL_Map_DSM_output_CV_*.R` | Per-cell uncertainty map |
| — | `UTIL_DSM_Diagnostics.R` | Residual autocorrelation, basis dimension, rootograms (see *Diagnostics*) |

**Step 5 in more detail.** It estimates abundance (N̂) and density (dolphins km⁻²) with 95% lognormal CIs, for every surveyed season × year.

- Four models per species are run side by side (see *Models*).
- Model-based variance comes from `dsm_var_gam` — the delta method over the per-cell-area prediction grid.
- One design-based Horvitz–Thompson estimate is computed and reused for all four, because it does not depend on the model. Its CV combines a transect-level encounter-rate CV (Fewster et al. 2009, estimator R2) with the detection CV.
- Writes summary CSVs and plots to `output/<Species>/Abundance/`.
- Uses `R/lnorm_ci.R` and `R/year_partial_effect.R`.

### The second driver

`9_RegenerateStudies_DD.R` and `9_RegenerateStudies_LO.R` regenerate the study outputs that are **not** part of the pipeline — they refit models the pipeline already fitted, or fit ones it deliberately does not. Run them after the master script, in the same session. They are kept separate so the main run does not pay for output nobody needs every time.

Three DD studies cannot be re-run at all: they exist to document the soap retune of 2026-09-16, and the configuration they measure no longer exists. Their CSVs are decision records — restore them, do not rebuild them. `9_RegenerateStudies_DD.R` says which.

### Where the output goes

```
output/
  EnvVars/                     environmental covariate maps (shared)
  CommonDolphin/
    EDA/                       exploratory plots
    DSM/                       density maps, selection tables
      autocorrelation/         correlograms
      tail/  TailMisfit/  tuned_models/    DD-only studies
    Abundance/                 abundance and density estimates
    Nobs2Sensitivity/          observer sensitivity analysis
    dd_output.RData            the whole workspace (~2 GB)
  DuskyDolphin/                same, minus the DD-only study folders
    lo_output.RData
```

The `.RData` files hold the entire workspace for that species, including the diagnostics objects `dd.diag` / `lo.diag`. They are gitignored and are what the Quarto reports `load()`, so you can re-render a report without refitting anything.

> The masters save with `save(list = ls(envir = .GlobalEnv), ...)`, **not** `save.image()`. `save.image()` writes dotted names too, so every config object created during the run (`.cfg`, `.SPP`, `.diag_dir` …) landed in the `.RData` and then clobbered the same names in the next script that loaded it. `ls()` keeps them out.

> ⚠️ `_quarto.yml` has no `render:` block, so *Render Website* renders **every** `.qmd` in the project root — including any `*_extras.qmd` working drafts sitting there.

---

## Repository structure

```
analysis/           R scripts, numbered by execution order
  1_*.R               master scripts (run these)
  9_*.R               second driver: non-pipeline studies
  UTIL_*.R            maps, diagnostics, and one-off tuning studies
  RUNBOOK_*.md        step-by-step procedure for a clean re-run
  archive/            superseded versions
R/                  package functions used by the pipeline:
                      lnorm_ci.R             lognormal CI from N and CV
                      improv.r               % improvement between two values
                      year_partial_effect.R  per-year effect from an fs smooth
                      make_surveyID.R        unique survey ID from vessel x year x month
                      cached_fit_row.R       on-disk cache for slow model fits
                      dsm_correlogram.R      along-track residual autocorrelation
                      dsm_rootogram.R        Tweedie rootogram
data/
  DistanceData/     distance sampling input CSVs (NOT in the repo)
  shp/              survey area, prediction grid, coastline shapefiles
output/             generated figures, tables and model output
assets/            manuscript draft, reference PDFs, supplementary material
references/        BibTeX bibliography
tests/testthat/    unit tests for R/
man/               roxygen documentation
*.qmd              Quarto reports, rendered into docs/
docs/              built website (GitHub Pages)
```

The project is also an R package, `gsmdolphins`, so RStudio shows a **Build** pane:

| Shortcut | Does |
|---|---|
| Ctrl+Shift+B | Install and restart |
| Ctrl+Shift+T | Run the `testthat` suite |
| Ctrl+Shift+D | Roxygenise `R/` into `man/` |

`.Rbuildignore` keeps `analysis/`, `data/`, `output/`, `docs/` and the `.qmd` files out of the package build. Only three of the seven `R/` files currently have `man/` pages.

---

# Reference

Everything below is background you consult when a question comes up. You do not need it to run the pipeline.

## Input data

All inputs live under `data/`. Paths are resolved with `here::here()`, so keep the folder names exactly as below.

### Distance-sampling tables — `data/DistanceData/`

Most files are semicolon-delimited (`;`); `preddataVV.csv` and `segdata_gui_vana.csv` are comma-delimited, and some headers carry trailing spaces. `fread` detects all of this, so nothing needs converting. Coordinates `x`/`y` are in EPSG:22193.

| File | Used for | Key columns |
|------|----------|-------------|
| `distdata_ddwholesample.csv` | Common dolphin sightings | object, Effort, distance, size, beaufort, ship, latitude, longitude, x, y, Dia, Mes, Ano, Sample.Label |
| `distdata_lowholesample.csv` | Dusky dolphin sightings | same as above |
| `obsdata_dd.csv` | Links common dolphin sightings to segments | object, Sample.Label, size, distance, Effort, latitude, longitude, x, y, Ano |
| `obsdata_lo.csv` | Same, dusky dolphin | same as above |
| `segdata.csv` | Transect segments: effort + environmental covariates | x, y, Effort, Transect.Label, Mes, Mes_n, Ano, est, Sample.Label, dist.coast, slope, depth, sst, clo, grad, dist.up |
| `segdata_gui_vana.csv` | Observer counts and vessel type, joined onto the segments | Sample.Label, N_ObsAcordado (renamed `n_obs`), vessel type, … |
| `preddataVV.csv` | **The prediction grid** (1353 cells × 12 months) | id, x, y, area, dist_coast, slope, depth, sst, clo, grad, dist_up, Mes_n, VelVert |

Two more files sit in the folder and are not part of the analysis:

- `preddata.csv` — an older prediction grid without `VelVert`. `0_ReadData_Plots.r` reads it and then immediately overwrites the object with `preddataVV.csv`, so that read does nothing.
- `distdata_dd_todos2.csv` — read into `distdata_ddoption2`, but the providers confirmed it must not be used for analysis.

### Spatial layers — `data/shp/`

Keep every sidecar file (`.shp`, `.shx`, `.dbf`, `.prj`, and any `.qpj`/`.sbn`/`.sbx`) together.

| Layer | Used for |
|-------|----------|
| `survey.area.*` | Survey-area polygon — defines the region and its total area |
| `gridproy41.1.*` | Prediction-grid polygons (~1.10–1.17 km² cells; not equal-area) |
| `Patagonia_Completa.*` | Coastline for maps |

---

## Detection functions

| Species | Key function | Truncation | Beaufort kept | Covariates |
|---------|-------------|------------|---------------|------------|
| Common dolphin | Hazard-rate | 325 m | ≤ 4 | none |
| Dusky dolphin | Half-normal | 450 m | ≤ 3 | none |

Distance bins use cutpoints, to absorb the rounding heaps at favoured distances.

Covariates were tested — `nobs_grp`, `ship`, `size_sc`, `beaufort_fct`/`beaufort_grp`, and pairs — but **neither final detection function uses one**. `nobs_grp` (number of observers, "1" vs "> 1") was the one that mattered, and it was rejected: `n_obs` is nearly a step function of year (1 only in 2006–2010, almost always 2 from 2015 on), so it cannot be separated from the year trend. For dusky dolphin it was also numerically degenerate — only 9 of 108 detections had `nobs_grp == "1"`, and the resulting SE inflated every downstream CV to nonsense. See `analysis/NOTE_nobsgrp_detection_function_issue.R`, and `6_*_Nobs2SensitivityAnalysis.R` for how the trend question is addressed instead: by holding the observer count constant through subsetting rather than modelling it.

---

## Density surface models

DSMs use `dsm::dsm()` with `family = tw(link = "log")` — mgcv's extended Tweedie, which **estimates the power parameter *p* inside each fit**. Offset = `segment_length × truncation_distance`. Every model uses `gam` with `method = "REML"`. Selection is by AIC, reported alongside deviance explained, the estimated *p*, and a residual autocorrelation flag.

### The four models that get reported

| Object | Formula | Spatial term |
|--------|---------|---------------|
| `*.dsm.xy.season.year` | `count ~ s(x,y) + season + s(Ano)` | one shared thin-plate surface |
| `*.dsm.xy.fsyear.season` | `count ~ s(x,y,year_fac,bs="fs") + season` | year-varying, **shrunk** — one smoothing parameter shared across years, so data-poor years pull toward the common pattern |
| `*.dsm.xy.byyear.season` | `count ~ s(x,y,by=year_fac) + year_fac + season` | year-varying, **unshrunk** — an independent surface per year |
| `*.dsm.soap.season.year` | `count ~ s(x,y,bs="so") + season + s(Ano)` | one shared soap-film surface, which respects the coastline instead of smoothing across it |

`*` = `dd` or `lo`. All four are estimated and compared against the same design-based check in step 5, and all four are mapped.

**The reported model differs by species:** soap for common dolphin, `fs` for dusky. Read ΔAIC *within* a block, not across the whole combined table.

`year_partial_effect()` (`R/year_partial_effect.R`) pulls an interpretable, area-weighted per-year effect with a proper covariance-based CI out of the `fs` model, where year is not a standalone coefficient the way it is under `s(Ano)` or `by=year_fac`.

### Why `tw()` rather than a fixed *p*

*p* used to be fixed per species (common 1.58, dusky 1.31) by an AIC grid search, now retired to `analysis/archive/`. That search was a profile likelihood over *p* that **paid no degrees of freedom for the *p* it chose**, and it forced every model in the table to share one hand-picked value. `tw()` costs exactly 1 df, so AIC stays comparable across rows even though each row has its own *p*. On the real dusky data `tw()` lands on *p* = 1.29–1.32 across model structures, so 1.31 was a good guess — this changes the bookkeeping, not the fits.

### Reading the `p_hat` column

`p_hat` is the Tweedie power estimated for that model. These distributions are indexed by *p* ∈ (1, 2), which sets how variance scales with the mean: Var(*y*) = φ·μ^*p*. At *p* → 1 it behaves like a quasi-Poisson, at *p* → 2 like a Gamma. In between it has an atom at zero plus a continuous positive part — which suits segment counts that are mostly zero with occasional large groups. Lower *p* puts more mass on exact zeros; higher *p* pushes more variance into the right tail.

What matters is the **spread down the column**. If all models land on a similar *p*, they are competing on the mean structure, which is what the table is for. A row whose *p* sits well away from the rest is partly winning by fitting the dispersion differently, and its AIC advantage should be read with that in mind. `NA` means a fixed-*p* family.

### Thin-plate / factor-smooth / by-year set — `4_*_DSM.R` (60 models per species)

| Group | Formula structure | # |
|-------|-------------------|---|
| Spatial only | `s(x,y)`, plus the `bs="ts"` and `te()` variants | 3 |
| + Season | `s(x,y) + season` | 1 |
| + Year | `s(x,y) + s(Ano)` | 1 |
| + Season + Year | `s(x,y) + season + s(Ano)` | 1 |
| + Season + Year + env | one per covariate | 7 |
| + Season + Year + two env | `dist.up + grad` | 1 |
| + Season + env | one per covariate | 7 |
| + Year + env | one per covariate | 7 |
| fs (year-varying, shrunk) | alone / + season / + season + env / + env | 16 |
| by-year (year-varying, unshrunk) | alone / + season / + season + env / + env | 16 |

Selection tables are built **by object name**, not from parallel positional vectors — each row looks its model up by name, so a name that does not resolve is reported rather than silently shifting every label below it. `.ms_table()` also warns if two models in the same table were fitted to different numbers of segments, since AIC is only comparable when they were not.

Dusky dolphin needs a shrinkage marginal, `bs="ts"`, on the by-year smooth to converge at all; common dolphin's by-year block converges with a plain `s(x,y,by=year_fac)`.

### Soap-film set — `4_*_DSM_soap.R` (25 models per species)

Same crossing of season, year and covariates, but with `s(x,y,bs="so")` (Wood, Bravington & Hedley 2008) in place of the thin-plate term. A soap film **cannot** be an `fs` or `by` marginal, so this set has no year-varying analogue.

| Group | # |
|---|---|
| `s(x,y,bs="so")` alone / + season / + season + year / + year | 4 |
| + season + year + env (one per covariate) | 7 |
| + season + env | 7 |
| + year + env | 7 |

`predict()` on a soap model returns `NA` for anything outside the fitted boundary, which is used directly for map masking.

**Soap setup is the most fragile part of the pipeline.** The boundary comes from `survey.area_m`, buffered outward far enough that every segment lies strictly inside it (soap errors if any data point or knot is on or outside the boundary), then simplified. Interior knots come from `dsm::make.soapgrid()`, filtered to be strictly inside and clear of the edge — clearance is measured to the boundary **edges**, not its vertices. If a fit dies with `NA/NaN/Inf in soap.basis`, coarsen the knot grid or raise the buffer.

The common-dolphin configuration was tuned on 2026-09-16 and is now pinned at the top of `4_CommonDolphin_DSM_soap.R`: `simplify_tol = 500`, `margin = 250`, a 14 × 11 knot grid giving **89 knots**, and `k = 20` on the seven environmental smooths. The script asserts the knot count and checks two reference AIC values, so it fails loudly if a knob moves. The dusky soap arm was tuned the same way and **nothing was adopted** — no knob was supported by the evidence.

`4_*_DSM_soap.R` also builds `table_*_combined_modselection`, stacking all 60 thin-plate/fs/by-year candidates with the 25 soap ones into one AIC-ranked table (85 rows). It needs the thin-plate models already in the workspace.

---

## Diagnostics

`UTIL_DSM_Diagnostics.R` runs inside both master scripts, before the save, so its results travel inside the workspace as `dd.diag` / `lo.diag`. It runs three checks, because an AIC ranking rests on assumptions AIC cannot test:

| Check | Asks | Fails when |
|---|---|---|
| **Correlogram** | Are the segments independent? They are contiguous pieces of one track, so they need not be | A *run* of positive correlations starting at lag 1 and decaying. Isolated exceedances at scattered lags are noise |
| **`k.check`** | Did a smooth run into its basis ceiling? If so its wiggliness reflects `k`, not the data | edf close to k′ **and** a low k-index with a small p-value. Either alone is weak evidence |
| **Rootogram** | Does the fitted distribution actually produce the counts observed? | A bar foot displaced beyond its band, and more importantly a *pattern* across bins |

Output lands in `output/<Species>/DSM/` and `output/<Species>/DSM/autocorrelation/`.

**Every selection table now carries a lag-1 autocorrelation column.** `lag1_sig = TRUE` means *do not select that row on its AIC* — the independence assumption underneath that AIC does not hold.

A flat correlogram means no *unmodelled* correlation, not no correlation: a flexible year-varying spatial term can absorb along-track structure into the fitted surface, which is the same mechanism as the over-fitting being tested for. That is why a no-year-term model is always included as a positive control.

---

## Maps

`UTIL_Map_DSM_output_*.R` writes **12 density maps for dusky dolphin and 14 for common dolphin**.

| # | Model | Facet |
|---|-------|-------|
| 1–2 | `season.year`, then `year.season.clo` (`dd`) / `year.season.depth` (`lo`) | season, at a reference year |
| 3–4 | the same two models | year, at season = Spring |
| 5–6 | `fsyear.season` | season (at a reference year) / year |
| 7–8 | `byyear.season` | season (at a reference year) / year |
| 9–10 | `soap.season.year` | season (at a reference year) / year |
| 11 | `fsyear.season` | the full season × year cross, chronological |
| 12 | `fsyear.season` | as 11, but panels blanked where that season × year was never surveyed |
| 13–14 | `soap.season.year` vs `soap.year.season.sst` — **common dolphin only** | season (columns) × model (rows); linear and log₁₀ versions |

### Which data is drawn on top

**The rule:** each panel's overlay shows the data that informed the surface in that panel.

| Facet | Spatial term | Overlay restricted to |
|---|---|---|
| Season | shared (`s(x,y)`, soap) | nothing — every year informed that one shared surface |
| Season | year-varying (`fs`, `by`) | the reference year only |
| Year | any | nothing — ggplot already restricts each panel to its own year, and that year's surface pools every season within it |

Getting this backwards caused two diagnosed map/overlay mismatches during development.

Two things the rule does not cover:

- **Every overlay excludes 2006** (`filter(Ano > 2006)`), although the 154 segments surveyed in 2006 *were* used to fit the models. So the 2006 panel of every year-facet map has no tracks or sightings on it.
- **Maps 11 and 12** facet on a combined season-by-year key, so their overlays are re-tagged with that same key. Without it, the tracks would repeat in full in every panel.

### Uncertainty maps

`UTIL_Map_DSM_output_CV_*.R` draws the per-cell coefficient of variation, CV = SE/N̂, combining spatial-model and detection uncertainty as CV²_total = CV²_spatial + CV²_detection (Williams et al. 2011; Miller et al. 2013, Fig. 5).

CV_spatial comes from a single `predict(type="lpmatrix")` call. For a log-link model this is algebraically identical to what `dsm_var_gam` returns per cell — verified numerically — and far faster than looping `dsm_var_gam` over one "region" per grid cell.

Common dolphin uses `dd.dsm.soap.season.year` with season facets. Dusky dolphin uses `lo.dsm.xy.fsyear.season` with year facets, because that is where uneven survey coverage shows up as elevated CV.

---

## Environmental covariates

| Variable | Description | Varies over |
|----------|-------------|-------------|
| `depth` | Water depth | space only |
| `slope` | Seafloor slope | space only |
| `sst` | Sea surface temperature | space + month |
| `grad` | SST gradient (frontal strength) | space + month |
| `clo` | Chlorophyll-a | space + month |
| `dist.up` | Distance to upwelling areas | space + month |
| `VelVert` | Vertical velocity (upwelling strength) | space + month |

**The five monthly variables are climatologies** — one field per calendar month (`Mes_n` 1–12), a long-term average rather than conditions on a particular survey date. They carry spatial and *seasonal* structure but **no year-to-year variation**: the same month in different years is the same field.

The consequence is worth holding on to: **no environmental covariate can explain interannual change in abundance.** That is carried entirely by `s(Ano)` and `year_fac`. The covariates account for spatial and seasonal structure only.

Note that `grad` is an *SST* gradient — a front-strength covariate, not a bathymetric one — so read `s(grad)` alongside `sst`, not alongside `depth`/`slope`.

`VelVert` is handled differently in one respect. It is not a column of `segdata`; it arrives only on the prediction grid, so `0_ReadData_Plots.r` attaches it to each segment from the nearest prediction cell **within that segment's month** (median displacement 443 m, max 2581 m), guarded by `stopifnot(!anyNA(segdata$VelVert))`. The other six were sampled at the segment centroids directly. That nearest-cell step adds covariate measurement error, which attenuates `s(VelVert)` toward flat — so it is mildly disadvantaged relative to the rest. Values span roughly ±3 × 10⁻⁴; mgcv scales each smooth's penalty internally, so the small magnitude is not itself a problem, but rescale before reading much into it if the smooth collapses to near-linear.

> **Open data-quality item.** In `segdata`, the `sst`, `clo` and `grad` columns vary between years, which a climatology should not.
>
> *How we know it is real.* Holding location fixed — segment pairs in the same grid cell and the same calendar month — pairs from the *same* year agree exactly ~58% of the time, while pairs from *different* years agree 0.0% (`sst`), 0.1% (`clo`) and 11.6% (`grad`). `depth`, which cannot change over time, gives 57.9% vs 56.6% on the identical comparison. So this is not an artefact of imperfect location matching.
>
> *Where they came from.* The `segdata` values almost never appear in the prediction grid's value set for the same month (`sst` 0.8%, `clo` 0.3%), and the two `clo` fields correlate only 0.45. The two sources are different.
>
> *What it means.* Until this is reconciled, `s(sst)`, `s(clo)` and `s(grad)` are **fitted** on values that behave as month-and-year specific, while the prediction grid supplies climatological ones.

> *Resolved (2026-08-25):* an earlier `preddataVV.csv` had `VelVert` for April byte-identical to May in all 1353 cells — only 11 distinct monthly fields. The replacement dataset fixes this. Any `.RData` workspace or `s(VelVert)` fit produced before that date needs regenerating.

---

## Known issues

**Soap knots: AIC points the wrong way.** For common dolphin, AIC keeps falling as the knot grid is refined — all the way to 485 knots, −132 in total. But residual lag-1 autocorrelation rises monotonically with it (0.0215 → 0.0441 against a band of 0.026). The independence assumption underwriting those AIC gains degrades exactly as the claimed gain grows. **89 knots is the most refined grid still inside the band**, and is what the pipeline uses. Do not refine further on the strength of AIC alone.

**`lag1_sig = TRUE` overrides AIC.** A row flagged in the selection table is not selectable on its AIC, however good that AIC looks.

**Year-varying models can extrapolate into unsurveyed regions.** A season × year combination with zero effort can still show an implausible high-density blob in `fs` or by-year maps — pure basis-function extrapolation, not a signal. Diagnosed for common dolphin (2015, south edge) and dusky dolphin (2009, west edge). Map 12 blanks any panel with zero effort.

**`bam` was tried for the dusky by-year block and removed.** Its approximations target *n* ≳ 10⁵; there are 6288 segments here. On the identical formula and the real data:

| `lo.dsm.xy.byyear.season` | AIC | edf | time |
|---|---|---|---|
| `bam` / `fREML` | 1031.89 | 20.62 | — |
| `gam` / `REML` | **976.65** | **39.80** | 782 s |

`bam` was shrinking the 11 year-specific surfaces to nearly nothing, costing ~55 AIC. Across structures its log-likelihood was lower than `gam`'s in every case (−0.7 to −32.8) while using *more* effective parameters — a worse fit, not a penalty artefact. `bs="ts"` is what makes that block converge; the engine never was. **Any dusky by-year result produced before this change is under-fitted and should not be reported.**

**Common dolphin rootogram tail.** The misfit in the upper count bins is a school-size problem: 92% of the count variance is group size (max 450 animals, against 11 for dusky). No single Tweedie can fit both the body and that tail. It costs the CIs ~29% and the point estimate ~6%, and is owned by 67 segments. Fitting a location-scale Tweedie makes it worse.

**Open — the dusky map 5 reference year.** `UTIL_Map_DSM_output_LO.R` computes the reference year as the fitted year nearest the median (2015) and then, on the next line, hard-codes it to 2017. `ref_yf_byyear` was left alone, so dusky maps 5 and 7 are drawn at different years, and the comment above the override no longer describes what it does. Common dolphin has no equivalent line. Unresolved.

---

## References

Fewster, R.M., Buckland, S.T., Burnham, K.P., Borchers, D.L., Jupp, P.E., Laake, J.L., & Thomas, L. (2009). Estimating the encounter rate variance in distance sampling. *Biometrics*, 65(1), 225–236. <https://doi.org/10.1111/j.1541-0420.2008.01018.x>

Kleiber, C., & Zeileis, A. (2016). Visualizing count data regressions using rootograms. *The American Statistician*, 70(3), 296–303. <https://doi.org/10.1080/00031305.2016.1173590>

Miller, D.L., Burt, M.L., Rexstad, E.A., & Thomas, L. (2013). Spatial models for distance sampling data: recent developments and future directions. *Methods in Ecology and Evolution*, 4(11), 1001–1010. <https://doi.org/10.1111/2041-210X.12105>

Williams, R., Hedley, S.L., Branch, T.A., Bravington, M.V., Zerbini, A.N., & Findlay, K.P. (2011). Chilean blue whales as a case study to illustrate methods to estimate abundance and evaluate conservation status of rare species. *Conservation Biology*, 25(3), 526–535.

Wood, S.N., Bravington, M.V., & Hedley, S.L. (2008). Soap film smoothing. *Journal of the Royal Statistical Society: Series B*, 70(5), 931–955.

Wood, S.N., Pya, N., & Säfken, B. (2016). Smoothing parameter and model selection for general smooth models. *Journal of the American Statistical Association*, 111(516), 1548–1563.

---

## Authors

- **Alejandro Buren** (analysis) — CONICET-IAA
- **PI: Dr. Silvana Dans** — CONICET / CESIMAR / UNPSJB / Fundación Azara
