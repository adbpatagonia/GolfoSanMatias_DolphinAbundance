# Dolphin Abundance in the San Matías Gulf, Argentina

Distance sampling and density surface models (DSMs) for two dolphin species in the San Matías Gulf, Patagonia, Argentina.

**Species:**
- *Delphinus delphis* — Common dolphin (`dd`)
- *Lagenorhynchus obscurus* — Dusky dolphin (`lo`)

**Survey period:** 2007–2018  
**Website:** <https://adbpatagonia.github.io/GolfoSanMatias_DolphinAbundance/>

---

## Research questions

1. Does dolphin density vary by season?
2. Did dolphin density change across survey years?

---

## Repository structure

```
analysis/           R scripts (numbered by execution order)
  archive/          Superseded/older versions of scripts
R/                  Package functions sourced by the pipeline:
                      lnorm_ci.R             lognormal CI from N and CV
                      improv.r               % improvement between two values
                      year_partial_effect.R  per-year partial effect of an
                                              s(x,y,year_fac,bs="fs") term
                      make_surveyID.R        build a unique survey identifier
                                              from vessel × year × month
data/
  DistanceData/     Distance sampling input CSVs
  shp/              Survey area, prediction grid, and coastline shapefiles
data-raw/           Scripts/sources used to build data/
output/             Generated figures and model output
  CommonDolphin/    EDA/ DetectionFunction/ DSM/ Abundance/
  DuskyDolphin/     EDA/ DSM/ Abundance/
  EnvVars/          Environmental covariate maps
assets/             Manuscript draft, reference PDFs (incl. Miller et al. 2013),
                    and supplementary materials
references/         BibTeX bibliography files
tests/testthat/     testthat unit tests for R/ (lnorm_ci, improv, year_partial_effect)
man/                Package documentation (roxygen)
DESCRIPTION, NAMESPACE   R-package metadata (project is structured as a package,
                         `gsmdolphins`); analysis/, data/, output/, docs/, *.qmd
                         are excluded from the package build via .Rbuildignore
*.qmd               Quarto reports (rendered to docs/)
docs/               Built website (GitHub Pages)
```

---

## Input data

All inputs live under `data/` and must be present before running the pipeline. Paths are resolved with `here::here()`, so keep the folder names and layout exactly as below (relative to the project root).

### Distance-sampling tables — store in `data/DistanceData/`

CSV files are **semicolon-delimited** (`;`); some column headers contain trailing spaces (read as-is by `data.table::fread`). Coordinates `x`/`y` are in the projected CRS (EPSG:22193).

| File | Used for | Key columns |
|------|----------|-------------|
| `distdata_ddwholesample.csv` | Common dolphin distance data (sightings) | object, Effort, distance, size, beaufort, ship, latitude, longitude, x, y, Dia, Mes, Ano, Sample.Label |
| `distdata_lowholesample.csv` | Dusky dolphin distance data (sightings) | same as above |
| `obsdata_dd.csv` | Common dolphin observation table (links sightings to segments) | object, Sample.Label, size, distance, Effort, latitude, longitude, x, y, Ano |
| `obsdata_lo.csv` | Dusky dolphin observation table | same as above |
| `segdata.csv` | Transect-segment table with per-segment effort and environmental covariates | x, y, Effort, Transect.Label, Mes, Mes_n, Ano, est, Sample.Label, dist.coast, slope, depth, sst, clo, grad, dist.up |
| `preddata.csv` | Prediction grid with per-cell environmental covariates | id, x, y, area, dist.coast, slope, depth, sst, clo, grad, dist.up, Mes_n |

### Spatial layers — store in `data/shp/`

Each layer is an ESRI shapefile; keep all sidecar files (`.shp`, `.shx`, `.dbf`, `.prj`, and any `.qpj`/`.sbn`/`.sbx`) together in this folder.

| Layer | Used for |
|-------|----------|
| `survey.area.*` | Survey-area polygon (defines the region and total area) |
| `gridproy41.1.*` | Prediction-grid polygons (~1.10–1.17 km² cells; not equal-area) |
| `Patagonia_Completa.*` | Coastline/landmass polygon for maps |

---

## Analysis pipeline

Scripts are sourced in order via the species master script:

| Step | Script | Description |
|------|--------|-------------|
| 0 | `0_ReadData_Plots.r` | Load all data; project to EPSG:22193; map environmental covariates |
| 1 | `1_CommonDolphin.R` / `1_DuskyDolphin.R` | Master scripts — source steps 0, 2, 3, 4, 5, and `UTIL_Map_DSM_output_*` in sequence |
| 2 | `2_*_EDA.R` | Exploratory analysis: ship effect, Beaufort filtering, covariate overlays |
| 3 | `3_*_DetectionFunction.R` | Fit and select detection function (AIC); set truncation distance |
| 4 | `4_*_DSM.R` | Fit the full thin-plate/factor-smooth/by-year candidate set (50 models per species, see *Density surface models* below); builds the model-selection table |
| 4 (soap) | `4_*_DSM_soap.R` | Fit the soap-film candidate set (22 models per species) on a boundary buffered outward so every segment/knot lies strictly inside it; builds the soap-only and the thin-plate-vs-soap combined selection tables |
| 5 | `5_CommonDolphin_Abundance.R` / `5_DuskyDolphin_Abundance.R` | Estimate abundance (N̂) and density (dolphins km⁻²) with 95% lognormal CIs for every surveyed season × year, for **four** models per species run in parallel against the same design-based check (see table below). Model-based estimates come from `dsm_var_gam` (delta-method variance over the per-cell-area prediction grid); a design-based Horvitz–Thompson estimate (transect-level Fewster R2 encounter-rate CV + detection CV) is computed once and reused across all four models, since it is model-independent. Writes summary CSVs and abundance/density plots (season-faceted, continuous, and `plotly` model-fit) to `output/<Species>/Abundance/`. Sources `R/lnorm_ci.R` and `R/year_partial_effect.R` |
| — | `UTIL_AssignPredCovariates.R` | Re-derives the environmental covariates on the segments from `preddata` by point-in-polygon, and contrasts that with the assignment already in `segdata.csv`. Adds `<var>.pred` columns; changes nothing unless `OVERWRITE_SEGDATA <- TRUE` |
| — | `UTIL_Map_DSM_output_*.R` | 12 density maps per species (see *Maps* below) |
| — | `UTIL_Map_DSM_output_CV_*.R` | Per-cell coefficient-of-variation map, following Fig. 5 of Miller et al. (2013) — see *Uncertainty mapping* below |

> **Note:** the full pipeline (`1_CommonDolphin.R` / `1_DuskyDolphin.R`, i.e. everything above) takes **hours** to run per species — most of that time is the `4_*_DSM.R` / `4_*_DSM_soap.R` model-fitting steps (72 models per species combined). Each master script ends with `save.image()`, writing the **entire workspace** for that species to `output/<Species>/<species>_output.RData` (`output/CommonDolphin/dd_output.RData`, `output/DuskyDolphin/lo_output.RData`). These files are too large for git to track (`*.RData` is in `.gitignore`) and are regenerated locally by re-running the pipeline. The Quarto reports (`*.qmd`) `load()` these `.RData` files to build the HTML report without re-running the pipeline each time.

### Models fitted per species

| Object | Formula | Spatial term |
|--------|---------|---------------|
| `*.dsm.xy.season.year` | `count ~ s(x,y) + season + s(Ano)` | shared thin-plate surface; **primary** model |
| `*.dsm.xy.fsyear.season` | `count ~ s(x,y,year_fac,bs="fs") + season` | year-varying, **shrunk** (factor-smooth; one shared smoothing parameter across years) |
| `*.dsm.xy.byyear.season` | `count ~ s(x,y,by=year_fac) + year_fac + season` | year-varying, **unshrunk** (independent surface per year) |
| `*.dsm.soap.season.year` | `count ~ s(x,y,bs="so") + season + s(Ano)` | shared soap-film surface, coastline/boundary-respecting (does not smooth across land) |

`*` = `dd` (common dolphin) or `lo` (dusky dolphin). All four are fitted and compared against the same design-based estimate in step 5 and mapped in `UTIL_Map_DSM_output_*.R`.

### Maps (`UTIL_Map_DSM_output_*.R`)

| # | Model | Facet |
|---|-------|-------|
| 1–2 | `season.year`, `year.season.clo` (`dd`) / `year.season.depth` (`lo`) | season (at reference year) |
| 3–4 | `season.year`, `year.season.clo` (`dd`) / `year.season.depth` (`lo`) | year (season = Spring) |
| 5–6 | `fsyear.season` | season (at reference year) / year (per-year surface) |
| 7–8 | `byyear.season` | season (at reference year) / year (per-year surface) |
| 9–10 | `soap.season.year` | season (at reference year) / year (shared surface) |
| 11 | `fsyear.season` | full season × year cross, chronological (Summer→Fall→Winter→Spring) |
| 12 | `fsyear.season` | as 11, with panels blanked where that season × year was never surveyed |

**Overlay convention** (tracks/sightings drawn on each panel): the overlay should show exactly the data that informed the spatial surface in that panel — no more, no less. In practice this only bites for **season**-facet maps, since ggplot's own per-layer facet matching already restricts each panel to its own facet value automatically wherever the overlay carries a matching column (`Ano` for year facets, `season` for season facets).
- Season-facet maps (1, 2, 9), shared spatial term, fixed at a reference year: overlay is *not* further restricted by year — every year's effort legitimately informed that one shared surface.
- Season-facet maps (5, 7), year-varying term (`fs`, `by`), fixed at a reference year: overlay is explicitly restricted to that reference year (`Ano == ref_yf_fs` / `ref_yf_byyear`) — other years' data has nothing to do with that specific year's surface.
- Year-facet maps (3, 4, 6, 8, 10), any term: overlay is *not* restricted by season — for the shared-term maps (3, 4, 10) this doesn't matter; for the year-varying maps (6, 8) it is deliberate, because that year's spatial surface pools data from every season within the year.

Getting this backwards was the cause of two diagnosed map/overlay mismatches during development (see *Known modelling issues* below).

### Uncertainty mapping (`UTIL_Map_DSM_output_CV_*.R`)

Per-cell coefficient of variation, CV = SE/N̂, combining spatial-model and detection-function uncertainty as CV²_total = CV²_spatial + CV²_detection (Williams et al. 2011; Miller et al. 2013, Fig. 5). CV_spatial is computed directly from one `predict(type="lpmatrix")` call (an identity that, for a log-link model, equals `dsm_var_gam`'s own per-cell result — verified numerically against it), which is far faster than looping `dsm_var_gam` over one prediction "region" per grid cell. Common dolphin uses `dd.dsm.soap.season.year` (season facets); dusky dolphin uses `lo.dsm.xy.fsyear.season` (year facets, since that is where uneven survey coverage shows up as elevated CV).

### Detection functions

| Species | Key function | Truncation | Beaufort cutoff | Selected covariate |
|---------|-------------|------------|-----------------|-------------------|
| Common dolphin | Hazard-rate | 325 m | ≤ 4 | `nobs_grp` (1 vs. >1 effective observer) |
| Dusky dolphin | Half-normal | 450 m | ≤ 3 | `nobs_grp` (1 vs. >1 effective observer) |

Distance bins are defined with cutpoints to account for rounding heaping at favored distances. Covariates tested: `nobs_grp`, `ship`, `size_sc`, `beaufort_fct`/`beaufort_grp`, and pairwise combinations; the final model is selected by AIC.

### Density surface models

DSMs use `dsm::dsm()` with `family = tw(link = "log")` — mgcv's extended Tweedie, which **estimates the power parameter *p* inside each fit**. Offset = `segment_length × truncation_distance`; every model is fitted with `gam` / `method = "REML"`. Model selection is by AIC, with deviance explained and the estimated *p* reported for all candidates.

#### Why `tw()` rather than a fixed *p*

*p* used to be fixed per species (common 1.58, dusky 1.31) by an AIC grid search in `UTIL_FindTweedieP_*.R` (now retired to `analysis/archive/`). That grid search was a profile likelihood over *p* that **paid no degrees of freedom for the *p* it chose**, and it forced every model in the table to share one hand-picked value. `tw()` costs exactly 1 df (`family$n.theta == 1`), so AIC stays comparable across rows even though each row has its own *p*. On the real dusky data `tw()` estimates *p* = 1.29–1.32 across model structures, so the old 1.31 was a good choice — this changes the bookkeeping, not the fits.

#### Reading the `p_hat` column

`p_hat` is the Tweedie power estimated for that model. The Tweedie distributions used here are indexed by *p* ∈ (1, 2), setting how variance scales with the mean, Var(*y*) = φ·μ^*p*: *p* → 1 behaves like a quasi-Poisson, *p* → 2 like a Gamma, and in between the distribution has an atom at zero plus a continuous positive part — which is what suits segment counts that are mostly zero with occasional large groups. Lower *p* puts relatively more mass on exact zeros; higher *p* pushes more variance into the right tail.

What matters is the **spread down the column**. If all models land on a similar *p* they are competing on the *mean* structure, which is what the table is meant to compare. A row whose *p* sits well away from the rest is partly winning by fitting the dispersion/tail differently, and its AIC advantage should be read with that in mind. `NA` means that model was fitted with a fixed-*p* family.

#### One engine everywhere

All models, both species, are fitted with `gam` / `REML`. The dusky by-year block briefly used `engine = "bam", method = "fREML"`; that was reversed after a direct comparison on the real data — see *Known modelling issues*.

#### Thin-plate / factor-smooth / by-year set — `4_*_DSM.R` (57 models per species)

| Group | Formula structure | # models |
|-------|-------------------|----------|
| Spatial | `s(x,y)` | 1 |
| Spatial + Season | `s(x,y) + season` | 1 |
| Spatial + Season + Year | `s(x,y) + season + s(Ano)` | 1 |
| Spatial + Season + Year + env | `s(x,y) + season + s(Ano) + s(env)` — each of 7 covariates | 7 |
| Spatial + Year | `s(x,y) + s(Ano)` | 1 |
| Spatial + Season + env | `s(x,y) + season + s(env)` — each of 7 covariates | 7 |
| Spatial + Year + env | `s(x,y) + s(Ano) + s(env)` — each of 7 covariates | 7 |
| fs (year-varying, shrunk) | `s(x,y,year_fac,bs="fs")`, alone / + season / + season + env / + env — 1+1+7+7 | 16 |
| by-year (year-varying, unshrunk) | `s(x,y,by=year_fac) + year_fac`, alone / + season / + season + env / + env — 1+1+7+7 | 16 |

Selection tables are built **by object name**, not from parallel positional vectors: each row looks its model up by name, so a name that does not resolve is reported rather than silently shifting every label below it — and `.ms_table()` warns if any two models in the same table were fitted to different numbers of segments, since AIC is only comparable when they were not.

**fs vs. by-year:** `fs` treats year as a random-effect-like grouping factor — one shared smoothing parameter, so data-poor years shrink toward the common spatial pattern. `by=year_fac` fits each year's surface fully independently (unshrunk). The `by` variant is far more expensive. For dusky dolphin it needs a shrinkage marginal `bs="ts"` on the by-year smooth to converge at all; common dolphin's `by`-year block converges fine with plain `s(x,y,by=year_fac)`. Both species use `gam`/`REML` — see *Known modelling issues* for why the `bam` engine was tried for dusky and then removed.

`year_partial_effect()` (`R/year_partial_effect.R`) extracts an interpretable, area-weighted per-year effect (with a proper covariance-based CI) from the `fs` model's smooth, since year is not a standalone coefficient there the way it is for `s(Ano)` or `by=year_fac`.

#### Soap-film set — `4_*_DSM_soap.R` (25 models per species)

Mirrors the same covariate/season/year crossing, but with `s(x,y,bs="so")` in place of the thin-plate term (Wood, Bravington & Hedley 2008), which respects the survey/coastline boundary instead of smoothing across it. A soap film **cannot** be used as a factor-smooth or `by` marginal, so this set has no year-varying analogue — it fits the same shared-surface family as `season.year`. `predict()` on a soap model automatically returns `NA` for any location outside its fitted boundary, which is used directly for map masking (no separate distance-based check needed for this).

Soap setup is the most fragile and slowest part of the pipeline: the boundary is taken from `survey.area_m`, buffered **outward** by (max segment-to-boundary distance) + a margin so every segment lies strictly inside it (soap errors if any data point or knot is on/outside the boundary), then simplified; interior knots come from `dsm::make.soapgrid()`, filtered to be strictly inside and off the edge. If a fit dies with `NA/NaN/Inf in soap.basis`, coarsen the knot grid or raise the buffer/margin.

`4_*_DSM_soap.R` also builds `table_*_combined_modselection`, stacking all thin-plate/fs/by-year candidates (57) with the soap candidates (25) into one AIC-ranked table — requires the thin-plate models from `4_*_DSM.R` to already be in the workspace.

### Known modelling issues to keep in mind

- **Year-varying spatial models can extrapolate into unsurveyed regions.** A season × year combination with zero effort can still show an implausible high-density "blob" in `fs`/`by`-year maps, purely from basis-function extrapolation, not a real signal — diagnosed for common dolphin (2015, south edge) and dusky dolphin (2009, west edge). Map 12 blanks any panel with zero survey effort for that combination.
- **`bam` was tried for the dusky by-year block and removed — it was fitting materially worse.** `bam`'s approximations target *n* ≳ 10⁵; there are 6288 segments here. Refitting the identical formula on the real dusky data:

  | `lo.dsm.xy.byyear.season` | AIC | edf | time |
  |---|---|---|---|
  | `bam` / `fREML` | 1031.89 | 20.62 | — |
  | `gam` / `REML` | **976.65** | **39.80** | 782 s |

  `bam` was shrinking the 11 year-specific surfaces to nearly nothing, costing ~55 AIC units. Across structures its log-likelihood was lower than `gam`'s in every case (−0.7 to −32.8) while using *more* effective parameters, so this is a worse fit rather than a penalty artifact. `bs="ts"` is what makes that block converge — the engine never was. **Any dusky by-year result produced before this change is under-fitted and should not be reported.**
- **A shared spatial term (thin-plate, soap) is legitimately informed by every season and year of data**; a year-varying term (`fs`, `by`) is legitimately informed by every *season* within its own year, but **not** by other years. Overlay tracks/sightings on each map must be filtered consistently with this, or an observation from an irrelevant year/season can appear to "explain" a feature it had nothing to do with.

---

## Environmental covariates

| Variable | Description | Temporal scale |
|----------|-------------|----------------|
| `depth` | Water depth | static |
| `slope` | Seafloor slope | static |
| `sst` | Sea surface temperature | climatological monthly |
| `grad` | Sea-surface-temperature gradient (frontal strength) | climatological monthly |
| `clo` | Chlorophyll-a concentration | climatological monthly |
| `dist.up` | Distance to upwelling areas | climatological monthly |
| `VelVert` | Vertical velocity (upwelling / downwelling strength) | climatological monthly |

**These are climatological monthly variables.** `sst`, `grad`, `clo`, `dist.up` and `VelVert` are supplied as one field per calendar month (`Mes_n` 1–12), representing a long-term monthly average rather than the conditions on any particular survey date. They therefore carry spatial and *seasonal* structure but **no interannual variation**: the same month in different years is the same field. `depth` and `slope` are static, varying in space only. Note that `grad` is an SST gradient — an oceanographic front-strength covariate, not a bathymetric one — so `s(grad)` should be interpreted alongside `sst`, not alongside `depth`/`slope`.

A consequence worth keeping in mind: because none of the seven varies between years, no environmental covariate can explain interannual change in abundance. That is carried entirely by the temporal terms (`s(Ano)`, `year_fac`), and the covariates account for spatial and seasonal structure only.

`VelVert` is nevertheless handled differently from the other six in one respect: it is **not** a column of `segdata.csv` and arrives only on the prediction grid (`preddataVV.csv`), so `0_ReadData_Plots.r` attaches it to each segment from the nearest prediction cell **within that segment's month** (median displacement 443 m, max 2581 m), guarded by `stopifnot(!anyNA(segdata$VelVert))`. The other six were sampled at the segment centroids directly. That nearest-cell step introduces covariate measurement error which attenuates `s(VelVert)` toward flat, so it is mildly disadvantaged relative to the rest. Values span roughly ±3 × 10⁻⁴; mgcv scales each smooth's penalty internally so the small magnitude is not itself a problem, but rescale before reading much into it if the smooth collapses to near-linear.

> **Open data-quality item.** In `segdata.csv`, the `sst`, `clo` and `grad` columns do vary between years, which is inconsistent with a climatology. Holding location fixed (segment pairs in the same grid cell and the same calendar month), pairs from the *same* year agree exactly ~58% of the time while pairs from *different* years agree 0.0% (`sst`), 0.1% (`clo`) and 11.6% (`grad`). `depth` — which cannot change over time — gives 57.9% vs 56.6% on the identical comparison, so this is not an artefact of imperfect location matching. The `segdata` values also almost never appear in the prediction grid's value set for the same month (`sst` 0.8%, `clo` 0.3%), i.e. the two came from different sources. Until reconciled, `s(sst)`, `s(clo)` and `s(grad)` are fitted on values that behave as month-and-year specific rather than climatological, while the prediction grid supplies climatological ones (the `clo` fields correlate only 0.45).
>
> *Resolved (2026-08-25):* an earlier `preddataVV.csv` had `VelVert` for April byte-identical to May in all 1353 cells, i.e. only 11 distinct monthly fields. The replacement dataset fixes this — no month pair is now systematically identical, and cells carry 10–12 distinct monthly values (the few ties are single-cell numerical coincidences, 1–9 cells out of 1353). Any `.RData` workspace or `s(VelVert)` fit produced before this date used the old field and needs regenerating.

### Number of observers (`n_obs`) — structure only

The model sets also define an `n_obs` twin of **every** candidate (`<model>.nobs`), covering the models both with and without environmental covariates, so each pair isolates the effect of observer number. **The column is not in the data yet**, so `has_n_obs` is `FALSE`, the fitting blocks are skipped, and the `.nobs` rows are simply absent from the name-driven selection tables. Nothing else has to change when it lands.

When wiring it up:

- Source is `data/DistanceData/segdata con nro observadores embarcacion.csv`. Join on **`Sample.Label` only** (6288 rows, 6288 unique labels — a clean 1:1 key): every *numeric* column in that file has been mangled by a thousands-separator round-trip (`x` reads as `3.587.989.862`, `depth` as `7.640.243`), so reading any of them would corrupt `segdata`. Rename `n obs` → `n_obs`.
- It must become a **factor with `"unknown"` as an explicit level**, not `NA`: 1457 of 6288 segments (23%) are `"unknown"`, and as `NA` those rows drop out of every `n_obs` model, making their AIC incomparable with every other row in the same table. It also has only 6 distinct numeric values, so it enters parametrically — `s(n_obs)` with mgcv's default `k = 10` errors outright.
- Enabling it **doubles** every model set (57 → 114 thin-plate, 25 → 50 soap per species), and so roughly doubles an already multi-hour run.
- **Caveat:** observer number is a *detectability* covariate, so its methodologically correct home is a covariate detection function, not the count model. It is kept in the count model because `dsm_var_gam()` — which the whole abundance pipeline depends on — requires a detection function without covariates, and `dsm_var_prop()` refits the model and fails on these data.

---

## Key dependencies

```r
dsm, Distance, mrds          # Distance sampling and DSM
mgcv                         # GAMs — factor-smooth (fs), by-year, soap-film (so) bases
sf, terra                    # Spatial data
data.table, tidyverse        # Data wrangling
ggplot2, plotly, patchwork, viridis  # Visualisation
gratia                       # GAM diagnostics
```

---

## References

Miller, D.L., Burt, M.L., Rexstad, E.A., & Thomas, L. (2013). Spatial models for distance sampling data: recent developments and future directions. *Methods in Ecology and Evolution*, 4(11), 1001–1010. <https://doi.org/10.1111/2041-210X.12105>

Wood, S.N., Bravington, M.V., & Hedley, S.L. (2008). Soap film smoothing. *Journal of the Royal Statistical Society: Series B*, 70(5), 931–955.

Williams, R., Hedley, S.L., Branch, T.A., Bravington, M.V., Zerbini, A.N., & Findlay, K.P. (2011). Chilean blue whales as a case study to illustrate methods to estimate abundance and evaluate conservation status of rare species. *Conservation Biology*, 25(3), 526–535.

---

## Authors

**Alejandro Buren** (analysis) — CONICET-IAA  
**PI: Dr. Silvana Dans** — CONICET / CESIMAR / UNPSJB / Fundación Azara
