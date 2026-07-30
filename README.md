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
| 1 | `1_CommonDolphin.R` / `1_DuskyDolphin.R` | Master scripts — source steps 0, 2, 3, 4, 5, and `UTIL_Map_DSM_output_*` in sequence (run `UTIL_FindTweedieP_*` first to obtain the Tweedie *p* used in step 4) |
| 2 | `2_*_EDA.R` | Exploratory analysis: ship effect, Beaufort filtering, covariate overlays |
| 3 | `3_*_DetectionFunction.R` | Fit and select detection function (AIC); set truncation distance |
| 4 | `4_*_DSM.R` | Fit the full thin-plate/factor-smooth/by-year candidate set (50 models per species, see *Density surface models* below); builds the model-selection table |
| 4 (soap) | `4_*_DSM_soap.R` | Fit the soap-film candidate set (22 models per species) on a boundary buffered outward so every segment/knot lies strictly inside it; builds the soap-only and the thin-plate-vs-soap combined selection tables |
| 5 | `5_CommonDolphin_Abundance.R` / `5_DuskyDolphin_Abundance.R` | Estimate abundance (N̂) and density (dolphins km⁻²) with 95% lognormal CIs for every surveyed season × year, for **four** models per species run in parallel against the same design-based check (see table below). Model-based estimates come from `dsm_var_gam` (delta-method variance over the per-cell-area prediction grid); a design-based Horvitz–Thompson estimate (transect-level Fewster R2 encounter-rate CV + detection CV) is computed once and reused across all four models, since it is model-independent. Writes summary CSVs and abundance/density plots (season-faceted, continuous, and `plotly` model-fit) to `output/<Species>/Abundance/`. Sources `R/lnorm_ci.R` and `R/year_partial_effect.R` |
| — | `UTIL_FindTweedieP_*.R` | Grid search for Tweedie *p* parameter (run before step 4) |
| — | `UTIL_Map_DSM_output_*.R` | 12 density maps per species (see *Maps* below) |
| — | `UTIL_Map_DSM_output_CV_*.R` | Per-cell coefficient-of-variation map, following Fig. 5 of Miller et al. (2013) — see *Uncertainty mapping* below |
| — | `UTIL_*_EdgeEffects.R` | Edge-effect diagnostics and mitigation: (1) `exclude.too.far()` masking of the `fs` per-year maps; (2) the soap-film boundary/knot construction reused by `4_*_DSM_soap.R` |

> **Note:** the full pipeline (`1_CommonDolphin.R` / `1_DuskyDolphin.R`, i.e. everything above) takes **hours** to run per species — most of that time is the `4_*_DSM.R` / `4_*_DSM_soap.R` model-fitting steps (72 models per species combined). Each master script ends with `save.image()`, writing the **entire workspace** for that species to `output/<Species>/<species>_output.RData` (`output/CommonDolphin/dd_output.RData`, `output/DuskyDolphin/lo_output.RData`). These files are too large for git to track (`*.RData` is in `.gitignore`) and are regenerated locally by re-running the pipeline. The Quarto reports (`*.qmd`) `load()` these `.RData` files to build the HTML report without re-running the pipeline each time.

### Models fitted per species

| Object | Formula | Spatial term |
|--------|---------|---------------|
| `*.dsm.xy.season.year` | `count ~ s(x,y) + season + s(Ano)` | shared thin-plate surface; **primary** model |
| `*.dsm.xy.fsyear.season` | `count ~ s(x,y,year_fac,bs="fs") + season` | year-varying, **shrunk** (factor-smooth; one shared smoothing parameter across years) |
| `*.dsm.xy.byyear.season` | `count ~ s(x,y,by=year_fac) + year_fac + season` | year-varying, **unshrunk** (independent surface per year) |
| `*.dsm.soap.season.year` | `count ~ s(x,y,bs="so") + season + s(Ano)` | shared soap-film surface, edge-effect controlled |

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

| Species | Key function | Truncation | Beaufort cutoff |
|---------|-------------|------------|-----------------|
| Common dolphin | Hazard-rate | 325 m | ≤ 4 |
| Dusky dolphin | Half-normal | 450 m | ≤ 3 |

Distance bins are defined with cutpoints to account for rounding heaping at favored distances.

### Density surface models

DSMs use `dsm::dsm()` with a Tweedie family. The power parameter *p* is selected by AIC grid search (`UTIL_FindTweedieP_*.R`) before model fitting (common dolphin *p* = 1.58; dusky dolphin *p* = 1.31). Offset = `segment_length × truncation_distance`; model selection is by AIC with deviance explained reported for all candidates.

#### Thin-plate / factor-smooth / by-year set — `4_*_DSM.R` (50 models per species)

| Group | Formula structure | # models |
|-------|-------------------|----------|
| Spatial | `s(x,y)` | 1 |
| Spatial + Season | `s(x,y) + season` | 1 |
| Spatial + Season + Year | `s(x,y) + season + s(Ano)` | 1 |
| Spatial + Season + Year + env | `s(x,y) + season + s(Ano) + s(env)` — each of 6 covariates | 6 |
| Spatial + Year | `s(x,y) + s(Ano)` | 1 |
| Spatial + Season + env | `s(x,y) + season + s(env)` — each of 6 covariates | 6 |
| Spatial + Year + env | `s(x,y) + s(Ano) + s(env)` — each of 6 covariates | 6 |
| fs (year-varying, shrunk) | `s(x,y,year_fac,bs="fs")`, alone / + season / + season + env / + env — 1+1+6+6 | 14 |
| by-year (year-varying, unshrunk) | `s(x,y,by=year_fac) + year_fac`, alone / + season / + season + env / + env — 1+1+6+6 | 14 |

**fs vs. by-year:** `fs` treats year as a random-effect-like grouping factor — one shared smoothing parameter, so data-poor years shrink toward the common spatial pattern. `by=year_fac` fits each year's surface fully independently (unshrunk). The `by` variant is far more expensive: for dusky dolphin it required `engine="bam", method="fREML"` plus a shrinkage marginal `bs="ts"` on the by-year smooth to converge in reasonable time — without this it either ran for hours with no output, or (with `bam` alone) converged too quickly with an `algorithm did not converge` warning, because both symptoms trace back to the same cause: sparse-coverage years leaving that year's surface poorly identified. Common dolphin's `by`-year block converges fine with plain `gam()`/`REML` and was left as-is.

`year_partial_effect()` (`R/year_partial_effect.R`) extracts an interpretable, area-weighted per-year effect (with a proper covariance-based CI) from the `fs` model's smooth, since year is not a standalone coefficient there the way it is for `s(Ano)` or `by=year_fac`.

#### Soap-film set — `4_*_DSM_soap.R` (22 models per species)

Mirrors the same covariate/season/year crossing, but with `s(x,y,bs="so")` in place of the thin-plate term (Wood, Bravington & Hedley 2008), which respects the survey/coastline boundary instead of smoothing across it. A soap film **cannot** be used as a factor-smooth or `by` marginal, so this set has no year-varying analogue — it fits the same shared-surface family as `season.year`. `predict()` on a soap model automatically returns `NA` for any location outside its fitted boundary, which is used directly for map masking (no separate distance-based check needed for this).

Soap setup is the most fragile and slowest part of the pipeline: the boundary is taken from `survey.area_m`, buffered **outward** by (max segment-to-boundary distance) + a margin so every segment lies strictly inside it (soap errors if any data point or knot is on/outside the boundary), then simplified; interior knots come from `dsm::make.soapgrid()`, filtered to be strictly inside and off the edge. If a fit dies with `NA/NaN/Inf in soap.basis`, coarsen the knot grid or raise the buffer/margin.

`4_*_DSM_soap.R` also builds `table_*_combined_modselection`, stacking all thin-plate/fs/by-year candidates (50) with the soap candidates (22) into one AIC-ranked table — requires the thin-plate models from `4_*_DSM.R` to already be in the workspace.

### Known modelling issues to keep in mind

- **Year-varying spatial models can extrapolate into unsurveyed regions.** A season × year combination with zero effort can still show an implausible high-density "blob" in `fs`/`by`-year maps, purely from basis-function extrapolation, not a real signal — diagnosed for common dolphin (2015, south edge) and dusky dolphin (2009, west edge). Map 12 blanks any panel with zero survey effort for that combination; `UTIL_*_EdgeEffects.R` Part 1 additionally masks by distance-to-nearest-segment (`exclude.too.far()`) within a panel.
- **A shared spatial term (thin-plate, soap) is legitimately informed by every season and year of data**; a year-varying term (`fs`, `by`) is legitimately informed by every *season* within its own year, but **not** by other years. Overlay tracks/sightings on each map must be filtered consistently with this, or an observation from an irrelevant year/season can appear to "explain" a feature it had nothing to do with.

---

## Environmental covariates

| Variable | Description |
|----------|-------------|
| `depth` | Water depth |
| `slope` | Seafloor slope |
| `grad` | Gradient (upslope magnitude) |
| `sst` | Sea surface temperature |
| `clo` | Chlorophyll-a concentration |
| `dist.up` | Distance to upwelling areas |

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
