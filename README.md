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
R/                  Helper functions sourced by the pipeline (e.g. lnorm_ci.R)
data/
  DistanceData/     Distance sampling input CSVs
  shp/              Survey area, prediction grid, and coastline shapefiles
data-raw/           Scripts/sources used to build data/
output/             Generated figures and model output
  CommonDolphin/    EDA/ DetectionFunction/ DSM/ Abundance/
  DuskyDolphin/     EDA/ DSM/ Abundance/
  EnvVars/          Environmental covariate maps
assets/             Manuscript draft, reference PDFs, and supplementary materials
references/         BibTeX bibliography files
tests/              testthat unit tests (e.g. lnorm_ci)
man/                Package documentation (roxygen)
DESCRIPTION, NAMESPACE   R-package metadata (project is structured as a package)
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
| `distdata_dd_todos2.csv` | Common dolphin — alternative ("option 2") distance data; loaded but pending review (see note below) | same as above |
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

> **Note:** `0_ReadData_Plots.r` loads both `distdata_ddwholesample.csv` and `distdata_dd_todos2.csv` for the common dolphin; the script flags the second as an open question for Silvana. It is currently loaded but never used. Only one should feed the analysis once that is resolved.

---

## Analysis pipeline

Scripts are sourced in order via the species master script:

| Step | Script | Description |
|------|--------|-------------|
| 0 | `0_ReadData_Plots.r` | Load all data; project to EPSG:22193; map environmental covariates |
| 1 | `1_CommonDolphin.R` / `1_DuskyDolphin.R` | Master scripts — source steps 0, 2, 3, 4, 5, and `UTIL_Map_DSM_output_*` in sequence (run `UTIL_FindTweedieP_*` first to obtain the Tweedie *p* used in step 4) |
| 2 | `2_*_EDA.R` | Exploratory analysis: ship effect, Beaufort filtering, covariate overlays |
| 3 | `3_*_DetectionFunction.R` | Fit and select detection function (AIC); set truncation distance |
| 4 | `4_*_DSM.R` | Fit Tweedie DSMs with spatial smooth + season + environmental covariates |
| 5 | `5_CommonDolphin_Abundance.R` / `5_DuskyDolphin_Abundance.R` | Estimate abundance (N̂) and density (dolphins km⁻²) with 95% lognormal CIs for every surveyed season × year, using the selected base model `count ~ s(x,y) + season + s(Ano)`. Model-based estimates come from `dsm_var_gam` (delta-method variance over the per-cell-area prediction grid); a design-based Horvitz–Thompson estimate is computed alongside as a check. Writes a summary CSV and abundance/density plots (season-faceted and continuous) to `output/<Species>/Abundance/`. Requires the step-4 workspace objects (`*.dsm.xy.season.year`, `pred.polys_m`, `survey.area_m`, `segdata`, `obsdata_*_mod`, `trunc.dist_*`) and sources `R/lnorm_ci.R` |
| — | `UTIL_FindTweedieP_*.R` | Grid search for Tweedie *p* parameter (run before step 4) |
| — | `UTIL_Map_DSM_output_*.R` | Four maps per species: season-faceted and year-faceted versions of the base model (`s(x,y) + season + s(Ano)`) and the top env-covariate model (clo for common dolphin, depth for dusky); `s(Ano)` evaluated at the 2015 survey year for season maps (median), season fixed at Spring for year maps |

### Detection functions

| Species | Key function | Truncation | Beaufort cutoff |
|---------|-------------|------------|-----------------|
| Common dolphin | Hazard-rate | 325 m | ≤ 4 |
| Dusky dolphin | Half-normal | 450 m | ≤ 3 |

Distance bins are defined with cutpoints to account for rounding heaping at favored distances.

### Density surface models

DSMs use `dsm::dsm()` with a Tweedie family. The power parameter *p* is selected by AIC grid search (`UTIL_FindTweedieP_*.R`) before model fitting (common dolphin *p* = 1.58; dusky dolphin *p* = 1.31).

Both species use the same 22-model candidate set, organised into systematic groups:

| Group | Formula structure |
|-------|-------------------|
| Spatial | `s(x,y)` |
| Spatial + Season | `s(x,y) + season` |
| Spatial + Year | `s(x,y) + s(Ano)` |
| Spatial + Season + Year | `s(x,y) + season + s(Ano)` |
| Spatial + Season + Year + env | `s(x,y) + season + s(Ano) + s(env)` — each of 6 covariates |
| Spatial + Season + env | `s(x,y) + season + s(env)` — each of 6 covariates |
| Spatial + Year + env | `s(x,y) + s(Ano) + s(env)` — each of 6 covariates |

Year enters as a continuous thin-plate spline `s(Ano)`. Model selection is by AIC; deviance explained is reported for all candidates. All models use REML. Offset = `segment_length × truncation_distance`.

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
sf, terra                    # Spatial data
data.table, tidyverse        # Data wrangling
ggplot2, patchwork, viridis  # Visualisation
gratia                       # GAM diagnostics
```

---

## Reference

Miller, D.L., Burt, M.L., Rexstad, E.A., & Thomas, L. (2013). Spatial models for distance sampling data: recent developments and future directions. *Methods in Ecology and Evolution*, 4(11), 1001–1010. <https://doi.org/10.1111/2041-210X.12105>

---

## Authors

**Alejandro Buren** (analysis) — CONICET-IAA  
**PI: Dr. Silvana Dans** — CONICET / CESIMAR / UNPSJB / Fundación Azara
