# Dolphin Abundance in the San Matías Gulf, Argentina

Distance sampling and density surface models (DSMs) for two dolphin species in the San Matías Gulf, Patagonia, Argentina.

**Species:**
- *Delphinus delphis* — Common dolphin (`dd`)
- *Lagenorhynchus obscurus* — Dusky dolphin (`lo`)

**Survey period:** 2007–2015  
**Website:** <https://adbpatagonia.github.io/GolfoSanMatias_DolphinAbundance/>

---

## Research questions

1. Does dolphin density vary by season?
2. Did dolphin density change across survey years?

---

## Repository structure

```
analysis/           R scripts (numbered by execution order)
data/
  DistanceData/     Distance sampling input CSVs
  shp/              Survey area and prediction grid shapefiles
output/             Generated figures and model output
  CommonDolphin/
  DuskyDolphin/
  EnvVars/
assets/             Manuscript draft, reference PDFs, and supplementary materials
references/         BibTeX bibliography files
*.qmd               Quarto reports (rendered to docs/)
docs/               Built website (GitHub Pages)
```

---

## Analysis pipeline

Scripts are sourced in order via the species master script:

| Step | Script | Description |
|------|--------|-------------|
| 0 | `0_ReadData_Plots.r` | Load all data; project to EPSG:22193; map environmental covariates |
| 1 | `1_CommonDolphin.R` / `1_DuskyDolphin.R` | Master scripts — source steps 0, 2, and 3 in sequence; step 4 must be run separately |
| 2 | `2_*_EDA.R` | Exploratory analysis: ship effect, Beaufort filtering, covariate overlays |
| 3 | `3_*_DetectionFunction.R` | Fit and select detection function (AIC); set truncation distance |
| 4 | `4_*_DSM.R` | Fit Tweedie DSMs with spatial smooth + season + environmental covariates |
| — | `UTIL_FindTweedieP_*.R` | Grid search for Tweedie *p* parameter (run before step 4) |
| — | `UTIL_Map_DSM_output_*.R` | Generate predicted density maps from fitted DSMs |

### Detection functions

| Species | Key function | Truncation | Beaufort cutoff |
|---------|-------------|------------|-----------------|
| Common dolphin | Hazard-rate | 325 m | ≤ 4 |
| Dusky dolphin | Half-normal | 450 m | ≤ 3 |

Distance bins are defined with cutpoints to account for rounding heaping at favored distances.

### Density surface models

DSMs use `dsm::dsm()` with a Tweedie family. The power parameter *p* is selected by AIC grid search (`UTIL_FindTweedieP_*.R`) before model fitting.

**Common dolphin** (*p* ≈ 1.58): candidate models include spatial smooth `s(x,y)`, season, year, and environmental covariates (depth, slope, gradient, SST, chlorophyll, distance-to-upwelling).

**Dusky dolphin** (*p* ≈ 1.31–1.33): simpler model set — spatial smooth, season, depth, slope, and year as a random effect.

All models use REML. Offset = `segment_length × truncation_distance`.

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
