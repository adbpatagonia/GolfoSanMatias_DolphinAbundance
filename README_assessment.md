# README.md — top-to-bottom assessment

Audited 2026-09-19 against the working tree. README.md last changed 2026-08-31 (`f1b368e`);
**41 commits have touched `analysis/` or `R/` since**. That gap explains most of what follows.

Verdict: the README is a good *reference document* that has drifted out of date, and it is
not a *usable document* — there is no way to get from "I cloned this" to "I ran this".

---

## A. Blockers for someone working in RStudio

### A1. The input data is not in the repository
`.gitignore` line 12 is `data/DistanceData/`. Every CSV the pipeline reads is untracked.
The README's "Input data" section (L55–81) describes those files in detail and says they
"must be present before running the pipeline" — but never says they are not shipped, and
never says where to get them. A fresh clone dies at `0_ReadData_Plots.r` line 49.

**Fix:** one sentence at the top of the section.

### A2. There is no "how to run this" section at all
Nowhere does the README say to open `GolfoSanMatias_DolphinAbundance.Rproj` and
`source()` a driver. This is the single biggest omission for an RStudio user.

### A3. The project is an RStudio *package* project and the README never says so
`.Rproj` sets `BuildType: Package`, `PackageUseDevtools: Yes`, `PackageRoxygenize: rd,collate,namespace`.
So RStudio shows a **Build** pane, and:
- **Ctrl+Shift+B** — Install and Restart
- **Ctrl+Shift+T** — run the `testthat` suite
- **Ctrl+Shift+D** — roxygenise `R/` into `man/`

None of these are mentioned. The `tests/` and `man/` folders are listed in the structure
block with no hint of how to use them.

### A4. No `install.packages()` line
"Key dependencies" (L242–251) lists package names in a code fence that is not runnable.
`DESCRIPTION` only declares `stats` / `mgcv` / `testthat`, so `devtools::install_deps()`
will not get you there either.

### A5. Rendering the website will render your working drafts
`_quarto.yml` has `project: type: website` and **no `render:` block**, so every `.qmd` in
the project root is rendered. `DelfinesComunes_extras.qmd` and `DelfinesOscuros_extras.qmd`
are in the root right now, and their own headers say
"NO ES UN DOCUMENTO PARA RENDERIZAR JUNTO AL SITIO". Pressing *Render Website* in RStudio
publishes them. The README should warn about this, or `_quarto.yml` should get a `render:` list.

### A6. The published website is stale
`docs/*.html` last built 2026-09-04. All the diagnostics and tuning work (2026-09-16/17)
post-dates it. The README links the site (L10) as if current.

---

## B. Factually wrong — these will mislead

| # | README | Says | Actually |
|---|--------|------|----------|
| B1 | L139–144 | Detection function "Selected covariate: `nobs_grp`", both species | **Neither final detection function has a covariate.** `3_CommonDolphin_DetectionFunction.R:483` → `df.dd <- dd.df.hr.trun.cp`; `3_DuskyDolphin_DetectionFunction.R:504` → `df.lo <- lo.df.hn.trun.cp`. The comment at L467–471 of that script says `nobs_grp` is explicitly *not* used (confounded with `s(Ano)`). |
| B2 | L101 | "Each master script ends with `save.image()`" | Both use `save(list = ls(envir = .GlobalEnv), ...)`, with a six-line comment explaining why `save.image()` was a bug (it leaked dotted config objects into the `.RData`, which then clobbered the next script that `load()`ed it). `1_CommonDolphin.R:56–63`. |
| B3 | L231 | "The model sets also define an `n_obs` twin of **every** candidate (`<model>.nobs`)… `has_n_obs` is `FALSE`" | No such code exists. `grep -c "\.nobs"` over all four `4_*_DSM*.R` = **0**. `has_n_obs` appears nowhere in `analysis/` or `R/`. The whole mechanism was removed, not deferred. |
| B4 | L235 | n_obs source is `data/DistanceData/segdata con nro observadores embarcacion.csv`; "rename `n obs` → `n_obs`" | That file does not exist (there is an `.xlsx` with a similar name). `0_ReadData_Plots.r:76` reads **`segdata_gui_vana.csv`**, and line 100 renames **`N_ObsAcordado`** → `n_obs`. The file carries both `n obs` and `N_ObsAcordado`; the code takes the latter. |
| B5 | L38 | `output/CommonDolphin/` contains `DetectionFunction/` | That directory does not exist and is never created. `UTIL_EnsureOutputDirs.R` creates `EDA`, `DSM`, `DSM/autocorrelation`, `Abundance`, `Nobs2Sensitivity` per species, plus `output/EnvVars`. |
| B6 | L61 | "CSV files are **semicolon-delimited**" | `segdata_gui_vana.csv` is comma-delimited (and is read with its own `fread` call at L76). |

### B7a. "12 density maps per species" is wrong for common dolphin
`UTIL_Map_DSM_output_LO.R` has 12 real `ggsave()` calls; `UTIL_Map_DSM_output_DD.R` has **14**.
The extra two are `DD_DSM_Tuned_SeasonByModel.png` and `..._log.png` (the tuned soap
season × model tile, added 2026-09-16). The maps table (L116–124) stops at 12.
The DD script also labels *two* different blocks `# Map 12` (lines 1035 and 1115).

### B7b. The dusky reference year is silently overridden
`UTIL_Map_DSM_output_LO.R:400` computes `ref_yf_fs` as the fitted year nearest the median,
then **line 401 overwrites it: `ref_yf_fs <- 2017`**. The median survey year is 2015, and
`ref_yf_byyear` was left alone. So dusky maps 1, 2, 7 and 9 are drawn at 2015 while map 5
is drawn at 2017. The code's own comment at line 396 ("fitted year nearest the median") is
now false, and the common-dolphin script has no such override. README L120/L128 present
this as one uniform rule.

### B7c. Every overlay silently drops 2006
All map overlays use `filter(Ano > 2006)`, but the models are fitted on the whole of
`segdata`, which holds **154 segments from 2006**. So the README's "no more, no less"
promise (L126) is not kept, and the 2006 panel of every year-facet map is blank of tracks.
Either say so, or drop the filter.

### B7d. The overlay rule does not cover maps 11–12
Those facet on a combined `year_season` key and need a re-tagged overlay
(`UTIL_Map_DSM_output_DD.R:973–985`). The three-case rule at L126–131 covers maps 1–10 only.

### B7e. "Fewster R2" is cited but not referenced
L96 names the estimator; the reference list (L257–261) has Miller, Wood and Williams but
no Fewster. Add Fewster et al. (2009), *Biometrics* 65(1), 225–236.

### B7. The model counts contradict each other three times over

| Claim | Location |
|---|---|
| "50 models per species" | L94 |
| "57 models per species" | L164 heading, L190, L237 |
| "22 models per species" (soap) | L95 |
| "25 models per species" (soap) | L184 heading, L190, L237 |
| "72 models per species combined" | L101 |

These are two self-consistent sets, not four random numbers: **50 + 22 = 72** is the old
set, and **57 / 25** is the current one, repeated four times. So L94/L95/L101 are simply
stale — change them to 57, 25 and **82**.

Separately, the *code* fits **60** candidates per species in `4_*_DSM.R`, not 57. The
breakdown table at L168–176 sums to 57 because it omits three models that are fitted:
`*.dsm.xy.ts`, `*.dsm.xy.te` and `*.dsm.xy.year.season.dist.up.grad`. The soap count of 25
is exactly right (3 + 7 + 1 + 7 + 7, generated from `spec` in the soap scripts).
True combined total: **85**.

---

## C. Stale or missing — the README describes an older project

### C1. The pipeline table is missing half the pipeline
L91 says the masters source "steps 0, 2, 3, 4, 5, and `UTIL_Map_DSM_output_*`".
The actual order in `1_CommonDolphin.R` / `1_DuskyDolphin.R` is:

```
UTIL_EnsureOutputDirs.R   <- first, and not in the README at all
0_ReadData_Plots.r
2_*_EDA.R
3_*_DetectionFunction.R
4_*_DSM.R
4_*_DSM_soap.R
5_*_Abundance.R
6_*_Nobs2SensitivityAnalysis.R   <- not in the README at all
UTIL_Map_DSM_output_*.R
UTIL_Map_DSM_output_CV_*.R
UTIL_DSM_Diagnostics.R           <- not in the README at all
save(...)
```

### C2. The whole diagnostics subsystem is undocumented
`UTIL_DSM_Diagnostics.R` (added 2026-09-09, wired into both drivers 2026-09-16) runs
three checks — along-track residual autocorrelation, `k.check` basis dimension, and
rootograms — and its results ride inside the saved workspace as `dd.diag` / `lo.diag`.
Since 2026-09-17 **every AIC table also carries a lag-1 residual autocorrelation column**.
The README's discussion of model selection is purely AIC-based and mentions none of this.

### C3. There is a second driver per species, unmentioned
`9_RegenerateStudies_DD.R` and `9_RegenerateStudies_LO.R` must be run *after* the main
driver to regenerate the non-pipeline studies. The README does not know they exist.

### C4. The soap section predates the retune
L184–190 describes the generic soap setup, which is still true but no longer the whole
story. `4_CommonDolphin_DSM_soap.R:60–71` now pins a tuned configuration adopted
2026-09-16: `simplify_tol = 500`, `margin = 250`, `knot_ngrid = c(14,11)` (89 knots),
`K_COV = 20`. It also carries a warning the README badly needs:

> DO NOT REFINE FURTHER. AIC keeps falling all the way to 485 knots (−132 in total)
> but residual lag-1 autocorrelation rises monotonically with it.

That is a real trap — AIC alone says "more knots", and it is wrong.

### C5. Inventory drift

| README says | Reality |
|---|---|
| `R/` holds 4 files (L26–32) | 7. Undocumented: `cached_fit_row.R`, `dsm_correlogram.R`, `dsm_rootogram.R` |
| `tests/testthat` has 3 tests (L44) | 4 — `test-dsm_rootogram.R` |
| `man/` documents the package | 3 of the 7 `R/` files |
| `output/` has `EDA/ DetectionFunction/ DSM/ Abundance/` (L38–39) | No `DetectionFunction/`; plus `Nobs2Sensitivity/` (both species) and, for DD, `DSM/tail/`, `DSM/TailMisfit/`, `DSM/tuned_models/`, `DSM/autocorrelation/` |
| 6 input CSVs (L63–70) | `0_ReadData_Plots.r` reads **9**: also `distdata_dd_todos2.csv` (L54), `segdata_gui_vana.csv` (L76), `preddataVV.csv` (L82) |
| `preddata.csv` is the prediction grid | L82 **overwrites** `preddata` with `preddataVV.csv`, so `preddata.csv` (read at L81) is discarded |
| `analysis/` = a handful of scripts | ~45 files, including `4_DuskyDolphin_DSM_BiologicalYear.R`, 13 `UTIL_*` studies, two `.ps1` helpers and `RUNBOOK_clean_rerun_2026-09-17.md` |

### C6. `.RData` size is understated
L101 says the workspaces are "too large for git to track". `dd_output.RData` is **2.26 GB**
(per the runbook). Worth stating, because it determines whether you can keep more than one.

---

### C7. Extra input-data detail worth adding
- `data-raw/`, described at L36 as "Scripts/sources used to build data/", is **empty**.
- `distdata_dd_todos2.csv` is read into `distdata_ddoption2` (`0_ReadData_Plots.r:54`) but
  the comment at lines 51–53 records that the providers confirmed it must not be used.
- `segdata con nro observadores embarcacion_gui_vana.xlsx` sits in the folder and is read
  by nothing.
- `preddataVV.csv` is comma-delimited and uses `dist_coast`/`dist_up`, renamed at
  `0_ReadData_Plots.r:84`. `segdata_gui_vana.csv` is comma-delimited with a UTF-8 BOM.
  `fread` auto-detects, so nothing breaks — but L61's blanket "semicolon-delimited" is wrong.

---

## D. Sections that are correct (leave them alone)

- The overlay *principle* (L126–131) matches the code for maps 1–10: shared-term season
  maps are not year-filtered, `fs`/`by`-year season maps use `filter(Ano == ref_yf_*)`.
  (See B7b–B7d for the three things it doesn't cover.)
- The `bam`-vs-`gam` figures (L195–202) match `4_DuskyDolphin_DSM.R:50–51` exactly.
- Uncertainty mapping (L135) — DD uses `dd.dsm.soap.season.year` with season facets,
  LO uses `lo.dsm.xy.fsyear.season` with year facets. Confirmed.
- The four models in the "Models fitted per species" table are all genuinely used in
  `5_*_Abundance.R`.
- The seven environmental covariates, and the `tw()` rationale (L150–158).

---

## E. Simplicity

The README is 3737 words across 24 headings. The reasoning is worth keeping — the problem
is sentence length and burial, not content.

### E1. L96 — the step-5 cell is a 130-word sentence inside a table
Move it out of the table. Suggested:

> **Step 5 — abundance.** Estimates N̂ and density (dolphins km⁻²) with 95% lognormal CIs
> for every surveyed season × year.
>
> - Four models per species are run side by side (see *Models fitted per species*).
> - Model-based variance comes from `dsm_var_gam` (delta method, over the per-cell-area
>   prediction grid).
> - One design-based Horvitz–Thompson estimate is computed and reused for all four,
>   because it does not depend on the model.
> - Writes summary CSVs and plots to `output/<Species>/Abundance/`.

### E2. L126–131 — the overlay rule needs three reads
Replace with the principle plus a table:

> **Rule:** each panel's overlay shows exactly the data that informed the surface in that panel.
>
> | Map facet | Spatial term | Overlay filtered by |
> |---|---|---|
> | Season | shared (`s(x,y)`, soap) | nothing — every year informed the one shared surface |
> | Season | year-varying (`fs`, `by`) | that reference year only |
> | Year | any | nothing — ggplot already restricts each panel to its own year |

### E3. L225 — the data-quality blockquote is one 150-word block
Keep every number; break into "What's wrong / How we know / What it means for the fits".

### E4. L156–158, L178, L223, L235–238 — long single sentences
Each carries one idea per clause; splitting costs nothing.

### E5. Redundancy
The `bam`-vs-`gam` story is told at L162 and again in full at L195–202. Keep the second,
make the first a pointer.

---

## F. Recommended structure

The README currently runs: questions → structure → data → pipeline → ~100 lines of
modelling → covariates → deps → refs. Reference material sits where a quick start should be.

Proposed order:

1. **What this is** (3 lines + the website link)
2. **Quick start in RStudio** ← new
3. **What you need** — data (not in the repo), packages, one `install.packages()` call
4. **How to run it** — the two drivers, the two `9_` drivers, expected runtime
5. **Where the output goes**
6. **Repository structure**
7. *— everything below is reference —*
8. Input data · Pipeline detail · Models · Maps · Diagnostics · Covariates · Known issues · References

### Drafted Quick start

```markdown
## Quick start (RStudio)

**Just want to read the results?** Open <https://adbpatagonia.github.io/GolfoSanMatias_DolphinAbundance/>
— nothing to install. (Note: last built 2026-09-04.)

**Want to re-run the analysis?**

1. Open `GolfoSanMatias_DolphinAbundance.Rproj`. This sets the working directory;
   every path in the code uses `here::here()`, so nothing else needs configuring.
2. Get the input data. `data/DistanceData/` is **not in the repository** — ask ADB for it
   and unzip it there. `data/shp/` is tracked and already present.
3. Install the packages (see *What you need*).
4. Run one species. In the Console:

   ```r
   source(file.path(here::here(), "analysis", "1_CommonDolphin.R"))   # common dolphin
   source(file.path(here::here(), "analysis", "1_DuskyDolphin.R"))    # dusky dolphin
   ```

   **This takes hours** — most of it is the ~85 GAMs per species in step 4. It ends by
   saving the whole workspace to `output/<Species>/<species>_output.RData` (~2 GB).
5. Regenerate the side studies (fast, optional):

   ```r
   source(file.path(here::here(), "analysis", "9_RegenerateStudies_DD.R"))
   ```
6. Build the report. Build pane → **Render Website**, output lands in `docs/`.
   The `.qmd` files `load()` the `.RData` from step 4, so you can re-render without refitting.
   ⚠️ Every `.qmd` in the project root is rendered, including any `*_extras.qmd` drafts.

**Package work** (the project is also an R package, `gsmdolphins`):
Ctrl+Shift+B install · Ctrl+Shift+T tests · Ctrl+Shift+D roxygenise `R/` → `man/`.
```

---

## G. Priority

1. B1 (detection-function covariate) — the only error that misstates a published result.
2. A1 + A2 (data not in repo, no quick start) — the usability blockers.
3. B7 + B2 + B3 + B4 (counts, `save.image()`, `n_obs`).
4. C1–C4 (pipeline table, diagnostics, `9_` drivers, soap retune).
5. E (simplicity) and F (structure).
