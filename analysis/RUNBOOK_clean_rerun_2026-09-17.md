# Clean re-run runbook — 2026-09-17

Goal: one output tree per species in which every file is either (a) written by
current code in this run, or (b) an explicitly dated decision record restored
from quarantine. Nothing stale, nothing orphaned.

Invoke a step by name: "run step 3", "do steps 1 and 2".

Helper scripts sit beside this file in `analysis/`:
`restore_dd_decision_records.ps1` (step 9) and `diff_output_vs_quarantine.ps1`
(step 10). Both hardcode the repo path.

Steps 5 and 7 are the long ones (hours). Everything else is seconds to minutes.

---

## Step 0 — prerequisite (DONE, committed 874aa0b)

`UTIL_DSM_TailMisfit_Impact_DD.R` added to `9_RegenerateStudies_DD.R`, last,
after `UTIL_DSM_TunedArm_MapsAbundance_DD.R`. Driver header corrected to list
`UTIL_DSM_SoapTuning_DD.R` as a fourth decision record. `A2_inflation.csv` added
to the TailMisfit OUTPUT header.

Nothing to run. Listed so the runbook is complete.

---

## Step 1 — quarantine the DD output tree

Move (not delete) `output/CommonDolphin` → `..\_quarantine_DD_output_20260917`,
then verify the file count landed before anything else happens.

Takes `dd_output.RData` with it — 2.26 GB, dated Sep 3, pre-retune and pre-lag1,
and the file `DelfinesComunes.qmd` currently load()s. `*.RData` is gitignored, so
after this move it exists in exactly one place.

Leaves `output/` root alone: `EnvVars/`, `SpeciesPlots.png` and
`env_covariate_assignment_comparison.csv` are shared or gitignored and are not
part of either species' tree.

**Reversible**: yes, it is a move on the same volume.
**Time**: seconds.

---

## Step 2 — snapshot the LO tree before it is touched  (DONE 2026-09-17)

`output/DuskyDolphin/` is not empty. It holds **73 files, 39,612 bytes**: 70 in
`DSM/.cache_soaptune_lo/`, plus three `DSM/LO_soap_*.csv` written 2026-09-16
19:30 by the soap tuning run that happened AFTER the LO quarantine. None of the
three exists in `../_quarantine_LO_output_20260916`, so the step 10 diff alone
would not have flagged them -- which is the whole reason for snapshotting.

`DSM/autocorrelation/` is an EMPTY directory created by UTIL_EnsureOutputDirs.R,
not held-over output. The LO autocorrelation files exist only in the quarantine
and are rebuilt by UTIL_DSM_Diagnostics.R inside step 7, not step 8.

Snapshot written OUTSIDE the repo, so it neither pollutes `git status` nor gets
picked up as "old tree" by the step 10 diff:

    D:\Buren_files\IAA\IAA_analyses\_rerun_snapshots_20260917\
        LO_pre_rerun_manifest.csv   73 rows: path, bytes, mtime, SHA256
        LO_pre_rerun_dirs.txt       empty dirs vanish from a file manifest
        LO_pre_rerun_gitref.txt     HEAD at snapshot time

SHA256 is recorded rather than just size and mtime because the interesting
question at step 10 is not whether the three CSVs were touched but whether they
came back IDENTICAL. `FORCE_CONFIG` pins step 8 to the same 500/250/10x8
configuration that produced them, so byte-identical is the expected result and
is a real reproducibility check. A changed hash means the pinned configuration
did not reproduce its own output and needs explaining before anything is
committed.

**Time**: seconds.

---

## Step 3 — strip every cache: BOTH species refit cold  (DONE 2026-09-17)

This step originally seeded `.cache_covk_lo` with seven `*_k20.rds` to save ~77
min of fs fitting. **That was reversed.** Seeding it exposed the reason: the
cached rows are dated **2026-09-10**, six days before `UTIL_DSM_CovariateK_LO.R`
was committed (7203b10), and they were fitted against a workspace that no longer
exists. Their schema checks out — all 18 columns, correlogram ones included —
and the AICs match the stored CSV, so they were usable. ADB's call was that a
risk of the two workspaces disagreeing is not worth ~77 minutes. Correct call:
the whole point of this re-run is that output cannot be traced to a workspace
nobody can inspect.

So `output/` now holds **no cache at all**:

| cache | files | moved to |
|---|---|---|
| `.cache_covk_lo` | 7 | `_rerun_snapshots_20260917\_caches_removed_for_cold_refit\` |
| `.cache_soaptune_lo` | 70 | same |

Moved, not deleted. `.cache_soaptune_lo` is NOT in the LO quarantine — it was
written on 2026-09-16 *after* it — so that holding directory is its only copy
outside git history.

The DD side needed nothing: step 1 moved the entire `output/CommonDolphin` tree,
its five caches with it.

Verified no other prior state can leak in:

* neither LO script has a CSV-level stage cache (the `file.exists() -> fread()`
  shortcut that `UTIL_DSM_CovariateK_DD.R` uses at its line 179). They cache only
  through `cached_fit_row()`, against the directories now gone.
* `UTIL_DSM_SoapTuning_LO.R` prefers the in-memory workspace over `load()`ing any
  `.RData`, so running it inside step 8's session cannot pick up a stale one.
* the three tracked `LO_soap_*.csv` are left in place ON PURPOSE. They are
  output, not cache; step 8 overwrites them, and because `FORCE_CONFIG` pins the
  configuration that wrote them, byte-identical output is the expected result and
  a silent `git status` is the check.

WHAT THIS BUYS. The 2026-09-10 values in
`_rerun_snapshots_20260917\LO_covk_expected_values.md` were going to be the
licence for reusing the cache. With nothing cached they become an independent
reproducibility check instead: every one of them is recomputed from scratch by a
workspace that has never seen them. That file also says how to read a mismatch —
k=10 differing means the WORKSPACE differs (a pipeline finding), k=20 differing
alone points at mgcv convergence (a methods note).

**Cost**: ~77 min of fs fitting in step 8, plus ~45 soap fits at 20-250 s.

**Time**: seconds.

---

## Step 4 — pre-flight gate  (PASSED 2026-09-17)

Ten hours of fitting is a long way to get before finding a missing package, so
this gate checks the things that fail late, not just the bookkeeping.

| check | result |
|---|---|
| `git status` | clean, HEAD 899d04a |
| `output/` cache dirs | 0 — both species cold |
| `output/` contents | 3 tracked `LO_soap_*.csv` + the shared `EnvVars/` root |
| DD quarantine | 372 files |
| LO quarantine | 81 files |
| caches held aside | 77 files |
| driver patches | TailMisfit in 9_DD, FORCE_CONFIG in 9_LO |
| **parse** | 35 scripts, 0 failures, 0 missing |
| **packages** | 20 referenced, all installed (R 4.6.1, mgcv 1.9.4, dsm 2.3.4, sf 1.1.2) |
| **data inputs** | 9 CSVs + 3 shapefiles present |
| **disk** | 192.8 GB free on D:; run needs ~4.5 GB |
| Excel | not running (step 6 hazard) |

### What the gate caught

Neither driver clears the workspace, and both end with
`save(list = ls(envir = .GlobalEnv), ...)`. Anything already in the session is
written into the new `.RData`. Two RStudio sessions were open at gate time, so
this was a live risk rather than a theoretical one. Steps 5 and 7 now carry a
FRESH-SESSION instruction; see step 5 for why it matters more at step 7 than at
step 5.

This is the same failure the drivers' own `save(list = ls())` comment guards
against for DOTTED names -- it just never covered ordinary leftovers.

---

## Step 5 — `1_CommonDolphin.R`  ⏳ LONG

Full DD pipeline: read data → EDA → detection function → DSM (18 fs + soap
block) → soap → abundance → nobs2 sensitivity → maps → CV maps → diagnostics →
save `dd_output.RData`.

**Time**: hours — the 18 `bs = "fs"` fits dominate at roughly 11 min each. Budget
4–6 h and treat that as an estimate, not a measurement.

**START FROM A FRESH R SESSION.** Restart R (Session -> Restart R, Ctrl+Shift+F10)
or `rm(list = ls(all.names = TRUE))` before sourcing. This is not hygiene, it is
the last contamination route left open:

* neither driver clears the workspace, and both end with
  `save(list = ls(envir = .GlobalEnv), ...)`. Every object sitting in the session
  when you start is therefore written into the new .RData.
* the `save(list = ls())` comment in the drivers explains why that beats
  `save.image()` -- it keeps DOTTED config objects out. It does nothing about
  ordinary leftovers. Start step 5 in a session that still holds `lo.dsm.*` and
  `dd_output.RData` ends up carrying dusky models, which `DelfinesComunes.qmd`
  then `load()`s into the common-dolphin report.
* worse, a leftover object can satisfy an `exists()` guard in one of the 9_
  drivers and let a script run against the wrong species' fit without erroring.

**Then keep that session open** — step 6 needs the workspace it built.

---

## Step 6 — `9_RegenerateStudies_DD.R`  ⏳ MEDIUM

Same R session as step 5 (its guard is `if (!exists("dd.dsm.soap.season.year"))`),
or `load("output/CommonDolphin/dd_output.RData")` first.

Sources, in this order:
1. `UTIL_DSM_TailFix_DD.R` — group-size variance decomposition, twlss ladder
2. `UTIL_DSM_TunedArm_MapsAbundance_DD.R` — tuned density maps + abundance
3. `UTIL_DSM_TailMisfit_Impact_DD.R` — **new here**; reads `dd_tuned_base.rds`
   and `DD_abundance_tuned.csv` from (2), so the order is load-bearing

**Before running**: close any of these CSVs in Excel. All three scripts divert a
locked `fwrite` to `*_new.csv` and carry on, which produces a silent hole rather
than an error. Step 9 sweeps for that, but not writing it is better.

**Time**: tens of minutes to ~2 h.

---

## Step 7 — `1_DuskyDolphin.R`  ⏳ LONG

Full LO pipeline, same shape as step 5. Also 18 `bs = "fs"` fits.

**Time**: hours. Budget 4–6 h.

**START FROM A FRESH R SESSION**, for the reason spelled out under step 5: the
driver saves `ls(envir = .GlobalEnv)` wholesale, so whatever the session already
holds is written into `lo_output.RData`. After step 6 the session holds a full
common-dolphin workspace, so this matters MORE here than it did at step 5, not
less -- do not simply carry on in the same R session you used for steps 5 and 6.

**Keep the R session open** for step 8.

---

## Step 8 — `9_RegenerateStudies_LO.R`  ⏳ LONG

Same session as step 7.

Runs COLD — step 3 removed every cache, so nothing here resumes.

1. `UTIL_DSM_CovariateK_LO.R` — 16 fits, the seven k=20 ones ~11 min each. It
   still prints `reproduction guard: refit AIC ... -> OK` or `MISMATCH` first.
   Cold, that guard now checks only that `.fit_fs()` reproduces the workspace's
   own stored base, which it should; `MISMATCH` here would mean the pipeline
   disagrees with itself and is a genuine stop.

   Then compare the CSV it writes against
   `_rerun_snapshots_20260917\LO_covk_expected_values.md` (2026-09-10 values,
   recomputed from scratch here, so a real check rather than a tautology).
   That file says how to read a mismatch: k=10 differing means the WORKSPACE
   differs, k=20 differing alone points at mgcv convergence.

2. `UTIL_DSM_SoapTuning_LO.R`, with `FORCE_CONFIG <- list(tol = 500, margin = 250,
   ngrid = c(10L, 8L))` set by the driver. **Do not remove it** — without it the
   script auto-picks the lowest-AIC configuration (26×21 / 361 knots), which is
   precisely what the study concluded against, and writes a tuned arm into
   `output/` that contradicts its own RESULT block.

   ~45 soap fits at 20-250 s. Its three CSVs are tracked and should come back
   byte-identical under the pinned config — a silent `git status` is the result.

**Time**: ~2.5–3 h, all of it cold.

---

## Step 9 — restore the 16 DD decision records

`restore_dd_decision_records.ps1`. Copies, never moves — the quarantine stays
intact. Warns instead of overwriting, because a file already present means the
re-run wrote something that should not exist.

| study | files |
|---|---|
| `UTIL_DSM_CovariateK_DD.R` | `DD_covariate_k_comparison`, `DD_soap_spatial_basis_check`, `DD_covariate_k20_diagnostics`, `DD_covariate_k20_denseknots`, `DD_covariate_2x2_grid`, `DD_covariate_2x2_attribution` |
| `UTIL_DSM_SoapRevised_DD.R` | `DD_soap_revised_selection`, `DD_soap_revised_paired` |
| `UTIL_DSM_RootogramKnots_DD.R` | `DD_rootogram_knotgrid{,_block}.csv`, `DD_rootogram_knotgrid.png` |
| `UTIL_DSM_SoapTuning_DD.R` | `DD_soap_boundary_variants`, `DD_soap_knot_sweep`, `DD_soap_tuned_selection`, `DD_soap_knot_correlogram` |
| `UTIL_DSM_CovariateK_DD_Figure.R` | `DD_two_defects.png` (or re-run the script — it only `fread`s the CSVs above) |

All 16 verified present on disk before step 1.

**Time**: seconds.

---

## Step 10 — verify  (PASSED 2026-09-19, with one real finding)

| check | result |
|---|---|
| DD tree vs quarantine | 2 not reproduced, 1 new — both explained below |
| LO tree vs quarantine | 2 not reproduced, 4 new — both explained below |
| `*_new.csv` sweep | none: every `fwrite` reached its intended filename |
| tracked files missing | none; `git status` on `output/` clean |
| restored records | 16/16 byte-identical to quarantine (SHA256) |

**Not reproduced, both species**: `DSM/{DD,LO}_dsm_correlogram.{csv,png}`. The old
trees held these in BOTH `DSM/` and `DSM/autocorrelation/`; current
`UTIL_DSM_Diagnostics.R` writes only to `.acf_dir` (= `autocorrelation/`). The
root-level pair are orphans of an older diagnostics script. Correctly dropped —
this is the stale output the re-run existed to remove.

**New**: `Nobs2Sensitivity/README_nobs2_fit_compare.txt` (both species), written
by the current `6_*_Nobs2SensitivityAnalysis.R` since 6623a6d. Plus, LO only, the
three `LO_soap_*.csv` — absent from the 2026-09-16 quarantine because they
postdate it, which is exactly why step 2 snapshotted them separately.

### Reproducibility check A — covariate-k: EXACT

Every `AIC_10`, `AIC_20`, `dAIC`, `edf`, `edf_frac` and `Dev` in the fresh
`LO_covariate_k_comparison.csv` matches the 2026-09-10 values to the last
decimal, recomputed cold by a workspace that had never seen them. Only `k_index`
and `p_value` differ, and they should: `dsm_kcheck(m, n.rep = 400)` is a
randomised test with no seed set. `at_ceiling` is FALSE throughout in both.

The LO fs block reproduces exactly across a nine-day, fully cold rebuild.

### Reproducibility check B — soap CSVs: ONE REAL CHANGE

`LO_soap_boundary_variants.csv` and `LO_soap_knot_sweep.csv` came back
byte-identical. `LO_soap_tuned_selection.csv` did not:

* all 25 `tuned` rows identical; all 25 `original` rows changed
* `k_prime_xy` **49 -> 48** in every changed row, with `edf_frac_xy` and AIC
  moving consequentially (~0.1-0.2 AIC)

CAUSE, and it is not a defect in this run. The `original` arm reads its knots
from the WORKSPACE. The workspace the 2026-09-16 study used was
`lo_output.RData` dated **2026-09-03 21:59** — thirteen days older than
0fccbb9 (2026-09-16 18:18), *"measure LO knot clearance to the boundary edges,
not its vertices"*. So that study measured its `original` arm against a soap
basis the code had already stopped building, and one knot the vertex test wrongly
admitted is now correctly excluded. 49 -> 48 is the bug fix finally reaching the
workspace.

CONSEQUENCES, checked rather than assumed:
* the study's conclusion holds. The nominal winner is still the `tuned` sst model
  at deltaAIC 0 with `lag1_sig = TRUE` — it still fails the autocorrelation check
  its own AIC assumes. The margin over the best `original` narrowed from 1.06 to
  0.85 AIC. Adopt nothing, for the same reason as before.
* the reported dusky numbers are untouched. Every fs-based LO abundance table is
  byte-identical to the quarantine — `LO_abundance_season_year{,_byyear,_fsyear}.csv`
  and `LO_year_partial_effect_fsyear.csv`. Only `_soap.csv` changed, and the soap
  arm is a considered-not-reported block for dusky.

**Time**: seconds, plus however long the one finding takes to run down.

---

## Step 11 — dispose

Only after step 10 passes: delete both quarantines, or keep them. Your call,
~4.5 GB between them. Commit the regenerated `output/` tree.
