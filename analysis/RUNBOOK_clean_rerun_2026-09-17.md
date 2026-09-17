# Clean re-run runbook — 2026-09-17

Goal: one output tree per species in which every file is either (a) written by
current code in this run, or (b) an explicitly dated decision record restored
from quarantine. Nothing stale, nothing orphaned.

Invoke a step by name: "run step 3", "do steps 1 and 2".

Helper scripts sit beside this file in `analysis/`:
`restore_dd_decision_records.ps1` (step 9) and `diff_output_vs_quarantine.ps1`
(step 10). Both hardcode the repo path.

DELIBERATELY NOT COMMITTED. This is operational state for one re-run, not part
of the analysis. It will show as untracked in `git status` until the re-run is
finished and it is deleted or, if it turns out to be worth keeping, committed on
purpose.

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

## Step 2 — snapshot the LO tree before it is touched

`output/DuskyDolphin/DSM/` is not empty: it holds `autocorrelation/` and three
`LO_soap_*.csv` from the 2026-09-16 tuning run that happened after the LO
quarantine. Step 7 overwrites all four. Snapshot now so that is confirmed at
step 9 rather than assumed.

**Time**: seconds.

---

## Step 3 — seed the LO covariate-k cache, selectively

Copy **only** the seven `*_k20.rds` from
`..\_quarantine_LO_output_20260916\DSM\.cache_covk_lo\` into
`output/DuskyDolphin/DSM/.cache_covk_lo/`.

Deliberately NOT copied:

| file | why not |
|---|---|
| `GUARD_base_refit.rds` | it is the cache's own validity check. Cached, it returns a stored `ok` without ever comparing against the new workspace. Left out, it refits once (~11 min) and licenses the seven k20 rows. |
| `BASE_diag.rds` | two correlograms, seconds. No reason to carry a stale one. |
| seven `*_k10.rds` | these read the stored k=10 models from the workspace rather than fitting them. Cheap, and re-deriving them means the k=10 arm comes from the NEW run. |

Saves ~77 min of fs fitting (7 × ~11 min) at the cost of ~11 min for the guard.

Licensed by: `UTIL_DSM_CovariateK_LO.R` has exactly one commit (7203b10,
2026-09-16), so the cache was written by the current version and its row schema
matches. `6623a6d` touched `4_DuskyDolphin_DSM.R` only to append `lag1`/`lag1_sig`
columns to selection tables — no formula, no data change. `cached_fit_row()`
invalidates by key only, never by hashing the expression, which is why this
needed checking rather than assuming.

**If you would rather not**: skip this step. Costs ~77 extra minutes in step 7,
changes nothing else.

**Time**: seconds.

---

## Step 4 — checkpoint before the long runs

Confirm: DD tree quarantined and counted, LO snapshot taken, LO cache seeded,
`git status` clean for `analysis/`. Nothing else to do; this is the gate.

---

## Step 5 — `1_CommonDolphin.R`  ⏳ LONG

Full DD pipeline: read data → EDA → detection function → DSM (18 fs + soap
block) → soap → abundance → nobs2 sensitivity → maps → CV maps → diagnostics →
save `dd_output.RData`.

**Time**: hours — the 18 `bs = "fs"` fits dominate at roughly 11 min each. Budget
4–6 h and treat that as an estimate, not a measurement.

**Run from an R session you will keep open** — step 6 needs the workspace.

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

**Keep the R session open** for step 8.

---

## Step 8 — `9_RegenerateStudies_LO.R`  ⏳ LONG

Same session as step 7.

1. `UTIL_DSM_CovariateK_LO.R` — **watch the first line of output**. It prints
   `reproduction guard: refit AIC ... -> OK` or `MISMATCH`. `MISMATCH` stops the
   script and means the step-3 cache was not licensed; delete
   `.cache_covk_lo/` and re-run cold.
2. `UTIL_DSM_SoapTuning_LO.R`, with `FORCE_CONFIG <- list(tol = 500, margin = 250,
   ngrid = c(10L, 8L))` set by the driver. **Do not remove it** — without it the
   script auto-picks the lowest-AIC configuration (26×21 / 361 knots), which is
   precisely what the study concluded against, and writes a tuned arm into
   `output/` that contradicts its own RESULT block.

**Time**: ~1–2 h with the seeded cache, ~2.5–3 h without.

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

## Step 10 — verify

Three checks; all three must pass before either quarantine is deleted.

1. `diff_output_vs_quarantine.ps1 -Species CommonDolphin` — every name surviving
   on the "not reproduced" side must be one of the 16 decision records.
2. `diff_output_vs_quarantine.ps1 -Species DuskyDolphin` — that side should be
   empty. Memory says 29 untracked LO diagnostics exist only in the quarantine;
   this is where that gets settled.
3. `Get-ChildItem output -Recurse -Filter '*_new.csv'` returns nothing.

Then `git status` on `output/` — the 51 LO deletions should all be resolved, and
any file still showing as deleted is a hole worth understanding before it is
committed.

**Time**: seconds.

---

## Step 11 — dispose

Only after step 10 passes: delete both quarantines, or keep them. Your call,
~4.5 GB between them. Commit the regenerated `output/` tree.
