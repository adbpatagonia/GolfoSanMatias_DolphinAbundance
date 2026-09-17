# Contamination check: what exists in the OLD output tree but not in the NEW one,
# and vice versa. Every surviving name on the left is either a declared decision
# record or a hole. Every name on the right is new output -- expected, but worth
# reading once.
#
# Usage:
#   .\diff_output_vs_quarantine.ps1 -Species CommonDolphin -Quarantine 'D:\...\_quarantine_DD_output_20260917'
#   .\diff_output_vs_quarantine.ps1 -Species DuskyDolphin  -Quarantine 'D:\...\_quarantine_LO_output_20260916'

param(
  [Parameter(Mandatory = $true)][string]$Species,
  [Parameter(Mandatory = $true)][string]$Quarantine
)

$ErrorActionPreference = 'Stop'
$repo = 'D:\Buren_files\IAA\IAA_analyses\GolfoSanMatias_DolphinAbundance'
$new  = Join-Path $repo "output\$Species"

function Rel($root) {
  Get-ChildItem $root -Recurse -File -Force |
    Where-Object { $_.FullName -notmatch '\\\.cache' } |   # caches are working state, not output
    ForEach-Object { $_.FullName.Substring($root.Length + 1) }
}

$old = @(Rel $Quarantine)
$cur = @(Rel $new)

Write-Host "`n=== in OLD tree, NOT reproduced by the re-run ($Species) ===" -ForegroundColor Yellow
$gone = $old | Where-Object { $cur -notcontains $_ }
if ($gone) { $gone | Sort-Object } else { Write-Host '(none)' }

Write-Host "`n=== NEW in the re-run, absent from the old tree ($Species) ===" -ForegroundColor Cyan
$added = $cur | Where-Object { $old -notcontains $_ }
if ($added) { $added | Sort-Object } else { Write-Host '(none)' }

Write-Host ("`nold {0} files, new {1} files" -f $old.Count, $cur.Count)
