#!/bin/bash
# Runs the 16 sampbias chains (land/sea x all records/one per project per cell x 4 seeds; 1,000,000 iterations each)
# 8 in parallel, then consolidates. Run from the repository root:  bash scripts/05_sampbias/run_all.sh
set -e
Rscript scripts/05_sampbias/01_oceanic_islands.R
for r in "mar 0.5" "terra 1"; do set -- $r; for v in A B; do for s in 1 2 3 4; do echo "$1 $v $2 1000000 $s"; done; done; done |
  xargs -P 8 -L 1 bash -c 'scripts/05_sampbias/run_job.sh "$0" "$1" "$2" "$3" "$4"'
Rscript scripts/05_sampbias/03_consolidate_chains.R
