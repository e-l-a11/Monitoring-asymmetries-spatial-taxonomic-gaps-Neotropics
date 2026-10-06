#!/bin/bash
# usage (from the repository root): scripts/05_sampbias/run_job.sh <terra|mar> <A|B> <grid_deg> <iterations> <seed>
realm=$1; variant=$2; res=$3; iters=$4; seed=$5
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
mkdir -p results/sampbias_runs
for try in 1 2 3; do
  nice -n 10 Rscript scripts/05_sampbias/02_sampbias_model.R data/site_data.csv data/site_habitat_class.csv data/research_institutions_ror_lac.csv $realm $variant $res $iters $seed final > "results/sampbias_runs/log_${realm}_${variant}_seed${seed}.log" 2>&1
  if ls results/sampbias_runs/sampbias_v6_${realm}_${variant}_${res}deg_it*_seed${seed}_final.rds >/dev/null 2>&1; then exit 0; fi
done
exit 1
