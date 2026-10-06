# Reproduces every table, number and figure of the paper, in order. Run from the repository root:
#   Rscript run_all.R            (everything except the sampbias MCMC; uses results/sampbias_results.csv)
#   Rscript run_all.R sampbias   (also re-runs the 16 sampbias chains first; ~25 min on 8 cores)
# Figure 4 and the recomputation of data/site_ecoregions.csv need the ecoregion shapefiles in data/shapefiles/ (see README).
args <- commandArgs(trailingOnly = TRUE)
passo <- function(f) { cat("\n====", f, "====\n"); t0 <- Sys.time(); source(f, local = new.env(), echo = FALSE)
  cat(sprintf("---- ok (%.0f s)\n", as.numeric(difftime(Sys.time(), t0, units = "secs")))) }
passo("scripts/01_fix_site_coordinates.R")
passo("scripts/02_clean_spreadsheets.R")
passo("scripts/03_ecoregions.R")
passo("scripts/04_density_figure1.R")
if ("sampbias" %in% args) stopifnot(system("bash scripts/05_sampbias/run_all.sh") == 0)
passo("scripts/06_figure2_sampbias.R")
passo("scripts/07_figure3_figureS1.R")
passo("scripts/08_duration_lifespan_figureS4.R")
if (file.exists("data/shapefiles/wwf_terr_ecos.shp")) passo("scripts/09_figure4_figuresS2_S3.R") else
  cat("\n(Skipping Figure 4 / S2 / S3: ecoregion shapefiles not found in data/shapefiles/)\n")
cat("\nAll done. Tables in results/, figures in figures/, logs in logs/.\n")
