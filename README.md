# Monitoring asymmetries leave spatial and taxonomic gaps in the Neotropics

Data and code for: Abbad, E. L., Dambros, C., Cronemberger, C., Severo, L. W., Peixoto, T. W. & Bender, M. *Monitoring asymmetries leave spatial and taxonomic gaps in the Neotropics* (manuscript).

The repository reproduces every table, number and figure of the paper from the compiled spreadsheets of the Neotropical Networks initiative (121 monitoring projects, 5,037 sites, 44 countries and territories).

## How to run

From the repository root, in R (≥ 4.3):

```r
Rscript run_all.R            # everything except the sampbias MCMC (uses results/sampbias_results.csv); ~1 min
Rscript run_all.R sampbias   # also re-runs the 16 sampbias chains (~25 min on 8 cores)
```

Figure 4 and the recomputation of `data/site_ecoregions.csv` need the ecoregion maps, which are not redistributed here. Download them and place the shapefiles in `data/shapefiles/` with these names:

- `wwf_terr_ecos.shp` (+ .shx, .dbf): WWF Terrestrial Ecoregions of the World, distributed by WWF (Olson et al. 2001, BioScience 51: 933–938, https://doi.org/10.1641/0006-3568(2001)051[0933:TEOTWA]2.0.CO;2)
- `meow_ecos.shp` (+ .shx, .dbf, .prj): Marine Ecoregions of the World, distributed by WWF/TNC (Spalding et al. 2007, BioScience 57: 573–583, https://doi.org/10.1641/B570707)

Without them, the scripts use the site-to-ecoregion table shipped in `data/site_ecoregions.csv`.

R packages: dplyr, ggplot2, patchwork, sf, rnaturalearth (+ rnaturalearthdata, rnaturalearthhires), ggspatial, terra, sampbias (2.0.0). Figures use the Liberation Sans font (metrically identical to Arial).

## Scripts (`scripts/`, run in this order by `run_all.R`)

| Script | What it does | Main outputs |
|---|---|---|
| `01_fix_site_coordinates.R` | Corrects site coordinates (see below) | `data/intermediate/`, `logs/LOG_coordinates_site_data.csv` |
| `02_clean_spreadsheets.R` | Harmonizes project IDs across tables, fixes shifted cells, removes duplicated rows, fills missing taxonomy | `data/*.csv`, `logs/LOG_cleaning_spreadsheets.csv` |
| `03_ecoregions.R` | Assigns each site to a terrestrial and a marine ecoregion; taxonomic composition per ecoregion | `data/site_ecoregions.csv`, `results/*_ecoregions_composition.csv` |
| `04_density_figure1.R` | Land-based site density per country; GDP partial correlation; Figure 1 | `results/land_site_density_by_country.csv`, `figures/Figure1` |
| `05_sampbias/` | Spatial sampling-bias models (sampbias), land and sea, 4 chains × 10⁶ iterations, with and without one record per project per cell | `results/sampbias_results.csv` |
| `06_figure2_sampbias.R` | Figure 2 | `figures/Figure2` |
| `07_figure3_figureS1.R` | Variable types per taxonomic group (Fig. 3) and abiotic variables (Fig. S1) | `figures/Figure3`, `figures/FigureS1` |
| `08_duration_lifespan_figureS4.R` | Project-level regression of monitoring duration on species lifespan; Figure S4 | `results/duration_lifespan_*.csv`, `figures/FigureS4` |
| `09_figure4_figuresS2_S3.R` | Figure 4 (ecoregion maps and donuts) and Figures S2–S3 | `figures/Figure4`, `figures/FigureS2`, `figures/FigureS3` |

## Data (`data/`)

Tables used in all analyses (outputs of scripts 01–02):

| File | Content |
|---|---|
| `network_data.csv` | One row per monitoring project (Network.ID): name, institution, objectives, funding, reports |
| `site_data.csv` | One row per site: project, coordinates, country, habitat. `linha_planilha` is the row in the original compiled spreadsheet |
| `site_habitat_class.csv` | Terrestrial / freshwater / marine class of each site and the rule used (`regra`) |
| `variable_data.csv` | Variables monitored by each project: biotic/abiotic, taxonomic group, region, metric type, protocol |
| `site_variables_data.csv` | Which variables are measured at each site, sampling frequency and dates |
| `groups_time.csv` | Species recorded in the projects' publications, with taxonomy, maximum lifespan and monitoring duration |
| `authors_data.csv`, `publications_data.csv` | Contributing researchers (no e-mails) and publications of each project |
| `site_ecoregions.csv` | Terrestrial (WWF) and marine (MEOW) ecoregion of each site |
| `country_land_area_gdp.csv` | Land area and GDP (World Bank, 2021) of the 28 countries and territories with land in the study region |
| `research_institutions_ror_lac.csv` | Research institutions from the Research Organization Registry (ROR API v2, accessed 22 Sep 2026) |
| `oceanic_islands_ne10m.rds` | Oceanic islands added to the marine study region of sampbias |

`data/raw/` holds the compiled spreadsheets as received from the contributing projects, from which scripts 01–02 rebuild the tables above. Personal e-mail addresses, links to private working files and the column of lifespan sources were removed before publication. `site_habitat_class_raw.csv` is the site classification produced in an earlier step (rules in the column `regra`).

## Corrections relative to the previous version of this repository

- **Site coordinates**: from row 1997 of the compiled site table to the end, latitude and longitude were offset by one row (each site carried the coordinates of the site above). This was corrected, together with a swapped latitude/longitude (BISC), sign and decimal errors, and country-label errors (Dominica, Tobago). Three sites in Príncipe (outside the study region) were removed, and eight sites whose coordinates are missing or could not be verified are kept without coordinates. Every change is listed in `logs/LOG_coordinates_site_data.csv`.
- **Spreadsheets**: project identifiers were harmonized across tables, cells shifted by one column were fixed, and identical duplicated rows were removed (`logs/LOG_cleaning_spreadsheets.csv`).
- All analyses and figures were re-run with the corrected data.
