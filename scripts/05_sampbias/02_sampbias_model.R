# 05b. sampbias model (one chain). Run from the repository root via scripts/05_sampbias/run_all.sh (16 chains; ~25 min on 8 cores)
# sampbias v6 (= v5 com o site.data v4 corrigido, 2026-10-05):
#   - usa a coluna linha_planilha do site_data_v4 para casar com classificacao_3classes.csv (Príncipe foi removido)
#   - filtro de longitude -30 -> -28 (Trindade/Martim Vaz ficavam de fora)
#   - universo: acrescenta as ilhas oceanicas do Natural Earth 1:10m que nao existem no 1:50m (data/oceanic_islands_ne10m.rds)
# sampbias v5: modelos SEPARADOS para sites terrestres e marinhos, agora com instituicoes de pesquisa
# instituicoes = universidades + institutos/centros de pesquisa + orgaos governamentais ambientais/cientificos
#                (ROR, filtrado por palavra-chave de dominio ambiental/biologico; ver ror_instituicoes_lac_filtrado.csv)
# uso: Rscript sampbias_analise_v5_com_instituicoes.R <site.data.csv> <classificacao.csv> <instituicoes.csv> <terra|mar> <A|B> <res_graus> <iteracoes> <seed> [rotulo]
suppressMessages({library(sampbias); library(dplyr); library(terra); library(sf); library(rnaturalearth)})
sf_use_s2(FALSE)

args    <- commandArgs(trailingOnly = TRUE)
f       <- args[1]; fclass <- args[2]; finst <- args[3]; realm <- args[4]; variant <- args[5]
res_deg <- as.numeric(args[6]); iters <- as.numeric(args[7]); seed <- as.numeric(args[8])
label   <- if (length(args) >= 9) paste0("_", args[9]) else ""   # rotulo opcional para nao sobrescrever resultados

get <- function(type, cat) st_make_valid(ne_download(scale = 10, type = type, category = cat, returnclass = "sf")[, 1])
O <- "results/sampbias_runs/"; dir.create(O, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(paste0(O, "gaz5.rds"))) saveRDS(list(roads = get("roads", "cultural"), cities = get("urban_areas", "cultural"),
    waterbodies = get("rivers_lake_centerlines", "physical"), airports = get("airports", "cultural"), ports = get("ports", "cultural")), paste0(O, "gaz5.rds"))
if (!file.exists(paste0(O, "gaz_coast.rds"))) saveRDS(get("coastline", "physical"), paste0(O, "gaz_coast.rds"))
g5 <- readRDS(paste0(O, "gaz5.rds"))

inst_df <- read.csv(finst, fileEncoding = "UTF-8-BOM")
institutions <- st_as_sf(inst_df, coords = c("lon", "lat"), crs = 4326)
cat("instituicoes carregadas:", nrow(institutions), "\n")

gaz <- if (realm == "terra") c(g5[c("roads", "cities", "waterbodies", "airports")], list(institutions = institutions)) else
       list(ports = g5$ports, airports = g5$airports, cities = g5$cities, coast = readRDS(paste0(O, "gaz_coast.rds")), institutions = institutions)
gaz <- lapply(gaz, terra::vect)

d  <- read.csv(f, fileEncoding = "UTF-8-BOM"); d$row <- if ("linha_planilha" %in% names(d)) d$linha_planilha else seq_len(nrow(d)) + 1
cl <- read.csv(fclass, fileEncoding = "UTF-8-BOM")
d  <- merge(d, cl[, c("linha_planilha", "classe")], by.x = "row", by.y = "linha_planilha")
d  <- d[d$classe == ifelse(realm == "terra", "terrestrial", "marine"), ]
d$decimalLongitude <- as.numeric(d$Site.Longitude); d$decimalLatitude <- as.numeric(d$Site.Latitude)
bad <- is.na(d$decimalLongitude) | is.na(d$decimalLatitude) | d$decimalLongitude < -120 | d$decimalLongitude > -28 |
       d$decimalLatitude < -56 | d$decimalLatitude > 33
cat("realm:", realm, "| sites da classe:", nrow(d), "| excluidos por coordenada invalida:", sum(bad), "\n")
d <- d[!bad, ]; d$species <- d$Network.ID
if (variant == "B") d <- d %>% mutate(cx = floor(decimalLongitude / res_deg), cy = floor(decimalLatitude / res_deg)) %>%
                          distinct(Network.ID, cx, cy, .keep_all = TRUE)

w        <- ne_countries(scale = 50, returnclass = "sf")
lac_land <- st_union(st_union(st_make_valid(w[w$subregion %in% c("South America", "Central America", "Caribbean"), ])), readRDS("data/oceanic_islands_ne10m.rds"))
simp     <- st_union(st_simplify(lac_land, dTolerance = 0.02), readRDS("data/oceanic_islands_ne10m.rds"))   # v6: o simplify apaga ilhas pequenas
uni <- if (realm == "terra") st_sf(geometry = lac_land) else
       st_sf(geometry = st_make_valid(st_difference(st_buffer(simp, dist = 3.3), st_buffer(simp, dist = -0.3))))
pts <- st_as_sf(d, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = FALSE)
cat("sites no universo (geometria):", sum(lengths(st_intersects(pts, uni)) > 0), "de", nrow(d), "\n")

set.seed(seed)
b <- calculate_bias(d[, c("species", "decimalLongitude", "decimalLatitude")], gaz = gaz, res = res_deg, terrestrial = FALSE,
                    restrict_sample = uni, mcmc_iterations = iters, mcmc_burnin = 0.2 * iters)

ess <- function(x) { n <- length(x); a <- acf(x, plot = FALSE, lag.max = min(500, n - 1))$acf[-1]
  k <- which(a < 0.05)[1]; if (is.na(k)) k <- length(a); n / (1 + 2 * sum(a[seq_len(k - 1)])) }
o <- b$occurrences; v <- values(o, na.rm = FALSE)[, 1]; e <- b$bias_estimate
facs <- sub("^w_", "", grep("^w_", names(e), value = TRUE))
out <- do.call(rbind, lapply(facs, function(fc) {
  wv <- e[[paste0("w_", fc)]]; h <- log(2) / wv
  data.frame(realm = realm, variant = variant, res_deg = res_deg, iterations = iters, seed = seed, sites_in_class = nrow(d),
             records_counted = sum(v, na.rm = TRUE), cells_with_records = sum(v > 0, na.rm = TRUE), cells_total = sum(!is.na(v)),
             factor = fc, half_distance_km = round(median(h)), half_lo = round(quantile(h, .025)), half_hi = round(quantile(h, .975)),
             rate_at_100km = round(median(exp(-wv * 100)), 3), ESS = round(ess(wv)), row.names = NULL) }))
print(out, row.names = FALSE)
tag <- sprintf("v6_%s_%s_%sdeg_it%g_seed%g%s", realm, variant, res_deg, iters, seed, label)
write.csv(out, paste0(O, "sampbias_", tag, ".csv"), row.names = FALSE)
saveRDS(b, paste0(O, "sampbias_", tag, ".rds"))
