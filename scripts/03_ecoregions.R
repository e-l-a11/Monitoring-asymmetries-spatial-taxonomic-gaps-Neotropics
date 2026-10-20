# 03. Ecoregion of each site and taxonomic composition of the ecoregions (Fig. 3, Figs. S4-S5, Table S4)
# 1) Each site with coordinates is joined to the WWF terrestrial ecoregions (ECO_NAME) and to the Marine Ecoregions of the World
#    (ECOREGION) -> data/site_ecoregions.csv (recomputed only if the shapefiles are in data/shapefiles/).
# 2) Composition: biotic variables (variable_data) x sites of each project, counted per ecoregion and taxonomic group
#    (biotic variable-by-site records). Each site counts only in ecoregions of its own realm: terrestrial and freshwater
#    sites in terrestrial ecoregions, marine sites in marine ecoregions (data/site_habitat_class.csv).
#    Terrestrial: without corals, plankton, seagrass, benthos, zooplankton. Marine: all groups.
#    (o filtro str_detect(REALM, "mar") do script original não pegava nenhuma linha; aqui o marinho é a coluna ECOREGION)
suppressMessages({library(sf); library(dplyr)}); sf_use_s2(FALSE)
s <- read.csv("data/site_data.csv", check.names = FALSE)
v <- read.csv("data/variable_data.csv")
# Ecoregion maps (not redistributed here): WWF Terrestrial Ecoregions (Olson et al. 2001) and Marine Ecoregions of the World
# (Spalding et al. 2007). Put wwf_terr_ecos.shp and meow_ecos.shp in data/shapefiles/ to recompute data/site_ecoregions.csv;
# otherwise the file shipped in data/ is used.
SH <- "data/shapefiles/"
if (file.exists(paste0(SH, "wwf_terr_ecos.shp")) && file.exists(paste0(SH, "meow_ecos.shp"))) {
  teow <- st_make_valid(st_set_crs(read_sf(paste0(SH, "wwf_terr_ecos.shp"))[, "ECO_NAME"], 4326))
  meow <- st_make_valid(st_set_crs(read_sf(paste0(SH, "meow_ecos.shp"))[, c("ECOREGION", "REALM")], 4326))
  ok <- !is.na(s$Site.Latitude) & !is.na(s$Site.Longitude)
  p <- st_as_sf(s[ok, c("linha_planilha", "Network.ID", "Site.Name", "Site.Country", "Site.Latitude", "Site.Longitude")],
                coords = c("Site.Longitude", "Site.Latitude"), crs = 4326, remove = FALSE)
  um <- function(j) j %>% st_drop_geometry() %>% group_by(linha_planilha) %>% slice(1) %>% ungroup()
  eco <- um(st_join(p, teow)) %>% left_join(um(st_join(p, meow))[, c("linha_planilha", "ECOREGION", "REALM")], by = "linha_planilha") %>%
    rename(MEOW_REALM = REALM)
  write.csv(eco, "data/site_ecoregions.csv", row.names = FALSE)
} else eco <- read.csv("data/site_ecoregions.csv", check.names = FALSE)
stopifnot(all(eco$Network.ID %in% s$Network.ID))
cat("03 | site_ecoregions:", nrow(eco), "sites | terrestrial ecoregion:", sum(!is.na(eco$ECO_NAME)), "| marine ecoregion:", sum(!is.na(eco$ECOREGION)), "\n")

# CORREÇÃO (19/10/2026): o shapefile MEOW cobre uma faixa de terra e a costa cai em ecorregiões terrestres (WWF); sem filtro,
# 2.101 sítios de terra/água doce entravam em ecorregiões marinhas e 561 sítios marinhos em terrestres. Agora cada sítio só
# entra nas ecorregiões do seu ambiente (classe de data/site_habitat_class.csv): terra + água doce nas WWF, marinhos nas MEOW.
cl <- read.csv("data/site_habitat_class.csv")
classe <- cl$classe[match(eco$linha_planilha, cl$linha_planilha)]; stopifnot(!any(is.na(classe)))
eco_comp <- eco; eco_comp$ECO_NAME[classe == "marine"] <- NA; eco_comp$ECOREGION[classe != "marine"] <- NA
cat("03 | composição: sítios em ecorregião terrestre:", sum(!is.na(eco_comp$ECO_NAME)), "| em ecorregião marinha:", sum(!is.na(eco_comp$ECOREGION)), "\n")

grupos_para_remover <- c("corals", "plankton", "seagrass", "benthos", "zooplaktons")
comp <- function(eco, col, remover) {
  v %>% filter(Variable.Type == "biotic", !Groups.variables %in% remover) %>%
    inner_join(eco[!is.na(eco[[col]]), c("Network.ID", col)], by = "Network.ID", relationship = "many-to-many") %>%
    count(eco = .data[[col]], grupo = Groups.variables) %>% group_by(eco) %>%
    mutate(total = sum(n), pct = round(100 * n / total, 1)) %>% ungroup() %>%
    mutate(rank = match(eco, unique(eco[order(-total, eco)]))) %>% arrange(rank, -n)   # empate: ordem alfabética, como no slice_head do original
}
terr <- comp(eco_comp, "ECO_NAME", grupos_para_remover)
mar  <- comp(eco_comp, "ECOREGION", c())
dir.create("results", showWarnings = FALSE)
write.csv(terr, "results/terrestrial_ecoregions_composition.csv", row.names = FALSE)
write.csv(mar, "results/marine_ecoregions_composition.csv", row.names = FALSE)
