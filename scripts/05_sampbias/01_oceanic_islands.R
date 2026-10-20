# 05a. Oceanic islands present in Natural Earth 1:10m but missing from the 1:50m map used as the land layer
# (e.g. Trindade, Fernando de Noronha, Cocos, Galapagos, Revillagigedo); added to the sampbias study region.
# Output: data/oceanic_islands_ne10m.rds (already shipped; recreated only if missing)
if (!file.exists("data/oceanic_islands_ne10m.rds")) {
  suppressMessages({library(sf); library(rnaturalearth)}); sf_use_s2(FALSE)
  sub <- c("South America", "Central America", "Caribbean")
  w50 <- ne_countries(scale = 50, returnclass = "sf"); l50 <- st_union(st_make_valid(w50[w50$subregion %in% sub, ]))
  w10 <- ne_countries(scale = 10, returnclass = "sf"); w10 <- st_make_valid(w10[w10$subregion %in% sub, ])
  pol <- st_cast(st_cast(st_geometry(w10), "MULTIPOLYGON"), "POLYGON")
  ilhas <- pol[lengths(st_intersects(pol, st_buffer(l50, 0.5))) == 0]
  saveRDS(st_union(ilhas), "data/oceanic_islands_ne10m.rds")
}
