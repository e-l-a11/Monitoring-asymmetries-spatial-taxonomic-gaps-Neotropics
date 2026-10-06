# 09. Figure 4 (maps + donuts) and Figures S2-S3. Requires the ecoregion shapefiles in data/shapefiles/ (see README).
# Figura 4: mesmo layout da Figura4 anterior (montagem das Figs. 5 e 6 originais), com os dados das planilhas finais.
# Diferenças em relação ao rascunho v4: fundo só com as Américas; % só nas 2 maiores fatias (empates incluídos), como na original.
# Figura 4 (A terrestre, B marinho) + Figs. S2 e S3 (top 20 ecorregiões em donuts), com o site.data v4
# Composição: ecorregioes_v4.R (rodar antes). Mesmo layout da Figura 4 anterior: ecorregiões coloridas
# pelo grupo dominante, top 20 numeradas, 5 donuts de destaque por painel (as mesmas ecorregiões de antes).
source("scripts/00_theme.R")
suppressMessages({library(sf); library(rnaturalearth)}); sf_use_s2(FALSE)
terr <- read.csv("results/terrestrial_ecoregions_composition.csv")
mar  <- read.csv("results/marine_ecoregions_composition.csv")

cores <- c("algae" = "#8DD3C7", "amphibians" = "#D6EFBA", "arthropods" = "#EBEABE", "benthos" = "#EEC2EF",
           "birds" = "#8F2D56", "corals" = "#BC86BD", "crustaceans" = "#E1979C", "fishes" = "#CF1170",
           "fungus" = "#8EABC7", "insects" = "#C2B297", "macroinvertebrates" = "#EFBB63", "mammals" = "#FCDE63",
           "parasites" = "#A3A3A3", "plankton" = "#D5D6A3", "plants" = "#218380", "reptiles" = "#755867",
           "seagrass" = "#EADA96", "sessile organisms" = "#B9D7E8", "zooplankton" = "#DAEF5E")
grupos <- sort(unique(c(terr$grupo, mar$grupo))); stopifnot(all(grupos %in% names(cores))); cores <- cores[grupos]
escala <- scale_fill_manual(values = cores, limits = grupos, name = "Taxonomic groups", drop = FALSE, na.value = "grey90")
quebra <- function(x, w = 22) sapply(x, function(s) paste(strwrap(s, w), collapse = "\n"))

donut <- function(d, base = 3.2) {
  d <- d %>% mutate(grupo = factor(grupo, levels = grupos)) %>% arrange(grupo) %>%
    mutate(rot = ifelse(round(pct) >= sort(round(pct), decreasing = TRUE)[min(2, n())] & pct >= 10, paste0(round(pct), "%"), ""))   # 2 maiores (empates entram), só >= 10%, como na original
  ggplot(d, aes(x = 2, y = n, fill = grupo)) +
    geom_col(colour = "white", linewidth = 0.3, width = 1) +
    geom_text(aes(x = 2.05, label = rot), position = position_stack(vjust = 0.5), size = base, family = FONTE, colour = "grey10") +
    coord_polar(theta = "y", start = 0) + xlim(c(0.9, 2.5)) + escala + theme_void() + theme(legend.position = "none")
}

xl <- c(-122, -2); yl <- c(-60, 42)
w0 <- ne_countries(scale = 50, returnclass = "sf"); w0 <- w0[w0$continent %in% c("North America", "South America"), ]   # só as Américas, como na original
mundo <- st_crop(st_make_valid(w0), c(xmin = xl[1], xmax = xl[2], ymin = yl[1], ymax = yl[2]))

painel <- function(comp, shp, col, destaques) {
  dom <- comp %>% group_by(eco) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% select(eco, dominante = grupo, rank)
  todas <- shp %>% rename(eco = all_of(col)) %>% st_make_valid() %>%
    st_crop(c(xmin = xl[1], xmax = xl[2], ymin = yl[1], ymax = yl[2]))   # todas as ecorregiões (as sem dados ficam em cinza), como na original
  todas <- todas[st_coordinates(suppressWarnings(st_point_on_surface(todas)))[, 1] < -25, ]   # só as Américas (sem África/Europa/Atlântico leste)
  shp <- todas %>% inner_join(dom, by = "eco")
  lab <- shp %>% filter(rank <= 20) %>% group_by(eco, rank) %>% summarise(.groups = "drop") %>%
    mutate(pt = st_point_on_surface(geometry)) %>% st_drop_geometry()
  lab[c("X", "Y")] <- st_coordinates(lab$pt)
  g <- ggplot() + geom_sf(data = mundo, fill = "grey90", colour = NA) +
    geom_sf(data = todas, fill = "grey85", colour = "black", linewidth = 0.1) +
    geom_sf(aes(fill = dominante), data = shp, colour = "black", linewidth = 0.1) +
    geom_text(aes(X, Y, label = rank), data = lab, size = 4.6, family = FONTE) +
    # legenda com todos os grupos (retângulos de área zero, só para a legenda)
    geom_rect(aes(xmin = -60, xmax = -60, ymin = 0, ymax = 0, fill = g), data = data.frame(g = grupos), inherit.aes = FALSE)
  for (k in seq_len(nrow(destaques))) {
    dk <- destaques[k, ]; r <- 10; d <- comp %>% filter(eco == dk$eco); lk <- lab[lab$eco == dk$eco, ]
    ang <- atan2(lk$Y - dk$y, lk$X - dk$x)
    g <- g + annotate("segment", x = dk$x + r * cos(ang), y = dk$y + r * sin(ang), xend = lk$X, yend = lk$Y, linewidth = 0.4) +
      annotation_custom(ggplotGrob(donut(d)), xmin = dk$x - r, xmax = dk$x + r, ymin = dk$y - r, ymax = dk$y + r) +
      annotate("text", x = dk$x, y = dk$y + r + 0.4, label = paste0(d$rank[1], ".\n", quebra(dk$eco)), vjust = 0,
               size = 3.9, family = FONTE, colour = "grey30", lineheight = 0.9)
  }
  g + escala +
    scale_x_continuous(breaks = seq(-120, -40, 20), labels = function(x) paste0(abs(x), "°W")) +
    scale_y_continuous(breaks = seq(-60, 40, 20), labels = function(y) ifelse(y == 0, "0°", paste0(abs(y), ifelse(y < 0, "°S", "°N")))) +
    coord_sf(xlim = xl, ylim = yl, expand = FALSE, clip = "off") + labs(x = "Longitude", y = "Latitude") +
    tema(14) + theme(panel.grid = element_blank(), legend.key.size = unit(0.8, "cm"), legend.text = element_text(size = 14),
                     legend.title = element_text(size = 16))
}

# posições dos donuts (centro, em graus) — as mesmas da Figura 4 anterior
dT <- data.frame(eco = c("Caribbean shrublands", "Tocantins/Pindare moist forests", "Purus varzeá", "Bahia interior forests", "Araucaria moist forests"),
                 x = c(-60, -36, -99, -17, -30), y = c(30, 4, -11, -22, -49))
dM <- data.frame(eco = c("Eastern Caribbean", "Nicoya", "Guianan", "Rio Grande", "Araucanian"),
                 x = c(-39, -104, -12, -23, -100), y = c(22, -10, -9, -45, -48))
sh <- list(teow = st_make_valid(st_set_crs(read_sf("data/shapefiles/wwf_terr_ecos.shp")[, "ECO_NAME"], 4326)),
           meow = st_make_valid(st_set_crs(read_sf("data/shapefiles/meow_ecos.shp")[, c("ECOREGION", "REALM")], 4326)))
pA <- painel(terr, sh$teow, "ECO_NAME", dT)
pB <- painel(mar, sh$meow, "ECOREGION", dM)
salvar((pA / (pB + theme(legend.position = "none"))) + plot_layout(guides = "collect") + tags, "Figure4", 15, 19.3)


# S2 / S3: top 20 em donuts
grade <- function(comp, nome) {
  x <- comp %>% filter(rank <= 20) %>% mutate(tit = factor(paste0(rank, ".\n", quebra(eco, 24)))) %>%
    mutate(tit = reorder(tit, rank), grupo = factor(grupo, levels = grupos)) %>% group_by(tit) %>% arrange(grupo, .by_group = TRUE) %>%
    mutate(rot = ifelse(pct >= 10, paste0(round(pct), "%"), "")) %>% ungroup()
  p <- ggplot(x, aes(x = 2, y = n, fill = grupo)) +
    geom_col(position = "fill", colour = "white", linewidth = 0.3, width = 1) +
    geom_text(aes(x = 2.05, label = rot), position = position_fill(vjust = 0.5), size = 3, family = FONTE, colour = "grey10") +
    coord_polar(theta = "y") + xlim(c(0.9, 2.5)) + facet_wrap(~ tit, ncol = 5) +
    scale_fill_manual(values = cores, limits = intersect(grupos, unique(as.character(x$grupo))), name = "Taxonomic groups") +
    theme_void(base_family = FONTE) +
    theme(strip.text = element_text(size = 10, colour = "grey20", margin = margin(b = 2, t = 6)),
          legend.title = element_text(size = 12), legend.text = element_text(size = 11))
  salvar(p, nome, 11, 9.5)
}
grade(terr, "FigureS2"); grade(mar, "FigureS3")
cat("ok\n")
