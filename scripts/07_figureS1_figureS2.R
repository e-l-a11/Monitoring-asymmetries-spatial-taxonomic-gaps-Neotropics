# 07. Figure S1 (variable types per taxonomic group; A terrestrial, B marine) and Figure S2 (abiotic variables)
# Figura S1 (era Figura 3): proporção de cada tipo de variável biótica por grupo taxonômico, (A) marinho e (B) terrestre
# Fonte: variable.data; variáveis bióticas; exclui métrica "na" (parasitas ficam de fora: todas "na")
source("scripts/00_theme.R")
v <- read.csv("data/variable_data.csv", check.names = FALSE, strip.white = TRUE)
names(v) <- trimws(names(v)); v <- v[, names(v) != ""]
v <- v %>% filter(tolower(Variable.Type) == "biotic", Metrics != "na") %>%
  transmute(regiao = tolower(Region), grupo = `Groups-variables`, metrica = Metrics)

cores <- c("abundance" = "#8DD3C7", "biomass" = "#D9F0B3", "biometric variables" = "#E5989B", "cover" = "#F0BB62",
           "dna analysis" = "#6B5468", "functional traits" = "#3C8A9A", "occurrence" = "#A6A6A6", "phenology" = "#2CA25F",
           "productivity" = "#1B7837", "richness" = "#8E2C55", "anthropogenic impact" = "#C9B394")

painel <- function(reg, titulo) {
  x <- v %>% filter(regiao == reg) %>% count(grupo, metrica) %>% group_by(grupo) %>% mutate(p = n / sum(n), N = sum(n)) %>% ungroup()
  ord <- x %>% distinct(grupo, N) %>% arrange(N)
  x$grupo <- factor(x$grupo, levels = ord$grupo)
  ggplot(x, aes(p, grupo, fill = metrica)) +
    geom_col(width = 0.75, colour = "white", linewidth = 0.2, show.legend = TRUE) +
    geom_text(data = ord, aes(x = 1.02, y = grupo, label = paste0("n = ", N)), inherit.aes = FALSE,
              hjust = 0, size = 3, colour = "grey25", family = FONTE) +
    scale_x_continuous(labels = scales::percent, breaks = c(0, .25, .5, .75, 1), expand = expansion(mult = c(0, 0.13))) +
    scale_fill_manual(values = cores, name = "Variable type", drop = FALSE, limits = names(cores)) +
    labs(title = titulo, x = "Proportion of variables", y = NULL) +
    tema() + theme(panel.grid.major.y = element_blank(), plot.title.position = "plot")
}
p <- (painel("terrestrial", "(A) Terrestrial") + theme(legend.position = "none") | painel("marine", "(B) Marine"))   # v6: terrestre (A), marinho (B), como nas outras figuras
salvar(p, "FigureS1", 12, 5)
for (r in c("marine", "terrestrial")) { x <- v %>% filter(regiao == r)
  cat(r, ": grupos", n_distinct(x$grupo), "| tipos", n_distinct(x$metrica), "| máx. tipos por grupo",
      max(tapply(x$metrica, x$grupo, n_distinct)), "\n") }

# Figura S2 (era S1): número de projetos que medem cada tipo de variável abiótica (terrestre e marinho juntos)
v <- read.csv("data/variable_data.csv", check.names = FALSE, strip.white = TRUE)
names(v) <- trimws(names(v)); v <- v[, names(v) != ""]
x <- v %>% filter(tolower(Variable.Type) == "abiotic", !Metrics %in% c("na", "")) %>%
  distinct(Network.ID, Metrics) %>% count(Metrics) %>% arrange(n)
print(x)
p <- ggplot(x, aes(n, factor(Metrics, levels = Metrics))) +
  geom_col(fill = "#8E2C55", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.3, size = 3.4, colour = "grey25", family = FONTE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Number of monitoring projects", y = NULL) +
  tema() + theme(panel.grid.major.y = element_blank())
salvar(p, "FigureS2", 7, 3.6)
