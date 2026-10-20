# 07. Figure S2 (heat map of variable types per taxonomic group; A terrestrial + freshwater, B marine) and Figure S3 (abiotic variables)
# Source: variable_data; biotic variables with a defined type (metric "na" excluded); each variable of each project counts once.
# Colour = % of the group's variables in each type (land orange, sea navy, as in Fig. 1); groups with < 3 variables are not shown.
source("scripts/00_theme.R")
v <- read.csv("data/variable_data.csv")
b <- v %>% filter(tolower(Variable.Type) == "biotic", Metrics != "na") %>%
  transmute(ambiente = ifelse(tolower(Region) == "marine", "(B) Marine", "(A) Terrestrial and freshwater"),
            grupo = Groups.variables, tipo = tolower(trimws(Metrics)))
cap <- function(x) sub("^Dna", "DNA", paste0(toupper(substring(x, 1, 1)), substring(x, 2)))
x <- b %>% count(ambiente, grupo, tipo) %>% group_by(ambiente, grupo) %>% mutate(N = sum(n), p = n / N) %>% ungroup()
write.csv(x, "results/variable_types_by_group.csv", row.names = FALSE)
x <- x %>% filter(N >= 3)
ord_t <- names(sort(table(b$tipo), decreasing = TRUE))
x <- x %>% mutate(tipo = factor(cap(tipo), levels = cap(ord_t)),
                  rot = paste0(cap(grupo), " (", N, ")"))
ordem <- x %>% distinct(ambiente, rot, N) %>% arrange(ambiente, N)
x$rot <- factor(x$rot, levels = unique(ordem$rot))
# grade completa (células vazias em branco com borda)
grade <- x %>% distinct(ambiente, rot) %>% tidyr::crossing(tipo = factor(cap(ord_t), levels = cap(ord_t)))
cat("07 | grupos na figura:", n_distinct(x$rot), "| tipos:", length(ord_t), "\n")
for (a in unique(x$ambiente)) { y <- x[x$ambiente == a, ]
  ab <- y %>% filter(tipo == "Abundance") %>% arrange(-p)
  cat(sprintf("07 | %s: abundância é o tipo mais comum em %d de %d grupos\n", a,
      sum(y %>% group_by(rot) %>% slice_max(p, n = 1, with_ties = FALSE) %>% pull(tipo) == "Abundance"), n_distinct(y$rot))) }

painel <- function(a, low, high) {
  y <- x[x$ambiente == a, ]; g <- grade[grade$ambiente == a, ]
  ggplot(y, aes(tipo, rot)) +
    geom_tile(data = g, fill = "white", colour = "grey88", linewidth = 0.3) +
    geom_tile(aes(fill = p), colour = "white", linewidth = 0.6) +
    geom_text(aes(label = ifelse(p < 0.005, "<1", round(100 * p)), colour = p > 0.5), size = 2.8, family = FONTE, show.legend = FALSE) +
    scale_fill_gradient(low = low, high = high, limits = c(0, 1), labels = scales::percent, name = "Share of the\ngroup's variables") +
    scale_colour_manual(values = c(`FALSE` = "grey20", `TRUE` = "white")) +
    labs(title = a, x = NULL, y = NULL) +
    tema(10) + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 40, hjust = 1),
                     plot.title.position = "plot", plot.title = element_text(size = 11), legend.position = "right")
}
fig <- (painel("(A) Terrestrial and freshwater", "#FDE8D0", "#B4500A") / painel("(B) Marine", "#DCE3F5", "#16236B")) +
  plot_layout(heights = c(13, 8))
salvar(fig, "FigureS2", 8.5, 9)

v0 <- read.csv("data/variable_data.csv", check.names = FALSE, strip.white = TRUE); names(v0) <- trimws(names(v0))
v0 <- v0[tolower(v0$Variable.Type) == "biotic" & v0$Metrics != "na", ]
for (r in c("marine", "terrestrial")) { x0 <- v0[tolower(v0$Region) == r, ]
  cat(r, ": grupos", length(unique(x0$`Groups-variables`)), "| tipos", length(unique(x0$Metrics)), "| máx. tipos por grupo",
      max(tapply(x0$Metrics, x0$`Groups-variables`, function(z) length(unique(z)))), "\n") }
for (m in c("dna analysis", "phenology", "productivity")) cat("07 | projetos que medem", m, ":", length(unique(v0$Network.ID[tolower(trimws(v0$Metrics)) == m])), "\n")

# Figure S3: number of projects measuring each type of abiotic variable (terrestrial and marine projects combined)
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
salvar(p, "FigureS3", 7, 3.6)
