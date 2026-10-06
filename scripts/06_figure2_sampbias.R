# 06. Figure 2: taxa de amostragem relativa x distancia (sampbias), (A) terra, (B) mar
# Roda local, so le sampbias_v6_resultados.csv (analise principal = variante A).
# Curva de cada fator: taxa = exp(-w * d), com w = log(2) / meia-distancia.
# Como cada fator tem um so parametro, a faixa do IC 95% sai direto de half_lo e half_hi.
suppressMessages({library(ggplot2); library(dplyr)})
r <- read.csv("results/sampbias_results.csv")

# Fatores que entram: convergiram e tem efeito. Fora: cidades (sem efeito), estradas (colineares
# com instituicoes, instaveis no teste B), portos e instituicoes no mar (nao convergiram).
keep <- data.frame(realm  = c("terra", "terra", "terra", "mar", "mar"),
                   factor = c("institutions", "waterbodies", "airports", "coast", "airports"),
                   nome   = c("Research institutions", "Rivers and lakes", "Airports", "Coastline", "Airports"))
r <- r %>% filter(variant == "A") %>% inner_join(keep, by = c("realm", "factor")) %>%
  mutate(painel = ifelse(realm == "terra", "(A) Land", "(B) Sea"))

cores <- c("Research institutions" = "#C0603F", "Rivers and lakes" = "#3A7CA5",
           "Coastline" = "#2A9D8F", "Airports" = "grey50")

d <- 10^seq(0, log10(3000), length.out = 300)
curvas <- r %>% group_by(painel, nome) %>%
  reframe(d = d, taxa = exp(-log(2) / half_distance_km * d),
          lo = exp(-log(2) / half_lo * d), hi = exp(-log(2) / half_hi * d))
pontos <- r %>% mutate(rot = paste0(nome, "\n", format(half_distance_km, big.mark = ","), " km"),
  # posicao do rotulo: rios a esquerda (acima), instituicoes a direita (abaixo), demais a direita (acima)
  esq = nome == "Rivers and lakes", baixo = nome == "Research institutions",
  lx = ifelse(esq, half_distance_km / 1.25, half_distance_km * ifelse(baixo, 1.3, 1.12)), ly = ifelse(baixo, 0.47, 0.53),
  hj = ifelse(esq, 1, 0), vj = ifelse(baixo, 1, 0))

p <- ggplot(curvas, aes(d, taxa, colour = nome, fill = nome)) +
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "grey55", linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(data = pontos, aes(half_distance_km, 0.5), size = 2.4) +
  geom_text(data = pontos, aes(lx, ly, label = rot, hjust = hj, vjust = vj),
            size = 3.3, lineheight = 0.9) +
  facet_wrap(~painel, ncol = 2) +
  scale_colour_manual(values = cores, guide = "none") +
  scale_fill_manual(values = cores, guide = "none") +
  scale_x_log10(breaks = c(1, 10, 100, 1000), labels = c("1", "10", "100", "1,000"),
                expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(limits = c(0, 1.02), breaks = seq(0, 1, 0.25), expand = expansion(mult = 0)) +
  labs(x = "Distance to feature (km, log scale)", y = "Relative sampling rate") +
  theme_minimal(base_size = 13, base_family = "Liberation Sans") +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(hjust = 0, face = "bold", size = 13),
        axis.text = element_text(colour = "grey25"), axis.title = element_text(colour = "grey25"),
        panel.spacing = unit(1.5, "lines"))

dir.create("figures", showWarnings = FALSE)
ggsave("figures/Figure2.png", p, width = 9, height = 4.2, dpi = 300, bg = "white")
ggsave("figures/Figure2.pdf", p, width = 9, height = 4.2, device = cairo_pdf)  # textos ficam como texto (Canva)
print(pontos[, c("painel", "nome", "half_distance_km", "half_lo", "half_hi")], row.names = FALSE)
