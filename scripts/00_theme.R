# Tema comum a todas as figuras do Capítulo 1
# Fonte: Liberation Sans (mesmas medidas da Arial; troca por Arial sem mudar o layout)
suppressMessages({library(ggplot2); library(dplyr); library(patchwork)})
FONTE <- "Liberation Sans"
tema <- function(base = 11) theme_minimal(base_size = base, base_family = FONTE) +
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "grey25"), axis.title = element_text(colour = "grey20"),
        plot.title = element_text(face = "bold", size = base + 2, hjust = 0),
        plot.tag = element_text(face = "bold", size = base + 3, family = FONTE),
        legend.text = element_text(colour = "grey20"), legend.title = element_text(colour = "grey20"))
# rótulo dos painéis: (A), (B) em negrito
tags <- plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")")
salvar <- function(p, nome, w, h) {
  dir.create("figures", showWarnings = FALSE)
  ggsave(file.path("figures", paste0(nome, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path("figures", paste0(nome, ".pdf")), p, width = w, height = h, device = cairo_pdf)   # textos editáveis
}
