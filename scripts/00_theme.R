# Common theme for all figures
# Font: Liberation Sans (metrically identical to Arial, so it can be swapped for Arial without changing the layout)
suppressMessages({library(ggplot2); library(dplyr); library(patchwork)})
FONTE <- "Liberation Sans"
tema <- function(base = 11) theme_minimal(base_size = base, base_family = FONTE) +
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "grey25"), axis.title = element_text(colour = "grey20"),
        plot.title = element_text(face = "bold", size = base + 2, hjust = 0),
        plot.tag = element_text(face = "bold", size = base + 3, family = FONTE),
        legend.text = element_text(colour = "grey20"), legend.title = element_text(colour = "grey20"))
# panel labels: (A), (B) in bold
tags <- plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")")
salvar <- function(p, nome, w, h) {
  dir.create("figures", showWarnings = FALSE)
  ggsave(file.path("figures", paste0(nome, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path("figures", paste0(nome, ".pdf")), p, width = w, height = h, device = cairo_pdf)   # editable text
}
