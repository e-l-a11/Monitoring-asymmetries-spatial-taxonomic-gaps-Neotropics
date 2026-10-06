# 08. Figure S5 + números do texto: duração do projeto x longevidade das espécies (nível de projeto)
# Método (Métodos do texto): para cada projeto com longevidade de pelo menos uma espécie,
# log10(mediana da duração) ~ log10(mediana da longevidade máxima das espécies), OLS.
# Robustez: bootstrap de projetos (5.000), Spearman, sem o projeto com mais espécies (ATDN).
# Mesmo método do script original: só Plantae e Animalia,
# bootstrap de 5.000 réplicas (seed 42), robustez sem o projeto com mais espécies (ATDN). Com a groups_time antiga reproduz
# exatamente o texto anterior (slope 0.075, n = 59).
source("scripts/00_theme.R")
g <- read.csv("data/groups_time.csv", check.names = FALSE)
g$dur <- suppressWarnings(as.numeric(g$`monitoring time (years)`)); g$ls <- suppressWarnings(as.numeric(g$lifespan.max))
p <- g %>% filter(!is.na(ls), ls > 0, !is.na(dur), kingdom %in% c("Plantae", "Animalia")) %>% group_by(Network.ID) %>%
  summarise(dur = median(dur), ls = median(ls), n_sp = n_distinct(species), .groups = "drop")
m <- lm(log10(dur) ~ log10(ls), p); s <- summary(m)$coefficients; ci <- confint(m)[2, ]
set.seed(42); b <- replicate(5000, { i <- sample(nrow(p), replace = TRUE); d <- p[i, ]; if (length(unique(d$ls)) < 2) return(NA); coef(lm(log10(dur) ~ log10(ls), d))[2] })
pesada <- p$Network.ID[which.max(p$n_sp)]; stopifnot(pesada == "ATDN"); sem <- coef(lm(log10(dur) ~ log10(ls), p[p$Network.ID != pesada, ]))[2]
res <- data.frame(n_projetos = nrow(p), slope = s[2, 1], SE = s[2, 2], P = s[2, 4], R2 = summary(m)$r.squared,
  IC95_lo = ci[1], IC95_hi = ci[2], boot_lo = quantile(b, .025, na.rm = TRUE), boot_hi = quantile(b, .975, na.rm = TRUE),
  spearman = cor(log10(p$ls), log10(p$dur), method = "spearman"), slope_sem_ATDN = sem, duracao_mediana = median(p$dur),
  lifespan_min = min(p$ls), lifespan_max = max(p$ls), registros = nrow(g), especies_unicas = n_distinct(g$species))
dir.create("results", showWarnings = FALSE)
write.csv(p, "results/duration_lifespan_by_project.csv", row.names = FALSE)
write.csv(res, "results/duration_lifespan_regression.csv", row.names = FALSE); print(t(round(res, 3)))

nd <- data.frame(ls = 10^seq(log10(min(p$ls)), log10(max(p$ls)), length.out = 200))
pr <- predict(m, nd, interval = "confidence"); nd[c("fit", "lo", "hi")] <- 10^pr
fig <- ggplot(p, aes(ls, dur)) +
  geom_ribbon(data = nd, aes(ls, ymin = lo, ymax = hi), inherit.aes = FALSE, fill = "grey80", alpha = 0.7) +
  geom_line(data = nd, aes(ls, fit), colour = "grey25", linewidth = 0.7) +
  geom_point(aes(size = n_sp), colour = "#8E2C55", alpha = 0.65) +
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000), labels = c("0.01", "0.1", "1", "10", "100", "1,000")) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50)) + scale_size_area(max_size = 6, name = "Species with\nlifespan data") +
  annotate("text", x = min(p$ls), y = max(p$dur) * 1.1, hjust = 0, vjust = 1, family = FONTE, size = 3.6, colour = "grey25",
           label = sprintf("slope = %.2f (95%% CI %.2f to %.2f)\nP = %.2f, R² = %.2f, n = %d projects", s[2, 1], ci[1], ci[2], s[2, 4], summary(m)$r.squared, nrow(p))) +
  labs(x = "Median maximum lifespan of monitored species (years, log scale)", y = "Median project duration (years, log scale)") + tema()
salvar(fig, "FigureS5", 7.5, 5)
