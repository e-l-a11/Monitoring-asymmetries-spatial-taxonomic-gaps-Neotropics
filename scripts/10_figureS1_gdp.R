# 10. Figure S1: land-based site density vs GDP per capita, both controlling for land area (partial residuals)
# Mesma correlação parcial do script 04 (países com PIB; Cuba não tem): resíduos de log(densidade + 0.1) ~ log(área)
# contra resíduos de log(PIB per capita) ~ log(área). Lê results/land_site_density_by_country.csv (gerado pelo 04).
source("scripts/00_theme.R")
suppressMessages(library(ggrepel))
d0 <- read.csv("data/country_land_area_gdp.csv")
d  <- read.csv("results/land_site_density_by_country.csv")
g <- d %>% left_join(d0[, c("iso3", "gdp_usd_2021", "gdp_per_capita_usd_2021")], by = "iso3") %>%
  filter(!is.na(gdp_usd_2021), !is.na(gdp_per_capita_usd_2021))
g$r_dens <- resid(lm(log(sites_per_100k_km2 + 0.1) ~ log(land_area_km2), g))
g$r_gdp  <- resid(lm(log(gdp_per_capita_usd_2021) ~ log(land_area_km2), g))
t <- cor.test(g$r_dens, g$r_gdp)
cat(sprintf("10 | GDP per capita: partial r = %.2f, P = %.2f, n = %d\n", t$estimate, t$p.value, nrow(g)))

g$br <- g$iso3 == "BRA"
fig <- ggplot(g, aes(r_gdp, r_dens)) +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "grey45", fill = "grey85", linewidth = 0.6, linetype = "dashed") +
  geom_point(aes(colour = br, shape = sites_on_land == 0), size = 2.6) +
  geom_text_repel(aes(label = country, colour = br), size = 3, family = FONTE, seed = 1,
                  min.segment.length = 0.3, segment.colour = "grey70", max.overlaps = Inf) +
  scale_colour_manual(values = c(`FALSE` = "grey40", `TRUE` = "#C0603F"), guide = "none") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = c("≥ 1 site on land", "no site on land"), name = NULL) +
  annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3, family = FONTE, size = 3.6, colour = "grey25",
           label = sprintf("partial r = %.2f, P = %.2f, n = %d countries", t$estimate, t$p.value, nrow(g))) +
  labs(x = "GDP per capita, controlling for land area (residuals of log values)",
       y = "Site density, controlling for land area\n(residuals of log values)") +
  tema() + theme(legend.position = "bottom")
salvar(fig, "FigureS1", 7.5, 6)
