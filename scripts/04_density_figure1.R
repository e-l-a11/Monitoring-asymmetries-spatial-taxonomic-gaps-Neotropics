# 04. Figure 1: (A) map of sites and (B) land-based site density per country (+ GDP per capita partial correlation)
# Densidade: sites cuja coordenada cai em terra (Natural Earth 1:50m) nos 28 países/territórios,
# área terrestre do Banco Mundial (AG.LND.TOTL.K2, 2021) — mesmo método de densidade_por_pais.csv
source("scripts/00_theme.R")
suppressMessages({library(sf); library(rnaturalearth); library(ggspatial)}); sf_use_s2(FALSE)
s <- read.csv("data/site_data.csv", check.names = FALSE); s <- s[!is.na(s$Site.Latitude) & !is.na(s$Site.Longitude), ]
s$lon <- as.numeric(s$Site.Longitude); s$lat <- as.numeric(s$Site.Latitude)

# ---- densidade por país (recalculada com o site.data v4)
d0 <- read.csv("data/country_land_area_gdp.csv")
w50 <- ne_countries(scale = 50, returnclass = "sf")
w50$iso <- ifelse(w50$adm0_a3 %in% d0$iso3, w50$adm0_a3, w50$iso_a3_eh)
w50 <- w50[w50$iso %in% d0$iso3, "iso"]
j <- st_join(st_as_sf(s, coords = c("lon", "lat"), crs = 4326), w50)
n <- table(j$iso)
d <- d0[, c("country", "iso3", "land_area_km2")]
d$sites_on_land <- as.integer(n[d$iso3]); d$sites_on_land[is.na(d$sites_on_land)] <- 0L
pp <- unique(data.frame(Network.ID = j$Network.ID, iso = j$iso)[!is.na(j$iso), ])
d$projects <- as.integer(table(pp$iso)[d$iso3]); d$projects[is.na(d$projects)] <- 0L
d$sites_per_100k_km2 <- round(1e5 * d$sites_on_land / d$land_area_km2, 2)
dir.create("results", showWarnings = FALSE)
write.csv(d, "results/land_site_density_by_country.csv", row.names = FALSE)
x <- d[d$sites_on_land > 0, ]; med <- median(x$sites_per_100k_km2); br <- d[d$iso3 == "BRA", ]
cat(sprintf("terra: %d sites | Brasil %d (%.1f%%) | pares projeto-país %d, Brasil %d (%.1f%%) | densidade BR %.1f | mediana %.1f | rank BR %d de %d\n",
    sum(d$sites_on_land), br$sites_on_land, 100 * br$sites_on_land / sum(d$sites_on_land), sum(d$projects), br$projects,
    100 * br$projects / sum(d$projects), br$sites_per_100k_km2, med, rank(-x$sites_per_100k_km2)[x$iso3 == "BRA"], nrow(x)))

# ---- (A) mapa
mundo <- ne_countries(scale = 50, returnclass = "sf")
pA <- ggplot() +
  geom_sf(data = mundo, fill = "grey90", colour = "white", linewidth = 0.2) +
  geom_point(data = s, aes(lon, lat), colour = "#D49C86", alpha = 0.6, size = 1.4) +
  annotation_north_arrow(location = "br", style = north_arrow_fancy_orienteering(text_family = FONTE),
                         height = unit(0.9, "cm"), width = unit(0.9, "cm")) +
  scale_x_continuous(breaks = seq(-120, -40, 20), labels = function(x) paste0(abs(x), "°W")) +
  scale_y_continuous(breaks = seq(-40, 20, 20), labels = function(y) ifelse(y == 0, "0°", paste0(abs(y), ifelse(y < 0, "°S", "°N")))) +
  coord_sf(xlim = c(-120, -25), ylim = c(-58, 35), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") + tema() + theme(panel.grid.major = element_line(colour = "grey94"))

# ---- (B) densidade
x <- x %>% mutate(rot = ifelse(land_area_km2 > 5e5, paste0(country, "*"), country))
pB <- ggplot(x, aes(sites_per_100k_km2, reorder(rot, sites_per_100k_km2))) +
  geom_segment(aes(x = 0.2, xend = sites_per_100k_km2, yend = reorder(rot, sites_per_100k_km2)), colour = "grey80", linewidth = 0.6) +
  geom_point(colour = "grey45", size = 2.6) +
  geom_vline(xintercept = med, linetype = "dashed", colour = "grey35", linewidth = 0.5) +
  annotate("text", x = med * 1.15, y = 1, label = sprintf("median = %.1f", med), hjust = 0, size = 3.4, colour = "grey30", family = FONTE) +
  scale_x_log10(breaks = c(1, 10, 100, 1000), labels = c("1", "10", "100", "1,000"), limits = c(0.2, 2500), expand = expansion(mult = c(0, 0.03))) +
  labs(x = "Land-based sites per 100,000 km² (log scale)", y = NULL, caption = "* countries > 500,000 km²") +
  tema() + theme(panel.grid.major.y = element_blank(), plot.caption = element_text(colour = "grey35", size = 9))

salvar((pA | pB) + plot_layout(widths = c(1.15, 1)) + tags, "Figure1", 12, 6.6)

# GDP: partial correlations between density and GDP, controlling for land area (countries with GDP data; Cuba has none)
# GDP per capita (NY.GDP.PCAP.CD) is the measure used by Moussy et al. (2022) and reported in the text;
# total GDP (NY.GDP.MKTP.CD) is kept as a check (reported in the response to reviewers)
g <- d %>% left_join(d0[, c("iso3", "gdp_usd_2021", "gdp_per_capita_usd_2021")], by = "iso3") %>%
  filter(!is.na(gdp_usd_2021), !is.na(gdp_per_capita_usd_2021))
r1 <- resid(lm(log(sites_per_100k_km2 + 0.1) ~ log(land_area_km2), g))
for (v in c("gdp_per_capita_usd_2021", "gdp_usd_2021")) {
  r2 <- resid(lm(log(g[[v]]) ~ log(g$land_area_km2)))
  t <- cor.test(r1, r2)
  cat(sprintf("04 | %s: partial r = %.2f, P = %.2f, n = %d\n",
              ifelse(v == "gdp_usd_2021", "GDP (total)", "GDP per capita"), t$estimate, t$p.value, nrow(g)))
}
