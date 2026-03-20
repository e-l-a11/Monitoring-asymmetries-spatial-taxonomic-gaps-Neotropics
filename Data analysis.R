# ==============================================================================
#eeabbad@gmail.com
#Elisa L. Abbad
# 0. LOAD LIBRARIES AND DATA
# ==============================================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(tidyverse)
library(sf)
library(patchwork)
library(ggridges)
library(networkD3)
library(htmlwidgets)
library(scales)
library(RColorBrewer)

# --- Load Shapefiles ---
mundo <- read_sf("Mapa_paises_mundo.shp")
eco_shape_terr <- read_sf("wwf_terr_ecos.shp")
eco_shape_marine <- read_sf("meow_ecos.shp")

# --- Load CSV Data ---
site_data <- read.csv("dados.networks.2025 - site.data (1).csv")
variable_data <- read.csv("dados.networks.2025 - variable.data (1).csv")
groups_time <- read.csv("dados.networks.2025 - groups_time (3).csv")
site_eco <- read.csv("dados.networks.2025 - site_eco (1).csv")


# ==============================================================================
# 1. FIGURE 1: MONITORING SITES MAP
# ==============================================================================
site_data$Site.Latitude <- as.numeric(site_data$Site.Latitude)
site_data$Site.Longitude <- as.numeric(site_data$Site.Longitude)
mundo.sf <- st_as_sf(mundo)
total_pontos <- nrow(site_data)

shape.mundo <- ggplot(data = mundo.sf) +
  geom_sf(fill = "grey90", color = "white", linewidth = 0.25) +
  coord_sf(xlim = c(-130, 0), ylim = c(-80, 60), expand = FALSE) +
  geom_point(data = site_data, aes(x = Site.Longitude, y = Site.Latitude), 
             color = "#d49d88", fill = "#d49d88", alpha = 0.6, shape = 21, size = 3) +
  labs(title = "Neotropical Monitoring Projects",
       subtitle = paste("Total projects (points):", total_pontos),
       y = "Latitude", x = "Longitude") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid = element_blank(), axis.line = element_blank(),
        plot.title = element_text(size = 18, face = "bold", color = "#444444", hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "#555555", hjust = 0.5))

print(shape.mundo)


# ==============================================================================
# 2. FIGURE 2: TEMPORAL ADEQUACY (PLANTAE AND ANIMALIA)
# ==============================================================================
# --- 2.1 Prepare PLANTAE Data ---
data_filtrada_order <- groups_time %>% 
  dplyr::filter(kingdom == "Plantae") %>%
  mutate(lifespan.max = as.numeric(lifespan.max)) %>%
  drop_na(monitoring.time..years., lifespan.max, order) %>%
  mutate(order = str_trim(order)) %>%
  dplyr::filter(!is.na(order), !tolower(order) %in% c("na", ""), order != "N/A", lifespan.max > 0) %>%
  mutate(lifespan_monitoring_ratio = as.numeric(monitoring.time..years.) / lifespan.max) %>%
  drop_na(lifespan_monitoring_ratio) %>%
  mutate(lifespan_monitoring_ratio = if_else(lifespan_monitoring_ratio > 1, 1, lifespan_monitoring_ratio),
         order = fct_drop(order)) %>%
  group_by(order) %>% mutate(n_count = n()) %>% ungroup() %>% dplyr::filter(n_count >= 10)

data_summarizada_order <- data_filtrada_order %>% 
  group_by(order, n_count) %>% 
  summarise(mean_ratio = mean(lifespan_monitoring_ratio), sd_ratio = sd(lifespan_monitoring_ratio), .groups = 'drop') %>%
  left_join(data_filtrada_order %>% group_by(order) %>% summarise(species_richness = n_distinct(species)), by = "order") %>%
  arrange(desc(species_richness)) %>% slice_head(n = 40) %>%
  mutate(order_label = fct_reorder(paste0(order, " (n=", n_count, ")"), species_richness, .desc = TRUE))

data_individual_final_order <- data_filtrada_order %>% 
  left_join(data_summarizada_order %>% select(order, order_label), by = "order") %>% drop_na(order_label)

# --- 2.2 Prepare ANIMALIA Data ---
data_filtrada_order_animalia <- groups_time %>% 
  dplyr::filter(kingdom == "Animalia") %>%
  mutate(lifespan.max = as.numeric(lifespan.max)) %>%
  drop_na(monitoring.time..years., lifespan.max, order) %>%
  mutate(order = str_trim(order)) %>%
  dplyr::filter(!is.na(order), !tolower(order) %in% c("na", ""), order != "N/A", lifespan.max > 0) %>%
  mutate(lifespan_monitoring_ratio = as.numeric(monitoring.time..years.) / lifespan.max) %>%
  drop_na(lifespan_monitoring_ratio) %>%
  mutate(lifespan_monitoring_ratio = if_else(lifespan_monitoring_ratio > 1, 1, lifespan_monitoring_ratio),
         order = fct_drop(order)) %>%
  group_by(order) %>% mutate(n_count = n()) %>% ungroup() %>% dplyr::filter(n_count >= 10)

data_summarizada_order_animalia <- data_filtrada_order_animalia %>% 
  group_by(order, n_count) %>% 
  summarise(mean_ratio = mean(lifespan_monitoring_ratio), sd_ratio = sd(lifespan_monitoring_ratio), .groups = 'drop') %>%
  left_join(data_filtrada_order_animalia %>% group_by(order) %>% summarise(species_richness = n_distinct(species)), by = "order") %>%
  arrange(desc(species_richness)) %>% slice_head(n = 40) %>%
  mutate(order_label = fct_reorder(paste0(order, " (n=", n_count, ")"), species_richness, .desc = TRUE))

data_individual_final_order_animalia <- data_filtrada_order_animalia %>% 
  left_join(data_summarizada_order_animalia %>% select(order, order_label), by = "order") %>% drop_na(order_label)

# --- 2.3 Generate and Combine Plots ---
p_left_plantae <- ggplot() +
  geom_col(data = data_summarizada_order, aes(x = mean_ratio, y = order_label, fill = order), alpha = 0.8, height = 0.5) +
  geom_errorbarh(data = data_summarizada_order, aes(y = order_label, xmin = pmax(0, mean_ratio - sd_ratio), xmax = pmin(1, mean_ratio + sd_ratio)), height = 0.2, color = "black", alpha = 0.7, linewidth = 0.4, linetype = "dotted") +
  geom_jitter(data = data_individual_final_order, aes(x = lifespan_monitoring_ratio, y = order_label, color = order), width = 0, height = 0.15, alpha = 0.8, size = 1.5) +
  scale_x_continuous("Mean (± 1 SD) & Individual Obs.", labels = percent_format(), limits = c(0, 1)) +
  scale_fill_viridis_d(option = "cividis", guide = "none") + scale_color_viridis_d(option = "cividis", guide = "none") +
  labs(y = "Order") + theme_minimal()

p_dist_plantae <- ggplot(data_individual_final_order, aes(x = lifespan.max, y = order_label, fill = order)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9, bandwidth = 0.1) +
  scale_x_log10("Lifespan (years)", breaks = c(1, 10, 100, 1000)) +
  scale_fill_viridis_d(option = "cividis", guide = "none") + labs(y = NULL) +
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p_final_plantae <- p_left_plantae + p_dist_plantae + plot_layout(widths = c(1.5, 1)) + plot_annotation(title = "Kingdom Plantae")

p_left_animalia <- ggplot() +
  geom_col(data = data_summarizada_order_animalia, aes(x = mean_ratio, y = order_label, fill = order), alpha = 0.8, height = 0.5) +
  geom_errorbarh(data = data_summarizada_order_animalia, aes(y = order_label, xmin = pmax(0, mean_ratio - sd_ratio), xmax = pmin(1, mean_ratio + sd_ratio)), height = 0.2, color = "black", alpha = 0.7, linewidth = 0.4, linetype = "dotted") +
  geom_jitter(data = data_individual_final_order_animalia, aes(x = lifespan_monitoring_ratio, y = order_label, color = order), width = 0, height = 0.15, alpha = 0.8, size = 1.5) +
  scale_x_continuous("Mean (± 1 SD) & Individual Obs.", labels = percent_format(), limits = c(0, 1)) +
  scale_fill_viridis_d(option = "cividis", guide = "none") + scale_color_viridis_d(option = "cividis", guide = "none") +
  labs(y = "Order") + theme_minimal()

p_dist_animalia <- ggplot(data_individual_final_order_animalia, aes(x = lifespan.max, y = order_label, fill = order)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9, bandwidth = 0.1) +
  scale_x_log10("Lifespan (years)", breaks = c(1, 10, 100, 1000)) +
  scale_fill_viridis_d(option = "cividis", guide = "none") + labs(y = NULL) +
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p_final_animalia <- p_left_animalia + p_dist_animalia + plot_layout(widths = c(1.5, 1)) + plot_annotation(title = "Kingdom Animalia")

print(p_final_plantae / p_final_animalia)


# ==============================================================================
# 3. FIGURE 3: BIOTIC COMPOSITION (MARINE VS TERRESTRIAL)
# ==============================================================================
biotic_marine <- variable_data %>% filter(Variable.Type == "biotic", Region == "marine")
data_totals_p1 <- biotic_marine %>% count(Groups.variables, name = "Total_Count") %>% 
  mutate(Groups.variables = fct_relevel(Groups.variables, levels(fct_infreq(biotic_marine$Groups.variables))))

p_biotic_marine <- ggplot(biotic_marine, aes(x = fct_rev(fct_infreq(Groups.variables)), fill = Metrics)) +
  geom_bar(position = "fill", width = 0.9, color = "white") +
  geom_text(data = data_totals_p1, aes(x = fct_rev(Groups.variables), y = 1.02, label = paste0("n=", Total_Count)), inherit.aes = FALSE, hjust = 0, size = 4) +
  scale_fill_viridis_d(option = "A") + scale_y_continuous(labels = percent, limits = c(0, 1.1)) +
  coord_flip() + theme_minimal() + labs(title = "(a) Biotic Marine", x = "Taxonomic Groups", y = "Proportion (%)")

biotic_terrestrial_cleaned <- variable_data %>% filter(Variable.Type == "biotic", Region == "terrestrial") %>% 
  mutate(Metrics = str_trim(tolower(Metrics))) %>% filter(!Metrics %in% c("na", ""))
data_totals_p3 <- biotic_terrestrial_cleaned %>% count(Groups.variables, name = "Total_Count") %>% 
  mutate(Groups.variables = fct_relevel(Groups.variables, levels(fct_infreq(biotic_terrestrial_cleaned$Groups.variables))))

p_biotic_terrestrial <- ggplot(biotic_terrestrial_cleaned, aes(x = fct_rev(fct_infreq(Groups.variables)), fill = Metrics)) +
  geom_bar(position = "fill", width = 0.9, color = "white") +
  geom_text(data = data_totals_p3, aes(x = fct_rev(Groups.variables), y = 1.02, label = paste0("n=", Total_Count)), inherit.aes = FALSE, hjust = 0, size = 4) +
  scale_fill_viridis_d(option = "D") + scale_y_continuous(labels = percent, limits = c(0, 1.1)) +
  coord_flip() + theme_minimal() + labs(title = "(b) Biotic Terrestrial", x = "Taxonomic Groups", y = "Proportion (%)")

print(p_biotic_marine / p_biotic_terrestrial)


# ==============================================================================
# 4. FIGURE 4: SANKEY DIAGRAM (TROPHIC FLOW)
# ==============================================================================
# Join the tables
sankey_data_raw <- inner_join(groups_time, variable_data, by = "Network.ID") # Standardized names

# Cleaning and standardization
sankey_data_cleaned <- sankey_data_raw %>%
  # REMOVE EVERYTHING 'abiotic'
  filter(tolower(Variable.Type) != "abiotic") %>%
  mutate(Metrics = str_trim(tolower(Metrics))) %>%
  filter(!Metrics %in% c("na", ""))

# Create the "links" table (now with biotic data only)
links <- sankey_data_cleaned %>%
  group_by(trophic.category, Metrics) %>%
  summarise(value = n(), .groups = 'drop')

colnames(links) <- c("source", "target", "value")


# --- CREATE 'nodes' WITH THE FIRST COLUMN ORDERED ---

# Calculate the total value of each SOURCE node (first column)
source_totals <- links %>%
  group_by(source) %>%
  summarise(total_value = sum(value)) %>%
  # Order by total count, descending
  arrange(desc(total_value))

# Get the source names (already ordered)
ordered_sources <- source_totals$source

# Get the target names (that are not sources) and sort them (alphabetically)
all_targets <- unique(as.character(links$target))
ordered_targets <- sort(all_targets[!all_targets %in% ordered_sources])

# Create the "nodes" table IN THE CORRECT ORDER
nodes <- data.frame(
  name = c(ordered_sources, ordered_targets)
)

# Convert names to numeric IDs (starting at 0)
links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1


# --- STEP 2: DEFINE COLOR PALETTE FOR ALL NODES ---
my_palette <- c("#ea698b", "#d55d92", "#c05299", "#ac46a1", "#973aa8",
                "#822faf", "#6d23b6", "#6411ad", "#571089", "#47126b")
color_generator <- colorRampPalette(my_palette)
n_nodes <- nrow(nodes)
all_node_colors <- color_generator(n_nodes)
my_color_palette_d3 <- sprintf("d3.scaleOrdinal().domain(['%s']).range(['%s'])",
                               paste(nodes$name, collapse="','"),
                               paste(all_node_colors, collapse="','"))


# --- STEP 3: GENERATE THE PLOT (GRAY LINES) ---
sankeyPlot_base <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  colourScale = my_color_palette_d3,
  sinksRight = FALSE,
  fontSize = 14,
  nodeWidth = 30,
  nodePadding = 10
)

# --- STEP 4 (OPTIONAL): MAKE THE GRAY LINES EVEN SOFTER ---
sankeyPlot_final <- sankeyPlot_base %>%
  onRender(
    '
    function(el, x) {
      // 1. Selects all lines (links) in the plot
      d3.select(el).selectAll(".link")
        // 2. Sets the stroke opacity to 20% (very soft)
        .style("stroke-opacity", 0.2);
    }
    '
  )

# Display the plot
sankeyPlot_final

# Carrega os objetos 'links_final' e 'nodes'
load("sankey_data_para_plotar_TOP10.RData")

#### 3. RECRIAR OS IDs (APENAS POR PRECAUÇÃO) ----
# Apenas para garantir que os links e nós estão corretos.
nodes$ID <- 0:(nrow(nodes)-1)
links_final$IDsource <- match(links_final$source, nodes$name) - 1
links_final$IDtarget <- match(links_final$target, nodes$name) - 1

#### 4. DEFINIR AS CORES (MÉTODO SIMPLES E CORRETO) ----
# Este método é muito mais robusto e vai funcionar.
# Ele mapeia os *grupos* (e não cada nome individual).
ColourScale_4col <- 'd3.scaleOrdinal()
  .domain(["Trophic Group", "Biotic Metric", "Terrestrial Ecoregion", "Marine Ecoregion"])
  .range(["#ea698b", "#ac46a1", "#2E8B57", "#0073e6"]);' 
# Cores: Rosa, Roxo, Verde ("SeaGreen"), Azul Forte

#### 5. PLOTAR O GRÁFICO (COM CORES CORRIGIDAS) ----

# 5.1: Gerar o gráfico Sankey
sankeyPlot_3col <- sankeyNetwork(
  Links = links_final,
  Nodes = nodes,            # Usamos os 'nodes' originais
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",      # DEVE ser "group"
  colourScale = ColourScale_4col, # DEVE ser a escala de 4 cores
  sinksRight = FALSE,       
  fontSize = 12,
  nodeWidth = 30,
  nodePadding = 12, 
  width = "100%",
  height = 800
)

# 5.2: Adicionar efeitos de opacidade e tooltips
sankeyPlot_3col_final <- sankeyPlot_3col %>%
  onRender(
    '
    function(el, x) {
      d3.select(el).selectAll(".link")
        .style("stroke-opacity", 0.2);
        
      d3.selectAll(".link").append("title")
        .text(function(d) { return d.source.name + " -> " + d.target.name + ": " + d.value; });
      d3.selectAll(".node").append("title")
        .text(function(d) { return d.name + ": " + d.value; });
    }
    '
  )

# 5.3: Exibir o plot
print(sankeyPlot_3col_final)
# (Logo depois do 'print(sankeyPlot_3col_final)')

# Salva o gráfico como um arquivo HTML interativo
saveWidget(sankeyPlot_3col_final, "meu_sankey_interativo.html")
# ==============================================================================
# 5. FIGURE 5: TERRESTRIAL ECOREGIONS (MAP + DONUT)
# ==============================================================================
grupos_para_remover <- c("corals", "plankton", "seagrass", "benthos", "zooplaktons")

eco_composition_data <- variable_data %>% filter(Variable.Type == "biotic", !Groups.variables %in% grupos_para_remover) %>% 
  left_join(site_eco, by = "Network.ID", relationship = "many-to-many") %>% 
  drop_na(ECO_NAME, Groups.variables) %>% group_by(ECO_NAME, Groups.variables) %>% summarise(count = n(), .groups = "drop")

top_20_eco <- eco_composition_data %>% group_by(ECO_NAME) %>% summarise(Total_Count = sum(count), .groups = "drop") %>% 
  arrange(desc(Total_Count)) %>% slice_head(n = 20) %>% mutate(Rank = 1:n())

eco_table_map <- eco_composition_data %>% group_by(ECO_NAME) %>% mutate(most_common = Groups.variables[which.max(count)]) %>% distinct(ECO_NAME, .keep_all = TRUE)
valid_groups <- unique(eco_composition_data$Groups.variables)[!is.na(unique(eco_composition_data$Groups.variables))]
group_colors <- colorRampPalette(brewer.pal(min(length(valid_groups), 12), "Set3"))(length(valid_groups))
names(group_colors) <- valid_groups

eco_map_simplified <- eco_shape_terr %>% left_join(eco_table_map, by = "ECO_NAME") %>% st_make_valid() %>% st_simplify(dTolerance = 0.01)
eco_labels <- eco_map_simplified %>% filter(ECO_NAME %in% top_20_eco$ECO_NAME) %>% left_join(top_20_eco, by = "ECO_NAME") %>% group_by(ECO_NAME, Rank) %>% summarise(geometry = st_union(geometry), .groups = "drop") %>% mutate(geometry_centroid = st_point_on_surface(geometry))

map_plot <- ggplot(data = eco_map_simplified) +
  geom_sf(aes(fill = most_common), color = "black", size = 0.1) +
  geom_sf_text(data = eco_labels, aes(geometry = geometry_centroid, label = Rank), size = 2.5, color = "black", fontface = "bold") +
  scale_fill_manual(values = group_colors, na.value = "grey80") + coord_sf(xlim = c(-130, 0), ylim = c(-80, 60)) + theme_minimal() + theme(legend.position = "none") +
  labs(title = "A) Terrestrial Ecoregions by Group")

donut_data <- eco_composition_data %>% filter(ECO_NAME %in% top_20_eco$ECO_NAME) %>% left_join(top_20_eco, by = "ECO_NAME") %>% 
  mutate(Rank_Label = fct_reorder(ECO_NAME, Rank, .desc = TRUE))
donut_plot <- ggplot(donut_data, aes(x = 2, y = count, fill = Groups.variables)) +
  geom_col(position = "fill", color = "white", linewidth = 0.5) + coord_polar(theta = "y", start = 0) + xlim(c(0.5, 2.5)) +
  facet_wrap(~ Rank_Label, ncol = 5) + scale_fill_manual(values = group_colors) + theme_minimal() + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(title = "B) Composition of the Top 20 Ecoregions", fill = "Taxonomic Groups", x = NULL, y = NULL)

print(map_plot / donut_plot + plot_layout(heights = c(2, 1.5)))


# ==============================================================================
# 6. FIGURE 6: MARINE ECOREGIONS (MAP + DONUT)
# ==============================================================================
eco_composition_data_marinho <- variable_data %>% filter(Variable.Type == "biotic") %>% 
  left_join(site_eco, by = "Network.ID", relationship = "many-to-many") %>% filter(str_detect(tolower(REALM), "mar")) %>%
  drop_na(ECOREGION, Groups.variables) %>% group_by(ECOREGION, Groups.variables) %>% summarise(count = n(), .groups = "drop")

top_20_eco_marinho <- eco_composition_data_marinho %>% group_by(ECOREGION) %>% summarise(Total_Count = sum(count), .groups = "drop") %>% 
  arrange(desc(Total_Count)) %>% slice_head(n = 20) %>% mutate(Rank = 1:n())

eco_table_map_marinho <- eco_composition_data_marinho %>% group_by(ECOREGION) %>% mutate(most_common = Groups.variables[which.max(count)]) %>% distinct(ECOREGION, .keep_all = TRUE)
valid_groups_marinho <- unique(eco_composition_data_marinho$Groups.variables)[!is.na(unique(eco_composition_data_marinho$Groups.variables))]
group_colors_marinho <- colorRampPalette(brewer.pal(min(length(valid_groups_marinho), 12), "Set3"))(length(valid_groups_marinho))
names(group_colors_marinho) <- valid_groups_marinho

eco_map_simplified_marinho <- eco_shape_marine %>% left_join(eco_table_map_marinho, by = "ECOREGION") %>% st_make_valid() %>% st_simplify(dTolerance = 0.01)
eco_labels_marinho <- eco_map_simplified_marinho %>% filter(ECOREGION %in% top_20_eco_marinho$ECOREGION) %>% left_join(top_20_eco_marinho, by = "ECOREGION") %>% group_by(ECOREGION, Rank) %>% summarise(geometry = st_union(geometry), .groups = "drop") %>% mutate(geometry_centroid = st_point_on_surface(geometry))

map_plot_marinho <- ggplot(data = eco_map_simplified_marinho) +
  geom_sf(aes(fill = most_common), color = "black", size = 0.1) +
  geom_sf_text(data = eco_labels_marinho, aes(geometry = geometry_centroid, label = Rank), size = 2.5, color = "black", fontface = "bold") +
  scale_fill_manual(values = group_colors_marinho, na.value = "grey80") + coord_sf(xlim = c(-130, 0), ylim = c(-80, 60)) + theme_minimal() + theme(legend.position = "none") +
  labs(title = "A) Marine Ecoregions by Group")

donut_data_marinho <- eco_composition_data_marinho %>% filter(ECOREGION %in% top_20_eco_marinho$ECOREGION) %>% left_join(top_20_eco_marinho, by = "ECOREGION") %>% left_join(eco_table_map_marinho %>% select(ECOREGION, most_common), by = "ECOREGION") %>% mutate(Rank_Label = fct_reorder(ECOREGION, most_common, .desc = TRUE))

donut_plot_marinho <- ggplot(donut_data_marinho, aes(x = 2, y = count, fill = Groups.variables)) +
  geom_col(position = "fill", color = "white", linewidth = 0.5) + coord_polar(theta = "y", start = 0) + xlim(c(0.5, 2.5)) +
  facet_wrap(~ Rank_Label, ncol = 5) + scale_fill_manual(values = group_colors_marinho) + theme_minimal() + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(title = "B) Composition of the Top 20 Marine Ecoregions", fill = "Taxonomic Groups", x = NULL, y = NULL)

print(map_plot_marinho / donut_plot_marinho + plot_layout(heights = c(2, 1.5)))


# ==============================================================================
# 7. FIGURE S1: ABIOTIC METRICS (SUPPLEMENTARY MATERIAL)
# ==============================================================================
abiotic_data <- variable_data %>% 
  filter(tolower(Variable.Type) == "abiotic") %>%
  mutate(Metrics = str_trim(tolower(Metrics))) %>%
  filter(!is.na(Metrics), Metrics != "") %>%
  count(Metrics, name = "Number_of_networks") %>%
  arrange(desc(Number_of_networks))

p_abiotic <- ggplot(abiotic_data, aes(x = Number_of_networks, y = fct_reorder(Metrics, Number_of_networks))) +
  geom_col(fill = "#9c3f60") + 
  labs(title = "Abiotic Metrics Used in Monitoring Programs",
       subtitle = "Terrestrial and marine environments combined",
       x = "Number of monitoring networks",
       y = "Abiotic metric") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

print(p_abiotic)


# ==============================================================================
# 8. TEXT ANALYSES: DECAY CURVES AND GRAPHS (HALF-LIFE)
# ==============================================================================
df_sp <- read_csv("dados.networks.2025 - groups_time (6).csv")
sp_final <- df_sp %>% 
  mutate(lifespan_num = as.numeric(lifespan.max), 
         mon_time_num = as.numeric(`monitoring time (years)`)) %>%
  filter(!is.na(lifespan_num), lifespan_num > 0, !is.na(detailed_group)) %>%
  mutate(coverage = pmin(100, (mon_time_num / lifespan_num) * 100))

calcular_decay <- function(dados) {
  nls(coverage ~ 100 * exp(-r * lifespan_num), data = dados, start = list(r = 0.01))
}

r_vert <- coef(calcular_decay(sp_final %>% filter(detailed_group == "Vertebrate")))["r"]
r_invert <- coef(calcular_decay(sp_final %>% filter(detailed_group == "Invertebrate")))["r"]
r_pla <- coef(calcular_decay(sp_final %>% filter(detailed_group == "Plantae")))["r"]

resultados_decay <- data.frame(
  Group = c("Vertebrates", "Invertebrates", "Plantae"),
  Decay_Rate_r = c(r_vert, r_invert, r_pla),
  Coverage_Half_Life_Years = c(log(2)/r_vert, log(2)/r_invert, log(2)/r_pla)
)
print("--- DECAY RESULTS ---")
print(resultados_decay)

p_vert <- sp_final %>% filter(detailed_group == "Vertebrate") %>% 
  ggplot(aes(x = lifespan_num, y = coverage)) + 
  geom_point(color = "#0072B2", alpha = 0.4, size = 2) +
  stat_function(fun = function(x) 100 * exp(-r_vert * x), color = "black", linewidth = 1) +
  scale_x_continuous(limits = c(0, 150)) + scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  theme_bw() + labs(title = "Vertebrates", x = "Lifespan (years)", y = "Coverage (%)")

p_invert <- sp_final %>% filter(detailed_group == "Invertebrate") %>% 
  ggplot(aes(x = lifespan_num, y = coverage)) + 
  geom_point(color = "#56B4E9", alpha = 0.4, size = 2) +
  stat_function(fun = function(x) 100 * exp(-r_invert * x), color = "black", linewidth = 1) +
  scale_x_continuous(limits = c(0, 150)) + scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  theme_bw() + labs(title = "Invertebrates", x = "Lifespan (years)", y = "")

p_plant <- sp_final %>% filter(detailed_group == "Plantae") %>% 
  ggplot(aes(x = lifespan_num, y = coverage)) + 
  geom_point(color = "#E69F00", alpha = 0.4, size = 2) +
  stat_function(fun = function(x) 100 * exp(-r_pla * x), color = "black", linewidth = 1) +
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  theme_bw() + labs(title = "Plantae", x = "Lifespan (years)", y = "")

print(p_vert | p_invert | p_plant)


# ==============================================================================
# 9. TEXT ANALYSES: ECOREGION PERCENTAGES (CSV EXPORT)
# ==============================================================================
get_eco_comp <- function(eco_name, data) {
  comp <- data %>% filter(ECO_NAME == eco_name) %>% mutate(total = sum(count), percentage = round((count / total) * 100, 1)) %>% arrange(desc(percentage))
  return(comp)
}
get_pct_safe <- function(composition, group_pattern) {
  result <- composition %>% filter(str_detect(tolower(Groups.variables), tolower(group_pattern))) %>% pull(percentage)
  if(length(result) == 0) return(0)
  return(result)
}

eco_composition_terr <- variable_data %>% filter(Variable.Type == "biotic") %>% left_join(site_eco, by = "Network.ID") %>% filter(str_detect(tolower(REALM), "terr")) %>% filter(!is.na(ECO_NAME)) %>% group_by(ECO_NAME, Groups.variables) %>% summarise(count = n(), .groups = "drop")
eco_composition_mar <- variable_data %>% filter(Variable.Type == "biotic") %>% left_join(site_eco, by = "Network.ID") %>% filter(str_detect(tolower(REALM), "mar")) %>% filter(!is.na(ECO_NAME)) %>% group_by(ECO_NAME, Groups.variables) %>% summarise(count = n(), .groups = "drop")

# Terrestrial Values
serra_mar <- get_eco_comp("Serra do Mar coastal forests", eco_composition_terr)
sw_amazon <- get_eco_comp("Southwest Amazon moist forests", eco_composition_terr)
cerrado <- get_eco_comp("Cerrado", eco_composition_terr)
mangrove <- get_eco_comp("Southern Atlantic mangroves", eco_composition_terr)

ecoregion_results <- data.frame(
  Metric = c("Serra do Mar - plants %", "Serra do Mar - mammals %", "Serra do Mar - birds %", "Serra do Mar - insects %",
             "Southwest Amazon - mammals %", "Cerrado - plants %", "Cerrado - insects %",
             "Southern Atlantic mangroves - plants %", "Southern Atlantic mangroves - crustaceans %", "Southern Atlantic mangroves - fishes %"),
  Value = c(get_pct_safe(serra_mar, "plant"), get_pct_safe(serra_mar, "mammal"), get_pct_safe(serra_mar, "bird"), get_pct_safe(serra_mar, "insect"),
            get_pct_safe(sw_amazon, "mammal"), get_pct_safe(cerrado, "plant"), get_pct_safe(cerrado, "insect"),
            get_pct_safe(mangrove, "plant"), get_pct_safe(mangrove, "crustacean"), get_pct_safe(mangrove, "fish"))
)
write.csv(ecoregion_results, "ecoregion_values_final.csv", row.names = FALSE)

# Marine Values
greater_antilles <- get_eco_comp("Greater Antilles", eco_composition_mar)
eastern_carib <- get_eco_comp("Eastern Caribbean", eco_composition_mar)
se_brazil <- get_eco_comp("Southeastern Brazil", eco_composition_mar)
channels_chile <- get_eco_comp("Channels and Fjords of Southern Chile", eco_composition_mar)
araucanian <- get_eco_comp("Araucanian", eco_composition_mar)

caribbean_tropical <- c("Greater Antilles", "Eastern Caribbean", "Western Caribbean", "Southern Caribbean", "Northeastern Brazil")
group_stats <- data.frame()
for(group in c("benthos", "coral", "crustacean")) {
  values <- c()
  for(eco in caribbean_tropical) {
    comp <- get_eco_comp(eco, eco_composition_mar)
    val <- get_pct_safe(comp, group)
    if(val > 0) values <- c(values, val)
  }
  if(length(values) > 0) {
    group_stats <- rbind(group_stats, data.frame(Group = group, Min = min(values), Max = max(values), Mean = round(mean(values), 1)))
  }
}

marine_detail_results <- data.frame(
  Metric = c("Greater Antilles - fishes %", "Greater Antilles - birds %", "Southeastern Brazil - fishes %", "Southeastern Brazil - birds %",
             "Channels & Fjords Chile - mammals %", "Araucanian - mammals %", 
             "Greater Antilles - n groups", "Eastern Caribbean - n groups"),
  Value = c(get_pct_safe(greater_antilles, "fish"), get_pct_safe(greater_antilles, "bird"), get_pct_safe(se_brazil, "fish"), get_pct_safe(se_brazil, "bird"),
            get_pct_safe(channels_chile, "mammal"), get_pct_safe(araucanian, "mammal"), 
            nrow(greater_antilles), nrow(eastern_carib))
)
write.csv(marine_detail_results, "marine_detail_values.csv", row.names = FALSE)

cat("\n✅ Analyses completed. Files successfully generated: ecoregion_values_final.csv and marine_detail_values.csv\n")