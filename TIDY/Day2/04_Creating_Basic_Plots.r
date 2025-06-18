####################################################################################################
###
### File:    04_Creating_Basic_Plots.R
### Purpose: Creating Basic Plots: Scatterplots, 
###         Bar Charts, and Line Graphs
### Authors: Gabriel Rodrigues Palma
### Date:    18/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
####################### Basic Plot Types in ggplot2 #########################
################################################################################

# Example 1: Scatterplots for Ecological Relationships
tree_allometry <- data.frame(
  species = rep(c("Quercus alba", "Pinus strobus", "Acer rubrum"), 
                each = 20),
  dbh_cm = c(runif(20, 10, 45), runif(20, 8, 40), runif(20, 12, 38)),
  height_m = c(runif(20, 8, 25), runif(20, 12, 30), runif(20, 6, 22)),
  age_years = c(sample(25:85, 20), sample(30:95, 20), 
                sample(15:70, 20)),
  site_quality = rep(c("High", "Medium", "Low"), 20)
)

# Basic scatterplot
basic_scatter <- ggplot(tree_allometry, 
                        aes(x = dbh_cm, y = height_m)) +
  geom_point() +
  labs(title = "Tree Height vs Diameter Relationship",
       x = "Diameter at Breast Height (cm)",
       y = "Height (m)")

# Enhanced scatterplot with grouping
enhanced_scatter <- ggplot(tree_allometry, 
                           aes(x = dbh_cm, y = height_m, 
                               color = species, 
                               shape = site_quality)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = species)) +
  scale_color_manual(values = c("#2E8B57", "#8B4513", "#CD853F")) +
  labs(title = "Allometric Relationships by Species",
       subtitle = "Tree height increases with diameter",
       x = "Diameter at Breast Height (cm)",
       y = "Height (m)",
       color = "Tree Species",
       shape = "Site Quality") +
  theme_bw()

enhanced_scatter_interaction <- ggplot(tree_allometry, 
       aes(x = dbh_cm, y = height_m, 
           color = species, 
           shape = site_quality)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, 
              aes(group = interaction(species, site_quality), 
                  color = species, linetype = site_quality)) +
  scale_color_manual(values = c("#2E8B57", "#8B4513", "#CD853F")) +
  labs(title = "Allometric Relationships by Species",
       subtitle = "Tree height increases with diameter",
       x = "Diameter at Breast Height (cm)",
       y = "Height (m)",
       color = "Tree Species",
       shape = "Site Quality",
       linetype = "Site Quality") +
  theme_bw()


# Bubble plot with third variable
bubble_plot <- ggplot(tree_allometry, 
                      aes(x = dbh_cm, y = height_m, 
                          size = age_years, color = species)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(range = c(2, 8)) +
  labs(title = "Tree Dimensions and Age",
       x = "DBH (cm)", y = "Height (m)",
       size = "Age (years)", color = "Species") +
  theme_minimal()

# Example 2: Bar Charts for Categorical Data
habitat_biodiversity <- data.frame(
  habitat = c("Grassland", "Forest", "Wetland", "Shrubland", 
              "Desert"),
  species_richness = c(42, 68, 55, 38, 28),
  endemic_species = c(8, 15, 12, 6, 9),
  threatened_species = c(3, 8, 6, 2, 4)
)

# Simple bar chart
simple_bar <- ggplot(habitat_biodiversity, 
                     aes(x = habitat, y = species_richness)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(title = "Species Richness by Habitat Type",
       x = "Habitat Type",
       y = "Number of Species") +
  theme_classic()

# Grouped bar chart using pivot_longer
biodiversity_long <- habitat_biodiversity %>%
  pivot_longer(cols = c(species_richness, endemic_species, 
                        threatened_species),
               names_to = "category", 
               values_to = "count")

grouped_bar <- ggplot(biodiversity_long, 
                      aes(x = habitat, y = count, fill = category)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("#2E8B57", "#FF6347", "#4682B4"),
                    labels = c("Endemic", "Richness", "Threatened")) +
  labs(title = "Biodiversity Metrics by Habitat",
       x = "Habitat Type", y = "Species Count",
       fill = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Horizontal bar chart
horizontal_bar <- ggplot(habitat_biodiversity, 
                         aes(x = reorder(habitat, species_richness), 
                             y = species_richness)) +
  geom_col(fill = "darkgreen", alpha = 0.7) +
  coord_flip() +
  labs(title = "Species Richness by Habitat (Ranked)",
       x = "Habitat Type", y = "Species Richness") +
  theme_bw()

# Example 3: Line Graphs for Time Series
population_trends <- data.frame(
  year = rep(2010:2023, 4),
  population = rep(c("Wolves", "Deer", "Bears", "Elk"), each = 14),
  count = c(
    # Wolves: recovering population
    c(45, 52, 48, 55, 62, 58, 65, 72, 68, 75, 82, 78, 85, 92),
    # Deer: fluctuating but stable
    c(1250, 1180, 1320, 1290, 1220, 1350, 1280, 1240, 1380, 
      1310, 1270, 1340, 1300, 1260),
    # Bears: slow increase
    c(28, 30, 32, 31, 35, 38, 36, 40, 43, 41, 45, 48, 46, 50),
    # Elk: declining
    c(890, 850, 820, 780, 740, 710, 680, 650, 620, 590, 560, 
      530, 500, 470)
  )
)

# Basic line plot
basic_line <- ggplot(population_trends, 
                     aes(x = year, y = count, color = population)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Wildlife Population Trends",
       x = "Year", y = "Population Count",
       color = "Species") +
  theme_minimal()

# Line plot with confidence intervals (simulated)
population_with_error <- population_trends %>%
  mutate(se = count * 0.1,  # Simulated standard error
         lower_ci = count - 1.96 * se,
         upper_ci = count + 1.96 * se)

line_with_ci <- ggplot(population_with_error, 
                       aes(x = year, y = count, color = population)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, 
                  fill = population), alpha = 0.2) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(title = "Wildlife Population Trends with Uncertainty",
       x = "Year", y = "Population Count") +
  theme_bw()

# Example 4: Combining Plot Types
climate_ecosystem <- data.frame(
  month = rep(month.abb, 2),
  temperature = c(2, 4, 8, 14, 20, 25, 28, 26, 21, 15, 8, 3,
                  5, 7, 11, 17, 23, 28, 31, 29, 24, 18, 11, 6),
  precipitation = c(45, 52, 68, 85, 95, 110, 85, 75, 65, 55, 
                    48, 42, 35, 42, 58, 75, 85, 100, 75, 65, 
                    55, 45, 38, 32),
  location = rep(c("Mountain", "Valley"), each = 12)
)

# Combining line and bar elements
combo_plot <- ggplot(climate_ecosystem, 
                     aes(x = factor(month, levels = month.abb))) +
  geom_col(aes(y = precipitation/5, fill = location), 
           alpha = 0.6, position = "dodge") +
  geom_line(aes(y = temperature, color = location, 
                group = location), size = 1.2) +
  geom_point(aes(y = temperature, color = location), size = 3) +
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(transform = ~.*5, name = "Precipitation (mm)")
  ) +
  labs(title = "Climate Patterns at Two Locations",
       x = "Month") +
  theme_minimal()

# Example 5: Specialized Ecological Plots
species_accumulation <- data.frame(
  sampling_effort = 1:20,
  cumulative_species = c(12, 18, 23, 27, 30, 33, 35, 37, 39, 
                         40, 41, 42, 43, 43, 44, 44, 45, 45, 
                         45, 45),
  site = rep(c("Pristine", "Disturbed"), each = 20)
)

# Species accumulation curve
accumulation_plot <- ggplot(species_accumulation, 
                            aes(x = sampling_effort, 
                                y = cumulative_species, 
                                color = site)) +
  geom_line(size = 1.3) +
  geom_point(size = 2) +
  geom_hline(yintercept = 40, linetype = "dashed", 
             color = "gray50") +
  annotate("text", x = 15, y = 42, 
           label = "Asymptote", color = "gray50") +
  labs(title = "Species Accumulation Curves",
       subtitle = "Comparing pristine vs disturbed sites",
       x = "Sampling Effort (hours)",
       y = "Cumulative Species Count",
       color = "Site Type") +
  theme_classic()

################################################################################
############### Basic Plot Types Exercises ###################################
################################################################################

# Exercise 1: Bird Migration Scatter Analysis
# Dataset: bird arrival dates and spring temperature
bird_migration <- data.frame(
  species = rep(c("Robin", "Warbler", "Swallow", "Flycatcher"), 
                each = 15),
  arrival_day = c(runif(15, 80, 120), runif(15, 100, 140), 
                  runif(15, 90, 130), runif(15, 110, 150)),
  spring_temp = runif(60, 5, 18),
  population_size = rpois(60, 1500),
  migration_distance = c(rep(1200, 15), rep(3500, 15), 
                         rep(8000, 15), rep(2800, 15))
)

# Create scatterplot of arrival dates vs spring temperature
# Color by species and size by population size
# Add trend lines and confidence intervals

# Exercise 2: Forest Management Bar Chart Comparison
# Dataset: tree survival rates across management practices
forest_management <- data.frame(
  management_type = rep(c("Clear_cut", "Selective", "No_harvest"), 
                        each = 12),
  forest_type = rep(c("Deciduous", "Coniferous", "Mixed", "Mixed"), 9),
  survival_rate = c(runif(12, 0.4, 0.7), runif(12, 0.7, 0.9), 
                    runif(12, 0.8, 0.95)),
  years_post_treatment = rep(c(1, 3, 5, 10), 9),
  regeneration_success = runif(36, 0.2, 0.9)
)

# Build grouped bar chart comparing survival rates
# Group by management practice and forest type
# Use error bars and custom color schemes

# Exercise 3: Stream Temperature Line Graph
# Dataset: daily temperature measurements across stations
stream_temperature <- data.frame(
  date = rep(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), 
                 by = "day"), 3),
  temperature = c(runif(365, 2, 15), runif(365, 4, 18), 
                  runif(365, 6, 22)),
  station = rep(c("Upstream", "Midstream", "Downstream"), 
                each = 365),
  flow_rate = runif(1095, 0.5, 5.2),
  air_temperature = rep(runif(365, -5, 30), 3)
)

# Design line graph showing temperature over the year
# Include multiple stations with different line styles
# Add seasonal reference lines and annotations

# Exercise 4: Pollinator Activity Combination Plot
# Dataset: flower bloom periods and pollinator visits
pollinator_activity <- data.frame(
  week = rep(1:26, 4),
  flower_abundance = c(rpois(26, 20), rpois(26, 35), 
                       rpois(26, 45), rpois(26, 25)),
  pollinator_visits = c(rpois(26, 150), rpois(26, 200), 
                        rpois(26, 180), rpois(26, 120)),
  site_type = rep(c("Urban", "Suburban", "Rural", "Natural"), 
                  each = 26),
  temperature = rep(runif(26, 12, 28), 4)
)

# Create combination plot with bloom periods (bars) and visits (lines)
# Use dual y-axes and coordinated color schemes
# Add smooth trend lines and seasonal markers

# Exercise 5: Soil Nutrient Bubble Plot
# Dataset: nitrogen vs phosphorus with organic matter
soil_nutrients <- data.frame(
  plot_id = paste0("Plot_", 1:30),
  nitrogen_ppm = runif(30, 20, 80),
  phosphorus_ppm = runif(30, 5, 25),
  organic_matter_percent = runif(30, 2, 8),
  pH = runif(30, 5.5, 8.0),
  land_use = rep(c("Cropland", "Pasture", "Forest", 
                   "Grassland", "Urban", "Wetland"), 5)
)

# Develop bubble plot with nitrogen vs phosphorus
# Bubble size represents organic matter, color shows pH
# Add reference lines for optimal nutrient ratios

################################################################################
###################### Advanced Basic Plot Techniques #######################
################################################################################

# Example 1: Advanced Scatterplot Techniques
coral_reef_data <- data.frame(
  coral_cover = runif(60, 10, 85),
  fish_diversity = runif(60, 15, 45),
  depth_m = runif(60, 2, 30),
  protection_status = rep(c("Protected", "Unprotected"), 30),
  temperature_anomaly = rnorm(60, 0, 1.5),
  bleaching_severity = sample(c("None", "Mild", "Severe"), 
                              60, replace = TRUE)
)

# Scatterplot with multiple aesthetics and faceting
advanced_scatter <- ggplot(coral_reef_data, 
                           aes(x = coral_cover, y = fish_diversity)) +
  geom_point(aes(color = protection_status, 
                 size = depth_m,
                 alpha = abs(temperature_anomaly))) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ bleaching_severity) +
  scale_color_manual(values = c("Protected" = "#2E8B57", 
                                "Unprotected" = "#CD5C5C")) +
  scale_size_continuous(range = c(1, 5)) +
  scale_alpha_continuous(range = c(0.3, 1)) +
  labs(title = "Coral Reef Ecosystem Health",
       subtitle = "Fish diversity vs coral cover by bleaching status",
       x = "Coral Cover (%)",
       y = "Fish Species Diversity",
       color = "Protection Status",
       size = "Depth (m)",
       alpha = "Temperature Anomaly") +
  theme_bw()

# Density scatterplot for large datasets
large_dataset <- data.frame(
  x = rnorm(1000, 15, 5),
  y = rnorm(1000, 25, 8)
)

density_scatter <- ggplot(large_dataset, aes(x = x, y = y)) +
  geom_hex(bins = 20) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "High-Density Ecological Data",
       x = "Environmental Variable 1",
       y = "Environmental Variable 2") +
  theme_minimal()

# Example 2: Advanced Bar Chart Techniques
ecosystem_services <- data.frame(
  ecosystem = rep(c("Forest", "Grassland", "Wetland", "Marine"), 
                  each = 5),
  service = rep(c("Carbon Storage", "Water Regulation", 
                  "Biodiversity", "Recreation", "Food Production"), 4),
  value_billion_usd = c(
    # Forest
    15.2, 8.7, 12.3, 4.1, 2.8,
    # Grassland  
    5.8, 6.2, 7.9, 2.3, 8.4,
    # Wetland
    3.2, 18.5, 9.1, 1.8, 3.7,
    # Marine
    2.1, 4.3, 11.8, 6.2, 12.9
  ),
  uncertainty = runif(20, 0.8, 1.4)
)

# Stacked bar chart
stacked_bar <- ggplot(ecosystem_services, 
                      aes(x = ecosystem, y = value_billion_usd, 
                          fill = service)) +
  geom_col(position = "stack") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(title = "Ecosystem Service Values by Ecosystem Type",
       x = "Ecosystem Type",
       y = "Economic Value (Billion USD)",
       fill = "Ecosystem Service") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Percentage stacked bar
percentage_bar <- ggplot(ecosystem_services, 
                         aes(x = ecosystem, y = value_billion_usd, 
                             fill = service)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  labs(title = "Relative Contribution of Ecosystem Services",
       x = "Ecosystem Type",
       y = "Percentage of Total Value",
       fill = "Service Type") +
  theme_classic()

# Bar chart with error bars
summary_data <- ecosystem_services %>%
  group_by(ecosystem) %>%
  summarise(
    mean_value = mean(value_billion_usd),
    se_value = sd(value_billion_usd) / sqrt(n()),
    .groups = 'drop'
  )

bar_with_error <- ggplot(summary_data, 
                         aes(x = ecosystem, y = mean_value)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_value - se_value,
                    ymax = mean_value + se_value),
                width = 0.2, color = "black") +
  labs(title = "Mean Ecosystem Service Value ± SE",
       x = "Ecosystem Type",
       y = "Mean Value (Billion USD)") +
  theme_bw()

# Example 3: Advanced Line Graph Techniques
phenology_data <- data.frame(
  day_of_year = rep(1:365, 6),
  species = rep(c("Early_Bloomer", "Mid_Bloomer", "Late_Bloomer"), 
                each = 730),
  year = rep(rep(c(2020, 2023), each = 365), 3),
  flowering_intensity = c(
    # Early bloomer patterns
    c(rep(0, 60), dnorm(61:120, 90, 15) * 100, rep(0, 245)),  
    c(rep(0, 50), dnorm(51:110, 80, 15) * 100, rep(0, 255)),
    # Mid bloomer patterns
    c(rep(0, 120), dnorm(121:180, 150, 20) * 100, rep(0, 185)),
    c(rep(0, 110), dnorm(111:170, 140, 20) * 100, rep(0, 195)),
    # Late bloomer patterns
    c(rep(0, 200), dnorm(201:260, 230, 25) * 100, rep(0, 105)),
    c(rep(0, 190), dnorm(191:250, 220, 25) * 100, rep(0, 115))
  )
)

# Multiple line plot with areas
phenology_plot <- ggplot(phenology_data, 
                         aes(x = day_of_year, y = flowering_intensity)) +
  geom_area(aes(fill = factor(year)), alpha = 0.4, 
            position = "identity") +
  geom_line(aes(color = factor(year)), size = 1) +
  facet_wrap(~ species, scales = "free_y") +
  scale_fill_manual(values = c("2020" = "blue", "2023" = "red")) +
  scale_color_manual(values = c("2020" = "darkblue", 
                                "2023" = "darkred")) +
  labs(title = "Flowering Phenology Shifts",
       subtitle = "Comparison between 2020 and 2023",
       x = "Day of Year",
       y = "Flowering Intensity",
       fill = "Year", color = "Year") +
  theme_minimal()

# Example 4: Interactive-style Annotations
conservation_timeline <- data.frame(
  year = 1970:2024,
  protected_area_percent = cumsum(c(5, rep(c(0.2, 0.3, 0.1, 0.4), 
                                           13), 0.2, 0.1)),
  species_extinctions = c(rep(c(2, 3, 1, 4, 2), 10), 1, 2, 1, 3, 1)
)

# Annotated timeline
#Rio Earth Summit (1992): Highlighted with a yellow vertical band Marks the landmark 1992 
#United Nations Conference on Environment and Development Established international 
#frameworks for biodiversity conservation

#CBD Targets (2010-2015): Indicated with a blue arrow and text 
#References the Convention on Biological Diversity's strategic targets
#Likely pointing to the Aichi Biodiversity Targets (2011-2020)

timeline_plot <- ggplot(conservation_timeline, 
                        aes(x = year)) +
  geom_line(aes(y = protected_area_percent), 
            color = "darkgreen", size = 1.2) +
  geom_point(aes(y = species_extinctions * 2), 
             color = "red", size = 2) +
  annotate("rect", xmin = 1992, xmax = 1993, 
           ymin = -Inf, ymax = Inf, 
           alpha = 0.3, fill = "yellow") +
  annotate("text", x = 1992.5, y = 15, 
           label = "Rio Earth\nSummit", 
           size = 3, fontface = "bold") +
  annotate("segment", x = 2010, y = 12, xend = 2015, yend = 8,
           arrow = arrow(length = unit(0.2, "cm")), 
           color = "blue") +
  annotate("text", x = 2005, y = 13, 
           label = "CBD Targets", color = "blue") +
  scale_y_continuous(
    name = "Protected Area (%)",
    sec.axis = sec_axis(trans = ~./2, 
                        name = "Species Extinctions")
  ) +
  labs(title = "Conservation Progress Over Time",
       x = "Year") +
  theme_bw()

# Example 5: Complex Multi-variable Plots
biodiversity_drivers <- data.frame(
  region = rep(c("Tropical", "Temperate", "Boreal", "Arctic"), 
               each = 25),
  habitat_area = runif(100, 100, 10000),
  fragmentation_index = runif(100, 0.1, 0.9),
  species_richness = runif(100, 10, 200),
  climate_velocity = runif(100, 0.5, 5.0),
  human_footprint = runif(100, 0, 100)
)

# Multi-dimensional plot
multi_plot <- ggplot(biodiversity_drivers, 
                     aes(x = habitat_area, y = species_richness)) +
  geom_point(aes(color = fragmentation_index,
                 size = climate_velocity,
                 alpha = human_footprint/100)) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ region, scales = "free") +
  scale_x_log10() +
  scale_color_gradient2(low = "green", mid = "yellow", 
                        high = "red", midpoint = 0.5) +
  scale_size_continuous(range = c(1, 6)) +
  labs(title = "Biodiversity Drivers Across Biomes",
       x = "Habitat Area (km²)",
       y = "Species Richness",
       color = "Fragmentation",
       size = "Climate Velocity",
       alpha = "Human Footprint") +
  theme_bw()

################################################################################
############# Advanced Basic Plot Techniques Exercises ######################
################################################################################

# Exercise 1: Advanced Pollinator Network Analysis
# Dataset: plant-pollinator interaction data with network metrics
pollinator_network <- data.frame(
  plant_species = rep(paste0("Plant_", 1:15), each = 8),
  pollinator_type = rep(c("Bee", "Butterfly", "Fly", "Beetle"), 30),
  interaction_strength = runif(120, 0, 1),
  network_specialization = runif(120, 0.1, 0.8),
  plant_abundance = rep(runif(15, 10, 100), each = 8),
  pollinator_size = runif(120, 2, 25),
  habitat_connectivity = runif(120, 0.2, 0.9),
  season = rep(c("Spring", "Summer", "Autumn"), 40)
)

# Create complex scatterplot with multiple aesthetic mappings
# Use faceting for seasons and custom color/size scales
# Add trend lines and statistical summaries

# Exercise 2: Ecosystem Service Value Stacking
# Dataset: economic values across landscape types
ecosystem_economics <- data.frame(
  landscape = c(rep(c("Urban", "Suburban", "Agricultural", 
                    "Natural"), each = 20), "Natural"),
  service_type = c(rep(c("Carbon", "Water", "Recreation", 
                       "Biodiversity", "Food"), 16), "Food"),
  economic_value = c(runif(20, 1000, 8000), runif(20, 2000, 12000),
                     runif(20, 5000, 25000), runif(21, 8000, 40000)),
  uncertainty_percent = runif(81, 10, 30),
  policy_support = rep(c("High", "Medium", "Low"), 27)
)

# Build percentage stacked bar chart
# Show relative importance across landscape types
# Include uncertainty indicators and policy annotations

# Exercise 3: Multi-year Species Population Trends
# Dataset: population monitoring with confidence intervals
species_monitoring <- data.frame(
  year = rep(2010:2023, 5),
  species = rep(c("Species_A", "Species_B", "Species_C", 
                  "Species_D", "Species_E"), each = 14),
  population_estimate = c(runif(14, 500, 800), runif(14, 200, 400),
                          runif(14, 1000, 1500), runif(14, 50, 150),
                          runif(14, 300, 600)),
  lower_ci = runif(70, 0.8, 0.9),
  upper_ci = runif(70, 1.1, 1.3),
  conservation_status = rep(c("Stable", "Declining", "Increasing", 
                              "Endangered", "Recovering"), each = 14)
)

# Design line graph with confidence ribbons
# Include trend annotations and conservation status indicators
# Add reference lines for management targets

# Exercise 4: Forest Fire Frequency and Impact Analysis
# Dataset: fire occurrence and ecosystem response
fire_analysis <- data.frame(
  year = rep(1980:2023, 2),
  fire_frequency = c(rpois(44, 8), rpois(44, 12)),
  average_fire_size = c(runif(44, 100, 800), runif(44, 200, 1200)),
  ecosystem_recovery_time = c(runif(44, 5, 15), runif(44, 8, 20)),
  region = rep(c("Boreal", "Temperate"), each = 44),
  climate_factor = runif(88, -2, 3)
)

# Create combination plot with frequency (bars) and size (lines)
# Include trend indicators and climate relationship
# Use dual axes and coordinated color schemes

# Exercise 5: Marine Food Web Dynamics
# Dataset: predator-prey relationships with environmental factors
marine_foodweb <- data.frame(
  predator_species = c(rep(c("Shark", "Tuna", "Cod", "Seal"), each = 20), 'Seal'),
  prey_biomass = runif(81, 50, 500),
  predation_rate = runif(81, 0.1, 0.8),
  predator_size = c(rep(200, 20), rep(80, 20), rep(15, 20), 
                    rep(120, 21)),
  ocean_temperature = runif(81, 8, 22),
  depth_zone = rep(c("Surface", "Mid", "Deep"), 27),
  fishing_pressure = runif(81, 0, 1)
)

# Develop multi-dimensional scatterplot
# Show trophic relationships with environmental gradients
# Use different aesthetics for size, temperature, and fishing pressure

