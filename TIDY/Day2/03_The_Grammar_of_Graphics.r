####################################################################################################
###
### File:    03_The_Grammar_of_Graphics.R
### Purpose: Examples and exercises for The Grammar of Graphics: 
###         Foundations of ggplot2
### Authors: Gabriel Rodrigues Palma
### Date:    18/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
###################### ggplot2 Grammar of Graphics ##########################
################################################################################

# Example 1: Basic ggplot2 Structure with Species Data
species_abundance <- data.frame(
  species = c("Quercus alba", "Pinus strobus", "Acer rubrum",
              "Betula nigra", "Fagus grandifolia"),
  count = c(45, 32, 28, 19, 15),
  site = c("Forest_A", "Forest_A", "Forest_B", "Forest_B", 
           "Forest_C")
)

# Basic scatter plot showing ggplot structure
basic_plot <- ggplot(data = species_abundance, 
                     aes(x = species, y = count)) +
  geom_point() +
  labs(title = "Species Abundance by Location",
       x = "Species", 
       y = "Individual Count")

# Adding color aesthetic mapping
colored_plot <- ggplot(species_abundance, 
                       aes(x = species, y = count, color = site)) +
  geom_point(size = 3) +
  labs(title = "Species Abundance by Site",
       x = "Species", 
       y = "Count",
       color = "Forest Site")

# Example 2: Aesthetic Mappings vs Fixed Values
climate_data <- data.frame(
  temperature = c(15.2, 18.7, 22.1, 25.8, 19.3, 16.9, 21.4),
  precipitation = c(45, 62, 38, 71, 55, 48, 59),
  season = c("Spring", "Summer", "Summer", "Summer", 
             "Spring", "Spring", "Summer"),
  elevation = c(450, 680, 320, 890, 560, 720, 380)
)

# Mapping aesthetics to variables
mapped_aesthetics <- ggplot(climate_data, 
                            aes(x = temperature, 
                                y = precipitation,
                                color = season,
                                size = elevation)) +
  geom_point() +
  labs(title = "Climate Variables with Mapped Aesthetics")

# Fixed aesthetic values
fixed_aesthetics <- ggplot(climate_data, 
                           aes(x = temperature, y = precipitation)) +
  geom_point(color = "darkgreen", size = 4, alpha = 0.7) +
  labs(title = "Climate Variables with Fixed Aesthetics")

# Example 3: Layers and Geometric Objects
plant_growth <- data.frame(
  day = 1:30,
  height_cm = c(2.1, 2.8, 3.5, 4.2, 5.1, 5.8, 6.7, 7.5, 8.4, 
                9.2, 10.1, 11.0, 11.8, 12.7, 13.5, 14.4, 15.2, 
                16.1, 16.9, 17.8, 18.6, 19.5, 20.3, 21.2, 22.0, 
                22.9, 23.7, 24.6, 25.4, 26.3),
  treatment = rep(c("Control", "Fertilized"), each = 15)
)

# Multiple layers on same plot
layered_plot <- ggplot(plant_growth, 
                       aes(x = day, y = height_cm, 
                           color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_line() +
  labs(title = "Plant Growth Over Time",
       x = "Days", 
       y = "Height (cm)",
       color = "Treatment")

# Example 4: Statistical Transformations
biodiversity_data <- data.frame(
  habitat = rep(c("Grassland", "Forest", "Wetland"), each = 20),
  species_count = c(rpois(20, 12), rpois(20, 18), rpois(20, 15))
)

# Using stat_summary for mean and error bars
summary_plot <- ggplot(biodiversity_data, 
                       aes(x = habitat, y = species_count)) +
  geom_point(position = position_jitter(width = 0.2), 
             alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", 
               color = "red", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               color = "red", width = 0.2) +
  labs(title = "Species Count by Habitat Type",
       x = "Habitat", 
       y = "Species Count")

# Using built-in statistical transformation
histogram_plot <- ggplot(biodiversity_data, 
                         aes(x = species_count)) +
  geom_histogram(bins = 10, fill = "lightblue", 
                 color = "black", alpha = 0.7) +
  labs(title = "Distribution of Species Counts",
       x = "Species Count", 
       y = "Frequency")

# Example 5: Coordinate Systems and Scales
tree_measurements <- data.frame(
  species = c("Oak", "Pine", "Maple", "Birch", "Cedar"),
  dbh_cm = c(45.2, 38.7, 52.1, 29.8, 41.3),
  height_m = c(18.5, 22.1, 16.8, 12.4, 19.7),
  age_years = c(85, 95, 72, 45, 88)
)

# Cartesian coordinates with custom scales
cartesian_plot <- ggplot(tree_measurements, 
                         aes(x = dbh_cm, y = height_m, 
                             color = age_years)) +
  geom_point(size = 4) +
  scale_x_continuous(limits = c(20, 60), 
                     breaks = seq(20, 60, 10)) +
  scale_y_continuous(limits = c(10, 25), 
                     breaks = seq(10, 25, 5)) +
  scale_color_gradient(low = "yellow", high = "darkgreen") +
  labs(title = "Tree Measurements with Custom Scales",
       x = "Diameter at Breast Height (cm)",
       y = "Height (m)",
       color = "Age (years)")

# Polar coordinates for circular data
polar_plot <- ggplot(tree_measurements, 
                     aes(x = species, y = dbh_cm, fill = species)) +
  geom_col() +
  coord_polar() +
  labs(title = "Tree DBH in Polar Coordinates") +
  theme_minimal()

################################################################################
################ ggplot2 Grammar of Graphics Exercises ######################
################################################################################

# Exercise 1: Soil pH Visualization Fundamentals
# Dataset: soil chemistry measurements across plots
soil_ph_data <- data.frame(
  plot_id = paste0("Plot_", 1:20),
  soil_pH = runif(20, 5.5, 8.0),
  plant_diversity = rpois(20, 15),
  moisture_level = sample(c("Low", "Medium", "High"), 20, 
                          replace = TRUE),
  organic_matter = runif(20, 2, 8)
)

# Create scatter plot mapping pH to diversity
# Add color for moisture level and size for organic matter
# Practice different geometric objects and aesthetic mappings

# Exercise 2: Bird Migration Pattern Basics
# Dataset: bird count observations over time
migration_counts <- data.frame(
  date = seq(as.Date("2023-03-01"), as.Date("2023-05-31"), 
             by = "week"),
  species_A = rpois(14, 25),
  species_B = rpois(14, 18),
  species_C = rpois(14, 12),
  weather = sample(c("Clear", "Cloudy", "Rainy"), 14, 
                   replace = TRUE)
)

# Build basic line plot with date and bird counts
# Add points for observations and lines for trends
# Experiment with different layer combinations

# Exercise 3: Forest Canopy Structure Analysis
# Dataset: tree measurements in different forest types
canopy_data <- data.frame(
  tree_height = runif(30, 5, 35),
  canopy_cover = runif(30, 40, 95),
  forest_type = rep(c("Deciduous", "Coniferous", "Mixed"), 10),
  diameter = runif(30, 10, 60),
  age_class = sample(c("Young", "Mature", "Old"), 30, 
                     replace = TRUE)
)

# Create scatter plot with height vs canopy cover
# Use different shapes for forest type and colors for age
# Practice scale customization and coordinate systems

# Exercise 4: Pollinator Activity Visualization
# Dataset: flower visits and pollinator abundance
pollinator_activity <- data.frame(
  flower_abundance = rpois(25, 50),
  pollinator_visits = rpois(25, 75),
  habitat_type = rep(c("Urban", "Suburban", "Rural", 
                       "Natural", "Agricultural"), 5),
  time_period = sample(c("Morning", "Afternoon", "Evening"), 
                       25, replace = TRUE)
)

# Build plot showing relationship between flowers and visits
# Add statistical summaries and error representations
# Practice with different geometric objects and statistics

# Exercise 5: Water Quality Assessment Basics
# Dataset: aquatic measurements with quality indicators
water_quality_data <- data.frame(
  temperature = runif(18, 8, 25),
  dissolved_oxygen = runif(18, 5, 12),
  site_type = rep(c("Upstream", "Midstream", "Downstream"), 6),
  season = rep(c("Spring", "Summer"), 9),
  pollution_level = sample(c("Low", "Medium", "High"), 18, 
                           replace = TRUE)
)

# Create multi-layer plot with temperature and oxygen
# Add trend lines and highlight critical thresholds
# Experiment with different aesthetic mappings and layers

################################################################################
#################### Advanced ggplot2 Grammar Elements ######################
################################################################################

# Example 1: Position Adjustments and Theme Basics
community_survey <- data.frame(
  site = rep(c("Site_A", "Site_B", "Site_C"), each = 40),
  species_group = rep(c("Birds", "Mammals", "Reptiles", 
                        "Amphibians"), 30),
  abundance = rpois(120, 15),
  season = rep(c("Spring", "Summer", "Autumn", "Winter"), 30),
  observer = rep(c("Team_1", "Team_2"), 60)
)

# Position adjustments for overlapping data
position_plot <- ggplot(community_survey, 
                        aes(x = species_group, y = abundance, 
                            fill = season)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Species Abundance by Group and Season",
       x = "Species Group", 
       y = "Total Abundance") +
  theme_minimal()

# Using position_jitter for points
jitter_plot <- ggplot(community_survey, 
                      aes(x = species_group, y = abundance, 
                          color = season)) +
  geom_point(position = position_jitter(width = 0.3), 
             alpha = 0.7) +
  labs(title = "Species Abundance with Jittered Points") +
  theme_bw()

# Example 2: Color and Fill Scales
water_chemistry <- data.frame(
  sample_id = 1:50,
  pH = runif(50, 6.5, 8.5),
  dissolved_oxygen = runif(50, 4, 12),
  temperature = runif(50, 5, 25),
  pollution_level = cut(runif(50, 0, 10), 
                        breaks = c(0, 3, 6, 10),
                        labels = c("Low", "Medium", "High")),
  water_body = sample(c("Lake_A", "Lake_B", "River_C"), 
                      50, replace = TRUE)
)

# Custom color scales for continuous variables
continuous_color <- ggplot(water_chemistry, 
                           aes(x = pH, y = dissolved_oxygen)) +
  geom_point(aes(color = temperature), size = 3) +
  scale_color_gradient2(low = "blue", mid = "white", 
                        high = "red", midpoint = 15) +
  labs(title = "Water Chemistry Relationships",
       x = "pH", 
       y = "Dissolved Oxygen (mg/L)",
       color = "Temperature (°C)") +
  theme_minimal()

# Discrete color palette for categorical variables
discrete_color <- ggplot(water_chemistry, 
                         aes(x = water_body, y = pH, 
                             fill = pollution_level)) +
  geom_violin() +
  scale_fill_brewer(type = "div", palette = "RdYlBu", 
                    direction = -1) +
  labs(title = "pH Distribution by Pollution Level",
       x = "Water Body", 
       y = "pH",
       fill = "Pollution Level") +
  theme_bw()

# Example 3: Faceting Basics
forest_structure <- data.frame(
  plot = paste0("Plot_", 1:25),
  canopy_cover = runif(25, 60, 95),
  understory_density = runif(25, 20, 80),
  dead_wood_volume = runif(25, 5, 25),
  management = rep(c("Managed", "Natural"), c(12, 13)),
  region = rep(c("North", "South"), c(13, 12))
)

# Basic faceting with wrap
facet_wrap_plot <- ggplot(forest_structure, 
                          aes(x = canopy_cover, 
                              y = understory_density,
                              color = management)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ region) +
  labs(title = "Forest Structure by Region",
       x = "Canopy Cover (%)",
       y = "Understory Density (stems/ha)") +
  theme_classic()

# Grid faceting
facet_grid_plot <- ggplot(forest_structure, 
                          aes(x = canopy_cover, 
                              y = understory_density)) +
  geom_point(aes(color = dead_wood_volume), size = 3) +
  facet_grid(management ~ region) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Forest Structure: Management vs Region") +
  theme_bw()

# Example 4: Annotations and Labels
species_richness <- data.frame(
  elevation = seq(100, 2000, 100),
  richness = c(45, 48, 52, 55, 58, 60, 58, 55, 52, 48, 
               45, 42, 38, 35, 32, 28, 25, 22, 18, 15),
  habitat_type = rep(c("Lowland", "Montane", "Alpine"), 
                     c(6, 8, 6))
)

# Adding annotations and reference lines
annotated_plot <- ggplot(species_richness, 
                         aes(x = elevation, y = richness)) +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  geom_point(aes(color = habitat_type), size = 3) +
  annotate("text", x = 1000, y = 50, 
           label = "Peak Diversity", 
           size = 4, fontface = "bold") +
  annotate("segment", x = 1000, y = 48, xend = 800, yend = 58,
           arrow = arrow(length = unit(0.3, "cm")),
           color = "red") +
  geom_hline(yintercept = 40, linetype = "dashed", 
             color = "blue", alpha = 0.7) +
  labs(title = "Species Richness Along Elevation Gradient",
       x = "Elevation (m)", 
       y = "Species Richness",
       color = "Habitat Type") +
  theme_minimal()

# Example 5: Coordinate System Transformations
ecosystem_data <- data.frame(
  biomass = c(100, 250, 400, 800, 1200, 2000, 3500, 5000),
  productivity = c(15, 25, 35, 45, 50, 55, 58, 60),
  ecosystem = rep(c("Grassland", "Forest"), each = 4),
  climate = rep(c("Temperate", "Tropical"), 4)
)

# Log scale transformation
log_scale_plot <- ggplot(ecosystem_data, 
                         aes(x = biomass, y = productivity,
                             color = ecosystem, shape = climate)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  labs(title = "Ecosystem Productivity vs Biomass",
       subtitle = "Log-transformed biomass axis",
       x = "Biomass (kg/ha, log scale)",
       y = "Productivity (g/m²/day)") +
  theme_minimal()

# Coordinate transformation with limits
coord_transform_plot <- ggplot(ecosystem_data, 
                               aes(x = biomass, y = productivity,
                                   color = ecosystem)) +
  geom_point(size = 4) +
  coord_trans(x = "log10") +
  xlim(50, 6000) +
  ylim(10, 65) +
  labs(title = "Transformed Coordinate System") +
  theme_classic()

################################################################################
########### Advanced ggplot2 Grammar Elements Exercises #####################
################################################################################

# Exercise 1: Phenology Analysis with Advanced Grammar
# Dataset: flowering times across species and years
phenology_data <- data.frame(
  species = rep(c("Early_Bloomer", "Mid_Bloomer", "Late_Bloomer"), 
                each = 24),
  year = rep(2020:2023, 18),
  flowering_day = c(runif(24, 80, 120), runif(24, 140, 180), 
                    runif(24, 200, 240)),
  temperature_spring = runif(72, 8, 18),
  precipitation_winter = runif(72, 200, 800),
  site = rep(c("North", "South"), 36)
)

# Create multi-faceted plot with custom themes and colors
# Use position adjustments and scale transformations
# Add annotations for important ecological thresholds

# Exercise 2: Marine Biodiversity Depth Gradients
# Dataset: species richness and abundance with depth
marine_biodiversity <- data.frame(
  depth_m = seq(0, 200, 10),
  species_richness = c(45, 42, 38, 35, 32, 28, 25, 22, 20, 18,
                       16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6),
  total_abundance = rpois(21, 150),
  temperature = runif(21, 4, 20),
  light_penetration = exp(-0.05 * seq(0, 200, 10)),
  zone = c(rep("Euphotic", 8), rep("Dysphotic", 8), 
           rep("Aphotic", 5))
)

# Design complex visualization with depth gradients
# Use color scales representing temperature and light
# Include annotations for important depth thresholds

# Exercise 3: Forest Succession Pattern Analysis
# Dataset: vegetation changes over succession time
succession_data <- data.frame(
  years_since_disturbance = rep(c(0, 5, 10, 20, 50, 100), each = 15),
  tree_density = c(rpois(15, 5), rpois(15, 25), rpois(15, 45),
                   rpois(15, 60), rpois(15, 50), rpois(15, 35)),
  canopy_height = c(runif(15, 0, 2), runif(15, 2, 8), 
                    runif(15, 8, 15), runif(15, 15, 25),
                    runif(15, 20, 30), runif(15, 25, 35)),
  succession_stage = rep(c("Pioneer", "Early", "Mid", "Late", 
                           "Mature", "Old-growth"), each = 15),
  disturbance_type = rep(c("Fire", "Logging", "Storm"), 30)
)

# Build plot with multiple layers and succession stages
# Use position adjustments and custom legends
# Practice with different coordinate systems

# Exercise 4: Pollinator Network Complex Visualization
# Dataset: plant-pollinator interaction strength data
network_data <- data.frame(
  plant_species = rep(paste0("Plant_", 1:10), each = 12),
  pollinator_group = rep(c("Bees", "Butterflies", "Flies", "Birds"), 30),
  interaction_strength = runif(120, 0, 1),
  plant_abundance = rep(runif(10, 10, 100), each = 12),
  pollinator_abundance = runif(120, 5, 50),
  season = rep(c("Spring", "Summer", "Autumn"), 40),
  habitat = rep(c("Urban", "Rural"), 60)
)

# Create sophisticated plot with interaction networks
# Use faceting for seasons and custom color schemes
# Add statistical summaries and position adjustments

# Exercise 5: Climate Change Impact Multi-panel
# Dataset: temperature trends and species responses
climate_impact <- data.frame(
  year = rep(1980:2023, 3),
  temperature_anomaly = c(rnorm(44, 0, 0.8), rnorm(44, 0.5, 0.9), 
                          rnorm(44, 1.2, 1.0)),
  species_abundance = c(rpois(44, 100), rpois(44, 85), rpois(44, 70)),
  precipitation_change = rnorm(132, 0, 20),
  region = rep(c("Arctic", "Temperate", "Tropical"), each = 44),
  ecosystem_type = rep(c("Tundra", "Forest", "Rainforest"), each = 44)
)

# Design multi-panel layout with coordinated themes
# Use scale transformations and custom annotations
# Practice advanced faceting and color coordination

