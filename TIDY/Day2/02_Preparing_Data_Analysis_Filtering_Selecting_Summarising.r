####################################################################################################
###
### File:    02_Preparing_Data_Analysis_Filtering_Selecting_Summarising.R
### Purpose: Examples and exercises for Preparing Data for Analysis: 
###         Filtering, Selecting, and Summarising
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
##################### Data Filtering & Selection ############################
################################################################################

# Example 1: Species Abundance Filtering
species_data <- data.frame(
  site = rep(c("Site_A", "Site_B", "Site_C"), each = 10),
  species = rep(c("Quercus_alba", "Pinus_strobus", "Acer_rubrum",
                  "Betula_nigra", "Fagus_grandifolia"), 6),
  dbh_cm = c(12.5, 8.2, 15.7, 22.1, 18.9, 9.8, 14.2, 25.6, 
             19.3, 11.7, 16.4, 10.1, 18.8, 26.3, 21.2, 12.9, 
             17.5, 28.1, 20.8, 13.6, 14.8, 8.9, 16.2, 24.7, 
             19.1, 11.3, 15.9, 27.4, 22.5, 12.1),
  height_m = c(6.2, 4.1, 8.9, 12.8, 10.2, 5.3, 7.8, 14.5, 
               10.8, 6.7, 9.1, 5.8, 10.4, 15.2, 11.9, 7.2, 
               9.7, 16.1, 11.3, 7.8, 8.3, 4.9, 9.0, 13.8, 
               10.5, 6.4, 8.7, 15.8, 12.6, 6.9)
)

# Filter mature trees and select key variables
mature_trees <- species_data %>%
  filter(dbh_cm >= 15) %>%
  select(site, species, dbh_cm, height_m) %>%
  arrange(desc(dbh_cm))

# Filter by species and site combinations
oak_sites <- species_data %>%
  filter(species == "Quercus_alba", 
         site %in% c("Site_A", "Site_B")) %>%
  select(site, dbh_cm, height_m)

# Example 2: Water Quality Data Selection
water_monitoring <- data.frame(
  date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), 
             by = "week"),
  station = rep(c("Upstream", "Midstream", "Downstream"), 
                length.out = 53),
  temperature_c = runif(53, 5, 25),
  pH = runif(53, 6.5, 8.5),
  dissolved_oxygen = runif(53, 6, 12),
  turbidity_ntu = runif(53, 1, 15),
  nitrate_mgl = runif(53, 0.1, 2.5),
  phosphate_mgl = runif(53, 0.05, 0.8)
)

# Filter summer months with quality concerns
summer_quality <- water_monitoring %>%
  filter(lubridate::month(date) %in% c(6, 7, 8),
         dissolved_oxygen < 8 | pH > 8.0) %>%
  select(date, station, temperature_c, dissolved_oxygen, pH) %>%
  arrange(date)

# Select essential parameters for reporting
essential_params <- water_monitoring %>%
  select(date, station, temperature_c, pH, dissolved_oxygen) %>%
  filter(station == "Downstream")

# Example 3: Bird Migration Data Filtering
migration_data <- data.frame(
  date = rep(seq(as.Date("2023-03-01"), as.Date("2023-05-31"), 
                 by = "day"), 3),
  location = rep(c("North_Station", "Central_Station", 
                   "South_Station"), each = 92),
  species = sample(c("Turdus_migratorius", "Hirundo_rustica", 
                     "Dendroica_petechia", "Vireo_olivaceus"), 
                   276, replace = TRUE),
  count = rpois(276, 8),
  weather = sample(c("Clear", "Cloudy", "Rainy", "Windy"), 
                   276, replace = TRUE),
  wind_speed = runif(276, 0, 25),
  temperature = runif(276, 5, 20)
)

# Filter peak migration with good weather conditions
peak_migration <- migration_data %>%
  filter(lubridate::month(date) == 4,
         weather %in% c("Clear", "Cloudy"),
         wind_speed < 15) %>%
  select(date, location, species, count, temperature) %>%
  arrange(date, location)

# Focus on specific species during migration
swallow_migration <- migration_data %>%
  filter(species == "Hirundo_rustica",
         count > 5) %>%
  select(date, location, count, weather, wind_speed)

# Example 4: Soil Microbe Community Selection
microbe_data <- data.frame(
  plot_id = rep(paste0("Plot_", 1:8), each = 20),
  treatment = rep(c("Control", "Fertilized"), each = 80),
  phylum = rep(c("Proteobacteria", "Acidobacteria", 
                 "Actinobacteria", "Bacteroidetes", "Firmicutes"), 
               32),
  genus = paste0("Genus_", 1:160),
  abundance = rpois(160, 50),
  diversity_index = runif(160, 0.5, 3.5),
  soil_ph = rep(runif(8, 5.5, 7.5), each = 20),
  organic_matter = rep(runif(8, 2, 8), each = 20)
)

# Filter high-abundance taxa in control plots
abundant_control <- microbe_data %>%
  filter(treatment == "Control",
         abundance > 40,
         diversity_index > 2.0) %>%
  select(plot_id, phylum, genus, abundance, soil_ph) %>%
  arrange(desc(abundance))

# Select core microbiome across treatments
core_microbiome <- microbe_data %>%
  filter(phylum %in% c("Proteobacteria", "Acidobacteria"),
         abundance > 30) %>%
  select(plot_id, treatment, phylum, abundance, organic_matter)

# Example 5: Pollinator Network Filtering
pollinator_data <- data.frame(
  site = rep(c("Meadow_1", "Meadow_2", "Forest_Edge"), each = 30),
  flower_species = rep(paste0("Plant_", 1:10), 9),
  pollinator_species = sample(paste0("Pollinator_", 1:15), 
                              90, replace = TRUE),
  interaction_frequency = rpois(90, 12),
  flower_abundance = rep(runif(30, 10, 100), 3),
  pollinator_size = runif(90, 2, 25),
  flowering_period = sample(c("Early", "Mid", "Late"), 
                            90, replace = TRUE)
)

# Filter frequent interactions during peak season
frequent_interactions <- pollinator_data %>%
  filter(interaction_frequency >= 10,
         flowering_period %in% c("Mid", "Late"),
         flower_abundance > 25) %>%
  select(site, flower_species, pollinator_species, 
         interaction_frequency) %>%
  arrange(desc(interaction_frequency))

# Select large pollinators for specialization analysis
large_pollinators <- pollinator_data %>%
  filter(pollinator_size > 15,
         site != "Forest_Edge") %>%
  select(site, pollinator_species, pollinator_size, 
         interaction_frequency)

################################################################################
################ Data Filtering & Selection Exercises #######################
################################################################################

# Exercise 1: Marine Fish Survey Filtering
# Dataset: commercial fish survey with size and abundance data
marine_fish <- data.frame(
  species = rep(c("Cod", "Haddock", "Flounder", "Sole"), each = 25),
  length_cm = runif(100, 15, 80),
  weight_kg = runif(100, 0.2, 8.5),
  depth_m = sample(10:200, 100, replace = TRUE),
  location = rep(c("North", "South", "East", "West"), 25),
  age_years = sample(1:12, 100, replace = TRUE)
)

# Filter commercial-size fish above legal limits
# Select key biological measurements for analysis
# Arrange by biomass and focus on specific locations

# Exercise 2: Plant Trait Database Selection
# Dataset: functional traits for different plant species
plant_traits <- data.frame(
  species = rep(paste0("Species_", 1:20), each = 3),
  trait_type = rep(c("Leaf_area", "Seed_mass", "Height"), 20),
  value = runif(60, 0.1, 100),
  biome = rep(c("Temperate", "Tropical", "Boreal"), 20),
  life_form = rep(c("Tree", "Shrub", "Herb"), 20),
  drought_tolerance = sample(c("Low", "Medium", "High"), 60, 
                             replace = TRUE)
)

# Filter perennial plants from temperate regions
# Select functional traits for drought adaptation analysis
# Focus on woody species with high drought tolerance

# Exercise 3: Camera Trap Detection Processing
# Dataset: wildlife camera observations with temporal data
camera_detections <- data.frame(
  species = sample(c("Deer", "Bear", "Wolf", "Lynx", "Fox"), 
                   150, replace = TRUE),
  detection_time = sample(0:23, 150, replace = TRUE),
  date = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), 
                    by = "day"), 150, replace = TRUE),
  camera_id = rep(paste0("CAM_", 1:10), 15),
  activity_level = sample(c("Low", "Medium", "High"), 150, 
                          replace = TRUE)
)

# Filter daytime observations of large mammals
# Select detection variables for temporal analysis
# Arrange by activity intensity and focus on peak periods

# Exercise 4: Streamflow Habitat Assessment
# Dataset: hydrological measurements for fish habitat
streamflow_data <- data.frame(
  date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), 
             by = "day"),
  discharge_cms = runif(365, 0.5, 15.2),
  temperature_c = runif(365, 2, 22),
  velocity_ms = runif(365, 0.1, 2.5),
  substrate_type = sample(c("Gravel", "Sand", "Bedrock", "Silt"), 
                          365, replace = TRUE),
  season = rep(c("Winter", "Spring", "Summer", "Autumn"), 
               c(90, 92, 92, 91))
)

# Filter spawning season conditions for fish habitat
# Select hydrological parameters critical for reproduction
# Focus on optimal temperature and flow combinations

# Exercise 5: Insect Diversity Conservation Priority
# Dataset: insect survey data with conservation status
insect_survey <- data.frame(
  species = paste0("Insect_", 1:80),
  abundance = rpois(80, 25),
  habitat_specificity = sample(c("Generalist", "Specialist"), 
                               80, replace = TRUE),
  conservation_status = sample(c("Common", "Rare", "Endangered"), 
                               80, replace = TRUE, 
                               prob = c(0.6, 0.3, 0.1)),
  habitat_type = rep(c("Forest", "Grassland", "Wetland", "Urban"), 20),
  body_size_mm = runif(80, 2, 45)
)

# Filter rare species with specific habitat requirements
# Select taxonomic and abundance data for analysis
# Arrange by conservation priority for management planning

################################################################################
####################### Data Summarisation & Grouping #######################
################################################################################

# Example 1: Species Richness and Abundance Summary
community_data <- data.frame(
  site = rep(c("Grassland", "Forest", "Wetland", "Shrubland"), 
             each = 25),
  species = paste0("Species_", rep(1:20, 5)),
  abundance = c(rpois(25, 15), rpois(25, 12), rpois(25, 18), 
                rpois(25, 10)),
  biomass_g = runif(100, 5, 50),
  trophic_level = sample(c("Producer", "Primary", "Secondary", 
                           "Tertiary"), 100, replace = TRUE)
)

# Summarise community metrics by site
community_summary <- community_data %>%
  group_by(site) %>%
  summarise(
    species_richness = n_distinct(species),
    total_abundance = sum(abundance),
    mean_abundance = mean(abundance),
    total_biomass = sum(biomass_g),
    mean_biomass = mean(biomass_g),
    shannon_diversity = -sum((abundance/sum(abundance)) * 
                               log(abundance/sum(abundance))),
    .groups = 'drop'
  ) %>%
  arrange(desc(species_richness))

# Summary by trophic level within sites
trophic_summary <- community_data %>%
  group_by(site, trophic_level) %>%
  summarise(
    n_species = n_distinct(species),
    avg_abundance = mean(abundance),
    total_biomass = sum(biomass_g),
    .groups = 'drop'
  ) %>%
  arrange(site, trophic_level)

# Example 2: Climate Data Seasonal Summaries
climate_yearly <- data.frame(
  date = seq(as.Date("2020-01-01"), as.Date("2023-12-31"), 
             by = "day"),
  station = rep(c("Mountain", "Valley", "Coastal"), 
                length.out = 1461),
  temperature = runif(1461, -5, 35),
  precipitation = rexp(1461, 1/25),
  humidity = runif(1461, 30, 95),
  wind_speed = runif(1461, 0, 20)
) %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn"
    )
  )

# Seasonal climate summaries by station
seasonal_climate <- climate_yearly %>%
  group_by(station, season) %>%
  summarise(
    mean_temp = mean(temperature),
    min_temp = min(temperature),
    max_temp = max(temperature),
    total_precip = sum(precipitation),
    mean_humidity = mean(humidity),
    max_wind = max(wind_speed),
    extreme_temp_days = sum(temperature < 0 | temperature > 30),
    .groups = 'drop'
  )

# Annual climate trends
annual_trends <- climate_yearly %>%
  group_by(station, year) %>%
  summarise(
    annual_temp = mean(temperature),
    annual_precip = sum(precipitation),
    growing_days = sum(temperature > 5),
    frost_days = sum(temperature < 0),
    .groups = 'drop'
  )

# Example 3: Forest Growth Analysis
forest_inventory <- data.frame(
  plot = rep(paste0("Plot_", 1:10), each = 30),
  species = rep(c("Oak", "Pine", "Maple"), 100),
  year_planted = rep(sample(1990:2010, 10, replace = TRUE), 
                     each = 30),
  dbh_2020 = runif(300, 5, 25),
  dbh_2023 = runif(300, 8, 30),
  height_2020 = runif(300, 3, 15),
  height_2023 = runif(300, 4, 18),
  mortality = sample(c(0, 1), 300, replace = TRUE, 
                     prob = c(0.9, 0.1))
) %>%
  mutate(
    age_2023 = 2023 - year_planted,
    dbh_growth = dbh_2023 - dbh_2020,
    height_growth = height_2023 - height_2020
  )

# Growth summary by species and age class
growth_summary <- forest_inventory %>%
  filter(mortality == 0) %>%
  mutate(age_class = case_when(
    age_2023 < 15 ~ "Young",
    age_2023 >= 15 & age_2023 < 25 ~ "Mature",
    age_2023 >= 25 ~ "Old"
  )) %>%
  group_by(species, age_class) %>%
  summarise(
    n_trees = n(),
    mean_dbh_growth = mean(dbh_growth),
    mean_height_growth = mean(height_growth),
    survival_rate = mean(mortality == 0),
    max_dbh = max(dbh_2023),
    .groups = 'drop'
  )

# Plot-level forest metrics
plot_metrics <- forest_inventory %>%
  group_by(plot) %>%
  summarise(
    tree_density = n(),
    basal_area = sum(pi * (dbh_2023/200)^2),
    avg_height = mean(height_2023),
    species_diversity = n_distinct(species),
    mortality_rate = mean(mortality),
    total_growth = sum(dbh_growth + height_growth),
    .groups = 'drop'
  )

# Example 4: Aquatic Ecosystem Assessment
stream_data <- data.frame(
  stream_id = rep(paste0("Stream_", 1:6), each = 40),
  sampling_date = rep(seq(as.Date("2023-01-01"), 
                          as.Date("2023-10-01"), by = "month"), 
                      24),
  temperature = runif(240, 8, 22),
  dissolved_oxygen = runif(240, 6, 14),
  pH = runif(240, 6.8, 8.2),
  nitrate = runif(240, 0.5, 3.5),
  phosphate = runif(240, 0.1, 1.2),
  macroinvertebrate_taxa = rpois(240, 12),
  fish_species = rpois(240, 6),
  pollution_index = runif(240, 1, 5)
) %>%
  mutate(
    season = case_when(
      lubridate::month(sampling_date) %in% c(3, 4, 5) ~ "Spring",
      lubridate::month(sampling_date) %in% c(6, 7, 8) ~ "Summer",
      lubridate::month(sampling_date) %in% c(9, 10, 11) ~ "Autumn",
      TRUE ~ "Winter"
    ),
    water_quality = case_when(
      pollution_index < 2 ~ "Excellent",
      pollution_index < 3 ~ "Good",
      pollution_index < 4 ~ "Fair",
      TRUE ~ "Poor"
    )
  )

# Stream health assessment by season
stream_health <- stream_data %>%
  group_by(stream_id, season) %>%
  summarise(
    avg_temperature = mean(temperature),
    avg_dissolved_oxygen = mean(dissolved_oxygen),
    avg_pH = mean(pH),
    biodiversity_score = mean(macroinvertebrate_taxa + 
                                fish_species),
    water_quality_score = mean(pollution_index),
    .groups = 'drop'
  )

# Overall stream comparison
stream_comparison <- stream_data %>%
  group_by(stream_id) %>%
  summarise(
    n_samples = n(),
    temperature_range = max(temperature) - min(temperature),
    oxygen_stability = sd(dissolved_oxygen),
    nutrient_load = mean(nitrate + phosphate),
    biodiversity_index = mean(macroinvertebrate_taxa * 
                                fish_species),
    quality_rating = mean(as.numeric(factor(water_quality))),
    .groups = 'drop'
  )

# Example 5: Pollination Network Analysis
pollination_network <- data.frame(
  site = rep(c("Urban", "Suburban", "Rural"), each = 60),
  plant_species = rep(paste0("Plant_", 1:15), 12),
  pollinator_group = rep(c("Bees", "Butterflies", "Flies", 
                           "Beetles"), 45),
  visit_frequency = rpois(180, 25),
  pollen_load = runif(180, 0, 100),
  flower_density = runif(180, 5, 50),
  pollinator_abundance = rpois(180, 15),
  successful_visits = rbinom(180, 25, 0.7)
) %>%
  mutate(
    visit_efficiency = successful_visits / visit_frequency,
    pollen_per_visit = pollen_load / visit_frequency
  )

# Pollination effectiveness by site and group
pollination_summary <- pollination_network %>%
  group_by(site, pollinator_group) %>%
  summarise(
    total_visits = sum(visit_frequency),
    mean_efficiency = mean(visit_efficiency),
    total_pollen_transferred = sum(pollen_load),
    pollinator_diversity = n_distinct(plant_species),
    success_rate = mean(successful_visits / visit_frequency),
    .groups = 'drop'
  )

# Plant-pollinator network metrics
network_metrics <- pollination_network %>%
  group_by(site, plant_species) %>%
  summarise(
    pollinator_richness = n_distinct(pollinator_group),
    total_visits_received = sum(visit_frequency),
    specialization_index = 1 / n_distinct(pollinator_group),
    reproduction_success = sum(successful_visits),
    .groups = 'drop'
  ) %>%
  arrange(site, desc(total_visits_received))

################################################################################
############# Data Summarisation & Grouping Exercises #######################
################################################################################

# Exercise 1: Bird Community Analysis by Habitat
# Dataset: bird survey data across different habitat types
bird_community <- data.frame(
  habitat = rep(c("Forest", "Grassland", "Wetland", "Urban"), each = 50),
  species = rep(paste0("Bird_", 1:25), 8),
  abundance = rpois(200, 12),
  body_mass_g = runif(200, 5, 150),
  migration_status = sample(c("Resident", "Migrant"), 200, 
                            replace = TRUE),
  breeding_success = runif(200, 0, 1)
)

# Group by habitat and migration status
# Summarize species richness, abundance, and biomass metrics
# Calculate breeding success rates by habitat type

# Exercise 2: Vegetation Growth Response to Treatments
# Dataset: plant growth experiment with different treatments
vegetation_experiment <- data.frame(
  treatment = rep(c("Control", "Fertilized", "Irrigated", "Both"), 
                  each = 40),
  species = rep(c("Grass", "Forb", "Shrub", "Tree"), 40),
  year = c(rep(2021, 40), rep(2022, 40), rep(2023, 40), rep(2024, 40)),
  height_cm = runif(160, 10, 80),
  biomass_g = runif(160, 2, 45),
  survival_rate = runif(160, 0.6, 1.0)
)

# Group by treatment and year
# Calculate growth rates, survival percentages, and productivity
# Summarize treatment effectiveness across species types

# Exercise 3: Soil Chemistry Analysis by Land Use
# Dataset: soil measurements from different land management practices
soil_analysis <- data.frame(
  land_use = rep(c("Forest", "Agriculture", "Pasture", "Urban"), 
                 each = 30),
  depth_cm = rep(c(0, 10, 20), 40),
  nitrogen_ppm = runif(120, 20, 80),
  phosphorus_ppm = runif(120, 5, 25),
  pH = runif(120, 5.5, 8.0),
  organic_matter_percent = runif(120, 1, 8)
)

# Group by land use and depth
# Summarize nutrient concentrations and pH levels
# Calculate soil quality indices by management type

# Exercise 4: Fish Population Demographics
# Dataset: fish survey with age and size structure data
fish_population <- data.frame(
  species = rep(c("Trout", "Bass", "Pike"), each = 60),
  water_body = rep(c("Lake_A", "Lake_B", "River_C"), 60),
  age_years = sample(1:8, 180, replace = TRUE),
  length_cm = runif(180, 15, 65),
  weight_g = runif(180, 50, 2000),
  reproductive_stage = sample(c("Juvenile", "Adult", "Spawning"), 
                              180, replace = TRUE)
)

# Group by species and water body
# Calculate population metrics and age structure
# Summarize recruitment rates and biomass by location

# Exercise 5: Phenology Summary Across Climate Zones
# Dataset: plant phenology observations from different regions
phenology_data <- data.frame(
  species = rep(paste0("Plant_", 1:12), each = 20),
  climate_zone = rep(c("Cool", "Moderate", "Warm"), 80),
  year = rep(2019:2023, 48),
  flowering_day = sample(90:180, 240, replace = TRUE),
  fruit_day = sample(200:280, 240, replace = TRUE),
  temperature_spring = runif(240, 8, 18)
)

# Group by species and climate zone
# Summarize timing of life cycle events
# Calculate temperature thresholds for phenological stages
