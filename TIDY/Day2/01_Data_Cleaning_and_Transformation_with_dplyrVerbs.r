####################################################################################################
###
### File:    01_Data_Cleaning_and_Transformation_with_dplyrVerbs.R
### Purpose: Examples and exercises for Data Cleaning and Transformation with dplyr Verbs
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
###################### Data Cleaning & dplyr Verbs ##########################
################################################################################

# Example 1: Species Survey Data Cleaning
bird_survey <- data.frame(
  species = c("Turdus migratorius", "parus major", "CORVUS CORVUS", 
              "turdus migratorius", "Parus major"),
  count = c(5, 3, 2, 7, 4),
  site = c("Forest_A", "Forest_A", "Forest_B", "Forest_A", "Forest_B")
)

# Clean species names to standardized format
bird_survey_clean <- bird_survey %>%
  mutate(species = stringr::str_to_title(species)) %>%
  mutate(species = case_when(
    species == "Turdus Migratorius" ~ "Turdus migratorius",
    species == "Parus Major" ~ "Parus major", 
    species == "Corvus Corvus" ~ "Corvus corvus",
    TRUE ~ species
  ))

# Example 2: Forest Inventory Data Transformation
forest_data <- data.frame(
  plot_id = 1:6,
  dbh_cm = c(15.2, 22.1, 18.7, 25.3, 12.4, 31.2),
  height_m = c(8.5, 12.3, 9.8, 14.2, 6.7, 18.1),
  species = c("Quercus", "Pinus", "Quercus", "Pinus", "Fagus", "Quercus")
)

# Calculate basal area and volume estimates
forest_processed <- forest_data %>%
  mutate(dbh_m = dbh_cm / 100) %>%
  mutate(basal_area_m2 = pi * (dbh_m/2)^2) %>%
  mutate(volume_m3 = basal_area_m2 * height_m * 0.45)

# Example 3: Water Quality Data Classification
water_quality <- data.frame(
  station = rep(c("Upstream", "Downstream"), each = 10),
  pH = c(7.2, 7.1, 7.3, 6.9, 7.0, 6.8, 6.5, 6.7, 6.4, 6.6,
         6.9, 7.0, 7.1, 6.8, 6.9, 7.2, 7.0, 6.8, 6.9, 7.1),
  dissolved_oxygen = c(8.5, 8.2, 8.7, 8.1, 8.3, 7.9, 7.5, 7.8,
                       7.2, 7.6, 8.0, 8.1, 8.3, 7.8, 8.0, 8.2,
                       8.1, 7.9, 8.0, 8.3)
)

# Classify water quality based on pH ranges
water_classified <- water_quality %>%
  mutate(pH_category = case_when(
    pH < 6.5 ~ "Acidic",
    pH >= 6.5 & pH <= 7.5 ~ "Neutral",
    pH > 7.5 ~ "Alkaline"
  )) %>%
  mutate(quality_index = (pH - 6.5) * dissolved_oxygen / 10)

# Example 4: Plant Phenology Processing
phenology <- data.frame(
  species = rep(c("Acer saccharum", "Betula papyrifera"), each = 8),
  year = rep(2020:2023, 4),
  leaf_out_doy = c(125, 118, 130, 122, 115, 108, 125, 120,
                   110, 105, 115, 112, 100, 95, 110, 108),
  leaf_drop_doy = c(285, 290, 280, 288, 275, 282, 278, 285,
                    270, 275, 265, 272, 260, 268, 263, 270)
)

# Calculate growing season metrics
phenology_processed <- phenology %>%
  mutate(growing_season_days = leaf_drop_doy - leaf_out_doy) %>%
  mutate(leaf_out_month = case_when(
    leaf_out_doy <= 90 ~ "March",
    leaf_out_doy <= 120 ~ "April", 
    leaf_out_doy <= 151 ~ "May",
    TRUE ~ "June"
  ))

# Example 5: Soil Nutrient Classification
soil_data <- data.frame(
  plot = paste0("Plot_", 1:12),
  nitrogen_ppm = c(45, 52, 38, 61, 47, 55, 42, 58, 49, 53, 44, 59),
  phosphorus_ppm = c(12, 15, 8, 18, 13, 16, 10, 17, 14, 16, 11, 18),
  organic_matter_percent = c(3.2, 4.1, 2.8, 5.2, 3.5, 4.3, 
                             3.0, 4.8, 3.7, 4.5, 3.1, 5.0)
)

# Create nutrient availability categories
soil_classified <- soil_data %>%
  mutate(n_level = case_when(
    nitrogen_ppm < 40 ~ "Low",
    nitrogen_ppm >= 40 & nitrogen_ppm < 55 ~ "Medium",
    nitrogen_ppm >= 55 ~ "High"
  )) %>%
  mutate(fertility_score = (nitrogen_ppm * 0.4) + 
           (phosphorus_ppm * 0.3) + 
           (organic_matter_percent * 10 * 0.3))

################################################################################
################## Data Cleaning & dplyr Verbs Exercises #####################
################################################################################

# Exercise 1: Marine Species Data Cleaning
# Dataset provided: messy marine survey data
marine_survey <- data.frame(
  species = c("homarus americanus", "CALLINECTES SAPIDUS", 
              "Homarus Americanus", "callinectes sapidus"),
  length_mm = c(285, 156, 298, 142),
  weight_g = c(580, 125, 635, 108),
  location = c("Bay_A", "Bay_A", "Bay_B", "Bay_B")
)

# Clean the species names and create size categories
# Add a column for biomass density calculations

# Exercise 2: Vegetation Survey Processing
# Dataset: plant height measurements with mixed units
vegetation_data <- data.frame(
  plot_id = 1:10,
  species = c("Quercus alba", "Pinus strobus", "Acer rubrum", 
              "Quercus alba", "Fagus grandifolia", "Pinus strobus",
              "Acer rubrum", "Fagus grandifolia", "Quercus alba", 
              "Pinus strobus"),
  height_measurement = c("2.5m", "450cm", "3.2m", "380cm", "2.8m",
                         "520cm", "2.1m", "290cm", "4.1m", "680cm"),
  coverage_percent = c(15, 25, 18, 22, 30, 35, 12, 28, 45, 38)
)

# Convert all heights to meters and create height classes
# Calculate coverage categories (Low: <20%, Medium: 20-35%, High: >35%)

# Exercise 3: Stream Temperature Data Transformation
# Dataset: daily temperature readings with seasonal analysis needed
stream_temp <- data.frame(
  date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
  temperature_c = c(rep(c(2, 3, 1, 4, 2), 73)),
  flow_rate = runif(365, 0.5, 5.2),
  site = rep(c("Upstream", "Downstream"), c(183, 182))
)

# Add season classification and temperature categories
# Create thermal stress indicators

# Exercise 4: Bird Migration Data Processing
# Dataset: bird count data with date formatting needed
migration_data <- data.frame(
  observation_date = c("2023-04-15", "2023-04-20", "2023-05-01", 
                       "2023-05-15", "2023-04-25", "2023-05-10"),
  species = c("Hirundo rustica", "Turdus migratorius", 
              "Dendroica petechia", "Hirundo rustica", 
              "Turdus migratorius", "Dendroica petechia"),
  count = c(45, 23, 67, 38, 19, 82),
  weather = c("Clear", "Cloudy", "Clear", "Rainy", "Clear", "Windy")
)

# Add migration timing categories and abundance classifications
# Calculate species migration patterns

# Exercise 5: Soil Chemistry Data Standardization
# Dataset: soil measurements from different labs with varying units
soil_chemistry <- data.frame(
  sample_id = paste0("S", 1:8),
  pH = c(6.2, 7.1, 5.8, 6.9, 7.3, 6.5, 7.8, 6.1),
  nitrogen_percent = c(0.15, 0.22, 0.08, 0.19, 0.31, 0.12, 0.28, 0.09),
  carbon_content = c("3.2%", "4.8%", "2.1%", "3.9%", "5.5%", 
                     "2.8%", "4.2%", "1.9%")
)

# Convert carbon content to numeric and create soil quality indices
# Add pH buffering capacity categories

################################################################################
################ Advanced Data Transformation Techniques ####################
################################################################################

# Example 1: Biodiversity Index Calculations
diversity_data <- data.frame(
  site = rep(c("Grassland", "Forest", "Wetland"), each = 5),
  species = rep(c("Species_A", "Species_B", "Species_C", 
                  "Species_D", "Species_E"), 3),
  abundance = c(25, 15, 8, 12, 5,   # Grassland
                18, 22, 14, 9, 7,   # Forest  
                12, 8, 20, 15, 10)  # Wetland
)

# Calculate Shannon diversity components
diversity_processed <- diversity_data %>%
  group_by(site) %>%
  mutate(total_abundance = sum(abundance)) %>%
  mutate(proportion = abundance / total_abundance) %>%
  mutate(ln_proportion = log(proportion)) %>%
  mutate(pi_ln_pi = proportion * ln_proportion) %>%
  ungroup() %>%
  mutate(rarity_score = 1 / abundance)

# Example 2: Climate Data Standardization
climate_data <- data.frame(
  station = rep(c("Mountain", "Valley", "Coastal"), each = 12),
  month = rep(1:12, 3),
  temp_celsius = c(-2, 0, 5, 12, 18, 24, 26, 25, 20, 13, 6, 1,
                   8, 10, 15, 22, 28, 34, 36, 35, 30, 23, 16, 11,
                   15, 16, 18, 21, 24, 27, 28, 28, 26, 23, 20, 17),
  precipitation_mm = c(45, 52, 68, 85, 95, 110, 85, 75, 65, 55, 
                       48, 42, 25, 28, 35, 45, 55, 65, 45, 35, 
                       30, 25, 22, 20, 85, 88, 92, 95, 98, 102, 
                       88, 82, 78, 75, 72, 80)
)

# Create seasonal categories and growing degree days
climate_processed <- climate_data %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2) ~ "Winter",
    month %in% c(3, 4, 5) ~ "Spring", 
    month %in% c(6, 7, 8) ~ "Summer",
    month %in% c(9, 10, 11) ~ "Autumn"
  )) %>%
  mutate(growing_degree_days = ifelse(temp_celsius > 5, 
                                      temp_celsius - 5, 0)) %>%
  mutate(temp_range = case_when(
    temp_celsius < 10 ~ "Cold",
    temp_celsius >= 10 & temp_celsius < 25 ~ "Moderate",
    temp_celsius >= 25 ~ "Warm"
  ))

# Example 3: Population Growth Rate Calculations
population_data <- data.frame(
  year = rep(2015:2024, 3),
  population = rep(c("Population_A", "Population_B", 
                     "Population_C"), each = 10),
  count = c(150, 165, 145, 180, 170, 185, 160, 175, 155, 190,
            85, 92, 78, 105, 98, 110, 88, 102, 85, 115,
            220, 240, 210, 265, 255, 280, 235, 270, 225, 295)
)

# Calculate population growth metrics
population_processed <- population_data %>%
  group_by(population) %>%
  arrange(year) %>%
  mutate(growth_rate = (count - lag(count)) / lag(count)) %>%
  mutate(cumulative_change = (count - first(count)) / 
           first(count)) %>%
  mutate(population_trend = case_when(
    growth_rate > 0.05 ~ "Increasing",
    growth_rate < -0.05 ~ "Decreasing", 
    TRUE ~ "Stable"
  )) %>%
  ungroup()

# Example 4: Habitat Quality Assessment
habitat_data <- data.frame(
  site_id = paste0("Site_", 1:15),
  canopy_cover = c(85, 72, 90, 68, 75, 82, 78, 88, 70, 80, 
                   74, 86, 79, 83, 77),
  understory_density = c(45, 38, 52, 35, 42, 48, 40, 50, 36, 
                         46, 41, 49, 43, 47, 39),
  dead_wood_volume = c(12, 8, 15, 6, 10, 14, 9, 16, 7, 13, 
                       11, 17, 12, 14, 8),
  edge_distance_m = c(50, 120, 25, 200, 80, 35, 150, 15, 180, 
                      45, 90, 20, 160, 40, 110)
)

# Create composite habitat quality scores
habitat_processed <- habitat_data %>%
  mutate(canopy_score = case_when(
    canopy_cover >= 80 ~ 3,
    canopy_cover >= 70 ~ 2, 
    TRUE ~ 1
  )) %>%
  mutate(structure_score = (understory_density / 20) + 
           (dead_wood_volume / 10)) %>%
  mutate(edge_effect = ifelse(edge_distance_m < 100, 
                              "High", "Low")) %>%
  mutate(habitat_quality = (canopy_score * 0.4) + 
           (structure_score * 0.4) + 
           (ifelse(edge_effect == "Low", 1, 0.5) * 0.2))

# Example 5: Species Occurrence Probability
occurrence_data <- data.frame(
  location = paste0("Loc_", 1:20),
  elevation_m = c(450, 680, 320, 890, 560, 720, 380, 940, 510, 
                  800, 420, 650, 290, 760, 580, 690, 350, 820, 
                  480, 740),
  temperature_avg = c(18.5, 15.2, 21.3, 12.8, 16.9, 14.1, 
                      20.2, 11.5, 17.8, 13.4, 19.1, 15.8, 22.1, 
                      14.6, 16.3, 14.8, 20.8, 13.0, 18.1, 14.3),
  rainfall_mm = c(1200, 1450, 950, 1680, 1320, 1520, 1050, 
                  1750, 1280, 1590, 1100, 1380, 890, 1480, 
                  1350, 1500, 1020, 1620, 1240, 1510)
)

# Calculate species suitability indices
occurrence_processed <- occurrence_data %>%
  mutate(elevation_zone = case_when(
    elevation_m < 500 ~ "Lowland",
    elevation_m >= 500 & elevation_m < 750 ~ "Montane",
    elevation_m >= 750 ~ "Alpine"
  )) %>%
  mutate(temp_suitability = exp(-((temperature_avg - 16)^2) / 
                                  (2 * 3^2))) %>%
  mutate(rainfall_optimal = abs(rainfall_mm - 1300) / 1300) %>%
  mutate(habitat_suitability = temp_suitability * 
           (1 - rainfall_optimal))

################################################################################
########### Advanced Data Transformation Exercises ##########################
################################################################################

# Exercise 1: Fish Community Analysis
# Dataset: fish length-weight measurements for biomass calculations
fish_data <- data.frame(
  species = rep(c("Salmo trutta", "Perca fluviatilis", "Esox lucius"), 
                each = 15),
  length_cm = c(runif(15, 20, 45), runif(15, 15, 35), runif(15, 30, 80)),
  weight_g = c(runif(15, 150, 800), runif(15, 80, 400), 
               runif(15, 400, 2500)),
  age_years = c(sample(2:6, 15, replace = TRUE), 
                sample(1:4, 15, replace = TRUE),
                sample(3:8, 15, replace = TRUE))
)

# Calculate biomass from length-weight relationships
# Create trophic level categories and age class groupings
# Compute condition factor (weight/length^3 * 100)

# Exercise 2: Plant Growth Performance Analysis
# Dataset: seedling growth measurements over time
seedling_data <- data.frame(
  plant_id = rep(paste0("P", 1:20), each = 4),
  week = rep(c(2, 4, 6, 8), 20),
  height_mm = runif(80, 15, 120),
  leaf_count = sample(4:25, 80, replace = TRUE),
  treatment = rep(c("Control", "Fertilized", "Watered", "Both"), 20)
)

# Calculate relative growth rates between measurements
# Create performance categories and treatment response indices
# Determine growth pattern classifications

# Exercise 3: Pollination Network Metrics
# Dataset: plant-pollinator interaction frequencies
pollination_data <- data.frame(
  plant_species = rep(paste0("Plant_", 1:8), each = 12),
  pollinator_type = rep(c("Bee", "Butterfly", "Fly"), 32),
  interaction_freq = rpois(96, 15),
  flower_abundance = rep(runif(8, 10, 100), each = 12),
  pollinator_size = runif(96, 2, 25)
)

# Calculate interaction strength relative to abundance
# Create specialization indices and network position metrics
# Determine keystone species based on interaction patterns

# Exercise 4: Microbial Community Processing
# Dataset: 16S rRNA sequencing abundance data
microbe_data <- data.frame(
  sample_id = rep(paste0("Sample_", 1:12), each = 20),
  phylum = rep(c("Proteobacteria", "Acidobacteria", "Actinobacteria", 
                 "Bacteroidetes"), 60),
  genus = paste0("Genus_", 1:240),
  read_count = rpois(240, 150),
  soil_ph = rep(runif(12, 5.5, 7.5), each = 20)
)

# Transform counts to relative abundances
# Calculate alpha diversity metrics per sample
# Create pH response categories for each genus

# Exercise 5: Camera Trap Activity Analysis
# Dataset: wildlife detection timestamps
camera_trap <- data.frame(
  species = rep(c("Deer", "Bear", "Wolf", "Lynx"), each = 30),
  detection_time = sample(0:23, 120, replace = TRUE),
  date = rep(seq(as.Date("2023-06-01"), as.Date("2023-08-30"), 
                 by = "day"), length.out = 120),
  camera_site = rep(paste0("Site_", 1:6), 20)
)

# Create activity period categories (dawn, day, dusk, night)
# Calculate detection rates per species per site
# Determine temporal overlap indices between species
