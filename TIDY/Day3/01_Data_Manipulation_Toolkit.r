####################################################################################################
###
### File:    01_Data_Manipulation_Toolkit.R
### Purpose: Examples and exercises for Data manipulation using
###         dplyr, tibble, tydyr and Readr
### Authors: Gabriel Rodrigues Palma
### Date:    19/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
######################## Data Manipulation Toolkit - Part 1 ###################
################################################################################
# Reading species abundance data from CSV
species_data <- read_csv("species_abundance.csv", 
                         col_types = cols(
                           site_id = col_character(),
                           species = col_character(),
                           abundance = col_double(),
                           biomass = col_double()
                         ))

# Example 2: Creating Tibbles for Marine Phytoplankton Data
phytoplankton <- tibble(
  species = c("Diatoma vulgaris", "Alexandrium tamarense", 
              "Skeletonema costatum"),
  cell_count = c(1250, 340, 2890),
  biomass_mg_l = c(0.45, 0.23, 0.78),
  habitat = c("coastal", "oceanic", "coastal")
)

# Example 3: Using dplyr filter() for Species Selection
forest_species <- tibble(
  species = c("Quercus robur", "Fagus sylvatica", "Pinus sylvestris",
              "Betula pendula", "Acer pseudoplatanus"),
  dbh_cm = c(45.2, 38.7, 52.1, 23.4, 41.8),
  height_m = c(18.5, 16.2, 22.3, 14.1, 17.9),
  age_years = c(85, 72, 95, 42, 68)
) %>%
  filter(dbh_cm > 40, height_m > 15)

# Example 4: Using select() and mutate() for Biodiversity Calculations
bird_census <- tibble(
  site = paste0("Site_", 1:10),
  species_richness = c(12, 8, 15, 9, 11, 13, 7, 16, 10, 14),
  total_individuals = c(245, 178, 312, 189, 223, 267, 156, 338, 201, 289)
) %>%
  select(site, species_richness, total_individuals) %>%
  mutate(shannon_diversity = log(species_richness),
         abundance_per_species = total_individuals / species_richness)

# Example 5: Using arrange() and summarise() for Ecological Ranking
soil_microbes <- tibble(
  site_type = rep(c("forest", "grassland", "wetland"), each = 4),
  microbial_biomass = c(245, 267, 231, 289, 178, 195, 167, 203, 
                        312, 334, 298, 356),
  soil_ph = c(6.2, 6.8, 5.9, 7.1, 7.3, 7.6, 7.0, 7.8, 
              5.4, 5.8, 5.1, 6.0)
) %>%
  arrange(desc(microbial_biomass)) %>%
  group_by(site_type) %>%
  summarise(mean_biomass = mean(microbial_biomass),
            mean_ph = mean(soil_ph),
            .groups = 'drop')

################################################################################
################## Data Manipulation Toolkit - Exercises 1 ####################
################################################################################
# Exercise 1: Compare a tibble and a data frame for marine ecosystem 
#CTD survey profiles. Use 8 stations (ST01-ST08) with generated values.
marine_ctd_tibble <- tibble(
  station_id = sprintf("ST%02d", 1:8),
  depth_m = c(10, 25, 40, 15, 33, 50, 28, 18),
  temperature_c = c(12.4, 14.3, 10.7, 11.2, 13.6, 9.5, 12.9, 13.1),
  salinity_ppt = c(34.8, 35.1, 34.0, 34.9, 35.2, 34.3, 35.0, 34.7),
  oxygen_mg_l = c(7.2, 6.8, 8.1, 7.5, 7.0, 8.4, 6.6, 7.4)
)
marine_ctd_dataframe <- data.frame(
  station_id = sprintf("ST%02d", 1:8),
  depth_m = c(10, 25, 40, 15, 33, 50, 28, 18),
  temperature_c = c(12.4, 14.3, 10.7, 11.2, 13.6, 9.5, 12.9, 13.1),
  salinity_ppt = c(34.8, 35.1, 34.0, 34.9, 35.2, 34.3, 35.0, 34.7),
  oxygen_mg_l = c(7.2, 6.8, 8.1, 7.5, 7.0, 8.4, 6.6, 7.4)
)
# Task: Print summary statistics for temperature_c and oxygen_mg_l


# Exercise 2: Read and filter coral reef fish community tibble
# Identify all herbivorous species with body length > 10 cm.

coral_fish <- tibble(
  species = c("Acanthurus triostegus", "Zebrasoma scopas", "Chaetodon lunula",
              "Ctenochaetus striatus", "Siganus vulpinus", "Lutjanus bohar",
              "Scarus ghobban", "Paracanthurus hepatus"),
  body_length_cm = c(12.3, 9.8, 14.2, 15.0, 8.5, 21.7, 18.9, 13.4),
  trophic_level = c("herbivore", "herbivore", "omnivore", "herbivore",
                    "herbivore", "carnivore", "herbivore", "omnivore"),
  abundance = c(34, 16, 10, 25, 7, 9, 41, 13)
)
# Task: Filter for herbivores with body_length_cm > 10


# Exercise 3: Calculate forest carbon storage for 6 sample plots
# Add basal_area (m^2), stem_volume (m^3), and carbon_kg columns.

forest_inventory <- tibble(
  plot_id = sprintf("P%02d", 1:6),
  tree_species = c("Quercus robur", "Pinus sylvestris", "Fagus sylvatica", 
                   "Picea abies", "Betula pendula", "Acer pseudoplatanus"),
  dbh_cm = c(32.1, 45.6, 28.4, 37.0, 20.2, 41.8),
  height_m = c(17.4, 23.9, 15.7, 20.5, 13.1, 18.2),
  wood_density = c(0.65, 0.51, 0.60, 0.42, 0.57, 0.63)
)
# Task: Use mutate() to add basal_area = pi*(dbh_cm/200)^2,
# stem_volume = basal_area*height_m*0.5,
# carbon_kg = stem_volume*wood_density*500*0.47


# Exercise 4: Summarize pollinator visits for different habitats

pollinator_survey <- tibble(
  habitat_type = rep(c("forest", "meadow", "wetland"), each = 4),
  pollinator_species = c("Bombus terrestris", "Apis mellifera", "Syrphus ribesii",
                         "Andrena cineraria", "Bombus terrestris",
                         "Melitta leporina", "Osmia bicornis", "Andrena cineraria",
                         "Apis mellifera", "Melitta leporina", "Episyrphus balteatus",
                         "Bombus terrestris"),
  visit_frequency = c(18, 22, 10, 7, 31, 14, 16, 24, 33, 11, 12, 19)
)
# Task: Calculate mean visit_frequency per pollinator_species within each habitat_type


# Exercise 5: Arrange butterfly species by descending wing span.
# Select the top 10 largest species.

butterflies <- tibble(
  species = c("Papilio machaon", "Vanessa atalanta", "Pieris brassicae",
              "Gonepteryx rhamni", "Aglais io", "Inachis io",
              "Polyommatus icarus", "Colias crocea", "Argynnis paphia", 
              "Nymphalis polychloros", "Melanargia galathea", "Maniola jurtina"),
  wingspan_mm = c(85, 72, 65, 60, 68, 70, 33, 54, 73, 76, 52, 50),
  flight_period = c("May-Sep", "June-Sep", "Apr-Oct", "Mar-Oct", "Feb-Nov",
                    "Feb-Nov", "Apr-Oct", "Mar-Nov", "May-Sep", "Mar-Sep",
                    "June-Aug", "May-Oct"),
  host_plant = c("Apiaceae", "Urtica dioica", "Brassicaceae", "Rhamnus frangula", 
                 "Urtica dioica", "Urtica dioica", "Fabaceae", "Fabaceae",
                 "Viola", "Salix", "Poaceae", "Poaceae")
)
# Task: Arrange by wingspan_mm descending, select top 10 rows

################################################################################
###################### Data Manipulation Toolkit - Part 2 #####################
################################################################################

# Example 1: Using tidyr pivot_longer() for Community Data
community_wide <- tibble(
  site = paste0("Plot_", 1:5),
  Quercus_robur = c(12, 8, 15, 3, 9),
  Fagus_sylvatica = c(6, 12, 2, 8, 11),
  Pinus_sylvestris = c(3, 5, 9, 12, 4),
  Betula_pendula = c(8, 2, 6, 5, 7)
)

community_long <- community_wide %>%
  pivot_longer(cols = -site,
               names_to = "species",
               values_to = "abundance") %>%
  mutate(species = str_replace(species, "_", " "))

# Example 2: Using pivot_wider() for Species Matrix
abundance_matrix <- community_long %>%
  pivot_wider(names_from = species,
              values_from = abundance,
              values_fill = 0)

# Example 3: Complex dplyr Pipelines for Microbiome Analysis
library(tidyverse)

# Step 1: Create initial data
microbiome_data <- tibble(
  sample_id = rep(paste0("S", 1:20), each = 5),
  phylum = rep(c("Proteobacteria", "Firmicutes", "Bacteroidetes", "Actinobacteria", "Cyanobacteria"), 20),
  relative_abundance = runif(100, 0, 0.4),
  treatment = rep(c("Control", "Fertilized"), each = 50)
) %>%
  group_by(sample_id, treatment) %>%
  mutate(total_abundance = sum(relative_abundance),
         standardized_abundance = relative_abundance / total_abundance) %>%
  ungroup()

# Step 2: Pull out standardized_abundance as a vector (example use of pull)
standard_vec <- microbiome_data %>% pull(standardized_abundance)

# Step 3: Sample 10 random rows and create a new object
micro_sample <- microbiome_data %>% slice_sample(n = 10)

# Step 4: Relocate columns to bring 'phylum' as the first column
micro_sample <- micro_sample %>% relocate(phylum)
micro_sample <- micro_sample %>% relocate(phylum, after = last_col())

# Display result
micro_sample


# Example 4: Nested Operations for Temporal Ecological Data
temporal_data <- tibble(
  date = rep(seq(as.Date("2023-01-01"), by = "month", length.out = 12), 3),
  site = rep(c("Forest", "Grassland", "Wetland"), each = 12),
  temperature = rnorm(36, mean = 15, sd = 8),
  precipitation = rgamma(36, shape = 2, rate = 0.1),
  species_count = rpois(36, lambda = 12)
) %>%
  group_by(site) %>%
  mutate(temp_anomaly = temperature - mean(temperature),
         precip_cumulative = cumsum(precipitation),
         species_trend = species_count - lag(species_count, default = first(species_count))) %>%
  ungroup()

# Example 5: Advanced Data Cleaning for Biodiversity Surveys
biodiversity_raw <- tibble(
  survey_id = paste0("BIO", sprintf("%03d", 1:50)),
  latitude = runif(50, 45.1, 45.9),
  longitude = runif(50, -74.2, -73.8),
  species_observed = sample(5:25, 50, replace = TRUE),
  survey_effort_hours = sample(c(1, 2, 3, 4, 6, 8), 50, replace = TRUE),
  weather_conditions = sample(c("sunny", "cloudy", "rainy", "windy"), 
                              50, replace = TRUE),
  observer_experience = sample(c("novice", "intermediate", "expert"), 
                               50, replace = TRUE)
) %>%
  mutate(species_per_hour = species_observed / survey_effort_hours,
         effort_category = case_when(
           survey_effort_hours <= 2 ~ "short",
           survey_effort_hours <= 4 ~ "medium",
           TRUE ~ "long"
         )) %>%
  filter(species_per_hour > 1, !is.na(weather_conditions))

################################################################################
################## Data Manipulation Toolkit - Exercises 2 ####################
################################################################################

# Exercise 1: Transform wide-format vegetation data to long format
# Objective: Reshape a vegetation cover dataset (by site) from wide to long format.
# Data provided: site, elevation, aspect, and cover (%) for 6 plant species.
# Task: Convert to a tibble with columns: site, elevation, aspect, species, cover_percentage.

vegetation_wide <- tibble::tibble(
  site = c("Plot1", "Plot2", "Plot3"),
  elevation = c(450, 520, 390),
  aspect = c("N", "E", "W"),
  Quercus = c(12, 8, 15),
  Pinus = c(9, 10, 0),
  Betula = c(0, 5, 3),
  Fagus = c(8, 0, 9),
  Picea = c(0, 7, 0),
  Sorbus = c(5, 1, 2)
)

# Exercise 2: Calculate diversity indices using grouped operations
# Objective: Summarize plot-level diversity using Shannon index and total biomass.
# Data provided: plot_id, species, individuals_counted, biomass_g.
# Task: Group by plot_id to get total individuals, total biomass, and calculate Shannon diversity.

diversity_data <- tibble::tibble(
  plot_id = rep(paste0("P", 1:4), each = 4),
  species = rep(c("Acer", "Quercus", "Betula", "Fagus"), 4),
  individuals_counted = c(12, 6, 3, 9,  8, 10, 4, 2,  9, 3, 6, 12, 2, 1, 7, 8),
  biomass_g = c(41.3, 36.7, 11.8, 29.9, 18.5, 44.1, 7.2, 4.7,
                27.2, 8.1, 13.6, 32.4, 4.3, 1.5, 21.1, 17.9)
)

# Exercise 3: Clean and standardize bird migration data
# Objective: Calculate migration durations and assign migration season.
# Data provided: species, arrival_date, departure_date, route, weather.
# Task: Calculate migration_duration (days between dates), assign season by month.

bird_migration <- tibble::tibble(
  species = c("Sturnus vulgaris", "Sylvia atricapilla", "Erithacus rubecula",
              "Phylloscopus collybita", "Luscinia megarhynchos"),
  arrival_date = as.Date(c("2024-03-05", "2024-04-18", "2024-03-25", "2024-05-02", "2024-04-12")),
  departure_date = as.Date(c("2024-10-10", "2024-09-29", "2024-10-18", "2024-09-24", "2024-09-08")),
  route = c("Western", "Eastern", "Central", "Western", "Eastern"),
  weather = c("clear", "rainy", "windy", "cloudy", "clear")
)

# Exercise 4: Analyze temporal trends in water quality
# Objective: Explore seasonal and temporal patterns in lake water quality data.
# Data provided: date (monthly), lake_id, temperature, pH, dissolved_oxygen, chlorophyll.
# Task: Aggregate by month for each lake and summarize all variables.

water_quality <- tibble::tibble(
  date = rep(seq(as.Date("2024-01-01"), by = "month", length.out = 12), 2),
  lake_id = rep(c("LakeA", "LakeB"), each = 12),
  temperature = round(runif(24, 8, 25), 1),
  pH = round(runif(24, 6.1, 8.7), 2),
  dissolved_oxygen = round(runif(24, 4, 12), 1),
  chlorophyll = round(runif(24, 0.5, 6.3), 2)
)

# Exercise 5: Process camera trap data with nested operations
# Objective: Quantify detection and daily activity patterns from camera trap records.
# Data provided: camera_id, timestamp, species, individuals, behavior for 3 cameras.
# Task: Summarize detection rates by species and activity (diurnal/nocturnal/crepuscular).

camera_trap <- tibble::tibble(
  camera_id = rep(c("CT01", "CT02", "CT03"), each = 8),
  timestamp = as.POSIXct(c(
    "2024-03-02 06:12", "2024-03-02 19:41", "2024-03-03 12:24", "2024-03-03 18:18",
    "2024-03-04 06:48", "2024-03-04 20:55", "2024-03-05 09:05", "2024-03-05 21:10",
    "2024-03-02 09:17", "2024-03-02 21:04", "2024-03-03 10:03", "2024-03-03 17:53",
    "2024-03-04 08:14", "2024-03-04 18:23", "2024-03-05 14:07", "2024-03-05 19:37",
    "2024-03-02 07:05", "2024-03-02 23:41", "2024-03-03 11:24", "2024-03-03 18:32",
    "2024-03-04 06:22", "2024-03-04 20:10", "2024-03-05 16:39", "2024-03-05 20:22"
  )),
  species = sample(c("Vulpes vulpes", "Lepus europaeus", "Meles meles"), 24, replace = TRUE),
  individuals = sample(1:4, 24, replace = TRUE),
  behavior = sample(c("foraging", "moving", "resting"), 24, replace = TRUE)
)


