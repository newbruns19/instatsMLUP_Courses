####################################################################################################
###
### File:    02_Combining_Tables.R
### Purpose: Examples and exercises for Combining tables
###         using dplyr
### Authors: Gabriel Rodrigues Palma
### Date:    19/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
############################# Joining Tables - Part 1 ########################
################################################################################

# Example 1: left_join() for Species and Habitat Data
species_traits <- tibble(
  species_code = c("QURO", "FASY", "PISY", "BEPE", "ACPS"),
  species_name = c("Quercus robur", "Fagus sylvatica", "Pinus sylvestris",
                   "Betula pendula", "Acer pseudoplatanus"),
  max_height_m = c(25, 30, 35, 20, 28),
  shade_tolerance = c("high", "very_high", "low", "medium", "high")
)

forest_inventory <- tibble(
  plot_id = paste0("P", 1:12),
  species_code = sample(c("QURO", "FASY", "PISY", "BEPE", "ACPS", "UNKN"), 
                        12, replace = TRUE),
  dbh_cm = runif(12, 15, 60),
  individual_id = paste0("T", 1:12)
)

# Join species traits with inventory data
forest_complete <- forest_inventory %>%
  left_join(species_traits, by = "species_code")

# Example 2: inner_join() for Complete Environmental Records
site_coordinates <- tibble(
  site_id = paste0("SITE", 1:8),
  latitude = runif(8, 44.5, 45.5),
  longitude = runif(8, -75.5, -74.5),
  elevation_m = sample(200:800, 8)
)

environmental_data <- tibble(
  site_id = paste0("SITE", c(1:6, 9, 10)),
  soil_ph = runif(8, 5.5, 7.5),
  annual_temp = runif(8, 8, 12),
  annual_precip = runif(8, 800, 1200)
)

# Keep only sites with both coordinate and environmental data
complete_sites <- site_coordinates %>%
  inner_join(environmental_data, by = "site_id")

# Example 3: full_join() for Comprehensive Species Lists
spring_survey <- tibble(
  species = c("Turdus migratorius", "Bombycilla cedrorum", "Sialia sialis",
              "Poecile atricapillus", "Corvus brachyrhynchos"),
  spring_abundance = c(45, 23, 12, 67, 8),
  breeding_pairs = c(12, 6, 4, 18, 2)
)

fall_survey <- tibble(
  species = c("Turdus migratorius", "Junco hyemalis", "Sialia sialis",
              "Poecile atricapillus", "Spinus tristis"),
  fall_abundance = c(23, 34, 8, 71, 19),
  migration_status = c("resident", "migrant", "resident", "resident", "partial")
)

# Combine all species observations
annual_survey <- spring_survey %>%
  full_join(fall_survey, by = "species") %>%
  mutate(across(c(spring_abundance, fall_abundance), ~replace_na(.x, 0)))

# Example 4: right_join() for Species Conservation Status
iucn_status <- tibble(
  scientific_name = c("Panthera leo", "Ursus americanus", "Canis lupus",
                      "Lynx canadensis", "Puma concolor", "Vulpes vulpes"),
  conservation_status = c("Vulnerable", "Least_Concern", "Least_Concern",
                          "Least_Concern", "Least_Concern", "Least_Concern"),
  population_trend = c("decreasing", "stable", "increasing", 
                       "stable", "stable", "increasing")
)

carnivore_observations <- tibble(
  scientific_name = c("Canis lupus", "Lynx canadensis", "Ursus americanus"),
  observation_date = as.Date(c("2023-06-15", "2023-07-22", "2023-08-10")),
  location = c("Algonquin Park", "Yukon Territory", "British Columbia"),
  individual_count = c(3, 1, 2)
)

# Include all IUCN species, keep only observed ones
conservation_observations <- carnivore_observations %>%
  right_join(iucn_status, by = "scientific_name")

# Example 5: anti_join() for Missing Species Analysis
expected_species <- tibble(
  species = c("Acer saccharum", "Betula alleghaniensis", "Fagus grandifolia",
              "Tsuga canadensis", "Picea rubens", "Abies balsamea"),
  expected_abundance = c(25, 15, 20, 10, 8, 12),
  habitat_requirement = c("rich_soil", "moist_soil", "well_drained",
                          "shade_tolerant", "acidic_soil", "cool_moist")
)

observed_species <- tibble(
  species = c("Acer saccharum", "Fagus grandifolia", "Picea rubens"),
  actual_abundance = c(18, 14, 5),
  plot_occurrence = c(8, 6, 3)
)

# Find species that were expected but not observed
missing_species <- expected_species %>%
  anti_join(observed_species, by = "species")

################################################################################
####################### Joining Tables - Exercises 1 #########################
################################################################################

# Exercise 1: Join pollinator and plant flowering data
# Objective: Match pollinator species to flower species that they can access.
# A pollinator can pollinate a flower if proboscis_length >= flower_depth
# and activity_period == bloom_period.

pollinators <- tibble::tibble(
  species = c("Bombus terrestris", "Apis mellifera", "Eristalis tenax"),
  proboscis_length = c(10.5, 6.2, 3.8),  # mm
  activity_period = c("Spring", "Summer", "Summer")
)

flowers <- tibble::tibble(
  plant_species = c("Primula veris", "Trifolium pratense", "Bellis perennis"),
  flower_depth = c(8.0, 5.5, 2.5),  # mm
  bloom_period = c("Spring", "Summer", "Summer")
)

# Exercise 2: Combine bird migration data with daily weather data.
# Objective: For each migratory observation, summarize the weather at arrival.
# The join matches bird arrival_date with weather date.

migration <- tibble::tibble(
  species = c("Sylvia atricapilla", "Hirundo rustica", "Phylloscopus collybita"),
  arrival_date = as.Date(c("2024-03-25", "2024-04-02", "2024-04-10")),
  departure_date = as.Date(c("2024-09-10", "2024-09-18", "2024-09-15")),
  route = c("Western", "Central", "Eastern")
)

weather <- tibble::tibble(
  date = as.Date("2024-03-20") + 0:30,
  temperature = c(seq(8, 16, length.out = 31)),
  wind_speed = runif(31, 0, 15),
  precipitation = runif(31, 0, 10)
)

# Exercise 3: Match fish species with stream habitat requirements.
# Objective: Determine which fish species can inhabit each stream, given
# temperature range, minimum DO, and substrate match.

streams <- tibble::tibble(
  stream_id = c("S1", "S2", "S3"),
  temperature = c(15, 12, 9),
  dissolved_oxygen = c(7.5, 8.2, 9.0),
  flow_rate = c(1.4, 0.9, 2.2),  # m/s
  substrate = c("gravel", "sand", "rock")
)

fish <- tibble::tibble(
  fish_species = c("Salmo trutta", "Cyprinus carpio", "Thymallus thymallus"),
  temp_range = c("8-18", "16-28", "6-15"),
  oxygen_min = c(7.0, 5.0, 8.5),
  preferred_substrate = c("gravel", "sand", "rock")
)

# Exercise 4: Identify gaps in species monitoring data.
# Objective: List high-priority species without any observations since
# "2024-03-01".

targets <- tibble::tibble(
  species = c("Lynx lynx", "Canis lupus", "Vulpes vulpes"),
  priority_level = c("high", "high", "medium")
)

surveys <- tibble::tibble(
  species = c("Lynx lynx", "Vulpes vulpes"),
  survey_date = as.Date(c("2024-02-10", "2024-03-15")),
  abundance = c(2, 13)
)

# Exercise 5: Link genetic samples with collection metadata.
# Objective: Merge genetic diversity results with metadata for reporting.

genetics <- tibble::tibble(
  sample_id = c("G001", "G002", "G003"),
  genetic_diversity = c(0.23, 0.19, 0.30),
  allele_count = c(7, 5, 9)
)

collection <- tibble::tibble(
  sample_id = c("G001", "G002", "G003"),
  collection_site = c("ForestA", "LakeB", "HillC"),
  collector = c("P. Smith", "J. Lee", "M. Gomez"),
  collection_date = as.Date(c("2024-03-01", "2024-03-12", "2024-03-19"))
)

################################################################################
############################# Joining Tables - Part 2 ########################
################################################################################

# Example 1: Multiple Joins for Ecosystem Food Web Analysis
primary_producers <- tibble(
  species_id = paste0("PP", 1:6),
  species_name = c("Chlorella vulgaris", "Scenedesmus obliquus", 
                   "Ankistrodesmus falcatus", "Pediastrum duplex",
                   "Cosmarium bioculatum", "Closterium acutum"),
  biomass_mg_l = runif(6, 0.5, 3.2),
  photosynthesis_rate = runif(6, 2.1, 8.7)
)

primary_consumers <- tibble(
  consumer_id = paste0("PC", 1:8),
  consumer_name = c("Daphnia magna", "Bosmina longirostris", "Cyclops vernalis",
                    "Diaptomus sicilis", "Keratella cochlearis", "Polyarthra vulgaris",
                    "Rotaria rotatoria", "Paramecium caudatum"),
  preferred_prey = sample(paste0("PP", 1:6), 8, replace = TRUE),
  feeding_rate = runif(8, 0.1, 1.2)
)

water_quality <- tibble(
  sample_date = rep(seq(as.Date("2023-01-01"), by = "week", length.out = 12), 2),
  lake_section = rep(c("north", "south"), each = 12),
  temperature = rnorm(24, 18, 4),
  nutrient_level = runif(24, 0.1, 2.5),
  ph_level = runif(24, 6.8, 8.2)
)

# Complex joining workflow
ecosystem_data <- primary_producers %>%
  left_join(primary_consumers, by = c("species_id" = "preferred_prey")) %>%
  mutate(predation_pressure = ifelse(is.na(consumer_id), 0, feeding_rate))

# Example 2: Joining with Different Key Types
bird_bands <- tibble(
  band_number = paste0("B", sprintf("%04d", 1:15)),
  species_code = sample(c("AMRO", "HOWR", "BCCH", "WREN", "GRCA"), 15, replace = TRUE),
  banding_date = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"), 15),
  age_class = sample(c("juvenile", "adult"), 15, replace = TRUE),
  sex = sample(c("M", "F", "U"), 15, replace = TRUE)
)

recapture_data <- tibble(
  band_number = sample(paste0("B", sprintf("%04d", 1:15)), 8),
  recapture_date = sample(seq(as.Date("2023-06-01"), as.Date("2024-01-31"), by = "day"), 8),
  recapture_location = sample(c("same_site", "1km_away", "5km_away", "10km_away"), 8, replace = TRUE),
  weight_g = runif(8, 8, 45),
  condition_score = sample(1:5, 8, replace = TRUE)
)

# Track bird survival and movement
bird_tracking <- bird_bands %>%
  left_join(recapture_data, by = "band_number") %>%
  mutate(days_at_liberty = as.numeric(recapture_date - banding_date),
         survival_status = ifelse(is.na(recapture_date), "unknown", "alive"))

# Example 3: Suffix Handling in Joins
vegetation_2022 <- tibble(
  plot_id = paste0("VP", 1:10),
  total_cover = runif(10, 60, 95),
  species_richness = sample(8:20, 10),
  dominant_species = sample(c("grass", "forb", "shrub"), 10, replace = TRUE)
)

vegetation_2023 <- tibble(
  plot_id = paste0("VP", 1:10),
  total_cover = runif(10, 55, 90),
  species_richness = sample(6:22, 10),
  dominant_species = sample(c("grass", "forb", "shrub"), 10, replace = TRUE)
)

# Compare vegetation changes between years
vegetation_change <- vegetation_2022 %>%
  full_join(vegetation_2023, by = "plot_id", suffix = c("_2022", "_2023")) %>%
  mutate(
    cover_change = total_cover_2023 - total_cover_2022,
    richness_change = species_richness_2023 - species_richness_2022,
    succession_direction = case_when(
      dominant_species_2022 != dominant_species_2023 ~ "changed",
      TRUE ~ "stable"
    )
  )

# Example 4: Conditional Joins for Species Interactions
plant_traits <- tibble(
  plant_species = c("Solidago canadensis", "Aster novae-angliae", 
                    "Echinacea purpurea", "Rudbeckia hirta", "Monarda fistulosa"),
  flower_color = c("yellow", "purple", "purple", "yellow", "pink"),
  flower_shape = c("small_clustered", "daisy", "cone", "daisy", "tube"),
  nectar_volume = runif(5, 0.1, 2.5),
  bloom_start = sample(150:200, 5),
  bloom_end = sample(220:280, 5)
)

pollinator_preferences <- tibble(
  pollinator_species = c("Bombus impatiens", "Apis mellifera", "Danaus plexippus",
                         "Vanessa cardui", "Megachile rotundata"),
  preferred_colors = c("purple,blue,white", "yellow,white", "red,orange,pink",
                       "red,purple,orange", "purple,blue"),
  tongue_length = c(12, 6, 15, 8, 9),
  activity_start = sample(140:180, 5),
  activity_end = sample(240:300, 5)
)

# Create interaction network based on compatibility
# This requires more complex logic, simplified here
plant_pollinator_network <- plant_traits %>%
  crossing(pollinator_preferences) %>%
  filter(
    bloom_start <= activity_end,
    bloom_end >= activity_start,
    str_detect(preferred_colors, str_extract(flower_color, "\\w+"))
  )

# Example 5: Join with Grouped Aggregation
soil_samples <- tibble(
  site_id = rep(paste0("S", 1:5), each = 4),
  depth_cm = rep(c(0, 10, 20, 30), 5),
  organic_matter = runif(20, 2, 12),
  nitrogen_ppm = runif(20, 5, 25),
  phosphorus_ppm = runif(20, 3, 18),
  sample_date = sample(seq(as.Date("2023-05-01"), as.Date("2023-05-31"), by = "day"), 20)
)

microbial_activity <- tibble(
  site_id = rep(paste0("S", 1:5), each = 2),
  measurement_type = rep(c("respiration_rate", "enzyme_activity"), 5),
  value = runif(10, 0.5, 4.2),
  units = rep(c("mg_CO2_g_soil_hr", "nmol_g_soil_hr"), 5)
)

# Aggregate soil data and join with microbial data
soil_summary <- soil_samples %>%
  group_by(site_id) %>%
  summarise(
    mean_organic_matter = mean(organic_matter),
    mean_nitrogen = mean(nitrogen_ppm),
    mean_phosphorus = mean(phosphorus_ppm),
    .groups = 'drop'
  ) %>%
  left_join(
    microbial_activity %>% 
      pivot_wider(names_from = measurement_type, values_from = value),
    by = "site_id"
  )

################################################################################
####################### Joining Tables - Exercises 2 #########################
################################################################################

# Exercise 1: Identify realized food web interactions.
# Use the tibbles below to join predator abilities, prey traits, 
# and habitat co-occurrence. Resulting table should list all plausible
# predator-prey-habitat links.

predators <- tibble::tibble(
  species = c("Lynx lynx", "Ursus arctos", "Vulpes vulpes"),
  hunt_method = c("ambush", "foraging", "stalking"),
  prey_size_range = c("small-medium", "small-large", "small")
)

prey <- tibble::tibble(
  species = c("Lepus europaeus", "Microtus arvalis", "Cervus elaphus"),
  body_size = c("medium", "small", "large"),
  defense_mechanism = c("camouflage", "burrowing", "herding")
)

habitat_overlap <- tibble::tibble(
  predator_species = c("Lynx lynx", "Lynx lynx", "Ursus arctos", 
                       "Vulpes vulpes", "Vulpes vulpes"),
  prey_species = c("Lepus europaeus", "Microtus arvalis", "Cervus elaphus", 
                   "Microtus arvalis", "Lepus europaeus"),
  habitat = c("forest", "forest", "forest", "meadow", "forest")
)

# Exercise 2: Reconstruct individual-based animal life histories.
# Use the provided tables for birth records, growth monitoring, and offspring.
# Merge by individual_id. Choose two individuals measured/reproduced more than once. 
birth_records <- tibble::tibble(
  individual_id = c("A1", "A2", "A3"),
  birth_date = as.Date(c("2022-04-01", "2022-05-12", "2022-03-25")),
  mother_id = c("M7", "M2", "M9"),
  birth_weight = c(2.1, 1.9, 2.3)
)

growth_data <- tibble::tibble(
  individual_id = rep(c("A1", "A2", "A3"), each = 3),
  measurement_date = as.Date(c("2022-05-01", "2022-06-01", "2022-07-01",
                               "2022-06-10", "2022-07-10", "2022-08-10",
                               "2022-05-10", "2022-06-10", "2022-07-10")),
  weight = c(2.6, 3.0, 3.3, 2.2, 2.6, 3.0, 2.7, 3.2, 3.4),
  length = c(9.1, 12.2, 14.0, 8.8, 12.0, 13.9, 9.3, 12.1, 14.1)
)

reproduction_data <- tibble::tibble(
  individual_id = c("A2", "A2", "A3", "A1"),
  mate_id = c("B8", "B6", "B7", "B5"),
  offspring_count = c(2, 1, 2, 1)
)

# Exercise 3: Assess community composition shifts due to disturbance.
# Analyze pre- and post-disturbance counts of species per sample plot,
# then join and quantify appearance/disappearance and change in abundance.
before_disturbance <- tibble::tibble(
  plot_id = c("P1", "P1", "P2", "P2", "P3"),
  species = c("Carex", "Salix", "Betula", "Salix", "Alnus"),
  count = c(20, 10, 17, 3, 11)
)

after_disturbance <- tibble::tibble(
  plot_id = c("P1", "P2", "P2", "P3", "P3"),
  species = c("Carex", "Salix", "Alnus", "Salix", "Alnus"),
  count = c(12, 7, 5, 0, 18)
)

# Exercise 4: Link species presence data to temperature/precipitation at 
# sample points. Join by exact coordinates for demonstration.
species_occurrences <- tibble::tibble(
  species = c("Rana temporaria", "Triturus cristatus", "Bufo bufo"),
  latitude = c(52.1, 52.2, 52.2),
  longitude = c(5.1, 5.0, 5.1),
  year = c(2023, 2023, 2023)
)

climate_data <- tibble::tibble(
  latitude = c(52.1, 52.2, 52.3),
  longitude = c(5.1, 5.0, 5.2),
  year = c(2023, 2023, 2023),
  temperature = c(14.5, 13.2, 15.0),
  precipitation = c(520, 640, 598)
)

# Exercise 5: Aggregate tree-level, plot-level, and stand-level information.
# Use sample hierarchical data for measurements at different scales.
individual_measurements <- tibble::tibble(
  tree_id = c("T1", "T2", "T3", "T4"),
  plot_id = c("PL1", "PL1", "PL2", "PL3"),
  dbh = c(18.6, 21.2, 16.4, 23.3),
  height = c(11.5, 13.2, 10.1, 14.4),
  health_score = c(5, 4, 3, 4)
)

plot_data <- tibble::tibble(
  plot_id = c("PL1", "PL2", "PL3"),
  stand_id = c("S1", "S1", "S2"),
  slope = c(5, 8, 2),
  aspect = c("N", "E", "W"),
  soil_type = c("loam", "sand", "clay")
)

stand_data <- tibble::tibble(
  stand_id = c("S1", "S2"),
  age = c(56, 40),
  management_history = c("thinned_2015", "clearcut_2000")
)
