####################################################################################################
###
### File:    01_data_science_workflow.R
### Purpose: The core steps from the data science workflow.
### Authors: Gabriel Rodrigues Palma
### Date:    23/06/25
###
####################################################################################################
# Load packages ------
source('00_source.r')

################################################################################
################## The Data Science Workflow - Examples #####################
################################################################################

# Example 1: Forest Inventory Data - Complete Workflow Demonstration
# Simulate forest inventory data
set.seed(123)
forest_data <- tibble(
  plot_id = 1:50,
  species = sample(c("Quercus_robur", "Pinus_sylvestris", "Fagus_sylvatica", 
                     "Betula_pendula"), 50, replace = TRUE),
  dbh_cm = rnorm(50, mean = 25, sd = 8),
  height_m = rnorm(50, mean = 15, sd = 5),
  age_years = sample(20:80, 50, replace = TRUE),
  soil_ph = rnorm(50, mean = 6.2, sd = 0.8),
  elevation_m = sample(200:800, 50, replace = TRUE)
)

# TIDY: Data is already in tidy format - each variable is a column, 
# each observation is a row, each value is a cell
forest_data

# TRANSFORM: Calculate biomass using allometric equations
forest_transformed <- forest_data %>%
  mutate(
    biomass_kg = 0.0673 * (dbh_cm^1.784) * (height_m^0.207),
    size_class = case_when(
      dbh_cm < 20 ~ "Small",
      dbh_cm >= 20 & dbh_cm < 40 ~ "Medium",
      dbh_cm >= 40 ~ "Large"
    )
  ) %>%
  filter(biomass_kg > 0)

# VISUALISE: Create exploratory plots
ggplot(forest_transformed, aes(x = dbh_cm, y = biomass_kg, 
                               color = species)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tree Biomass vs Diameter by Species",
       x = "Diameter at Breast Height (cm)",
       y = "Biomass (kg)")

# MODEL: Predict biomass using tidymodels
biomass_recipe <- recipe(biomass_kg ~ dbh_cm + height_m + age_years + 
                           soil_ph, data = forest_transformed) %>%
  step_normalize(all_numeric_predictors())

linear_model <- linear_reg() %>%
  set_engine("lm")

biomass_workflow <- workflow() %>%
  add_recipe(biomass_recipe) %>%
  add_model(linear_model)

biomass_fit <- biomass_workflow %>%
  fit(data = forest_transformed)

# COMMUNICATE: Extract and interpret results
tidy(biomass_fit) %>%
  mutate(p.value = round(p.value, 3))

# Example 2: Species Abundance Monitoring - Temporal Analysis
# Simulate bird monitoring data across multiple years
bird_data <- tibble(
  site_id = rep(paste0("Site_", 1:10), each = 4),
  year = rep(2020:2023, 10),
  species_richness = sample(8:25, 40, replace = TRUE),
  total_abundance = sample(50:200, 40, replace = TRUE),
  habitat_type = rep(sample(c("Forest", "Grassland", "Wetland"), 
                            10, replace = TRUE), each = 4),
  temperature_avg = rnorm(40, mean = 12, sd = 3),
  precipitation_mm = rnorm(40, mean = 800, sd = 200)
)

# TIDY: Ensure data structure is appropriate
bird_tidy <- bird_data %>%
  mutate(year = as.factor(year))

# TRANSFORM: Calculate diversity indices and trends
bird_transformed <- bird_tidy %>%
  group_by(site_id) %>%
  mutate(
    richness_change = species_richness - lag(species_richness),
    abundance_trend = (total_abundance - first(total_abundance)) / 
      first(total_abundance) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(richness_change))

# VISUALISE: Temporal trends
ggplot(bird_transformed, aes(x = year, y = species_richness, 
                             fill = habitat_type)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Species Richness Trends by Habitat Type",
       x = "Year", y = "Species Richness") +
  theme_new()

# MODEL: Analyze factors affecting species richness
richness_recipe <- recipe(species_richness ~ habitat_type + 
                            temperature_avg + precipitation_mm + year, 
                          data = bird_transformed) %>%
  step_dummy(all_nominal_predictors())

glm_model <- linear_reg() %>%
  set_engine("glm")

richness_workflow <- workflow() %>%
  add_recipe(richness_recipe) %>%
  add_model(glm_model)

richness_fit <- richness_workflow %>%
  fit(data = bird_transformed)

# COMMUNICATE: Model summary and predictions
richness_results <- tidy(richness_fit) %>%
  mutate(across(where(is.numeric), round, 3))

# Example 3: Plant Community Analysis - Multivariate Approach
# Simulate plant quadrat data
plant_data <- tibble(
  quadrat_id = 1:30,
  site_type = rep(c("Disturbed", "Undisturbed"), each = 15),
  grass_cover = runif(30, 10, 60),
  forb_cover = runif(30, 5, 30),
  shrub_cover = runif(30, 0, 25),
  bare_soil = runif(30, 5, 40),
  moisture_index = rnorm(30, mean = 0, sd = 1),
  nutrient_index = rnorm(30, mean = 0, sd = 1)
) %>%
  mutate(
    total_vegetation = grass_cover + forb_cover + shrub_cover,
    other_cover = 100 - total_vegetation - bare_soil
  ) %>%
  filter(total_vegetation <= 100)

# TIDY: Convert to long format for vegetation analysis
plant_long <- plant_data %>%
  pivot_longer(cols = c(grass_cover, forb_cover, shrub_cover), 
               names_to = "vegetation_type", 
               values_to = "cover_percent") %>%
  mutate(vegetation_type = str_remove(vegetation_type, "_cover"))

# TRANSFORM: Calculate vegetation diversity measures
plant_diversity <- plant_data %>%
  mutate(
    shannon_index = -((grass_cover/100) * log(grass_cover/100 + 0.001) +
                        (forb_cover/100) * log(forb_cover/100 + 0.001) +
                        (shrub_cover/100) * log(shrub_cover/100 + 0.001)),
    dominance_index = pmax(grass_cover, forb_cover, shrub_cover) / 
      total_vegetation
  )

# VISUALISE: Community composition
ggplot(plant_long, aes(x = site_type, y = cover_percent)) +
  geom_boxplot() +
  facet_wrap(~vegetation_type) +
  labs(title = "Vegetation Cover by Site Type",
       x = "Site Type", y = "Cover Percentage") +
  theme_new()

# MODEL: Predict diversity based on environmental factors
diversity_recipe <- recipe(shannon_index ~ site_type + moisture_index + 
                             nutrient_index + bare_soil, 
                           data = plant_diversity) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

rf_model <- rand_forest() %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

diversity_workflow <- workflow() %>%
  add_recipe(diversity_recipe) %>%
  add_model(rf_model)

diversity_fit <- diversity_workflow %>%
  fit(data = plant_diversity)

# COMMUNICATE: Variable importance
library(vip)
diversity_fit %>%
  extract_fit_parsnip() %>%
  vip()

# Example 4: Water Quality and Aquatic Biodiversity
# Simulate stream monitoring data
stream_data <- tibble(
  stream_id = rep(paste0("Stream_", LETTERS[1:8]), each = 12),
  month = rep(1:12, 8),
  water_temp_c = 5 + 15 * sin(2 * pi * (month - 3) / 12) + 
    rnorm(96, 0, 2),
  dissolved_oxygen = 12 - 0.3 * water_temp_c + rnorm(96, 0, 1),
  ph_level = rnorm(96, mean = 7.2, sd = 0.5),
  turbidity_ntu = rlnorm(96, meanlog = 1, sdlog = 0.5),
  macroinvertebrate_taxa = rpois(96, lambda = 15),
  fish_abundance = rpois(96, lambda = 8)
)

# TIDY: Ensure proper data types and structure
stream_tidy <- stream_data %>%
  mutate(
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    ),
    season = factor(season, levels = c("Spring", "Summer", "Fall", 
                                       "Winter"))
  )

# TRANSFORM: Calculate water quality indices
stream_transformed <- stream_tidy %>%
  mutate(
    wqi_score = (dissolved_oxygen / 12) * 0.4 + 
      (1 - abs(ph_level - 7) / 3) * 0.3 +
      (1 / (1 + turbidity_ntu / 10)) * 0.3,
    biodiversity_index = log(macroinvertebrate_taxa + 1) + 
      log(fish_abundance + 1)
  ) %>%
  group_by(stream_id) %>%
  mutate(
    temp_anomaly = water_temp_c - mean(water_temp_c, na.rm = TRUE)
  ) %>%
  ungroup()

# VISUALISE: Seasonal patterns
ggplot(stream_transformed, aes(x = season, y = wqi_score)) +
  geom_violin( alpha = 0.7) +
  geom_point(position = position_jitter(width = 0.2)) +
  labs(title = "Water Quality Index by Season",
       x = "Season", y = "Water Quality Index") +
  theme_new()

# MODEL: Predict biodiversity from water quality parameters
biodiversity_recipe <- recipe(biodiversity_index ~ water_temp_c + 
                                dissolved_oxygen + ph_level + turbidity_ntu + 
                                season, data = stream_transformed) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

gbm_model <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression")

biodiversity_workflow <- workflow() %>%
  add_recipe(biodiversity_recipe) %>%
  add_model(gbm_model)

biodiversity_fit <- biodiversity_workflow %>%
  fit(data = stream_transformed)

# COMMUNICATE: Model performance metrics
biodiversity_pred <- predict(biodiversity_fit, stream_transformed) %>%
  bind_cols(stream_transformed) %>%
  metrics(truth = biodiversity_index, estimate = .pred)

# Example 5: Soil Microbiome and Plant Health
# Simulate soil-plant interaction data
soil_plant_data <- tibble(
  plot_id = 1:40,
  soil_treatment = rep(c("Control", "Organic", "Fertilized", "Amended"), 
                       each = 10),
  microbial_diversity = rnorm(40, mean = 3.5, sd = 0.8),
  soil_carbon_percent = rnorm(40, mean = 2.8, sd = 0.6),
  nitrogen_ppm = rnorm(40, mean = 45, sd = 12),
  phosphorus_ppm = rnorm(40, mean = 28, sd = 8),
  plant_biomass_g = rnorm(40, mean = 150, sd = 30),
  root_length_cm = rnorm(40, mean = 25, sd = 8),
  chlorophyll_content = rnorm(40, mean = 42, sd = 7)
)

# TIDY: Verify data structure and completeness
soil_plant_tidy <- soil_plant_data %>%
  mutate(soil_treatment = factor(soil_treatment, 
                                 levels = c("Control", "Organic", 
                                            "Fertilized", "Amended")))

# TRANSFORM: Calculate plant health indices
soil_plant_transformed <- soil_plant_tidy %>%
  mutate(
    nutrient_ratio = nitrogen_ppm / phosphorus_ppm,
    plant_health_index = scale(plant_biomass_g)[,1] + 
      scale(root_length_cm)[,1] + 
      scale(chlorophyll_content)[,1],
    soil_quality_index = scale(microbial_diversity)[,1] + 
      scale(soil_carbon_percent)[,1]
  )

# VISUALISE: Treatment effects
ggplot(soil_plant_transformed, 
       aes(x = soil_treatment, y = plant_health_index)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  labs(title = "Plant Health Index by Soil Treatment",
       x = "Soil Treatment", y = "Plant Health Index") +
  theme_new()

# MODEL: Analyze soil-plant relationships
health_recipe <- recipe(plant_health_index ~ soil_treatment + 
                          microbial_diversity + soil_carbon_percent + 
                          nutrient_ratio, data = soil_plant_transformed) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(terms = ~ microbial_diversity:soil_carbon_percent)

lm_model <- linear_reg() %>%
  set_engine("lm")

health_workflow <- workflow() %>%
  add_recipe(health_recipe) %>%
  add_model(lm_model)

health_fit <- health_workflow %>%
  fit(data = soil_plant_transformed)

# COMMUNICATE: Effect sizes and significance
tidy(health_fit, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), round, 3))

################################################################################
################## The Data Science Workflow - Exercises ####################
################################################################################

# Exercise 1: Complete Wildlife Camera Survey Analysis
# You are analyzing wildlife camera data to understand activity patterns
# and species abundance. Follow the complete data science workflow.

# Create the dataset
set.seed(456)
camera_survey <- tibble(
  camera_id = rep(paste0("CAM_", sprintf("%02d", 1:15)), each = 8),
  species = rep(c("Deer", "Fox", "Rabbit", "Squirrel", "Bird", 
                  "Raccoon", "Opossum", "Skunk"), 15),
  detections = rpois(120, lambda = 12),
  day_detections = rbinom(120, detections, 0.6),
  night_detections = detections - day_detections,
  habitat_type = rep(sample(c("Forest", "Edge", "Open"), 15, 
                            replace = TRUE), each = 8),
  distance_to_water = rep(runif(15, 50, 500), each = 8),
  canopy_cover = rep(runif(15, 20, 95), each = 8)
)

# YOUR TASK: Complete the data science workflow
# TIDY: Check if data needs restructuring - create activity_ratio 
# (day_detections/total_detections)

# TRANSFORM: 
# - Calculate detection_rate per camera
# - Create size categories for species (Large: Deer; Medium: Fox, Raccoon; 
#   Small: others)
# - Calculate activity pattern index (day vs night preference)

# VISUALISE: 
# - Create a plot showing species detections by habitat type
# - Create a plot showing day vs night activity patterns by species

# MODEL: 
# - Build a model predicting total detections based on habitat_type, 
#   distance_to_water, and canopy_cover
# - Use either linear regression or random forest

# COMMUNICATE: 
# - Extract and interpret model results
# - What factors most influence wildlife detections?

################################################################################

# Exercise 2: Lake Eutrophication Study
# Analyze factors contributing to lake eutrophication using water quality data

# Create the dataset
lake_data <- tibble(
  lake_id = rep(paste0("Lake_", LETTERS[1:12]), each = 6),
  season = rep(c("Spring", "Summer", "Fall", "Winter", "Spring", "Summer"), 
               12),
  chlorophyll_a = rlnorm(72, meanlog = 2, sdlog = 0.8),
  total_phosphorus = rlnorm(72, meanlog = 1.5, sdlog = 0.6),
  total_nitrogen = rlnorm(72, meanlog = 3, sdlog = 0.4),
  water_clarity_m = rexp(72, rate = 0.5),
  surface_temp_c = rnorm(72, mean = 15, sd = 8),
  dissolved_oxygen = rnorm(72, mean = 8, sd = 2),
  watershed_agriculture = rep(runif(12, 0, 80), each = 6),
  lake_depth_m = rep(runif(12, 5, 50), each = 6)
)

# YOUR TASK: Complete the data science workflow
# TIDY: Ensure proper data types, handle any missing values

# TRANSFORM: 
# - Calculate trophic state index: TSI = 14.42 * ln(chlorophyll_a) + 4.15
# - Create eutrophication categories: TSI < 40 (oligotrophic), 
#   40-50 (mesotrophic), >50 (eutrophic)
# - Calculate N:P ratio (total_nitrogen/total_phosphorus)

# VISUALISE: 
# - Plot TSI values by season and agriculture intensity
# - Create correlation plot between nutrients and water clarity

# MODEL: 
# - Predict trophic state index using environmental variables
# - Try both linear and non-linear approaches

# COMMUNICATE: 
# - Which factors most strongly predict eutrophication?
# - What seasonal patterns do you observe?

################################################################################

# Exercise 3: Pollinator Network Analysis
# Study plant-pollinator interactions and network properties

# Create the dataset
pollinator_data <- tibble(
  site_id = rep(paste0("Site_", 1:10), each = 20),
  plant_species = rep(sample(c("Echinacea", "Rudbeckia", "Monarda", 
                               "Asclepias", "Solidago"), 20, replace = TRUE), 10),
  pollinator_species = rep(sample(c("Honeybee", "Bumblebee", "Butterfly", 
                                    "Syrphid_fly", "Sweat_bee"), 20, 
                                  replace = TRUE), 10),
  interaction_frequency = rpois(200, lambda = 8),
  flower_abundance = sample(50:500, 200, replace = TRUE),
  bloom_period_days = sample(30:120, 200, replace = TRUE),
  habitat_quality = rnorm(200, mean = 0, sd = 1),
  temperature_avg = rnorm(200, mean = 22, sd = 4),
  precipitation_mm = rnorm(200, mean = 600, sd = 150)
)

# YOUR TASK: Complete the data science workflow
# TIDY: Check for appropriate data structure for network analysis

# TRANSFORM: 
# - Calculate interaction strength (interaction_frequency/flower_abundance)
# - Create specialization index for each pollinator species
# - Calculate site-level diversity indices

# VISUALISE: 
# - Create network visualization showing plant-pollinator connections
# - Plot interaction patterns by habitat quality

# MODEL: 
# - Predict interaction frequency using plant and environmental variables
# - Analyze factors affecting pollinator specialization

# COMMUNICATE: 
# - What drives pollinator-plant interaction strength?
# - How does habitat quality affect network structure?

################################################################################

# Exercise 4: Forest Succession Dynamics
# Analyze long-term forest change and succession patterns

# Create the dataset
succession_data <- tibble(
  plot_id = rep(paste0("Plot_", sprintf("%03d", 1:25)), each = 12),
  year_since_disturbance = rep(c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 
                                 50, 55), 25),
  pioneer_species_cover = pmax(0, 80 * exp(-0.1 * year_since_disturbance) + 
                                 rnorm(300, 0, 5)),
  intermediate_species_cover = pmax(0, 60 * sin(0.05 * year_since_disturbance) + 
                                      rnorm(300, 0, 8)),
  climax_species_cover = pmax(0, 70 * (1 - exp(-0.05 * year_since_disturbance)) + 
                                rnorm(300, 0, 6)),
  soil_depth_cm = 10 + 0.8 * year_since_disturbance + rnorm(300, 0, 3),
  canopy_height_m = 2 + 0.6 * year_since_disturbance + rnorm(300, 0, 2),
  disturbance_type = rep(sample(c("Fire", "Logging", "Windstorm"), 25, 
                                replace = TRUE), each = 12),
  elevation_m = rep(sample(300:1200, 25, replace = TRUE), each = 12)
)

# YOUR TASK: Complete the data science workflow
# TIDY: Ensure data is ready for succession analysis

# TRANSFORM: 
# - Calculate total vegetation cover
# - Create succession stage categories based on dominant species
# - Calculate diversity indices for each plot and time point

# VISUALISE: 
# - Plot succession trajectories by disturbance type
# - Show changes in species composition over time

# MODEL: 
# - Model climax species cover as function of time and environmental factors
# - Predict succession stage based on multiple variables

# COMMUNICATE: 
# - How do different disturbance types affect succession patterns?
# - What environmental factors accelerate or slow succession?

################################################################################

# Exercise 5: Marine Biodiversity Assessment
# Analyze relationships between ocean conditions and marine biodiversity

# Create the dataset
marine_data <- tibble(
  station_id = rep(paste0("STN_", sprintf("%02d", 1:20)), each = 4),
  season = rep(c("Spring", "Summer", "Fall", "Winter"), 20),
  sea_surface_temp = rnorm(80, mean = 18, sd = 6),
  salinity_ppt = rnorm(80, mean = 35, sd = 2),
  chlorophyll_concentration = rlnorm(80, meanlog = 0.5, sdlog = 0.8),
  zooplankton_biomass = rlnorm(80, meanlog = 2, sdlog = 0.6),
  fish_species_richness = rpois(80, lambda = 15),
  fish_abundance = rpois(80, lambda = 200),
  depth_m = rep(sample(10:200, 20, replace = TRUE), each = 4),
  distance_to_shore_km = rep(runif(20, 1, 50), each = 4),
  upwelling_index = rnorm(80, mean = 0, sd = 1)
)

# YOUR TASK: Complete the data science workflow
# TIDY: Check data structure and seasonal factors

# TRANSFORM: 
# - Calculate Simpson's diversity index for fish communities
# - Create productivity categories based on chlorophyll concentration
# - Calculate fish density (abundance/area approximation)

# VISUALISE: 
# - Plot biodiversity patterns by distance from shore and depth
# - Show seasonal variation in key oceanographic variables

# MODEL: 
# - Predict fish species richness using oceanographic variables
# - Model the relationship between productivity and biodiversity

# COMMUNICATE: 
# - What oceanographic factors best predict marine biodiversity?
# - How do seasonal patterns affect species richness?

################################################################################
############### Advanced Data Science Workflow - Examples ####################
################################################################################

# Example 1: Multi-scale Biodiversity Analysis with Spatial Components
# Simulate hierarchical biodiversity data
set.seed(789)
biodiversity_data <- tibble(
  region_id = rep(1:5, each = 40),
  site_id = 1:200,
  longitude = runif(200, -120, -100),
  latitude = runif(200, 35, 45),
  alpha_diversity = rpois(200, lambda = 20),
  beta_diversity = rnorm(200, mean = 0.6, sd = 0.15),
  gamma_diversity = rpois(200, lambda = 60),
  habitat_heterogeneity = rnorm(200, mean = 2.5, sd = 0.8),
  human_impact_index = runif(200, 0, 1),
  climate_suitability = rnorm(200, mean = 0, sd = 1),
  connectivity_index = rnorm(200, mean = 0.5, sd = 0.2),
  area_km2 = rlnorm(200, meanlog = 2, sdlog = 1)
)

# TIDY: Add spatial reference and hierarchical structure
biodiversity_spatial <- biodiversity_data %>%
  mutate(
    region_name = case_when(
      region_id == 1 ~ "Northern_Mountains",
      region_id == 2 ~ "Central_Plains",
      region_id == 3 ~ "Southern_Desert",
      region_id == 4 ~ "Eastern_Forest",
      region_id == 5 ~ "Western_Coast"
    )
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# TRANSFORM: Multi-scale diversity calculations
biodiversity_transformed <- biodiversity_spatial %>%
  st_drop_geometry() %>%
  group_by(region_id) %>%
  mutate(
    regional_alpha_mean = mean(alpha_diversity),
    regional_beta_mean = mean(beta_diversity),
    site_alpha_deviation = alpha_diversity - regional_alpha_mean,
    diversity_ratio = alpha_diversity / gamma_diversity,
    scaled_connectivity = scale(connectivity_index)[,1]
  ) %>%
  ungroup() %>%
  mutate(
    conservation_priority = (alpha_diversity * 0.3) + 
      (connectivity_index * 0.3) +
      ((1 - human_impact_index) * 0.4)
  )

# VISUALISE: Multi-panel diversity relationships
ggplot(biodiversity_transformed, 
             aes(x = habitat_heterogeneity, y = alpha_diversity)) +
  geom_point(aes(size = area_km2), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~region_name) +
  ylab('Alpha diversity') +
  xlab('Habitat Heterogeneity') +
  theme_new()

# MODEL: Hierarchical modeling approach
diversity_recipe <- recipe(alpha_diversity ~ habitat_heterogeneity + 
                             human_impact_index + climate_suitability + 
                             connectivity_index + region_name + area_km2, 
                           data = biodiversity_transformed) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_interact(terms = ~ habitat_heterogeneity:starts_with("region_name"))

linear_model <- linear_reg() %>%
  set_engine("lm")

diversity_workflow <- workflow() %>%
  add_recipe(diversity_recipe) %>%
  add_model(linear_model)

diversity_fit <- diversity_workflow %>%
  fit(data = biodiversity_transformed)

# COMMUNICATE: Regional patterns and conservation implications
diversity_results <- tidy(diversity_fit) %>%
  filter(p.value < 0.05) %>%
  arrange(desc(abs(estimate))) %>%
  mutate(across(where(is.numeric), round, 3))

# Example 2: Community Assembly and Functional Diversity
# Simulate functional trait data and community assembly processes
trait_data <- tibble(
  species_id = paste0("Species_", sprintf("%03d", 1:100)),
  body_size_log = rnorm(100, mean = 2, sd = 1),
  trophic_level = sample(1:4, 100, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
  dispersal_ability = runif(100, 0.1, 1),
  habitat_specialization = rbeta(100, 2, 2),
  thermal_tolerance = rnorm(100, mean = 15, sd = 5),
  reproductive_rate = rlnorm(100, meanlog = 0, sdlog = 0.8)
)

# Community composition across environmental gradients
community_data <- expand_grid(
  site_id = 1:50,
  species_id = trait_data$species_id
) %>%
  left_join(trait_data, by = "species_id") %>%
  mutate(
    temperature = rep(seq(5, 25, length.out = 50), each = 100),
    habitat_complexity = rep(runif(50, 0, 1), each = 100),
    resource_availability = rep(rnorm(50, mean = 0, sd = 1), each = 100)
  ) %>%
  mutate(
    # Probability of occurrence based on environmental filtering
    occurrence_prob = plogis(
      -2 + 
        0.5 * (1 - abs(thermal_tolerance - temperature) / 10) +
        0.3 * (1 - abs(habitat_specialization - habitat_complexity)) +
        0.2 * resource_availability * (trophic_level == 1)
    ),
    present = rbinom(n(), 1, occurrence_prob),
    abundance = ifelse(present == 1, rpois(n(), lambda = 10), 0)
  )

# TIDY: Community matrix and site-level metrics
community_matrix <- community_data %>%
  filter(present == 1) %>%
  select(site_id, species_id, abundance) %>%
  pivot_wider(names_from = species_id, values_from = abundance, 
              values_fill = 0)

site_metrics <- community_data %>%
  group_by(site_id) %>%
  summarise(
    species_richness = sum(present),
    total_abundance = sum(abundance),
    mean_body_size = weighted.mean(body_size_log, abundance, na.rm = TRUE),
    functional_dispersion = sd(body_size_log[present == 1], na.rm = TRUE),
    trophic_diversity = length(unique(trophic_level[present == 1])),
    temperature = first(temperature),
    habitat_complexity = first(habitat_complexity),
    resource_availability = first(resource_availability),
    .groups = "drop"
  ) %>%
  mutate(
    functional_evenness = ifelse(species_richness > 1, 
                                 functional_dispersion / log(species_richness), 
                                 0)
  )

# TRANSFORM: Functional diversity indices
trait_summary <- trait_data %>%
  mutate(
    size_category = case_when(
      body_size_log < 1 ~ "Small",
      body_size_log >= 1 & body_size_log < 3 ~ "Medium",
      body_size_log >= 3 ~ "Large"
    )
  )

# VISUALISE: Environmental filtering effects
ggplot(site_metrics, aes(x = temperature, y = species_richness)) +
  geom_point(aes(size = habitat_complexity, color = resource_availability)) +
  scale_color_gradient2(low = "red", mid = "white", high = "blue") +
  labs(title = "Species Richness Along Temperature Gradient",
       x = "Temperature (°C)", y = "Species Richness") +
  theme_new()

# MODEL: Community assembly drivers
assembly_recipe <- recipe(species_richness ~ temperature + habitat_complexity + 
                            resource_availability, data = site_metrics) %>%
  step_poly(temperature, degree = 2) %>%
  step_normalize(all_numeric_predictors()) 

gam_model <- poisson_reg() %>%
  set_engine("glm") %>%
  set_mode("regression")

assembly_workflow <- workflow() %>%
  add_recipe(assembly_recipe) %>%
  add_model(gam_model)

assembly_fit <- assembly_workflow %>%
  fit(data = site_metrics)

# COMMUNICATE: Assembly rules and functional patterns
functional_summary <- community_data %>%
  group_by(site_id) %>%
  filter(present == 1) %>%
  summarise(
    mean_dispersal = mean(dispersal_ability),
    mean_specialization = mean(habitat_specialization),
    trophic_structure = paste(sort(unique(trophic_level)), collapse = "-"),
    .groups = "drop"
  ) %>%
  left_join(site_metrics, by = "site_id")


# Example 3: Ecosystem Services Valuation and Trade-offs
# Simulate ecosystem services across landscape mosaic
ecosystem_services <- tibble(
  grid_cell_id = 1:500,
  x_coord = rep(1:25, each = 20),
  y_coord = rep(1:20, 25),
  land_use = sample(c("Forest", "Agriculture", "Urban", "Wetland", "Grassland"), 
                    500, replace = TRUE, prob = c(0.3, 0.25, 0.15, 0.1, 0.2)),
  elevation = rnorm(500, mean = 200, sd = 100),
  slope_percent = abs(rnorm(500, mean = 15, sd = 10)),
  soil_quality = rnorm(500, mean = 0, sd = 1),
  water_access = rbinom(500, 1, 0.3),
  human_population = rpois(500, lambda = 100)
)

# TIDY: Add landscape context
landscape_data <- ecosystem_services %>%
  mutate(
    # Distance to urban areas (simplified)
    distance_to_urban = pmin(
      sqrt((x_coord - 12)^2 + (y_coord - 10)^2) * 100,
      sqrt((x_coord - 20)^2 + (y_coord - 15)^2) * 100
    ),
    land_use = factor(land_use)
  )

# TRANSFORM: Calculate ecosystem service values
services_calculated <- landscape_data %>%
  mutate(
    # Carbon sequestration (tonnes C/ha/year)
    carbon_sequestration = case_when(
      land_use == "Forest" ~ 5 + rnorm(n(), 0, 1),
      land_use == "Wetland" ~ 3 + rnorm(n(), 0, 0.5),
      land_use == "Grassland" ~ 2 + rnorm(n(), 0, 0.5),
      land_use == "Agriculture" ~ 1 + rnorm(n(), 0, 0.3),
      land_use == "Urban" ~ 0.5 + rnorm(n(), 0, 0.2)
    ),
    
    # Water regulation (mm/year)
    water_regulation = case_when(
      land_use == "Wetland" ~ 500 + rnorm(n(), 0, 50),
      land_use == "Forest" ~ 300 + rnorm(n(), 0, 30),
      land_use == "Grassland" ~ 200 + rnorm(n(), 0, 20),
      land_use == "Agriculture" ~ 100 + rnorm(n(), 0, 15),
      land_use == "Urban" ~ 50 + rnorm(n(), 0, 10)
    ) * (1 + 0.3 * water_access),
    
    # Food production (tonnes/ha/year)
    food_production = case_when(
      land_use == "Agriculture" ~ 8 + 2 * soil_quality + rnorm(n(), 0, 1),
      land_use == "Grassland" ~ 2 + soil_quality + rnorm(n(), 0, 0.5),
      TRUE ~ 0
    ),
    
    # Recreation value (visitor-days/year)
    recreation_value = case_when(
      land_use == "Forest" ~ 100 + rnorm(n(), 0, 20),
      land_use == "Wetland" ~ 80 + rnorm(n(), 0, 15),
      land_use == "Grassland" ~ 50 + rnorm(n(), 0, 10),
      TRUE ~ 10 + rnorm(n(), 0, 5)
    ) * exp(-distance_to_urban / 1000),
    
    # Biodiversity index (species equivalent)
    biodiversity_index = case_when(
      land_use == "Forest" ~ 85 + rnorm(n(), 0, 15),
      land_use == "Wetland" ~ 75 + rnorm(n(), 0, 12),
      land_use == "Grassland" ~ 60 + rnorm(n(), 0, 10),
      land_use == "Agriculture" ~ 25 + rnorm(n(), 0, 8),
      land_use == "Urban" ~ 15 + rnorm(n(), 0, 5)
    )
  ) %>%
  # Economic valuation ($/ha/year)
  mutate(
    carbon_value = carbon_sequestration * 30,  # $30/tonne CO2
    water_value = water_regulation * 0.001,    # $0.001/mm
    food_value = food_production * 500,        # $500/tonne
    recreation_value_economic = recreation_value * 25,  # $25/visitor-day
    biodiversity_value = biodiversity_index * 2,       # $2/species equivalent
    
    total_ecosystem_value = carbon_value + water_value + food_value + 
      recreation_value_economic + biodiversity_value
  )

# VISUALISE: Service trade-offs and spatial patterns
services_long <- services_calculated %>%
  select(grid_cell_id, land_use, carbon_sequestration, water_regulation, 
         food_production, recreation_value, biodiversity_index) %>%
  pivot_longer(cols = c(carbon_sequestration, water_regulation, 
                        food_production, recreation_value, biodiversity_index),
               names_to = "service_type", values_to = "service_value")

ggplot(services_long, aes(x = land_use, y = service_value)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~service_type, scales = "free") +
  labs(title = "Ecosystem Services by Land Use Type",
       x = "Land Use", y = "Service Value") + 
  theme_new() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# MODEL: Service provisioning drivers
service_recipe <- recipe(total_ecosystem_value ~ land_use + elevation + 
                           slope_percent + soil_quality + water_access + 
                           distance_to_urban, data = services_calculated) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_interact(terms = ~ soil_quality:starts_with("land_use"))

rf_model <- rand_forest(trees = 500) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

service_workflow <- workflow() %>%
  add_recipe(service_recipe) %>%
  add_model(rf_model)

service_fit <- service_workflow %>%
  fit(data = services_calculated)

# COMMUNICATE: Land use optimization and trade-offs
service_importance <- service_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 10)

# Example 4: Climate Change Impact Assessment
# Simulate species distribution shifts under climate scenarios
climate_impact <- tibble(
  species_id = rep(paste0("Species_", LETTERS[1:15]), each = 200),
  grid_cell = rep(1:200, 15),
  current_temp = rep(rnorm(200, mean = 12, sd = 6), 15),
  current_precip = rep(rnorm(200, mean = 800, sd = 300), 15),
  future_temp_rcp45 = rep(rnorm(200, mean = 15, sd = 6), 15),
  future_precip_rcp45 = rep(rnorm(200, mean = 750, sd = 320), 15),
  future_temp_rcp85 = rep(rnorm(200, mean = 18, sd = 7), 15),
  future_precip_rcp85 = rep(rnorm(200, mean = 700, sd = 350), 15),
  elevation = rep(sample(0:2000, 200, replace = TRUE), 15),
  habitat_quality = rep(runif(200, 0, 1), 15)
)

# Species-specific climate preferences
species_traits <- tibble(
  species_id = paste0("Species_", LETTERS[1:15]),
  temp_optimum = runif(15, 8, 20),
  temp_tolerance = runif(15, 2, 8),
  precip_optimum = runif(15, 400, 1200),
  precip_tolerance = runif(15, 100, 400),
  dispersal_rate = runif(15, 0.1, 5),
  habitat_specificity = runif(15, 0.2, 0.9)
)

# TIDY: Combine species traits with environmental data
climate_data <- climate_impact %>%
  left_join(species_traits, by = "species_id")

# TRANSFORM: Calculate habitat suitability under different scenarios
suitability_data <- climate_data %>%
  mutate(
    # Current suitability
    temp_suitability_current = exp(-((current_temp - temp_optimum)^2) / 
                                     (2 * temp_tolerance^2)),
    precip_suitability_current = exp(-((current_precip - precip_optimum)^2) / 
                                       (2 * precip_tolerance^2)),
    current_suitability = temp_suitability_current * precip_suitability_current * 
      habitat_quality,
    
    # RCP 4.5 scenario
    temp_suitability_rcp45 = exp(-((future_temp_rcp45 - temp_optimum)^2) / 
                                   (2 * temp_tolerance^2)),
    precip_suitability_rcp45 = exp(-((future_precip_rcp45 - precip_optimum)^2) / 
                                     (2 * precip_tolerance^2)),
    rcp45_suitability = temp_suitability_rcp45 * precip_suitability_rcp45 * 
      habitat_quality,
    
    # RCP 8.5 scenario
    temp_suitability_rcp85 = exp(-((future_temp_rcp85 - temp_optimum)^2) / 
                                   (2 * temp_tolerance^2)),
    precip_suitability_rcp85 = exp(-((future_precip_rcp85 - precip_optimum)^2) / 
                                     (2 * precip_tolerance^2)),
    rcp85_suitability = temp_suitability_rcp85 * precip_suitability_rcp85 * 
      habitat_quality,
    
    # Changes in suitability
    change_rcp45 = rcp45_suitability - current_suitability,
    change_rcp85 = rcp85_suitability - current_suitability,
    percent_change_rcp45 = (change_rcp45 / current_suitability) * 100,
    percent_change_rcp85 = (change_rcp85 / current_suitability) * 100
  )

# VISUALISE: Climate change impacts by species
impact_summary <- suitability_data %>%
  group_by(species_id) %>%
  summarise(
    current_range_size = sum(current_suitability > 0.1),
    rcp45_range_size = sum(rcp45_suitability > 0.1),
    rcp85_range_size = sum(rcp85_suitability > 0.1),
    range_change_rcp45 = (rcp45_range_size - current_range_size) / 
      current_range_size * 100,
    range_change_rcp85 = (rcp85_range_size - current_range_size) / 
      current_range_size * 100,
    mean_suitability_change_rcp45 = mean(change_rcp45, na.rm = TRUE),
    mean_suitability_change_rcp85 = mean(change_rcp85, na.rm = TRUE),
    temp_optimum = first(temp_optimum),
    dispersal_rate = first(dispersal_rate),
    .groups = "drop"
  )

ggplot(impact_summary, aes(x = temp_optimum, y = range_change_rcp85, 
                           size = dispersal_rate)) +
  geom_point(alpha = 0.7, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Projected Range Changes Under RCP 8.5",
       x = "Temperature Optimum (°C)", 
       y = "Range Size Change (%)") +
  theme_

# MODEL: Predict climate change vulnerability
colnames(impact_summary)
vulnerability_recipe <- recipe(temp_optimum ~ range_change_rcp85 + 
                               dispersal_rate,
                               data = impact_summary) %>%
  step_normalize(all_numeric_predictors())

vulnerability_model <- linear_reg() %>%
  set_engine("lm")

vulnerability_workflow <- workflow() %>%
  add_recipe(vulnerability_recipe) %>%
  add_model(vulnerability_model)

vulnerability_fit <- vulnerability_workflow %>%
  fit(data = impact_summary)

# COMMUNICATE: Conservation prioritization under climate change
vulnerability_results <- tidy(vulnerability_fit) %>%
  mutate(across(where(is.numeric), round, 3))
