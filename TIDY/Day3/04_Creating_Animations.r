####################################################################################################
###
### File:    04_Creating_Animations.R
### Purpose: Examples and exercises for creating animations
### Authors: Gabriel Rodrigues Palma
### Date:    19/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
######################### Creating Animations with gganimate #################
################################################################################

# Example 1: Animated Species Population Growth Over Time
# Simulate population data
years <- 2010:2020
species_data <- expand.grid(
  year = years,
  species = c("Quercus alba", "Acer rubrum", "Betula papyrifera")
) %>%
  mutate(
    population = case_when(
      species == "Quercus alba" ~ 1000 + year * 15 + rnorm(n(), 0, 20),
      species == "Acer rubrum" ~ 800 + year * 12 + rnorm(n(), 0, 15),
      species == "Betula papyrifera" ~ 600 + year * 8 + rnorm(n(), 0, 10)
    )
  )

p1 <- ggplot(species_data, aes(x = year, y = population, color = species)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Forest Species Population: Year {closest_state}",
       x = "Year", y = "Population Count") +
  transition_reveal(year)


# Example 2: Animated Ecosystem Succession Stages
succession_data <- expand.grid(
  year = 1:50,
  stage = c("Pioneer", "Early Secondary", "Late Secondary", "Climax")
) %>%
  mutate(
    coverage = case_when(
      stage == "Pioneer" ~ pmax(0, 80 - year * 2 + rnorm(n(), 0, 5)),
      stage == "Early Secondary" ~ pmax(0, pmin(60, year * 1.5 - 10 + 
                                                  rnorm(n(), 0, 3))),
      stage == "Late Secondary" ~ pmax(0, pmin(40, year * 0.8 - 20 + 
                                                 rnorm(n(), 0, 2))),
      stage == "Climax" ~ pmax(0, pmin(30, year * 0.6 - 30 + rnorm(n(), 0, 2)))
    ),
    # Convert stage to factor with proper order
    stage = factor(stage, levels = c("Pioneer", "Early Secondary", 
                                     "Late Secondary", "Climax"))
  )

# Create animated plot with stat = "identity"
p3 <- ggplot(succession_data, aes(x = year, y = coverage, fill = stage)) +
  geom_area(alpha = 0.7, stat = "identity") +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Ecological Succession: Year {closest_state}",
       x = "Years Since Disturbance", y = "Vegetation Coverage (%)",
       fill = "Succession Stage") +
  transition_reveal(year)

# Render animation
anim <- animate(p3, width = 800, height = 600, fps = 10, duration = 8)
anim

# Example 3: Animated Predator-Prey Dynamics
predator_prey <- data.frame(
  time = rep(seq(0, 20, 0.5), 2),
  species = rep(c("Lepus americanus", "Lynx canadensis"), each = 41),
  population = c(
    10 + 8 * sin(seq(0, 20, 0.5)) + rnorm(41, 0, 0.5),
    5 + 3 * sin(seq(0, 20, 0.5) + pi/4) + rnorm(41, 0, 0.3)
  )
)

p4 <- ggplot(predator_prey, aes(x = time, y = population, color = species)) +
  geom_line(size = 1.5) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Predator-Prey Dynamics: Time {closest_state}",
       x = "Time (years)", y = "Population (thousands)") +
  transition_reveal(time)

# Example 4: Animated Pollinator Network Changes
pollinator_data <- expand.grid(
  week = 1:20,
  plant = c("Helianthus annuus", "Rudbeckia hirta", "Echinacea purpurea"),
  pollinator = c("Apis mellifera", "Bombus impatiens", "Megachile rotundata")
) %>%
  mutate(
    interactions = rpois(180, lambda = 5 + 3 * sin(week/3) + rnorm(n(), 0, 1))
  )

p5 <- ggplot(pollinator_data, aes(x = plant, y = pollinator, 
                                  size = interactions)) +
  geom_point(alpha = 0.7, color = "orange") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Pollinator Network: Week {closest_state}",
       x = "Plant Species", y = "Pollinator Species",
       size = "Interactions") +
  transition_states(week, transition_length = 1, state_length = 1)

################################################################################
####################### Creating Animations with gganimate Exercises ##################################
################################################################################

# Exercise 1: Create animated carbon sequestration over forest age
# Create a visualization showing how different forest types accumulate carbon
# over time (0-100 years). The animation should reveal the carbon accumulation
# curves progressively as forests age.

# Here's the data you'll need:
library(tidyverse)
library(gganimate)

# Create carbon sequestration dataset
carbon_seq_data <- expand.grid(
  forest_age = 0:100,
  forest_type = c("Tropical Rainforest", "Temperate Deciduous", 
                  "Boreal Forest", "Mangrove Forest")
) %>%
  mutate(carbon_storage = case_when(
    forest_type == "Tropical Rainforest" ~ 
      200 * (1 - exp(-0.03 * forest_age)) + rnorm(n(), 0, 5),
    forest_type == "Temperate Deciduous" ~ 
      150 * (1 - exp(-0.025 * forest_age)) + rnorm(n(), 0, 4),
    forest_type == "Boreal Forest" ~ 
      100 * (1 - exp(-0.02 * forest_age)) + rnorm(n(), 0, 3),
    forest_type == "Mangrove Forest" ~ 
      180 * (1 - exp(-0.035 * forest_age)) + rnorm(n(), 0, 4.5)
  ))

# Your task: Create an animated plot showing carbon accumulation over time
# Use transition_reveal() to progressively show the accumulation curves
# Add appropriate labels, colors, and a title
# Hint: geom_line() and geom_point() will be useful here

# Your code here:



# Exercise 2: Animate seasonal changes in leaf area index (LAI)
# Create an animation showing how leaf area index changes throughout the year
# for different tree species, highlighting the contrast between deciduous and
# evergreen species.

# Here's the data you'll need:
lai_data <- expand.grid(
  month = 1:12,
  species = c("Quercus alba", "Acer saccharum", "Fagus grandifolia", 
              "Pinus strobus", "Picea abies")
) %>%
  mutate(
    type = case_when(
      species %in% c("Quercus alba", "Acer saccharum", "Fagus grandifolia") ~ 
        "Deciduous",
      TRUE ~ "Evergreen"
    ),
    lai = case_when(
      # Deciduous species with seasonal patterns
      species == "Quercus alba" ~ # White Oak
        ifelse(month %in% c(11, 12, 1, 2, 3), 
               runif(1, 0, 0.5), # Winter dormancy
               ifelse(month %in% c(4, 10), 
                      runif(1, 2, 4), # Spring/Fall transition
                      runif(1, 4.5, 6))), # Summer full leaf
      species == "Acer saccharum" ~ # Sugar Maple
        ifelse(month %in% c(11, 12, 1, 2, 3), 
               runif(1, 0, 0.3), # Winter dormancy
               ifelse(month %in% c(4, 10), 
                      runif(1, 2.5, 4.5), # Spring/Fall transition
                      runif(1, 5, 6.5))), # Summer full leaf
      species == "Fagus grandifolia" ~ # American Beech
        ifelse(month %in% c(11, 12, 1, 2, 3), 
               runif(1, 0, 0.4), # Winter dormancy
               ifelse(month %in% c(4, 10), 
                      runif(1, 1.8, 3.8), # Spring/Fall transition
                      runif(1, 4, 5.5))), # Summer full leaf
      # Evergreen species with less dramatic seasonal changes
      species == "Pinus strobus" ~ # White Pine
        ifelse(month %in% c(12, 1, 2), 
               runif(1, 2.5, 3.5), # Winter slight reduction
               ifelse(month %in% c(5, 6, 7, 8), 
                      runif(1, 3.8, 4.8), # Summer peak
                      runif(1, 3.2, 4.2))), # Spring/Fall
      species == "Picea abies" ~ # Norway Spruce
        ifelse(month %in% c(12, 1, 2), 
               runif(1, 3, 4), # Winter slight reduction
               ifelse(month %in% c(5, 6, 7, 8), 
                      runif(1, 4.5, 5.5), # Summer peak
                      runif(1, 3.8, 4.8))) # Spring/Fall
      

################################################################################
################### Advanced gganimate Techniques ############################
################################################################################

# Example 1: Animated Species Distribution with Climate Change
climate_species <- expand.grid(
  year = seq(1970, 2020, 5),
  latitude = seq(30, 60, 2),
  longitude = seq(-120, -80, 2)
) %>%
  mutate(
    temperature = 15 + (year - 1970) * 0.03 + (latitude - 45) * (-0.5) + 
      rnorm(n(), 0, 2),
    suitability = pmax(0, pmin(1, 
                               exp(-(temperature - 18)^2 / 20) + rnorm(n(), 0, 0.1)))
  ) %>%
  filter(suitability > 0.3)

p6 <- ggplot(climate_species, aes(x = longitude, y = latitude, 
                                  color = suitability)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_viridis_c(name = "Habitat\nSuitability") +
  theme_minimal() +
  labs(title = "Species Distribution Change: {closest_state}",
       x = "Longitude", y = "Latitude") +
  transition_states(year, transition_length = 2, state_length = 1)

# Example 2: Animated Food Web Complexity
food_web <- data.frame(
  time = rep(1:15, each = 6),
  trophic_level = rep(c("Primary Producer", "Primary Consumer", 
                        "Secondary Consumer", "Tertiary Consumer",
                        "Decomposer", "Apex Predator"), 15),
  biomass = c(
    rep(c(1000, 500, 250, 125, 300, 50), 15) + 
      rnorm(90, 0, 50) + rep(sin(1:15/3) * 100, each = 6)
  ),
  species_count = round(c(
    rep(c(50, 30, 15, 8, 25, 3), 15) + 
      rnorm(90, 0, 5) + rep(cos(1:15/2) * 5, each = 6)
  ))
)

p7 <- ggplot(food_web, aes(x = trophic_level, y = biomass, 
                           size = species_count)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Food Web Dynamics: Time Step {closest_state}",
       x = "Trophic Level", y = "Biomass (kg/ha)",
       size = "Species Count") +
  transition_states(time, transition_length = 1, state_length = 2)

# Example 3: Animated Phenology Tracking
phenology <- expand.grid(
  day_of_year = 1:365,
  species = c("Acer saccharum", "Quercus rubra", "Betula alleghaniensis")
) %>%
  mutate(
    leaf_emergence = case_when(
      species == "Acer saccharum" ~ pmax(0, pmin(1, 
                                                 (day_of_year - 120) / 20 - (day_of_year - 120)^2 / 2000)),
      species == "Quercus rubra" ~ pmax(0, pmin(1, 
                                                (day_of_year - 130) / 25 - (day_of_year - 130)^2 / 2500)),
      species == "Betula alleghaniensis" ~ pmax(0, pmin(1, 
                                                        (day_of_year - 110) / 18 - (day_of_year - 110)^2 / 1800))
    )
  ) %>%
  filter(leaf_emergence > 0)

p8 <- ggplot(phenology, aes(x = day_of_year, y = leaf_emergence, 
                            color = species)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Leaf Emergence Phenology: Day {closest_state}",
       x = "Day of Year", y = "Leaf Emergence (0-1)") +
  transition_reveal(day_of_year)

# Example 4: Animated Ecosystem Services Valuation
ecosystem_services <- expand.grid(
  year = 2000:2020,
  service = c("Carbon Storage", "Water Purification", "Biodiversity", 
              "Soil Formation", "Pollination")
) %>%
  mutate(
    value = case_when(
      service == "Carbon Storage" ~ 5000 + year * 50 + rnorm(n(), 0, 200),
      service == "Water Purification" ~ 3000 + year * 30 + rnorm(n(), 0, 150),
      service == "Biodiversity" ~ 4000 + year * 20 + rnorm(n(), 0, 180),
      service == "Soil Formation" ~ 2000 + year * 40 + rnorm(n(), 0, 100),
      service == "Pollination" ~ 1500 + year * 25 + rnorm(n(), 0, 80)
    )
  )

p9 <- ggplot(ecosystem_services, aes(x = service, y = value, fill = service)) +
  geom_col(alpha = 0.8) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Ecosystem Services Value: {closest_state}",
       x = "Service Type", y = "Value (USD/hectare/year)") +
  transition_states(year, transition_length = 1, state_length = 1)

# Example 5: Animated Species Interaction Network
interaction_data <- expand.grid(
  season = 1:4,
  species_a = c("Lynx", "Wolf", "Bear", "Deer", "Rabbit"),
  species_b = c("Lynx", "Wolf", "Bear", "Deer", "Rabbit")
) %>%
  filter(species_a != species_b) %>%
  mutate(
    interaction_strength = case_when(
      (species_a == "Lynx" & species_b == "Rabbit") ~ 0.8 + season * 0.1,
      (species_a == "Wolf" & species_b == "Deer") ~ 0.7 + season * 0.05,
      (species_a == "Bear" & species_b == "Deer") ~ 0.4 + season * 0.15,
      TRUE ~ runif(n(), 0, 0.3)
    )
  ) %>%
  filter(interaction_strength > 0.3)

p10 <- ggplot(interaction_data, aes(x = species_a, y = species_b, 
                                    size = interaction_strength)) +
  geom_point(alpha = 0.7, color = "red") +
  theme_minimal() +
  labs(title = "Species Interactions: Season {closest_state}",
       x = "Predator", y = "Prey", size = "Interaction\nStrength") +
  transition_states(season, transition_length = 1, state_length = 2)

################################################################################
################# Advanced gganimate Exercises ###############################
################################################################################

# Exercise 1: Create animated temperature gradient effects on species
# Create a visualization showing how temperature changes affect multiple species 
# distributions across an elevation gradient. The animation should demonstrate 
# how species ranges shift as temperatures increase over time.

# First, create the dataset:
# Generate temperature gradient data across years, elevations, and species
set.seed(123)
temp_gradient_data <- expand.grid(
  year = 2000:2050,
  elevation = seq(0, 3000, by = 500),  # elevation in meters
  species = c("Pinus ponderosa", "Abies concolor", "Quercus kelloggii", 
              "Pseudotsuga menziesii")
) %>%
  mutate(
    # Base temperature decreases with elevation (lapse rate)
    base_temp = 25 - (elevation/100) * 0.65,
    
    # Temperature increases over time (climate change)
    annual_temp = base_temp + (year - 2000) * 0.04,
    
    # Species-specific temperature optimum and tolerance
    temp_optimum = case_when(
      species == "Pinus ponderosa" ~ 18,
      species == "Abies concolor" ~ 12,
      species == "Quercus kelloggii" ~ 15,
      species == "Pseudotsuga menziesii" ~ 14
    ),
    
    temp_tolerance = case_when(
      species == "Pinus ponderosa" ~ 5,
      species == "Abies concolor" ~ 3,
      species == "Quercus kelloggii" ~ 4,
      species == "Pseudotsuga menziesii" ~ 4.5
    ),
    
    # Calculate suitability based on distance from optimum temperature
    suitability = exp(-(annual_temp - temp_optimum)^2 / (2 * temp_tolerance^2))
  )

# Your task: Create an animated plot showing how species distributions shift
# along the elevation gradient as temperatures change over time.
# Use transition_time(year) for smooth animation.
# Hint: Try using geom_line() or geom_area() with suitability on the y-axis
# and elevation on the x-axis, with color or fill mapped to species.

# Your code here:



# Exercise 2: Animate competitive exclusion principle
# Create an animation demonstrating the competitive exclusion principle, showing
# how two species competing for the same resource cannot coexist indefinitely
# if their ecological niches completely overlap.

# Create dataset for two competing species over time:
set.seed(456)
# Parameters
K1 <- 1000  # Carrying capacity for species 1
K2 <- 900   # Carrying capacity for species 2
r1 <- 0.2   # Growth rate for species 1
r2 <- 0.18  # Growth rate for species 2
alpha12 <- 1.2  # Effect of species 2 on species 1
alpha21 <- 0.9  # Effect of species 1 on species 2

# Generate population dynamics using Lotka-Volterra competition model
competition_data <- data.frame(time = 0, N1 = 100, N2 = 100)

for(t in 1:100) {
  last <- nrow(competition_data)
  N1 <- competition_data$N1[last]
  N2 <- competition_data$N2[last]
  
  # Lotka-Volterra competition equations
  dN1 <- r1 * N1 * (1 - (N1 + alpha12 * N2) / K1)
  dN2 <- r2 * N2 * (1 - (N2 + alpha21 * N1) / K2)
  
  # Add some stochasticity
  dN1 <- dN1 + rnorm(1, 0, 5)
  dN2 <- dN2 + rnorm(1, 0, 5)
  
  # Update populations
  N1_new <- max(0, N1 + dN1)
  N2_new <- max(0, N2 + dN2)
  
  competition_data <- rbind(competition_data, 
                            data.frame(time = t, N1 = N1_new, N2 = N2_new))
}

# Convert to long format for easier plotting
competition_long <- competition_data %>%
  pivot_longer(cols = c(N1, N2), 
               names_to = "species", 
               values_to = "population")

# Your task: Create an animated plot showing the population dynamics of both
# species over time, demonstrating competitive exclusion or coexistence.
# Include a phase plot (N1 vs N2) that updates as time progresses.
# Use transition_time(time) for smooth animation.

# Your code here:



# Exercise 3: Create animated nutrient cycling visualization
# Develop an animation showing how nutrients (N, P, C) cycle through different
# ecosystem compartments, with arrows indicating flux rates that change seasonally.

# Create dataset for nutrient pools and fluxes:
set.seed(789)
# Generate seasonal nutrient cycling data
seasons <- c("Winter", "Spring", "Summer", "Fall")
nutrient_cycles <- expand.grid(
  year = 2020:2022,
  season = seasons,
  nutrient = c("Nitrogen", "Phosphorus", "Carbon")
) %>%
  mutate(
    season_num = case_when(
      season == "Winter" ~ 1,
      season == "Spring" ~ 2,
      season == "Summer" ~ 3,
      season == "Fall" ~ 4
    ),
    time_point = (year - 2020) * 4 + season_num
  )

# Create compartment data
compartments <- data.frame(
  compartment = c("Soil", "Vegetation", "Litter", "Atmosphere", "Water"),
  x_pos = c(2, 4, 3, 1, 5),
  y_pos = c(1, 3, 2, 4, 1.5)
)

# Create flux data between compartments
set.seed(101)
flux_data <- expand.grid(
  from = compartments$compartment,
  to = compartments$compartment,
  nutrient = c("Nitrogen", "Phosphorus", "Carbon"),
  time_point = 1:12
) %>%
  filter(from != to) %>%  # Remove self-loops
  # Remove some impossible pathways
  filter(!(from == "Atmosphere" & to == "Water" & nutrient == "Phosphorus")) %>%
  filter(!(from == "Water" & to == "Atmosphere" & nutrient == "Phosphorus")) %>%
  # Add seasonal flux rates
  mutate(
    base_rate = case_when(
      nutrient == "Carbon" ~ runif(n(), 0.5, 2),
      nutrient == "Nitrogen" ~ runif(n(), 0.2, 1),
      nutrient == "Phosphorus" ~ runif(n(), 0.1, 0.5)
    ),
    # Add seasonal variation
    season_effect = case_when(
      (time_point %% 4) == 1 ~ 0.7,  # Winter
      (time_point %% 4) == 2 ~ 1.5,  # Spring
      (time_point %% 4) == 3 ~ 1.2,  # Summer
      (time_point %% 4) == 0 ~ 0.9   # Fall
    ),
    flux_rate = base_rate * season_effect
  ) %>%
  # Join with compartment positions
  left_join(compartments, by = c("from" = "compartment")) %>%
  rename(x_from = x_pos, y_from = y_pos) %>%
  left_join(compartments, by = c("to" = "compartment")) %>%
  rename(x_to = x_pos, y_to = y_pos)

# Your task: Create an animated network diagram showing nutrient flows
# between ecosystem compartments, with arrow thickness representing flux rates.
# Use transition_states(time_point) to show seasonal changes.
# Hint: Use geom_segment() with arrows for fluxes and geom_point() or geom_text()
# for compartments.

# Your code here:



# Exercise 4: Animate habitat fragmentation effects
# Create an animation showing how increasing habitat fragmentation affects
# species abundance and diversity over time, with metrics changing as
# fragmentation increases.

# Create dataset for habitat fragmentation:
set.seed(246)
# Generate fragmentation progression data
years <- 1980:2020
fragmentation_data <- data.frame(
  year = years,
  habitat_area = 1000 - (years - 1980) * 5,  # Declining habitat area
  patch_count = 1 + floor((years - 1980) / 5),  # Increasing number of patches
  mean_patch_size = (1000 - (years - 1980) * 5) / (1 + floor((years - 1980) / 5)),
  connectivity_index = pmax(0, 1 - (years - 1980) * 0.025)  # Declining connectivity
)

# Generate species response data
species_response <- expand.grid(
  year = years,
  species = c("Forest specialist", "Edge specialist", "Generalist", 
              "Disturbance-sensitive", "Disturbance-tolerant")
) %>%
  left_join(fragmentation_data, by = "year") %>%
  mutate(
    # Different responses to fragmentation based on species traits
    abundance = case_when(
      species == "Forest specialist" ~ 
        100 * (habitat_area/1000)^1.5 * connectivity_index^0.8 + rnorm(n(), 0, 5),
      species == "Edge specialist" ~ 
        50 + 30 * patch_count * (1 - connectivity_index) + rnorm(n(), 0, 8),
      species == "Generalist" ~ 
        80 + rnorm(n(), 0, 10),  # Relatively stable
      species == "Disturbance-sensitive" ~ 
        120 * connectivity_index^2 * (habitat_area/1000) + rnorm(n(), 0, 7),
      species == "Disturbance-tolerant" ~ 
        40 + 60 * (1 - habitat_area/1000) + rnorm(n(), 0, 6)
    ),
    # Ensure abundance is non-negative
    abundance = pmax(0, abundance)
  )

# Your task: Create an animated visualization showing how habitat fragmentation
# affects species abundance over time. Include a secondary plot showing changes
# in landscape metrics (patch size, connectivity).
# Use transition_reveal(year) to show temporal progression.

# Your code here:



# Exercise 5: Create animated climate envelope shifts
# Visualize how species' climate envelopes (suitable temperature and precipitation
# combinations) shift over time with climate change, showing range contractions
# or expansions.

# Create climate envelope dataset:
set.seed(357)
# Generate climate data over time
climate_years <- 2000:2100
climate_data <- expand.grid(
  year = climate_years,
  species = c("Fagus sylvatica", "Picea abies", "Quercus ilex", 
              "Pinus sylvestris", "Betula pendula")
)

# Add species-specific climate parameters
climate_envelopes <- climate_data %>%
  mutate(
    # Temperature optimum and range for each species
    temp_optimum_base = case_when(
      species == "Fagus sylvatica" ~ 8,      # European beech
      species == "Picea abies" ~ 4,          # Norway spruce
      species == "Quercus ilex" ~ 15,        # Holm oak
      species == "Pinus sylvestris" ~ 6,     # Scots pine
      species == "Betula pendula" ~ 7        # Silver birch
    ),
    
    # Temperature optimum shifts with climate change
    temp_optimum = temp_optimum_base + (year - 2000) * 0.03,
    
    temp_min = temp_optimum - case_when(
      species == "Fagus sylvatica" ~ 5,
      species == "Picea abies" ~ 6,
      species == "Quercus ilex" ~ 7,
      species == "Pinus sylvestris" ~ 8,
      species == "Betula pendula" ~ 6
    ),
    
    temp_max = temp_optimum + case_when(
      species == "Fagus sylvatica" ~ 5,
      species == "Picea abies" ~ 4,
      species == "Quercus ilex" ~ 8,
      species == "Pinus sylvestris" ~ 6,
      species == "Betula pendula" ~ 5
    ),
    
    # Precipitation optimum and range
    precip_optimum_base = case_when(
      species == "Fagus sylvatica" ~ 900,
      species == "Picea abies" ~ 800,
      species == "Quercus ilex" ~ 600,
      species == "Pinus sylvestris" ~ 650,
      species == "Betula pendula" ~ 700
    ),
    
    # Precipitation optimum shifts with climate change (some regions getting drier)
    precip_optimum = precip_optimum_base - (year - 2000) * 0.5,
    
    precip_min = precip_optimum - case_when(
      species == "Fagus sylvatica" ~ 300,
      species == "Picea abies" ~ 250,
      species == "Quercus ilex" ~ 200,
      species == "Pinus sylvestris" ~ 300,
      species == "Betula pendula" ~ 250
    ),
    
    precip_max = precip_optimum + case_when(
      species == "Fagus sylvatica" ~ 400,
      species == "Picea abies" ~ 350,
      species == "Quercus ilex" ~ 300,
      species == "Pinus sylvestris" ~ 350,
      species == "Betula pendula" ~ 400
    )
  )

# Generate points for climate envelope visualization
envelope_points <- data.frame()
for(i in 1:nrow(climate_envelopes)) {
  row <- climate_envelopes[i,]
  # Generate 100 points within the climate envelope for each species-year
  temp_range <- seq(row$temp_min, row$temp_max, length.out = 10)
  precip_range <- seq(row$precip_min, row$precip_max, length.out = 10)
  
  points <- expand.grid(
    temperature = temp_range,
    precipitation = precip_range,
    year = row$year,
    species = row$species
  )
  
  # Calculate distance from optimum (for density/color)
  points$distance <- sqrt(
    ((points$temperature - row$temp_optimum) / (row$temp_max - row$temp_min))^2 +
      ((points$precipitation - row$precip_optimum) / (row$precip_max - row$precip_min))^2
  )
  
  # Add suitability score
  points$suitability <- 1 - points$distance
  
  envelope_points <- rbind(envelope_points, points)
}

# Filter to specific years for visualization (every 20 years)
envelope_points_subset <- envelope_points %>%
  filter(year %in% seq(2000, 2100, by = 20))

# Your task: Create an animated plot showing how climate envelopes shift over time.
# Use temperature on x-axis and precipitation on y-axis, with species shown by
# color and suitability by point size or alpha.
# Use transition_states(year) to show changes over time.

# Your code here: