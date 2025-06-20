####################################################################################################
###
### File:    05_Ggplot2_Maps.R
### Purpose: Examples and exercises for creating animations
### Authors: Gabriel Rodrigues Palma
### Date:    19/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
########################## geom_sf and geom_map Basics #######################
################################################################################

# Load required libraries and dataset
library(sf)
library(ggplot2)
library(dplyr)

# Load the North Carolina shapefile
nc <- st_read(system.file("shape/nc.shp", package="sf"))

# Example 1: Basic Species Distribution Mapping with geom_sf
bird_observations <- data.frame(
  longitude = c(-80.5, -79.2, -81.1, -78.8, -82.3),
  latitude = c(35.2, 36.1, 34.8, 35.9, 35.4),
  species = c("Cardinal", "Blue Jay", "Robin", "Cardinal", "Sparrow"),
  count = c(5, 3, 8, 2, 12)
)

# Convert to sf object
bird_sf <- st_as_sf(bird_observations, 
                    coords = c("longitude", "latitude"), 
                    crs = 4326)

# Basic species distribution map
ggplot() +
  geom_sf(data = nc, fill = "lightgreen", alpha = 0.3) +
  geom_sf(data = bird_sf, aes(color = species, size = count)) +
  labs(title = "Bird Species Distribution in North Carolina") +
  theme_new()

# Example 2: Biodiversity Hotspot Mapping
biodiversity_data <- nc %>%
  mutate(
    species_richness = sample(15:45, nrow(nc), replace = TRUE),
    endemic_species = sample(1:8, nrow(nc), replace = TRUE)
  )

ggplot(biodiversity_data) +
  geom_sf(aes(fill = species_richness)) +
  scale_fill_viridis_c(name = "Species\nRichness") +
  labs(title = "Biodiversity Hotspots Across Counties",
       subtitle = "Species richness distribution") +
  theme_void()

# Example 3: Forest Cover Change Analysis
forest_data <- nc %>%
  mutate(
    forest_cover_1990 = runif(nrow(nc), 20, 80),
    forest_cover_2020 = forest_cover_1990 - runif(nrow(nc), 0, 15),
    forest_change = forest_cover_2020 - forest_cover_1990
  )

ggplot(forest_data) +
  geom_sf(aes(fill = forest_change)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen",
                       midpoint = 0, name = "Forest Change\n(%)") +
  labs(title = "Forest Cover Change (1990-2020)") +
  theme_minimal()

# Example 4: Watershed Management Zones
watershed_zones <- nc %>%
  mutate(
    management_zone = sample(c("Conservation", "Restoration", 
                               "Sustainable Use", "Research"), 
                             nrow(nc), replace = TRUE),
    water_quality = sample(c("Excellent", "Good", "Fair", "Poor"), 
                           nrow(nc), replace = TRUE)
  )

ggplot(watershed_zones) +
  geom_sf(aes(fill = management_zone)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(title = "Watershed Management Zones",
       fill = "Zone Type") +
  theme_void()

# Example 5: Wildlife Corridor Analysis
corridor_data <- nc %>%
  mutate(
    corridor_value = sample(1:10, nrow(nc), replace = TRUE),
    connectivity = ifelse(corridor_value > 7, "High", 
                          ifelse(corridor_value > 4, "Medium", "Low"))
  )

ggplot(corridor_data) +
  geom_sf(aes(fill = connectivity)) +
  scale_fill_manual(values = c("High" = "darkgreen", 
                               "Medium" = "yellow", 
                               "Low" = "red")) +
  labs(title = "Wildlife Corridor Connectivity Assessment") +
  theme_minimal()

################################################################################
##################### geom_sf and geom_map Exercises ########################
################################################################################
# Download world countries data for exercises
world <- ne_countries(scale = "medium", returnclass = "sf")

# Exercise 1: Create a habitat suitability map for European countries
# INSTRUCTIONS:
# 1. Filter world data to get European countries
# 2. Create simulated ecological variables
# 3. Calculate a composite habitat suitability index
# 4. Create a choropleth map with appropriate colors and labels

# Step 1: Filter for European countries
europe <- world %>% 
  filter(continent == "Europe")

# Step 2: Add simulated habitat variables
habitat_data <- europe %>%
  mutate(
    elevation = runif(nrow(europe), 100, 3000),
    annual_precipitation = runif(nrow(europe), 400, 2000),
    mean_temperature = runif(nrow(europe), 5, 18),
    forest_cover = runif(nrow(europe), 10, 80)
  )
# Step 3: Calculate habitat suitability index
habitat_data <- habitat_data %>%
  mutate(
    # Normalize variables to 0-1 scale
    norm_precip = (annual_precipitation - min(annual_precipitation)) / 
      (max(annual_precipitation) - min(annual_precipitation)),
    norm_temp = (mean_temperature - min(mean_temperature)) / 
      (max(mean_temperature) - min(mean_temperature)),
    norm_forest = (forest_cover - min(forest_cover)) / 
      (max(forest_cover) - min(forest_cover)),
    # Calculate composite suitability index
    habitat_suitability = (norm_precip + norm_temp + norm_forest) / 3
  )

# Step 4: Create habitat suitability map
ggplot(habitat_data) +
  geom_sf(aes(fill = habitat_suitability)) +
  scale_fill_viridis_c(name = "Habitat\nSuitability", 
                       option = "viridis", 
                       direction = 1) +
  labs(title = "European Habitat Suitability for Forest Species",
       subtitle = "Based on precipitation, temperature, and forest cover",
       caption = "Data source: Natural Earth") +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10))
# YOUR TASK: Calculate habitat suitability index using the formula:
# suitability = (normalized_precipitation + normalized_temperature + 
#               normalized_forest_cover) / 3
# Normalize each variable to 0-1 scale using: (x - min(x)) / (max(x) - min(x))

# Create your habitat suitability map here:
# Use scale_fill_viridis_c() for color scale
# Add title "European Habitat Suitability for Forest Species"
# Include theme_void() for clean appearance

# Exercise 2: Map invasive species spread across South America
# INSTRUCTIONS:
# 1. Filter world data for South American countries
# 2. Create random points representing invasive species observations
# 3. Show temporal spread and population intensity

# Step 1: Filter for South American countries
south_america <- world %>%
  filter(continent == "South America")

# Step 2: Create invasive species observation points
set.seed(123)  # For reproducible random data
invasive_points <- data.frame(
  longitude = runif(50, -80, -35),
  latitude = runif(50, -55, 12),
  year_detected = sample(2010:2024, 50, replace = TRUE),
  population_density = sample(c("Low", "Medium", "High"), 50, replace = TRUE),
  species = sample(c("Invasive Plant A", "Invasive Insect B", 
                     "Invasive Fish C"), 50, replace = TRUE)
)

# YOUR TASK: 
# 1. Convert invasive_points to sf object using st_as_sf()
# 2. Create a map showing South American countries with invasive species points
# 3. Use aes(color = species, size = year_detected) for points
# 4. Add scale_size_continuous() and scale_color_viridis_d()
# 5. Title: "Invasive Species Spread in South America (2010-2024)"

# Exercise 3: Protected areas network in Africa
# INSTRUCTIONS:
# 1. Filter for African countries
# 2. Create simulated protected areas using random sampling
# 3. Show different protection levels with color coding

# Step 1: Filter for African countries
africa <- world %>%
  filter(continent == "Africa")

# Step 2: Create protected areas by sampling countries
protected_areas <- africa %>%
  slice_sample(n = 25) %>%
  mutate(
    protection_level = sample(c("Strict Nature Reserve", "National Park", 
                                "Natural Monument", "Wildlife Sanctuary"), 
                              nrow(.), replace = TRUE),
    area_km2 = runif(nrow(.), 100, 5000),
    year_established = sample(1960:2020, nrow(.), replace = TRUE)
  )

# YOUR TASK:
# 1. Create a map showing all African countries in light gray
# 2. Overlay protected areas with colors based on protection_level
# 3. Use scale_fill_brewer(type = "qual", palette = "Set2") 
# 4. Add labs() with title and legend labels
# 5. Use theme_minimal()

# Exercise 4: Migration route mapping across Asia
# INSTRUCTIONS:
# 1. Filter for Asian countries
# 2. Create bird migration routes using st_linestring()
# 3. Show seasonal migration patterns

# Step 1: Filter for Asian countries  
asia <- world %>%
  filter(continent == "Asia")

# Step 2: Create migration route coordinates
migration_coords <- list(
  route1 = matrix(c(100, 110, 120, 130,  # longitudes
                    50, 45, 40, 35), ncol = 2),  # latitudes
  route2 = matrix(c(70, 80, 90, 95,
                    60, 55, 50, 45), ncol = 2),
  route3 = matrix(c(120, 125, 130, 135,
                    45, 40, 35, 30), ncol = 2)
)

# YOUR TASK:
# 1. Convert migration_coords to sf linestring objects
# 2. Create a data frame with route information (species, season)
# 3. Plot Asian countries with migration routes overlaid
# 4. Use different colors for each route
# 5. Add legend showing species names
# 6. Title: "Bird Migration Routes Across Asia"

# Exercise 5: Biodiversity hotspots in Australia and Oceania
# INSTRUCTIONS:
# 1. Filter for Australia and Oceania region
# 2. Create biodiversity metrics for each country
# 3. Classify into hotspot categories

# Step 1: Filter for Australia and Oceania
oceania <- world %>%
  filter(continent == "Oceania")

# Step 2: Add biodiversity data
biodiversity_data <- oceania %>%
  mutate(
    endemic_species = rpois(nrow(oceania), lambda = 50),
    threatened_species = rpois(nrow(oceania), lambda = 15),
    species_richness = endemic_species + threatened_species + 
      rpois(nrow(oceania), lambda = 200),
    hotspot_status = case_when(
      endemic_species > 60 & threatened_species > 20 ~ "Critical Hotspot",
      endemic_species > 40 | threatened_species > 15 ~ "Hotspot", 
      endemic_species > 20 | threatened_species > 10 ~ "Important Area",
      TRUE ~ "Standard"
    )
  )

# YOUR TASK:
# 1. Create a map showing hotspot_status with distinct colors
# 2. Use scale_fill_manual() with appropriate colors:
#    Critical Hotspot = "red", Hotspot = "orange", 
#    Important Area = "yellow", Standard = "lightblue"
# 3. Add country labels using geom_sf_text()
# 4. Title: "Biodiversity Hotspots in Australia and Oceania"
# 5. Include a subtitle with data source information

################################################################################
###################### Advanced geom_sf Techniques ##########################
################################################################################

# Example 1: Multi-layer Ecological Analysis
# Combining multiple spatial datasets
protected_areas <- nc %>%
  slice_sample(n = 15) %>%
  mutate(protection_type = sample(c("National Park", "State Park", 
                                    "Wildlife Refuge"), 
                                  nrow(.), replace = TRUE))

water_bodies <- st_buffer(st_sample(nc, 8), dist = 0.1) %>%
  st_sf() %>%
  mutate(water_type = sample(c("Lake", "River", "Wetland"), 
                             nrow(.), replace = TRUE))

ggplot() +
  geom_sf(data = nc, fill = "lightgray", alpha = 0.5) +
  geom_sf(data = water_bodies, aes(fill = water_type), alpha = 0.7) +
  geom_sf(data = protected_areas, aes(color = protection_type), 
          fill = NA, size = 1.2) +
  scale_fill_manual(values = c("Lake" = "blue", "River" = "lightblue", 
                               "Wetland" = "darkblue")) +
  scale_color_manual(values = c("National Park" = "darkgreen", 
                                "State Park" = "green", 
                                "Wildlife Refuge" = "orange")) +
  labs(title = "Integrated Conservation Landscape") +
  theme_void()

# Example 2: Species Abundance Heat Maps
abundance_grid <- st_make_grid(nc, n = c(10, 8)) %>%
  st_sf() %>%
  mutate(
    species_abundance = rpois(nrow(.), lambda = 15),
    sampling_effort = runif(nrow(.), 1, 5)
  )

ggplot() +
  geom_sf(data = nc, fill = NA, color = "black", size = 0.3) +
  geom_sf(data = abundance_grid, aes(fill = species_abundance), 
          alpha = 0.8) +
  scale_fill_viridis_c(name = "Abundance\n(individuals)") +
  labs(title = "Species Abundance Spatial Distribution",
       subtitle = "Grid-based sampling results") +
  theme_minimal()

# Example 3: Elevation and Vegetation Gradients
elevation_data <- nc %>%
  mutate(
    elevation = runif(nrow(nc), 0, 1500),
    vegetation_type = case_when(
      elevation < 300 ~ "Coastal Plain",
      elevation < 800 ~ "Piedmont",
      elevation < 1200 ~ "Mountain Forest",
      TRUE ~ "Alpine"
    ),
    ndvi = 0.8 - (elevation / 2000) + rnorm(nrow(nc), 0, 0.1)
  )

ggplot(elevation_data) +
  geom_sf(aes(fill = vegetation_type)) +
  scale_fill_brewer(type = "seq", palette = "YlGn") +
  labs(title = "Vegetation Types by Elevation Gradient",
       fill = "Vegetation\nType") +
  theme_void() +
  theme(legend.position = "bottom")

# Example 4: Seasonal Migration Patterns
migration_routes <- data.frame(
  route_id = rep(1:3, each = 4),
  lon = c(-84, -82, -80, -78, -83, -81, -79, -77, -82.5, -80.5, 
          -78.5, -76.5),
  lat = c(34.5, 35, 35.5, 36, 34.8, 35.3, 35.8, 36.3, 34.2, 
          34.7, 35.2, 35.7),
  season = rep(c("Spring", "Summer", "Fall", "Winter"), 3)
) %>%
  group_by(route_id) %>%
  summarise(geometry = st_cast(st_sfc(st_linestring(cbind(lon, lat))), 
                               "LINESTRING"),
            .groups = "drop") %>%
  st_sf(crs = 4326) %>%
  mutate(species = paste("Species", LETTERS[route_id]))

ggplot() +
  geom_sf(data = nc, fill = "lightgray", alpha = 0.3) +
  geom_sf(data = migration_routes, aes(color = species), 
          size = 2, alpha = 0.8) +
  scale_color_viridis_d() +
  labs(title = "Seasonal Migration Routes") +
  theme_minimal()

# Example 5: Fragmentation Analysis
fragmentation_data <- nc %>%
  mutate(
    patch_size = rexp(nrow(nc), rate = 0.01),
    edge_density = runif(nrow(nc), 0, 50),
    connectivity_index = patch_size / (edge_density + 1)
  )

ggplot(fragmentation_data) +
  geom_sf(aes(fill = connectivity_index)) +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green",
                       midpoint = median(fragmentation_data$connectivity_index),
                       name = "Connectivity\nIndex") +
  labs(title = "Habitat Fragmentation Analysis",
       subtitle = "Connectivity assessment across landscape") +
  theme_void()

################################################################################
################# Advanced geom_sf Exercises #################################
################################################################################
rivers <- ne_download(scale = 50, type = "rivers_lake_centerlines", 
                      category = "physical", returnclass = "sf")
coastlines <- ne_coastline(scale = 50, returnclass = "sf")

# Exercise 1: Climate change vulnerability mapping for Europe
# INSTRUCTIONS:
# 1. Use European countries from previous exercise
# 2. Create climate vulnerability index from multiple variables
# 3. Add climate adaptation zones based on vulnerability scores

# Use europe dataset from previous exercises
climate_data <- europe %>%
  mutate(
    temp_increase = rnorm(nrow(europe), 2.5, 0.8),
    precip_change = rnorm(nrow(europe), -5, 15),
    extreme_events_freq = rpois(nrow(europe), 3),
    adaptive_capacity = runif(nrow(europe), 0.3, 0.9)
  )

# YOUR TASK:
# 1. Calculate vulnerability index: 
#    vulnerability = (temp_increase * 0.3 + abs(precip_change) * 0.2 + 
#                    extreme_events_freq * 0.3) / adaptive_capacity
# 2. Create adaptation zones: High Risk (>8), Medium Risk (4-8), Low Risk (<4)
# 3. Create a map with diverging color scale (red-white-blue)
# 4. Add European rivers overlay using rivers dataset
# 5. Title: "Climate Change Vulnerability Assessment - Europe 2024"
# 6. Include facet by adaptation zone using facet_wrap()

# Step 3: Calculate vulnerability index
climate_data <- climate_data %>%
  mutate(
    vulnerability = (temp_increase * 0.3 + abs(precip_change) * 0.2 + 
                       extreme_events_freq * 0.3) / adaptive_capacity,
    adaptation_zone = case_when(
      vulnerability > 8 ~ "High Risk",
      vulnerability > 4 ~ "Medium Risk",
      TRUE ~ "Low Risk"
    )
  )

# Step 4: Filter European rivers
europe_bbox <- st_bbox(europe)
europe_rivers <- rivers %>%
  st_crop(europe_bbox)

# Step 5: Create vulnerability map
ggplot() +
  geom_sf(data = climate_data, 
          aes(fill = vulnerability)) +
  geom_sf(data = europe_rivers, 
          color = "steelblue", 
          size = 0.2) +
  scale_fill_gradient2(
    low = "blue", 
    mid = "white", 
    high = "red",
    midpoint = median(climate_data$vulnerability),
    name = "Vulnerability\nIndex"
  ) +
  facet_wrap(~adaptation_zone) +
  labs(title = "Climate Change Vulnerability Assessment - Europe 2024",
       subtitle = "Vulnerability index based on temperature increase, precipitation change, and adaptive capacity",
       caption = "Data source: Natural Earth with simulated climate data") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  )

# Exercise 2: Marine biodiversity analysis for North America
# INSTRUCTIONS:
# 1. Filter for North American countries
# 2. Create coastal buffer zones for marine analysis
# 3. Show marine protected areas and fishing zones

# Step 1: Filter for North American countries
north_america <- world %>%
  filter(continent == "North America")

# Step 2: Create coastal buffers (simplified for this exercise)
# In real analysis, you would use proper coastal boundary data
coastal_zones <- north_america %>%
  filter(name %in% c("United States of America", "Canada", "Mexico")) %>%
  st_buffer(dist = 1) %>%  # 1 degree buffer (approximately 111 km)
  mutate(
    marine_biodiversity = runif(nrow(.), 0.2, 0.95),
    fishing_intensity = runif(nrow(.), 0.1, 0.8),
    protection_status = sample(c("Marine Protected Area", "Fishing Zone", 
                                 "Mixed Use", "Unregulated"), 
                               nrow(.), replace = TRUE)
  )

# YOUR TASK:
# 1. Create base map of North American countries
# 2. Add coastal_zones with fill based on marine_biodiversity
# 3. Add points for major cities (you can create 10 random coastal points)
# 4. Use facet_wrap(~protection_status) to show different management zones
# 5. Add coastlines using the coastlines dataset
# 6. Title: "Marine Biodiversity and Management Zones - North America"

# Exercise 3: Fire risk assessment across Africa
# INSTRUCTIONS:
# 1. Use African countries data
# 2. Combine multiple risk factors into composite fire risk
# 3. Create risk classification system

# Use africa dataset from previous exercises
fire_risk_data <- africa %>%
  mutate(
    vegetation_type = sample(c("Forest", "Savanna", "Grassland", 
                               "Desert", "Wetland"), 
                             nrow(africa), replace = TRUE),
    fuel_load = case_when(
      vegetation_type == "Forest" ~ runif(nrow(africa), 7, 10),
      vegetation_type == "Savanna" ~ runif(nrow(africa), 5, 8),
      vegetation_type == "Grassland" ~ runif(nrow(africa), 3, 6),
      vegetation_type == "Desert" ~ runif(nrow(africa), 1, 3),
      vegetation_type == "Wetland" ~ runif(nrow(africa), 1, 2)
    ),
    avg_temperature = runif(nrow(africa), 15, 35),
    drought_frequency = rpois(nrow(africa), 2),
    human_activity = runif(nrow(africa), 0.1, 0.9)
  )

# YOUR TASK:
# 1. Calculate fire_risk_index: 
#    (fuel_load * 0.3 + (avg_temperature-15)/20 * 0.3 + 
#     drought_frequency/5 * 0.2 + human_activity * 0.2)
# 2. Create risk categories: Very High (>0.8), High (0.6-0.8), 
#    Medium (0.4-0.6), Low (<0.4)
# 3. Create choropleth map with red color gradient
# 4. Add text labels for countries with Very High risk
# 5. Create small multiples by vegetation_type
# 6. Title: "Wildfire Risk Assessment by Vegetation Type - Africa"

# Exercise 4: Urban ecology mapping for Asian megacities
# INSTRUCTIONS:
# 1. Focus on major Asian countries
# 2. Create urban ecological indicators
# 3. Show relationship between urbanization and biodiversity

# Create urban ecology dataset
asian_cities <- asia %>%
  filter(name %in% c("China", "India", "Japan", "South Korea", 
                     "Thailand", "Vietnam", "Indonesia", "Malaysia")) %>%
  mutate(
    urban_population_pct = runif(nrow(.), 30, 95),
    green_space_per_capita = runif(nrow(.), 2, 25),
    air_quality_index = runif(nrow(.), 50, 300),
    urban_biodiversity_index = pmax(0, 
                                    100 - urban_population_pct + green_space_per_capita * 2 - 
                                      air_quality_index/10 + rnorm(nrow(.), 0, 10))
  )

# YOUR TASK:
# 1. Create a bivariate map showing both urban_population_pct and 
#    urban_biodiversity_index
# 2. Use st_centroid() to add city points
# 3. Size points by green_space_per_capita
# 4. Color points by air_quality_index using viridis scale
# 5. Add country boundaries in gray
# 6. Title: "Urban Ecology Indicators - Asian Megacities"
# 7. Create correlation plot in corner showing relationships

# Exercise 5: Arctic ecosystem monitoring
# INSTRUCTIONS:
# 1. Filter for Arctic/northern countries
# 2. Show permafrost, sea ice, and wildlife data
# 3. Demonstrate climate change impacts

# Filter for Arctic region countries
arctic_region <- world %>%
  filter(name %in% c("Greenland", "Iceland", "Norway", "Sweden", 
                     "Finland", "Russian Federation", "Canada", 
                     "United States of America", "Denmark")) %>%
  # Focus on northern latitudes
  filter(st_bbox(.)[4] > 60)  # Northern boundary > 60°N

# Add Arctic ecological data
arctic_data <- arctic_region %>%
  mutate(
    permafrost_extent = runif(nrow(.), 0, 100),
    ice_cover_days = runif(nrow(.), 150, 350),
    polar_bear_population = rpois(nrow(.), 200),
    sea_ice_thickness = runif(nrow(.), 0.5, 3.0),
    temperature_change_1990_2020 = runif(nrow(.), 1.5, 4.5),
    ecosystem_threat_level = case_when(
      temperature_change_1990_2020 > 3.5 ~ "Critical",
      temperature_change_1990_2020 > 2.5 ~ "High",
      temperature_change_1990_2020 > 1.8 ~ "Moderate",
      TRUE ~ "Low"
    )
  )

# YOUR TASK:
# 1. Create Arctic-centered map projection using st_transform()
# 2. Show permafrost_extent with blue-white color gradient  
# 3. Add polar bear populations as proportional circles
# 4. Use coord_sf() to set Arctic projection limits
# 5. Add gridlines for latitude/longitude reference
# 6. Color countries by ecosystem_threat_level
# 7. Title: "Arctic Ecosystem Monitoring - Climate Change Impacts"
# 8. Add inset map showing global context

# BONUS CHALLENGE: Multi-scale analysis
# Create a function that can generate similar analyses for any continent
# Function should take continent name and return publication-ready map

create_continent_biodiversity_map <- function(continent_name) {
  # YOUR TASK: Fill in this function
  # 1. Filter world data for specified continent
  # 2. Add simulated biodiversity metrics
  # 3. Create standardized map output
  # 4. Return ggplot object
  
  # Function code here...
}

# Test your function:
# asia_map <- create_continent_biodiversity_map("Asia")
# print(asia_map)

