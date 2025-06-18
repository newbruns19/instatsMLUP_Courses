####################################################################################################
###
### File:    05_Building_Plots_with_Layers_Scales_Faceting.R
### Purpose: Building Plots with Layers, Scales, and Faceting
### Authors: Gabriel Rodrigues Palma
### Date:    18/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
#################### Layered Plots and Complex Visualizations ################
################################################################################

# Example 1: Multi-layer Ecosystem Visualization
forest_monitoring <- data.frame(
  plot_id = rep(paste0("Plot_", 1:8), each = 50),
  x_coord = runif(400, 0, 100),
  y_coord = runif(400, 0, 100),
  species = sample(c("Quercus", "Pinus", "Acer", "Betula"), 
                   400, replace = TRUE),
  dbh_cm = rlnorm(400, 2.5, 0.8),
  health_status = sample(c("Healthy", "Stressed", "Dead"), 
                         400, replace = TRUE, 
                         prob = c(0.7, 0.2, 0.1)),
  canopy_position = sample(c("Canopy", "Understory"), 
                           400, replace = TRUE)
)

# Multi-layer forest plot
forest_plot <- ggplot(forest_monitoring, 
                      aes(x = x_coord, y = y_coord)) +
  # Background layer - plot boundaries
  annotate("rect", xmin = 0, ymin = 0, xmax = 100, ymax = 100,
           fill = "lightgreen", alpha = 0.1) +
  # Tree positions by health status
  geom_point(aes(size = dbh_cm, color = health_status,
                 shape = canopy_position), alpha = 0.7) +
  # Add plot center points
  geom_point(data = data.frame(x = 50, y = 50), 
             aes(x = x, y = y), color = "red", 
             size = 3, shape = 3, inherit.aes = FALSE) +
  scale_size_continuous(range = c(1, 8), name = "DBH (cm)") +
  scale_color_manual(values = c("Healthy" = "#228B22", 
                                "Stressed" = "#DAA520",
                                "Dead" = "#8B4513")) +
  facet_wrap(~ plot_id, scales = "free") +
  labs(title = "Forest Plot Spatial Analysis",
       subtitle = "Tree distribution and health status",
       x = "X Coordinate (m)", y = "Y Coordinate (m)") +
  theme_minimal() +
  theme(axis.text = element_blank())

# Example 2: Temporal Layers with Environmental Data
lake_ecosystem <- data.frame(
  date = rep(seq(as.Date("2023-01-01"), 
                 as.Date("2023-12-31"), by = "week"), 3),
  lake = rep(c("Lake_A", "Lake_B", "Lake_C"), each = 53),
  temperature = c(
    # Lake A - shallow, variable
    c(2, 1, 3, 6, 12, 18, 24, 28, 30, 32, 29, 26, 22, 18, 
      16, 14, 12, 10, 8, 6, 4, 2, 1, 0, 1, 3, 5, 8, 14, 
      20, 26, 30, 32, 30, 28, 24, 20, 16, 12, 8, 6, 4, 2, 
      1, 2, 4, 7, 12, 18, 24, 28, 30, 31),
    # Lake B - deeper, more stable
    c(4, 4, 5, 7, 10, 14, 18, 22, 24, 26, 25, 23, 21, 19, 
      17, 15, 13, 11, 9, 8, 7, 6, 5, 4, 4, 5, 6, 8, 11, 
      15, 19, 23, 25, 24, 23, 21, 18, 15, 12, 9, 7, 6, 5, 
      4, 4, 5, 7, 10, 14, 18, 22, 24, 21),
    # Lake C - high altitude, cold
    c(0, -1, 0, 2, 6, 10, 14, 18, 20, 22, 21, 19, 17, 15, 
      13, 11, 9, 7, 5, 3, 2, 1, 0, -1, 0, 1, 3, 5, 8, 
      12, 16, 19, 21, 20, 19, 17, 14, 11, 8, 5, 3, 2, 1, 
      0, 0, 1, 3, 6, 10, 14, 18, 20, 17)
  ),
  dissolved_oxygen = runif(159, 6, 14),
  algae_biomass = runif(159, 0.5, 8.5),
  fish_activity = runif(159, 0, 100)
) %>%
  mutate(
    ice_cover = temperature < 2,
    thermal_stratified = temperature > 15 & dissolved_oxygen > 8,
    season = case_when(
      lubridate::month(date) %in% c(12, 1, 2) ~ "Winter",
      lubridate::month(date) %in% c(3, 4, 5) ~ "Spring",
      lubridate::month(date) %in% c(6, 7, 8) ~ "Summer",
      lubridate::month(date) %in% c(9, 10, 11) ~ "Autumn"
    )
  )

# Complex temporal plot with multiple layers
temporal_plot <- ggplot(lake_ecosystem, aes(x = date)) +
  # Ice cover periods
  geom_rect(data = lake_ecosystem %>% filter(ice_cover),
            aes(xmin = date - 3, xmax = date + 3, 
                ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.3, inherit.aes = FALSE) +
  # Temperature trend line
  geom_smooth(aes(y = temperature), method = "loess", 
              span = 0.3, color = "red", size = 1.2, se = FALSE) +
  # Dissolved oxygen as points
  geom_point(aes(y = dissolved_oxygen * 2, color = lake), 
             alpha = 0.6, size = 1.5) +
  # Fish activity as area
  geom_area(aes(y = fish_activity / 10, fill = lake), 
            alpha = 0.3, position = "identity") +
  # Seasonal boundaries
  geom_vline(xintercept = as.Date(c("2023-03-20", "2023-06-21", 
                                    "2023-09-22", "2023-12-21")),
             linetype = "dashed", color = "gray50", alpha = 0.7) +
  # Dual y-axis
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(trans = ~./2, name = "Dissolved Oxygen (mg/L)")
  ) +
  facet_wrap(~ lake, ncol = 1) +
  labs(title = "Lake Ecosystem Dynamics Throughout the Year",
       subtitle = "Temperature, oxygen, and fish activity patterns",
       x = "Date") +
  theme_bw()

# Example 3: Scale Transformations and Custom Breaks
biodiversity_sampling <- data.frame(
  sampling_effort_hours = c(1, 2, 4, 8, 16, 32, 64, 128, 256, 
                            512, 1024),
  species_richness = c(8, 15, 22, 28, 33, 37, 40, 42, 43, 44, 45),
  site_type = rep(c("Pristine", "Managed"), c(6, 5)),
  cumulative_cost = c(100, 250, 600, 1400, 3200, 7200, 16000, 
                      35000, 75000, 160000, 340000)
)

# Log-scale transformation plot
log_scale_plot <- ggplot(biodiversity_sampling, 
                         aes(x = sampling_effort_hours, 
                             y = species_richness)) +
  geom_line(aes(color = site_type), size = 1.2) +
  geom_point(aes(color = site_type, size = cumulative_cost), 
             alpha = 0.7) +
  # Log scale for x-axis
  scale_x_log10(breaks = c(1, 4, 16, 64, 256, 1024),
                labels = c("1h", "4h", "16h", "64h", "256h", "1024h")) +
  # Custom breaks for y-axis
  scale_y_continuous(breaks = seq(0, 50, 10),
                     minor_breaks = seq(0, 50, 5)) +
  # Size scale for cost
  scale_size_continuous(range = c(2, 8), 
                        labels = scales::dollar,
                        name = "Cumulative Cost") +
  # Color scale
  scale_color_manual(values = c("Pristine" = "#2E8B57", 
                                "Managed" = "#CD853F")) +
  labs(title = "Species Accumulation vs Sampling Effort",
       subtitle = "Logarithmic scale shows diminishing returns",
       x = "Sampling Effort (hours, log scale)",
       y = "Species Richness",
       color = "Site Type") +
  theme_minimal()

# Example 4: Custom Date Scales and Phenology
phenology_shift <- data.frame(
  year = 1980:2023,
  species = rep(c("Spring Ephemeral", "Summer Bloomer", 
                  "Fall Species"), each = 44),
  first_flowering_doy = c(
    # Spring ephemeral - getting earlier
    seq(95, 75, length.out = 44) + rnorm(44, 0, 3),
    # Summer bloomer - relatively stable  
    seq(165, 160, length.out = 44) + rnorm(44, 0, 5),
    # Fall species - getting later
    seq(255, 275, length.out = 44) + rnorm(44, 0, 4)
  )
) %>%
  mutate(
    first_flowering_date = as.Date(paste(year, 
                                         round(first_flowering_doy)), 
                                   format = "%Y %j"),
    month_day = format(first_flowering_date, "%m-%d")
  )

# Custom date labels and breaks
phenology_plot <- ggplot(phenology_shift, 
                         aes(x = year, y = first_flowering_doy, 
                             color = species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  # Custom y-axis with month labels
  scale_y_continuous(
    breaks = c(60, 91, 121, 152, 182, 213, 244, 274, 305),
    labels = c("Mar 1", "Apr 1", "May 1", "Jun 1", 
               "Jul 1", "Aug 1", "Sep 1", "Oct 1", "Nov 1"),
    name = "First Flowering Date"
  ) +
  # Custom x-axis breaks
  scale_x_continuous(breaks = seq(1980, 2020, 10),
                     minor_breaks = seq(1980, 2023, 5)) +
  scale_color_viridis_d(option = "plasma") +
  labs(title = "Phenological Shifts in Flowering Time",
       subtitle = "Climate change impacts on plant phenology",
       x = "Year", color = "Species Group") +
  theme_bw()

# Example 5: Advanced Faceting Layouts
ecosystem_comparison <- data.frame(
  biome = rep(c("Tropical", "Temperate", "Boreal", "Tundra"), 
              each = 100),
  productivity = c(rnorm(100, 2500, 400),   # Tropical
                   rnorm(100, 1200, 300),    # Temperate  
                   rnorm(100, 800, 200),     # Boreal
                   rnorm(100, 150, 50)),     # Tundra
  diversity = c(rnorm(100, 180, 30),        # Tropical
                rnorm(100, 85, 20),          # Temperate
                rnorm(100, 45, 15),          # Boreal  
                rnorm(100, 12, 5)),          # Tundra
  disturbance_freq = runif(400, 0, 5),
  conservation_status = rep(c("Protected", "Unprotected"), 200),
  climate_stability = runif(400, 0.2, 1.0)
)

# Complex faceted plot
faceted_plot <- ggplot(ecosystem_comparison, 
                       aes(x = productivity, y = diversity)) +
  geom_point(aes(color = disturbance_freq, 
                 size = climate_stability), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  # Faceting by biome and conservation status
  facet_grid(conservation_status ~ biome, 
             scales = "free", 
             labeller = labeller(
               biome = c("Boreal" = "Boreal Forest",
                         "Temperate" = "Temperate Forest", 
                         "Tropical" = "Tropical Forest",
                         "Tundra" = "Arctic Tundra")
             )) +
  scale_color_gradient2(low = "blue", mid = "yellow", 
                        high = "red", midpoint = 2.5,
                        name = "Disturbance\nFrequency") +
  scale_size_continuous(range = c(1, 4), 
                        name = "Climate\nStability") +
  labs(title = "Ecosystem Productivity-Diversity Relationships",
       subtitle = "Comparing biomes and protection status",
       x = "Primary Productivity (g C/m²/year)",
       y = "Species Diversity Index") +
  theme_bw() +
  theme(strip.text = element_text(size = 10, face = "bold"))

################################################################################
########### Layered Plots and Complex Visualizations Exercises ##############
################################################################################

# Exercise 1: Coral Reef Monitoring Multi-layer Analysis
# Dataset: coral health data with environmental factors
coral_monitoring <- data.frame(
  site_id = rep(paste0("Reef_", 1:6), each = 24),
  date = rep(seq(as.Date("2022-01-01"), as.Date("2023-12-31"), 
                 by = "month"), 6),
  coral_cover_percent = runif(144, 20, 85),
  fish_abundance = rpois(144, 150),
  bleaching_events = sample(c(0, 1), 144, replace = TRUE, 
                            prob = c(0.8, 0.2)),
  sea_temperature = runif(144, 24, 31),
  depth_m = rep(c(5, 10, 15, 20, 25, 30), each = 24),
  protection_level = rep(c("High", "Medium", "Low"), 48)
)

# Create multi-layer plot with coral coverage, fish abundance, 
# and bleaching events
# Use different geometric layers and temporal annotations
# Include environmental gradient overlays

# Exercise 2: Migration Route Multi-dimensional Visualization
# Dataset: bird migration with elevation and weather data
migration_routes <- data.frame(
  species = rep(c("Warbler", "Hawk", "Crane"), each = 50),
  latitude = runif(150, 25, 65),
  day_of_year = rep(seq(80, 260, length.out = 50), 3),
  elevation_m = runif(150, 0, 3000),
  wind_speed = runif(150, 5, 25),
  weather_type = sample(c("Clear", "Cloudy", "Storm"), 150, 
                        replace = TRUE),
  flock_size = rpois(150, 45),
  energy_reserves = runif(150, 0.2, 1.0)
)

# Build layered map-style plot with routes and elevations
# Add seasonal timing indicators and weather overlays
# Use different line types and point aesthetics

# Exercise 3: Forest Fire History Complex Timeline
# Dataset: fire events with ecological recovery data
fire_history <- data.frame(
  year = rep(1950:2023, 4),
  region = rep(c("North", "South", "East", "West"), each = 74),
  fire_occurrence = rbinom(296, 1, 0.15),
  fire_intensity = runif(296, 0, 1),
  drought_index = rnorm(296, 0, 1),
  vegetation_recovery = runif(296, 0.1, 1.0),
  species_return_rate = runif(296, 0.05, 0.8),
  climate_anomaly = rnorm(296, 0, 1.5)
)

# Design complex temporal plot with fire events, drought, 
# and recovery
# Use log scales and custom breaks for intensity data
# Include trend indicators and threshold annotations

# Exercise 4: Pollination Network Seasonal Layers
# Dataset: plant-pollinator interactions across seasons
pollination_seasonal <- data.frame(
  plant_id = rep(paste0("Plant_", 1:20), each = 36),
  week = rep(1:36, 20),
  flowering_intensity = runif(720, 0, 100),
  pollinator_visits = rpois(720, 25),
  network_connectivity = runif(720, 0.1, 0.9),
  temperature = rep(runif(36, 5, 30), 20),
  precipitation = rep(runif(36, 0, 50), 20),
  habitat_quality = rep(runif(20, 0.3, 1.0), each = 36)
)

# Create multi-layer visualization with flowering periods
# Include pollinator activity and network connectivity
# Use seasonal color schemes and faceting

# Exercise 5: Marine Ecosystem Depth Profile Analysis
# Dataset: oceanographic data with biological components
marine_profile <- data.frame(
  depth_m = rep(seq(0, 500, 10), 6),
  station = rep(paste0("Station_", 1:6), each = 51),
  temperature = rep(c(20, 18, 15, 12, 8, 5), each = 51) + 
    runif(306, -2, 2),
  salinity = runif(306, 34, 37),
  oxygen_concentration = runif(306, 2, 8),
  phytoplankton_density = rpois(306, 150),
  fish_biomass = runif(306, 0, 200),
  light_penetration = exp(-0.05 * rep(seq(0, 500, 10), 6))
)

# Build complex depth profile with multiple variables
# Use different geometric layers for biological and physical data
# Include depth zone annotations and scale transformations

################################################################################
############### Advanced Layered Plots and Complex Visualizations ###############
################################################################################

# Example 1: Spatio-temporal Meta-community Dynamics with Network Analysis
metacommunity_dynamics <- data.frame(
  patch_id = rep(paste0("Patch_", sprintf("%02d", 1:25)), each = 120),
  time_step = rep(1:120, 25),
  x_coord = rep(rep(1:5, each = 5), each = 120),
  y_coord = rep(rep(1:5, 5), each = 120),
  species_richness = rpois(3000, lambda = 15),
  connectivity = runif(3000, 0.1, 1.0),
  habitat_quality = rep(runif(25, 0.3, 1.0), each = 120),
  colonization_events = rbinom(3000, 5, 0.15),
  extinction_events = rbinom(3000, 3, 0.08),
  dispersal_limitation = runif(3000, 0, 0.8),
  environmental_stochasticity = rnorm(3000, 0, 0.3),
  patch_size = rep(runif(25, 0.5, 5.0), each = 120),
  isolation_index = rep(runif(25, 0.1, 2.0), each = 120)
) %>%
  mutate(
    metapopulation_stability = 1 - (extinction_events / 
                                      (colonization_events + 1)),
    landscape_coherence = connectivity * habitat_quality,
    temporal_phase = case_when(
      time_step <= 30 ~ "Establishment",
      time_step <= 60 ~ "Expansion", 
      time_step <= 90 ~ "Equilibration",
      TRUE ~ "Disturbance"
    )
  )

# Calculate network edges for connectivity visualization
patch_distances <- expand.grid(
  from_patch = 1:25, 
  to_patch = 1:25
) %>%
  filter(from_patch != to_patch) %>%
  mutate(
    from_x = rep(1:5, each = 5)[from_patch],
    from_y = rep(1:5, 5)[from_patch],
    to_x = rep(1:5, each = 5)[to_patch],
    to_y = rep(1:5, 5)[to_patch],
    distance = sqrt((to_x - from_x)^2 + (to_y - from_y)^2),
    connection_strength = exp(-distance/2)
  ) %>%
  filter(connection_strength > 0.3)

# Advanced multi-layer metacommunity plot
metacommunity_plot <- metacommunity_dynamics %>%
  filter(time_step %in% c(30, 60, 90, 120)) %>%
  ggplot(aes(x = x_coord, y = y_coord)) +
  # Background landscape connectivity network
  geom_segment(data = patch_distances,
               aes(x = from_x, y = from_y, 
                   xend = to_x, yend = to_y,
                   alpha = connection_strength),
               color = "lightgray", size = 0.5, 
               inherit.aes = FALSE) +
  # Patch size as base layer
  geom_point(aes(size = patch_size), color = "darkgreen", 
             alpha = 0.3, shape = 1) +
  # Species richness as filled circles
  geom_point(aes(size = species_richness, 
                 color = metapopulation_stability,
                 alpha = landscape_coherence)) +
  # Colonization/extinction events as directional arrows
  geom_spoke(aes(angle = colonization_events * pi/3, 
                 radius = extinction_events * 0.2),
             arrow = arrow(length = unit(0.1, "cm")),
             color = "red", alpha = 0.7) +
  # Temporal phase boundaries
  geom_rect(data = data.frame(
    temporal_phase = unique(metacommunity_dynamics$temporal_phase)
  ), aes(fill = temporal_phase), 
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
  alpha = 0.1, inherit.aes = FALSE) +
  scale_size_continuous(range = c(2, 12), 
                        name = "Richness/Size") +
  scale_color_gradient2(low = "red", mid = "yellow", high = "green",
                        midpoint = 0.5, name = "Stability") +
  scale_alpha_continuous(range = c(0.3, 1.0), 
                         name = "Coherence") +
  facet_wrap(~ paste("Time:", time_step, "-", temporal_phase),
             ncol = 2, scales = "free") +
  labs(title = "Metacommunity Dynamics Network Analysis",
       subtitle = "Spatiotemporal patterns in patch connectivity",
       x = "Landscape X-coordinate", 
       y = "Landscape Y-coordinate") +
  theme_void() +
  theme(strip.text = element_text(size = 10, face = "bold"))


# Example 5: Ecosystem Service Trade-offs with Stakeholder Preferences
ecosystem_services <- data.frame(
  landscape_unit_id = rep(paste0("Unit_", sprintf("%03d", 1:500)), 4),
  management_scenario = rep(c("Conservation", "Sustainable Use", 
                              "Intensive Use", "Restoration"), 
                            each = 500),
  carbon_storage = rnorm(2000, 150, 50),
  water_regulation = rnorm(2000, 75, 25),
  biodiversity_value = rnorm(2000, 85, 30),
  timber_production = rnorm(2000, 45, 20),
  recreation_potential = rnorm(2000, 60, 15),
  soil_protection = rnorm(2000, 70, 20),
  pollination_service = rnorm(2000, 55, 18),
  cultural_heritage = rnorm(2000, 40, 15),
  economic_value = rnorm(2000, 8000, 2500),
  implementation_cost = rnorm(2000, 5000, 1500),
  stakeholder_acceptance = runif(2000, 0.2, 0.95),
  ecological_risk = runif(2000, 0.05, 0.7),
  social_equity = runif(2000, 0.3, 0.9),
  long_term_sustainability = runif(2000, 0.4, 0.95)
) %>%
  mutate(
    net_benefit = economic_value - implementation_cost,
    service_synergy = (carbon_storage + biodiversity_value + 
                         water_regulation) / 3,
    production_value = (timber_production + recreation_potential) / 2,
    conservation_value = (biodiversity_value + soil_protection + 
                            cultural_heritage) / 3,
    trade_off_intensity = abs(service_synergy - production_value) / 
      pmax(service_synergy, production_value),
    management_efficiency = net_benefit / (ecological_risk + 0.1),
    stakeholder_category = cut(stakeholder_acceptance,
                               breaks = c(0, 0.4, 0.7, 1),
                               labels = c("Opposition", "Neutral", 
                                          "Support"))
  )

# Calculate Pareto frontier for trade-off analysis
pareto_frontier <- ecosystem_services %>%
  filter(management_scenario == "Sustainable Use") %>%
  arrange(desc(conservation_value)) %>%
  mutate(
    max_production = cummax(production_value),
    is_pareto = production_value == max_production
  ) %>%
  filter(is_pareto)

# Complex ecosystem service trade-off visualization
service_tradeoff_plot <- ecosystem_services %>%
  ggplot(aes(x = conservation_value, y = production_value)) +
  # Background density for all scenarios
  geom_density_2d_filled(alpha = 0.3, bins = 8, 
                         show.legend = FALSE) +
  # Pareto frontier
  geom_line(data = pareto_frontier,
            aes(x = conservation_value, y = production_value),
            color = "red", size = 1.5, alpha = 0.8) +
  geom_point(data = pareto_frontier,
             aes(x = conservation_value, y = production_value),
             color = "red", size = 3, alpha = 0.8) +
  # Management scenarios with multiple aesthetics
  geom_point(aes(size = net_benefit, 
                 color = management_scenario,
                 alpha = stakeholder_acceptance,
                 shape = stakeholder_category)) +
  # Trade-off intensity contours
  geom_contour(aes(z = trade_off_intensity), 
               color = "white", alpha = 0.7, bins = 6) +
  # Uncertainty ellipses for each scenario
  stat_ellipse(aes(color = management_scenario), 
               level = 0.75, size = 1.2, alpha = 0.6) +
  # Risk gradient overlay
  geom_smooth(aes(color = management_scenario, 
                  weight = 1 - ecological_risk),
              method = "loess", se = FALSE, 
              span = 0.8, size = 1, alpha = 0.7) +
  # Efficiency annotation arrows
  geom_segment(data = ecosystem_services %>% 
                 filter(management_efficiency > 
                          quantile(management_efficiency, 0.95)),
               aes(x = conservation_value, y = production_value,
                   xend = conservation_value + 5, 
                   yend = production_value + 5),
               arrow = arrow(length = unit(0.1, "cm")),
               color = "gold", alpha = 0.8) +
  scale_size_continuous(range = c(1, 8), 
                        labels = scales::dollar,
                        name = "Net Benefit") +
  scale_color_viridis_d(option = "turbo", name = "Management\nScenario") +
  scale_alpha_continuous(range = c(0.4, 0.9), 
                         name = "Stakeholder\nAcceptance") +
  scale_shape_manual(values = c(1, 16, 17), 
                     name = "Stakeholder\nCategory") +
  facet_grid(. ~ management_scenario, scales = "free") +
  labs(title = "Ecosystem Service Trade-off Analysis",
       subtitle = "Conservation vs. Production value optimization",
       x = "Conservation Value Index",
       y = "Production Value Index") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 10))

################################################################################
##### Advanced Layered Plots and Complex Visualizations Exercises ######################
################################################################################

# Exercise 1: Landscape Genetics Network Analysis
landscape_genetics <- data.frame(
  population_id = rep(paste0("Pop_", sprintf("%02d", 1:40)), each = 30),
  individual_id = paste0("Ind_", sprintf("%04d", 1:1200)),
  x_coordinate = runif(1200, 0, 100),
  y_coordinate = runif(1200, 0, 80),
  genetic_diversity_he = runif(1200, 0.3, 0.8),
  inbreeding_coefficient = runif(1200, -0.1, 0.4),
  migration_rate = runif(1200, 0.001, 0.05),
  effective_population_size = rpois(1200, 150),
  habitat_connectivity = runif(1200, 0.1, 0.9),
  landscape_resistance = runif(1200, 10, 500),
  genetic_distance = runif(1200, 0, 2.5),
  geographic_distance = runif(1200, 0.5, 50),
  isolation_by_distance = runif(1200, 0, 1),
  adaptive_divergence = runif(1200, 0, 0.3),
  demographic_bottleneck = rbinom(1200, 1, 0.15)
)

# Create multi-layer landscape genetics visualization
# Include: population connectivity networks, genetic diversity gradients,
# landscape resistance surfaces, and migration corridors
# Use Mantel test results and isolation-by-distance patterns
# Add demographic history indicators and adaptive potential

# Exercise 2: Functional Trait Hypervolume Analysis
trait_hypervolume <- data.frame(
  species_id = paste0("Sp_", sprintf("%03d", 1:300)),
  leaf_area = rlnorm(300, 2.5, 0.8),
  wood_density = runif(300, 0.3, 0.9),
  seed_mass = rlnorm(300, 1, 1.2),
  plant_height = rlnorm(300, 1.8, 0.7),
  sla_specific_leaf_area = runif(300, 5, 40),
  leaf_nitrogen = runif(300, 1, 5),
  photosynthetic_capacity = runif(300, 10, 80),
  water_use_efficiency = runif(300, 2, 8),
  stress_tolerance = runif(300, 0.2, 0.9),
  growth_rate = runif(300, 0.1, 2.5),
  reproductive_output = rpois(300, 500),
  dispersal_syndrome = sample(c("Wind", "Animal", "Ballistic", 
                                "Water"), 300, replace = TRUE),
  life_form = sample(c("Tree", "Shrub", "Herb", "Graminoid"), 
                     300, replace = TRUE),
  habitat_preference = sample(c("Forest", "Grassland", "Wetland", 
                                "Alpine"), 300, replace = TRUE),
  pollination_type = sample(c("Insect", "Wind", "Bird", 
                              "Self"), 300, replace = TRUE)
)

# Build complex functional trait space visualization
# Include: n-dimensional hypervolume projections, trait correlation networks
# Add functional diversity metrics and niche overlap calculations
# Use convex hulls and density estimates for functional groups
# Show trait syndromes and evolutionary constraints

# Exercise 3: Spatiotemporal Disease Transmission Dynamics
disease_dynamics <- data.frame(
  time_point = rep(1:200, 50),
  patch_id = rep(paste0("Patch_", sprintf("%02d", 1:50)), each = 200),
  susceptible_individuals = rpois(10000, 100),
  infected_individuals = rpois(10000, 15),
  recovered_individuals = rpois(10000, 25),
  pathogen_load = rlnorm(10000, 2, 1),
  transmission_rate = runif(10000, 0.001, 0.1),
  recovery_rate = runif(10000, 0.05, 0.3),
  mortality_rate = runif(10000, 0.01, 0.08),
  host_density = rpois(10000, 200),
  vector_abundance = rpois(10000, 500),
  environmental_suitability = runif(10000, 0.2, 0.9),
  movement_frequency = runif(10000, 0.1, 0.8),
  vaccination_coverage = runif(10000, 0, 0.7),
  surveillance_effort = runif(10000, 0.1, 1.0),
  outbreak_size = rpois(10000, 30)
)

# Design comprehensive epidemiological visualization
# Include: SIR model dynamics, spatial transmission networks
# Add reproduction number calculations and outbreak predictions
# Use animation-ready temporal progressions
# Show intervention effects and surveillance gaps

# Exercise 4: Multi-trophic Rewilding Assessment
rewilding_assessment <- data.frame(
  site_id = rep(paste0("Site_", sprintf("%02d", 1:30)), each = 48),
  year_since_rewilding = rep(0:47, 30),
  large_herbivore_biomass = rlnorm(1440, 5, 1.2),
  predator_abundance = rpois(1440, 8),
  small_mammal_diversity = runif(1440, 5, 35),
  bird_species_richness = rpois(1440, 45),
  vegetation_structure_complexity = runif(1440, 0.2, 0.95),
  soil_carbon_stock = rnorm(1440, 120, 30),
  water_table_depth = runif(1440, 0.5, 3.5),
  erosion_rate = runif(1440, 0, 15),
  fire_frequency = rpois(1440, 2),
  human_disturbance_index = runif(1440, 0.1, 0.8),
  ecosystem_service_value = rlnorm(1440, 8, 0.8),
  restoration_cost = rlnorm(1440, 9, 0.6),
  stakeholder_acceptance = runif(1440, 0.3, 0.9),
  ecological_authenticity = runif(1440, 0.4, 0.95)
)

# Create advanced rewilding success visualization
# Include: trophic cascade indicators, ecosystem recovery trajectories
# Add cost-benefit analysis with uncertainty bounds
# Use before-after-control-impact design visualization
# Show multiple success criteria and trade-offs

# Exercise 5: Global Change Interaction Matrix
global_change_matrix <- data.frame(
  ecosystem_type = rep(c("Tropical_Forest", "Temperate_Grassland", 
                         "Boreal_Forest", "Arctic_Tundra", 
                         "Mediterranean_Shrubland", "Freshwater_Lake"), 
                       each = 100),
  climate_warming_intensity = runif(600, 1.5, 4.5),
  precipitation_change_percent = runif(600, -40, 30),
  nitrogen_deposition = runif(600, 2, 25),
  habitat_fragmentation = runif(600, 0.1, 0.9),
  invasive_species_pressure = runif(600, 0, 0.8),
  pollution_load = runif(600, 0.1, 0.9),
  land_use_intensity = runif(600, 0.2, 0.95),
  co2_concentration = runif(600, 380, 550),
  biodiversity_response = rnorm(600, 0, 0.3),
  ecosystem_function_change = rnorm(600, -0.1, 0.4),
  resilience_capacity = runif(600, 0.2, 0.9),
  adaptation_potential = runif(600, 0.1, 0.8),
  interaction_strength = runif(600, 0, 1),
  synergistic_effects = runif(600, -0.5, 1.5),
  threshold_proximity = runif(600, 0, 1)
)

# Build comprehensive global change interaction visualization
# Include: interaction matrices with hierarchical clustering
# Add synergy/antagonism detection and threshold analysis
# Use parallel coordinates for multi-driver visualization
# Show ecosystem-specific vulnerability profiles
# Include uncertainty propagation and scenario projections
# Multiple layers example

species_data <- data.frame(
  site = rep(c("Forest", "Grassland", "Wetland"), each = 3),
  species = rep(c("Oak", "Pine", "Maple"), 3),
  abundance = c(45, 23, 12, 8, 67, 34, 15, 5, 89)
)
ggplot(species_data, aes(x = site, y = abundance)) +
  geom_point(size = 3) +
  geom_line(aes(group = species, color = species))
ggsave('Plots/first.png')
