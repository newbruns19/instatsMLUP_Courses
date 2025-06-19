####################################################################################################
###
### File:    03_Working_With_Factors_Strings.R
### Purpose: Examples and exercises for working with Strings 
###         and Factors.
### Authors: Gabriel Rodrigues Palma
### Date:    19/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
#################### Working with Strings and Factors - Part 1 ################
################################################################################

# Example 1: String Manipulation for Taxonomic Names
species_names <- c("Quercus robur L.", "Fagus sylvatica L.", 
                   "Pinus sylvestris L.", "betula pendula roth",
                   "ACER PSEUDOPLATANUS L.", "quercus petraea (matt.) liebl.")

# Clean and standardize species names
cleaned_names <- species_names %>%
  str_to_title() %>%
  str_remove(" L\\.") %>%
  str_remove(" \\(Matt\\.\\) Liebl\\.") %>%
  str_remove(" Roth") %>%
  str_trim()

# Extract genus and species
taxonomy <- tibble(
  original = species_names,
  cleaned = cleaned_names,
  genus = str_extract(cleaned_names, "^\\w+"),
  species = str_extract(cleaned_names, "(?<=\\s)\\w+"),
  binomial = paste(genus, species)
)

# Example 2: Pattern Matching for Habitat Classification
habitat_descriptions <- c(
  "Mixed deciduous forest with oak and maple dominance",
  "Coniferous plantation - primarily spruce and fir",
  "Grassland prairie with scattered oak groves",
  "Wetland marsh adjacent to deciduous forest edge",
  "Alpine meadow above treeline with sparse vegetation",
  "Old-growth forest dominated by hemlock and beech"
)

habitat_types <- tibble(
  description = habitat_descriptions,
  has_forest = str_detect(description, "forest"),
  has_grassland = str_detect(description, "grassland|prairie|meadow"),
  has_wetland = str_detect(description, "wetland|marsh"),
  dominant_trees = str_extract_all(description, 
                                   "oak|maple|spruce|fir|hemlock|beech"),
  habitat_category = case_when(
    str_detect(description, "forest") ~ "Forest",
    str_detect(description, "grassland|prairie|meadow") ~ "Grassland",
    str_detect(description, "wetland|marsh") ~ "Wetland",
    TRUE ~ "Other"
  )
)

# Example 3: Creating and Working with Factors for Life Stages
life_stages <- c("egg", "larva", "pupa", "adult", "larva", "egg", 
                 "adult", "pupa", "larva", "adult", "egg", "adult")

# Create ordered factor
life_stage_factor <- factor(life_stages, 
                            levels = c("egg", "larva", "pupa", "adult"),
                            ordered = TRUE)

insect_development <- tibble(
  individual_id = paste0("ID", 1:12),
  current_stage = life_stage_factor,
  days_in_stage = sample(1:15, 12, replace = TRUE),
  temperature = rnorm(12, 22, 3)
) %>%
  mutate(
    stage_numeric = as.numeric(current_stage),
    development_progress = stage_numeric / 4,
    next_stage = case_when(
      current_stage == "egg" ~ "larva",
      current_stage == "larva" ~ "pupa", 
      current_stage == "pupa" ~ "adult",
      current_stage == "adult" ~ "reproductive"
    )
  )

# Example 4: Factor Releveling for Conservation Status
conservation_status <- c("Least Concern", "Near Threatened", "Vulnerable",
                         "Endangered", "Critically Endangered", "Least Concern",
                         "Vulnerable", "Near Threatened", "Endangered")

species_conservation <- tibble(
  species = paste("Species", LETTERS[1:9]),
  status = factor(conservation_status),
  population_size = c(10000, 5000, 1500, 200, 50, 8000, 800, 3000, 150)
) %>%
  mutate(
    # Reorder factor by threat level
    status_ordered = fct_relevel(status, 
                                 "Least Concern", "Near Threatened", 
                                 "Vulnerable", "Endangered", 
                                 "Critically Endangered"),
    # Group low-threat categories
    status_grouped = fct_collapse(status_ordered,
                                  "Low Risk" = c("Least Concern", "Near Threatened"),
                                  "At Risk" = c("Vulnerable", "Endangered", 
                                                "Critically Endangered"))
  )

# Example 5: String Processing for GPS Coordinates
gps_coordinates <- c("45°23'12.5\"N 75°41'23.8\"W", 
                     "44°15'45.2\"N 76°32'11.1\"W",
                     "46°05'33.7\"N 74°58'44.3\"W",
                     "45°47'21.9\"N 75°12'55.6\"W")

coordinate_data <- tibble(
  site_id = paste0("GPS", 1:4),
  coordinates_raw = gps_coordinates
) %>%
  mutate(
    # Extract latitude components
    lat_degrees = str_extract(coordinates_raw, "\\d+(?=°)"),
    lat_minutes = str_extract(coordinates_raw, "\\d+(?=')"),
    lat_seconds = str_extract(coordinates_raw, "\\d+\\.\\d+(?=\"N)"),
    
    # Extract longitude components  
    lon_degrees = str_extract(coordinates_raw, "\\d+(?=°.*W)"),
    lon_minutes = str_extract(coordinates_raw, "(?<=°)\\d+(?='.*W)"),
    lon_seconds = str_extract(coordinates_raw, "\\d+\\.\\d+(?=\"W)"),
    
    # Convert to decimal degrees
    latitude = as.numeric(lat_degrees) + 
      as.numeric(lat_minutes)/60 + 
      as.numeric(lat_seconds)/3600,
    longitude = -(as.numeric(lon_degrees) + 
                    as.numeric(lon_minutes)/60 + 
                    as.numeric(lon_seconds)/3600)
  ) %>%
  select(site_id, coordinates_raw, latitude, longitude)

################################################################################
################ Working with Strings and Factors - Exercises 1 ###############
################################################################################

# Exercise 1: Clean species occurrence records with string functions.
# Use tibble species_occurrence with columns: observation
species_occurrence <- tibble(
  observation = c(
    "Red maple (Acer rubrum) - 15 individuals observed",
    "Sugar maple (Acer saccharum) - 22 individuals observed",
    "White oak (Quercus alba) - 9 individuals observed"
  )
)
# Extract common name, scientific name, and count.

# Exercise 2: Classify bird behavior from field notes and create factors.
# Use tibble bird_notes with column: behavior_note
bird_notes <- tibble(
  behavior_note = c(
    "Foraging among shrubs near ground",
    "Carrying nest material into tree hole",
    "Singing loudly to defend territory",
    "Flock departing south at dawn"
  )
)
# Categorize into: Foraging, Nesting, Territorial, Migration.
# Create an ordered factor: Nesting < Foraging < Territorial < Migration.

# Exercise 3: Parse weather station IDs to extract metadata.
# Use tibble weather_ids with column: station_id
weather_ids <- tibble(
  station_id = c("STN_001_TEMP_2023", "STN_002_PRECIP_2023", "STN_003_TEMP_2024")
)
# Extract station number, type (TEMP or PRECIP), and year.
# Create a factor for type, with levels TEMP, PRECIP.

# Exercise 4: Standardize and order plant phenology stages.
# Use tibble phenology with column: stage_raw
phenology <- tibble(
  stage_raw = c("flowering", "Fruiting", "leaf_drop", "DORMANT", "budding",
                "senescence", "Leaf_Drop", "flowering", "dormant")
)
# Clean and standardize to: dormant, budding, flowering, fruiting, senescence.
# Create an ordered factor with those levels.

# Exercise 5: Parse research plot codes into components and group habitats.
# Use tibble plot_codes with column: code
plot_codes <- tibble(
  code = c("FOR_A1_OAK_2023", "MEAD_B2_BIRCH_2022", "FOR_C1_MAPLE_2023",
           "WET_A3_WILLOW_2021", "MEAD_B1_OAK_2022", "WET_A4_MAPLE_2021")
)
# Extract: habitat (FOR, MEAD, WET), block, species, year.
# Group habitats using fct_collapse: "Forest" = FOR, "Meadow" = MEAD, "Wetland" = WET.

################################################################################
################## Working with Strings and Factors - Part 2 ##################
################################################################################

# Example 1: Advanced String Operations for Field Notes
field_notes <- c(
  "Plot 15: Observed 3 deer (Odocoileus virginianus) at 0800h, grazing behavior, clear weather",
  "Plot 23: Bird count - 5 AMRO, 3 BCCH, 1 HOWR heard calling, temp 18°C, wind 5km/h",
  "Plot 07: Fresh bear scat found, diameter ~3cm, contains berry seeds, GPS: 45.234°N",
  "Plot 31: Vegetation survey complete, 85% canopy cover, DBH range 15-45cm, 12 species ID'd"
)

parsed_notes <- tibble(
  original_note = field_notes
) %>%
  mutate(
    plot_number = str_extract(original_note, "(?<=Plot )\\d+"),
    temperature = str_extract(original_note, "\\d+(?=°C)"),
    time_observed = str_extract(original_note, "\\d{4}h"),
    species_count = str_count(original_note, "\\d+(?=\\s+(\\w+\\s+)?species)"),
    contains_gps = str_detect(original_note, "GPS:|°N|°S"),
    observation_type = case_when(
      str_detect(original_note, "deer|bear|scat") ~ "Mammal",
      str_detect(original_note, "bird|AMRO|BCCH|HOWR") ~ "Bird", 
      str_detect(original_note, "vegetation|canopy|DBH") ~ "Vegetation",
      TRUE ~ "Other"
    )
  )

# Example 2: Complex Factor Recoding for Ecological Guilds
bird_species <- c("American Robin", "Black-capped Chickadee", "House Wren",
                  "Red-winged Blackbird", "Northern Cardinal", "Blue Jay",
                  "Downy Woodpecker", "American Goldfinch", "House Sparrow")

bird_data <- tibble(
  species = bird_species,
  foraging_behavior = c("ground", "bark", "cavity", "marsh", "ground", 
                        "omnivore", "bark", "seed", "ground"),
  habitat_preference = c("forest_edge", "forest", "shrubland", "wetland",
                         "forest_edge", "forest", "forest", "grassland", "urban"),
  migration_status = c("short", "resident", "long", "short", "short",
                       "resident", "resident", "short", "resident")
) %>%
  mutate(
    # Create ecological guild factors
    foraging_guild = factor(foraging_behavior) %>%
      fct_recode(
        "Insectivore" = "bark",
        "Ground_forager" = "ground", 
        "Cavity_nester" = "cavity",
        "Wetland_specialist" = "marsh",
        "Granivore" = "seed",
        "Generalist" = "omnivore"
      ),
    
    # Lump rare habitat categories
    habitat_lumped = factor(habitat_preference) %>%
      fct_lump_n(4, other_level = "Other_habitat"),
    
    # Reorder by frequency
    migration_ordered = factor(migration_status) %>%
      fct_infreq() %>%
      fct_rev()
  )

# Example 3: String Splitting for Multi-Species Records
multi_species_records <- c(
  "Acer_saccharum|Quercus_rubra|Fagus_grandifolia",
  "Pinus_strobus|Tsuga_canadensis", 
  "Betula_papyrifera|Populus_tremuloides|Acer_saccharum|Quercus_rubra",
  "Picea_glauca",
  "Abies_balsamea|Picea_mariana|Betula_papyrifera"
)

species_occurrence <- tibble(
  plot_id = paste0("P", 1:5),
  species_list = multi_species_records
) %>%
  mutate(
    # Split species and count
    species_vector = str_split(species_list, "\\|"),
    species_count = map_int(species_vector, length),
    
    # Check for specific species
    has_maple = str_detect(species_list, "Acer"),
    has_conifer = str_detect(species_list, "Pinus|Picea|Abies|Tsuga"),
    has_oak = str_detect(species_list, "Quercus"),
    
    # Classify plot type
    forest_type = case_when(
      has_conifer & !has_maple & !has_oak ~ "Coniferous",
      !has_conifer & (has_maple | has_oak) ~ "Deciduous",
      has_conifer & (has_maple | has_oak) ~ "Mixed",
      TRUE ~ "Other"
    ) %>% factor()
  )

# Example 4: Advanced Factor Operations for Phenology Data
phenology_observations <- expand_grid(
  species = c("Acer saccharum", "Quercus rubra", "Betula papyrifera"),
  year = 2020:2023,
  phenophase = c("budburst", "leaf_expansion", "flowering", "fruiting", 
                 "leaf_coloring", "leaf_drop")
) %>%
  mutate(
    observation_date = sample(seq(as.Date("2020-03-01"), 
                                  as.Date("2023-11-30"), by = "day"), 
                              nrow(.)),
    intensity = sample(c("low", "medium", "high"), nrow(.), replace = TRUE)
  ) %>%
  mutate(
    # Create ordered phenological factor
    phenophase_ordered = factor(phenophase, 
                                levels = c("budburst", "leaf_expansion", "flowering",
                                           "fruiting", "leaf_coloring", "leaf_drop"),
                                ordered = TRUE),
    
    # Create seasonal groupings
    season_group = fct_collapse(phenophase_ordered,
                                "Spring" = c("budburst", "leaf_expansion", "flowering"),
                                "Summer" = "fruiting",
                                "Fall" = c("leaf_coloring", "leaf_drop")),
    
    # Reorder species by observation frequency
    species_reordered = fct_infreq(factor(species))
  )

# Example 5: String Validation and Cleaning for Research Data
research_codes <- c("BIO2023_001_QR", "ECO-2023-002-FS", "BIO2023_003_invalid",
                    "GEO_2023_004_BP", "bio2023_005_qr", "ECO2023006TS",
                    "BIO-2023-007-AR", "INVALID_CODE", "ECO_2023_008_FG")

code_validation <- tibble(
  raw_code = research_codes
) %>%
  mutate(
    # Standardize format
    clean_code = str_to_upper(raw_code) %>%
      str_replace_all("-", "_") %>%
      str_remove_all("\\s+"),
    
    # Validate structure
    valid_format = str_detect(clean_code, "^[A-Z]{3}_?\\d{4}_\\d{3}_[A-Z]{2}$"),
    
    # Extract components
    department = str_extract(clean_code, "^[A-Z]{3}"),
    year = str_extract(clean_code, "\\d{4}"),
    sequence = str_extract(clean_code, "(?<=_)\\d{3}(?=_)"),
    species_code = str_extract(clean_code, "[A-Z]{2}$"),
    
    # Create factors for departments
    dept_factor = factor(department) %>%
      fct_recode(
        "Biology" = "BIO",
        "Ecology" = "ECO", 
        "Geography" = "GEO"
      ) %>%
      fct_na_value_to_level("Unknown"),
    
    # Flag problematic codes
    needs_correction = !valid_format | is.na(dept_factor)
  ) %>%
  filter(valid_format) %>%  # Keep only valid codes for analysis
  arrange(dept_factor, year, sequence)

################################################################################
################ Working with Strings and Factors - Exercises 2 ###############
################################################################################

# Exercise 1: Ecological sampling string parsing
# Given a tibble of standardized field notes, extract the site, date,
# temperature, and species abundance as separate columns. Then, create
# factors for species_code and abundance_category.

library(tibble)
sampling_strings <- tibble(
  note = c(
    "Site_A12: 2023-06-15 | Temp:22.5°C | Species: QURU(3), ACSA(7), BEAL(2)",
    "Site_B05: 2023-06-18 | Temp:20.1°C | Species: QURU(4), BEAL(1)",
    "Site_C23: 2023-06-21 | Temp:18.3°C | Species: ACSA(10), QURU(2)"
  )
)

# Task: For each row, extract site_id, date (YYYY-MM-DD), temperature (C),
# and a species-abundance table. Build factors for species_code and create
# an 'abundance_cat' factor (Low <4, Medium 4-7, High >7).


# Exercise 2: Conservation priority standardization and categorization
# Given a tibble with inconsistent conservation_priority entries,
# clean and standardize these values, create an ordered factor with levels:
# "Immediate", "Moderate", "Low".

priority_raw <- tibble(
  species = c("QURU", "ACSA", "BEAL", "PINU"),
  conservation_priority = c("High Priority", "high_priority",
                            "Medium-Priority", "Low priority")
)

# Task: Clean and harmonize text in conservation_priority. Recode to an ordered
# factor with levels: Immediate > Moderate > Low. Group all variations accordingly.


# Exercise 3: Transect location parsing and habitat gradient
# Given a tibble of transect records, where each record contains several
# pipe-separated locations (formatted as Transect_Distance_Habitat),
# extract transect, distance (numeric, in meters), and habitat into columns,
# and create a habitat gradient factor (ordered: Forest < Edge < Meadow).

transect_strings <- tibble(
  locations = c(
    "T1_100m_Forest|T1_200m_Edge|T1_300m_Meadow",
    "T2_50m_Forest|T2_150m_Edge|T2_250m_Meadow"
  )
)

# Task: Split the string, extract transect, distance (integer), habitat,
# and build a habitat_gradient ordered factor (Forest < Edge < Meadow).


# Exercise 4: Coordinate standardization and validation
# Given a tibble of species occurrence records with various coordinate formats,
# standardize all coordinates to decimal degrees, create a column flagging
# invalid or ambiguous entries, and categorize coordinate precision.

coords <- tibble(
  raw_coord = c(
    "45.234N, -75.567W",
    "45°14'12\"N 75°34'3\"W",
    "invalid coordinate",
    "44.872N, -76.123W"
  )
)

# Task: For each coordinate, convert to decimal degrees (2 columns: lat, lon).
# Add a logical column 'valid' and a 'precision' factor ("decimal", "DMS", "invalid").


# Exercise 5: Phenology note parsing and timeline factors
# Given a tibble of plant phenology notes, extract the event names and dates,
# encode events as an ordered factor ("bud", "flower", "fruit"), and calculate
# the interval in days between each event.

phenology_notes <- tibble(
  notes = c(
    "First buds observed March 15, full bloom April 22, fruit set June 3",
    "Budburst March 20, flowers April 30, fruits June 10"
  )
)

# Task: For each note, extract event labels and date (assume current year is 2024).
# Create ordered factor for event_phase and interval_days columns for each plant.


