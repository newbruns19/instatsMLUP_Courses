####################################################################################################
###
### File:    03_regression_II_with_tidymodels.R
### Purpose: Examples and exercises for regression II with tidymodels
### Authors: Gabriel Rodrigues Palma
### Date:    20/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
###################### Generalized Linear Models ##############################
################################################################################
# Insect species count vs habitat variables
insect_survey <- tibble(
  temperature = c(15, 18, 21, 24, 27, 30, 33, 36),
  humidity = c(6, 5, 0, 5, 8, 5, 9, 9),
  species_count = c(8, 12, 15, 18, 22, 25, 20, 16)
)

# Poisson GLM specification
poisson_model <- poisson_reg() %>%
  set_engine("glm")

poisson_fit <- poisson_model %>%
  fit(species_count ~ temperature + humidity, data = insect_survey)

my_poisson_regression <- glm(species_count ~ temperature + humidity, 
                              data = insect_survey)
drop1(my_poisson_regression, test = 'Chisq')

tidy(my_poisson_regression)
tidy(poisson_fit)
extracted_poisson_engine <- poisson_fit %>%
            extract_fit_engine()
drop1(extracted_poisson_engine, test = 'Chisq')

# Example 2: Logistic Regression for Presence/Absence Data
bird_presence <- tibble(
  elevation = c(100, 300, 500, 700, 900, 1100, 1300, 1500),
  present = c(1, 1, 1, 1, 0, 0, 0, 0)
)

# Logistic regression specification
logistic_model <- logistic_reg() %>%
  set_engine("glm") 

# Convert presence to factor for classification
bird_presence$present <- factor(bird_presence$present)

logistic_fit <- logistic_model %>%
  fit(present ~ elevation , data = bird_presence)

tidy(logistic_fit)

my_logistic_regression <- glm(present ~ elevation,
                              data = bird_presence, family = 'binomial')
tidy(my_logistic_regression)

# Example 3: Gamma Regression for Continuous Positive Data
biomass_data <- tibble(
  age = c(5, 10, 15, 20, 25, 30, 35, 40),
  nutrients = c(2.1, 4.5, 6.6, 8.1, 10.9, 12.2, 14.3, 16.4),
  biomass = c(15.2, 45.8, 89.3, 156.7, 234.1, 321.5, 398.2, 467.9)
)

# Using generalized linear model with gamma family
gamma_spec <- linear_reg() %>%
  set_engine("glm", family = Gamma(link = "log"))

gamma_fit <- gamma_spec %>%
  fit(biomass ~ age + nutrients, data = biomass_data)

tidy(gamma_fit)

my_gamma_regression <- glm(biomass ~ age + nutrients, 
    data = biomass_data, family = Gamma(link = 'log'))
tidy(my_gamma_regression)

# Example 4: Negative Binomial for Overdispersed Count Data
parasite_count <- tibble(
  host_size = c(12, 15, 18, 21, 24, 27, 30, 33),
  water_temp = c(15, 17, 19, 21, 23, 25, 27, 29),
  parasite_load = c(5, 8, 15, 22, 35, 45, 38, 28)
)

# Negative binomial using MASS::glm.nb
nb_spec <- linear_reg() %>%
  set_engine("glm", family = "nb")  # Note: requires MASS package

# Alternative using poisson with dispersion parameter
overdispersed_fit <- poisson_model %>%
  fit(parasite_load ~ host_size + water_temp, data = parasite_count)

# Check for overdispersion
library(hnp)
my_negative_binomial_reg <- overdispersed_fit %>%
  extract_fit_engine()

hnp(my_negative_binomial_reg, col.paint.out = T, print.on = T)
glance(overdispersed_fit)
tidy(overdispersed_fit)
BIC(my_negative_binomial_reg)

#1. Understand response variable;
#2. Explore your data (Y - X1, X2, ..., Xn);
#3. Goodness-of-fit -> linear model, generalised linear model, 
#                     mixed effects model -> Half-normal Plots (Same distributions) 
#                     generalised additive model and 
#                     generalised additive models for location, 
#                     scale and shape -> worm (wp()) or Half-normal Plots hnp().


################################################################################
###################### GLM Exercises #######################################
################################################################################

# Exercise 1: Poisson regression for seed production
# Use the seed_production dataset below

seed_production <- data.frame(
  plant_height = c(50, 75, 100, 125, 150, 175, 200, 225),
  sunlight_hours = c(4, 5, 6, 7, 8, 9, 10, 11),
  seed_count = c(25, 45, 78, 120, 165, 220, 180, 145)
)

# Fit Poisson GLM predicting seed_count from plant_height and sunlight_hours
# Use poisson_reg() with "glm" engine
# Extract and interpret coefficients
# Are both predictors significant?

# Exercise 2: Logistic regression for disease occurrence
# Use the disease_data dataset

disease_data <- data.frame(
  temperature = c(20, 22, 24, 26, 28, 30, 32, 34, 36, 38),
  humidity = c(40, 50, 60, 70, 80, 90, 95, 85, 75, 65),
  disease_present = c(0, 0, 0, 1, 1, 1, 1, 1, 0, 0)
)

# Convert disease_present to factor
# Fit logistic regression: disease_present ~ temperature + humidity
# Calculate odds ratios by exponentiating coefficients
# Predict probability of disease at temperature=30, humidity=80
# Create probability surface plot using ggplot2

# Exercise 3: Gamma regression for tree diameter growth
# Use the diameter_growth dataset

diameter_growth <- data.frame(
  initial_diameter = c(5, 8, 12, 15, 18, 22, 25, 30),
  fertilizer_kg = c(0, 2, 4, 6, 8, 10, 12, 14),
  annual_growth = c(0.5, 1.2, 2.1, 3.2, 4.1, 4.8, 5.2, 5.5)
)

# Fit Gamma GLM with log link for annual_growth
# Use linear_reg() with engine="glm" and family=Gamma(link="log")
# Interpret coefficients on the log scale
# Make predictions for a tree with 20cm diameter and 5kg fertilizer
# Compare with normal linear regression - which fits better?

# Exercise 4: Negative binomial for tick abundance
# Use the tick_abundance dataset

tick_abundance <- data.frame(
  deer_density = c(5, 8, 12, 15, 18, 22, 25, 30, 35, 40),
  vegetation_cover = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.85, 0.75, 0.6),
  tick_count = c(12, 28, 45, 62, 95, 130, 85, 110, 75, 45)
)

# First fit Poisson model
# Check for overdispersion (variance >> mean)
# Then fit negative binomial model using MASS::glm.nb
# Compare AIC values between Poisson and negative binomial
# Which model better handles the overdispersion?

# Exercise 5: Beta regression for vegetation cover proportion
# Use the vegetation_cover dataset

vegetation_cover <- data.frame(
  elevation = c(100, 300, 500, 700, 900, 1100, 1300, 1500),
  slope_degree = c(5, 10, 15, 20, 25, 30, 35, 40),
  cover_proportion = c(0.95, 0.88, 0.82, 0.75, 0.65, 0.58, 0.45, 0.32)
)

# Fit beta regression model: cover_proportion ~ elevation + slope_degree
# Use workflow with betareg engine
# Note: Beta regression requires values in (0,1), not [0,1]
# Transform if needed: (y * (n-1) + 0.5) / n where n is sample size
# Interpret coefficients and their significance
# Predict cover at elevation=800m, slope=25 degrees


################################################################################
##################### Generalized Additive Models ##############################
################################################################################
# Generalised linear Models
# Y ~ Binomial(mu, pi)
# logit(pi/(1-pi)) = B1 + B2*X1 + B2*X2

# Linear Model
# Y ~ Normal(mu, sigma)
# mu = B1 + B2*X1 + B2*X2

# Linear Mixed effects models
# Y_i ~ Normal(mu_i, sigma)
# mu_i = B1 + B2*X1 + B2*X2 + b1 + b1_i*Groups
# Random effect: b1 (Intercept) + b1_i (Slope) * Groups -> 
#Incorporate variability, sample from a population
# b1_i ~ Normal(mu, sigma)
# Fixed effects: B1 + B2*X1 + B2*X2 ->   
lmer(Y~X1 +X2 + (1 | Groups) + (0|Groups))

# Generalised Linear Mixed effects models
# Y_i ~ Binomial(mu_i, pi)
# logit(pi/(1-pi)) = B1 + B2*X1 + B2*X2 + b1 + b1_i*Groups
# Random effect: b1 (Intercept) + b1_i (Slope) * Groups -> 
#Incorporate variability, sample from a population
# b1_i ~ Normal(mu, sigma)
# Fixed effects: B1 + B2*X1 + B2*X2 ->   
glmer(Y~X1 +X2 + (1 | Groups) + (0|Groups))

# Generalised additive model
# Y ~ Normal(mu, sigma)
# mu = B1 + B2*X1 + B2*X2 + f(X1, X2)
lmer()
glmer()


# Species abundance vs temperature (non-linear relationship)
temp_abundance <- tibble(
  temperature = seq(5, 35, length.out = 20),
  abundance = 10 + 15 * exp(-0.5 * ((temperature - 20)/5)^2) + 
    rnorm(20, 0, 2)
)
#glms and lm -> Stats package
library(lme4)
library(mgcv)
library(gamlss)

# Using mgcv for GAM
gam_model <- gam(abundance ~ s(temperature), data = temp_abundance)
tidy(gam_model)
summary(gam_model)
plot(gam_model)

################## Tidyverse version ##################
gam_spec <- gen_additive_mod() %>%
  set_engine(engine = 'mgcv') %>%
  set_mode('regression')

# Fit the model
gam_model <- gam_spec %>%
  fit(abundance ~ s(temperature), data = temp_abundance)

# Summary and plotting
gam_model$fit %>% summary()
gam_model$fit %>% plot()
gam_model$fit %>% tidy()

# Example 2: Multiple Predictor GAM for Biodiversity
biodiversity_data <- tibble(
  elevation = runif(50, 0, 2000),
  rainfall = runif(50, 500, 2500),
  temperature = 25 - 0.006 * elevation + rnorm(50, 0, 2),
  species_richness = 50 - 0.01 * elevation + 0.02 * rainfall + 
    5 * sin(temperature * pi / 20) + rnorm(50, 0, 5)
)

# GAM with smooth terms for each predictor
multi_gam <- gam(species_richness ~ s(elevation) + s(rainfall) + s(temperature),
                 data = biodiversity_data)
summary(multi_gam)
plot(multi_gam, pages = 1)

################## Tidyverse version ##################
# Define and fit GAM with multiple smooth terms
multi_gam_spec <- gen_additive_mod() %>%
  set_engine(engine = 'mgcv') %>%
  set_mode('regression')

multi_gam <- multi_gam_spec %>%
  fit(species_richness ~ s(elevation) + s(rainfall) + s(temperature),
      data = biodiversity_data)

# Summary and plotting
multi_gam$fit %>% summary()
multi_gam$fit %>% plot(pages = 1)

# Example 3: GAM with Different Smooth Types
phenology_data <- tibble(
  day_of_year = 1:365,
  temperature = 15 + 10 * sin(2 * pi * day_of_year / 365) + 
    rnorm(365, 0, 2),
  flowering = 50 + 20 * sin(2 * pi * (day_of_year - 120) / 365) + 
    rnorm(365, 0, 5)
)

# Cyclic smooth for seasonal patterns
seasonal_gam <- gam(flowering ~ s(day_of_year, bs = "cc") + s(temperature),
                    data = phenology_data)
summary(seasonal_gam)

################## Tidyverse version ##################
# GAM with cyclic smooth for seasonal patterns
seasonal_gam_spec <- gen_additive_mod() %>%
  set_engine(engine = 'mgcv') %>%
  set_mode('regression')

seasonal_gam <- seasonal_gam_spec %>%
  fit(flowering ~ s(day_of_year, bs = "cc") + s(temperature),
      data = phenology_data)

seasonal_gam$fit %>% summary()
seasonal_gam$fit %>% plot(pages = 1)

# Example 5: Spatial GAM with 2D Smooths
spatial_data <- tibble(
  x_coord = runif(100, 0, 100),
  y_coord = runif(100, 0, 100),
  pollutant_level = 50 + 20 * sin(x_coord/10) * cos(y_coord/10) + 
    rnorm(100, 0, 5)
)

# 2D spatial smooth
spatial_gam <- gam(pollutant_level ~ s(x_coord, y_coord),
                   data = spatial_data)
summary(spatial_gam)
plot(spatial_gam)

# 3D visualization
vis.gam(spatial_gam, view = c("x_coord", "y_coord"), 
        theta = 30, phi = 30)

################## Tidyverse version ##################
# 2D spatial smooth
spatial_gam_spec <- gen_additive_mod() %>%
  set_engine(engine = 'mgcv') %>%
  set_mode('regression')

spatial_gam <- spatial_gam_spec %>%
  fit(pollutant_level ~ s(x_coord, y_coord), data = spatial_data)

spatial_gam$fit %>% summary()
spatial_gam$fit %>% plot()

# 3D visualization
spatial_gam$fit %>% 
  vis.gam(view = c("x_coord", "y_coord"), theta = 30, phi = 30)

################################################################################
####################### GAM Exercises ######################################
################################################################################

# Exercise 1: GAM for photosynthesis response to light
# Use the photosynthesis_light dataset below
# Create tibble instead of data.frame for tidyverse compatibility
photosynthesis_light <- tibble(
  light_intensity = seq(0, 2000, by = 50),
  photosynthesis_rate = pmax(0, 
                             30 * (1 - exp(-0.002 * light_intensity)) - 0.00001 * light_intensity^2 + 
                               rnorm(length(seq(0, 2000, by = 50)), 0, 2)
  )
)

# Define GAM specification
gam_spec <- gen_additive_mod() %>%
  set_engine('mgcv') %>%
  set_mode('regression')

# Fit GAM using tidymodels
photo_gam <- gam_spec %>%
  fit(photosynthesis_rate ~ s(light_intensity), data = photosynthesis_light)

# Compare with linear model
lm_spec <- linear_reg() %>%
  set_engine('lm') %>%
  set_mode('regression')

photo_lm <- lm_spec %>%
  fit(photosynthesis_rate ~ light_intensity, data = photosynthesis_light)

# Tasks:
# - Examine summary: photo_gam$fit %>% summary()
# - Plot smooth: photo_gam$fit %>% plot()
# - Compare models using cross-validation metrics
# - Find optimal light intensity using predict() with new data

# Exercise 2: Multiple Predictor GAM for Bird Abundance
# Create tibble
bird_habitat <- tibble(
  forest_age = runif(80, 5, 100),
  edge_distance = runif(80, 0, 1000),
  elevation = runif(80, 200, 1500),
  abundance = round(
    10 + 0.1 * forest_age - 0.005 * edge_distance + 
      0.01 * elevation - 0.000005 * elevation^2 + 
      rnorm(80, 0, 3)
  )
)

# Split data for validation
set.seed(123)
bird_split <- initial_split(bird_habitat, prop = 0.75)
bird_train <- training(bird_split)
bird_test <- testing(bird_split)

# Define GAM specification
multi_gam_spec <- gen_additive_mod() %>%
  set_engine('mgcv') %>%
  set_mode('regression')

# Create workflow
bird_workflow <- workflow() %>%
  add_model(multi_gam_spec) %>%
  add_formula(abundance ~ s(forest_age) + s(edge_distance) + s(elevation))

# Fit model
bird_gam_fit <- fit(bird_workflow, bird_train)

# Cross-validation for model assessment
bird_folds <- vfold_cv(bird_train, v = 5)
bird_cv_results <- fit_resamples(
  bird_workflow,
  resamples = bird_folds,
  metrics = metric_set(rmse, rsq)
)

# Tasks:
# - Check summary: bird_gam_fit %>% extract_fit_engine() %>% summary()
# - Plot smooths: bird_gam_fit %>% extract_fit_engine() %>% plot(pages = 1)
# - Assess CV results: collect_metrics(bird_cv_results)
# - Test set performance: predict(bird_gam_fit, bird_test)

#Exercise 3: Seasonal GAM for Insect Emergence
# Create tibble
insect_emergence <- tibble(
  day_of_year = rep(1:365, 2),
  year = rep(c(2022, 2023), each = 365),
  temperature = rep(15 + 12 * sin(2 * pi * (1:365 - 80) / 365), 2) + 
    rnorm(730, 0, 2),
  emergence_count = rpois(730, 
                          lambda = exp(1 + 0.1 * rep(sin(2 * pi * (1:365 - 120) / 365), 2) + 
                                         0.05 * rep(15 + 12 * sin(2 * pi * (1:365 - 80) / 365), 2))
  )
)

# For Poisson family, we need a custom approach
# Since tidymodels gen_additive_mod doesn't directly support family specification,
# we'll use a workflow with a custom model
poisson_gam_spec <- gen_additive_mod() %>%
  set_engine('mgcv') %>%
  set_mode('regression')

# Alternative: Create a custom function for Poisson GAM
fit_poisson_gam <- function(formula, data) {
  gam(formula, family = poisson, data = data)
}

# Direct fit for Poisson (fallback approach)
insect_gam <- gam(emergence_count ~ s(day_of_year, bs="cc") + s(temperature),
                  family = poisson, data = insect_emergence)

# Create prediction data for visualization
pred_data <- tibble(
  day_of_year = 1:365,
  temperature = 15 + 12 * sin(2 * pi * (1:365 - 80) / 365)
)

# Tasks:
# - Examine model: summary(insect_gam)
# - Plot: plot(insect_gam, pages = 1)
# - Predict across year: predict(insect_gam, pred_data, type = "response")
# - Find peak emergence day

# Exercise 4: GAM for Water Quality and Fish Diversity
# Create tibble
water_quality <- tibble(
  pH = runif(60, 5.5, 8.5),
  dissolved_oxygen = runif(60, 2, 12),
  temperature = runif(60, 10, 30),
  fish_diversity = round(
    pmax(0, 15 + 5 * (pH - 7)^2 - (pH - 7)^4 + 
           2 * dissolved_oxygen - 0.1 * dissolved_oxygen^2 +
           0.5 * temperature - 0.02 * temperature^2 +
           rnorm(60, 0, 2))
  )
)

# Data splitting
set.seed(456)
water_split <- initial_split(water_quality, prop = 0.8)
water_train <- training(water_split)
water_test <- testing(water_split)

# For count data, use Poisson GAM (direct mgcv approach)
water_gam <- gam(fish_diversity ~ s(pH) + s(dissolved_oxygen) + s(temperature),
                 family = quasipoisson, data = water_train)

# Alternative tidymodels approach for continuous approximation
gam_spec <- gen_additive_mod() %>%
  set_engine('mgcv') %>%
  set_mode('regression')

water_workflow <- workflow() %>%
  add_model(gam_spec) %>%
  add_formula(fish_diversity ~ s(pH) + s(dissolved_oxygen) + s(temperature))

water_gam_tidy <- fit(water_workflow, water_train)

# Prediction for specific conditions
new_conditions <- tibble(
  pH = 7.0,
  dissolved_oxygen = 8.0,
  temperature = 20.0
)

# Tasks:
# - Plot effects: plot(water_gam, pages = 1)
# - Make predictions: predict(water_gam, new_conditions, type = "response")
# - Compare tidymodels vs direct mgcv approach

