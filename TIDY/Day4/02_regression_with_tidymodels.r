####################################################################################################
###
### File:    02_regression_with_tidymodels.R
### Purpose: Examples and exercises for regression with tidymodels
### Authors: Gabriel Rodrigues Palma
### Date:    20/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
######################## Linear Regression in Ecology ########################
################################################################################

# Example 1: Plant growth response to fertilizer treatment
fertilizer_data <- data.frame(
  plot_id = 1:80,
  nitrogen_kg_ha = runif(80, 0, 200),
  phosphorus_kg_ha = runif(80, 0, 50),
  soil_ph = runif(80, 5.5, 8.0),
  rainfall_mm = runif(80, 300, 800),
  plant_height_cm = 15 + 0.3 * runif(80, 0, 200) + 
    0.5 * runif(80, 0, 50) + rnorm(80, 0, 5)
)

# Example 2: Linear regression model specification for growth data
plant_split <- initial_split(fertilizer_data, prop = 0.75)
plant_train <- training(plant_split)
plant_test <- testing(plant_split)

lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")


# Example 3: Recipe for plant growth analysis
plant_recipe <- recipe(plant_height_cm ~ nitrogen_kg_ha + phosphorus_kg_ha + 
                         soil_ph + rainfall_mm, data = plant_train) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

# Example 4: Complete workflow for plant growth modeling
plant_workflow <- workflow() %>%
  add_recipe(plant_recipe) %>%
  add_model(lm_spec)

plant_fit <- plant_workflow %>%
  fit(data = plant_train)

# Example 5: Model interpretation and coefficient extraction
tidy(plant_fit)
glance(plant_fit)

# Traditional way
normalisation <- function(x) {
  # This function is created to normalise my exaplanatory variables
  #by their mean and standard deviation
  #Input:
  #     x (vector): The vector containing a specific explanatory variable
  #Output:
  #      the standardised explanatory variable
  return((x - mean(x))/sd(x))
} 

plant_train$nitrogen_kg_ha <- normalisation(plant_train$nitrogen_kg_ha)
plant_train$phosphorus_kg_ha <- normalisation(plant_train$phosphorus_kg_ha)
plant_train$soil_ph <- normalisation(plant_train$soil_ph)
plant_train$rainfall_mm <- normalisation(plant_train$rainfall_mm)
plant_train$plant_height_cm <- normalisation(plant_train$plant_height_cm)

linear_model <- lm(plant_height_cm ~ nitrogen_kg_ha + phosphorus_kg_ha + 
                    soil_ph + rainfall_mm, data = plant_train)
tidy(linear_model)


################################################################################
###################### Linear Regression Exercises ###########################
################################################################################
# Dataset creation for all exercises
set.seed(101)
coral_data <- tibble(
  temperature = rnorm(100, mean = 27, sd = 2),             # degrees Celsius
  light_intensity = rnorm(100, mean = 220, sd = 40),       # µmol photons/m²/s
  depth = runif(100, min = 2, max = 25),                   # meters
  growth_rate = 10 + 0.8 * temperature + 0.03 * light_intensity - 0.28 * depth +
    rnorm(100, mean = 0, sd = 4)                           # mm/year
)

################################################################################
# Exercise 1: Build coral growth model
# Use the variables: temperature, light_intensity, depth, growth_rate
# Use the coral_data dataset created above (100 observations)
################################################################################

# Your code here:
my_model <- linear_reg() %>%
  set_engine('lm') 
my_recipe <- recipe(growth_rate ~ temperature + light_intensity + 
                         depth, data = coral_data)



################################################################################
# Exercise 2: Implement stepwise variable selection
# Use coral_data and different recipes to compare model performance
################################################################################

# Your code here:
linear_model <- lm(growth_rate ~ temperature + light_intensity + depth, 
            data = coral_data)
simpler_linear_model <- lm(growth_rate ~ light_intensity + depth, 
                           data = coral_data)
anova(simpler_linear_model, linear_model, test = 'Chisq') # Likelihood Ratio Test

drop1(linear_model, test = 'Chisq')
################################################################################
###################### Model Evaluation and Diagnostics ######################
################################################################################
# Y~Normal(mu, sigma), mu = 60.4562 - 1.2499*Nitrogen + 0.731 * Phosphorus +
# 3.1224*soil + 0.4804*rainfall

# Example 1: Model performance assessment
plant_predictions <- plant_fit %>%
  predict(new_data = plant_test) %>%
  bind_cols(plant_test)

rmse_value <- rmse(plant_predictions, truth = plant_height_cm, 
                   estimate = .pred)
print(rmse_value)

# Example 2: Residual analysis for ecological models
plant_augmented <- plant_fit %>%
  augment(new_data = plant_test)

plant_augmented %>%
  ggplot(aes(x = .pred, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Fitted Values")

# Example 3: Feature importance in plant growth
plant_fit %>%
  extract_fit_engine() %>%
  broom::tidy() %>%
  arrange(desc(abs(estimate)))

linear_model_plant <- plant_fit %>%
  extract_fit_engine()
drop1(linear_model_plant, test = "F")

# Example 4: Cross-validation for model robustness
set.seed(789)
plant_folds <- vfold_cv(plant_train, v = 10)
plant_cv_results <- plant_workflow %>%
  fit_resamples(resamples = plant_folds)

collect_metrics(plant_cv_results)

# Example 5: Prediction visualization
plant_predictions %>%
  ggplot(aes(x = plant_height_cm, y = .pred)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Actual Height (cm)", y = "Predicted Height (cm)")


# Set up the workflow
lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

recipe_spec <- recipe(medv ~ ., data = Boston)

workflow_spec <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(lm_spec)

# Fit the model
fitted_workflow <- fit(workflow_spec, data = Boston)

# Extract the underlying lm object
lm_model <- extract_fit_engine(fitted_workflow)

# Perform drop1 analysis
drop1_results <- drop1(lm_model, test = "F")
print(drop1_results)

# Create reduced model for comparison
reduced_workflow <- fitted_workflow %>%
  update_recipe(recipe_spec %>% step_rm(age)) %>%
  fit(data = Boston)

reduced_lm <- extract_fit_engine(reduced_workflow)

# Compare models with F test
anova(reduced_lm, lm_model, test = 'F')

################################################################################
################## Model Evaluation Exercises #################################
################################################################################
set.seed(101)
coral_data <- tibble(
  temperature = rnorm(100, mean = 27, sd = 2),             # degrees Celsius
  light_intensity = rnorm(100, mean = 220, sd = 40),       # µmol photons/m²/s
  depth = runif(100, min = 2, max = 25),                   # meters
  growth_rate = 10 + 0.8 * temperature + 0.03 * light_intensity - 0.28 * depth +
    rnorm(100, mean = 0, sd = 4)                           # mm/year
)

# Exercise 1: Implement different performance metrics
# Calculate MAE, RMSE, and R-squared for your coral model

# Exercise 2: Model validation techniques
# Implement leave-one-out and k-fold cross-validation

# Exercise 4: Model selection criteria comparison
# Compare AIC, BIC, and cross-validated metrics
