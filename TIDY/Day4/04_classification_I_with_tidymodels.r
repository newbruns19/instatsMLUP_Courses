####################################################################################################
###
### File:    04_classification_I_with_tidymodels.R
### Purpose: Examples and exercises for classification with tidymodels
### Authors: Gabriel Rodrigues Palma
### Date:    20/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
############################ SVM Basics for Ecology ############################
################################################################################
### Example 1
set.seed(123)
species_data <- tibble(
  temperature = rnorm(100, mean = 15, sd = 3),
  precipitation = rnorm(100, mean = 800, sd = 200),
  species = factor(sample(c("Oak", "Pine"), 100, replace = TRUE))
)

# Visualize the data
ggplot(species_data, aes(x = temperature, y = precipitation, color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Species Distribution by Climate Variables",
       x = "Temperature (°C)",
       y = "Precipitation (mm)") +
  theme_minimal()

# Basic SVM model with linear kernel
svm_model <- svm_linear() %>%
  set_engine("kernlab") %>%
  set_mode("classification") %>%
  fit(species ~ ., data = species_data)

# Print model summary
print(svm_model)

### Example 2
# Create a dataset with non-linear species distribution
set.seed(456)
n <- 200
x1 <- runif(n, -5, 5)
x2 <- runif(n, -5, 5)
# Non-linear boundary: species occurs in a circular pattern
species <- factor(ifelse(x1^2 + x2^2 < 10, "Present", "Absent"))

habitat_data <- tibble(
  soil_moisture = x1,
  light_intensity = x2,
  species = species
)

# Visualize the non-linear pattern
ggplot(habitat_data, aes(x = soil_moisture, y = light_intensity, 
                         color = species)) +
  geom_point(size = 2) +
  labs(title = "Species Distribution with Non-linear Boundary",
       x = "Soil Moisture",
       y = "Light Intensity") +
  theme_minimal()

# Compare different SVM kernels
# Linear kernel
svm_linear_spec <- svm_linear() %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# Radial basis function kernel
svm_rbf_spec <- svm_rbf() %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# Polynomial kernel
svm_poly_spec <- svm_poly() %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# Fit models
svm_linear_fit <- svm_linear_spec %>% fit(species ~ ., data = habitat_data)
svm_rbf_fit <- svm_rbf_spec %>% fit(species ~ ., data = habitat_data)
svm_poly_fit <- svm_poly_spec %>% fit(species ~ ., data = habitat_data)

# Create a grid for prediction
grid_points <- expand_grid(
  soil_moisture = seq(-5, 5, length.out = 100),
  light_intensity = seq(-5, 5, length.out = 100)
)

# Make predictions with each model
grid_preds_linear <- augment(svm_linear_fit, grid_points)
grid_preds_rbf <- augment(svm_rbf_fit, grid_points)
grid_preds_poly <- augment(svm_poly_fit, grid_points)

# Visualize decision boundaries
plot_decision_boundary <- function(preds, title) {
  ggplot() +
    geom_raster(data = preds, 
                aes(x = soil_moisture, y = light_intensity, fill = .pred_class),
                alpha = 0.3) +
    geom_point(data = habitat_data, 
               aes(x = soil_moisture, y = light_intensity, color = species),
               size = 2) +
    labs(title = title,
         x = "Soil Moisture",
         y = "Light Intensity") +
    theme_minimal()
}

plot_decision_boundary(grid_preds_linear, "Linear Kernel Decision Boundary")
plot_decision_boundary(grid_preds_rbf, "RBF Kernel Decision Boundary")
plot_decision_boundary(grid_preds_poly, "Polynomial Kernel Decision Boundary")

### Example 3
data(penguins)

# Remove rows with missing values
penguins_clean <- penguins %>% drop_na()

# Examine the data
glimpse(penguins_clean)

# Create a recipe for preprocessing
penguin_recipe <- recipe(species ~ bill_length_mm + bill_depth_mm, 
                         data = penguins_clean) %>%
  # Handle categorical predictors
  step_dummy(all_nominal_predictors()) %>%
  # Normalize numeric predictors
  step_normalize(all_numeric_predictors()) %>%
  # Remove highly correlated predictors
  step_corr(all_numeric_predictors(), threshold = 0.9)

# Prep the recipe to see the transformations
prepped_recipe <- prep(penguin_recipe)
print(prepped_recipe)

# Apply the recipe to the data
transformed_data <- bake(prepped_recipe, new_data = NULL)
glimpse(transformed_data)
split_obj <- initial_split(transformed_data, prop = 0.60)
penguin_train <- training(split_obj)
penguin_test  <- testing(split_obj)

# Visualize the transformed data
ggplot(transformed_data, aes(x = bill_length_mm, y = bill_depth_mm, 
                             color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Normalized Bill Dimensions by Penguin Species",
       x = "Normalized Bill Length",
       y = "Normalized Bill Depth") +
  theme_minimal()

# Create Support Vector Machines
svm_spec <- svm_rbf(
  cost = tune(),
  rbf_sigma = tune()
) %>%
  set_engine("kernlab") %>%
  set_mode("classification")


# Create a workflow
svm_workflow <- workflow() %>%
  add_recipe(penguin_recipe) %>%
  add_model(svm_spec)

# Create a grid of hyperparameters
svm_grid <- grid_regular(
  cost(),
  rbf_sigma(),
  levels = 5
)

# Set up cross-validation
set.seed(234)
penguin_folds <- vfold_cv(penguin_train, v = 5, strata = species)

# Tune the model
svm_tune_results <- svm_workflow %>%
  tune_grid(
    resamples = penguin_folds,
    grid = svm_grid,
    metrics = metric_set(accuracy)
  )

# Visualize tuning results
autoplot(svm_tune_results)

# Select the best hyperparameters
best_svm_params <- svm_tune_results %>%
  select_best(metric = "accuracy")
print(best_svm_params)

# Finalize the workflow with the best parameters
final_svm_workflow <- svm_workflow %>%
  finalize_workflow(best_svm_params)

# Fit the final model
final_svm_fit <- final_svm_workflow %>%
  fit(data = penguin_train)

# Evaluate on the test set
svm_test_results <- final_svm_fit %>%
  predict(new_data = penguin_test) %>%
  bind_cols(penguin_test %>% select(species))

# Calculate accuracy
accuracy(svm_test_results, truth = species, estimate = .pred_class)

# Create a confusion matrix
conf_mat(svm_test_results, truth = species, estimate = .pred_class)

### Example 5
# Create a simulated habitat suitability dataset
set.seed(789)
n <- 300

# Environmental variables
temperature <- rnorm(n, mean = 15, sd = 5)
precipitation <- rnorm(n, mean = 800, sd = 200)
elevation <- rnorm(n, mean = 500, sd = 150)
soil_ph <- rnorm(n, mean = 6.5, sd = 1)

# Complex interaction for habitat suitability
# Species prefers moderate temperatures, high precipitation, 
# lower elevations, and neutral pH
suitability_score <- -0.5 * (temperature - 15)^2 + 
  0.003 * precipitation - 
  0.001 * elevation + 
  -0.8 * (soil_ph - 7)^2 + 
  rnorm(n, 0, 2)

# Convert to binary presence/absence
presence <- factor(ifelse(suitability_score > median(suitability_score), 
                          "Present", "Absent"))

# Create the dataset
habitat_data <- tibble(
  temperature = temperature,
  precipitation = precipitation,
  elevation = elevation,
  soil_ph = soil_ph,
  presence = presence
)

# Split the data
set.seed(123)
habitat_split <- initial_split(habitat_data, prop = 0.75, strata = presence)
habitat_train <- training(habitat_split)
habitat_test <- testing(habitat_split)

# Create a recipe
habitat_recipe <- recipe(presence ~ ., data = habitat_train) %>%
  step_normalize(all_predictors())

# Create an SVM model specification
svm_spec <- svm_rbf(
  cost = 10,
  rbf_sigma = 0.1
) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# Create a workflow
svm_workflow <- workflow() %>%
  add_recipe(habitat_recipe) %>%
  add_model(svm_spec)

# Fit the model
habitat_fit <- svm_workflow %>%
  fit(data = habitat_train)

# Make predictions on the test set
habitat_preds <- habitat_fit %>%
  predict(new_data = habitat_test) %>%
  bind_cols(
    predict(habitat_fit, new_data = habitat_test, type = "prob"),
    habitat_test %>% select(presence)
  )

# Calculate metrics
accuracy(habitat_preds, truth = presence, estimate = .pred_class)

# Create a confusion matrix
conf_mat(habitat_preds, truth = presence, estimate = .pred_class)

################################################################################
############################ SVM Classification Exercises #####################
################################################################################

# Exercise 1: Predicting Tree Species from Environmental Variables

# In this exercise, you will use SVM to classify tree species based on 
# environmental variables. The dataset contains information about four tree 
# species and their environmental conditions.

# Load the simulated ecological dataset
set.seed(123)
n <- 1000
ecological_data <- tibble(
  temperature = rnorm(n, mean = 12.5, sd = 4),
  precipitation = rnorm(n, mean = 850, sd = 200),
  elevation = rnorm(n, mean = 350, sd = 110),
  soil_ph = rnorm(n, mean = 5.6, sd = 0.6),
  canopy_cover = runif(n, 30, 95),
  distance_to_water = rlnorm(n, log(300), 0.8),
  slope = runif(n, 0, 30),
  aspect = runif(n, 0, 360),
  vegetation_density = runif(n, 0, 1),
  human_disturbance = sample(0:5, size = n, replace = TRUE),
  species = factor(sample(
    c("Quercus_robur", "Pinus_sylvestris", "Betula_pendula", "Fagus_sylvatica"),
    size = n, replace = TRUE, prob = c(0.35, 0.30, 0.20, 0.15)
  ))
)


# 1. Explore the dataset
# - Calculate summary statistics for each environmental variable
# - Create visualizations to explore relationships between variables
# - Check for class imbalance in the species distribution

# 2. Prepare the data for modeling
# - Split the data into training and testing sets (75% training, 25% testing)
# - Create a recipe for preprocessing that normalizes all predictors
# - Remove highly correlated predictors (threshold = 0.8)

# 3. Train an SVM model with a radial basis function kernel
# - Use cross-validation to tune the cost and rbf_sigma parameters
# - Create a grid with 5 levels for each parameter
# - Use 5-fold cross-validation

# 4. Evaluate the model
# - Calculate accuracy, kappa, and ROC AUC on the test set
# - Create a confusion matrix
# - Visualize the results

# 5. Compare with a linear SVM
# - Train an SVM model with a linear kernel
# - Compare the performance with the RBF kernel model

# Dataset is provided as a simulated ecological dataset with the following variables:
# - temperature: Average temperature in Celsius
# - precipitation: Annual precipitation in mm
# - elevation: Elevation in meters
# - soil_ph: Soil pH
# - canopy_cover: Percentage of canopy cover
# - distance_to_water: Distance to nearest water source in meters
# - slope: Terrain slope in degrees
# - aspect: Terrain aspect in degrees
# - vegetation_density: Vegetation density index (0-1)
# - human_disturbance: Human disturbance index (0-5)
# - species: Tree species (Quercus_robur, Pinus_sylvestris, Betula_pendula, Fagus_sylvatica)

################################################################################
################### Multi-class SVM for Species Classification #################
################################################################################
### Example 1
# Create a multi-class ecological dataset
set.seed(123)
n <- 500

# Environmental variables
temperature <- rnorm(n, mean = 15, sd = 5)
precipitation <- rnorm(n, mean = 800, sd = 200)
elevation <- rnorm(n, mean = 500, sd = 150)
soil_ph <- rnorm(n, mean = 6.5, sd = 1)

# Create species probabilities based on environmental conditions
species_prob <- matrix(0, nrow = n, ncol = 5)

# Species 1: Prefers cool, wet, high elevation, acidic soil
species_prob[,1] <- -0.2 * (temperature - 10)^2 + 0.002 * precipitation + 
  0.001 * elevation - 0.5 * (soil_ph - 5)^2

# Species 2: Prefers warm, dry, low elevation, alkaline soil
species_prob[,2] <- -0.2 * (temperature - 20)^2 - 0.001 * (precipitation - 500)^2 - 
  0.001 * elevation + 0.5 * soil_ph

# Species 3: Prefers moderate temperature, moderate precipitation, mid elevation
species_prob[,3] <- -0.2 * (temperature - 15)^2 - 0.001 * (precipitation - 800)^2 - 
  0.001 * (elevation - 500)^2 - 0.5 * (soil_ph - 6.5)^2

# Species 4: Prefers cool, moderate precipitation, high elevation
species_prob[,4] <- -0.2 * (temperature - 12)^2 - 0.001 * (precipitation - 700)^2 + 
  0.001 * elevation - 0.5 * (soil_ph - 6)^2

# Species 5: Prefers warm, wet, low elevation
species_prob[,5] <- -0.2 * (temperature - 18)^2 + 0.002 * precipitation - 
  0.001 * elevation - 0.5 * (soil_ph - 7)^2

# Add random noise
for (i in 1:5) {
  species_prob[,i] <- species_prob[,i] + rnorm(n, 0, 3)
}

# Determine species based on highest probability
species <- factor(apply(species_prob, 1, which.max),
                  labels = c("Quercus_robur", "Pinus_sylvestris", 
                             "Betula_pendula", "Fagus_sylvatica", "Acer_pseudoplatanus"))

# Create the dataset
species_data <- tibble(
  temperature = temperature,
  precipitation = precipitation,
  elevation = elevation,
  soil_ph = soil_ph,
  species = species
)

# Split the data
set.seed(234)
species_split <- initial_split(species_data, prop = 0.75, strata = species)
species_train <- training(species_split)
species_test <- testing(species_split)

# Create a recipe
species_recipe <- recipe(species ~ ., data = species_train) %>%
  step_normalize(all_predictors())

# Create an SVM model specification
svm_spec <- svm_rbf(
  cost = 10,
  rbf_sigma = 0.1
) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# Create a workflow
svm_workflow <- workflow() %>%
  add_recipe(species_recipe) %>%
  add_model(svm_spec)

# Fit the model
species_fit <- svm_workflow %>%
  fit(data = species_train)

# Make predictions on the test set
species_preds <- species_fit %>%
  predict(new_data = species_test) %>%
  bind_cols(
    predict(species_fit, new_data = species_test, type = "prob"),
    species_test %>% select(species)
  )

# Calculate accuracy
accuracy(species_preds, truth = species, estimate = .pred_class)

# Create a confusion matrix
conf_mat(species_preds, truth = species, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

# Calculate multiclass metrics
species_metrics <- metric_set(accuracy, kap, mn_log_loss)
species_metrics(species_preds, truth = species, estimate = .pred_class,
                .pred_Quercus_robur, .pred_Pinus_sylvestris, 
                .pred_Betula_pendula, .pred_Fagus_sylvatica, .pred_Acer_pseudoplatanus)

### Example 2
# Create an imbalanced ecological dataset (rare species detection)
set.seed(456)
n <- 1000

# Environmental variables
temperature <- rnorm(n, mean = 15, sd = 5)
precipitation <- rnorm(n, mean = 800, sd = 200)
elevation <- rnorm(n, mean = 500, sd = 150)
soil_ph <- rnorm(n, mean = 6.5, sd = 1)

# Create a rare species scenario (10% occurrence)
rare_species_score <- -0.5 * (temperature - 12)^2 + 
  0.002 * precipitation + 
  0.001 * elevation - 
  0.8 * (soil_ph - 5.5)^2 + 
  rnorm(n, 0, 2)

# Set threshold to create 10% presence
threshold <- quantile(rare_species_score, 0.9)
presence <- factor(ifelse(rare_species_score > threshold, "Present", "Absent"))

# Create the dataset
rare_species_data <- tibble(
  temperature = temperature,
  precipitation = precipitation,
  elevation = elevation,
  soil_ph = soil_ph,
  presence = presence
)

# Check class distribution
table(rare_species_data$presence)

# Split the data
set.seed(789)
rare_split <- initial_split(rare_species_data, prop = 0.75, strata = presence)
rare_train <- training(rare_split)
rare_test <- testing(rare_split)

# Create recipes with different approaches to class imbalance

# 1. Standard recipe (no handling of class imbalance)
recipe_standard <- recipe(presence ~ ., data = rare_train) %>%
  step_normalize(all_predictors())

# 2. Recipe with SMOTE (Synthetic Minority Over-sampling Technique)
recipe_smote <- recipe(presence ~ ., data = rare_train) %>%
  step_normalize(all_predictors()) %>%
  step_smote(presence)

# 3. Recipe with up-sampling
recipe_upsample <- recipe(presence ~ ., data = rare_train) %>%
  step_normalize(all_predictors()) %>%
  step_upsample(presence)

# 4. Recipe with down-sampling
recipe_downsample <- recipe(presence ~ ., data = rare_train) %>%
  step_normalize(all_predictors()) %>%
  step_downsample(presence)

# 5. Recipe with ROSE (Random Over-Sampling Examples)
recipe_rose <- recipe(presence ~ ., data = rare_train) %>%
  step_normalize(all_predictors()) %>%
  step_rose(presence)

# Create an SVM model specification
svm_spec <- svm_rbf(
  cost = 10,
  rbf_sigma = 0.1
) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# Create workflows for each approach
workflow_standard <- workflow() %>%
  add_recipe(recipe_standard) %>%
  add_model(svm_spec)

workflow_smote <- workflow() %>%
  add_recipe(recipe_smote) %>%
  add_model(svm_spec)

workflow_upsample <- workflow() %>%
  add_recipe(recipe_upsample) %>%
  add_model(svm_spec)

workflow_downsample <- workflow() %>%
  add_recipe(recipe_downsample) %>%
  add_model(svm_spec)

workflow_rose <- workflow() %>%
  add_recipe(recipe_rose) %>%
  add_model(svm_spec)

# Fit the models
fit_standard <- workflow_standard %>% fit(data = rare_train)
fit_smote <- workflow_smote %>% fit(data = rare_train)
fit_upsample <- workflow_upsample %>% fit(data = rare_train)
fit_downsample <- workflow_downsample %>% fit(data = rare_train)
fit_rose <- workflow_rose %>% fit(data = rare_train)

# Function to evaluate models
evaluate_model <- function(fit, name) {
  preds <- fit %>%
    predict(new_data = rare_test) %>%
    bind_cols(
      predict(fit, new_data = rare_test, type = "prob"),
      rare_test %>% select(presence)
    )
  
  # Calculate metrics
  acc <- accuracy(preds, truth = presence, estimate = .pred_class)
  sens <- sensitivity(preds, truth = presence, estimate = .pred_class)
  spec <- specificity(preds, truth = presence, estimate = .pred_class)
  f1 <- f_meas(preds, truth = presence, estimate = .pred_class)
  roc <- roc_auc(preds, truth = presence, .pred_Present)
  
  # Combine metrics
  metrics <- bind_rows(acc, sens, spec, f1, roc) %>%
    mutate(model = name)
  
  return(metrics)
}

# Evaluate all models
results <- bind_rows(
  evaluate_model(fit_standard, "Standard"),
  evaluate_model(fit_smote, "SMOTE"),
  evaluate_model(fit_upsample, "Upsample"),
  evaluate_model(fit_downsample, "Downsample"),
  evaluate_model(fit_rose, "ROSE")
)

# Visualize results
results %>%
  filter(.metric != 'roc_auc') %>%
  ggplot(aes(x = .metric, y = .estimate, fill = model)) +
  geom_col(position = "dodge") +
  labs(title = "Performance Comparison of Class Imbalance Techniques",
       x = "Technique",
       y = "Metric Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create confusion matrices
conf_mat(predict(fit_standard, rare_test) %>% 
           bind_cols(rare_test %>% select(presence)), 
         truth = presence, estimate = .pred_class) %>%
  autoplot(type = "heatmap") +
  labs(title = "Standard Approach")

conf_mat(predict(fit_smote, rare_test) %>% 
           bind_cols(rare_test %>% select(presence)), 
         truth = presence, estimate = .pred_class) %>%
  autoplot(type = "heatmap") +
  labs(title = "SMOTE Approach")
