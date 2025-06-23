####################################################################################################
###
### File:    05_classification_II_with_tidymodels.R
### Purpose: Examples and exercises for classification with tidymodels
### Authors: Gabriel Rodrigues Palma
### Date:    20/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
######################## Neural Networks Fundamentals ##########################
################################################################################

# Example 1: Basic Species Classification Setup with Tidymodels
# Simulate species classification data
set.seed(42)
species_data <- tibble(
  leaf_length = rnorm(200, 5, 1.5),
  leaf_width = rnorm(200, 3, 0.8),
  petal_ratio = runif(200, 0.2, 0.8),
  species = factor(sample(c("Quercus_alba", "Quercus_rubra"), 200, 
                          replace = TRUE))
)

# Create neural network specification
nn_spec <- mlp(
  hidden_units = 5,
  epochs = 100
) %>%
  set_engine("brulee") %>%
  set_mode("classification")

# Example 2: Data Splitting for Ecological Classification
# Split data for model validation
species_split <- initial_split(species_data, prop = 0.8, 
                               strata = species)
species_train <- training(species_split)
species_test <- testing(species_split)

cat("Training samples:", nrow(species_train), "\n")
cat("Testing samples:", nrow(species_test), "\n")
cat("Species distribution:\n")
count(species_train, species)

# Example 3: Creating Recipe for Feature Preprocessing
species_recipe <- recipe(species ~ ., data = species_train) %>%
  step_normalize(all_predictors()) %>%
  step_center(all_predictors())

# Preview the preprocessing steps
species_recipe

# Example 4: Workflow Creation for Neural Network Pipeline
species_workflow <- workflow() %>%
  add_model(nn_spec) %>%
  add_recipe(species_recipe)

# Display workflow components
species_workflow

# Example 5: Model Training and Basic Predictions
# Fit the neural network model
fitted_nn <- fit(species_workflow, data = species_train)

# Make predictions on test set
predictions <- predict(fitted_nn, species_test) %>%
  bind_cols(species_test)

# Display first few predictions
head(predictions)

################################################################################
####################### Neural Networks Fundamentals Exercises ##############
################################################################################

# Exercise 1: Create Bird Migration Classification Dataset
# Task: Create variables for migratory bird classification based on wing_span, 
# body_mass, and beak_length. Classify as "Migratory" or "Resident"

# Create the dataset with 150 observations
set.seed(123)
bird_data <- tibble(
  wing_span = # Add wingspan measurements (15-35 cm range)
    body_mass = # Add body mass measurements (20-200 g range) 
    beak_length = # Add beak length measurements (1-5 cm range)
    migration_status = # Create factor with "Migratory" and "Resident"
)

# Print summary of the dataset

# Exercise 2: Setup Neural Network for Plant Disease Classification
# Task: Create a neural network specification with 8 hidden units and 150 epochs
# for classifying plant disease status based on leaf characteristics

plant_nn_spec <- # Create mlp specification
  # Set hidden units to 8
  # Set epochs to 150
  # Set engine to "brulee"
  # Set mode to "classification"
  
  # Display the model specification
  
  # Exercise 3: Create Data Preprocessing Recipe for Soil Quality
  # Task: Create a recipe that normalizes pH, organic_matter, and nitrogen levels
  # to predict soil_quality (Low, Medium, High)
  
  # Sample data
  soil_data <- tibble(
    pH = rnorm(100, 6.5, 0.8),
    organic_matter = rnorm(100, 3.2, 1.1),
    nitrogen = rnorm(100, 15, 4),
    soil_quality = factor(sample(c("Low", "Medium", "High"), 100, 
                                 replace = TRUE))
  )

soil_recipe <- # Create recipe with soil_quality as outcome
  # Add step to normalize all predictors
  # Add step to center all predictors
  
  # Exercise 4: Build Complete Workflow for Tree Height Prediction
  # Task: Create a complete workflow combining model and recipe for predicting
  # tree growth rate category based on diameter, age, and sunlight_hours
  
  # Create tree growth data
  tree_data <- tibble(
    diameter = runif(120, 5, 45),
    age = sample(5:80, 120, replace = TRUE), 
    sunlight_hours = rnorm(120, 6, 1.5),
    growth_rate = factor(sample(c("Slow", "Medium", "Fast"), 120, 
                                replace = TRUE))
  )

# Create model specification
tree_nn_spec <- # Your neural network specification here

# Create recipe  
tree_recipe <- # Your preprocessing recipe here

# Create complete workflow
tree_workflow <- # Combine model and recipe

# Exercise 5: Train Model and Evaluate Basic Performance
# Task: Use the tree_workflow to fit data and make predictions
# Calculate accuracy of predictions

# Split the tree data
tree_split <- # Create 80/20 split stratified by growth_rate
tree_train <- # Extract training data
tree_test <- # Extract testing data

# Fit the model
fitted_tree_model <- # Fit workflow to training data

# Make predictions and calculate accuracy
tree_predictions <- # Make predictions on test set
accuracy_result <- # Calculate accuracy using yardstick

# Display results

################################################################################
#################### Advanced Neural Network Techniques ########################
################################################################################
# Example 1: Hyperparameter Tuning for Pollinator Classification
# Create tunable neural network specification  
pollinator_tune_spec <- mlp(
  hidden_units = tune(),
  penalty = tune(),
  epochs = tune()
) %>%
  set_engine("brulee") %>%
  set_mode("classification")

# Define parameter ranges for ecological relevance
pollinator_grid <- grid_regular(
  hidden_units(range = c(5, 10)),
  penalty(range = c(-4, -1)),
  epochs(range = c(50, 100)),
  levels = 3
)

# Display tuning grid
pollinator_grid

# Example 2: Cross-Validation for Robust Model Assessment
# Create resampling strategy
set.seed(456)
ecosystem_folds <- vfold_cv(species_train, v = 3, strata = species)

# Perform hyperparameter tuning with cross-validation
tune_results <- tune_grid(
  pollinator_tune_spec,
  species_recipe,
  resamples = ecosystem_folds,
  grid = pollinator_grid,
  metrics = metric_set(accuracy)
)

# Show best performing models
show_best(tune_results, metric = "accuracy")

# Example 3: Model Performance Evaluation with Ecological Metrics
# Select best model parameters
best_params <- select_best(tune_results, metric = "accuracy")

# Finalize workflow with best parameters
best_model <- pollinator_tune_spec %>%
  finalize_model(best_params) 

final_workflow <- 
  workflow() %>%
  add_recipe(species_recipe) %>%
  add_model(best_model)

# Fit final model
final_fit <- fit(final_workflow, species_train)

# Comprehensive evaluation
final_predictions <- predict(final_fit, species_test, type = "prob") %>%
  bind_cols(predict(final_fit, species_test)) %>%
  bind_cols(species_test)

# Calculate multiple performance metrics
performance_metrics <- final_predictions %>%
  metrics(truth = species, estimate = .pred_class, 
          .pred_Quercus_alba)

performance_metrics


################################################################################
################# Advanced Neural Network Techniques Exercises ##############
################################################################################

# Exercise 1: Optimize Neural Network for Fish Species Classification
# Task: Create a tunable neural network with hidden_units, penalty, and epochs
# parameters for classifying fish species based on length, weight, and fin_ratio

# Create fish classification dataset
set.seed(789)
fish_data <- tibble(
  length = rnorm(180, 25, 8),
  weight = rnorm(180, 300, 120),
  fin_ratio = runif(180, 0.15, 0.45),
  species = factor(sample(c("Trout", "Bass", "Pike"), 180, 
                          replace = TRUE))
)

# Create tunable model specification
fish_tune_spec <- # Create mlp with tune() for hidden_units, penalty, epochs

# Define tuning grid
fish_grid <- # Create grid_regular with appropriate ranges

# Exercise 2: Implement Cross-Validation for Habitat Suitability
# Task: Use 10-fold cross-validation to evaluate habitat classification model
# based on temperature, humidity, and elevation

habitat_data <- tibble(
  temperature = rnorm(200, 18, 6),
  humidity = rnorm(200, 65, 15), 
  elevation = runif(200, 100, 2000),
  habitat_type = factor(sample(c("Forest", "Grassland", "Desert"), 200, 
                               replace = TRUE))
)

# Create data split and folds
habitat_split <- # Create 75/25 split
habitat_train <- # Extract training data
habitat_folds <- # Create 10-fold CV with stratification

# Create recipe and workflow
habitat_recipe <- # Normalize and center predictors
habitat_workflow <- # Combine model and recipe

# Perform tuning
habitat_tune_results <- # Use tune_grid with your workflow and folds

# Exercise 3: Evaluate Multiple Performance Metrics for Conservation
# Task: Calculate accuracy, sensitivity, specificity, and AUC for predicting
# endangered species status based on population_size, habitat_loss, and threats

conservation_data <- tibble(
  population_size = exp(rnorm(150, 6, 1.5)),
  habitat_loss = runif(150, 0, 80),
  threats = sample(1:5, 150, replace = TRUE),
  endangered_status = factor(sample(c("Safe", "Threatened", "Endangered"), 
                                    150, replace = TRUE))
)

# Split data and create workflow
conservation_split <- # Create stratified split
conservation_train <- # Training data  
conservation_test <- # Testing data

# Fit model and make predictions
conservation_fit <- # Fit your workflow
conservation_pred <- # Make predictions with probabilities

# Calculate comprehensive metrics
conservation_metrics <- # Use metrics() with multiple metrics

# Exercise 4: Analyze Variable Importance for Ecosystem Health
# Task: Use variable importance analysis to understand which factors most
# influence ecosystem health predictions
  
ecosystem_data <- tibble(
    biodiversity_index = rnorm(100, 0.7, 0.2),
    water_quality = rnorm(100, 75, 15),
    pollution_level = runif(100, 0, 50),
    human_impact = rnorm(100, 30, 12),
    ecosystem_health = factor(sample(c("Poor", "Good", "Excellent"), 100, 
                                     replace = TRUE))
  )

# Create and fit model
ecosystem_workflow <- # Your complete workflow
ecosystem_fit <- # Fit to data
  