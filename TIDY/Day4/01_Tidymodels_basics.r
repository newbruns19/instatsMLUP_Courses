####################################################################################################
###
### File:    01_Tidymodels_basics.R
### Purpose: Examples and exercises for tidymodels basics
### Authors: Gabriel Rodrigues Palma
### Date:    20/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
############################ Tidymodels Basics ##############################
################################################################################
urchins <-
  # Data were assembled for a tutorial 
  # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
  # Change the names to be a little more verbose
  setNames(c("food_regime", "initial_volume", "width")) %>% 
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))
colnames(urchins)

linear_reg() %>%
  fit(width ~ initial_volume * food_regime, data = urchins)

fit <- lm(width ~ initial_volume * food_regime, data = urchins)
fit_simpler <- lm(width ~ initial_volume + food_regime, data = urchins)
anova(fit_simpler, fit, test = 'F')



#> Rows: 72 Columns: 3
#> ── Column specification ──────────────────────────────────────────────
#> Delimiter: ","
#> chr (1): TREAT
#> dbl (2): IV, SUTW
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
# Load simulated bird abundance data
bird_data <- tibble(
  site_id = 1:100,
  habitat_type = sample(c("Forest", "Grassland", "Wetland"), 100, 
                        replace = TRUE),
  temperature = runif(100, 15, 30),
  precipitation = runif(100, 500, 1500),
  bird_abundance = rpois(100, 20)
)
glimpse(bird_data)
as_tibble(bird_data)
# Example 2: Creating data splits for ecological modeling
set.seed(123)
bird_split <- initial_split(bird_data, prop = 0.8, strata = habitat_type)
bird_train <- training(bird_split)
bird_test <- testing(bird_split)
cat("Training set size:", nrow(bird_train), "\n")
cat("Testing set size:", nrow(bird_test), "\n")

# Example 3: Basic recipe creation for species data preprocessing
species_recipe <- recipe(bird_abundance ~ ., data = bird_train) %>%
  step_dummy(habitat_type) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_predictors())
species_recipe

# Example 4: Model specification for ecological count data
poisson_spec <- poisson_reg() %>%
  set_engine("glmnet") %>%
  set_mode("regression")
poisson_spec

# Example 5: Creating workflows for reproducible ecology analysis
bird_workflow <- workflow() %>%
  add_recipe(species_recipe) %>%
  add_model(poisson_spec)
print(bird_workflow)

################################################################################
############################ Tidymodels Basics Exercises ####################
################################################################################

# Exercise 1: Forest Biomass Dataset Creation and Exploration
# Create a forest biomass dataset for analysis
set.seed(42)
forest_data <- tibble(
  plot_id = 1:200,
  forest_type = sample(c("deciduous", "coniferous", "mixed"), 200, replace = TRUE),
  age = sample(10:80, 200, replace = TRUE),
  dbh = rnorm(200, mean = 25, sd = 8),  # diameter at breast height (cm)
  precipitation = rnorm(200, mean = 1200, sd = 300),  # annual precipitation (mm)
  biomass = 0.5 * age + 2 * dbh + 0.002 * precipitation + 
    ifelse(forest_type == "coniferous", 10, 
           ifelse(forest_type == "mixed", 5, 0)) + 
    rnorm(200, 0, 15)  # total biomass (kg)
) %>%
  filter(dbh > 0, precipitation > 0, biomass > 0) %>%
  mutate(forest_type = factor(forest_type))

# Required actions:
# 1. Examine the structure of the dataset using glimpse()
# 2. Create summary statistics using summary()
# 3. Visualize the relationship between dbh and biomass
# 4. Create an initial split with 70% training data using initial_split()
# 5. Extract training and testing sets using training() and testing()

# Your code here:

# Exercise 2: Recipe Building for Tree Growth Prediction


# Using the forest_data from Exercise 1, build a preprocessing recipe
# Target variable: biomass
# Predictors: forest_type, age, dbh, precipitation

# Required actions:
# 1. Create a recipe using recipe() with biomass as outcome
# 2. Add step_dummy() for categorical variables (forest_type)
# 3. Add step_normalize() for all numeric predictors
# 4. Add step_corr() to remove highly correlated predictors (threshold = 0.9)
# 5. Print the recipe to see all steps

# Your code here:

# Exercise 3: Model Specification for Ecological Data


# Create a species abundance dataset
set.seed(123)
species_data <- tibble(
  site_id = 1:150,
  temperature = rnorm(150, mean = 15, sd = 5),
  rainfall = rnorm(150, mean = 800, sd = 200),
  elevation = sample(100:1500, 150, replace = TRUE),
  habitat_type = sample(c("grassland", "forest", "wetland"), 150, replace = TRUE),
  abundance = pmax(0, 
                   20 + 2 * temperature - 0.01 * elevation + 0.005 * rainfall +
                     ifelse(habitat_type == "forest", 15,
                            ifelse(habitat_type == "wetland", 10, 0)) +
                     rnorm(150, 0, 8))
) %>%
  mutate(habitat_type = factor(habitat_type))

# Required actions:
# 1. Specify a linear regression model using linear_reg() with "lm" engine
# 2. Specify a random forest model using rand_forest() with "ranger" engine
# 3. Specify a k-nearest neighbors model using nearest_neighbor() with "kknn" engine
# 4. Set the mode to "regression" for all models
# 5. Print each model specification to verify the setup

# Your code here:


# Exercise 4: Workflow Creation and Model Fitting

# Using the species_data and one of your models from Exercise 3
# Create a complete modeling workflow

# Required actions:
# 1. Create a recipe for species abundance prediction with the following steps:
#    - Use abundance as outcome, all other variables as predictors
#    - Add step_dummy() for habitat_type
#    - Add step_normalize() for numeric predictors
# 2. Create a workflow using workflow()
# 3. Add your recipe using add_recipe()
# 4. Add your chosen model using add_model()
# 5. Split the species_data (70% training)
# 6. Fit the workflow to training data using fit()
# 7. Print the fitted workflow summary

# Your code here:


# Exercise 5: Cross-Validation for Species Richness Analysis

    

################################################################################
#################### Advanced Tidymodels Components ##########################
################################################################################
habitat_type <- sample(c("Forest", "Grassland", "Wetland"), 100, replace = TRUE)

# Simulate elevation (higher in Forest, mid in Grassland, lower in Wetland for realism)
elev <- rnorm(100, 
              mean = ifelse(habitat_type == "Forest", 350,
                            ifelse(habitat_type == "Grassland", 250, 100)),
              sd = 30
)

# Set baseline bird_abundance and add effects: 
# - higher for Forest, positive effect for elevation, some random noise
bird_abundance <- 
  10 + 
  ifelse(habitat_type == "Forest", 12, 
         ifelse(habitat_type == "Grassland", 6, 0)) +
  0.06 * elev + 
  rnorm(100, 0, 5)

# Combine into a tibble
bird_data <- tibble(
  bird_abundance = bird_abundance,
  habitat_type = habitat_type,
  elev = elev
)
n_samples <- nrow(bird_data)
indices <- sample(x = 1:n_samples, size = n_samples * 0.2)
training_set <- bird_data[indices, ]
testing_set <- bird_data[-indices, ]

# Create a data split
bird_split <- initial_split(bird_data, strata = habitat_type)
bird_train <- training(bird_split)
bird_test <- testing(bird_split)

# Example 1: Cross-validation folds (stratified if habitat_type exists)
bird_folds <- vfold_cv(bird_train, v = 3, strata = habitat_type)
print(bird_folds)

# Example 2: Define recipe, model, and workflow for predictions
bird_recipe <- recipe(bird_abundance ~ habitat_type + elev, data = bird_train)

bird_model <- linear_reg() %>%
  set_engine("lm")

bird_workflow <- workflow() %>%
  add_recipe(bird_recipe) %>%
  add_model(bird_model)

# Fit with cross-validation resamples
bird_fit_rs <- bird_workflow %>%
  fit_resamples(resamples = bird_folds)
collect_metrics(bird_fit_rs)

# Example 3: Model validation from a data split
bird_final_fit <- bird_workflow %>%
  last_fit(bird_split)
collect_metrics(bird_final_fit)

# Example 4: Extract predictions for evaluation
bird_predictions <- bird_final_fit %>%
  collect_predictions()
head(bird_predictions)

# Example 5: Visualization of model performance
bird_predictions %>%
  ggplot(aes(x = bird_abundance, y = .pred)) +
  geom_point(alpha = 0.6) +
  geom_abline(color = "red") +
  labs(title = "Bird Abundance: Predicted vs Actual")

################################################################################
#################### Advanced Tidymodels Exercises ###########################
################################################################################
set.seed(42)
forest_data <- tibble(
  plot_id = 1:200,
  forest_type = sample(c("deciduous", "coniferous", "mixed"), 200, replace = TRUE),
  age = sample(10:80, 200, replace = TRUE),
  dbh = rnorm(200, mean = 25, sd = 8),
  precipitation = rnorm(200, mean = 1200, sd = 300),
  biomass = 0.5 * age + 2 * dbh + 0.002 * precipitation +
    ifelse(forest_type == "coniferous", 10, ifelse(forest_type == "mixed", 5, 0)) +
    rnorm(200, 0, 15)
) %>%
  filter(dbh > 0, precipitation > 0, biomass > 0) %>%
  mutate(forest_type = factor(forest_type))

# Exercise 1: Extract and analyze model metrics
# Use collect_metrics() and visualize RMSE across folds


# Exercise 2: Create prediction intervals for ecological forecasts
# Generate predictions with confidence intervals


# Exercise 3: Model diagnostics for ecological data
# Check residuals
