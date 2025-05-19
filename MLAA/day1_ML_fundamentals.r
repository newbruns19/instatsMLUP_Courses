####################################################################################################
###
### File:    day1_ML_fundamentals.r
### Purpose: Introduce simple algorithms, and packages commonly used in ML. Also, the
###         this file also contain important metrics commonly extracted in ML papers to
###         evaluate the performance of the algoriths
### Authors: Gabriel Rodrigues Palma
### Date:    16/05/25
###
####################################################################################################
# Example of using Rpart documentation and how to find them
install.packages('rpart', dep = T)
?randomForest
?rpart
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
fit2 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
              parms = list(prior = c(.65,.35), split = "information"))
fit3 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
              control = rpart.control(cp = 0.05))
par(mfrow = c(1,1)) # otherwise on some devices the text is clipped
plot(fit)
text(fit, use.n = TRUE)
plot(fit2)
text(fit2, use.n = TRUE)
plot(fit)

##################################################################################################
############################################ KNN #################################################
##################################################################################################
# Load packages -----
source('00_source.R')

set.seed(123)

#### Scenario 1 -----
# 1. Create a dataset inspired by agriculture
n_samples <- 200
agri_data <- tibble(soil_moisture = runif(n_samples, 10, 40),
                    rainfall = rnorm(n_samples, mean = 100, sd = 20)) %>%
            mutate(yield = 0.5 * soil_moisture + 0.3 * rainfall + rnorm(n_samples, 0, 5),
                  high_yield = factor(if_else(yield > median(yield), "high", "low")))

# 2. Obtain descriptive statistics
summary(agri_data)

# 3. Visualize pairwise relationships with ggpairs
ggpairs(agri_data,
        columns = c("soil_moisture", "rainfall", "yield"),
        mapping = aes(color = high_yield))

# 4. Split into training and testing sets
train_indices <- sample(1:n_samples, n_samples*0.8) 

x_train_data <- agri_data[train_indices,] %>% 
                  dplyr::select(-high_yield) %>%
                  as.matrix()
y_train_data <- agri_data[train_indices,] %>% 
                  dplyr::select(high_yield) %>%
                  as.matrix()

x_test_data <- agri_data[-train_indices,] %>%
                  dplyr::select(-high_yield) %>%
                  as.matrix()
y_test_data <- agri_data[-train_indices,] %>% 
                  dplyr::select(high_yield) %>%
                  as.matrix()

?knn
knn_results <- knn(train = x_train_data, 
                   test = x_test_data,
                   cl = y_train_data, 
                   prob = T)

# 4.1) extract "probability" of the positive class
#    class::knn stores the proportion of votes for the *winning* class
#    so we need to flip when the winning class is the negative level
positive_label <- 'high'

# attr(knn_out,"prob") is the vote proportion for the predicted class
vote_prob <- attr(knn_results, "prob")  

# build a vector of P(y = positive_label)
pred_prob_pos <- ifelse(
  knn_results == positive_label,
  vote_prob,
  1 - vote_prob
)

confusion_table <- table(knn_results, y_test_data)
acc <- sum(diag(confusion_table))/sum(confusion_table)
tpr <- confusion_table[1, 1]/sum(confusion_table[1, 1] + confusion_table[2, 1])
fpr <- confusion_table[2, 2]/sum(confusion_table[1, 2] + confusion_table[2, 2])

# 5) ROC curve and AUC with pROC
roc_obj <- roc(
  response = factor(y_test_data),
  predictor = pred_prob_pos,
  levels = levels(factor(y_test_data))
)

# plot ROC
plot(roc_obj, col = "#1c61b6", lwd = 2,
     main = "KNN ROC Curve")

# AUC
auc_val <- auc(roc_obj)
cat(sprintf("AUC = %.3f\n", auc_val))

#### Scenario 2 -----
# 1. Create a dataset inspired by agriculture
View(iris)
n_samples <- nrow(iris)
# 2. Obtain descriptive statistics
summary(iris)
colnames(iris)
# 3. Visualize pairwise relationships with ggpairs
ggpairs(iris,
        columns = c("Sepal.Length", "Sepal.Width", "Petal.Length"),
        mapping = aes(color = Species))

# 4. Split into training and testing sets
train_indices <- sample(1:n_samples, n_samples*0.5) 

x_train_data <- iris[train_indices,] %>% 
  dplyr::select(-Species) %>%
  as.matrix()
y_train_data <- iris[train_indices,] %>% 
  dplyr::select(Species) %>%
  as.matrix()

x_test_data <- iris[-train_indices,] %>%
  dplyr::select(-Species) %>%
  as.matrix()
y_test_data <- iris[-train_indices,] %>% 
  dplyr::select(Species) %>%
  as.matrix()

knn_results <- knn(train = x_train_data, 
                   test = x_test_data,
                   cl = y_train_data, 
                   prob = T)

confusion_table <- table(knn_results, y_test_data)
acc <- sum(diag(confusion_table))/sum(confusion_table)
setosa_acc <- confusion_table[1, 1]/sum(confusion_table[1, 1] + 
                                        confusion_table[2, 1] + 
                                        confusion_table[3, 1])
versicolor_acc <- confusion_table[2, 2]/sum(confusion_table[1, 2] + 
                                              confusion_table[2, 2] + 
                                              confusion_table[3, 2])
virginica_acc <- confusion_table[3, 3]/sum(confusion_table[1, 3] + 
                                             confusion_table[2, 3] + 
                                             confusion_table[3, 3])

##################################################################################################
############################################ ANN #################################################
##################################################################################################

n_samples <- 500
agri_data <- tibble(soil_moisture = runif(n_samples, 10, 40),
                    rainfall = rnorm(n_samples, mean = 100, sd = 20)) %>%
  mutate(yield = 0.5 * soil_moisture + 0.3 * rainfall + rnorm(n_samples, 0, 5),
         high_yield = factor(if_else(yield > median(yield), "high", "low")))

# 2. Obtain descriptive statistics
summary(agri_data)

# 3. Visualize pairwise relationships with ggpairs
ggpairs(agri_data,
        columns = c("soil_moisture", "rainfall", "yield"),
        mapping = aes(color = high_yield))

# 4. Split into training and testing sets
train_indices <- sample(1:n_samples, n_samples*0.8) 

x_train_data <- agri_data[train_indices,] %>% 
  dplyr::select(-high_yield) %>%
  as.matrix()
y_train_data <- agri_data[train_indices,] %>% 
  dplyr::select(high_yield) %>%
  mutate(high_yield = ifelse(high_yield == 'high', 1, 0)) %>%
  as.matrix()

x_test_data <- agri_data[-train_indices,] %>%
  dplyr::select(-high_yield) %>%
  as.matrix()
y_test_data <- agri_data[-train_indices,] %>% 
  dplyr::select(high_yield) %>%
  mutate(high_yield = ifelse(high_yield == 'high', 1, 0)) %>%
  as.matrix()

?keras_model_sequential
model <- keras_model_sequential()

model %>%
  # Adds a densely-connected layer with 4 units to the model:
  layer_dense(units = 1, activation = 'sigmoid') 

model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)

model %>% fit(
  x_train_data,
  y_train_data,
  epochs = 200,
  batch_size = 5
)
perceptron_results <- model %>% predict(x_test_data, batch_size = 5)
perceptron_results <- round(perceptron_results)

confusion_table <- table(perceptron_results, y_test_data)
acc <- sum(diag(confusion_table))/sum(confusion_table)
tpr <- confusion_table[1, 1]/sum(confusion_table[1, 1] + confusion_table[2, 1])
fpr <- confusion_table[2, 2]/sum(confusion_table[1, 2] + confusion_table[2, 2])

model %>% evaluate(x_test_data, y_test_data, batch_size = 5)

#### Scenario 2 -----
# 1. Create a dataset inspired by agriculture
View(iris)
n_samples <- nrow(iris)
# 2. Obtain descriptive statistics
summary(iris)
colnames(iris)
# 3. Visualize pairwise relationships with ggpairs
ggpairs(iris,
        columns = c("Sepal.Length", "Sepal.Width", "Petal.Length"),
        mapping = aes(color = Species))

# 4. Split into training and testing sets
train_indices <- sample(1:n_samples, n_samples*0.5) 

x_train_data <- iris[train_indices,] %>% 
  dplyr::select(-Species) %>%
  as.matrix()
y_train_data <- iris[train_indices,] %>% 
  dplyr::select(Species) %>%
  mutate(versicolor = ifelse(Species == 'versicolor', 1, 0), 
         setosa = ifelse(Species == 'setosa', 1, 0), 
         virginica = ifelse(Species == 'virginica', 1, 0)) %>%
  dplyr::select(-Species) %>%
  as.matrix()

x_test_data <- iris[-train_indices,] %>%
  dplyr::select(-Species) %>%
  as.matrix()
y_test_data <- iris[-train_indices,] %>% 
  dplyr::select(Species) %>%
  mutate(versicolor = ifelse(Species == 'versicolor', 1, 0), 
         setosa = ifelse(Species == 'setosa', 1, 0), 
         virginica = ifelse(Species == 'virginica', 1, 0)) %>%
  dplyr::select(-Species) %>%
  as.matrix()

model <- keras_model_sequential()

model %>%
  # Adds a densely-connected layer with 4 units to the model:
  layer_dense(units = 4, activation = 'sigmoid') %>%
  layer_dense(units = 3, activation = 'sigmoid') 

model %>% compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',
  metrics = list('categorical_accuracy')
)

model %>% fit(
  x_train_data,
  y_train_data,
  epochs = 200,
  batch_size = 5
)
mlp_results <- model %>% predict(x_test_data, batch_size = 5)
mlp_results <- apply(mlp_results, MARGIN = 1, FUN = function(x) which.max(x))
y_test_data_transformed <- as.vector(apply(y_test_data, MARGIN = 1, FUN = function(x) which.max(x)))

confusion_table <- table(mlp_results, y_test_data_transformed)
acc <- sum(diag(confusion_table))/sum(confusion_table)
setosa_acc <- confusion_table[1, 1]/sum(confusion_table[1, 1] + 
                                          confusion_table[2, 1] + 
                                          confusion_table[3, 1])
versicolor_acc <- confusion_table[2, 2]/sum(confusion_table[1, 2] + 
                                              confusion_table[2, 2] + 
                                              confusion_table[3, 2])
virginica_acc <- confusion_table[3, 3]/sum(confusion_table[1, 3] + 
                                             confusion_table[2, 3] + 
                                             confusion_table[3, 3])
model %>% evaluate(x_test_data, y_test_data, batch_size = 5)

##################################################################################################
############################################ DT #################################################
##################################################################################################

#### Scenario 1 -----
# 1. Create a dataset inspired by agriculture
n_samples <- 500
agri_data <- tibble(soil_moisture = runif(n_samples, 10, 40),
                    rainfall      = rnorm(n_samples, mean = 100, sd = 20)) %>%
  mutate(yield      = 0.5 * soil_moisture + 0.3 * rainfall + rnorm(n_samples, 0, 5),
         high_yield = factor(if_else(yield > median(yield), "high", "low")))

# 2. Obtain descriptive statistics
summary(agri_data)

# 3. Visualize pairwise relationships with ggpairs
ggpairs(agri_data,
        columns = c("soil_moisture", "rainfall", "yield"),
        mapping = aes(color = high_yield))

# 4. Split into training and testing sets
set.seed(123)
train_indices <- sample(1:n_samples, n_samples * 0.8)
x_train_data  <- agri_data[train_indices, ] %>% 
                  dplyr::select(-high_yield) %>% 
                  as.matrix()
y_train_data  <- agri_data[train_indices, ] %>% 
                  dplyr::select(high_yield) %>%
                  mutate(high_yield = ifelse(high_yield == "high", 1, 0)) %>%
                  as.matrix()
x_test_data   <- agri_data[-train_indices, ] %>% 
                  dplyr::select(-high_yield) %>% 
                  as.matrix()
y_test_data   <- agri_data[-train_indices, ] %>% 
                  dplyr::select(high_yield) %>%
                  mutate(high_yield = ifelse(high_yield == "high", 1, 0)) %>%
                  as.matrix()

# 5. Fit decision tree model
model <- rpart(high_yield ~ soil_moisture + rainfall,
               data   = agri_data[train_indices, ],
               method = "class")

# Visualize the partitioning
rpart.plot(model, type = 4)

# 6. Predict on test data and compute metrics
dt_results <- predict(model, agri_data[-train_indices, ], type = "class")
confusion_table <- table(dt_results, agri_data[-train_indices, ]$high_yield)
acc <- sum(diag(confusion_table)) / sum(confusion_table)
tpr <- confusion_table[1,1] / sum(confusion_table[1,1] + confusion_table[2,1])
fpr <- confusion_table[2,2] / sum(confusion_table[1,2] + confusion_table[2,2])

confusion_table
acc
tpr
fpr

#### Scenario 2 -----
# 1. Create a dataset inspired by agriculture
View(iris)
n_samples <- nrow(iris)

# 2. Obtain descriptive statistics
summary(iris)
colnames(iris)

# 3. Visualize pairwise relationships with ggpairs
ggpairs(iris,
        columns = c("Sepal.Length", "Sepal.Width", "Petal.Length"),
        mapping = aes(color = Species))

# 4. Split into training and testing sets
set.seed(123)
train_indices <- sample(1:n_samples, n_samples * 0.5)
x_train_data  <- iris[train_indices, ] %>% 
                  dplyr::select(-Species) %>% 
                  as.matrix()
y_train_data  <- iris[train_indices, ] %>% dplyr::select(Species) %>%
                  mutate(versicolor = ifelse(Species == "versicolor", 1, 0),
                         setosa     = ifelse(Species == "setosa",     1, 0),
                         virginica  = ifelse(Species == "virginica",  1, 0)) %>%
                  dplyr::select(-Species) %>%
                  as.matrix()
x_test_data <- iris[-train_indices, ] %>% 
                dplyr::select(-Species) %>% 
                as.matrix()
y_test_data <- iris[-train_indices, ] %>% 
                dplyr::select(Species) %>%
                mutate(versicolor = ifelse(Species == "versicolor", 1, 0),
                       setosa     = ifelse(Species == "setosa",     1, 0),
                       virginica  = ifelse(Species == "virginica",  1, 0)) %>%
                dplyr::select(-Species) %>%
                as.matrix()

# 5. Fit decision tree model
model <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
               data   = iris[train_indices, ],
               method = "class")

# Visualize the partitioning
rpart.plot(model, type = 4)

# 6. Predict on test data and compute metrics
dt_results <- predict(model, iris[-train_indices, ], type = "class")
confusion_table <- table(dt_results, iris[-train_indices, ]$Species)
acc <- sum(diag(confusion_table)) / sum(confusion_table)
setosa_acc  <- confusion_table[1,1] / sum(confusion_table[1,1] + confusion_table[2,1] + confusion_table[3,1])
versicolor_acc <- confusion_table[2,2] / sum(confusion_table[1,2] + confusion_table[2,2] + confusion_table[3,2])
virginica_acc <- confusion_table[3,3] / sum(confusion_table[1,3] + confusion_table[2,3] + confusion_table[3,3])

confusion_table
acc
setosa_acc
versicolor_acc
virginica_acc