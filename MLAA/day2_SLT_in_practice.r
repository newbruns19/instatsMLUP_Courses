####################################################################################################
###
### File:    day2_SLT_in_practice.r
### Purpose: Introduce additional ML model and some important definitions in Statistical Learning
###         Theory.
### Authors: Gabriel Rodrigues Palma
### Date:    17/05/25
###
####################################################################################################
# Load packages -----
source('00_source.R')

##################################################################################################
####################################### Random Forests ###########################################
##################################################################################################
data(fgl)
View(fgl)
n_samples <- nrow(fgl)

# 2. Obtain descriptive statistics
summary(fgl)
colnames(fgl)

# 3. Visualize pairwise relationships with ggpairs
ggpairs(fgl,
        columns = c("RI", "Na", "Mg"),
        mapping = aes(color = type))

# 4. Split into training and testing sets
set.seed(123)
train_indices <- sample(1:n_samples, n_samples * 0.5)


# 5. Fit a Random Forests algorithm
?randomForest
model <- randomForest(type ~ RI + Na + Mg + Al + Si + K + Ca + Ba + Fe,
               data   = fgl[train_indices, ], localImp = TRUE)

nrow(fgl %>% drop_na())
nrow(fgl)

summary(fgl)

# Visualize the partitioning
plot(model)

# 6. Predict on test data and compute metrics
rf_results <- predict(model, fgl[-train_indices, ])
confusion_table <- table(rf_results, fgl[-train_indices, ]$type)
acc <- sum(diag(confusion_table)) / sum(confusion_table)
WinF_acc  <- confusion_table[1,1] / sum(confusion_table[1,1] + confusion_table[2,1] + confusion_table[3,1])
WinNF_acc <- confusion_table[2,2] / sum(confusion_table[1,2] + confusion_table[2,2] + confusion_table[3,2])
Veh_acc <- confusion_table[3,3] / sum(confusion_table[1,3] + confusion_table[2,3] + confusion_table[3,3])
Con_acc  <- confusion_table[1,1] / sum(confusion_table[1,1] + confusion_table[2,1] + confusion_table[3,1])
Tabl_acc <- confusion_table[2,2] / sum(confusion_table[1,2] + confusion_table[2,2] + confusion_table[3,2])
Head_acc <- confusion_table[3,3] / sum(confusion_table[1,3] + confusion_table[2,3] + confusion_table[3,3])

# Using randomForestExplainer
explain_forest(model)

##################################################################################################
####################################### SVM ######################################################
##################################################################################################
# Reuse existing train/test split
train_data <- fgl[train_indices, ]
test_data  <- fgl[-train_indices, ]

# 7.1. Create a recipe to normalize predictors
fgl_rec <- recipe(type ~ RI + Na + Mg + Al + Si + K + Ca + Ba + Fe, 
                  data = train_data) %>%
  step_normalize(all_predictors())

install.packages('kernlab', dep = T)
library('kernlab')
# 7.2. Specify an RBF SVM model
svm_spec <- svm_rbf(cost = 1) %>%            # cost can be tuned if desired
  set_engine("kernlab") %>%
  set_mode("classification")

# 7.3. Build a workflow
svm_wf <- workflow() %>%
  add_model(svm_spec) %>%
  add_recipe(fgl_rec)

# 7.4. Fit the SVM
svm_fit <- fit(svm_wf, data = train_data)

# 7.5. Predict on the test set
svm_results <- predict(svm_fit, test_data) %>%
  bind_cols(test_data)

# 7.6. Compute confusion matrix and accuracies
confusion_table_svm <- table(svm_results$.pred_class, svm_results$type)

acc_svm <- sum(diag(confusion_table_svm)) / sum(confusion_table_svm)
WinF_acc  <- confusion_table_svm[1,1] / sum(confusion_table_svm[1,1] + confusion_table_svm[2,1] + confusion_table_svm[3,1])
WinNF_acc <- confusion_table_svm[2,2] / sum(confusion_table_svm[1,2] + confusion_table_svm[2,2] + confusion_table_svm[3,2])
Veh_acc <- confusion_table_svm[3,3] / sum(confusion_table_svm[1,3] + confusion_table_svm[2,3] + confusion_table_svm[3,3])

Con_acc  <- confusion_table_svm[1,1] / sum(confusion_table_svm[1,1] + confusion_table_svm[2,1] + confusion_table_svm[3,1])
Tabl_acc <- confusion_table_svm[2,2] / sum(confusion_table_svm[1,2] + confusion_table_svm[2,2] + confusion_table_svm[3,2])
Head_acc <- confusion_table_svm[3,3] / sum(confusion_table_svm[1,3] + confusion_table_svm[2,3] + confusion_table_svm[3,3])


#######################################################################################################
####################################### Practice ######################################################
#######################################################################################################
# Using the wine dataset for classification -----
# Install and load spinifex
install.packages("spinifex", dep = T)
library(spinifex)

# Load the Wine dataset
data("wine", package = "spinifex")

# Select your own dataset for classification -----
