##################################################################################################
############################################ Exercise #################################################
##################################################################################################
source('00_source.R')
# Passo 1
new_dataset <- read.csv('NFRUTOS SIDERICA PMF Saluidade Dataset.csv')
colnames(new_dataset) <- c('Genotipo', 'n_frutos', 
                           'severidade', 'p_m_f', 
                           'salinidade')
new_dataset$salinidade <- factor(new_dataset$salinidade)
new_dataset$Genotipo <- factor(new_dataset$Genotipo)
new_dataset$n_frutos <- as.numeric(new_dataset$n_frutos)
new_dataset <- new_dataset %>% 
                dplyr::select(-Genotipo)

# Passo 2
summary(new_dataset)
new_dataset %>% 
  group_by(salinidade) %>%
  summarise(mean = mean(n_frutos))

# Passo 3
ggpairs(new_dataset,
        columns = c("n_frutos", "severidade", "p_m_f"),
        mapping = aes(color = salinidade))

new_dataset %>%
  dplyr::select(-severidade)

n_samples <- nrow(new_dataset)
train_indices <- sample(1:n_samples, n_samples*0.5) 

x_train_data <- new_dataset[train_indices,] %>% 
  dplyr::select(-salinidade) %>%
  as.matrix()
y_train_data <- new_dataset[train_indices,] %>% 
  dplyr::select(salinidade) %>%
  as.matrix()

x_test_data <- new_dataset[-train_indices,] %>%
  dplyr::select(-salinidade) %>%
  as.matrix()
y_test_data <- new_dataset[-train_indices,] %>% 
  dplyr::select(salinidade) %>%
  as.matrix()

knn_results <- knn(train = x_train_data, 
                   test = x_test_data,
                   cl = y_train_data, 
                   prob = T)

# 4.1) extract "probability" of the positive class
#    class::knn stores the proportion of votes for the *winning* class
#    so we need to flip when the winning class is the negative level
positive_label <- 'ALTA'

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


