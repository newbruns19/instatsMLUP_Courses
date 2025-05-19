####################################################################################################
###
### File:    00_source.R
### Purpose: Load required packages and functions used in the MLAA lecture.
### Authors: Gabriel Rodrigues Palma
### Date:    16/05/25
###
####################################################################################################

### packages required
packages <- c('tidyverse', # data-wrangling + plotting
              'here', # efficient file structures
              'randomForest',
              'readxl', # read excel files
              'writexl', # Write excel files
              'randomForestExplainer',
              'DescTools', 
              'glmnet', 
              'lightgbm', 
              'MASS', 
              'rpart', 
              'tidymodels', 
              'keras',
              'GGally', 
              'class', 
              'pROC', 
              'rpart.plot'
)
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

######################## Functions used in this class ####################################
theme_new <- function(base_size = 16, base_family = "Arial"){
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(size = 16, colour = "grey30"),
      legend.key=element_rect(colour=NA, fill =NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks =         element_line(colour = "grey20"),
      plot.title.position = 'plot',
      legend.position = "bottom"
    )
}
