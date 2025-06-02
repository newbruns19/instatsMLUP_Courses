####################################################################################################
###
### File:    day2_Cross_Validation.r
### Purpose: Various ways of performing Cross validation with the presented learning algorithms
### Authors: Gabriel Rodrigues Palma
### Date:    17/05/25
###
####################################################################################################
# Load packages -----
source('00_source.R')

##################################################################################################
####################################### SVM ######################################################
##################################################################################################
# install.packages("e1071")
library(e1071)

# here we use 5-fold CV:
svm_cv5 <- tune.svm(
  type ~ RI + Na + Mg + Al + Si + K + Ca + Ba + Fe,
  data        = fgl,
  kernel      = "radial",
  cost        = c(0.1, 1, 10),
  tunecontrol = tune.control(cross = 5)
)

# View the cross‐validated performance for each cost
print(svm_cv5)    

# For leave-one-out CV simply set cross = nrow(fgl):
svm_loocv <- tune.svm(
  type ~ RI + Na + Mg + Al + Si + K + Ca + Ba + Fe,
  data        = fgl,
  kernel      = "radial",
  cost        = c(0.1, 1, 10),
  tunecontrol = tune.control(cross = nrow(fgl))
)

print(svm_loocv)  
################################################################################################################
####################################### Creating your own ######################################################
################################################################################################################
#1. Criar grupos com mesmo numbero de dados
#2. avaliar cada grupo como teste, e treinar com todos os
#complementares
#3. Calcular uma media de todas os k performances
k_fold <- funciton(k_grupos, n_observacoes, dataset){
  # Funcao para a criacao de validacao cruzada
  #Entrada: k_grupos: Numero de grupos utilizados na validacao
  #         n_observacoes: Numero de observacoers por grupo
  #         dataset:   
  #Saida:
  #     performance_media: A media de acuracias por cada k grupo
  dataset$grupos <- ?gl(0) # 1. 
  metricas <- numeric(k_grupos)
  for(k in k_grupos){
    dados_teste <- dataset %>%
      filter(grupos == k)
    dados_treinamento <- dataset %>%
      filter(grupos != k)
    
    #2. 
  }
  #3
  accuracia_media <- mean(metricas)
  return(accuracia_media)
  
}

