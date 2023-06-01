###########################################
###     OVERALL  RESULTS
###########################################

# Generar dataframe

resultados <- data.frame(
  Metodología = rep(NA, 5),
  Precisión = rep(NA, 5)
)

# Cargar cosas
decisiontree_confusionmatrix <- readRDS("~/GitHub/TFM-Statistical-Learning/decisiontree_confusionmatrix.RDS")
randomforest_decisiontree <- readRDS("~/GitHub/TFM-Statistical-Learning/randomforest_decisiontree.RDS")
svm_tuned_confusion_matrix <- readRDS("~/GitHub/TFM-Statistical-Learning/svm_tuned_confusion_matrix.RDS")

# Árbol de Decision
resultados$Metodología[1] <- "Árbol de Decisión"
resultados$Precisión[1] <- decisiontree_confusionmatrix$overall["Accuracy"] 

# Random Forest
resultados$Metodología[2] <- "Bosques Aleatorios"
resultados$Precisión[2] <- randomforest_decisiontree$overall["Accuracy"]

# Support Vector Machine
resultados$Metodología[3] <- "Máquinas de Vectores de Soporte"
resultados$Precisión[3] <- svm_tuned_confusion_matrix$overall["Accuracy"] 

# Aprendizaje Profundo
resultados$Metodología[4] <- "Aprendizaje Profundo (AP)"
resultados$Precisión[4] <- 0.85 

# Aprendizaje Funcional Profundo
resultados$Metodología[5] <- "AP & Datos Funcionales"
resultados$Precisión[5] <- 0.85 




