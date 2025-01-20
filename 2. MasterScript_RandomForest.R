###---
###  SCRIPT PARA RANDOM FOREST
###---

# install.packages("randomForest")

# Cargar paquetes necesarios
library(randomForest)
library(caret)

# Establecer el directorio de trabajo
setwd("~/GitHub/TFM-Statistical-Learning")
# setwd("C:/Users/Juan A. Arias/Desktop/TFM")

# Cargar los datos preprocesados
combined_data <- read.csv("combined_data.csv")

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_index <- createDataPartition(y = combined_data$Group, p = 0.9, list = FALSE)
train_data <- combined_data[train_index, ]
test_data <- combined_data[-train_index, ]

# Convertir la variable objetivo en un factor
train_data$Group <- as.factor(train_data$Group)
test_data$Group <- as.factor(test_data$Group)

# Definir la cuadrícula de hiperparámetros
hyper_grid <- expand.grid(
  ntree = seq(100, 500, 100),
  mtry = seq(floor(sqrt(ncol(train_data) - 1)),
             floor((ncol(train_data) - 1) / 2),
             length.out = 5)
)

# Crear una función para calcular la precisión
accuracy <- function(actual, predicted) {
  sum(actual == predicted) / length(actual)
}

# Inicializar un data.frame para almacenar los resultados
results <- data.frame()
set.seed(123)

# Bucle a través de los hiperparámetros
for (i in 1:nrow(hyper_grid)) {
  ntree <- hyper_grid[i, "ntree"]
  mtry <- hyper_grid[i, "mtry"]

  # Ajustar el modelo de Random Forest con los hiperparámetros actuales
  model <- randomForest(Group ~ ., data = train_data, ntree = ntree, mtry = mtry)

  # Calcular la precisión en el conjunto de prueba
  pred <- predict(model, newdata = test_data)
  acc <- accuracy(test_data$Group, pred)

  # Agregar los resultados al data.frame
  results <- rbind(results, data.frame(ntree = ntree, mtry = mtry, accuracy = acc))
}

# write.csv2(results, file = "decisiontree.results.csv")
results <- read.csv2(file = "decisiontree.results.csv")

# Encontrar los hiperparámetros que producen la mayor precisión
best_params <- results[which.max(results$accuracy), ]

# Ajustar el modelo final utilizando los mejores hiperparámetros
rf_model_tuned <- randomForest(Group ~ ., data = train_data, ntree = best_params$ntree, mtry = best_params$mtry)

# Predecir en el conjunto de prueba
predictions_rf_best <- predict(rf_model_tuned, newdata = test_data)

# Crear matriz de confusión y calcular la precisión
confusion_matrix_rf_best <- confusionMatrix(predictions_rf_best, test_data$Group)
saveRDS(confusion_matrix_rf_best, file = "randomforest_decisiontree.RDS")
accuracy_rf_best <- confusion_matrix_rf_best$overall["Accuracy"]

# Cargar la matriz de confusión del modelo anterior
confusion_matrix_anterior <- readRDS("C:/Users/Juan A. Arias/Desktop/TFM/Tree_confusion_matrix.RDS")
accuracy_anterior <- confusion_matrix_anterior$overall["Accuracy"]

# Comparar las precisiónes de ambos modelos
cat("Accuracy del modelo anterior: ", accuracy_anterior, "\n")
cat("Accuracy del modelo Random Forest: ", accuracy_rf_best, "\n")


# Obtener el número total de árboles
num_trees <- length(rf_model_tuned$err.rate[,1])

# Crear un gráfico vacío con las etiquetas deseadas
plot(1:num_trees, 
     rf_model_tuned$err.rate[1:num_trees,1], 
     type="n", 
     xlab="",
     ylab="",
     ylim=c(0,0.5),
     main="")

# Añadir las etiquetas de los ejes con un tamaño de fuente más grande
mtext("Número de Árboles", side = 1, line = 3, cex = 1.2)
mtext("Tasa de Error", side = 2, line = 3, cex = 1.2)

# Añadir las líneas del gráfico de error
lines(1:num_trees, rf_model_tuned$err.rate[1:num_trees,1], col = "black", lwd = 2)
lines(1:num_trees, rf_model_tuned$err.rate[1:num_trees,2], col = "red", lwd = 2)
lines(1:num_trees, rf_model_tuned$err.rate[1:num_trees,3], col = "green", lwd = 2)

# Añadir la leyenda
legend("topright", 
       legend = c("Error OOB General", "Error OOB Clase 1", "Error OOB Clase 2"), 
       lty = 1, 
       col = c("black", "red", "green"), 
       bty = "n", 
       cex = 1.2)



# Plot de la importancia de las variables
varImpPlot(rf_model_tuned, main = "", n.var = 20)

# install.packages("vivid")
library(vivid)
fit_rf  <- vivi(data = train_data, fit = rf_model_tuned, response = "Group", importanceType = "%IncMSE")
viviHeatmap(mat = fit_rf[1:5,1:5])

require(igraph)
viviNetwork(mat = fit_rf)


min_depth_frame <- min_depth_distribution(rf_model_tuned) 
plot_min_depth_distribution(min_depth_frame)
plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)

# mean_sample = "all_trees" (filling missing value): the minimal depth of a variable in a tree that does not use it for splitting is equal to the mean depth of trees. Note that the depth of a tree is equal to the length of the longest path from root to leave in this tree. This equals the maximum depth of a variable in this tree plus one, as leaves are by definition not split by any variable.
# 
# mean_sample = "top_trees" (restricting the sample): to calculate the mean minimal depth only B~
#  out of B
#  (number of trees) observations are considered, where B~
#  is equal to the maximum number of trees in which any variable was used for splitting. Remaining missing values for variables that were used for splitting less than B~
#  times are filled in as in mean_sample = "all_trees".
# 
# mean_sample = "relevant_trees" (ignoring missing values): mean minimal depth is calculated using only non-missing values.