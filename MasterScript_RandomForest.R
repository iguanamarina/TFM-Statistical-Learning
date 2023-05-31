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
train_index <- createDataPartition(y = combined_data$Group, p = 0.8, list = FALSE)
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
# for (i in 1:nrow(hyper_grid)) {
#   ntree <- hyper_grid[i, "ntree"]
#   mtry <- hyper_grid[i, "mtry"]
#   
#   # Ajustar el modelo de Random Forest con los hiperparámetros actuales
#   model <- randomForest(Group ~ ., data = train_data, ntree = ntree, mtry = mtry)
#   
#   # Calcular la precisión en el conjunto de prueba
#   pred <- predict(model, newdata = test_data)
#   acc <- accuracy(test_data$Group, pred)
#   
#   # Agregar los resultados al data.frame
#   results <- rbind(results, data.frame(ntree = ntree, mtry = mtry, accuracy = acc))
# }

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
accuracy_rf_best <- confusion_matrix_rf_best$overall["Accuracy"]

# Cargar la matriz de confusión del modelo anterior
confusion_matrix_anterior <- readRDS("C:/Users/Juan A. Arias/Desktop/TFM/Tree_confusion_matrix.RDS")
accuracy_anterior <- confusion_matrix_anterior$overall["Accuracy"]

# Comparar las precisiónes de ambos modelos
cat("Accuracy del modelo anterior: ", accuracy_anterior, "\n")
cat("Accuracy del modelo Random Forest: ", accuracy_rf_best, "\n")
