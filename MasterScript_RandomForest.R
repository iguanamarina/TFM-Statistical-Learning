###---
###  SCRIPT PARA RANDOM FOREST
###---

# install.packages("randomForest")

# Cargar paquetes necesarios
library(randomForest)
library(caret)

# Establecer el directorio de trabajo
setwd("~/GitHub/TFM-Statistical-Learning")

# Cargar los datos preprocesados
combined_data <- read.csv("combined_data.csv")

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_index <- createDataPartition(y = combined_data$variable_objetivo, p = 0.8, list = FALSE)
train_data <- combined_data[train_index, ]
test_data <- combined_data[-train_index, ]

# Crear un modelo de Random Forest
set.seed(123)
rf_model <- randomForest(variable_objetivo ~ ., data = train_data)

# Predecir en el conjunto de prueba
predictions_rf <- predict(rf_model, newdata = test_data)

# Crear matriz de confusión y calcular la precisión
confusion_matrix_rf <- confusionMatrix(predictions_rf, test_data$variable_objetivo)
accuracy_rf <- confusion_matrix_rf$overall["Accuracy"]

# Cargar la matriz de confusión del modelo anterior
confusion_matrix_anterior <- read.csv("confusion_matrix.csv")
accuracy_anterior <- confusion_matrix_anterior$overall["Accuracy"]

# Comparar las precisiónes de ambos modelos
cat("Accuracy del modelo anterior: ", accuracy_anterior, "\n")
cat("Accuracy del modelo Random Forest: ", accuracy_rf, "\n")
