###---
###  SCRIPT PARA RANDOM FOREST
###---

# install.packages("randomForest")

# Cargar paquetes necesarios
library(randomForest)
library(caret)

# Establecer el directorio de trabajo
# setwd("~/GitHub/TFM-Statistical-Learning")
setwd("C:/Users/Juan A. Arias/Desktop/TFM")

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

# Definir el método de entrenamiento y control
train_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  search = "grid",
  classProbs = TRUE,
  summaryFunction = multiClassSummary
)

# Entrenar el modelo con la búsqueda en cuadrícula
set.seed(123)
rf_model_tuned <- train(
  Group ~ .,
  data = train_data,
  method = "rf",
  metric = "Accuracy",
  trControl = train_control,
  tuneGrid = hyper_grid
)

# Obtener los mejores hiperparámetros encontrados
best_hyper <- rf_model_tuned$bestTune

# Entrenar un modelo de Random Forest con los mejores hiperparámetros
set.seed(123)
rf_model_best <- randomForest(Group ~ ., data = train_data, ntree = best_hyper$ntree, mtry = best_hyper$mtry)

# Predecir en el conjunto de prueba
predictions_rf_best <- predict(rf_model_best, newdata = test_data)

# Crear matriz de confusión y calcular la precisión
confusion_matrix_rf_best <- confusionMatrix(predictions_rf_best, test_data$Group)
accuracy_rf_best <- confusion_matrix_rf_best$overall["Accuracy"]

# Cargar la matriz de confusión del modelo anterior
confusion_matrix_anterior <- readRDS("C:/Users/Juan A. Arias/Desktop/TFM/Tree_confusion_matrix.RDS")
accuracy_anterior <- confusion_matrix_anterior$overall["Accuracy"]

# Comparar las precisiónes de ambos modelos
cat("Accuracy del modelo anterior: ", accuracy_anterior, "\n")
cat("Accuracy del modelo Random Forest: ", accuracy_rf, "\n")
