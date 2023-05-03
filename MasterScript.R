###---
###  SCRIPT PARA TFM
###---

# Paso 1: Importar las bibliotecas necesarias

install.packages(c("caret", "rpart.plot", "oro.nifti"))

library("fs") # Para trabajar con rutas de archivos
library("readr") # Para cargar y trabajar con archivos CSV
library("matrixStats") # Para trabajar con matrices y datos numéricos
library("caret") # Para Aprendizaje Estadístico
library("rpart") # Para entrenar modelos
library("rpart.plot") # Para visualizar el árbol de decisión
library("RNifti"); library("oro.nifti") # Para cambiar el tamaño de las imágenes a un tamaño de voxel común
library("neurobase") # por si a caso
library("dplyr") # para las cañerías

# Paso 2: Cargar y aplanar los datos

# Establecer la ruta a la carpeta que contiene las imágenes NIfTI
data_folder <- "~/GitHub/TFM-Statistical-Learning/PETmasked"

# Cargar los archivos con extensión .hdr en la carpeta
nifti_data_z30 <- list()

# Loop de carga y quedarnos ya con Z=30 solamente
for (filename in list.files(data_folder)) {
  if (endsWith(filename, ".hdr")) {
    nifti_file <- file.path(data_folder, filename)
    nifti_full <- oro.nifti::readNIfTI(nifti_file)
    nifti_data_z30 <- c(nifti_data_z30, list(as.array(nifti_full)[,,30]))
  }
}

# Trasponer a 1x7505
nifti_matrix_flat <- matrix(nrow = length(nifti_data_z30), ncol = 79 * 95)

for (i in 1:length(nifti_data_z30)) {
  nifti_matrix_flat[i,] <- as.vector(t(nifti_data_z30[[i]]))
}

dim(nifti_matrix_flat)

# Leer los datos demográficos desde un archivo CSV
demographic_data <- read.csv("Demographics.csv", sep=";", header=TRUE)

# Mantener solo las variables "Group", "Age" y "Sex"
demographic_data <- demographic_data[, c("Group", "Age", "Sex")]

# Combinar los datos demográficos con los datos de imagen aplanados
combined_data <- cbind(demographic_data, as.matrix(nifti_matrix_flat))

# Reemplazar todos los valores NaN con 0
combined_data[is.na(combined_data)] <- 0

# Paso 3: Dividir los datos en conjuntos de entrenamiento y prueba

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(42)
train_index <- createDataPartition(combined_data[, "Group"], p = 0.8, list = FALSE)
train_data <- combined_data[train_index, ]
test_data <- combined_data[-train_index, ]

#HASTA AQUI FUNCIONA#

# Paso 4: Entrenar el árbol de decisiones

# Convertir las columnas "Group", "Age" y "Sex" 
train_data$Group <- as.factor(train_data$Group)
train_data$Age <- as.numeric(train_data$Age)
train_data$Sex <- as.factor(train_data$Sex)

# Asegurarse de que las columnas restantes sean numéricas
train_data[,-c(1,2,3)] <- apply(train_data[,-c(1,2,3)], 2, as.numeric)

# Crear el modelo del árbol de decisiones
library(rpart)

# Cálculo de Hiper-parámetros

# Ajustar hiperparámetros usando validación cruzada y búsqueda en cuadrícula
set.seed(42)

# Definir los rangos de hiperparámetros para la búsqueda en cuadrícula
tune_grid <- expand.grid(
  .cp = seq(0.001, 0.1, length.out = 10),
  .maxdepth = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)

# Función para calcular la métrica de evaluación en la validación cruzada
compute_metric <- function(model, data) {
  predictions <- predict(model, newdata = data, type = "class")
  accuracy <- mean(predictions == data$Group)
  return(accuracy)
}

# Bucle para buscar en la cuadrícula de hiperparámetros
best_accuracy <- 0
best_model <- NULL

for (cp in tune_grid$.cp) {
  for (maxdepth in tune_grid$.maxdepth) {
    tree_model <- rpart(Group ~ ., data = train_data, method = "class",
                        control = rpart.control(minsplit = 10,
                                                cp = cp,
                                                maxcompete = 4,
                                                maxsurrogate = 5,
                                                usesurrogate = 2,
                                                xval = 10,
                                                surrogatestyle = 0,
                                                maxdepth = maxdepth))
    accuracy <- mean(tree_model$cptable[, "xerror"])
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_model <- tree_model
    }
  }
}

# Mejor modelo
print(best_model)

# Obtener los hiperparámetros óptimos del objeto de resultados
best_params <- tree_model$bestTune

# Entrenar el modelo de árbol de decisión final utilizando los hiperparámetros óptimos
final_tree <- rpart(
  Group ~ .,
  data = train_data,
  method = "class",
  control = rpart.control(
    minsplit = 10,
    cp = best_params$.cp,
    maxcompete = 4,
    maxsurrogate = 5,
    usesurrogate = 2,
    xval = 10,
    surrogatestyle = 0,
    
    maxdepth = best_params$.maxdepth
  )
)

# Entrenar el árbol de decisión utilizando rpart
tree <- rpart(Group ~ ., data = train_data, method = "class",
              control = rpart.control(minsplit = 10,
                                      cp = 0.01,
                                      maxcompete = 4,
                                      maxsurrogate = 5,
                                      usesurrogate = 2,
                                      xval = 10,
                                      surrogatestyle = 0,
                                      maxdepth = 8))

# Imprimir el resultado
print(tree)

# Visualizar el árbol de decisión
library(rpart.plot)
rpart.plot(tree, type = 3, box.palette = "auto", shadow.col = "gray")


# Testear contra 'test_data'
test_data$Group <- as.factor(test_data$Group)
test_data$Age <- as.numeric(test_data$Age)
test_data$Sex <- as.factor(test_data$Sex)
test_data[,-c(1,2,3)] <- apply(test_data[,-c(1,2,3)], 2, as.numeric)
predicted_labels <- predict(tree, test_data, type = "class")

# Calcular la matriz de confusión
confusion_matrix <- confusionMatrix(predicted_labels, test_data$Group)

# Imprimir la matriz de confusión
print(confusion_matrix)

# Imprimir la precisión general
cat("Accuracy: ", confusion_matrix$overall["Accuracy"], "\n")
