###---
###  SCRIPT PARA DEEP LEARNING
###---

# El aprendizaje profundo es una rama del machine learning basada en redes neuronales artificiales con varias capas. 
# Estas capas se conocen como capas ocultas, y cada una de ellas suele contener un número de neuronas o nodos. La 
# idea es que cada capa aprende diferentes características de los datos. Cuantas más capas tenga una red, más 
# "profunda" se considera, de ahí el término "aprendizaje profundo".

# Dependencias Python 
library(reticulate)
reticulate::use_condaenv("base")
reticulate::py_install(c("tensorflow", "numpy", "keras"), envname = "base")

# Instalar el paquete keras si no está instalado
if (!requireNamespace("keras", quietly = TRUE)) {
    install.packages("keras")
}

# Cargar el paquete
library(keras)
library(caret)
library(tensorflow)

# Establecer el directorio de trabajo
setwd("~/GitHub/TFM-Statistical-Learning")
# setwd("C:/Users/Juan A. Arias/Desktop/TFM")

# Cargar los datos preprocesados
combined_data <- read.csv("combined_data.csv")

# Las Deep Learning solo trabajan con números asique se codifican asi:

# Asignar 0 a "CN" y 1 a "AD"
combined_data$Group <- ifelse(combined_data$Group == "AD", 1, 0)

# Asignar 1 a "F" y 2 a "M" 
combined_data$Sex <- ifelse(combined_data$Sex == "M", 2, 1)

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_index <- createDataPartition(y = combined_data$Group, p = 0.9, list = FALSE)
train_data <- combined_data[train_index, ]
test_data <- combined_data[-train_index, ]

# Codificar las etiquetas de clase como vectores binarios
train_data$Group <- as.factor(train_data$Group)
train_data$Group <- as.integer(train_data$Group) - 1
train_labels <- to_categorical(train_data$Group)

test_data$Group <- as.factor(test_data$Group)
test_data$Group <- as.integer(test_data$Group) - 1
test_labels <- to_categorical(test_data$Group)

# Algunos de los hiperparámetros más importantes en el aprendizaje profundo incluyen:
#     
# - Número de capas ocultas
# - Número de nodos en cada capa
# - La función de activación utilizada en los nodos
# - La tasa de aprendizaje
# - El número de épocas de entrenamiento

# Número de características
num_features <- ncol(train_data[, -which(names(train_data) %in% c("Group"))])

# Definir la arquitectura del modelo
model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu", input_shape = c(num_features)) %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dense(units = length(unique(train_data$Group)), activation = "softmax")

# Compilar el modelo
model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
)

# Ajustar el modelo
history <- model %>% fit(
    x = as.matrix(train_data[, -which(names(train_data) %in% c("Group"))]),
    y = train_labels,
    epochs = 20,
    batch_size = 32,
    validation_split = 0.1
)

# Este código compila el modelo con la función de pérdida de entropía cruzada categórica (una elección común 
# para problemas de clasificación multiclase), el optimizador RMSprop (una variante de descenso de gradiente
# estocástico que a menudo funciona bien en la práctica), y solicita que se calcule la precisión durante el entrenamiento.
# 
# Luego, ajusta el modelo a los datos de entrenamiento. Aquí, epochs = 20 y batch_size = 32 determina que el modelo 
# pasará por los datos de entrenamiento 20 veces en lotes de 32 ejemplos a la vez. validation_split = 0.2 reserva el
# 20% de los datos para validar el rendimiento del modelo durante el entrenamiento.

# Evaluar el modelo de nuevo
score <- model %>% evaluate(
    x = as.matrix(test_data[, -which(names(test_data) == "Group")]),
    y = test_labels
)

# Imprimir la pérdida y la precisión
cat('Test loss:', score[[1]], '\n')
cat('Test accuracy:', score[[2]], '\n')

## Hyper-grid:

# *units*: Este es el número de nodos o neuronas en una capa de la red neuronal. Este valor puede afectar la capacidad 
# de la red para aprender patrones complejos. Un número mayor de unidades puede permitir a la red aprender patrones más complejos, 
# pero también puede llevar a un sobreajuste.
# 
# *activation*: Esta es la función de activación utilizada en los nodos de la red. La función de activación determina la 
# salida de un nodo en función de sus entradas. "relu" y "tanh" son dos funciones de activación comunes. La elección de 
# la función de activación puede afectar la velocidad y la calidad del aprendizaje de la red.
# 
# *lr (learning rate)*: Esta es la tasa de aprendizaje para el optimizador de la red. Esta tasa determina cuánto cambian 
# los pesos de la red en cada paso del entrenamiento. Una tasa de aprendizaje más alta puede permitir que la red aprenda más rápidamente, 
# pero también puede hacer que la red se pase de largo y no logre encontrar la solución óptima. Por otro lado, una tasa de aprendizaje 
# más baja puede hacer que la red aprenda más lentamente, pero también puede permitir que encuentre una solución más precisa.
# 
# *epochs*: Este es el número de veces que la red pasa por todo el conjunto de datos de entrenamiento. Un número mayor de épocas 
# puede permitir a la red aprender más sobre los datos, pero también puede llevar a un sobreajuste si la red se entrena durante demasiado tiempo.


# Lista de hiperparámetros para probar
hyper_grid <- expand.grid(
    units = c(64, 128, 256),
    activation = c("relu", "tanh", "sigmoid"),
    lr = c(0.001, 0.01, 0.1),
    epochs = c(10, 20, 30)
)
    
# Inicializar un data frame para almacenar los resultados
results <- data.frame()

# Bucle sobre todas las combinaciones de hiperparámetros
for (i in 1:nrow(hyper_grid)) {
    
    # Extraer los hiperparámetros para esta iteración
    units <- hyper_grid[i, "units"]
    activation <- hyper_grid[i, "activation"]
    lr <- hyper_grid[i, "lr"]
    epochs <- hyper_grid[i, "epochs"]
    
    # Definir el modelo
    model <- keras_model_sequential() %>%
        layer_dense(units = units, activation = activation, input_shape = c(ncol(train_data) - 1)) %>%
        layer_dense(units = length(unique(train_data$Group)), activation = "softmax")
    
    # Compilar el modelo
    model %>% compile(
        optimizer = optimizer_rmsprop(learning_rate = lr),
        loss = "categorical_crossentropy",
        metrics = c("accuracy")
    )

    
    # Entrenar el modelo
    history <- model %>% fit(
        x = as.matrix(train_data[, -which(names(train_data) == "Group")]),
        y = train_labels,
        epochs = epochs,
        batch_size = 128,
        validation_split = 0.1 
    )
    
    # Evaluar el modelo en el conjunto de prueba
    score <- model %>% evaluate(
        x = as.matrix(test_data[, -which(names(test_data) == "Group")]),
        y = test_labels
    )
    
    # Añadir los resultados a la tabla de resultados
    results <- rbind(results, cbind(hyper_grid[i, ], Loss = score[[1]], Accuracy = score[[2]]))
}

# Ver los resultados
print(results) # 0'8
saveRDS(results, "deeplearning_results.RDS")


####
# INGENIERÍA DE CARACTERÍSTICAS
####

list_points <- readRDS("~/GitHub/TFM-Statistical-Learning/list_points.RDS")

# 1. Carga las coordenadas con pesos segun SCC_COMP_1

# Dimensiones originales de la imagen
dim1 <- 79
dim2 <- 95

# Inicializar un vector de ceros con longitud 7505 para representar todos los píxeles
pixel_weights <- rep(0, dim1 * dim2)

# Función para convertir las coordenadas de 2D a 1D
convert_2D_to_1D <- function(row, col, dim1) {
  return((row - 1) * dim1 + col)
}

# Actualizar los valores en pixel_weights basados en 'points.N' y 'points.P'
for(i in 1:nrow(list_points$points.N)) {
  idx <- convert_2D_to_1D(list_points$points.N[i, "row"], list_points$points.N[i, "col"], dim1)
  pixel_weights[idx] <- 1
}

for(i in 1:nrow(list_points$points.P)) {
  idx <- convert_2D_to_1D(list_points$points.P[i, "row"], list_points$points.P[i, "col"], dim1)
  pixel_weights[idx] <- -1
}

# Asignar nombres a pixel_weights
names(pixel_weights) <- paste0("V", 1:(dim1 * dim2))


# 2. Crea un dataframe basándote en combined_data para generar nuevas variables

library(e1071)

# Crear nuevas características basadas en puntos relevantes
combined_data$Avg_Important_Pixels <- rowMeans(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * pixel_weights)
combined_data$Median_Important_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * pixel_weights, 1, median, na.rm = TRUE)
combined_data$Max_Important_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * pixel_weights, 1, max, na.rm = TRUE)
combined_data$Min_Important_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * pixel_weights, 1, min, na.rm = TRUE)
combined_data$StdDev_Important_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * pixel_weights, 1, sd, na.rm = TRUE)
combined_data$Var_Important_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * pixel_weights, 1, var, na.rm = TRUE)

# Crear nuevas características basadas en points.N
combined_data$Avg_N_Pixels <- rowMeans(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == 1))
combined_data$Median_N_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == 1), 1, median, na.rm = TRUE)
combined_data$Max_N_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == 1), 1, max, na.rm = TRUE)
combined_data$StdDev_N_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == 1), 1, sd, na.rm = TRUE)
combined_data$Var_N_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == 1), 1, var, na.rm = TRUE)
combined_data$Skewness_N_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == 1), 1, skewness, na.rm = TRUE)
combined_data$Kurtosis_N_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == 1), 1, kurtosis, na.rm = TRUE)

# Crear nuevas características basadas en points.P
combined_data$Avg_P_Pixels <- rowMeans(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == -1))
combined_data$Median_P_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == -1), 1, median, na.rm = TRUE)
combined_data$Max_P_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == -1), 1, max, na.rm = TRUE)
combined_data$StdDev_P_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == -1), 1, sd, na.rm = TRUE)
combined_data$Var_P_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == -1), 1, var, na.rm = TRUE)
combined_data$Skewness_P_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == -1), 1, skewness, na.rm = TRUE)
combined_data$Kurtosis_P_Pixels <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))] * (pixel_weights == -1), 1, kurtosis, na.rm = TRUE)

# Función para calcular la diferencia con los vecinos
neighbor_diff_basic <- function(image, dim1, dim2) {
    image_matrix <- matrix(image, nrow = dim1, ncol = dim2)
    diff_sum <- 0
    for (row in 1:dim1) {
        for (col in 1:dim2) {
            if (pixel_weights[(row - 1) * dim1 + col] == 1) {
                neighbors <- image_matrix[max(1, row - 1):min(dim1, row + 1),
                                          max(1, col - 1):min(dim2, col + 1)]
                diff_sum <- diff_sum + sum(abs(neighbors - image_matrix[row, col]))
            }
        }
    }
    return(diff_sum)
}

# Función para calcular la diferencia con los vecinos separada por sub-grupo    
neighbor_diff <- function(image, dim1, dim2, pixel_weights) {
    image_matrix <- matrix(image, nrow = dim1, ncol = dim2)
    diff_sum <- 0
    for (row in 1:dim1) {
        for (col in 1:dim2) {
            if (pixel_weights[(row - 1) * dim1 + col] == 1) {
                neighbors <- image_matrix[max(1, row - 1):min(dim1, row + 1),
                                          max(1, col - 1):min(dim2, col + 1)]
                diff_sum <- diff_sum + sum(abs(neighbors - image_matrix[row, col]))
            }
        }
    }
    return(diff_sum)
}

# Inicializar un vector de ceros con longitud 7505 para representar todos los píxeles
pixel_weights_N <- rep(0, dim1 * dim2)
pixel_weights_P <- rep(0, dim1 * dim2)

# Actualizar los valores en pixel_weights_N y pixel_weights_P
for(i in 1:nrow(list_points$points.N)) {
    idx <- convert_2D_to_1D(list_points$points.N[i, "row"], list_points$points.N[i, "col"], dim1)
    pixel_weights_N[idx] <- 1
}

for(i in 1:nrow(list_points$points.P)) {
    idx <- convert_2D_to_1D(list_points$points.P[i, "row"], list_points$points.P[i, "col"], dim1)
    pixel_weights_P[idx] <- -1
}

# Calcular la diferencia con los vecinos para cada imagen

combined_data$Neighbor_Diff <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))], 
                                     1, neighbor_diff_basic, dim1 = dim1, dim2 = dim2)

combined_data$Neighbor_Diff_N <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))], 
                                       1, neighbor_diff, dim1 = dim1, dim2 = dim2, pixel_weights = pixel_weights_N)

combined_data$Neighbor_Diff_P <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))], 
                                       1, neighbor_diff, dim1 = dim1, dim2 = dim2, pixel_weights = pixel_weights_P)


# 3. Divide los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_index <- createDataPartition(y = combined_data$Group, p = 0.9, list = FALSE)
train_data <- combined_data[train_index, ]
test_data <- combined_data[-train_index, ]

# 4. Codifica las etiquetas de clase como vectores binarios
train_data$Group <- as.factor(train_data$Group)
train_data$Group <- as.integer(train_data$Group) - 1
train_labels <- to_categorical(train_data$Group)

test_data$Group <- as.factor(test_data$Group)
test_data$Group <- as.integer(test_data$Group) - 1
test_labels <- to_categorical(test_data$Group)

# 5. Define el número de características
num_features <- ncol(train_data[, -which(names(train_data) %in% c("Group"))])

# 6. Bucle sobre todas las combinaciones de hiperparámetros
hyper_grid <- expand.grid(
    units = c(64, 128, 256),
    activation = c("relu", "tanh", "sigmoid"),
    lr = c(0.001, 0.01, 0.1),
    epochs = c(10, 20, 30)
)

# 7. Inicializar un data frame para almacenar los resultados
ponderated_results <- data.frame()

# 8. Bucle sobre todas las combinaciones de hiperparámetros
for (i in 1:nrow(hyper_grid)) {
    
    # Extraer los hiperparámetros para esta iteración
    units <- hyper_grid[i, "units"]
    activation <- hyper_grid[i, "activation"]
    lr <- hyper_grid[i, "lr"]
    epochs <- hyper_grid[i, "epochs"]
    
    # Definir el modelo
    model <- keras_model_sequential() %>%
        layer_dense(units = units, activation = activation, input_shape = c(num_features)) %>%
        layer_dense(units = length(unique(train_data$Group)), activation = "softmax")
    
    # Compilar el modelo
    model %>% compile(
        optimizer = optimizer_rmsprop(learning_rate = lr),
        loss = "categorical_crossentropy",
        metrics = c("accuracy")
    )
    
    # Entrenar el modelo SIN ponderación de muestras
    history <- model %>% fit(
        x = as.matrix(train_data[, -which(names(train_data) == "Group")]),
        y = train_labels,
        epochs = epochs,
        batch_size = 128,
        validation_split = 0.1 
    )
    
    # Evaluar el modelo en el conjunto de prueba
    score <- model %>% evaluate(
        x = as.matrix(test_data[, -which(names(test_data) == "Group")]),
        y = test_labels
    )
    
    # Añadir los resultados a la tabla de resultados
    ponderated_results <- rbind(ponderated_results, cbind(hyper_grid[i, ], Loss = score[[1]], Accuracy = score[[2]]))
}


# Ver los resultados
print(ponderated_results) # 0.91
saveRDS(ponderated_results, "deeplearning_ponderado_results.RDS")






## PONDERACIÓN DE MUESTRAS EN LA RED NEURONAL

# Inicializar un data frame para almacenar los resultados
results_weighted <- data.frame()

# Calcular la varianza de cada píxel
pixel_variance <- apply(combined_data[, -which(names(combined_data) %in% c("Group", "Age", "Sex"))], 2, var)

# Normalizar los valores para que estén en el rango [0,1]
pixel_weights <- pixel_variance / max(pixel_variance)

# Usar estos pesos al entrenar el modelo
sample_weights <- pixel_weights

# Bucle sobre todas las combinaciones de hiperparámetros
for (i in 1:nrow(hyper_grid)) {
    
    # Extraer los hiperparámetros para esta iteración
    units <- hyper_grid[i, "units"]
    activation <- hyper_grid[i, "activation"]
    lr <- hyper_grid[i, "lr"]
    epochs <- hyper_grid[i, "epochs"]
    
    # Definir el modelo
    model <- keras_model_sequential() %>%
        layer_dense(units = units, activation = activation, input_shape = c(ncol(train_data) - 1)) %>%
        layer_dense(units = length(unique(train_data$Group)), activation = "softmax")
    
    # Compilar el modelo
    model %>% compile(
        optimizer = optimizer_rmsprop(lr = lr),
        loss = "categorical_crossentropy",
        metrics = c("accuracy")
    )
    
    # Entrenar el modelo con ponderación de muestras
    history <- model %>% fit(
        x = as.matrix(train_data[, -which(names(train_data) == "Group")]),
        y = train_labels,
        sample_weight = sample_weights,
        epochs = epochs,
        batch_size = 128,
        validation_split = 0.2
    )
    
    # Evaluar el modelo en el conjunto de prueba
    score <- model %>% evaluate(
        x = as.matrix(test_data[, -which(names(test_data) == "Group")]),
        y = test_labels
    )
    
    # Añadir los resultados a la tabla de resultados
    results_weighted <- rbind(results_weighted, cbind(hyper_grid[i, ], Loss = score[[1]], Accuracy = score[[2]]))
}

# Ver los resultados
print(results_weighted)
saveRDS(results_weighted, "deeplearning_weighted_results.RDS")



## COMBINACIÓN DE INGENERÍA DE CARACTERÍSTICAS Y PONDERACIÓN DE MUESTRAS


# Inicializar un data frame para almacenar los resultados
results_combined <- data.frame()

# Bucle sobre todas las combinaciones de hiperparámetros
for (i in 1:nrow(hyper_grid)) {
    
    # Extraer los hiperparámetros para esta iteración
    units <- hyper_grid[i, "units"]
    activation <- hyper_grid[i, "activation"]
    lr <- hyper_grid[i, "lr"]
    epochs <- hyper_grid[i, "epochs"]
    
    # Definir el modelo
    model <- keras_model_sequential() %>%
        layer_dense(units = units, activation = activation, input_shape = c(ncol(train_data) - 1)) %>%
        layer_dense(units = length(unique(train_data$Group)), activation = "softmax")
    
    # Compilar el modelo
    model %>% compile(
        optimizer = optimizer_rmsprop(lr = lr),
        loss = "categorical_crossentropy",
        metrics = c("accuracy")
    )
    
    # Entrenar el modelo con las nuevas características y ponderación de muestras
    history <- model %>% fit(
        x = as.matrix(train_data[, -which(names(train_data) == "Group")]),
        y = train_labels,
        sample_weight = sample_weights,
        epochs = epochs,
        batch_size = 128,
        validation_split = 0.2
    )
    
    # Evaluar el modelo en el conjunto de prueba
    score <- model %>% evaluate(
        x = as.matrix(test_data[, -which(names(test_data) == "Group")]),
        y = test_labels
    )
    
    # Añadir los resultados a la tabla de resultados
    results_combined <- rbind(results_combined, cbind(hyper_grid[i, ], Loss = score[[1]], Accuracy = score[[2]]))
}

# Ver los resultados
print(results_combined)
saveRDS(results_combined, "deeplearning_combined_results.RDS")

        