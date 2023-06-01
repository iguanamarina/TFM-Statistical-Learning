###---
###  SCRIPT PARA SVM
###---

# Los Support Vector Machines (SVM) son un tipo de algoritmo de aprendizaje supervisado que se utiliza 
# para problemas de clasificación y regresión. La idea central de los SVM es encontrar un hiperplano que 
# separe de manera óptima las clases en el espacio de características. En el caso de la clasificación, 
# el hiperplano se selecciona de tal manera que maximice el margen entre las clases, donde el margen es
# la distancia entre el hiperplano y los puntos de datos más cercanos de cada clase. Estos puntos de 
# datos se denominan "vectores de soporte", de ahí el nombre del método.

# Instalar el paquete si no está instalado
if (!requireNamespace("e1071", quietly = TRUE)) {
    install.packages("e1071")
}

# Cargar el paquete
library(e1071)
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

# Ajustar el modelo SVM utilizando un kernel lineal
svm_model <- svm(Group ~ ., data = train_data, kernel = "linear", cost = 1)

# En la mayoría de los casos reales, los datos no son linealmente separables en el espacio 
# original de características. Para abordar este problema, los SVM utilizan una técnica 
# llamada "kernel trick". El truco del kernel consiste en mapear los datos a un espacio de 
# características de mayor dimensión donde los datos puedan ser linealmente separables. Una 
# función kernel es una función que calcula el producto escalar de dos puntos en este espacio 
# de características de mayor dimensión sin tener que calcular explícitamente las coordenadas 
# de los puntos en ese espacio.

# Puedes probar con otros tipos de kernel, como el kernel radial (RBF):
# svm_model <- svm(Group ~ ., data = train_data, kernel = "radial", gamma = 0.1, cost = 1)

# Los kernel más comunes son el lineal, polinomial, sigmoide y el radial basis function (RBF). 
# El kernel lineal no realiza ninguna transformación, mientras que los otros kernels permiten 
# trabajar con datos no lineales.

# Predecir en el conjunto de prueba
svm_pred <- predict(svm_model, newdata = test_data)

# Calcular la matriz de confusión y la precisión
svm_confusion_matrix <- confusionMatrix(svm_pred, test_data$Group)
svm_accuracy <- svm_confusion_matrix$overall["Accuracy"]

print(svm_confusion_matrix)
print(svm_accuracy)

# Definir la cuadrícula de hiperparámetros
svm_hyper_grid <- expand.grid(
    cost = 2^(-2:2),
    gamma = c(0.1, 0.5, 1, 2, 5)
)

# **Cost**: El parámetro 'cost' (también llamado 'C') controla el equilibrio entre la maximización del 
# margen y la minimización del error de clasificación en el conjunto de entrenamiento. Un valor de 
# 'C' más pequeño produce un margen más amplio pero permite cierta tolerancia al error de clasificación, 
# mientras que un valor de 'C' más grande busca minimizar el error de clasificación, lo que podría 
# resultar en un margen más estrecho y un modelo más sensible al ruido.
# 
# **Gamma**: El hiperparámetro 'gamma' es específico del kernel RBF y controla la "forma" de la función 
# de base radial. Un valor de 'gamma' más pequeño produce una función de base radial más amplia y suave, 
# lo que resulta en un modelo más flexible y menos sensible al ruido. Por otro lado, un valor de 'gamma'
# más grande genera una función de base radial más estrecha, lo que puede llevar a un modelo más complejo 
# pero con mayor riesgo de sobreajuste.


# Realizar la búsqueda en cuadrícula
set.seed(123)
svm_tuned <- tune(
    svm, Group ~ ., data = train_data, kernel = "radial",
    ranges = svm_hyper_grid,
    tunecontrol = tune.control(sampling = "cross", cross = 5)
)

# Ajustar el modelo SVM con los mejores hiperparámetros
svm_model_tuned <- svm(Group ~ ., data = train_data, kernel = "radial", cost = svm_tuned$best.parameters$cost, gamma = svm_tuned$best.parameters$gamma)

# Predecir en el conjunto de prueba
svm_tuned_pred <- predict(svm_model_tuned, newdata = test_data)

# Calcular la matriz de confusión y la precisión
svm_tuned_confusion_matrix <- confusionMatrix(svm_tuned_pred, test_data$Group)

saveRDS(svm_tuned_confusion_matrix, file = "svm_tuned_confusion_matrix.RDS")
svm_tuned_accuracy <- svm_tuned_confusion_matrix$overall["Accuracy"]

print(svm_tuned_confusion_matrix)
print(svm_tuned_accuracy)

## VISUALIZACIÓN:

# Cargar paquete para PCA
library(FactoMineR)

# Realizar PCA en los datos de entrenamiento
pca_train <- PCA(train_data[,-c(1, 2, 3, 4)], scale.unit = TRUE, ncp = 2)

# Agregar las etiquetas de los valores reales y las predicciones al conjunto de datos transformado
train_data_2d <- cbind.data.frame(pca_train$ind$coord, Actual = train_data$Group)
svm_pred_train <- predict(svm_model_tuned, newdata = train_data)
train_data_2d$Predicted <- svm_pred_train

# Cargar paquete
library(ggplot2)

# Crear gráfico de dispersión de los datos transformados con PCA y colorear por la etiqueta real y predicha
plot_svm <- ggplot(train_data_2d, aes(x = Dim.1, y = Dim.2, color = Actual, shape = Predicted)) +
    geom_point(size = 3) +
    theme_minimal() +
    labs(title = "SVM (Kernel lineal) en Datos Reducidos a 2D con PCA",
         x = "Componente Principal 1",
         y = "Componente Principal 2",
         color = "Etiqueta Real",
         shape = "Etiqueta Predicha")

print(plot_svm)


