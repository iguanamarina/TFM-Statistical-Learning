# Cargar librerías
library(tidyverse)

# Cargar y procesar el CSV

data <- read.csv(file = "Demographics.csv", header = TRUE, sep= ";")
colnames(data) <- c("Participante", "Grupo de Diagnóstico", "Sexo", "Edad")
data <- data %>% mutate(Sexo = recode(Sexo, F = "Mujer", M = "Hombre"))
data <- data %>% mutate(`Grupo de Diagnóstico` = recode(`Grupo de Diagnóstico`, AD = "Alzheimer", CN = "Control"))


# Calcular estadísticas descriptivas
media_edad <- mean(data$Edad)
media_edad_H <- mean(data$Edad[data$Sexo == "Hombre"])
media_edad_M <- mean(data$Edad[data$Sexo == "Mujer"])
max_edad <- max(data$Edad)
max_edad_H <- max(data$Edad[data$Sexo == "Hombre"])
max_edad_M <- max(data$Edad[data$Sexo == "Mujer"])
min_edad_H <- min(data$Edad[data$Sexo == "Hombre"])
min_edad_M <- min(data$Edad[data$Sexo == "Mujer"])
min_edad <- min(data$Edad)
sd_edad <- sd(data$Edad)
n_mujeres <- sum(data$Sexo == "Mujer")
n_hombres <- sum(data$Sexo == "Hombre")
proporcion_sexo <- n_mujeres / n_hombres
n_alzheimer <- sum(data$`Grupo de Diagnóstico` == "Alzheimer")
n_control <- sum(data$`Grupo de Diagnóstico` == "Control")
proporcion_diagnostico <- n_alzheimer / n_control

# Gráfico de cajas para la edad

data_total <- data.frame(Edad = data$Edad, Sexo = "Total")
data_sex <- data[, c("Edad", "Sexo")]
data_sex$Sexo <- factor(data_sex$Sexo, levels = c("Total", "Mujer", "Hombre"))
data_combined <- rbind(data_total, data_sex)

edad_boxplot <- ggplot(data_combined, aes(x = Sexo, y = Edad, fill = Sexo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Total" = "#A9A9A9", "Mujer" = "#99C1E1", "Hombre" = "#D1A5B7")) +
  labs(x = "", y = "Edad") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 22), axis.title.y = element_text(size = 22)) +
  coord_cartesian(ylim = c(min(data_combined$Edad) - 10, max(data_combined$Edad) + 10))

print(edad_boxplot)

# Gráfico de barras para visualizar cuántos pacientes son hombre vs mujeres
genero_barplot <- ggplot(data, aes(x = Sexo, fill = Sexo)) +
  geom_bar() +
  scale_fill_manual(values = c("Mujer" = "#99C1E1", "Hombre" = "#D1A5B7")) +
  scale_y_continuous(limits = c(0, 90)) +
  labs(x = "", y = "Número de pacientes") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 22), axis.title.y = element_text(size = 22))

print(genero_barplot)



# Gráfico de barras para visualizar cuantos pacientes son Alzheimer y cuantos son Control
diagnostico_barplot <- ggplot(data, aes(x = `Grupo de Diagnóstico`, fill = `Grupo de Diagnóstico`)) +
  geom_bar() +
  scale_fill_manual(values = c("Alzheimer" = "#A4C4A8", "Control" = "#DFB78E")) +
  labs(x = "", y = "Número de pacientes") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 22), axis.title.y = element_text(size = 22))

print(diagnostico_barplot)


# Gráfico de barras para visualizar cuantos pacientes son Alzheimer y cuantos son Control, desglosado por sexo
diagnostico_sexo_barplot <- ggplot(data, aes(x = `Grupo de Diagnóstico`, fill = Sexo)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Mujer" = "#99C1E1", "Hombre" = "#D1A5B7")) +
  labs(x = "", y = "Número de pacientes") +
  theme_minimal() +
  theme(legend.position = c(0.98, 0.98), legend.justification = c(1, 1), legend.background = element_blank(), legend.title = element_blank(), axis.text.y = element_text(size = 22), axis.text.x = element_text(size = 22), axis.title.y = element_text(size = 22))

print(diagnostico_sexo_barplot)



library(ggplot2)
library(gridExtra)
library(dplyr)

# Filtramos los datos por sexo
data_H <- data %>% filter(Sexo == "Hombre")
data_M <- data %>% filter(Sexo == "Mujer")

# Gráficos de histogramas para la edad
p1 <- ggplot(data_H, aes(x = Edad)) +
    geom_histogram(fill="#D1A5B7", color="#e9c46a", alpha=0.5, bins=30) + 
    labs(x = "Edad", y = "Frecuencia") +
    theme_minimal() +
    theme(legend.position = "none", 
          axis.text.y = element_text(size = 12), 
          axis.text.x = element_text(size = 22), 
          axis.title.y = element_text(size = 22)) +
    coord_cartesian(xlim = c(min(data$Edad), max(data$Edad)))

p2 <- ggplot(data_M, aes(x = Edad)) +
    geom_histogram(fill="#99C1E1", color="#e9c46a", alpha=0.5, bins=30) + 
    labs(x = "Edad", y = "Frecuencia") +
    theme_minimal() +
    theme(legend.position = "none", 
          axis.text.y = element_text(size = 12), 
          axis.text.x = element_text(size = 22), 
          axis.title.y = element_text(size = 22)) +
    coord_cartesian(xlim = c(min(data$Edad), max(data$Edad)))

# Ahora los mostramos juntos
gridExtra::grid.arrange(p1, p2, ncol=2)


# Filtrar los datos por Sexo
data_hombre <- data[data$Sexo == "Hombre", ]
data_mujer <- data[data$Sexo == "Mujer", ]





scale_x_continuous(breaks = seq(min(data$Edad), max(data$Edad), by = 5))


# Crear el histograma
hist_plot <- ggplot() +
    geom_histogram(data = data_hombre, aes(x = Edad, fill = Sexo), bins = 30, alpha = 0.7) +
    geom_histogram(data = data_mujer, aes(x = Edad, fill = Sexo), bins = 30, alpha = 0.7) +
    scale_fill_manual(values = c("Hombre" = "#D1A5B7", "Mujer" = "#99C1E1")) +
    labs(x = "Edad", y = "Número de Pacientes", fill = "Sexo") +
    theme_minimal() +
    theme(legend.position = "top", legend.title = element_text(size = 16), legend.text = element_text(size = 14),
          axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 16)) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.vjust = 0.5)) +
    scale_x_continuous(breaks = seq(min(data$Edad), max(data$Edad), by = 1), labels = seq(min(data$Edad), max(data$Edad), by = 1)) +
    coord_cartesian(xlim = c(min(data$Edad), max(data$Edad))) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    theme(legend.justification = c(0, 1), legend.position = c(0, 1),
          legend.box.just = "left", legend.margin = margin(10, 10, 10, 10))

# Imprimir el histograma
print(hist_plot)



# Crear el histograma
hist_plot <- ggplot() +
    geom_bar(data = data_hombre, aes(x = Edad, fill = Sexo), alpha = 0.7, position = "dodge") +
    geom_bar(data = data_mujer, aes(x = Edad, fill = Sexo), alpha = 0.7, position = "dodge") +
    scale_fill_manual(values = c("Hombre" = "#D1A5B7", "Mujer" = "#99C1E1")) +
    labs(x = "Edad", y = "Número de Pacientes", fill = "Sexo") +
    theme_minimal() +
    theme(legend.position = "top-left", 
          legend.title = element_text(size = 16), 
          legend.text = element_text(size = 14),
          axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16),
          panel.grid.major = element_line(color = "gray", linetype = "dashed"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.vjust = 0.5)) +
    scale_x_continuous(breaks = seq(min(data$Edad), max(data$Edad), by = 1), labels = seq(min(data$Edad), max(data$Edad), by = 1)) +
    scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, by = 1)) +
    coord_cartesian(xlim = c(min(data$Edad) - 0.5, max(data$Edad) + 0.5)) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
          legend.justification = c(0, 1), legend.position = c(0.1, 0.9),
          legend.box.just = "left", legend.margin = margin(10, 10, 10, 10))

# Aumentar el tamaño de las etiquetas y títulos
hist_plot <- hist_plot + 
    theme(axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18))

# Imprimir el histograma
print(hist_plot)




###### Para edad en función de diagnóstico:

# Estadísticas para el grupo con diagnóstico de Alzheimer
media_edad_AD <- mean(data$Edad[data$`Grupo de Diagnóstico` == "Alzheimer"])
sd_edad_AD <- sd(data$Edad[data$`Grupo de Diagnóstico` == "Alzheimer"])
max_edad_AD <- max(data$Edad[data$`Grupo de Diagnóstico` == "Alzheimer"])
min_edad_AD <- min(data$Edad[data$`Grupo de Diagnóstico` == "Alzheimer"])

# Estadísticas para el grupo Control
media_edad_CN <- mean(data$Edad[data$`Grupo de Diagnóstico` == "Control"])
sd_edad_CN <- sd(data$Edad[data$`Grupo de Diagnóstico` == "Control"])
max_edad_CN <- max(data$Edad[data$`Grupo de Diagnóstico` == "Control"])
min_edad_CN <- min(data$Edad[data$`Grupo de Diagnóstico` == "Control"])

# Crear el histograma
hist_plot <- ggplot() +
    geom_bar(data = data[data$`Grupo de Diagnóstico` == "Alzheimer",], aes(x = Edad, fill = `Grupo de Diagnóstico`), alpha = 0.7, position = "dodge") +
    geom_bar(data = data[data$`Grupo de Diagnóstico` == "Control",], aes(x = Edad, fill = `Grupo de Diagnóstico`), alpha = 0.7, position = "dodge") +
    scale_fill_manual(values = c("Alzheimer" = "#D1A5B7", "Control" = "#99C1E1")) +
    labs(x = "Edad", y = "Número de Pacientes", fill = "Grupo de Diagnóstico") +
    theme_minimal() +
    theme(legend.position = "top-left", 
          legend.title = element_text(size = 16), 
          legend.text = element_text(size = 14),
          axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16),
          panel.grid.major = element_line(color = "gray", linetype = "dashed"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, title.vjust = 0.5)) +
    scale_x_continuous(breaks = seq(min(data$Edad), max(data$Edad), by = 1), labels = seq(min(data$Edad), max(data$Edad), by = 1)) +
    scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, by = 1)) +
    coord_cartesian(xlim = c(min(data$Edad) - 0.5, max(data$Edad) + 0.5)) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
          legend.justification = c(0, 1), legend.position = c(0.1, 0.9),
          legend.box.just = "left", legend.margin = margin(10, 10, 10, 10))

# Aumentar el tamaño de las etiquetas y títulos
hist_plot <- hist_plot + 
    theme(axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18))

# Imprimir el histograma
print(hist_plot)
