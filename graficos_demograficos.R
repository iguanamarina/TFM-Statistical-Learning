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
  theme(legend.position = "none", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(size = 14)) +
  coord_cartesian(ylim = c(min(data_combined$Edad) - 10, max(data_combined$Edad) + 10))

print(edad_boxplot)

# Gráfico de barras para visualizar cuántos pacientes son hombre vs mujeres
genero_barplot <- ggplot(data, aes(x = Sexo, fill = Sexo)) +
  geom_bar() +
  scale_fill_manual(values = c("Mujer" = "#99C1E1", "Hombre" = "#D1A5B7")) +
  scale_y_continuous(limits = c(0, 90)) +
  labs(x = "", y = "Número de pacientes") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(size = 14), plot.margin = margin(0, 0, 0, 25))

print(genero_barplot)



# Gráfico de barras para visualizar cuantos pacientes son Alzheimer y cuantos son Control
diagnostico_barplot <- ggplot(data, aes(x = `Grupo de Diagnóstico`, fill = `Grupo de Diagnóstico`)) +
  geom_bar() +
  scale_fill_manual(values = c("Alzheimer" = "#A4C4A8", "Control" = "#DFB78E")) +
  labs(x = "", y = "Número de pacientes") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(size = 14))

print(diagnostico_barplot)


# Gráfico de barras para visualizar cuantos pacientes son Alzheimer y cuantos son Control, desglosado por sexo
diagnostico_sexo_barplot <- ggplot(data, aes(x = `Grupo de Diagnóstico`, fill = Sexo)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Mujer" = "#99C1E1", "Hombre" = "#D1A5B7")) +
  labs(x = "", y = "Número de pacientes") +
  theme_minimal() +
  theme(legend.position = c(0.98, 0.98), legend.justification = c(1, 1), legend.background = element_blank(), legend.title = element_blank(), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(size = 14))

print(diagnostico_sexo_barplot)
