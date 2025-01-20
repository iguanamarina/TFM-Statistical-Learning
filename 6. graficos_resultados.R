library(ggplot2)
library(stringr)

# Crear un dataframe con los datos

data <- data.frame(
  Modelo = factor(c("AD", "BA", "SVM", "AP", "AP & FDA"), 
                  levels = c("AD", "BA", "SVM", "AP", "AP & FDA")),
  Sensibilidad = c(0.2, 0.6, 0, 0.6667, 0.8),
  Especificidad = c(0.8571, 0.7142, 1, 0.8333, 0.86),
  Precisión = c(0.5833, 0.6666, 0.5833, 0.75, 0.8),
  P_Balanceada = c(0.5285, 0.6571, 0.5, 0.75, 0.83),
  Kappa_Coef = c(0.0625, 0.3142, 0, 0.5, 0.6571)
)

# Convertir los datos a formato largo
data_long <- reshape2::melt(data, id.vars = "Modelo")

# Convertir la variable 'variable' a un factor y especificar el orden de los niveles
data_long$variable <- factor(data_long$variable, levels = c("Sensibilidad", "Especificidad", "Precisión", "P_Balanceada", "Kappa_Coef"))


# Crear el gráfico
ggplot(data_long, aes(x = Modelo, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "YlGnBu", 
                    labels = c("Sensibilidad", "Especificidad", "Precisión", "Precisión Balanceada", "Coeficiente Kappa")) +
  theme_minimal() +
  labs(x = "", y = "", fill = "") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "top") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(0, 1)    



library(gridExtra)

# Crear los gráficos por separado
g1 <- ggplot(subset(data_long, variable == "Sensibilidad"), aes(x = Modelo, y = value, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(title = "Sensibilidad", x = "", y = "") +
  ylim(0, 1)

g2 <- ggplot(subset(data_long, variable == "Especificidad"), aes(x = Modelo, y = value, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Greens") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(title = "Especificidad", x = "", y = "") +
  ylim(0, 1)

g3 <- ggplot(subset(data_long, variable == "Precisión"), aes(x = Modelo, y = value, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Oranges") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(title = "Precisión", x = "", y = "") +
  ylim(0, 1)

g4 <- ggplot(subset(data_long, variable == "P_Balanceada"), aes(x = Modelo, y = value, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Purples") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(title = "Prec. Balanceada", x = "", y = "") +
  ylim(0, 1)

g5 <- ggplot(subset(data_long, variable == "Kappa_Coef"), aes(x = Modelo, y = value, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Reds") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(title = "Kappa", x = "", y = "") +
  ylim(0, 1)

# Combinar los gráficos en una sola fila
grid.arrange(g1, g2, g3, g4, g5, nrow = 1)
