# Instalar y cargar el paquete partykit si aún no lo has hecho
# install.packages("partykit")
library(partykit)

# Convertir el objeto rpart en un objeto party
tree_party <- as.party(tree)

# Visualizar el árbol de decisión
plot(tree_party)



# Instalar y cargar los paquetes necesarios si aún no lo has hecho
# install.packages("visNetwork")
# install.packages("rpart.plot")
library(visNetwork)
library(rpart.plot)

# Crear un gráfico de red a partir del árbol de decisión
vis_tree <- visTree(tree)

# Visualizar el gráfico de red
visNetwork(vis_tree$nodes, vis_tree$edges)



# Instalar y cargar el paquete plotly si aún no lo has hecho
# install.packages("plotly")
library(plotly)

# Crear un dendrograma a partir del árbol de decisión
dend <- as.dendrogram(tree)

# Visualizar el dendrograma en 3D
plot_ly(x = dend, type = "scatter3d", mode = "lines")
