
data<-read.csv(file.choose(),sep=",",header=TRUE)


library("igraph")                           

#Haze que del csv que hemos cargado se cree un grafo
g <- graph.data.frame(data, directed = FALSE) 
#Mira la clase de "g" q es igraph
class(g)  
#Nombra los vectores de "g"
V(g)$name
#Dice el peso de los vectores de "g"
E(g)$weight
#Te lo representa en una nueva ventana interactiva
tkplot(g)                           
#Representar el grafo en el entorno de R, mostrando el peso de las aristas como etiquetas
plot(g, edge.label = paste(E(g)$weight, sep = "")) 
# Camino más corto entre 1 y 5
sp <- shortest.paths(g, v = "1", to = "5")    
#Mostrar la longitud calculada del camino más corto
sp[]  
# Obtener los vértices involucrados en el camino más corto entre "1" y "5"
gsp <- get.shortest.paths(g, from = "1", to = "5")
# Mostrar los nombres de los vértices que componen el camino más corto
V(g)[gsp$vpath[[1]]]     
# Crear una matriz de adyacencia, utilizando el peso de las aristas como valores
adj <- get.adjacency(g, attr='weight', sparse = FALSE)
# Mostrar la matriz de adyacencia
adj
# Calcular la matriz de distancias más cortas entre todos los nodos usando los pesos de las aristas
distMatrix <- shortest.paths(g, weights = E(g)$weight)
# Mostrar la distancia más corta entre el nodo "1" y el nodo "5"
distMatrix[1, 5] 
# Obtener todos los caminos más cortos desde el nodo "1" a todos los demás nodos
allsp <- get.all.shortest.paths(g, from = "1")
# Inspeccionar la estructura de los caminos más cortos encontrados
str(allsp)
