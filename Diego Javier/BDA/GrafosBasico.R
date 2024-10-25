install.packages("igraph",dependencies = T)
library(igraph)
#-------------------------------------------------
#Creamos el grafo no dirigido(si no pones nada sale dirigido por defecto)
g<-graph(c(1,2, 2,3, 1,3, 1,4),directed=FALSE)
#Lo representamos
plot(g)

#Estudiamos si es conexo
is.connected(g)

#Cuantos vertices
length(V(g))

#Quienes son los vertices
V(g)

#Quienes son los aristas
E(g)

#Nombres, Peso, Color a los vertices
V(g)$name<-c("Ana","Diego","Sebas","Manolo")
V(g)$weight<-c(2,6.2,7.1,4)
V(g)$color<-c("green","gray","red","purple")





