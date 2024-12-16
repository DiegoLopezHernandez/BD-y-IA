# Función para calcular TPR (tasa de verdaderos positivos) y FPR (tasa de falsos positivos) dado un corte
tabla <- function(corte, prob, realidad) {
  # Inicialización de contadores para los diferentes casos
  TP <- 0  # Verdaderos Positivos
  TN <- 0  # Verdaderos Negativos
  FP <- 0  # Falsos Positivos
  FN <- 0  # Falsos Negativos
  i <- 1   # Índice para recorrer la lista
  
  # Bucle para recorrer todas las probabilidades y clasificar
  while (i <= length(prob)) {
    if (prob[[i]] >= corte & realidad[[i]] == "SI") { # Caso: Verdadero Positivo
      TP <- TP + 1
    }
    if (prob[[i]] >= corte & realidad[[i]] == "NO") { # Caso: Falso Positivo
      FP <- FP + 1
    }
    if (prob[[i]] < corte & realidad[[i]] == "SI") {  # Caso: Falso Negativo
      FN <- FN + 1
    }
    if (prob[[i]] < corte & realidad[[i]] == "NO") {  # Caso: Verdadero Negativo
      TN <- TN + 1
    }
    i <- i + 1  # Incrementa el índice
  }
  
  # Cálculo de TPR y FPR
  TPR <- TP / (TP + FN)  # Sensibilidad
  FPR <- FP / (FP + TN)  # 1 - Especificidad
  
  # Devuelve los valores calculados como un vector
  x <- c(TPR, FPR)
  return(x)
}

# Lista de probabilidades de predicción
probabilidades <- c(0.95,0.9,0.85,0.8,0.75,0.7,0.65,0.6,0.55,0.5,0.45,0.4,0.35,0.3,0.25)

# Lista de valores reales ("SI" o "NO") para cada caso
R <- rep("SI", 4)  # 9 positivos
R <- append(R, rep("NO", 1))  # 11 negativos
R <- append(R, rep("SI", 3))  # 11 negativos
R <- append(R, rep("NO", 7))  # 11 negativos

# Inicialización de listas para almacenar TPR y FPR
TPR <- c()  # Lista para TPR
FPR <- c()  # Lista para FPR

# Lista de cortes para calcular las tasas
cortes <- c(0.9, 0.8, 0.7, 0.6, 0.5)

# Bucle para calcular TPR y FPR para cada corte
i <- 1
while (i <= length(cortes)) {
  y <- tabla(cortes[[i]], probabilidades, R)  # Llama a la función 'tabla' para el corte actual
  TPR <- append(TPR, y[[1]])  # Agrega el TPR calculado a la lista
  FPR <- append(FPR, y[[2]])  # Agrega el FPR calculado a la lista
  i <- i + 1  # Incrementa el índice
}

# Construcción de un data frame con los valores de TPR y FPR
frame <- data.frame(TPR = TPR, FPR = FPR)
frame  # Muestra el data frame

# Función para calcular el área bajo la curva usando la regla del trapecio
trapecio <- function(x1, x2, y1, y2) {
  # Fórmula del área del trapecio: (base * altura promedio)
  z <- (x2 - x1) * (y1 + y2) / 2
  return(z)
}

# Función para calcular el AUC usando la regla del trapecio
Area <- function(x, y) {
  i <- 1  # Índice para iterar
  A <- 0  # Acumulador del área
  j <- 1  # Índice auxiliar
  
  # Bucle para calcular el área bajo la curva
  while (i < length(x)) {
    j <- i + 1  # Punto siguiente
    A <- A + trapecio(x[[i]], x[[j]], y[[i]], y[[j]])  # Suma el área del trapecio
    i <- i + 1  # Incrementa el índice
  }
  return(abs(A))  # Devuelve el área total (en valor absoluto)
}

# Verifica que las listas TPR y FPR tengan la misma longitud
length(frame$FPR)
length(frame$TPR)

# Calcula el área bajo la curva (AUC)
A <- Area(frame$FPR, frame$TPR)
A  # Muestra el valor del AUC

# Representación del resultado en una gráfica ROC
Result <- paste("AUC=", A)  # Crea una etiqueta con el AUC
Result

# Dibuja la curva ROC
plot(frame$FPR, frame$TPR, main = "ROC", type = 'b', pch = 1, cex = 1, lty = 1, lwd = 1, col = 'blue',
     col.main = 'black', ylab = "Sensibilidad", xlab = "1-Especificidad", sub = Result,
     col.sub = "green")

#-------------------------------------------------------------------------------

# Instala el paquete "igraph" con todas sus dependencias
install.packages("igraph", dependencies=TRUE)

# Carga la librería "igraph" para trabajar con grafos
library(igraph)

# Define el conjunto de enlaces entre nodos como un data frame
enlaces <- data.frame(stringsAsFactors=FALSE,
                      from = c(16, 1, 1, 14, 14, 1, 14, 14, 1, 13, 13, 13, 14, 1, 15, 1),
                      to = c(15, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1, 16))

# Define las características de los nodos en un data frame
nodos <- data.frame(stringsAsFactors=FALSE,
                    to = c(15, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1, 16),
                    punto_destino = c("Captacion planta", "C.RR.A naranjos", "Balsa C.RR.A",
                                      "Fuente Pobre", "Toma nueva C.RR.A", "C.RR.P 1", "C.RR.P bombeo", 
                                      "C.RR.P 2", "Ayuntamiento", "C.RR.T", "C.RR.T 2", "C.RR.L", 
                                      "Embalse", "Bombeo", "Producto planta", "Mar"),
                    Volumen = c("12155.437", "64.585", "682.884", "442.198", "307.868",
                                "125.661", "453.680", "384.699", "0.000", "685.458", "272.581",
                                "1942.262", "2900.301", "4614.407", "5361.876", "6793.561"),
                    Grupo = c("PLANTA", "C.RR.A", "C.RR.A", "C.RR.A", "C.RR.A", 
                              "C.RR.P", "C.RR.P", "C.RR.P", "Ayuntamiento", "C.RR.T", 
                              "C.RR.T", "C.RR.L", "PLANTA", "PLANTA", "PLANTA", "PLANTA"))

# Convierte la columna Volumen a numérico y la escala a miles (Hm3)
nodos$Volumen <- as.numeric(nodos$Volumen) / 1000

# Crea un grafo no dirigido utilizando los enlaces y nodos definidos
red <- graph_from_data_frame(enlaces, directed=FALSE, vertices=nodos)

# Imprime las características del grafo (enlaces y nodos)
print(red, e=TRUE, v=TRUE)

# Define colores para los grupos de nodos
colrs <- c("gray50", "tomato", "gold", "orange", "green", "lightblue")

# Asigna colores a los nodos según su grupo
V(red)$color <- colrs[as.factor(nodos$Grupo)]

# Ajusta el tamaño de los nodos en función del volumen
V(red)$size <- nodos$Volumen

# Muestra las aristas del grafo
E(red)

# Muestra los vértices del grafo
V(red)

# Dibuja el grafo básico
plot(red)

# Ajusta parámetros gráficos para evitar márgenes grandes
par(mfrow=c(1,1), mar=c(0,0,1,0))

# Dibuja el grafo con etiquetas y ajustes personalizados
plot(red, vertex.label=nodos$punto_destino,
     vertex.color="lightblue",
     vertex.size=nodos$Volumen*2,
     edge.arrow.size=1, edge.color="darkblue",
     edge.curved=.1,
     edge.width=nodos$Volumen*3)

# Calcula un diseño de disposición para los nodos utilizando el algoritmo Fruchterman-Reingold
l <- layout_with_fr(red)

# Dibuja el grafo con un diseño mejorado y más detalles
plot(red, layout=l, main="Distribución agua en la red - feb 2019",
     vertex.label=paste(nodos$punto_destino, "\n", signif(nodos$Volumen, digits=2), "Hm3"),
     vertex.label.cex=.7, 
     vertex.label.color="black",
     vertex.color=V(red)$color,
     vertex.size=nodos$Volumen*2,
     edge.color=V(red)$color,
     edge.curved=.1,
     edge.width=nodos$Volumen*2)

# Agrega una leyenda para identificar los grupos por color
legend(x=-1, y=1.1, levels(as.factor(nodos$Grupo)), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#Para ver si es conexo el grafico
is_connected(red)

#
shortest_paths(red, from=1,to=12)$vpath

Media1<-closeness(red, vids=1)
Media13<-closeness(red, vids=13)

list(Media1=Media1,Media13=Media13)

adj_matrix <- as_adjacency_matrix(red, sparse=FALSE)
adj_matrix

caminocorto<-all_simple_paths(red, from=1, to=14,cutoff = 3)
caminocorto

