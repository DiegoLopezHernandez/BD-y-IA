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
probabilidades <- c(0.99, 0.97, 0.92, 0.9, 0.88, 0.85, 0.78, 0.75, 0.73, 0.6, 0.18, 0.16, 0.15, 0.14, 0.13, 0.11, 0.1, 0.06, 0.04, 0.02)

# Lista de valores reales ("SI" o "NO") para cada caso
R <- rep("SI", 9)  # 9 positivos
R <- append(R, rep("NO", 11))  # 11 negativos

# Inicialización de listas para almacenar TPR y FPR
TPR <- c()  # Lista para TPR
FPR <- c()  # Lista para FPR

# Lista de cortes para calcular las tasas
cortes <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

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

