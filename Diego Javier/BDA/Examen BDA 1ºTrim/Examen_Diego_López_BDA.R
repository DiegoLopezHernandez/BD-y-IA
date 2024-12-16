Salud <- read.csv(file.choose(), header = TRUE, sep = ',')

#---------------------------------------------------------------------------------------------

# Función para permutar columnas directamente en la tabla original
PermutarColumnas <- function(Salud, permutacion) {
  # Reorganiza las columnas de la tabla "Salud" en el orden indicado por la permutación
  Salud <- Salud[, permutacion]
  return(Salud)
}

# Generar una permutación aleatoria para las columnas
Numeros <- seq(1, dim(Salud)[[2]])  # Genera una secuencia de números del 1 al número de columnas
P <- sample(Numeros, dim(Salud)[[2]], replace = FALSE)  # Genera una permutación aleatoria de los índices de columnas

# Aplicar la permutación directamente en la tabla
Salud <- PermutarColumnas(Salud, P)

# Muestra las primeras filas de la tabla con las columnas permutadas
print(head(Salud))

# Función para invertir una permutación
InvertirPermutacion <- function(permutacion) {
  # Devuelve una permutación inversa que deshace la permutación original
  inversa <- numeric(length(permutacion))
  for (i in seq_along(permutacion)) {
    inversa[permutacion[i]] <- i
  }
  return(inversa)
}

# Obtener la permutación inversa para restaurar el orden original
P_inversa <- InvertirPermutacion(P)

# Restaurar el orden original de las columnas
Salud <- PermutarColumnas(Salud, P_inversa)

# Muestra las primeras filas de la tabla restaurada
print(head(Salud))

#---------------------------------------------------------------------------------------------

# Función para encriptar el nombre de una columna usando una ecuación lineal
EncriptarNombre <- function(table, index, ecA, ecB) {
  # Convierte el nombre de la columna a mayúsculas y lo divide en caracteres individuales
  z <- toupper(names(table)[[index]])
  x <- strsplit(z, split = "")
  A <- LETTERS  # Lista de letras mayúsculas del alfabeto
  cod <- c()  # Inicializa un vector para almacenar el resultado encriptado
  i <- 1
  while (i <= length(x[[1]])) {
    w <- which(A == x[[1]][[i]])  # Encuentra la posición de la letra en el alfabeto
    s <- (ecA * w + ecB) %% length(A)  # Aplica la ecuación y = (ecA * x + ecB) mod longitud del alfabeto
    s <- s + 1  # Ajuste para el índice basado en R (comienza en 1)
    cod <- append(cod, A[[s]])  # Agrega el caracter encriptado
    i <- i + 1
  }
  C <- paste(cod, collapse = "")  # Junta los caracteres encriptados en un solo string
  return(C)
}

# Función para calcular el inverso modular
inverso <- function(a, longitud) {
  # Encuentra el valor que cumple (i * a) mod longitud = 1
  i <- 1
  while (i < longitud && (i * a) %% longitud != 1) {
    i <- i + 1
  }
  return(i)
}

# Función para desencriptar un nombre previamente encriptado
DesencriptarNombre <- function(nombre, ecA, ecB) {
  # Divide el nombre en caracteres individuales
  x <- strsplit(nombre, split = "")
  A <- LETTERS  # Lista de letras mayúsculas del alfabeto
  cod <- c()  # Inicializa un vector para almacenar el resultado desencriptado
  i <- 1
  while (i <= length(x[[1]])) {
    w <- which(A == x[[1]][[i]]) - 1  # Encuentra la posición en el alfabeto y ajusta el índice
    s <- (inverso(ecA, length(A)) * (w - ecB)) %% length(A)  # Aplica la fórmula de desencriptación
    cod <- append(cod, A[[s]])  # Agrega el caracter desencriptado
    i <- i + 1
  }
  C <- paste(cod, collapse = "")  # Junta los caracteres desencriptados en un solo string
  return(C)
}

# Ejemplo de encriptación y desencriptación del nombre de una columna
PerimetroEncriptado <- EncriptarNombre(Salud, 2, 15, 14)
PerimetroEncriptado
PerimetroDesencriptado <- DesencriptarNombre(PerimetroEncriptado, 15, 14)
PerimetroDesencriptado
#---------------------------------------------------------------------------------------------

# Introducir ruido aleatorio en una columna (Edad)
ruido1 <- runif(dim(Salud)[[1]], -10, 10)  # Genera ruido uniforme entre -10 y 10
Salud[,5] <- Salud[,5] + ruido1  # Agrega el ruido a la columna de interés
head(Salud)  # Muestra las primeras filas con ruido añadido

# Eliminar el ruido para restaurar los valores originales
Salud[,5] <- Salud[,5] - ruido1  # Resta el ruido para recuperar los valores originales
head(Salud)  # Muestra las primeras filas restauradas

#---------------------------------------------------------------------------------------------

# Funciones auxiliares para cifrado RSA y operaciones relacionadas
findQ <- function(num) {
  # Encuentra el menor divisor primo de un número
  q <- 2
  while (num %% q != 0) {
    q <- q + 1
  }
  return(q)
}

findP <- function(numN, numQ) {
  # Calcula 'p' a partir de 'n' y 'q'
  return(numN / numQ)
}

findD <- function(numE, numEuler) {
  # Calcula el inverso modular de 'e' módulo φ(n)
  d <- 1
  while ((d * numE) %% numEuler != 1) {
    d <- d + 1
  }
  return(d)
}

calculateEuler <- function(numP, numQ) {
  # Calcula el número de Euler φ(n) = (p-1)*(q-1)
  return((numP - 1) * (numQ - 1))
}

decypher <- function(numD, numM1, numN) {
  # Descifra un mensaje cifrado usando RSA
  m <- 1
  i <- 1
  while (i <= numD) {
    m <- (numM1 * m) %% numN
    i <- i + 1
  }
  return(m)
}

cypher <- function(num.m, num.e, num.n) {
  # Cifra un mensaje usando la clave pública
  return(num.m^num.e %% num.n)
}

Descifrar <- function(num.e, num.n, num.m1) {
  # Descifra un mensaje cifrado utilizando RSA
  q <- findQ(num.n)
  p <- findP(num.n, q)
  Euler <- calculateEuler(p, q)
  d <- findD(num.e, Euler)
  return(decypher(d, num.m1, num.n))
}

ClavePrivada <- function(num.e, num.n) {
  # Calcula la clave privada RSA
  q <- findQ(num.n)
  p <- findP(num.n, q)
  Euler <- calculateEuler(p, q)
  return(findD(num.e, Euler))
}

# Ejemplo de cifrado y descifrado con RSA
e <- 13
m1 <- 8
n <- 807791
ClavePrivada(e, n)
cifrado <- cypher(8, 13, 807791)
cifrado
Descifrar(e, n, cifrado)

#---------------------------------------------------------------------------------------------

# Modelos de regresión y pruebas estadísticas
modelo <- lm(PRN ~ Perimetro + Edad + PAB, data = Salud)  # Ajusta un modelo lineal
summary(modelo)  # Resumen del modelo
#p-value: 0.003527 tiene un efecto significativo

# Separación de datos según sexo
hombres <- which(Salud$SEXO == "V")
mujeres <- which(Salud$SEXO == "M")
corte <- length(hombres)

# Construcción de un nuevo dataset para aplicar el test de Chow
Perimetro <- append(Salud[hombres,]$Perimetro, Salud[mujeres,]$Perimetro)
SEXO <- append(Salud[hombres,]$SEXO, Salud[mujeres,]$SEXO)
Edad <- append(Salud[hombres,]$Edad, Salud[mujeres,]$Edad)
PAB <- append(Salud[hombres,]$PAB, Salud[mujeres,]$PAB)
PRN <- append(Salud[hombres,]$PRN, Salud[mujeres,]$PRN)

Frame <- data.frame(PRN = PRN, SEXO = SEXO, Edad = Edad, Perimetro = Perimetro, PAB = PAB)

library(strucchange)  # Carga el paquete para pruebas estructurales
sctest(Frame$PRN ~ Frame$Perimetro + Frame$Edad + Frame$PAB, type = "Chow", point = corte)  # Prueba de Chow para cambios estructurales
#No existe permanencia estructural

# Prueba de normalidad para la columna PAB
ks.test(Salud[,9], "pnorm", mean(Salud[,9]), sd(Salud[,9]))  # Realiza el test de Kolmogorov-Smirnov
#Si siguen una distribuccion normal

#---------------------------------------------------------------------------------------------
