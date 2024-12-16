# Leer el archivo CSV que seleccionemos
Salud <- read.csv(file.choose(), header = TRUE, sep = ',')

# Seleccionar columnas específicas del archivo
x <- c(2,3,4,5,10,12,14,15)  # Índices de las columnas que queremos conservar
Salud <- Salud[,x]  # Filtramos el dataset con las columnas seleccionadas

# Función para encriptar el nombre de una columna usando una ecuación lineal
EncriptarNombre <- function(table, index, ecA, ecB) {
  # Convertir el nombre de la columna a mayúsculas
  z <- toupper(names(table)[[index]])
  
  # Dividir el nombre en caracteres individuales
  x <- strsplit(z, split = "")
  
  # Lista de letras del alfabeto en mayúscula
  A <- LETTERS
  
  # Inicializamos un vector para almacenar los caracteres encriptados
  cod <- c()
  i <- 1
  
  # Iterar sobre cada letra del nombre
  while (i <= length(x[[1]])) {
    # Encontrar la posición de la letra en el alfabeto
    w <- which(A == x[[1]][[i]])
    
    # Aplicar la fórmula de encriptación: y = (ecA * x + ecB) mod longitud del alfabeto
    s <- (ecA * w + ecB) %% length(A)
    s <- s + 1  # Ajustamos la posición al índice R (que comienza en 1)
    
    # Agregar el caracter encriptado al vector
    cod <- append(cod, A[[s]])
    i <- i + 1
  }
  
  # Unir las letras encriptadas en un único string
  C <- paste(cod, collapse = "")
  return(C)
}

# Función para encontrar el inverso modular de un número
inverso <- function(a, longitud) {
  i <- 1
  # Buscar el número que cumple la condición (i * a) mod longitud = 1
  while (i < longitud && (i * a) %% longitud != 1) {
    i <- i + 1
  }
  return(i)
}

# Función para desencriptar un nombre previamente encriptado
DesencriptarNombre <- function(nombre, ecA, ecB) {
  # Dividir el nombre en caracteres individuales
  x <- strsplit(nombre, split = "")
  
  # Lista de letras del alfabeto en mayúscula
  A <- LETTERS
  
  # Inicializamos un vector para los caracteres desencriptados
  cod <- c()
  i <- 1
  
  # Iterar sobre cada letra del nombre
  while (i <= length(x[[1]])) {
    # Encontrar la posición de la letra en el alfabeto
    w <- which(A == x[[1]][[i]])
    w <- w - 1  # Ajustar posición al índice del alfabeto
    
    # Aplicar la fórmula de desencriptación: x = inverso(ecA) * (y - ecB) mod longitud del alfabeto
    s <- (inverso(ecA, length(A)) * (w - ecB)) %% length(A)
    
    # Agregar el caracter desencriptado al vector
    cod <- append(cod, A[[s]])
    i <- i + 1
  }
  
  # Unir las letras desencriptadas en un único string
  C <- paste(cod, collapse = "")
  return(C)
}

# Encriptar el nombre de la columna "Gender" con la fórmula y = 7 * x + 11
genderEncriptado <- EncriptarNombre(Salud, 2, 7, 11)
genderEncriptado  # Mostrar el nombre encriptado

# Desencriptar el nombre previamente encriptado
genderDesencriptado <- DesencriptarNombre(genderEncriptado, 7, 11)
genderDesencriptado  # Mostrar el nombre desencriptado

# Función para permutar las columnas de una tabla
PermutarColumnas <- function(tabla, permutacion) {
  tabla1 <- tabla  # Copia de la tabla original
  k <- 1
  
  # Intercambiar las columnas según la permutación
  while (k <= dim(tabla)[[2]]) {
    tabla1[,k] <- tabla[,permutacion[[k]]]
    k <- k + 1
  }
  
  tabla <- tabla1  # Actualizamos la tabla original con la permutada
  return(tabla)
}

# Generar una permutación aleatoria para las columnas
Numeros <- seq(1, dim(Salud)[[2]], 1)  # Secuencia de números del 1 al número de columnas
P <- sample(Numeros, dim(Salud)[[2]], replace = FALSE)  # Permutación aleatoria

# Aplicar la permutación a las columnas
permutado <- PermutarColumnas(Salud, P)
head(permutado)  # Mostrar las primeras filas de la tabla permutada

# Introducir ruido aleatorio en la columna de edad (Age)
ruido1 <- rnorm(dim(Salud)[[1]], 10, 2)  # Generar ruido con media 10 y desviación estándar 2
Salud[,1] <- Salud[,1] + ruido1  # Sumar el ruido a la columna de edad
head(Salud)  # Mostrar las primeras filas con ruido añadido

# Eliminar el ruido de la columna de edad
Salud[,1] <- Salud[,1] - ruido1  # Restar el ruido para recuperar los valores originales
head(Salud)  # Mostrar las primeras filas con los valores originales restaurados

# Test de normalidad Kolmogorov-Smirnov para la columna de edad
ks.test(Salud[,1], "pnorm", mean(Salud[,1]), sd(Salud[,1]))  # Realizar el test de normalidad

