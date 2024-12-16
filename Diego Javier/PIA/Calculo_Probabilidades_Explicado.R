# PROBABILIDADES EN DISTRIBUCIONES DE POISSON
# -------------------------------------------
# 'a': Complemento de la probabilidad acumulada de 0 a 3 para una λ = 3 en Poisson
1 - ppois(3, 3)  # Probabilidad de más de 3 eventos en una distribución de Poisson con media 3

# 'b': Probabilidad de que ocurran exactamente 0 eventos con λ = 1.5
dpois(0, 1.5)  # f(0) en Poisson con media 1.5

# 'c': Probabilidad de que ocurra exactamente 1 evento con λ = 1.5
dpois(1, 1.5)  # f(1) en Poisson con media 1.5

# 'd': Complemento de la probabilidad acumulada de 0 a 1 para λ = 1.5
1 - ppois(1, 1.5)  # Probabilidad de más de 1 evento en una distribución de Poisson con media 1.5


# CALCULO DE PROBABILIDAD PARA EXTRACCIONES SIN REPOSICIÓN
# --------------------------------------------------------

# Función que calcula el coeficiente binomial "a sobre b"
# Representa el número de combinaciones posibles
Combinatorio <- function(a, b) {
  f <- factorial(a) / (factorial(b) * factorial(a - b))  # Fórmula del coeficiente binomial
  return(f)  # Devuelve el número de combinaciones posibles
}

# Función de probabilidad de una muestra sin reposición
# Calcula la probabilidad de obtener r elementos marcados en una muestra de tamaño n
# Parámetros:
# N -> Tamaño total de la población
# D -> Número total de elementos marcados
# n -> Tamaño de la muestra
# r -> Número de elementos marcados en la muestra
F <- function(N, D, n, r) {
  x <- (Combinatorio(D, r) * Combinatorio(N - D, n - r)) / Combinatorio(N, n)  # Fórmula de probabilidad
  return(x)  # Devuelve la probabilidad
}

# EJEMPLO: Dado D = 5, n = 10, r = 1, encontrar el N que maximiza la probabilidad
D <- 5  # Número total de elementos marcados
n <- 10  # Tamaño de la muestra
r <- 1  # Número de elementos marcados en la muestra

# Encontrar N (tamaño total de la población) maximizando la probabilidad
N <- n + D - r  # Valor inicial de N basado en la lógica del problema
Siguiente <- N + 1  # Comenzamos evaluando el siguiente valor de N
NProb <- F(N, D, n, r)  # Probabilidad inicial para N
SiguienteProb <- F(Siguiente, D, n, r)  # Probabilidad para el siguiente N

# Almacenamos las probabilidades para graficar si es necesario
probabilidades <- c()

# Iteramos hasta que la probabilidad comience a disminuir
while (NProb <= SiguienteProb) {
  Siguiente <- Siguiente + 1  # Avanzamos al siguiente N
  NProb <- SiguienteProb  # Actualizamos la probabilidad de N
  SiguienteProb <- F(Siguiente, D, n, r)  # Calculamos la probabilidad para el siguiente N
  probabilidades <- c(probabilidades, SiguienteProb)  # Guardamos la probabilidad
}

#plot(probabilidades)  # Gráfica de probabilidades (descomentar para visualizar)
#Siguiente  # Muestra el valor de N donde la probabilidad dejó de aumentar


# FUNCIÓN PARA CALCULAR N MAXIMIZANDO LA PROBABILIDAD
CalcularN <- function(D, n, r) {
  N <- n + D - r  # Valor inicial de N basado en la lógica del problema
  Siguiente <- N + 1  # Comenzamos evaluando el siguiente valor de N
  NProb <- F(N, D, n, r)  # Probabilidad inicial para N
  SiguienteProb <- F(Siguiente, D, n, r)  # Probabilidad para el siguiente N
  
  # Iteramos hasta que la probabilidad comience a disminuir
  while (NProb <= SiguienteProb) {
    Siguiente <- Siguiente + 1  # Avanzamos al siguiente N
    NProb <- SiguienteProb  # Actualizamos la probabilidad de N
    SiguienteProb <- F(Siguiente, D, n, r)  # Calculamos la probabilidad para el siguiente N
  }
  return(Siguiente - 1)  # Devolvemos el valor de N que maximiza la probabilidad
}

CalcularN(5, 10, 1)  # Ejemplo: D = 5, n = 10, r = 1


# FUNCIÓN PARA CALCULAR D (NÚMERO DE ELEMENTOS MARCADOS EN TOTAL)
CalcularD <- function(N, n, r) {
  D <- r  # Iniciamos D en el valor mínimo lógico (igual a r)
  Siguiente <- D + 1  # Evaluamos el siguiente valor de D
  DProb <- F(N, D, n, r)  # Probabilidad inicial para D
  SiguienteProb <- F(N, Siguiente, n, r)  # Probabilidad para el siguiente D
  
  # Iteramos hasta que la probabilidad comience a disminuir
  while (DProb <= SiguienteProb) {
    DProb <- SiguienteProb  # Actualizamos la probabilidad de D
    Siguiente <- Siguiente + 1  # Avanzamos al siguiente D
    SiguienteProb <- F(N, Siguiente, n, r)  # Calculamos la probabilidad para el siguiente D
  }
  return(D)  # Devolvemos el valor de D que maximiza la probabilidad
}

CalcularD(30, 10, 1)  # Ejemplo: N = 30, n = 10, r = 1


# TEST DE HIPÓTESIS PARA LA MEDIA
# -------------------------------

# Simulamos 5000 observaciones de una distribución de Poisson con λ = 3
x <- rpois(5000, 3)

# Planteamos la hipótesis:
# Ho: La media es igual a 3 (hipótesis nula)
# H1: La media no es igual a 3 (hipótesis alternativa)
t.test(x, mu = 3, alternative = 'two.sided', conf.level = 0.99)
# Resultado:
# - La media estimada está cerca de 3
# - Intervalo de confianza del 99%
# - Si p-valor < 0.02, se rechaza Ho y aceptamos H1

CalcularN(4, 3, 1)  # Cálculo adicional como validación

