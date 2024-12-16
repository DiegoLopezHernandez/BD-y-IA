# Función que encuentra el menor divisor de un número (distinto de 1)
# Esto ayuda a encontrar 'q' en el caso del cálculo de RSA
findQ <- function(num) {
  q <- 2  # Inicializamos 'q' en 2, el menor número primo
  while (num %% q != 0) {  # Buscamos el menor número que divide a 'num' sin residuo
    q <- q + 1  # Incrementamos 'q' hasta encontrar el divisor
  }
  return(q)  # Devolvemos el divisor encontrado
}

# Función que calcula 'p' usando la relación n = p * q
findP <- function(numN, numQ) {
  return(numN / numQ)  # 'p' se calcula como el cociente entre 'n' y 'q'
}

# Función que calcula 'd', el inverso modular de 'e' módulo de Euler
findD <- function(numE, numEuler) {
  d <- 1  # Iniciamos 'd' en 1
  # Incrementamos 'd' hasta que (d * e) % Euler == 1, es decir, cuando es su inverso modular
  while ((d * numE) %% numEuler != 1) {
    d <- d + 1
  }
  return(d)  # Devolvemos el valor de 'd'
}

# Función que calcula el valor de Euler φ(n) = (p-1)*(q-1)
calculateEuler <- function(numP, numQ) {
  euler <- (numP - 1) * (numQ - 1)  # Fórmula para calcular el número de Euler
  return(euler)  # Devolvemos el resultado
}

# Función que descifra un mensaje cifrado (m1) usando 'd', 'n', y RSA
decypher <- function(numD, numM1, numN) {
  m <- 1  # Inicializamos el mensaje descifrado 'm'
  i <- 1  # Contador
  # Aplicamos el descifrado m = (m1^d) mod n usando exponenciación modular
  while (i <= numD) {
    m <- (numM1 * m) %% numN
    i <- i + 1
  }
  return(m)  # Devolvemos el mensaje descifrado
}

# Función que cifra un mensaje 'm' usando la clave pública (e, n)
cypher <- function(num.m, num.e, num.n) {
  c <- num.m^num.e %% num.n  # Fórmula de cifrado RSA: c = (m^e) mod n
  return(c)  # Devolvemos el mensaje cifrado
}

# Función que descifra un mensaje cifrado 'm1' usando 'e' y 'n'
Descifrar <- function(num.e, num.n, num.m1) {
  q <- findQ(num.n)  # Encontramos 'q' (el menor divisor de 'n')
  p <- findP(num.n, q)  # Calculamos 'p' usando n = p * q
  Euler <- calculateEuler(p, q)  # Calculamos φ(n) = (p-1)*(q-1)
  d <- findD(num.e, Euler)  # Calculamos 'd', el inverso modular de 'e' módulo φ(n)
  m <- decypher(d, num.m1, num.n)  # Desciframos el mensaje usando 'd', 'm1', y 'n'
  return(m)  # Devolvemos el mensaje descifrado
}

# Función que calcula la clave privada 'd' usando la clave pública (e, n)
ClavePrivada <- function(num.e, num.n) {
  q <- findQ(num.n)  # Encontramos 'q' (el menor divisor de 'n')
  p <- findP(num.n, q)  # Calculamos 'p' usando n = p * q
  Euler <- calculateEuler(p, q)  # Calculamos φ(n) = (p-1)*(q-1)
  d <- findD(num.e, Euler)  # Calculamos 'd', el inverso modular de 'e' módulo φ(n)
  return(d)  # Devolvemos 'd', que forma parte de la clave privada
}

# EJERCICIO DE EXAMEN
# Nos dan: e, n, m' (clave pública y un mensaje cifrado)

# Ejemplo:
# e = 7, n = 601723, m' = 7
e <- 7  # Exponente público de la clave RSA
m1 <- 7  # Mensaje cifrado
n <- 601723  # Producto de dos números primos p y q

# Calculamos la clave privada
ClavePrivada(e, n)

# Ciframos el mensaje 7 con la clave pública (e, n)
cifrado <- cypher(7, 7, 601723)

# Desciframos el mensaje cifrado con la clave pública y el cifrado
Descifrar(e, n, cifrado)

