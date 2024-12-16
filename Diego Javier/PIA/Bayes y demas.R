datos <- read.csv(file.choose(), header = TRUE, sep = ";")
head(datos)
# Modelo de regresión lineal (normal)
modelo <- lm(PRN ~ Edad + Perimetro + Talla, data = datos)
summary(modelo)

# Modelo de regresión logarítmica (solo log de la variable dependiente)
modelo_log <- lm(log(PRN) ~ Edad + Perimetro + Talla, data = datos)
summary(modelo_log)

# Modelo de regresión doble logarítmica (log de la variable dependiente e independientes)
modelo_doble_log <- lm(log(PRN) ~ log(Edad) + log(Perimetro) + log(Talla), data = datos)
summary(modelo_doble_log)

# Prueba t de Student para comparar la talla según el tipo de parto
t.test(Talla ~ Tipo.Parto, data = datos)






# Calcular la media y el IC por sexo
aggregate(Perimetro ~ Sexo, data = datos, FUN = function(x) {
  c(media = mean(x), IC = t.test(x)$conf.int)
})

