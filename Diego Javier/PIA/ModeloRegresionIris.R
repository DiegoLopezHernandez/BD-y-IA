Salud<-read.csv(file.choose(), header=TRUE,sep = ",")

#Ejercicio. Modelo de regresion para pronosticar la longitud del petalo
#a traves de las variables longitud tallo, anchura tallo y anchura petalo
dim(Salud)
head(Salud)
# Cargar librerías necesarias
library(dplyr)

# Cargar el archivo de datos (cambia el nombre del archivo si es diferente)
# Verificar que las columnas que necesitamos están presentes
head(Salud)

# Crear un modelo de regresión lineal múltiple para predecir PRN
modelo <- lm(PRN ~ Edad + Perimetro + IMC + AumPeso + Ngest + PAB, data = Salud)

# Resumen del modelo (para ver los parámetros y la validez)
summary(modelo)

modelo_arreglado <- lm (PRN ~ Edad + PAB + Ngest, data= Salud)
summary(modelo_arreglado)

# Modelo logarítmico: transformamos la variable dependiente (PRN) logarítmicamente
modelo_log <- lm(log(PRN) ~ Edad + PAB, data = Salud)
summary(modelo_log)

# Modelo doble logarítmico: transformamos tanto la variable dependiente (PRN) como las independientes
modelo_doble_log <- lm(log(PRN) ~ log(Edad) + log(PAB) , data = Salud)
summary(modelo_doble_log)

