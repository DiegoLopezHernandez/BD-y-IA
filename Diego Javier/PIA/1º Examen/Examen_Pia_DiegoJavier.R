combinatorio<-function(a,b){
  f<-factorial(a)/(factorial(b)*factorial(a-b))
  return(f)}

F<-function(N,D,n,r){
  x<-combinatorio(D,r)*combinatorio(N-D,n-r)/combinatorio(N,n)
  return(x)
}
#Se desea conocer el numero de ejemplares de una especie
#Para ello se extraen y se marcan
D<-30
#Se deposita con el resto y se extraen
n<-20
#y marcados hay
r<-2
#Estimar N dandole valor con todas las truchas encontradas en la recogida
N<-48
Siguiente<-49
vector<-c(F(N,D,n,r))
while (F(N,D,n,r)<=F(Siguiente,D,n,r)) {
  N<-Siguiente
  Siguiente<-Siguiente+1
  vector<-append(vector,F(N,D,n,r))
}
plot(vector)
N



#-------------------------------------------------------------------------------
#Se desea saber el numero de piezas defectuosas en una produccion

#Tenemos 40 piezas
N<-40

#Desconocemos D=Numero de defectuosas
#Se extraen 5
n<-5

#Defectuosas hay 1
r<-1
D<-1
Siguiente<-2
vector<-c(F(N,D,n,r))

while (F(N,D,n,r)<=F(N,Siguiente,n,r)) {
  D<-Siguiente
  Siguiente<-Siguiente+1
  vector<-append(vector,F(N,D,n,r))
}
D
plot(vector)

#-------------------------------------------------------------------------------------------

# Probabilidades iniciales
P_E <- 0.0065  # Probabilidad de tener la enfermedad
P_noE <- 1 - P_E  # Probabilidad de no tener la enfermedad

P_Pos_given_E <- 0.999  # Sensibilidad: P(Positivo | Enfermedad)
P_Pos_given_noE <- 0.001  # Falso positivo: P(Positivo | No Enfermedad)

# (a) Probabilidad de que el diagnóstico sea positivo
P_Pos <- P_Pos_given_E * P_E + P_Pos_given_noE * P_noE  # Teorema de probabilidad total
P_Pos

# (b) Probabilidad de error dado un diagnóstico positivo
P_noE_given_Pos <- (P_Pos_given_noE * P_noE) / P_Pos  # Teorema de Bayes
P_noE_given_Pos

#--------------------------------------------------------------------------------------------

Salud<-read.csv(file.choose(), header=TRUE,sep = ",")
Salud
# Cargar librerías necesarias
library(dplyr)

# Cargar el archivo de datos (cambia el nombre de archivo si es diferente)
# Verificar que las columnas que necesitamos están presentes
head(Salud)

# Aplicar el t-test para comparar las medias de PAB según el SEXO
t_test_result <- t.test(PAB ~ SEXO, data = Salud)

# Mostrar los resultados del t-test
t_test_result

#p-value = 0.6708 esto significa que al ser el p-value superior a 0.05 
#No tiene relevancia el SEXO con el PAB.

#----------------------------------------------------------------------------------------





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

