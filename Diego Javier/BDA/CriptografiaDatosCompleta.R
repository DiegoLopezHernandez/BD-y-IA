y<-"Edad"

# Convertir a mayusculas

z<-toupper(y)
z
x<-strsplit(z,split="")
x
x[[1]][[1]]
x[[1]][[2]]
x[[1]][[3]]
x[[1]][[4]]

# Software para codificar

# Considero un alfabeto
A<-LETTERS
A
# Cargamos el archivo
tabla<-read.table(file.choose(),header=TRUE,sep=",")
# Consultamos la cabecera
head(tabla)
# Eliminamos la 1 º columna por ser identificador
tabla<-tabla[,-1]

# Ecuacion de cifrado  s<-3*w+5
# Inicializo los nuevos nombrtes a un vector vacio
nombres<-c()

j<-1
# Recorremos todos los verdaderos nombres de la cabecera
while(j<=length(names(tabla))){
  # Convertir a mayusculas
  z<-toupper(names(tabla)[[j]])
  # Separa en cartacteres
  x<-strsplit(z,split="")
  
  # Comenzamos la codificacion de un nombre
  cod<-c()
  i<-1
  while(i<=length(x[[1]])){
    # Buscamos la posicion de cada letra del nombrfe en el Alfabeto
   w<-which(A==x[[1]][[i]])
   # Codificamos
   s<-(3*w+5)%% length(A)
   s<-s+1
   #Añadimos el caracter codificado
   cod<-append(cod,A[[s]])
   i<-i+1}

   # Pegamos las letras para formar un nuevo nombre
   C<-paste(cod,collapse="")
   # Lo añadimos a la lista de nombres
   nombres<-append(nombres,C)
   j<-j+1}

# Sustituir los nombres reales de la cabecera por sus nombres
#codificados
nombres
names(tabla) 
j<-1
while(j<=length(nombres)){
  names(tabla)[[j]]<-nombres[[j]]
  j<-j+1}
# Comprobar que los nombres han sido cambiados
head(tabla)

# Generamos una permutacion de 6 elementos
Numeros<-seq(1,6,1)
Numeros
P<-sample(Numeros,6,replace=FALSE)
#Hacemos un copia del data.frame
tabla1<-tabla
# Cada columna de tabla1 se le asocia la columna de la permutacion
k<-1
while(k<=dim(tabla)[[2]]){
  tabla1[,k]<-tabla[,P[[k]]]
   k<-k+1}

tabla<-tabla1
head(tabla)

# Preparación para el ruido
# Dicotomizamos OPIKU

Zeros<-which(tabla[,2]=="No")
Unos<-which(tabla[,2]=="Si")
tabla[Zeros,2]<-0
tabla[Unos,2]<-1
head(tabla)
# Dicotomizamos BHUOGY
Male<-which(tabla[,6]=="male")
Female<-which(tabla[,6]=="female")
tabla[Male,6]<-0
tabla[Female,6]<-1
head(tabla)

# Introducimos ruido Uniforme(0,10) en 1 º columna

ruido1<-runif(dim(tabla)[[1]],0,10)
tabla[,1]<-tabla[,1]+ruido1
head(tabla)

# Proceso de reconstruccion
# Eliminacion del ruido en la 1 º columna
tabla[,1]<-tabla[,1]-ruido1
head(tabla)
# En la 2 º columna cambiamos 0<-No 1<-Si
No<-which(tabla[,2]==0)
Si<-which(tabla[,2]==1)
tabla[No,2]<-"No"
tabla[Si,2]<-"Si"
head(tabla)

# En la 6 º columna cambiamos 0<-male 1<-female
male<-which(tabla[,6]==0)
female<-which(tabla[,6]==1)
tabla[male,6]<-"male"
tabla[female,6]<-"female"
head(tabla)

# Ejecucion de la permutacion inversa

tabla1<-tabla
# Cada columna de tabla1 se le asocia la columna de la permutacion
k<-1
while(k<=dim(tabla)[[2]]){
  tabla1[,P[[k]]]<-tabla[,k]
   k<-k+1}
tabla<-tabla1
head(tabla)

# Calculo de la ecuacion inversa

#  s<-3*w+5
# w<-(s-5)*inv(3)

# Necesito inv(3)

d<-1
while((d*3)%%(length(A))!=1){
   d<-d+1}

#w<-(s-5)*9
names(tabla)

# Decodificacion de las cabeceras

nombres<-c()

j<-1
# Recorremos todos los verdaderos nombres de la cabecera
while(j<=length(names(tabla))){
    # Separa en cartacteres
     x<-strsplit(names(tabla)[[j]],split="")
  
  # Comenzamos la codificacion de un nombre
    cod<-c()
    i<-1
  while(i<=length(x[[1]])){
    # Buscamos la posicion de cada letra del nombrfe en el Alfabeto
   w<-which(A==x[[1]][[i]])
   # Codificamos
   w<-w-1
   s<-(9*(w-5))%% length(A)
   
   #Añadimos el caracter codificado
   cod<-append(cod,A[[s]])
   i<-i+1}

   # Pegamos las letras para formar un nuevo nombre
   C<-paste(cod,collapse="")
   # Lo añadimos a la lista de nombres
   nombres<-append(nombres,C)
   j<-j+1}

# Sustituir los nombres reales de la cabecera por sus nombres
#codificados
nombres
names(tabla) 
j<-1
while(j<=length(nombres)){
  names(tabla)[[j]]<-nombres[[j]]
  j<-j+1}
# Comprobar que los nombres han sido cambiados
head(tabla)




A[[s]]
names(tabla)








   

















