Titanic<-read.choose()

#Nombre de la cabecera
names(Titanic)

#Visualizamos las 6 primeras filas
head(Titanic)

#Dimensiones del data.frame
dim(Titanic)
x<-seq(2,7,1)
Titanic<-Titanic[,x]
head(Titanic)

#Creas el ruido que son tantos numeros como dim tiene el titanic(1 son las dim de filas)
#y es del 10 al 100 aleatorios para sumarlos y encriptarlo
RuidoPrecio<-runif(dim(Titanic)[[1]],10,100)

#Añades el ruido al PRECIO
Titanic$Precio<-Titanic$Precio+RuidoPrecio

#Recuperas Informacion del precio real
Titanic$Precio<-Titanic$Precio-RuidoPrecio

#Añadimos ruido a la edad
#Este ruido sera cerca de 20 con una variabilidad de 5 por encima o por debajo(Aunque puede ser más)
RuidoEdad<-rnorm(dim(Titanic)[[1]],20,5)
RuidoEdad
Titanic$Edad<-Titanic$Edad+RuidoEdad
Titanic$Edad<-Titanic$Edad-RuidoEdad

#Tratamiento del atributo survived

#Buscamos las filas donde dicho atributo es no
No<-which(Titanic$survived=="NO")
NO

#Cambiar SI=1 y NO=0
S<-rep(1,dim(Titanic)[[1]])
S[NO]<-0
S

Titanic&survived<-S
Titanic&survived

#Ruido Binomial o Poisson
RuidoSurvived<-rpois(Titanic)[[1]],5)

#Añadimos el ruido
Titanic$survived<-Titanic$survived+RuidoSurvived

#Quitamos el ruido
Titanic$survived<-Titanic$survived-RuidoSurvived

#Como queda el data.frame hasta ahora
head(Titanic)

#Tratamiento de NA (Valores nulos) para la reconstruccion de un archivo
#Creamos lista con NAs Varios
x<-seq(1,100,1)
x<-append(x,rep(NA,20))
x<-append(x,seq(1,20,1))
x

#Omision de los NA
x<-na.omit(x)

#Una alternativa es sustituir los NA por la media en variables cuantitativas como esta

#Buscamos los datos NA
N<-which(is.na(x)==TRUE)
N

#Calculamos media de datos que no son NA
media<-mean(x[-N])
media
x[N]<-media
x





