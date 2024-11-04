#Bayesiano Ingenuo. Algoritmo de clasificacion supervisada.
install.packages("e1071",dependencies = T)
library(e1071)

#Cargamos el dataset iris.
data(iris)

#Nombres de los atributos.
names(iris)

#Dimension del dataset.
dim(iris)

#Cabecera del data.
head(iris)

#Contruimos el conjunto de entrenamiento.
muestra<-sample(1:150,100,replace=FALSE)
muestra

#Se eligen 2/3 partes del dataset para entrenar
train<-iris[muestra,]

#Construimos el conjunto de evaluacion
eval<-iris[-muestra,]

#Modelo
nbClass<-naiveBayes(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=train)
predicted<-predict(nbClass, eval[,-5])
predicted

#Comparamos pronostico con realidad
frame<-data.frame(Pronostico=predicted,Realidad=eval[,5])
frame

matrizconf<-table(predicted,eval[,5])
matrizconf
T<-46/50
T
