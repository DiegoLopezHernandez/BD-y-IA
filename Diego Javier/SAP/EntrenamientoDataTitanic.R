library(e1071)

#Bayesiano ingenuo para el Titanic
Titanic<-read.table(file.choose(), header=TRUE,sep = ",")
head(Titanic)
male<-which(Titanic$Sexo=="male")
female<-which(Titanic$Sexo=="female")
Titanic$Sexo[male]<-0
Titanic$Sexo[female]<-1
Titanic$Sexo<-as.numeric(Titanic$Sexo)
is.numeric(Titanic$Sexo)
head(Titanic)
x<-seq(2,5,1)
Titanic<-Titanic[,x]

dim(Titanic)
Entrenamiento<-dim(Titanic)[[1]]*2/3
Entrenamiento
muestra<-sample(1:891,Entrenamiento,replace=FALSE)
muestra

#Se eligen 2/3 partes del dataset para entrenar
train<-Titanic[muestra,]

#Construimos el conjunto de evaluacion
eval<-Titanic[-muestra,]

#Modelo
nbClass<-naiveBayes(Survived~Clase+Sexo+Edad,data=train)
predicted<-predict(nbClass, eval[,-1])
predicted

#Comparamos pronostico con realidad
frame<-data.frame(Pronostico=predicted,Realidad=eval[,1])
frame

matrizconf<-table(predicted,eval[,1])
matrizconf
T<-sum(diag(matrizconf))/sum(matrizconf)
T
