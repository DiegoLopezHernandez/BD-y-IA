#Ejercicio. Modelo de regresion para pronosticar la longitud del petalo
#a traves de las variables longitud tallo, anchura tallo y anchura petalo
dim(iris)
head(iris)

#Normal
modelo<-lm(Petal.Length~Sepal.Width+Sepal.Length+Petal.Width,data = iris)
summary(modelo)

#Logaritmica
modelolog<-lm(log(Petal.Length)~Sepal.Width+Sepal.Length+Petal.Width,data = iris)
summary(modelolog)

#Doble Logaritmica
modelolog2<-lm(log(Sepal.Length)~log(Sepal.Width)+log(Sepal.Length)+log(Petal.Width),data = iris)
summary(modelolog2)
