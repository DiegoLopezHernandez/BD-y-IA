x<-rnorm(1000,6,1)
#Suponer que x es una muestra del gasto de combustible de una marca /100km
#Se desea contrastar si su gasto medio es de 5L/100km 
x

#Planteamiento del test
#H0: media<=5
#H1: media>5
#Se trata de un test unilateral izquierdo
#alternative es H1, greater es porque tiene que ser mas grande
#conf.level es la confianza que tiene que tener de normal 95% 

t.test(x,mu=5,alternative = "greater",conf.level = 0.95)

#Al ser p-value < 2.2e-16 menor que 0,05 es H1.
#p-value < 2.2e-16<0.05=>H1=>media>5
#¿Cuanto vale la media?
#6.017107 esta es la estimada con las 1000 pruebas que tenemos

#-------------------------------------------------------------------------------
#Se extrae una prueba de unas bombillas en duracion por años
x<-rexp(1000,2)

#El fabricante dice que las bombillas duran 3 años de media
#H0: media>=3
#H1: media <3

#Alternative less porque H1 estaria en la izquierda
t.test(x,mu=3,alternative = "less",conf.level = 0.95)

#p-value < 2.2e-16 < 0.05 => H1 => media<3
#Media estimada = 0.4897964

#-------------------------------------------------------------------------------
#Test para dos muestras
#Se extraen dos muestras sobre el numero de km que se recorren con un L 
#de combustible en dos marcas distintas

muestra1<-rnorm(1500,20,1) 
muestra2<-rnorm(1500,18,2)

#Discutir si de media no hay diferencia entre las marcas
#H0: media1=media2
#H1: media1<>media2
t.test(muestra1,muestra2,alternative = "two.sided",conf.level = 0.95)

#p-value < 2.2e-16 < 0.05 => H1 => media1<> media2
#Media1 estimada 20.00311
#Media2 estimada 17.97809 

#Comprobar que media1 es mas grande que media2
#H0: media1<=media2
#H1: media1 > media2

t.test(muestra1,muestra2,alternative = "greater",conf.level = 0.95)

#p-value < 2.2e-16 < 0.05 => Acepto H1 => media1>media2

#<------------------------------------------------------------------------------

