#Test de hipotesis bilateral
#Extraemos una muestra

x<-rpois(500,3)

#Hipotesis

#Nula H0:media=4
#Alternativa H1:media<>4

t.test(x,mu=4,alternative="two.sided",conf.level=0.98)
#Estimacion puntual de la media:3.044

#Intervalo de confianza al 98%

#p-valor<0.02>=>Acepto H1

#------------------------------------------------------------------------------

t.test(x,mu=3,alternative="two.sided",conf.level=0.98)

