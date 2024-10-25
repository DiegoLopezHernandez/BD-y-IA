#Encriptar cabezera.
#Permutar columnas.
#Hacer ruido columnas.
#Reconstruir archivo.
#Eliminar datos atipicos.
a<-7
b<-2
Alfabeto<-c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z')

inverso<-function(clave,n){
	i<-1
	while((i<n) && (i*clave) %% n!=1){
		i<-i+1
	}
	return(i)
}
codificar<-function(x,a,b,n){
	y<-(a*x+b)%%n
	return(y)
}

decodificar<-function(y,a,b,n){
	x<-(inverso(a,n)*(y-b))%%n
	return(x)
}

#Funcion encriptar un caracter

Encriptar<-function(a,b,Caracter,Alfabeto){
	n<-length(Alfabeto)
	x<-which(Alfabeto==Caracter)-1
	y<-codificar(x,a,b,n)
	y<-y+1
	return(Alfabeto[[y]])		
}

#Codificamos un string

Inicial<-c('D','I','E','G','O')
Final<-c()
i<-1
while(i<=length(Inicial)){
	D<-Encriptar(a,b,Inicial[[i]],Alfabeto)
	Final<-append(Final,D)
	i<-i+1
}
Final

#Funcion de descodificar

Desencriptar<-function(a,b,Caracter,Alfabeto){
	n<-length(Alfabeto)
	y<-which(Alfabeto==Caracter)-1
	x<-decodificar(y,a,b,n)
	x<-x+1
	return(Alfabeto[[x]])		
}
#Simulacion de encriptar y desencriptar un caracter
Dec<-c()
i<-1
while(i<=length(Final)){
	D<-Desencriptar(a,b,Final[[i]],Alfabeto)
	Dec<-append(Dec,D)
	i<-i+1
}
Dec