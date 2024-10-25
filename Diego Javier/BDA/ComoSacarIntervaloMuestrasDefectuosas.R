combinatorio<-function(a,b){
  f<-factorial(a)/(factorial(b)*factorial(a-b))
  return(f)}

F<-function(N,D,n,r){
  x<-combinatorio(D,r)*combinatorio(N-D,n-r)/combinatorio(N,n)
  return(x)
}
#Se desea conocer el numero de ejemplares de una especie
#Para ello se extraen y se marcan
D<-5
#Se deposita con el resto y se extraen
n<-10
#y marcados hay
r<-1
#Estimar N siendo que hemos recogido 10 5 las habiamos marcado y solo una esta marcada
#Asi que 9+5 marcadas mas las de la segunda extraccion
N<-14
Siguiente<-15
vector<-c(F(N,D,n,r))
while (F(N,D,n,r)<=F(Siguiente,D,n,r)) {
  N<-Siguiente
  Siguiente<-Siguiente+1
  vector<-append(vector,F(N,D,n,r))
}
plot(vector)


#-------------------------------------------------------------------------------
#Se desea saber el numero de piezas defectuosas en una produccion

#Tenemos 30 piezas
N<-30

#Desconocemos D=Numero de defectuosas
#Se extraen 10
n<-10

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
