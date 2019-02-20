##DESCOMPONER UNA SERIE DE TIEMPO
america<-read.csv("C:/Users/ALUMNO-D27/Desktop/america movil.csv")
cemex<-read.csv("C:/Users/ALUMNO-D27/Desktop/cemex.csv")
tamerica<-ts(america, start = 2017, frequency = 120)
tcemex<-ts(cemex, start = 2017, frequency = 120)
plot(tamerica)
deame<-decompose(tamerica)
amesesta<-tamerica-deame$seasonal
plot(amesesta)


##CORRELACIÒN Y COVARIANZA A MANO
base<-read.csv("C://Users//sala-D19.RTIC-D19//Documents//Series//base.csv")
base
x<-base$X10.2..Tasa.de.desocupación
y<-base$X10.5..Tasa.de.trabajo.asalariado
n<-56

###Covarianza
#Medias
medx<-mean(x)
medy<-mean(y)
#Resta
xmenos<-x-medx
ymenos<-y-medy
##Cov
cova<-(sum(xmenos*ymenos))/(n-1)
cova
##Comprobacion
cov(x,y)

###Correlacion
corre<-cova/sqrt(var(x)*var(y))
corre
##Comprobacion
cor(x,y)
