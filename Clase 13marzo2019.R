a1<-window(ausair, start = 1990)
pronostico<-holt(ausair,h=5)
plot(pronostico)


#EJEMPLO 
prono1<-holt(a1, h=15)
prono2<-holt(a1, damped = TRUE, phi =0.9, h=15)
autoplot(a1)+
  autolayer(prono1, series = "Holt", PI = FALSE)+
  autolayer(prono2, series = "Amortiguado Holt", PI = FALSE)+
  ggtitle("pronosticos")+xlab("Año")+
  ylab("pasajeros")+
  guides(colour=guide_legend(title = "Prono"))


#Con SES
prono1<-holt(a1, h=15)
prono2<-holt(a1, damped = TRUE, phi =0.9, h=15)
prono3<-ses(a1, alpha = 0.9, initial = "simple", h=15)
autoplot(a1)+
  autolayer(prono1, series = "Holt", PI = FALSE)+
  autolayer(prono2, series = "Amortiguado Holt", PI = FALSE)+
  autolayer(prono3, series = "SES", PI = FALSE)+
  ggtitle("pronosticos")+xlab("Año")+
  ylab("pasajeros")+
  guides(colour=guide_legend(title = "Prono"))


#ENCONTRAR EL MAE, MAPE, MSE sin recurrir a accuracy 
e1 <-tsCV(a1, ses, h =1)   #(REGRESE LOS ERRORES DE PRONOSTICO)
e2<-tsCV(a1, holt, h =1)
e3<-tsCV(a1, holt, damped = TRUE, h =1)
e1
e2
e3

#MSE
mean(e1^2, na.rm = TRUE) #es para SES
mean(e2^2, na.rm = TRUE) #MODELO HOLT^(nos quedamos co holt, es el mas cercano a cero)
mean(e3^2, na.rm = TRUE) #Amortiguado
#na.rm elimina NA, se le pone true para que se pueda calcular la media, si quiero que remueva los NA

#MAE
mean(abs(e1), na.rm = TRUE)
mean(abs(e2), na.rm = TRUE)
mean(abs(e3), na.rm = TRUE)
#Nos quedamos con el Holt jajajaja


#EJERCICIO PIB 1980-2017
PIB<-read.csv("C:/Users/Elisa/Documents/8vo semestre/PIB 80-17.csv", header = FALSE)
PIBts<-ts(PIB, start = 1980, end = 2017)

P1<-holt(PIBts, h=15)
P2<-holt(PIBts, damped = TRUE, phi =0.9, h=15)
P3<-ses(PIBts, alpha = 0.9, initial = "simple", h=15)
autoplot(PIBts)+
  autolayer(P1, series = "Holt", PI = FALSE)+
  autolayer(P2, series = "Amortiguado Holt", PI = FALSE)+
  autolayer(P3, series = "SES", PI = FALSE)+
  ggtitle("Pronósticos")+xlab("Años")+
  ylab("%crecimiento")+
  guides(colour=guide_legend(title = "Métodos de pronóstico"))

E1 <-tsCV(PIBts, ses, h =1)   #(REGRESE LOS ERRORES DE PRONOSTICO)
E2<-tsCV(PIBts, holt, h =1)
E3<-tsCV(PIBts, holt, damped = TRUE, h =1)
E1
E2
E3

#MSE
mean(E1^2, na.rm = TRUE) 
mean(E2^2, na.rm = TRUE) 
mean(E3^2, na.rm = TRUE) 

#MAE
mean(abs(E1), na.rm = TRUE)
mean(abs(E2), na.rm = TRUE)
mean(abs(E3), na.rm = TRUE)

