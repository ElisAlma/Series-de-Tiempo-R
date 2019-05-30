#**************************Salario mínimo*****************************************
salario<-read.csv("C:/Users/Elisa/Documents/8vo semestre/Series de Tiempo/SMínimo.csv")
salariots<-ts(salario, start = 1982)
autoplot(salariots)

adf.test(salariots)
#Augmented Dickey-Fuller Test
#H0: No estacionario   H1: Estacionario
#data:  salariots
#Dickey-Fuller = -2.8639, Lag order = 3, p-value = 0.2389
#alternative hypothesis: stationary
#Se acepta H0, es no estacionario

dif1<-diff(salariots)
dif2<-diff(dif1)
dif3<-diff(dif2)

adf.test(dif3)
#data:  dif3
#Dickey-Fuller = -4.8485, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
#Se rechaza H0, es estacionario
#Entonces p=3

ggAcf(dif3) #q=0 por tener un pico en el primer retraso y despues se desvanece abruptamente.
ggPacf(dif3) #p=0 por el comportamiento sinuoso.

Mod1<-Arima(salariots, order=c(0,3,0)) #AICc=613.58
Mod2<-Arima(salariots, order=c(1,3,0)) #AICc=603.55
Mod3<-Arima(salariots, order=c(1,3,1)) #AICc=585.11 

Box.test(residuals(Mod1)) #p-value = 0.0007279
Box.test(residuals(Mod2)) #p-value = 0.05903
Box.test(residuals(Mod3)) #p-value = 0.2497

pronostico<-forecast(Mod3, h=12) #include es para observar solo cierta cantidad de datos.
autoplot(pronostico)

#H0: ruido blanco   H1: No ruido blanco
#Apartir de lo anterior, el modelo elegido: Mod3 con Arima(1.3,1)

#Con autoarima:
auto.arima(salariots)
Box.test(residuals(auto.arima(salariots)))
autoplot(pronos<-forecast(auto.arima(salariots), h=12))
#Arima(0,1,0) con  AICc=596.03 y p-value = 0.9304

#Nos quedamos con Arima(1,3,1)







