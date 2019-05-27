#*****************************ARIMA NO ESTACIONAL********************************

desempleo
plot(desempleo$unemp)
desempleots<-ts(desempleo$unemp, start = c(1997,6), frequency = 12)
autoplot(desempleots)
#pasa D-F significa que es estacional, no tiene que ver con el ejercicio, es nota adicional

adf.test(desempleots)

#H0: No estacionario   H1: Estacionario
#data:  desempleots
#Dickey-Fuller = -1.5963, Lag order = 4, p-value = 0.7439
#alternative hypothesis: stationary
#por lo tanto, es no estacionaria

dif1<-diff(desempleots)
dif2<-diff(dif1)

#cuando no tiene forma o es sinuoso, p y q son 0

adf.test(dif2)
#se sacaron dos diferencias porque tras la primera diferenciación todavia no se cumplia el
#criterio de estacionariedad. A continuación se muestra el resultado de la 2da diferenciación
#donde se pasa la prueba

#Augmented Dickey-Fuller Test
#data:  dif2
#Dickey-Fuller = -6.8611, Lag order = 4, p-value = 0.01
#alternative hypothesis: stationary

ggAcf(dif2)
ggPacf(dif2)
#El valor de q se propone con base en el ACF, existen valores significativos y se va perdiendo 
#a lo largo de la gráfica, entonces q=0. Para el valor de p, se observa un comportamiento
#gráfico similar, entonces p=0.

#Se proponen 3 modelos, elegir el que tenga el AICc más chico :v
mod1<-Arima(desempleots, order= c(0,2,0))  #AICc=1372.01
mod2<-Arima(desempleots, order= c(1,2,1))  #AICc=1296.39
mod3<-Arima(desempleots, order= c(1,2,0))  #AICc=1319.69 
mod4<-Arima(desempleots, order= c(0,2,1))  #AICc=1300.37

#Prueba box text. En esta prueba se busca rechazar la H0 y p-valor a 0.05
#H0: ruido blanco   H1: No ruido blanco
#Se busca que sea ruido blanco para que no exista tanta variabilidad con los datos y se pueda
#hacer mejores pronósticos.

Box.test(residuals(mod1)) #p-value = 8.542e-11
Box.test(residuals(mod2)) #p-value = 0.9367
Box.test(residuals(mod3)) #p-value = 0.02175
Box.test(residuals(mod4)) #p-value = 0.03471

#A partir de lo anterior, el modelo elegido es "Mod2" con Arima(1,2,1)
#Se procede a hacer un pronóstico
pronostico<-forecast(mod2, h=12, include= 50) #include es para observar solo cierta cantidad de datos.
autoplot(pronostico)

#Con auto.arima
AA<-auto.arima(desempleots)
Box.test(desempleots)
autoplot(pronos<-forecast(AA, h=12))

#Resultados
#Series: desempleots 
#ARIMA(0,1,0) propone un ingenuo 
#sigma^2 estimated as 32098:  log likelihood=-654.11
#AIC=1310.23   AICc=1310.27   BIC=1312.82

#Box-Pierce test
#data:  desempleots
#X-squared = 97.356, df = 1, p-value < 2.2e-16 

#Conclusión
#De forma grafica y por AICc se elige finalmente "Mod2", pues mostró mejores resultados.