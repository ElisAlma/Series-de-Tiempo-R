BIMBO<-read.csv("C:/Users/Elisa/Documents/8vo semestre/Series de Tiempo/BIMBO.csv")
BIMBOts<-ts(BIMBO, start=2018, frequency = 254)
plot(BIMBOts, xlab = "Tiempo", ylab = "Precio de cierre", main = "Precios de cierre de BIMBO")

#En apariencia se trata de una ST estacionaria, no hay un patr�n predecible a largo plazo.

adf.test(BIMBOts)

#Se aplic� la prueba Dickey-Fuller, su valor calculado = -2.3696 y p-valor = 0.4202. Se trata de 
#una serie no estacionaria. Se busca que los datos sean estacionarios, se hace la primera
#diferenciaci�n:

D1Bimbo<-diff(BIMBOts)

adf.test(D1Bimbo)

#Se aplic� or segunda vez la prueba D-F con valor calculado = -5.5394 y p-valor = 0.01. Ya es 
#estacionaria la serie

#An�lisis de las gr�ficas ACF y PACF a la primera diferenciaciaci�n:
ACF<-acf(D1Bimbo, main = "Funci�n de Autocorrelaci�n")
PACF<-pacf(D1Bimbo, main = "Funcion de Autocorrelaci�n Parcial")

#Puede ajustarse un ARIMA(0,d,q) de acuerdo a lo visto en las gr�ficas. Puede tratarse de un
#ARIMA(0,1,1) porque en el retraso q=1 en en ACF hay un aumento significativo. Se intentar� con
#la funci�n auto.arima para encontrar los valores de p,d,q.

auto.arima(BIMBOts)

#Se propone ARIMA(0,1,1)

Mod1<-arima(BIMBOts, c(0,1,1))

#Se busca que los residuos sean ruido blanco
plot(Mod1$residuals, main = "Residuales del Modelo 1", xlab = "Tiempo", ylab = "Residuales")
acf(Mod1$residuals, main= "ACF de los residuales del Modelo 1", xlab= "Tiempo", ylab="Autocorrelaci�n")
pacf(Mod1$residuals, main= "PACF de los residuales del Modelo 1", xlab= "Tiempo", ylab="Autocorrelaci�n")
Box.test(Mod1$residuals, lag = 12, type = "Ljung-Box")
#Los resultados de prueba son: X-cuadrada=15.336 y p-valor=0.2236; se sugiere que hay ruido blanco.

forecast(Mod1, h=12)
plot(forecast(Mod1, h=12), include=18)
shapiro.test(Mod1$residuals)
#Los resultados de la prueba son: valor calculado = 0.98317 y p-valor=0.004265; los residuos tienen una
#distribuci�n normal.