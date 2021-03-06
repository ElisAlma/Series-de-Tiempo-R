FRAGUAB<-read.csv("C:/Users/Elisa/Documents/8vo semestre/FRAGUAB.csv")
FRAGUABts<-ts(FRAGUAB,start=2018, frequency = 63)
plot(FRAGUABts, xlab = "Tiempo", ylab = "Precio de cierre", main = "Precios de cierre de FRAGUAB")

ACF<-acf(FRAGUABts, main = "Funci�n de Autocorrelaci�n")
pACF<-pacf(FRAGUABts, main = "Funcion de Autocorrelaci�n Parcial")

#La gr�fica se desvanece lento, mostrando estacionariedad.Se procede a hacer la prueba
#Dickey - Fuller. Se necesita la paqueter�a fUnitRoots; la prueba de hip�tesis es:
#Ho: La serie es no estacionaria: Tiene ra�z unitaria.
#H1: La serie es estacionaria: No Tiene ra�z unitaria.
adf.test(FRAGUABts)

#El valor del estad�stico es -2.6092 y tiene p-valor = 0.3282 por lo que Ho no se acepta y no tiene
#ra�z unitaria. Adem�s, la serie de tiempo es estacionaria.
#Se procede a tomar la primera diferencia a la serie.

D1FRAGUABts<-diff(FRAGUABts)
adf.test(D1FRAGUABts)
acf(D1FRAGUABts, main = "Funci�n de Autocorrelaci�n 1� diferencia")
pacf(D1FRAGUABts, main = "Funci�n de Autocorrelaci�n Parcial 1� diferencia")

#Aplicando la prueba Dickey - Fuller a la primera diferencia, el resultado es que se mantiene 
#la estacionariedad.

#Una vez que se ha obtenido una serie estacionaria, debemos identificar la forma del modelo que 
#se utilizar�. La identificaci�n de la forma del modelo se lleva a cabo comparando las 
#autocorrelaciones y las autocorrelaciones parciales calculadas con los datos de la autocorrelaciones
#y las autocorrelaciones parciales te�ricas de los diferentes modelos ARIMA.
#Se propone el modelo ARIMA(p=2, d=1, q=1) y en general:

Mod1<-Arima(BMWts, c(2,1,1))
Mod2<-Arima(BMWts, c(5,1,1))
Mod3<-Arima(BMWts, c(3,1,3))

Mod1
Mod2
Mod3


#Se elige el modelo 3, ie, ARIMA(3,1,3)
#Se procede a hacer la validaci�n del modelo a trav�s del an�lisis de los residuales, se busca que 
#�stos sean ruido blanco. Se har� a trav�s de correlogramas:
plot(Mod3$residuals, main = "Residuales del Modelo 3", xlab = "Tiempo", ylab = "Residuales")
acf(Mod3$residuals, main= "Autocorrelaciones de los residuales del Modelo 3", xlab= "Tiempo", ylab="Autocorrelaci�n")
pacf(Mod3$residuals, main= "Autocorrelaciones de los residuales del Modelo 3", xlab= "Tiempo", ylab="Autocorrelaci�n")
Box.test(Mod3$residuals, lag = 12, type = "Ljung-Box")

forecast(Mod3, h=12)
plot(forecast(Mod3, h=12), include=18)
shapiro.test(Mod3$residuals)

#Hay ruido blanco, se acepta el modelo.

