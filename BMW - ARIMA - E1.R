BMW<-read.csv("file:///C:/Users/Elisa/Documents/8vo semestre/BMW.csv")
BMWts<-ts(BMW, start = 2018,frequency=47)
BMW

#El primer paso en la identificación del modelo es determinar si la serie es estacionaria, 
#es decir, si la serie de tiempo parece variar alrededor de un nivel fijo. Es útil observar 
#una gráfica de la serie junto con la función de autocorrelación de la muestra.Se examina el
#comportamiento de la serie a través del tiempo mediante un gráfico.

plot(BMWts, xlab = "Tiempo", ylab = "Precio de cierre", main = "Precios de cierre de BMWM5N")

#Aparece el gráfico de la serie original, la cual muestra un comportamiento decreciente,
#indicando con esto la no presencia de estacionariedad. A l mismo tiempo parece ser 
#heterocedastica. El siguiente paso en la identificación de un modelo tentativo es examinar 
#las autocorrelaciones de muestra de datos.

ACF<-acf(BMWts, main = "Función de Autocorrelación")
pACF<-pacf(BMWts, main = "Funcion de Autocorrelación Parcial")

#La gráfica se desvanece lento, mostrando estacionariedad.Se procede a hacer la prueba
#Dickey - Fuller. Se necesita la paquetería fUnitRoots; la prueba de hipótesis es:
    #Ho: La serie es no estacionaria: Tiene raíz unitaria.
    #H1: La serie es estacionaria: No Tiene raíz unitaria.
adf.test(BMWts)

#El valor del estadístico es -3.185 y tiene p-valor = 0.0999 por lo que Ho se acepta y tiene
#raíz unitaria. Además, la serie de tiempo no es estacionaria.
#Se procede a tomar la primera diferencia a la serie.

D1BMWts<-diff(BMWts)
adf.test(D1BMWts)
acf(D1BMWts, main = "Función de Autocorrelación 1° diferencia")
pacf(D1BMWts, main = "Función de Autocorrelación Parcial 1° diferencia")

#Aplicando la prueba Dickey - Fuller a la primera diferencia, el resultado es que se mantiene 
#la no estacionariedad.

D2BMWts<-diff(D1BMWts)
adf.test(D2BMWts)
acf(D2BMWts, main = "Función de Autocorrelación 2° diferencia")
pacf(D2BMWts, main = "Función de Autocorrelación Parcial 2° diferencia")

#Para la segunda diferencia el valor del estadístico es -4.5251 y tiene p-valor = 0.01 por lo
#que Ho se rechaza y no tiene raíz unitaria. Además, la serie es estacionaria.

#Una vez que se ha obtenido una serie estacionaria, debemos identificar la forma del modelo que 
#se utilizará. La identificación de la forma del modelo se lleva a cabo comparando las 
#autocorrelaciones y las autocorrelaciones parciales calculadas con los datos de la autocorrelaciones
#y las autocorrelaciones parciales teóricas de los diferentes modelos ARIMA.
#Se propone el modelo ARIMA(p=6, d=2, q=4) y en general:

Mod1<-Arima(BMWts, c(6,2,4))
Mod2<-Arima(BMWts, c(5,2,4))
Mod3<-Arima(BMWts, c(1,2,4))
Mod4<-Arima(BMWts, c(3,2,4))
Mod5<-Arima(BMWts, c(6,2,1))

Mod1
Mod2
Mod3
Mod4
Mod5

#Se elige el modelo 4, ie, ARIMA(3,2,4)
#Se procede a hacer la validación del modelo a través del análisis de los residuales, se busca que 
#éstos sean ruido blanco. Se hará a través de correlogramas:
plot(Mod4$residuals, main = "Residuales del Modelo 4", xlab = "Tiempo", ylab = "Residuales")
acf(Mod4$residuals, main= "Autocorrelaciones de los residuales del Modelo 4", xlab= "Tiempo", ylab="Autocorrelación")
pacf(Mod4$residuals, main= "Autocorrelaciones de los residuales del Modelo 4", xlab= "Tiempo", ylab="Autocorrelación")
Box.test(Mod4$residuals, lag = 12, type = "Ljung-Box")

forecast(Mod4, h=12)
plot(forecast(Mod4, h=12), include=18)
shapiro.test(Mod4$residuals)

#Hay ruido blanco, se acepta el modelo.
