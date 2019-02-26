#Ejemplo de la clase 25 de febrero de 2019
pronos<-naive(goog200)
names(pronos)
res1<-residuals(naive(goog200)) #Método del ingenuo, no se te olvide.
autoplot(goog200)
autoplot(res1) #paso 1, graficar los residuales
gghistogram(res1)+ggtitle("Histograma de residuales")
#No se cumple la normalidad en los errores, se parece más a la F, 
#verificado de forma constante, este es el paso 2, la cola derecha parece 
#demasiado larga, incluso cuando se ignora el valor atipico.
#procede a hacerse el analisis de autocorrelación:
#se busca la no correlación
ggAcf(res1) #Grafica de autocorrelograma
#las fronteras azules ayudan a ver si alguno de los residuos están 
#significativamente autocorrelacionados, si pasa esa frontera el residuo 
#está autocorrelacionado, si lo está significa que se puede mejorar el 
#modelo, las fronteras se basan en el 90-95% confianza determinadas a través de
#pruebas de hipótesis.
#Si la ST tiene media cero y varianza constante se trata de una serie con ruido blanco, 
#si esto ocurre el pronóstico es preciso. 

#Población Mexicana (1960-2017), con datos anuales.
#Se toma el método de la deriva para pronosticar.
Población<-read.csv("C:/Users/Elisa/Documents/8vo semestre/POBMÉX.csv")
Poblaciónts<-ts(Población, start = 1960)
mderivA<-rwf(Poblaciónts, h=10, drift = TRUE)
names(mderivA)
residuales<-residuals(rwf(Poblaciónts, h=10, drift = TRUE))
plot(Poblaciónts, xlab = "Años", ylab = "Población", main = "Evolución de la población mexicana")
#Gráficas del análisis residual
autoplot(residuales, xlab = "Años", ylab = "Residuales", main = "Residuales de evolución de la población mexicana")
gghistogram(residuales)+ggtitle("Histograma de residuales de la población mexicana")
ggAcf(residuales, xlab= "Desfase", ylab="Autocorrelación")+ggtitle("Gráfica de autocorrelación de la población mexicana")
#Interpretación
#La evolución de la población mexicana muestra una clara tendencia lineal al alza, sin aparente
#estacionalidad y ligera ciclicidad. No cumple con la normalidad de los errores, el tercer 
#supuesto del análisis residual, se observa que los datos son asimétricos
#hacia la derecha y existe un valor atípico cerca de 2 millones; no se debe ignorar que a 
#veces el histograma depende del número de intervalos usados para agrupar los datos, si se 
#requiere precisión se sugiere usar la gráfica de la probabilidad normal, misma que debe seguir
#una línea recta para probar la normalidad. Respecto a la autocorrelación, 
#existe un pico grande que va disminuyendo de forma gradual,lo que a su vez significa 
#que el patrón indica un término autorregresivo; asimismo, se intuye que el modelo se puede
#mejorar. Finalmente, la gráfica de los residuales se utiliza para determinar si
#los patrones pueden sugerir si el modelo no se ajusta a los datos; en este caso, cerca de 1990
#está demaiado alejado de los demás "puntos", y se puede pensar que existe un valor atipico.


#Precio de cierre de las acciones de televisa durante el último año bursátil, datos 
#presentados de forma diaria.
#Se toma el método del ingenuo para pronosticar.
Televisa<-read.csv("C:/Users/Elisa/Documents/8vo semestre/Televisa.csv")
Televisats<-ts(Televisa, start = 2018, end = 2019, frequency = 252)
mingenuO<-naive(Televisats, h=10)
names(mingenuO)
residualeS<-residuals(naive(Televisats, h=10))
plot(Televisats, xlab = "Años", ylab= "Precio de cierre", main= "Precios de cierre de Televisa")
#Gráficas del análisis residual
autoplot(residualeS, xlab= "Periodos", ylab = "Residuales", main = "Residuales del precio de cierre de Televisa")
gghistogram(residualeS)+ggtitle("Histograma de residuales del precio de cierre de Televisa")
ggAcf(residualeS, xlab= "Desfase", ylab="Autocorrelación")+ggtitle("Gráfica de autocorrelación del precio de cierre de televisa")
#Interpretación
#Los precios de cierre de las acciones de Televisa tiene una tendencia lineal en aparencia a la
#baja sin ciclicidad y "poca" estacionalidad, puede pensarse que tiene mucha volatilidad. Tampoco se
#cumple la normalidad en los errores, esta vez los datos muestran asimetria a la izquierda. En cuanto
#a la autocorrelación, se observa ondas crecientes y decrecientes que alternan correlaciones
#positivas y negativas; además se nota que ninguna sobrepasa la frontera azul.