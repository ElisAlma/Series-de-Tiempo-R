#Ejemplo de la clase 25 de febrero de 2019
pronos<-naive(goog200)
names(pronos)
res1<-residuals(naive(goog200)) #M�todo del ingenuo, no se te olvide.
autoplot(goog200)
autoplot(res1) #paso 1, graficar los residuales
gghistogram(res1)+ggtitle("Histograma de residuales")
#No se cumple la normalidad en los errores, se parece m�s a la F, 
#verificado de forma constante, este es el paso 2, la cola derecha parece 
#demasiado larga, incluso cuando se ignora el valor atipico.
#procede a hacerse el analisis de autocorrelaci�n:
#se busca la no correlaci�n
ggAcf(res1) #Grafica de autocorrelograma
#las fronteras azules ayudan a ver si alguno de los residuos est�n 
#significativamente autocorrelacionados, si pasa esa frontera el residuo 
#est� autocorrelacionado, si lo est� significa que se puede mejorar el 
#modelo, las fronteras se basan en el 90-95% confianza determinadas a trav�s de
#pruebas de hip�tesis.
#Si la ST tiene media cero y varianza constante se trata de una serie con ruido blanco, 
#si esto ocurre el pron�stico es preciso. 

#Poblaci�n Mexicana (1960-2017), con datos anuales.
#Se toma el m�todo de la deriva para pronosticar.
Poblaci�n<-read.csv("C:/Users/Elisa/Documents/8vo semestre/POBM�X.csv")
Poblaci�nts<-ts(Poblaci�n, start = 1960)
mderivA<-rwf(Poblaci�nts, h=10, drift = TRUE)
names(mderivA)
residuales<-residuals(rwf(Poblaci�nts, h=10, drift = TRUE))
plot(Poblaci�nts, xlab = "A�os", ylab = "Poblaci�n", main = "Evoluci�n de la poblaci�n mexicana")
#Gr�ficas del an�lisis residual
autoplot(residuales, xlab = "A�os", ylab = "Residuales", main = "Residuales de evoluci�n de la poblaci�n mexicana")
gghistogram(residuales)+ggtitle("Histograma de residuales de la poblaci�n mexicana")
ggAcf(residuales, xlab= "Desfase", ylab="Autocorrelaci�n")+ggtitle("Gr�fica de autocorrelaci�n de la poblaci�n mexicana")
#Interpretaci�n
#La evoluci�n de la poblaci�n mexicana muestra una clara tendencia lineal al alza, sin aparente
#estacionalidad y ligera ciclicidad. No cumple con la normalidad de los errores, el tercer 
#supuesto del an�lisis residual, se observa que los datos son asim�tricos
#hacia la derecha y existe un valor at�pico cerca de 2 millones; no se debe ignorar que a 
#veces el histograma depende del n�mero de intervalos usados para agrupar los datos, si se 
#requiere precisi�n se sugiere usar la gr�fica de la probabilidad normal, misma que debe seguir
#una l�nea recta para probar la normalidad. Respecto a la autocorrelaci�n, 
#existe un pico grande que va disminuyendo de forma gradual,lo que a su vez significa 
#que el patr�n indica un t�rmino autorregresivo; asimismo, se intuye que el modelo se puede
#mejorar. Finalmente, la gr�fica de los residuales se utiliza para determinar si
#los patrones pueden sugerir si el modelo no se ajusta a los datos; en este caso, cerca de 1990
#est� demaiado alejado de los dem�s "puntos", y se puede pensar que existe un valor atipico.


#Precio de cierre de las acciones de televisa durante el �ltimo a�o burs�til, datos 
#presentados de forma diaria.
#Se toma el m�todo del ingenuo para pronosticar.
Televisa<-read.csv("C:/Users/Elisa/Documents/8vo semestre/Televisa.csv")
Televisats<-ts(Televisa, start = 2018, end = 2019, frequency = 252)
mingenuO<-naive(Televisats, h=10)
names(mingenuO)
residualeS<-residuals(naive(Televisats, h=10))
plot(Televisats, xlab = "A�os", ylab= "Precio de cierre", main= "Precios de cierre de Televisa")
#Gr�ficas del an�lisis residual
autoplot(residualeS, xlab= "Periodos", ylab = "Residuales", main = "Residuales del precio de cierre de Televisa")
gghistogram(residualeS)+ggtitle("Histograma de residuales del precio de cierre de Televisa")
ggAcf(residualeS, xlab= "Desfase", ylab="Autocorrelaci�n")+ggtitle("Gr�fica de autocorrelaci�n del precio de cierre de televisa")
#Interpretaci�n
#Los precios de cierre de las acciones de Televisa tiene una tendencia lineal en aparencia a la
#baja sin ciclicidad y "poca" estacionalidad, puede pensarse que tiene mucha volatilidad. Tampoco se
#cumple la normalidad en los errores, esta vez los datos muestran asimetria a la izquierda. En cuanto
#a la autocorrelaci�n, se observa ondas crecientes y decrecientes que alternan correlaciones
#positivas y negativas; adem�s se nota que ninguna sobrepasa la frontera azul.