pronos<-naive(goog200)
names(pronos)
res1<-residuals(naive(goog200)) #M�todo del ingenuo, no se te olvide.
autoplot(goog200)
autoplot(res1) #paso 1, gregicar los residuales

gghistogram(res1)+ggtitle("Histograma de residuales")
#No se cumple la normalidad en los errores, se parece m�s a la F, 
#verificado de forma constante, este es el paso 2, la cola derecha parece 
#demasiado larga, inclusos cuando se ignora el valor atipico.

#procede a hacerse el analisis de autocorrelaci�n
#se busca la no correlaci�n
ggAcf(res1) #Grafica de autocorrelograma
#las fronteras azules ayudan a ver si alguno de los residuos estan 
#significativamente autocorrelacionados, si pasa esa forntera el residuo 
#est� autocorrelacionado, si lo est� significa que se puede mejorar el
#modelo, las fronteras se basan en el 90-95% confianza determinadas a trav�s 
#pruebas de hip�tesis.
#Si la ST tiene media cero y varianza constante se trata de una serie con ruido blanco, 
#si esto ocurre el pronostico es preciso. 


