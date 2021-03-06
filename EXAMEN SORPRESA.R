#EXAMEN SORPRESA >:v
# 1) Calcular los residuales con un pron�stico ingenuo estacional aplicado a la serie ausbeer.

Pron�stico<-snaive((ausbeer))
Residuos<-residuals(snaive(ausbeer))
autoplot(ausbeer)
autoplot(Residuos)

# 1.1) Prueba si los residuales son ruido blanco y se distribuyen normal

gghistogram(Residuos)+ggtitle("Histograma de residuales")
ggAcf(Residuos, xlab= "Desfase", ylab="Autocorrelaci�n")+ggtitle("Gr�fica de autocorrelaci�n de Ausbeer")
#Interpretaci�n: No cumple con la normalidad de los errores, el tercer 
#supuesto del an�lisis residual, se observa que los datos son asim�tricos
#hacia la derecha y existe un valor at�pico, se debe tomar en cuenta que a 
#veces el histograma depende del n�mero de intervalos usados para agrupar los datos. 

#Para que una serie de tiempo sea ruido blanco se tiene que cumpir que la media de los errores
#sea cero, la varianza constante y no debe existir autocorrelaci�n; a partir del grafico se concluye que 
#no es ruido blanco porque traspasa las fronteras azules. Adem�s, todav�a se puede mejorar el modelo y al mismo
#tiempo se nota que no tiene media cero por el sesgo que tiene hacia la derecha.

#2) Calcular residuales para las series WWWusage y bricksq y seleccionar que pron�stico es m�s
   #apropiado: ingenuo o iestacional en cada serie. Argumenta la respuesta.
#WWWusage
Ingenuo1<-naive(WWWusage,20)
Res1w<-residuals(naive(WWWusage))
autoplot(Res1w) 
gghistogram(Res1w)+ggtitle("Histograma de Residuales WWWusage (Ingenuo)")
ggAcf(Res1w,xlab= "Desfase", ylab="Autocorrelaci�n")+ggtitle("Gr�fica de autocorrelaci�n WWWusage (Ingenuo)")
#El histograma tiene un sesgo a la derecha sin valores atipicos, con varianza no constante.
#Pico grande en la primera parte seguido por una onda decreciente que alterna entre correlaciones positivas y negativas.

IEstacional1<-snaive(WWWusage,20)
Res2w<-residuals(snaive(WWWusage))
autoplot(Res2w) 
gghistogram(Res2w)+ggtitle("Histograma de Residuales WWWusage (IEstacional)")
ggAcf(Res2w, xlab= "Desfase", ylab="Autocorrelaci�n")+ggtitle("Gr�fica de autocorrelaci�n WWWusage (IEstacional)")
#Tambi�n el histograma tiene un sesgo a la derecha con varianza no constante ni valores atipicos.
#Pico grande en el desfase 1 seguido por una onda decreciente que alterna entre correlaciones positivas y negativas.

#bricksq
Ing2<-naive(bricksq,20)
res2<-residuals(naive(bricksq))
autoplot(res2)
gghistogram(res2)+ggtitle("Histograma de Residuales bricksq (Ingenuo)")
ggAcf(res2, xlab= "Desfase", ylab="Autocorrelaci�n")+ggtitle("Gr�fica de autocorrelaci�n bricksq (Ingenuo)")
#Tambi�n el histograma tiene un sesgo a la derecha con varianza no constante ni valores atipicos.
#Correlaciones significativas en el primer o segundo desfase, seguidas por correlaciones que no son significativas.

ingb2<-snaive(bricksq,20)
resb2<-residuals(snaive(bricksq))
autoplot(resb2)
gghistogram(resb2)+ggtitle("Histograma de Residuales bricksq (IEstacional) ")
ggAcf(resb2, xlab= "Desfase", ylab="Autocorrelaci�n")+ggtitle("Gr�fica de autocorrelaci�n bricksq (IEstacional)")
#Histograma con aparente comportamiento normal pero con un ligero sesgo  a la izquierda y con valores atiicos lejanos a cero del mismo lado.
#Pico grande en el desfase 1 seguido por una onda decreciente que alterna entre correlaciones positivas y negativas.
#Conviene el ingenuo por dos situaciones: no presentan estacionalidad y  por el principio de parsimonia (quedrse con el m�s simple).

#3) Contesta falso o verdadero y justifica.
 ## Los buenos m�todos de pron�stico deber�an tener residuos distribuidos normalmente. VERDADERO: se busca cumplir el criterio de
 ## homocedasticidad a lo largo de la tendencia de la serie de tiempo.

 ## Un modelo con peque�os residuos dar� buenos pron�sticos. VERDADERO, se busca que los residuos tiendan a cero.

 ## La mejor medida de los errores del pron�stico es MAPE: VERDADERO est�n estandarizados en una esclaa y eso lo facilita para su uso e interpretaci�n.

#4)�Qu� quiere decir que una serie de tiempo sea ruido blanco? Que todas las variables de la serie de tiempo son independientes,
   #es la parte impredecible de la serie de tiempo (comportamiento permanentemente aleatorio). Se cumple que la media y covarianza
   #sean cero y la varianza constante. 

#Ya no pude acabar, es que soy lenta :c #

#5) Usa la serie de ibmclose: produce una gr�fica de la serie, divida los datos en un conjunto de entrenamiento
   #de 300 observaciones y u conjunto de prueba de69 observaciones. Use m�todos simples de pron�stico para el conjunto
   #de entrenamiento y compara los resultados en el conjunto de prueba �Qu� metodo es el mejor? Argumenta la respuesta. 
   #Compruebe los residuos del m�todo elegdo, �son ruido blanco?

#6) Utiliza la serie hsales, grafica la serie y dividela en datos de entrenamiento y datos de prueba, donde los datos de prueba son 
   #los �ltimos dos a�os de los datos. Utiliza varios m�todos para pronosticar el conjunto de entrenamiento y compara los resultados en
   #el conjunto de prueba. �Qu� m�todo se ajusta mejor? Justifica la respuesta. Compruebe los residuos del m�todo elegido �son ruido
   #blanco?
