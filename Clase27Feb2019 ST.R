#EJERCICIO DE CLASE:
Pib<-read.csv("C:/Users/ALUMNO-D12/Documents/PIB MÉX.csv")
Pibts<-ts(Pib, start = 1961, end = 2017)

#Modelo de suavizado exponencial simple 
#con alpha que va de 0 a 1, es lo que permite dar mayor peso a los 
#ultimos datos, h num de pronosticos a definir, no se te ocurra poner "Simple"
Mod1<-ses(Pibts, alpha = 0.1, initial = "simple", h=8)
Mod2<-ses(Pibts, alpha = 0.3, initial = "simple", h=8)
Mod3<-ses(Pibts, alpha = 0.9, initial = "simple", h=8)


#se grafican los elementos reales del modelo 1, type es el tipo por eso salen los circulos, luego
#se agregan los valores ajustados del modelo 1 de color azul, en la misma grafica se ponen los ajustados del
#modleo 2 en rojo, se agrega los ajustados del modelo 3 en verde, se pone la leyenda en el superior 
#izquierdo, ancho de la linea con lty, 1 es el negro (los datos del modelo 1).
plot(Mod1, ylab = "PIB", xlab = "Año", main = "Suavizamiento exponencial simple:PIB", type = "o")
lines(fitted(Mod1),col="blue", type = "o")
lines(fitted(Mod2),col="red", type = "o")
lines(fitted(Mod3),col="green", type = "o")
legend("topleft", lty = 1, col = c(1,"blue","red","green"),
       c("data",expression(alpha==0.1), expression(alpha==0.3),
         expression(alpha==0.9)),pch=1)
#Entre más se acerque a 1 se suaviza de forma más agresiva, modifica menos datos, lo ideal es
#quedarse con el 0.9, se busca acercarse a los datos reales. No hay método para las alphas, no hay 
#reglas para elegirlas, es más intuitivo, cuando hay saltos importantes se recomienda alphas grandes.


#EJERCICIO: Con la función windows separar la serie del PIB en datos de entrenamiento y datos de
#prueba (20%) y graficar los modelos solamente en los que corresponde a los datos de prueba los
#valores ajustados de los 3 modelos.

Entrenamiento<-window(Pibts, start = 1961, end = 2006)
Prueba<-window(Pibts, start = 2006, end = 2017)
Mod1p<-ses(Prueba, alpha = .1, initial = "simple", h=8)
Mod2p<-ses(Prueba, alpha=.3, initial="simple", h=8)
Mod3p<-ses(Prueba, alpha=.9, initial="simple", h=8)
plot(Mod1p, ylab = "PIB", xlab = "Año", main = "Ejercicio SES: PIB", type = "o")
lines(fitted(Mod1p), col="blue", type="o")
lines(fitted(Mod2p), col="red", type="o")
lines(fitted(Mod3p), col="green", type="o")
legend("topleft", lty = 1, col = c("1", "blue", "red", "green"),
       c("data", expression(alpha==0.1), expression(alpha==0.3),
         expression(alpha==.9)), pch = 1)

