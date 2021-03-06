Nflx<-read.csv("C:/Users/Alumno-H29/Desktop/NETFLIX.csv")
Nflxts<-ts(Nflx, start = 2018, end = 2019, frequency = 252)
plot(Nflxts)

#M�todo de la media (ya se est� pronosticando)
help("meanf")
GrafNflx<-meanf(Nflxts, h=20)
plot(GrafNflx)

#M�todo del Ingenuo
help("naive")
Graf2Nflx<-naive(Nflxts, h=20)
plot(Graf2Nflx)

#M�todo del ingenuo estacional
help("snaive")
Graf3Nflx<-snaive(Nflxts, 20)
plot(Graf3Nflx)

#M�todo de la deriva
Graf4Nflx<-rwf(Nflxts, h=20, drift = TRUE)
plot(Graf4Nflx)

cerveza<-window(ausbeer, start=1992, end=c(2007, 4)) #El window nos permite cortar una serie
#Grafica algunos pron�sticos
autoplot(cerveza)+autolayer(meanf(cerveza, h=11), series="Media", PI=FALSE)+
  autolayer(naive(cerveza,h=11),series = "Ingenuo", PI=FALSE)+
  autolayer(rwf(cerveza, h=11, drift = T), series = "Deriva", PI=FALSE)+
  autolayer(snaive(cerveza, h=11), series = "Ingenuo estacional", PI=FALSE)+
  ggtitle("PRON�STICOS PRODUCCI�N CERVEZA")+xlab("A�o")+ylab("Miles de litros")+
  guides(colour=guide_legend(title = "Pron�stico"))
# El PI=FALSE hace que todas los pron�sticos queden en el mismo gr�fico

# Gr�fica 2
autoplot(cerveza)+autolayer(rwf(cerveza, h=11, drift = T), series="Deriva", PI=FALSE)+
  autolayer(naive(cerveza,h=11),series = "Ingenuo", PI=FALSE)+
  autolayer(snaive(cerveza, h=11), series = "Ingenuo estacional", PI=FALSE)+
  ggtitle("PRON�STICOS PRODUCCI�N CERVEZA")+xlab("A�o")+ylab("Miles de litros")+
  guides(colour=guide_legend(title = "Pron�stico"))





         
         
