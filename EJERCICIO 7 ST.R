#7)Utiliza la serie hsales, grafica la serie y dividela en datos de entrenamiento y datos de prueba, donde los datos de prueba son 
   #los �ltimos dos a�os de los datos. Utiliza varios m�todos para pronosticar el conjunto de entrenamiento y compara los resultados en
   #el conjunto de prueba. �Qu� m�todo se ajusta mejor? Justifica la respuesta. Compruebe los residuos del m�todo elegido �son ruido
   #blanco?

hs<-hsales
autoplot(hs,col="blue", xlab = "",ylab = "Ventas")+ggtitle("Ventas mensuales de casa familiares en EUA desde 1973")

hswin<-window(hs, start=1973,end=c(1993,12))
hswoe<-window(hs,start=1974, end=c(1995,11))

MMedia<-meanf(hswin,h=24)
MDeriva<-rwf(hswin, h=24, drift = TRUE)
MIEst<-snaive(hswin, h=24)
MIng<-naive(hswin, h=24)
autoplot(hs)+
  autolayer(MMedia, series="Media", PI=FALSE)+
  autolayer(MDeriva,series = "Deriva", PI=FALSE)+
  autolayer(MIEst, series = "Ingenuo estacional", PI=FALSE)+
  autolayer(MIng, series = "Ingenuo", PI=FALSE)+
  ggtitle("PRON�STICOS CONJUNTO ENTRENAMIENTO")+xlab("A�os")+ylab("Ventas")
guides(colour=guide_legend(title = "M�todos de Pron�stico"))

resven1<-residuals(MMedia)
resven2<-residuals(MDeriva)
resven3<-residuals(MIEst)
resven4<-residuals(MIng)

ggAcf(MMedia)
ggAcf(MDeriva)
ggAcf(MIEst)
ggAcf(MIng)


