#ARIMA NO ESTACIONAL 
autoplot(ausbeer)
modar<-auto.arima(ausbeer, seasonal = FALSE)
#se tienen 3 autorregresivos y 2 promeidos móviles, se pasan los resultados obtendios con base en la
#ecuación predefinida.

#Se define asi la serie de tiempo:
#yt = c-0.9569*(yt-1)-0.9872*(yt-2)-0.9247(yt-3)-1.0425(et-1)+0.1416(et-2)

c = mean(ausbeer)*(1+0.9569)
#c = 812.8406
#este caso es c y d distintos de cero

modar %>% forecast(h=10) %>% autoplot(include=80) #incluir 80 datos

#___________________________________________________________________________________________________________#
autoplot(AirPassengers)
MOOD<-auto.arima(AirPassengers, seasonal = F)
#se tienen 4 autorregresivos y 2 promedios móviles.

#La serie de tiempo queda:
#yt = c+0.2243(yt-1)+0.3689(yt-2)-0.2567(yt-3)-0.2391(yt-4)-0.0971(et-1)-0.8519(et-2)

c=mean(AirPassengers)*(1+0.2243)
#c = 343.1696
#en este caso, nuevamente c y d distintos de cero.

MOOD %>% forecast(h=10) %>% autoplot(include=80)

