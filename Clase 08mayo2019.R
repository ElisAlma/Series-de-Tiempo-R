#1. Graficar
autoplot(gas)

#2.Ver si es estacional, se aplica adf.test, se espera qe p-value sea menor a 0.05 para saber si es estacional
adf.test(gas)
#Augmented Dickey-Fuller Test

#data:  gas
#Dickey-Fuller = -2.7131, Lag order = 7, p-value = 0.2764
#alternative hypothesis: stationary

#3.Aplicar auto.arima
auto.arima(gas)

#si no es estacionaria se aplica la diferenciación y transformación de box-cox
#Series: gas 
#ARIMA(2,1,1)(0,1,1)[12] 

#Coefficients:
  #ar1     ar2      ma1     sma1
#0.3756  0.1457  -0.8620  -0.6216
#s.e.  0.0780  0.0621   0.0571   0.0376

#sigma^2 estimated as 2587081:  log likelihood=-4076.58
#AIC=8163.16   AICc=8163.29   BIC=8183.85

gas1<-diff(gas)
adf.test(gas1)

#Augmented Dickey-Fuller Test

#data:  gas1
#Dickey-Fuller = -19.321, Lag order = 7, p-value = 0.01
#alternative hypothesis: stationary

gas %>% ggtsdisplay()
#El ACF es sinuoso, se propone p=1, d=1 (por la diferenciación)
#la estacionalidad se ve en le retardo 12, 24, 36 (unica significativa es la estacional), entonces P= 1 (por la sinucidad del ACP)
#es 12 por la frecuencia
aa<-auto.arima(gas)
aa
#Coefficients:
#ar1     ar2      ma1     sma1
#0.3756  0.1457  -0.8620  -0.6216
#s.e.  0.0780  0.0621   0.0571   0.0376

#sigma^2 estimated as 2587081:  log likelihood=-4076.58
#AIC=8163.16   AICc=8163.29   BIC=8183.85

#----------------------------------------------------------
#De forma manual se hace así:
gas %>% 
  Arima(order = c(1,1,1), seasonal = C(1,1,1)) #este tiene AICc = 8168.68

gas %>% Arima(order = c(1,1,1), seasonal = c(0,1,1)) #se queda este
#Series: . 
#ARIMA(1,1,1)(0,1,1)[12] 

#Coefficients:
 # ar1      ma1     sma1
#0.2802  -0.7458  -0.6090
#s.e.  0.0969   0.0697   0.0387

#sigma^2 estimated as 2614267:  log likelihood=-4079.23
#AIC=8166.47   AICc=8166.56   BIC=8183.02

residuals() %>% ggtsdisplay()
mod1<-forecast(residuals(gas),h=12)
autoplot(mod1)

