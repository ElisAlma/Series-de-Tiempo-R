#Se toma en cuenta que existe una base de datos en formato csv precargada
##para ver gr�ficamente una descomposicion de una Ts 
plot(tdesempcadi)

#para descomponer una setrie de tiempo en un modelo aditivo 
#tdesempcadi<-decompose(tdesmp)
#ahora para descomponer una setrie de tiempo en un modelo multiplicativo
tdesempmult<-decompose(tdesmp, type = "mult")

#Gr�fica de los modelos
plot(tdesempcadi)
plot(tdesempmult)

#para descomponer ua TS  tenemos diversos elementos
#La descomposici�n de ST nos genera diversos elementos como la estacionalidad, la tendencia y la aleatoriedad... son los elementos que nos interesan
#para esto se utiliza la funci�n names()
names(tdesempcadi)
#tiene el objeto 
##Para extraer variables
tdesempcadi$trend