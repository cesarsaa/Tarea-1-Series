library(tidyverse)
library(fpp2) 
library(readxl)
library(ggfortify)

#Leer base de datos
Datos <- read_excel("C:/Users/KEVINSTEVEN/Desktop/UNIVALLE/SERIES DE TIEMPO Y PRONOSTICO/SUAVIZACIÓN EXPONENCIAL/Oro.xlsx")
View(Datos)

series<-ts(Datos$Oro,start = c(2017,1),frequency = 365)
plot(series)
autoplot(series, ts.colour = "blue", ts.linetype = "dashed")

descsermult<-decompose(series,"multiplicative")
autoplot(descsermult)

#Serie original, serie diferenciada y correlograma
seriedif<-diff(series)
x11()
par(mfrow=c(2,2))
plot(series,ylab="Precio por gramo")
plot(seriedif,ylab="Diferencia")
acf(series,main="Correlograma serie precio del oro por gramo")
acf(seriedif,main="Correlograma serie diferencia")

#Suavización exponencial simple
f_ses <- ses(series, h = 30)
plot(f_ses)

#Hacemos un resumen del modelo de suavizado exponencial simple.
summary(f_ses)

#Estimamos un modelo de suavizado exponencial usando la función ets con los parámetros por defecto.
fit_ets_default <- ets(series)
summary(fit_ets_default)
#Luego pasamos el modelo como input para una predicción (usando la función textsl{forecast}) para los próximos 12 meses.
fcast_ets_default <- forecast(fit_ets_default, h = 30)

#Finalmente dibujamos la predicción con la función plot.
plot(fcast_ets_default)












