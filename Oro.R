library(tidyverse)
library(fpp2) 
library(readxl)
library(ggfortify)

#Leer base de datos
Datos <- read_excel("/Users/cesar.saavedra/Documents/GitHub/Tarea-1-Series/Oro.xlsx")
View(Datos)

series<-ts(Datos$Oro,start = c(2017,1),frequency = 365)
plot(series)
autoplot(series, ts.colour = "blue", ts.linetype = "dashed")

descsermult<-decompose(series,"multiplicative")
autoplot(descsermult)

#Serie original, serie diferenciada y correlograma
seriedif<-diff(series)

par(mfrow=c(2,2))
plot(series,ylab="Precio por gramo")
plot(seriedif,ylab="Diferencia")
acf(series,main="Correlograma serie precio del oro por gramo")
acf(seriedif,main="Correlograma serie diferencia")

#Suavizaci?n exponencial simple
f_ses <- ses(series, h = 30)
plot(f_ses)

#Hacemos un resumen del modelo de suavizado exponencial simple.
summary(f_ses)

#Estimamos un modelo de suavizado exponencial usando la funci?n ets con los par?metros por defecto.
fit_ets_default <- ets(series)
summary(fit_ets_default)
#Luego pasamos el modelo como input para una predicci?n (usando la funci?n textsl{forecast}) para los pr?ximos 12 meses.
fcast_ets_default <- forecast(fit_ets_default, h = 30)

#Finalmente dibujamos la predicci?n con la funci?n plot.
plot(fcast_ets_default)


#Suavizacion Holt
S2 <- HoltWinters(series, alpha=T, beta=T, gamma=T)

plot(HoltWinters(series, alpha=0.1, beta=F, gamma=F))
forecast <- predict(S, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(S, forecast)

#Suavizacion Holt-Winters
(S3 <- HoltWinters(series, seasonal = "mult"))
plot(S3)
forecast <- predict(S3, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(S3, forecast)

#----------------------------------------------------------------------------#
# Suavizado exponencial simple: beta=FALSE, gamma=FALSE
# Suavizado de Holt: gamma=FALSE

# Suavizado exponencial simple
ses.cantidad<-HoltWinters(ts(series), beta=FALSE, gamma=FALSE)
plot(ses.cantidad)
plot(fitted(ses.cantidad))
ses.cantidad$alpha

# Suavizado de Holt
sholt.cantidad<-HoltWinters(ts(series), gamma=FALSE)
plot(sholt.cantidad)
plot(fitted(sholt.cantidad))
sholt.cantidad$alpha
sholt.cantidad$beta


p.ses.cantidad<-predict(ses.cantidad, n.ahead=49)
p.sholt.cantidad<-predict(sholt.cantidad, n.ahead=49)


# Suavizado de Holt-Winters
S3 <- HoltWinters(series, seasonal = "mult")
plot(S3)
forecast <- predict(S3, prediction.interval = T, level = 0.95, n.ahead = 12)
plot(S3, forecast)



