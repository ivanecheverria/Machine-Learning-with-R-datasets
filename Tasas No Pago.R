##### Graficar serie inicial 
library(tseries)
tasas<-read.csv2("//nas//Datos//Liquidacion//Liquidacion 1//Ivan Echeverría//Análisis Tasas No Pago (PAC-PAT-MAILER-DESC. PLANILLA)//Tasas No Pago sept.2017.csv",head=T)
str(tasas)
View(tasas)
tasas = c(tasas$Descuento.por.Planilla)
ts.tasas = ts(tasas, frequency=12, start=c(2014,8) )
plot(ts.tasas)
dim(as.matrix(ts.tasas))

##### Asignación de datos 
data.train = window(ts.tasas, start = c(2014,8), end = c(2016,12)) 
plot(data.train)
dim(as.matrix(data.train))

##### Descomposición de la serie 
plot(decompose(data.train))

##### Prueba de Estacionalidad 
adf.test(data.train)
pp.test(data.train)
kpss.test(data.train)

##### Diferenciación de la serie (solo si es necesaria)
dif<-diff(data.train,1)
adf.test(dif)
#pp.test(dif)
#kpss.test(dif)

##### Prueba de modelos tentativos ARIMA
library(forecast)
ar2<-arima(data.train,order=c(1,1,0),include.mean=TRUE)
summary(ar2)
ar3<-arima(data.train,order=c(2,1,0),include.mean=TRUE)
summary(ar3)
ar4<-arima(data.train,order=c(3,1,0),include.mean=TRUE)
summary(ar4)
ar5<-arima(data.train,order=c(0,1,1),include.mean=TRUE)
summary(ar5)
ar6<-arima(data.train,order=c(1,1,1),include.mean=TRUE)
summary(ar6)
ar7<-arima(data.train,order=c(2,1,1),include,mean=TRUE)
summary(ar7)
ar8<-arima(data.train,order=c(3,1,1),include.mean=TRUE)
summary(ar8)
ar9<-arima(data.train,order=c(0,1,2),include.mean=TRUE)
summary(ar9)
ar10<-arima(data.train,order=c(1,0,8),include.mean=TRUE)
summary(ar10)

auto.arima(data.train,seasonal = FALSE,stationary = FALSE)

## arima (1,1,2) mejor modelo (ar10) ##

fit<-auto.arima(data.train, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,0,6) Model Residuals')
tsdisplay(residuals(ar10), lag.max=45, main='(1,0,6) Model Residuals')


fcast <- forecast(ar10, h=12)
plot(fcast)

##### Nivel de confianza y Significación de la serie
library(lmtest)
confint(ar4)
coeftest(ar4)
acf(ar4$residuals,72)
pacf(ar4$residuals,72)

##### Diagnóstico de los Residuos
plot.ts(ar4$residuals)
Box.test(ar4$residuals,lag=5, type="Ljung-Box")
Box.test(ar4$residuals,lag=10, type="Ljung-Box")
Box.test(ar4$residuals,lag=15, type="Ljung-Box")
Box.test(ar4$residuals,lag=20, type="Ljung-Box")

##### Prueba de Heterocedasticidad
acf(ar4$residuals^2, lag.max=72, main="ACF of the Model")
Box.test(ar4$residuals^2,lag=1, type="Ljung-Box")

##### Bondad de ajuste (test de normalidad)
jarque.bera.test(ar4$residuals)

##### Ajuste de ARIMA a la serie original
plot(data.train,type="l",ylab="Bookings (miles de dólares)",xlab="Serie mensual")
lines(fitted(ar4),col="red")
legend(2014.5,34000,c("datos originales"," Modelo arima"),lty=c(1,1),lwd=c(1.5,1.5),col=c("black","red"))

##### Predicción del modelo
ar10.forecast= forecast.Arima(ar10, h=12)
plot(ar10.forecast) ## PREDICCIÓN
lines(fitted(ar4),col="red")
ar4.forecast

##### Desviación de los errores
ar4
sqrt(26089218)
