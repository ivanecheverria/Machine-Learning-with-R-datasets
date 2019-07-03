##### Graficar serie inicial 
library(tseries)
myebacklog <-read.csv2("//nas//Datos//RespaldoPC//IVECLE01//Proyeccción CobxRec 2017//Proyeccion.PAC.csv",head=T)
View(myebacklog)
str(myebacklog)
names(myebacklog)
#[1] "Fecha"            "BCI"              "Otros.Bancos"    
#[4] "Rec.BCI"          "Rec.Otros.Bancos"         
myebacklog = c(myebacklog$Rec.BCI)

myebacklog$Fecha2 <- as.Date(myebacklog$Fecha2, "%d-%m-%Y")
str(myebacklog$Fecha)
plot(myebacklog$Fecha2, myebacklog$Rec.BCI, type = "l")

ts.myebacklog = ts(myebacklog$Rec.BCI, frequency = 30, start=c(2015,1))#, end=c(2015,3,15))
#plot(ts.myebacklog)
#dim(as.matrix(ts.myebacklog))

decomposes <- decompose(ts.myebacklog)
plot(decomposes)

##### Automatic
library(forecast)
fit <- arima(ts.myebacklog, order=c(p, d, q))
fit <- auto.arima(ts.myebacklog,seasonal=F)
##### Asignación de datos 
dim(myebacklog)
#d = sort(sample(nrow(myebacklog), nrow(myebacklog)*.70))
#train<-myebacklog[1:31]
#dim(train)
#test<-myebacklog[32:43]
#dim(myebacklog)

data.train = window(ts.myebacklog, start = c(2015,1))#,end=c(2016,12)) 
plot(data.train)
#dim(as.matrix(data.train))
#library(reshape)
#library(kernlab)
#library(e1071)
#model <- svm(myebacklog$Cobranza ~ myebacklog$Mis.Maes, data = train, gamma= 1, cost=0.5, kernel= "linear", method="eps-regression", probability=T)
#prediction <- predict(model, test)#Se debe obtener la prediccion de los resultados 
#summary(prediction)
#prediccion<-melt(prediction)
##### Descomposición de la serie 
plot(decompose(data.train))

##### Prueba de Estacionalidad 
adf.test(data.train) #Estacionalidad > 0,05
pp.test(data.train)
kpss.test(data.train)
#x <- 0.3 * (1: 1000) + rnorm (1000) # es tendencia estacionaria
#kpss.test(x, null = "Trend")

##### Diferenciación de la serie (solo si es necesaria)
dif<-diff(data.train)
adf.test(dif)
pp.test(dif)
kpss.test(dif)

tsdisplay(diff(data.train),main="")

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
ar10<-arima(data.train,order=c(2,1,1),include.mean=TRUE)
summary(ar10)
ar10<-arima(data.train,order=c(1,1,3),include.mean=TRUE)
summary(ar10)

ar01<-arma(data.train,order=c(2,1))
summary(ar01)
## arima (1,1,2) mejor modelo (ar10) ##

##### Nivel de confianza y Significación de la serie
library(lmtest)
confint(ar10)
coeftest(ar10)
acf(ar10$residuals,72)
pacf(ar10$residuals,72)

##### Diagnóstico de los Residuos
plot.ts(ar10$residuals)
Box.testar10$residuals,lag=5, type="Ljung-Box")
Box.test(ar10$residuals,lag=10, type="Ljung-Box")
Box.test(ar10$residuals,lag=15, type="Ljung-Box")
Box.test(ar10$residuals,lag=20, type="Ljung-Box")

##### Prueba de Heterocedasticidad
acf(ar10$residuals^2, lag.max=72, main="ACF of the Model")
Box.test(ar10$residuals^2,lag=1, type="Ljung-Box")

##### Bondad de ajuste (test de normalidad)
jarque.bera.test(ar10$residuals)

##### Ajuste de ARIMA a la serie original
plot(data.train,type="l",ylab="Cantidad de transacciones",xlab="Serie mensual")
lines(fitted(ar10),col="red")
lines(fitted(ar01),col="red")
#lines(fitted(ar11),col="blue")
#legend(2015.2,xxx,c("Datos originales"," Modelo arima"),lty=c(1,1),lwd=c(1,1),col=c("black","red"))

##### Pronostico del modelo
ar10.forecast= forecast.Arima(ar10,h=12)
ar10.forecast= forecast.Arima(ar10,h=12)
plot(ar10.forecast) ## PREDICCIÓN
lines(fitted(ar10),col="red")
lines(ts.myebacklog, col="blue")
legend(2016.2,1300,c("Datos originales"," Modelo arima"),lty=c(1,1),lwd=c(1,1),col=c("black","red"))
ar10.forecast
accuracy(ar10.forecast)
#rwf(data.train,drift=TRUE,h=3)
snaive(data.train,h=3)
HoltWinters(data.train)
forecast(data.train,h=3)
##### gráfico Rcharts
library(reshape)
(fit <- arima(log(data.train), c(2, 1, 1)))
pred <- predict(fit, n.ahead = 12)
ts.plot(data.train,exp(pred$pred), log = "y", lty = c(1,3))
exp(16.25901)
exp(21.75172)
exp(21.86038)

exp(21.82900)
exp(21.75171)
exp(21.86038)

#library(devtools)
#install_github('rCharts', 'ramnathv', force = TRUE)
#library(rCharts)
#library(ggplot2)
#data(myebacklog, package = "ggplot2")
#econ <- transform(myebacklog, date = as.character(year))
#m1 <- mPlot(x = "date", y = c("Reajuste.del.Sector.P.blico", "I.P.C.", "U.F."), type = "Line", data = econ)
#m1$set(pointSize = 0, lineWidth = 1)
#m1$print("chart2")


##### Desviación de los errores
ar10
sqrt(1.351e+15)

####Holt-winters#####
m <- HoltWinters(ts.myebacklog)
p <- predict(m, 6, prediction.interval = TRUE)
plot(m, p)



###Trading Garch###
require(e1071)
require(quantmod)
require(parallel)
library(e1071)
source("e1071.R")

tt = get( getSymbols( "^GSPC", from="1900-01-01" ) )

rets = na.trim( ROC( Cl( tt ), type="discrete" ) )

# only the first two features so that we may see some results in reasonable time
data = svmFeatures(tt)[,c(1,2)]

rets = rets[index(data)]
data = data[index(rets)]

stopifnot( NROW( rets ) == NROW( data ) )

fore = svmComputeForecasts(
  data=data,
  history=500,
  response=rets,
  cores=8,
  trace=T,
  modelPeriod="days",
  startDate="1959-12-28",
  endDate="1959-12-31",
  featureSelection="all" )

fore$pred
