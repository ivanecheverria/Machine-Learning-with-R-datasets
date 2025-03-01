library(forecast)
AirPassengers # a built-in dataset
#      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
# 1949 112 118 132 129 121 135 148 148 136 119 104 118
# 1950 115 126 141 135 125 149 170 170 158 133 114 140
# 1951 145 150 178 163 172 178 199 199 184 162 146 166
# 1952 171 180 193 181 183 218 230 242 209 191 172 194
# 1953 196 196 236 235 229 243 264 272 237 211 180 201
# 1954 204 188 235 227 234 264 302 293 259 229 203 229
# 1955 242 233 267 269 270 315 364 347 312 274 237 278
# 1956 284 277 317 313 318 374 413 405 355 306 271 306
# 1957 315 301 356 348 355 422 465 467 404 347 305 336
# 1958 340 318 362 348 363 435 491 505 404 359 310 337
# 1959 360 342 406 396 420 472 548 559 463 407 362 405
# 1960 417 391 419 461 472 535 622 606 508 461 390 432

set.seed(1) # for reproducibility
promos <- rep(0,length(AirPassengers))
promos[sample(seq_along(AirPassengers),10)] <- 1
promos.future <- c(0,1,0,0,1,0,0,1,0,0,1,0)
AP.with.promos <- AirPassengers
AP.with.promos[promos==1] <- AP.with.promos[promos==1]+120

model <- auto.arima(AP.with.promos,xreg=promos)
summary(model) # examine the model - you'll see the estimated promo coefficient
# Series: AP.with.promos 
# ARIMA(0,1,1)(0,1,0)[12]                    

# Coefficients:
#           ma1    promos
#       -0.3099  122.2599
# s.e.   0.0947    2.2999

# sigma^2 estimated as 151.2:  log likelihood=-457.4
# AIC=920.79   AICc=920.98   BIC=929.42

# Training set error measures:
#                     ME     RMSE     MAE        MPE     MAPE      MASE         ACF1
# Training set 0.2682805 11.12974 8.24397 0.06139784 2.867274 0.1860814 0.0008326436

fcast <- forecast(model,xreg=promos.future,h=length(promos.future))
fcast
#          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# Jan 1961       447.1516 431.3951 462.9081 423.0542 471.2490
# Feb 1961       543.4115 524.2670 562.5559 514.1326 572.6904
# Mar 1961       449.1516 427.1345 471.1687 415.4793 482.8239
# Apr 1961       491.1516 466.5956 515.7076 453.5964 528.7068
# May 1961       624.4115 597.5556 651.2674 583.3389 665.4841
# Jun 1961       565.1516 536.1777 594.1255 520.8399 609.4633
# Jul 1961       652.1516 621.2044 683.0988 604.8220 699.4812
# Aug 1961       758.4115 725.6095 791.2135 708.2452 808.5778
# Sep 1961       538.1516 503.5942 572.7090 485.3006 591.0026
# Oct 1961       491.1516 454.9237 527.3795 435.7459 546.5573
# Nov 1961       542.4115 504.5869 580.2361 484.5637 600.2593
# Dec 1961       462.1516 422.7950 501.5082 401.9608 522.3424
promos.ts <- ts(c(AP.with.promos,fcast$mean),
                start=start(AirPassengers),frequency=frequency(AirPassengers))
promos.ts[c(promos,promos.future)==0] <- NA

plot(fcast)
points(promos.ts,pch=19,col="red")


########Moedeling Volatility######################
library(tseries)
myebacklog <-read.csv2("//nas//Datos//Liquidacion//Liquidacion 1//PAMELA//Nuevo Informe Recaudaci�n//08.2017 (Per�odo an�lisis Mayo 2017)//Carlos Ahumada.csv",head=T)

attach(myebacklog)
View(myebacklog)
str(myebacklog)
names(myebacklog)
#[[1] "Fecha"                 "Clausula"              "Vida.Entera"          
#[4] "Unipersonal"           "MIS.MAES"              "Capital.Garantizado"  
#[7] "Seguros.Familiares"    "Seguros.Oncol�gicos"   "Accidentes.Personales"
#[10] "Saldado"               "CAF"                   "Secora"               
#[13] "Colectivos"            "Secoal"    
myebacklog = c(Seguros.Familiares)

ts.myebacklog = ts(myebacklog,frequency = 12, start=c(2016,10), end=c(2017,4))
plot(ts.myebacklog)
dim(as.matrix(ts.myebacklog))

#outliers#

library(tsoutliers)
library(expsmooth)
library(fma)

outlier <- tso(ts.myebacklog,types = c("AO","LS","TC"), maxit.iloop = " ")
outlier
plot(outlier)#para detectar outliers


#box.text#

Box.test(coredata(ts.myebacklog^2), type = "Ljung-Box", lag = 12)
#Box-Ljung test
#data:  coredata(ts.myebacklog^2)
#X-squared = 25.755, df = 12, p-value = 0.01162

#install.packages("FinTS")
library("FinTS")
ArchTest(coredata(ts.myebacklog)) #para observar la volatilidad de una serie de tiempo

#ARCH LM-test; Null hypothesis: no ARCH effects

#data:  coredata(ts.myebacklog)
#Chi-squared = 19.182, df = 12, p-value = 0.08422
library(forecast)
model<-auto.arima(ts.myebacklog,max.P =3, max.Q = 3)
fcast <- forecast(model,h=6)
fcast
plot(fcast)

fit <- HoltWinters(ts.myebacklog)
summary(fit)
plot(forecast(fit,h=6))

p <- predict(fit, 6, prediction.interval = TRUE)
plot(fit, p)

snaive(ts.myebacklog,h=3)

#SVM in time series#

rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 10.13878

#Tuning regression model#
library(e1071)

data<-data.frame(Fecha[1:69,],ts.myebacklog)
tuneResult <- tune(svm, as.numeric(myebacklog$BCI) ~ as.numeric(myebacklog$Rec.BCI),  data = data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult)
# best performance: MSE = 8.371412, RMSE = 2.89 epsilon 1e-04 cost 4
# Draw the tuning graph
plot(tuneResult)

tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, data) 

plot(tunedModelY, type="l")
plot(myebacklog$Rec.BCI, type="l")
error <- myebacklog$Rec.BCI - as.numeric(tunedModelY  
lines(error)
abline(error)
par(mfrow=c(2,1))

# this value can be different on your computer
# because the tune method  randomly shuffles the data
tunedModelRMSE <- rmse(error)  # 197119831  




library("markovchain")
library(VGAMdata)
# NOT RUN 
{
rate <- exp(2); myshape <- 3
edata <- data.frame(y = rep(0, nn <- 1000))
for (ii in 1:myshape)
  edata <- transform(edata, y = y + rexp(nn, rate = rate))
fit <- vglm(y ~ 1, erlang(shape = myshape), data = edata, trace = TRUE)
coef(fit, matrix = TRUE)
Coef(fit)  # Answer = 1/rate
1/rate
summary(fit)
}







