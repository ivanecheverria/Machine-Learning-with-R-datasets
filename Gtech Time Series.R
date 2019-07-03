rates <- read.csv2("//nas//Datos//RespaldoPC//IVECLE01//Datos_R.csv",header = T)
rates[1:2, ]
names(rates)
str(rates$Periodo)
rates$Periodo <- as.Date(rates$Periodo, "%d-%b-%Y")
range(rates$Periodo)
rates <- rates[order(rates$Periodo), ]
plot(type="l",rates$year, rates$Reajuste...Var.UF)

years <- format(rates$Date, "%Y-%m-%d")
tab <- table(years)
tab

mean(tab[1:(length(tab) - 1)])


# simple exponential - models level
fit <- HoltWinters(rates$Reajuste...Var.UF, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit <- HoltWinters(rates$Reajuste...Var.UF, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(rates$Reajuste...Var.UF)

# predictive accuracy
library(forecast)
accuracy(fit)

# predict next three future values
library(forecast)
forecast(fit, 3)
plot(forecast(fit, 3))

library(forecast)  ## see code file in section 5
fit <- arfima(rates$Reajuste...Var.UF)
plot(forecast(fit,h=3))
result.arima <- forecast(rates)


# fit an ARIMA model of order P, D, Q
fit <- arima(rates$Reajuste...Var.UF, order=c(p, d, q))
fit <- auto.arima(rates$Reajuste...Var.UF,seasonal=FALSE)
             
             
# predictive accuracy
library(forecast)
accuracy(fit)
             
# predict next 5 observations
library(forecast)
forecast(fit, 1)
plot(forecast(fit, 1))

