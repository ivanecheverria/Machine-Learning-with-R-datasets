library(timeSeries)
library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
###########################################################################

tickers <- c("CLP=X")

#Calculate Returns: Daily
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(Ticker, from="2018-01-01", auto.assign=FALSE)[,4])

#portfolioPrices <- portfolioPrices[!is.na(portfolioPrices$CLP.X.Close),]
portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(portfolioPrices) <- tickers

#Calculate Returns: Daily RoC
portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
#portfolioReturns <- as.timeSeries(portfolioReturns)


names(portfolioReturns)
mean(portfolioReturns)
sd(portfolioReturns)
tail(portfolioPrices)
#mean(portfolioReturns$"CLP=X")

barplot(portfolioReturns)

#plot.xts(as.xts(portfolioReturns ))
PerformanceAnalytics::chart.TimeSeries(as.xts(portfolioReturns))

library(sde)

mu=0.0003588011; sigma=0.00709235; P0=696.43; T = (1/12)/30 ##1/12 = 1 month
nt=10000; n=365 #nt; cantidad de secuencias.-
#############Generate nt trajectories
dt=T/n; t=seq(0,T,by=dt)
X=matrix(rep(0,length(t)*nt), nrow=nt)
for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T,N=n)}
##Plot
ymax=max(X); ymin=min(X) #bounds for simulated prices
plot(t,X[1,],t='l',ylim=c(ymin, ymax), col=1,
     ylab="Price P(t)",xlab="time t")
for(i in 1:nt){lines(t,X[i,], t='l',ylim=c(ymin, ymax),col=i)}
#

for(i in 1:nt){lines(t,X[i,], t='l',ylim=c(ymin, ymax),col=i)}

(GBM(x=P0,r=mu,sigma=sigma,T=T,N=n))

mean(X[i,])
