library(ggplot2)
library(survival)
require(foreign)
require(nnet)
require(ggplot2)
library(aod)
library(Rcpp)
require(reshape2)
library(neuralnet)


fuga<-read.csv2("//nas//Datos//Liquidacion//Liquidacion 1//Ivan Echeverría//Informe CobXAsegurado Mensual//Cob x Ase in R//BBDD rpart 2.csv",head=T)
attach(fuga)
names(fuga)

fuga<-fuga[,-1]
names(fuga)

str(fuga)

m<-glm(formula = Estado.Oficial ~ Condición.de.Pago + Morosidad + 
         Clas..Vigencia.Convenio + Suspende.Descuentos + Acepta.Producción + 
         Sector + Marcado + Grupo.Cobranza.Cobrador + Categoría + 
         Clas..Tasa.recaudacion. + Clas..Tasa.Asegurado, family = "binomial", 
       data = fuga)

d = sort(sample(nrow(fuga), nrow(fuga)*.66))

train<-fuga[d,]
dim(train)
test<-fuga[-d,]
dim(test)




#load tree package
library(rpart)
fit1<-rpart(Estado.Oficial ~ ., data=train)


plot(fit1);text(fit1);
#test$t<-predict(fit1,type='class',test)


#build model using 90% 10% priors
#with smaller complexity parameter to allow more complex trees
# for tuning complexity vs. pruning see Thernau 1997
#Credit Scoring in R 9 of 45
fit2<-  rpart(Estado.Oficial~., data=train,parms=list(prior=c(.9,.1)),cp=.0002)
plot(fit2);text(fit2);

test$tscore2<-predict(fit2,type="prob",test)

#score test data
test$tscore1<-predict(fit1,type='prob',test)
pred5<-prediction(test$tscore1[,2],test$Estado.Oficial)
perf5 <- performance(pred5,"tpr","fpr")

