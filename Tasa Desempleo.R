library(ggplot2)
library(survival)
require(foreign)
require(nnet)
require(ggplot2)
library(aod)
library(Rcpp)
require(reshape2)
library(neuralnet)
library(e1071)


Desempleo<-read.csv2("//nas//Datos//RespaldoPC//IVECLE01//Estudio Tasa Desempleo//Tasa por Sucursal.csv",head=T)
attach(Desempleo)
names(Desempleo)
dim(Desempleo)
head(Desempleo)
Desempleo<-Desempleo[,-1]
names(Desempleo)

str(Desempleo)
#[1] "Tasa.Desempleo"         "PIB"                    "Tasa.Bco.Central"      
#[4] "Desaprobaci.n.Gobierno" "Total.Fuerza.Laboral"   "IPC"                   
#[7] "Terremoto"   
modelo<-glm(Arica ~ Tasa.de.desocupaci.n.Arica,data = Desempleo ,family = "gaussian")
summary(modelo)
cor.test(Arica,Tasa.de.desocupaci.n.Arica)
cor.test(Iquique,Tasa.de.desocupaci.n.Iquique)
cor.test(Antofagasta,Tasa.de.desocupaci.n.Antofagasta)
cor.test(Copiapo,Tasa.de.desocupaci.n.Copiap.)
cor.test(La.Serena,Tasa.de.desocupaci.n.La.Serena)
cor.test(Valpara.so..Vi.a.del.Mar,Tasa.de.desocupaci.n.Valpara.so)
cor.test(Santiago.Moneda.P.Oriente,Tasa.de.desocupaci.n.Santiago)
cor.test(Rancagua,Tasa.de.desocupaci.n.Rancagua)
cor.test(Talca,Tasa.de.desocupaci.n..Talca)
cor.test(Concepci.n,Tasa.de.desocupaci.n.Concepci.n)
cor.test(Temuco,Tasa.de.desocupaci.n.Temuco)
cor.test(Valdivia,Tasa.de.desocupaci.n.Valdivia)
cor.test(Puerto.Montt,Tasa.de.desocupaci.n.Puerto.Montt)
cor.test(Punta.Arenas,Tasa.de.desocupaci.n.Punta.Arenas)



modelo<-glm(formula = Tasa.Desempleo ~ PIB + Tasa.Bco.Central + Desaprobaci.n.Gobierno + 
              Total.Fuerza.Laboral + IPC + Terremoto, family = "Gamma", 
            data = Desempleo)

summary(modelo$coefficients)

summary(modelo)

step(modelo,direction = c("both", "backward", "forward"))
step(modelo,direction = c("both"))
step(modelo,direction = c("backward"))
step(modelo,direction = c("forward"))
step(modelo)
modelo<-glm(formula = Tasa.Desempleo ~ Tasa.Bco.Central + Terremoto + 
      Total.Fuerza.Laboral, family = "Gamma", data = Desempleo)

cor(modelo$fitted.values,Iquique)^2
summary(modelo)

head(modelo$fitted.values)

prob <- predict(modelo, response=F)
df1<-data.frame(prob)
write.csv2(df1, "//nas//Datos//Liquidacion//Liquidacion 1//PAMELA//75%//1. An�lisis 75% (Per�odo Junio 2017)//Prob.csv")
res <- residuals(modelo, type = "deviance")

#Plot Residuals
plot(predict(modelo), res,
     xlab="Fitted values", ylab = "Residuals",
     ylim = max(abs(res)) * c(-1,1))


#modelo<-glm(factor(Churn) ~ Cod.Contrato +Cod.Banco+ factor(Cod.Tipo.Cta)
#            + Cod.Cant.Vig + Cod.Ayuda + factor(Cod.Edad),data = datoslog ,family = "binomial")

#Test de m�xima verosimilitud del modelo#
#G2 <- modelo$deviance - modelo1$deviance
#G2
#1 - pchisq(G2, df = 1)

# coeficientes del modelo
modelo[1]

# devianza
modelo$deviance
## [1] 2246.242


# primeros 6 valores predichos para p(x)
head(modelo$fitted.values)

# primeros 6 valores de los residuos del m�todo de ajuste
head(modelo$residuals)

# primeros 6 valores de los residuos de pearson
head(residuals(modelo, type = "pearson"))

summary(modelo)

confint(modelo)

d = sort(sample(nrow(fuga), nrow(fuga)*.66))

train<-fuga[d,]
dim(train)
test<-fuga[-d,]
dim(test)
#train<-subset(train)
#train<-data.frame(train)

#m<-glm(factor(Churn) ~ .,data = train,family = "binomial")
m<-glm(formula = Acepta.Producci.n ~ Oficina + Condici.n.de.Pago + 
             Morosidad + Sector + Cant..Trabajadores + 
             Cant..Asegurados.P.C. + Producidas + Anuladas + Cant..Aseg.201706 + 
             rec..75..201704 + rec..75..201705, family = "binomial", data = fuga)

m <- step(m)
summary(m)

#significant.variables <- summary(m)$coeff[-1,4] < 0.01
#names(significant.variables)[significant.variables == TRUE]

prob <- predict(m, type = "response")
df1<-data.frame(prob)
write.csv2(df1, "//nas/Datos/RespaldoPC/IVECLE01/8. Credit Scoring/credit_scoring_1.csv")
res <- residuals(m, type = "deviance")

#Plot Residuals
plot(predict(m), res,
     xlab="Fitted values", ylab = "Residuals",
     ylim = max(abs(res)) * c(-1,1))

confint(m)
confint.default(m)
## odds ratios and 95% CI
exp(cbind(OR = coef(m), confint(m)))

#score test data set
library(caret)
library(ROCR)
test$m1_score <- predict(m,type='response',test)
m1_pred <- prediction(test$m1_score, test$Churn)
m1_perf <- performance(m1_pred,"tpr","fpr")

# Plot precision/recall curve
m1_perf_precision <- performance(m1_pred, measure = "prec", x.measure = "rec")
plot(m1_perf_precision, main="m1 Logistic:Precision/recall curve")

#KS, Gini & AUC m1
m1_KS <- round(max(attr(m1_perf,'y.values')[[1]]-attr(m1_perf,'x.values')[[1]])*100, 2)
m1_AUROC <- round(performance(m1_pred, measure = "auc")@y.values[[1]]*100, 2)
m1_Gini <- (2*m1_AUROC - 100)
cat("AUROC: ",m1_AUROC,"\tKS: ", m1_KS, "\tGini:", m1_Gini, "\n")

#auc = performance(m1_pred, "auc")
#auc = unlist(auc@y.values)
#auc

df.coef<-data.frame(m$coefficients)


# Cross Validation
#load Data Analysis And Graphics Package for R (DAAG)
library(DAAG)

#calculate accuracy over 100 random folds of data for simple logit
m1_h <- CVbinary(obj=m, rand=NULL, nfolds=100, print.details=TRUE)

#ROC
plot(m1_perf, lwd=2, colorize=TRUE, main="ROC m1: Logistic Regression Performance")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

#modelo <- predict(m, newdata = test, type = "response")#obtenemos las probabilidades estimadas.
#length(modelo)
#modelo
#head(modelo)

test$predicted.risk = predict(m, newdata=test, type="response")
(table<-table(test$Churn, as.numeric(test$predicted.risk >= 0.1)))
#(correctos <- sum(diag(table))/nrow(test) *100)
(Overall_Accuracy = sum(diag(table))/nrow(test))
(Sensitivity = table[2,2]/ sum(table[2,]))*100
(Specificity = table[1,1]/ sum(table[1,]))*100

library(ROCR)
pred = prediction(test$predicted.risk, test$Churn)
as.numeric(performance(pred, "auc")@y.values)

# Make predictions on training set
predictTrain = predict(m, newdata=train, type="response")
predictTest = predict(m, newdata=test, type="response")
df2<-data.frame(predictTest)
write.csv2(df2, "C:/Users/ivecle01/Documents/predictTest_1.csv")
# Prediction function
ROCRpred = prediction(predictTrain, train$Churn)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(0,1)

#t1 = table(test$Churn, as.numeric(test$predicted.risk >= 0.7349407))
#t1


#this code builds on ROCR library by taking the max delt
#between cumulative bad and good rates being plotted by
#ROCR
max(attr(ROCRperf,'y.values')[[1]]-attr(ROCRperf,'x.values')[[1]]) #No es �ptimo pero se utiliza


# valores ajustados
head(prob.ajustadas[[1]])
length(m)
summary(m)
# errores
head(prob.ajustadas[[2]])
# calculamos los intervalos
valores.inf <- prob.ajustadas[[1]] - 2 * prob.ajustadas[[2]]
valores.sup <- prob.ajustadas[[1]] + 2 * prob.ajustadas[[2]]

summary(newdata2)
newdata2
# var.explicativas, sexo y edad, modelo con 594 datos
(RsqrMcFadden <- 1 - m$deviance/m$null.deviance)
length(m)
#[1] 0.8254229


#Asociado a este punto de corte podemos calcular la tasa de clasificaciones correctas como los individuos
#correctamente clasificados entre el n�mero total de individuos.
tabla.clasif <- table(test$Churn, modelo)
tcc <- 100 * sum(diag(tabla.clasif))/sum(tabla.clasif)
tcc



library(ROCR)
#Con la funci�n performance, calculamos varias medidas asociadas a la tabla de clasificaci�n.
pred <- prediction(fitted.values(m), test$Churn)
perf1 <- performance(pred, measure = "acc")
# el punto de corte que maximiza 'acc' es
(posicion.max <- sapply(perf1@y.values, which.max))
(punto.corte <- sapply(perf1@x.values, "[", posicion.max))


table(test$Churn)

prediccion <- ifelse(fitted.values(m) >= 0.6816457  , 1, 0)#con punto de corte
table(prediccion)

table(test$Churn, prediccion)

plot(perf1, col = "darkred", xlab="Punto de corte", ylab="Exactitud")
# A�adimos una l�nea horizontal al valor de 0.8
abline(h = 0.935, lty = 2, col="black")
# A�adimos recta con el punto de corte que maximiza la tasa de
# clasificaciones correctas
abline(v = punto.corte, lty = 2, col="black")

# auc : Area under curve, �rea bajo la curva
AUC <- performance(pred, "auc")
AUC@y.name

# con performance se selecciona tpr (true positive rate) y fpr (false
# positive rate)
perf2 <- performance(pred, "tpr", "fpr")
plot(perf2, xlab="Raz�n de falsos positivos", ylab="Raz�n de verdaderos positivos",colorize = TRUE,cex = 1.2) # mostramos colores seg�n el punto de corte
# A�adimos la recta y=x que ser�a la correspondiente al peor clasificador
abline(a = 0, b = 1)
# a�adimos el valor del �rea bajo la curva
text(0.3, 0.6, paste("�rea bajo la curva", "\n", round(unlist(AUC@y.values),3)), cex = 1.2)

####Calculating top 3 variables affecting Credit Score Function in R####

#get results of terms in regression
g<-predict(m,type='terms',test)
#function to pick top 3 reasons
#works by sorting coefficient terms in equation
# and selecting top 3 in sort for each loan scored
ftopk<- function(x,top=3){
  res=names(x)[order(x, decreasing = TRUE)][1:top]
  paste(res,collapse=";",sep="")
}
# Application of the function using the top 3 rows
topk=apply(g,1,ftopk,top=3)
#add reason list to scored tets sample
test<-cbind(test, topk)


####Residuos de pearson####


res.p <- residuals(m, type = "pearson")
# Residuos de pearson de los 6 primeros individuos de la encuesta
head(res.p)

#Podemos ver cu�ntos son significativos
res.p.sig <- abs(res.p) > 2
table(res.p.sig)
#res.p.sig
#FALSE  TRUE 
#2905    61 
#Que suponen menos del 5 % de los residuos
20/574
#Podemos ordenar el conjunto de datos de mayor a menor valor de los residuos.
res.orde <- sort(abs(res.p[res.p.sig]), decreasing = TRUE)
# mostramos solo los m�s altos
head(res.orde)

# con names(res.orde) obtenemos la fila a la que corresponde, y ordenamos
# el data.frame utilizando names(res.orde) como �ndice
head(test[names(res.orde), ])

fitted.values(m)[324]

#Dato Observardo vs Probabilidad
cbind(observado = test$Churn, predicho = fitted.values(m))[c(1:20), ]


exp(-54.667)
###Residuos de pearson estandarizados###

res.p.std <- rstandard(modelo, type = "pearson")

#Ser�n significativos aquellos cuyo valor absoluto sea mayor de 2.
res.p.std.sig <- abs(res.p.std) > 2
table(res.p.std.sig)

# utilizamos res.p.std.sig como indice para ver el valor de los
# significativos
head(res.p.std[res.p.std.sig])

#Al igual que antes, ordenamos de mayor a menor y vemos qu� valor tienen en las variables predictoras.
res.orde <- sort(abs(res.p.std[res.p.std.sig]), decreasing = TRUE)

head(res.orde)
head(test[names(res.orde), ])

fitted.values(modelo)[17]

#Dato Observardo vs Probabilidad
cbind(observado = test$Churn, predicho = fitted.values(modelo))[c(1,7,18,70,172,243,164,145), ]

####Residuos de la devianza estandarizados####

res.dev.std <- rstandard(modelo, type = "deviance")
# significativos
table(abs(res.dev.std) > 2)


####Residuos studentizados####

res.student <- rstudent(modelo)
res.orde <- sort(res.student, decreasing = TRUE)
head(res.orde)
head(datoslog[names(res.orde), ])

table(abs(res.student) > 2)




#####Medidas de influencia#####

#Pueden existir observaciones que, situ�ndose lejos del resto pueden influir en las estimaciones del
#modelo

distancias.cook <- cooks.distance(modelo)
head(distancias.cook)

hat.valores <- hatvalues(modelo)
head(hat.valores)


#Se consideran valores influyentes aquellos cuya distancia de cook sea mayor que 1 (Cook and Weisberg,
#                                                                                   1982) .
table(distancias.cook > 1)

#En nuestro caso, no hay ninguna distancia de cook significativa.

# con id.n=3 indicamos que muestre los tres mayores valores en cada
# gr�fico
influenceIndexPlot(modelo, id.cex = 0.7, id.n = 3)


#####Validaci�n cruzada####

library(boot)
#La funci�n cost mide para cada observaci�n si el modelo la clasifica incorrectamente mediante
#abs(r-pi)>0.5, que aplicado a todas las observaciones devuelve un vector con el valor TRUE si
#el valor absoluto de la diferencia entre los valores observados y predichos es mayor de 0.5, y FALSE
#en caso contrario

cost <- function(r, pi) mean(abs(r - pi) > 0.5)

#Por tanto, si el modelo clasifica incorrectamente a un encuestado el
#valor de abs(r-pi)>0.5 es TRUE
#Con mean(abs(r-pi)>0.5) lo que estamos haciendo es obtener
#la proporci�n de individuos mal clasificados.

res.10.fold <- cv.glm(datoslog, modelo1, cost, K = 15)

#Dentro del objeto res.10.fold se ha guardado el valor medio de la tasa de casos incorrectamente
#clasificados a la que se accede mediante el operador $.
res.10.fold$delta
[1] 0.03540121 0.03550295

#el primer valor corresponde a la media
#de la tasa de clasificaciones incorrectas, y el segundo es un valor corregido de dicha tasa para
#compensar el sesgo que se introduce al no utilizar el m�todo leave-one-out cross-validation (Efron, 1974).


1 - res.10.fold$delta

#El valor obtenido indica que, en media, el modelo clasifica correctamente a m�s del 96% de los
#encuestados cuando estos no han sido usados en el ajuste del modelo.


#Si lo que queremos es validaci�n cruzada mediante el m�todo leave-one-out cross-validation, elegimos
#K igual al n�mero de observaciones en los datos.
# puede tardar en realizar el c�lculo, 7-9 minutos

res.loocv <- cv.glm(datoslog, modelo1, cost, K = nrow(datoslog))
1 -res.loocv$delta

#Otros paquetes tambi�n incorporan sus propias funciones para validaci�n cruzada, tal es el caso del
#paquete DAAG (Maindonald and Braun, 2013) que incorpora la funci�n CVbinary o del paquete rms
#(Harrel, 2013) con la funci�n validate. Como ejemplo, vamos a utilizar la funci�n CVbinary.
library(DAAG)
args(CVbinary)
res.daag <- CVbinary(modelo1)
res.daag$acc.cv

#Y los valores predichos mediante validaci�n cruzada.
# mostramos s�lo los 6 primeros
head(res.daag$cvhat)

#[1] 1.00000000 0.84145265 1.00000000 0.03270124 0.01179902 0.54018747
#A la vista de la alta tasa de clasificaciones correctas calculada mediante validaci�n cruzada, de la no
#existencia de valores influyentes y del escaso porcentaje de residuos significativos, concluimos que el
#modelo no presenta falta de ajuste ni tampoco problemas de sobreajuste.
-------------------------------------------------------------------------------



exp(coef(modelo))
exp(cbind( OR  =  coef (modelo),  confint (modelo)))



newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

modelo<-glm(Cod.Estado.Poliza~Capital.Asegurado + as.factor(Cod.dif.prima) + 
              as.factor(Cod.Mandato) , family = "binomial") 

summary(modelo)


modelo<-glm(Cod.Rescate~ Cod.dif.prima + 
              Cod.Sexo + Cod.Mandato + Cod.Mes.Activacion , family = "binomial") 

summary(modelo)


modelo<-glm(Cod.Estado.Poliza~Capital.Asegurado + Cod.dif.prima 
            + Cod.Mandato , family = "binomial")
summary(modelo)

modelo<-glm(Cod.no.pago~ Cod.Sexo + Meses.Activos + Capital.Asegurado + 
              Cod.dif.prima + Benificiario
            + Cod.Rescate + Edad, family = "binomial")

summary(modelo)

summary(modelo)
plot(modelo)

modelo2<-glm(Cod.no.pago ~ Cod.Sexo  +
              Cod.dif.prima + Meses.Activos, family = "binomial") 

summary(modelo2)
names(modelomodelo2)
modelomodelo2$effects
plot(modelomodelo2$linear.predictors)
plot(modelomodelo2$effects)











names(dat)
dat1<-data.frame(Nro..Poliza, Mes.Activo ,Edad,Cod.Sexo,Cod.Motivo.Anulaci�n2,Cod.Motivo.Rescate,Fecha.Inicio.Condicion,Fecha.Anulacion,
                 Capital.Asegurado, Cod.Plan.2,Cod.Beneficiario)
names(dat1)
str(dat1)
with(dat1, table(Cod.Sexo, Cod.Plan))

test <- glm(Cod.Motivo.Anulaci�n2 ~ Edad + as.factor(Cod.Sexo) + as.factor(Cod.Plan.2) + Cod.Beneficiario +
                   Capital.Asegurado, data = dat1)

test2 <- glm(Cod.Motivo.Rescate ~ Edad + Cod.Sexo + Cod.Plan.2 + Cod.Beneficiario +
              Capital.Asegurado, data = dat1)
summary(test)
summary(test2)



1502+1464

summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
z

#2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1))*2
p

exp(coef(test))
head(pp <- fitted(test))

dses <- data.frame(ses = c("low", "middle", "high"),
                   Capital.Asegurado = mean(Capital.Asegurado))
predict(test, newdata = dses, "probs")
##################################################################################################
library(MASS)
dat<-read.csv2("C:/Users/ivecle01/Documents/Iv�n Echeverr�a/PAC-GRAL-LOGISTIC 2013-2015.csv",head=T)
attach(dat)
names(dat)

str(dat)

# [1] "Nro..Poliza"         "Meses.Activos"       "Anos.Inicio.Termino" "Capital.Asegurado"  
# [5] "Cod.dif.prima"       "Cod.no.pago"         "Cod.Rescate"         "Churn"              
# [9] "Cod.Plan"            "Cod.Estado.Poliza"   "Benificiario"        "Edad"               
# [13] "Cod.Sexo"            "Cod.Mandato"         "Cod.Mes.Activacion"  "Regioin.Asociado"   
# [17] "Cod.Edad"            "Cod.D�a.Cargo"       "Cod.Mes.Inter"       "Cod.Tipo.Cta"       
# [21] "Cod.Prima.Pactada"   "Cant.Polizas"        "Cod.Cant.Pol"        "Cant.Pol.Vig."      
# [25] "Cod.Pol.Vig"         "Cant.Pol.Anu"        "Cod.Pol.Anu"         "Cta.Ayudas"         
# [29] "Cod.Ayuda"          

# plot(Edad)
# hist(Edad)
# kedad<-kmeans(Edad)
# summary(kedad, centers=7, nstart=40)
# table(kedad$betweenss)
# 
# kedad$cluster <- as.factor(kedad$cluster)
# plot(kedad$cluster)
datos<-dat
set.seed(42)  #  fija la secuencia de numeros aleatorios
sampletrain  <- sample(nrow(datos),(nrow(datos)*.7)) #obtenemos el tama�o muestral
Train        <- datos[sampleTrain,] #datos de Prueba
dim(Train)
Test         <- datos[-sampleTrain,] #datos de Entrenamiento
dim(Test)

modelo<-glm(as.factor(Churn) ~ factor(Cod.Edad < 3) + factor(Cod.Tipo.Cta < 2)+ Cod.Mandato + factor(Cod.Mes.Activacion < 2)
            + Cod.Mes.Inter +  Cod.dif.prima ,data= Test,family = "binomial") 
summary(modelo)
newdata1 <- predict(modelo, newdata = Test, type = "response")
newdata1
summary(newdata1)
par(mfrow=c(1,2))
plot(sort(newdata1), type="l")

newdata2 <- predict(modelo, type="response", newdata=Train)
summary(newdata2)
plot(sort(newdata2), type="l")

colnames<-c(suceso,p)
data1<-data.frame(newdata1)
names(data1)

suceso = c (0 ,0 ,0 ,1 ,1 ,1 ,0 ,1 ,1 ,0 ,0 ,0 ,0 ,1 ,1)
p = c (0 .928 ,0 .576 ,0 .008 ,0 .944 ,0 .832 ,0 .816 ,0 .136 ,
       0 .584 ,0 .032 ,0 .016 ,0 .28 ,0 .024 ,0 ,0 .984 ,0 .952 )

A = data.frame(suceso , p)
roc.plot(data1$, data1$newdata1) # Dibujo la ROC
roc.area(A$suceso , A$p) # Area bajo ROC (Ho: A=0 .5)


# GRAFICO CURVA ROC
#------------------------------------------------------------------------------
auc <- as.numeric(performance(newdata2 ,"auc")@y.values)
plot(perf.rocr,type='o', main = paste('Area Bajo la Curva =',round(auc,2)))  
abline(a=0, b= 1)















summary(modelo, cor = T)
res = step (modelo, list ( lower = ??? 1 ,upper = formula (modelo)) , scale =1 , trace =F ,
             direction = "backward")
res$anova
res


stepAIC(modelo) 
confint(modelo)




library(verification)
# Se consideran valores 0 -1 frente a probabilidades pi_i
suceso = c (0 ,0 ,0 ,1 ,1 ,1 ,0 ,1 ,1 ,0 ,0 ,0 ,0 ,1 ,1)
p = c (0.928 ,0.576 ,0.008 ,0.944 ,0.832 ,0.816 ,0.136 ,
       0.584 ,0.032 ,0.016 ,0.28 ,0.024 ,0 ,0.984 ,0.952)
A = data.frame ( suceso , p )
roc.plot(A$suceso,A$p) # Dibujo la ROC
roc.area(A$suceso,A$p) # Area bajo ROC (Ho: A=0 .5)

predict.rpart <- predict(modelo,type = "prob")[,2] #prob. clase=yes
predict.rocr  <- prediction (predict.rpart,Test$RainTomorrow)
perf.rocr     <- performance(predict.rocr,"tpr","fpr") #True y False postivie.rate






## odds ratios and 95% CI
exp(cbind(OR = coef(modelo), confint(modelo)))


cbind(logit = predict (modelo) ,
        fitted.prop = predict (modelo , type = "response"))


wald.test(b = coef(modelo), Sigma = vcov(modelo), Terms = 3:4)
wald.test(b = coef(modelo), Sigma = vcov(modelo), Terms = 7:14)


l <-  cbind ( 0 , 0 , 0, 0 , 0 , 0 , 1 , -1 , 1 , -1 , 1 , -1 , 1 , -1) 
wald.test ( b  =  coef (modelo),  Sigma  =  vcov (modelo),  L  = l)

exp(coef(modelo))
exp(cbind( OR  =  coef (modelo),  confint (modelo)))

newdata1 <- with(dat,
                 data.frame(Cod.Tipo.Cta = factor(1:3), Cod.Mes.Activacion = factor(1:9), Cod.Sexo))

## view data frame
newdata1







modelo<-glm(Cod.Estado.Poliza~Capital.Asegurado + as.factor(Cod.dif.prima) + 
              as.factor(Cod.Mandato) , family = "binomial") 

summary(modelo)


modelo<-glm(Cod.Rescate~ Cod.dif.prima + 
              Cod.Sexo + Cod.Mandato + Cod.Mes.Activacion , family = "binomial") 

summary(modelo)


modelo<-glm(Cod.Estado.Poliza~Capital.Asegurado + Cod.dif.prima 
            + Cod.Mandato , family = "binomial")
summary(modelo)

modelo<-glm(Cod.no.pago~ Cod.Sexo + Meses.Activos + Capital.Asegurado + 
              Cod.dif.prima + Benificiario
            + Cod.Rescate + Edad, family = "binomial")

summary(modelo)

summary(modelo)
plot(modelo)

modelo2<-glm(Cod.no.pago ~ Cod.Sexo  +
               Cod.dif.prima + Meses.Activos, family = "binomial") 

summary(modelo2)
names(modelomodelo2)
modelomodelo2$effects
plot(modelomodelo2$linear.predictors)
plot(modelomodelo2$effects)











names(dat)
dat1<-data.frame(Nro..Poliza, Mes.Activo ,Edad,Cod.Sexo,Cod.Motivo.Anulaci�n2,Cod.Motivo.Rescate,Fecha.Inicio.Condicion,Fecha.Anulacion,
                 Capital.Asegurado, Cod.Plan.2,Cod.Beneficiario)
names(dat1)
str(dat1)
with(dat1, table(Cod.Sexo, Cod.Plan))

test <- glm(Cod.Motivo.Anulaci�n2 ~ Edad + as.factor(Cod.Sexo) + as.factor(Cod.Plan.2) + Cod.Beneficiario +
              Capital.Asegurado, data = dat1)

test2 <- glm(Cod.Motivo.Rescate ~ Edad + Cod.Sexo + Cod.Plan.2 + Cod.Beneficiario +
               Capital.Asegurado, data = dat1)
summary(test)
summary(test2)



1502+1464

summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
z

#2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1))*2
p

exp(coef(test))
head(pp <- fitted(test))

dses <- data.frame(ses = c("low", "middle", "high"),
                   Capital.Asegurado = mean(Capital.Asegurado))
predict(test, newdata = dses, "probs")


############################################
# CARGA LIBRERIA Y DATOS
#------------------------------------------------------------------------------
library(rpart)   # para arbol decision
library(rattle)  # para data set, y arbol decision
library(ROCR)    # para curva ROC

datos         <- fuga
names(datos)
dim(datos)

#datos         <- within(datos, rm("Date","Location","RISK_MM")) #borra dummy 
#names(datos)

set.seed(42)  #  fija la secuencia de numeros aleatorios
sampletrain  <- sample(nrow(datos),(nrow(datos)*.7)) #obtenemos el tama�o muestral
Train <- datos[sampletrain,] #datos de entrenamiento
dim(Train)
Test          <- datos[-sampletrain,] #datos de prueba
dim(Test)

# MODELO 
#------------------------------------------------------------------------------
modelo.rpart  <- rpart(Churn ~ .,Train, method="class")
modelo1<-glm(Churn ~ factor(Cod.Edad < 3) + factor(Cod.Mes.Activacion < 2)
            + Cod.Mes.Inter +  Cod.dif.prima ,data= Train,family = "binomial")
modelo2<-glm(Churn ~ factor(Cod.Edad < 3) + factor(Cod.Mes.Activacion < 2)
            + Cod.Mes.Inter +  Cod.dif.prima ,data= Test,family = "binomial")
summary(modelo1)
summary(modelo2)
library(ROCR)
# PREDICCION
#------------------------------------------------------------------------------
predict.rpart <- predict(modelo.rpart,Test[,-length(Test)]) #prob. clase=yes
predict.rocr  <- prediction(predict.rpart,Test$Churn)

perf.rocr     <- performance(predict.rocr,"tpr","fpr") #True y False postivie.rate


# GRAFICO CURVA ROC
#------------------------------------------------------------------------------
auc <- as.numeric(performance(predict.rocr ,"auc")@y.values)
plot(perf.rocr,type='o', main = paste('Area Bajo la Curva =',round(auc,2)))  
abline(a=0, b= 1)


# GRAFICO ARBOL DECISION
#------------------------------------------------------------------------------
fancyRpartPlot(modelo.rpart)     


data(ROCR.hiv)
attach(ROCR.hiv)
pred.svm <- prediction(hiv.svm$predictions, hiv.svm$labels)
perf.svm <- performance(pred.svm, 'tpr', 'fpr')
pred.nn <- prediction(hiv.nn$predictions, hiv.svm$labels)

##########nuevo credit Scotring########################

library(ggplot2)
library(survival)
require(foreign)
require(nnet)
require(ggplot2)
library(aod)
library(Rcpp)
require(reshape2)

fuga<-read.csv2("//nas//Datos//RespaldoPC//IVECLE01//Modelo Credit Scoring//MCC.csv",head=T)
attach(fuga)
names(fuga)
dim(fuga)

fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-20]

devtools::install_github("jbkunst/riskr")
library(ggplot2)
library("riskr")
data("predictions")
head(predictions)

target<-factor(predictions$target)
str(predictions)
score <- predictions$score

target <- factor(predictions$target)
ks(score,target)
aucroc(score, target)
gini(score, target)
perf(score, target)
plot_roc(score, target)
