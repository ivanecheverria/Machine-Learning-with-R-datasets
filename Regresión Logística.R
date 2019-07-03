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
#[1] "Nro.Poliza"         "Fecha.Vigencia"     "Cod.Mes.Activacion" "Cod.dif.prima"     
#[5] "Cod.Edad"           "Cod.Mes.Inter"      "Cod.Mandato"        "Banco"             
#[9] "Cod.Tipo.Cta"       "Cod.Cant.Vig"       "Cod.Cant.Anu"       "Cod.Ayuda"         
#[13] "Remuneracion"       "Meses.Gracia"       "Cod.No.Pago"        "Estado.Contrato"   
#[17] "Dia.Pago"           "Agencia"            "Genero"             "Capital"           
#[21] "Churn"              "ID"    

dim(fuga)
head(fuga)
#variables significativas#
chisq.test(Churn,Cod.Mes.Activacion)#p-value < 2.2e-16
chisq.test(Churn,Cod.dif.prima)#p-value = 9.345e-05
chisq.test(Churn,Cod.Mes.Inter)#p-value < 2.2e-16
cor.test(Churn,Cod.Mandato)#p-value < 2.2e-16
chisq.test(Churn,Cod.Tipo.Cta)#p-value < 2.2e-16
cor.test(Churn,Cod.Cant.Vig)#p-value < 2.2e-1
cor.test(Churn,Cod.Cant.Anu)#p-value < 2.2e-16
chisq.test(Churn,Cod.Ayuda)#p-value = 0.0005441
chisq.test(Churn,Remuneracion)#p-value < 2.2e-16
cor.test(Churn,Meses.Gracia)# p-value < 2.2e-16
chisq.test(Churn,Cod.No.Pago)#p-value < 2.2e-16
chisq.test(Churn,Estado.Contrato)#p-value < 2.2e-16
chisq.test(Churn,Dia.Pago)#p-value = 0.0006093
#variables no significativas#
cor.test(Churn,Agencia)#p-value = 0.4125
chisq.test(Churn,Genero)#p-value = 0.2651
cor.test(Churn,Capital)#p-value = 0.2347
chisq.test(Churn,Cod.Edad)#p-value = 0.6216
cor.test(Churn,Banco)#p-value = 0.9295


fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-21]
names(fuga)
fuga<-fuga[,-22]
names(fuga)


modelo<-glm(factor(Churn) ~ factor(Cod.Mes.Activacion)+factor(Cod.dif.prima)
            +factor(Cod.Mes.Inter)+factor(Cod.Mandato)+
              factor(Cod.Tipo.Cta)+factor(Cod.Cant.Vig)+
              factor(Cod.Cant.Anu)+factor(Cod.Ayuda)+factor(Remuneracion)+
              factor(Cod.No.Pago)+factor(Dia.Pago),data = fuga ,family = "binomial")

summary(modelo$coefficients)

summary(modelo)

head(modelo$fitted.values)

#modelo<-glm(factor(Churn) ~ Cod.Contrato +Cod.Banco+ factor(Cod.Tipo.Cta)
#            + Cod.Cant.Vig + Cod.Ayuda + factor(Cod.Edad),data = datoslog ,family = "binomial")

#Test de máxima verosimilitud del modelo#
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

# primeros 6 valores de los residuos del método de ajuste
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
m<-glm(factor(Churn) ~ factor(Cod.Mes.Activacion)+factor(Cod.dif.prima)
            +factor(Cod.Mes.Inter)+factor(Cod.Mandato)+
              factor(Cod.Tipo.Cta)+factor(Cod.Cant.Vig)+
              factor(Cod.Cant.Anu)+factor(Cod.Ayuda)+factor(Remuneracion)+
              factor(Cod.No.Pago)+factor(Dia.Pago),data = fuga ,family = "binomial")

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
max(attr(ROCRperf,'y.values')[[1]]-attr(ROCRperf,'x.values')[[1]]) #No es óptimo pero se utiliza


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
#correctamente clasificados entre el número total de individuos.
tabla.clasif <- table(test$Churn, modelo)
tcc <- 100 * sum(diag(tabla.clasif))/sum(tabla.clasif)
tcc



library(ROCR)
#Con la función performance, calculamos varias medidas asociadas a la tabla de clasificación.
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
# Añadimos una línea horizontal al valor de 0.8
abline(h = 0.935, lty = 2, col="black")
# Añadimos recta con el punto de corte que maximiza la tasa de
# clasificaciones correctas
abline(v = punto.corte, lty = 2, col="black")

# auc : Area under curve, área bajo la curva
AUC <- performance(pred, "auc")
AUC@y.name

# con performance se selecciona tpr (true positive rate) y fpr (false
# positive rate)
perf2 <- performance(pred, "tpr", "fpr")
plot(perf2, xlab="Razón de falsos positivos", ylab="Razón de verdaderos positivos",colorize = TRUE,cex = 1.2) # mostramos colores según el punto de corte
# Añadimos la recta y=x que sería la correspondiente al peor clasificador
abline(a = 0, b = 1)
# añadimos el valor del área bajo la curva
text(0.3, 0.6, paste("Área bajo la curva", "\n", round(unlist(AUC@y.values),3)), cex = 1.2)

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

#Podemos ver cuántos son significativos
res.p.sig <- abs(res.p) > 2
table(res.p.sig)
#res.p.sig
#FALSE  TRUE 
#2905    61 
#Que suponen menos del 5 % de los residuos
20/574
#Podemos ordenar el conjunto de datos de mayor a menor valor de los residuos.
res.orde <- sort(abs(res.p[res.p.sig]), decreasing = TRUE)
# mostramos solo los más altos
head(res.orde)

# con names(res.orde) obtenemos la fila a la que corresponde, y ordenamos
# el data.frame utilizando names(res.orde) como índice
head(test[names(res.orde), ])

fitted.values(m)[324]

#Dato Observardo vs Probabilidad
cbind(observado = test$Churn, predicho = fitted.values(m))[c(1:20), ]


exp(-54.667)
###Residuos de pearson estandarizados###

res.p.std <- rstandard(modelo, type = "pearson")

#Serán significativos aquellos cuyo valor absoluto sea mayor de 2.
res.p.std.sig <- abs(res.p.std) > 2
table(res.p.std.sig)

# utilizamos res.p.std.sig como indice para ver el valor de los
# significativos
head(res.p.std[res.p.std.sig])

#Al igual que antes, ordenamos de mayor a menor y vemos qué valor tienen en las variables predictoras.
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

#Pueden existir observaciones que, situándose lejos del resto pueden influir en las estimaciones del
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
# gráfico
influenceIndexPlot(modelo, id.cex = 0.7, id.n = 3)


#####Validación cruzada####

library(boot)
#La función cost mide para cada observación si el modelo la clasifica incorrectamente mediante
#abs(r-pi)>0.5, que aplicado a todas las observaciones devuelve un vector con el valor TRUE si
#el valor absoluto de la diferencia entre los valores observados y predichos es mayor de 0.5, y FALSE
#en caso contrario

cost <- function(r, pi) mean(abs(r - pi) > 0.5)

#Por tanto, si el modelo clasifica incorrectamente a un encuestado el
#valor de abs(r-pi)>0.5 es TRUE
#Con mean(abs(r-pi)>0.5) lo que estamos haciendo es obtener
#la proporción de individuos mal clasificados.

res.10.fold <- cv.glm(datoslog, modelo1, cost, K = 15)

#Dentro del objeto res.10.fold se ha guardado el valor medio de la tasa de casos incorrectamente
#clasificados a la que se accede mediante el operador $.
res.10.fold$delta
[1] 0.03540121 0.03550295

#el primer valor corresponde a la media
#de la tasa de clasificaciones incorrectas, y el segundo es un valor corregido de dicha tasa para
#compensar el sesgo que se introduce al no utilizar el método leave-one-out cross-validation (Efron, 1974).


1 - res.10.fold$delta

#El valor obtenido indica que, en media, el modelo clasifica correctamente a más del 96% de los
#encuestados cuando estos no han sido usados en el ajuste del modelo.


#Si lo que queremos es validación cruzada mediante el método leave-one-out cross-validation, elegimos
#K igual al número de observaciones en los datos.
# puede tardar en realizar el cálculo, 7-9 minutos

res.loocv <- cv.glm(datoslog, modelo1, cost, K = nrow(datoslog))
1 -res.loocv$delta

#Otros paquetes también incorporan sus propias funciones para validación cruzada, tal es el caso del
#paquete DAAG (Maindonald and Braun, 2013) que incorpora la función CVbinary o del paquete rms
#(Harrel, 2013) con la función validate. Como ejemplo, vamos a utilizar la función CVbinary.
library(DAAG)
args(CVbinary)
res.daag <- CVbinary(modelo1)
res.daag$acc.cv

#Y los valores predichos mediante validación cruzada.
# mostramos sólo los 6 primeros
head(res.daag$cvhat)

#[1] 1.00000000 0.84145265 1.00000000 0.03270124 0.01179902 0.54018747
#A la vista de la alta tasa de clasificaciones correctas calculada mediante validación cruzada, de la no
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
dat1<-data.frame(Nro..Poliza, Mes.Activo ,Edad,Cod.Sexo,Cod.Motivo.Anulación2,Cod.Motivo.Rescate,Fecha.Inicio.Condicion,Fecha.Anulacion,
                 Capital.Asegurado, Cod.Plan.2,Cod.Beneficiario)
names(dat1)
str(dat1)
with(dat1, table(Cod.Sexo, Cod.Plan))

test <- glm(Cod.Motivo.Anulación2 ~ Edad + as.factor(Cod.Sexo) + as.factor(Cod.Plan.2) + Cod.Beneficiario +
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
dat<-read.csv2("C:/Users/ivecle01/Documents/Iván Echeverría/PAC-GRAL-LOGISTIC 2013-2015.csv",head=T)
attach(dat)
names(dat)

str(dat)

# [1] "Nro..Poliza"         "Meses.Activos"       "Anos.Inicio.Termino" "Capital.Asegurado"  
# [5] "Cod.dif.prima"       "Cod.no.pago"         "Cod.Rescate"         "Churn"              
# [9] "Cod.Plan"            "Cod.Estado.Poliza"   "Benificiario"        "Edad"               
# [13] "Cod.Sexo"            "Cod.Mandato"         "Cod.Mes.Activacion"  "Regioin.Asociado"   
# [17] "Cod.Edad"            "Cod.Día.Cargo"       "Cod.Mes.Inter"       "Cod.Tipo.Cta"       
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
sampletrain  <- sample(nrow(datos),(nrow(datos)*.7)) #obtenemos el tamaño muestral
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
dat1<-data.frame(Nro..Poliza, Mes.Activo ,Edad,Cod.Sexo,Cod.Motivo.Anulación2,Cod.Motivo.Rescate,Fecha.Inicio.Condicion,Fecha.Anulacion,
                 Capital.Asegurado, Cod.Plan.2,Cod.Beneficiario)
names(dat1)
str(dat1)
with(dat1, table(Cod.Sexo, Cod.Plan))

test <- glm(Cod.Motivo.Anulación2 ~ Edad + as.factor(Cod.Sexo) + as.factor(Cod.Plan.2) + Cod.Beneficiario +
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
sampletrain  <- sample(nrow(datos),(nrow(datos)*.7)) #obtenemos el tamaño muestral
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
