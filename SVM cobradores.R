library("klaR")
library("caret")
library(e1071)
library(kernlab)
library(reshape)
library(ggplot2)
library(scales)
library(zoo)

#fuga<-read.csv2("//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Enero 2017//701-706//701-706.csv",head=T)
fuga<-read.csv2("//nas//Datos//Liquidacion//Liquidacion 1//Ivan Echeverría//Informe CobXAsegurado Mensual//Cob x Ase in R//BBDD rpart 2.csv",head=T)

head(fuga)
names(fuga)

#[1] "Nro..Cobrador"           "Clas..Vigencia.Convenio" "Grupo.Cobranza.Cobrador"
#[4] "Comision.Cobranza"       "Medio.Magnetico"         "Categoría"              
#[7] "Paga"                    "Condición.de.Pago"       "Morosidad"              
#[10] "Suspende.Descuentos"     "Acepta.Producción"       "Marcado"                
#[13] "Oficina.Centralizador"   "Sector"                  "Clas..Tasa.recaudacion."
#[16] "Clas..Tasa.Asegurado"    "Tasa.Anulación"          "Clas..Cant..Asegurados" 
#[19] "Estado.Oficial"         
dim(fuga)
attach(fuga)

fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)


#fuga1<-fuga1[,-1]
#names(fuga1)
#fuga1<-fuga1[,-1]
#names(fuga1)
#fuga1<-fuga1[,-1]
#names(fuga1)
#fuga1<-fuga1[,-1]
#names(fuga1)

#dim(fuga1)
dim(fuga)

muestra <- sample(nrow(fuga), 0.8*nrow(fuga))

fuga_train <- fuga[muestra,]
dim(fuga_train)
names(fuga_train)
fuga_test  <- fuga[-muestra,]
dim(fuga_test)
names(fuga_test)
fuga_validacion <- fuga[15902:15946,] 
dim(fuga_validacion)
fuga_validacion[1,]

(prop<-data.frame(prop.table(table(fuga_train$Estado.Oficial))))

prop.table(table(fuga_test$Estado.Oficial))
#prop.table(table(fuga_validacion$Churn))
prop.table(table(fuga$Estado.Oficial))
# fuga_train <- fuga[1:711,] 
# dim(fuga_train) 
# fuga_test  <- fuga[712:860,]
# dim(fuga_test)

#########using tune() to do automatic grid-search in CV: Encuentra de forma automatica el mejor costo y gamma.
ir.tune<-tune(svm, Estado.Oficial ~., data=fuga_train, 
              ranges=list(gamma=2^(-2:2), cost=2^(-2:2)),
              control = tune.control(sampling="cross", cross=5),kernel="linear")

summary(ir.tune)

#ir.tune$best.parameters[,1]#gamma
#ir.tune$best.parameters[,1]#cost

PRUEBA <- svm(Estado.Oficial ~., data = fuga_train, cross = 5, gamma= ir.tune$best.parameters[,1], cost=ir.tune$best.parameters[,2], kernel= "linear",method="C-classification", probability=T,class.weights = c(No = prop[1,2], Yes = prop[2,2]))

summary(PRUEBA)#Cross validacion tiene un 90% de acierto en promedio

model <- svm(Estado.Oficial ~., data = fuga_train, gamma= ir.tune$best.parameters[,1], cost=ir.tune$best.parameters[,2], kernel= "linear", method="C-classification", probability=T,class.weights = c(No = prop[1,2], Yes = prop[2,2]))

summary(model) 

dim(fuga)
prediction <- predict(model, fuga_test[,-length(fuga_test)])#Se debe obtener la prediccion de los resultados 
summary(prediction)
head(prediccion<-melt(prediction))#Datos ordenados

model$index# Indices de los vectores soporte
model$index
# Coeficientes por los que se multiplican las observaciones para obtener 
# el vector perpendicular al hiperplano que resuelve el problema
plot(model$coefs)
model$rho# Termino independiente 
length(fuga_test)
tab <- table(pred = prediction, true = fuga_test[,length(fuga_test)],dnn = c("Prediccion","Original"))
tab
#Especificidad y Sensibilidad del modelo
classAgreement(tab)
# % correctamente clasificados
#(correctos <- sum(diag(tab)) / nrow(fuga_train) *100)
(correctos <- sum(diag(tab)) / nrow(fuga_test) *100)

#pred <- predict(model, fuga_train[,-17], decision.values = TRUE)
pred <- predict(model, fuga_test[,-length(fuga_test)], decision.values = TRUE, probability=T)
summary(pred)

#svm.model <- svm(y~.,data=dataset,probability=TRUE) 
#svm.pred<-predict(svm.model, test.set, decision.values = TRUE, 
#                  probability = TRUE) 
library(ROCR) 
svm.roc <- prediction(attributes(pred)$decision.values, fuga_test[,length(fuga_test)]) 
svm.auc <- performance(svm.roc, 'tpr', 'fpr') 
plot(svm.auc, avg="vertical", lwd=3, col="blue")
abline(0,1,col="grey")

# Creating performance object
perf.obj <- prediction(attributes(pred)$decision.values, fuga_test[,length(fuga_test)]) 
# Get data for ROC curve
lift.obj <- performance(perf.obj, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")



prob<-attr(pred, "decision.values")
prob2<-attr(pred, "probabilities")

plot(prob,col="black", ylab="Indicador", xlab="Cantidad de Pólizas")
abline(h=model$rho,col = "blue", lty = 1)
points(prob, pch = ifelse(prob > model$rho, 19, 10))

x<-sort(prob)
plot(x)
abline(h=model$rho,col = "blue", lty = 1)


#Guardar probabilidades en un excel
write.table(prob2,"//nas//Datos//Liquidacion//Liquidacion 1//PAMELA//75%//1. Análisis 75% (Período Junio 2017)//Prob-svm3.txt")
write.table(prob, "//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Enero 2017//701-706//Prob 701-706//Probabilidad-Global-histórico.txt")


#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
#####VALIDACION#####
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
       
prediction <- predict(model, fuga_validacion[,-length(fuga_validacion)])

qplot(prediction)
summary(prediction)

# Indices de los vectores soporte
model$index

# Coeficientes por los que se multiplican las observaciones para obtener 
# el vector perpendicular al hiperplano que resuelve el problema
model$coefs
plot(model$coefs)

# Termino independiente 
model$rho

tab <- table(pred = prediction, true = fuga_validacion[,length(fuga_validacion)],dnn = c("Prediccion","Original"))
tab
#Especificidad y Sensibilidad del modelo
classAgreement(tab)
# % correctamente clasificados
#(correctos <- sum(diag(tab)) / nrow(fuga_train) *100)
(correctos <- sum(diag(tab)) / nrow(fuga_validacion) *100)

#pred <- predict(model, fuga_train[,-17], decision.values = TRUE)
pred <- predict(model, fuga_validacion[,-dim(fuga_validacion)], decision.values = TRUE,probability = TRUE)

prob<-attr(pred, "decision.values")
prob2<-attr(pred, "probabilities")
#xtable(prob)
plot(prob,col="black", ylab="Indicador", xlab="Cantidad de Pólizas")
abline(h=model$rho,col = "blue", lty = 1)
points(prob, pch = ifelse(prob > model$rho, 19, 10))

x<-sort(prob)
plot(x)
abline(h=model$rho,col = "blue", lty = 1)


#summary(tuned)

#Guardar probabilidades en un excel
write.table(prob, "//nas//Datos//Liquidacion//Liquidacion 1//Ivan Echeverría//Informe CobXAsegurado Mensual//Cob x Ase in R//Indicador-Global-DatosNuevos.txt")
write.table(prob2, "//nas//Datos//Liquidacion//Liquidacion 1//Ivan Echeverría//Informe CobXAsegurado Mensual//Cob x Ase in R//Probabilidad-Global-DatosNuevos.txt")

