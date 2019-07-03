library("klaR")
library("caret")
library(e1071)
library(kernlab)
library(reshape)
library(ggplot2)
library(scales)
library(zoo)
library(partykit)

fuga<-read.csv2("//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Marzo 2017//R-PAT.csv",head=T)

head(fuga)
names(fuga)

#[1] "Nro..Poliza"            "Fecha.vigencia"         "Cod.Mes.Activacion"     "Cod.Dif.Prima"         
#[5] "Cod.Edad"               "Cod.Mes.Inter"          "Tarjeta"                "Cuenta.Polizas"        
#[9] "Cuenta.Anuladas"        "Cuenta.Ayudas"          "Cuenta.No.Pagos"        "Rango.Remuneracion"    
#[13] "Sexo.Asociado"          "Agencia"                "Tipo.Contrato.Agente"   "Estado.Contrato.Agente"
#[17] "Churn"  

dim(fuga)
attach(fuga)

(x<-Sys.Date())
date2<-substr(x,1,7)
mm<-"01"
sep<-"-"
d<-paste0(date2,sep,mm)
d
fuga1<-fuga[as.Date(fuga$Fecha.vigencia,"%d-%m-%Y") >= d,]#Reconocimiento de datos con fecha de vigencia 

dim(fuga1)
dim(fuga)

fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
#fuga<-fuga[,-1]
#names(fuga)
fuga1<-fuga1[,-1]
names(fuga1)
fuga1<-fuga1[,-1]
names(fuga1)
#fuga1<-fuga1[,-1]
#names(fuga1)

dim(fuga1)
dim(fuga)

muestra <- sample(nrow(fuga), 0.6*nrow(fuga))

fuga_train <- fuga[muestra,]
dim(fuga_train)
names(fuga_train)
fuga_test  <- fuga[-muestra,]
dim(fuga_test)
names(fuga_test)
fuga_validacion <- fuga1 #la validacion corresponde a los datos nuevos al mes correspondiente.
dim(fuga_validacion)
fuga_validacion[1,]

(prop<-data.frame(prop.table(table(fuga_train$Churn))))

prop.table(table(fuga_test$Churn))
prop.table(table(fuga_validacion$Churn))
prop.table(table(fuga$Churn))
# fuga_train <- fuga[1:711,] 
# dim(fuga_train) 
# fuga_test  <- fuga[712:860,]
# dim(fuga_test)

#########using tune() to do automatic grid-search in CV: Encuentra de forma automatica el mejor costo y gamma.
ir.tune<-tune(svm, Churn ~., data=fuga_train, 
              ranges=list(gamma=2^(-10:10), cost=2^(-10:10)),
              control = tune.control(sampling="cross", cross=5),kernel="radial")

summary(ir.tune)

#gamma<-ir.tune$best.parameters[,1]#gamma
#cost<-ir.tune$best.parameters[,2]#cost

PRUEBA <- svm(Churn ~ ., data = fuga_train, cross = 5, gamma= ir.tune$best.parameters[,1], cost=ir.tune$best.parameters[,2], kernel= "linear",method="C-classification", probability=T,class.weights = c(No = prop[1,2], Yes = prop[2,2]))

summary(PRUEBA)#Cross validacion tiene un 90% de acierto en promedio

model <- svm(Churn ~., data = fuga_train, gamma= ir.tune$best.parameters[,1], cost=ir.tune$best.parameters[,2], kernel= "linear", method="C-classification", probability=T,class.weights = c(No = prop[1,2], Yes = prop[2,2]))

summary(model) 

dim(fuga)
prediction <- predict(model, fuga_test[,-length(fuga_test)])#Se debe obtener la prediccion de los resultados 
summary(prediction)
head(prediccion<-melt(prediction))#Datos ordenados

model$index# Indices de los vectores soporte

# Coeficientes por los que se multiplican las observaciones para obtener 
# el vector perpendicular al hiperplano que resuelve el problema
model$coefs
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
write.table(prob, "//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Marzo 2017//Indicador-Global-histórico.txt")
write.table(prob2, "//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Marzo 2017//Probabilidad-Global-histórico.txt")


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
pred <- predict(model, fuga_validacion[,-length(fuga_validacion)], decision.values = TRUE,probability = TRUE)

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
write.table(prob, "//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Marzo 2017//Indicador-Global-DatosNuevos.txt")
write.table(prob2, "//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Marzo 2017//Probabilidad-Global-DatosNuevos.txt")


# Classification R con tarjetas#

library(e1071)
library(rpart)

fuga<-read.csv2("//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Agosto 2017//R-Tarjetas.csv",head=T)

head(fuga)
names(fuga)
dim(fuga)
#[1] "Nro..Poliza"                "Capital.Asegurado"          "Prima.Minima"              
#[4] "Prima.Pactada"              "Nro..Plan"                  "Cod.Vig.Pol"               
#[7] "Cod.Edad"                   "Oficina.Agente"             "Tipo.Contrato.Agente"      
#[10] "Estado.Contrato.Agente"     "Clas..Meses.Intermediarios" "Tarjeta.2"                 
#[13] "Churn"

#Realizamos la separación de la BBDD#
fuga<-fuga[,-1]
names(fuga)


fuga1<-fuga[1370:1617,]#Reconocimiento de datos con fecha de vigencia 
fuga2<-fuga[1:1369,]

dim(fuga1)
dim(fuga2)


index<-1:nrow(fuga2)
testindex<-sample(index,trunc(length(index)/3))
test<-fuga2[testindex,]
train<-fuga2[-testindex,]

##SVM model##

#using tune() to do automatic grid-search in CV: 
#Encuentra de forma automatica el mejor costo (parámetro de clasificación) 
#y gamma (parámetro radial para la función específica del kernel).

ir.tune<-tune(svm, Churn ~., data=test, 
              ranges=list(gamma=2^(-2:2), cost=2^(-2:2)),
              control = tune.control(sampling="cross", cross=5),kernel="radial")

summary(ir.tune)

PRUEBA <- svm(Churn ~ ., data = train, cross = 5, gamma= ir.tune$best.parameters[,1], cost=ir.tune$best.parameters[,2], kernel= "linear",method="C-classification", probability=T)
               
summary(PRUEBA)#Cross validacion tiene un 90% de acierto en promedio)

model <- svm(Churn ~ ., data = train, gamma= ir.tune$best.parameters[,1], cost=ir.tune$best.parameters[,2], kernel= "linear", method="C-classification", probability=T)

svm.pred <- predict(model, test[,-12])

#rpart#
rpart.model<-rpart(Churn~., data=train)
rpart.pred<-predict(rpart.model,test[,-11], type="class")


# matriz de confusión#
tabsvm<-table(pred=svm.pred, true=test[,11])
table(pred=rpart.pred, true=test[,11])

classAgreement(tabsvm)

#validación#
svm.pred2 <- predict(model, fuga1[,-11])
tabsvm<-table(pred=svm.pred2, true=fuga1[,11])

classAgreement(tabsvm)

# Classification R SIN tarjetas#

library(e1071)
library(rpart)
library(rpart.plot)
fuga<-read.csv2("//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Agosto 2017//R-Sin Tarjetas.csv",head=T)

head(fuga)
names(fuga)
dim(fuga)
#[1] "Nro..Poliza"                "Capital.Asegurado"          "Prima.Minima"              
#[4] "Prima.Pactada"              "Nro..Plan"                  "Cod.Vig.Pol"               
#[7] "Cod.Edad"                   "Oficina.Agente"             "Tipo.Contrato.Agente"      
#[10] "Estado.Contrato.Agente"     "Clas..Meses.Intermediarios" "Tarjeta.2"                 
#[13] "Churn"

#Realizamos la separación de la BBDD#
fuga<-fuga[,-1]
names(fuga)


dim(fuga)

muestra <- sample(nrow(fuga), 0.6*nrow(fuga))

train <- fuga[muestra,]
dim(train)
names(train)
test  <- fuga[-muestra,]
dim(test)
names(test)
#validacion <- fuga1 #la validacion corresponde a los datos nuevos al mes correspondiente.
#dim(validacion)
#validacion[1,]

(prop<-data.frame(prop.table(table(train$Churn))))

prop.table(table(test$Churn))
prop.table(table(validacion$Churn))
prop.table(table(fuga$Churn))
 ##SVM model##

#using tune() to do automatic grid-search in CV: 
#Encuentra de forma automatica el mejor costo (parámetro de clasificación) 
#y gamma (parámetro radial para la función específica del kernel).

ir.tune<-tune(svm, Churn ~., data=test, 
              ranges=list(gamma=2^(-2:2), cost=2^(-2:2)),
              control = tune.control(sampling="cross", cross=5),kernel="radial")

summary(ir.tune)

PRUEBA <- svm(Churn ~ ., data = train, cross = 5, gamma= ir.tune$best.parameters[,1], cost=ir.tune$best.parameters[,2], kernel= "radial",method="C-classification", probability=T)

summary(PRUEBA)#Cross validacion tiene un 70% de acierto en promedio)

model <- svm(Churn ~ ., data = train, gamma= ir.tune$best.parameters[,1], cost=ir.tune$best.parameters[,2], kernel= "radial", method="C-classification", probability=T)

svm.pred <- predict(model, test[,-11], probabilities=T, decision.values = TRUE ,getOption("max.print"))


summary(model)


prob<-attr(svm.pred, "decision.values")
prob2<-attr(svm.pred, "probabilities")
#Guardar probabilidades en un excel
write.table(prob, "//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Agosto 2017//svm.pred1-DatosNuevos.txt")
write.table(prob2, "//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Agosto 2017//svm.pred2-DatosNuevos.txt")



#rpart#
rpart.model<-rpart(Churn~., data=train)
rpart.pred<-predict(rpart.model,test[,-11], type="class")

plot(rpart.model)
text(rpart.model)

rpart.plot(rpart.model, type=1, extra=100,cex = .5,
           box.col=c("gray99", "gray88")[rpart.model$frame$yval],
           main="PAT",faclen=0)
prp(rpart.model, main="default prp\n(type = 0, extra = 0)")
# matriz de confusión#
tabsvm<-table(pred=svm.pred, true=test[,11])
table(pred=rpart.pred, true=test[,11])

classAgreement(tabsvm)

#validación#
svm.pred2 <- predict(model, fuga[,-11])
tabsvm<-table(pred=svm.pred2, true=fuga[,11])

classAgreement(tabsvm)


library(lime)
iris_test <- iris[1:5, 1:4]
iris_train <- iris[-(1:5), 1:4]
iris_lab <- iris[[5]][-(1:5)]
dim(train)
dim(train$Churn[1:575])
model <- train(train, train$Churn, method = 'rf')
explainer<-lime(train, rpart.model)
explanation <- explain(test, explainer, n_labels = 1, n_features = 2)
