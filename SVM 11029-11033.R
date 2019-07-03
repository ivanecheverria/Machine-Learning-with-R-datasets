library("klaR")
library("caret")
library(e1071)
library(kernlab)
library(reshape)
library(ggplot2)
library(scales)
library(zoo)

fuga<-read.csv2("//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Enero 2017//701-706//11029-11033.csv",head=T)

head(fuga)
names(fuga)

#[1] "Nro..Poliza"            "NøPlan"                 "NøCobrador"            
#[4] "Fecha.vigencia"         "Cod.Mes.Activacion"     "Cod.Edad"              
#[7] "Cod.Mes.Inter"          "Cuenta.Polizas"         "Cuenta.Vigentes"       
#[10] "Cuenta.Anuladas"        "Cuenta.Ayudas"          "Cuenta.No.Pagos"       
#[13] "Sexo.Asociado"          "Agencia"                "Tipo.Contrato.Agente"  
#[16] "Estado.Contrato.Agente" "Churn" 

dim(fuga)
attach(fuga)

#x<-Sys.Date()
#date2<-substr(x,1,7)
#mm<-"01"
#sep<-"-"
#d<-paste0(date2,sep,mm)
d<-as.Date("2017-01-01")
fuga1<-fuga[as.Date(fuga$Fecha.vigencia,"%d-%m-%Y") == d,]#Reconocimiento de datos con fecha de vigencia 

dim(fuga1)

fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)
fuga<-fuga[,-1]
names(fuga)

fuga1<-fuga1[,-1]
names(fuga1)
fuga1<-fuga1[,-1]
names(fuga1)
fuga1<-fuga1[,-1]
names(fuga1)
fuga1<-fuga1[,-1]
names(fuga)

dim(fuga1)
dim(fuga)

muestra <- sample(nrow(fuga), 0.66*nrow(fuga))

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
              ranges=list(gamma=2^(-2:2), cost=2^(-2:2)),
              control = tune.control(best.model=TRUE, performance=TRUE,sampling="cross", cross=5),kernel="linear")

summary(ir.tune)

#plot(ir.tune, transform.x = log2, transform.y = log2,main="Contour SVM - German Credit Data",color.palette =terrain.colors)
#plot(ir.tune, type = "contour", main="Contour SVM - German Credit Data")

#ir.tune$best.parameters[,1]#gamma
#ir.tune$best.parameters[,1]#cost

#library(SVMMaj) revisar esta librería que entrega los gráficos de correctas predicciones.
#model2 <- svmmaj(fuga[1:11000,],fuga_train$Churn[1:11000],hinge='quadratic', lambda=1)
#q4 <- predict(model2,fuga_train[1:11000,],fuga$Churn[1:11000],show.plot=TRUE)


PRUEBA <- svm(Churn ~., data = fuga_train, cross = 5, gamma= ir.tune$best.parameters[,1], cost=ir.tune$best.parameters[,1], kernel= "linear",method="C-classification", probability=T,class.weights = c(No = prop[1,2], Yes = prop[2,2]))

summary(PRUEBA)#Cross validacion tiene un 90% de acierto en promedio

model <- svm(Churn ~., data = fuga_train, gamma= ir.tune$best.parameters[,1], cost=ir.tune$best.parameters[,1], kernel= "linear", method="C-classification", probability=T,class.weights = c(No = prop[1,2], Yes = prop[2,2]))

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
# Add colors
plot(svm.auc, colorize=TRUE)

# Add threshold labels 
plot(svm.auc, colorize=TRUE, print.cutoffs.at=seq(0,2,by=0.2), text.adj=c(-0.2,1.7))
abline(0,1)
#plot(svm.auc, avg="vertical", lwd=3, col="blue")
#abline(0,1,col="grey")


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
write.table(prob, "//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Enero 2017//701-706//Indicador-Global-histórico-11029-11033.txt")
write.table(prob2, "//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Enero 2017//701-706//Probabilidad-Global-histórico-11029-11033.txt")


#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
#####VALIDACION#####
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
       
prediction <- predict(model, fuga_validacion[,-dim(fuga_validacion)])

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
write.table(prob, "//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Enero 2017//701-706//Indicador-Global-DatosNuevos-11029-11033.txt")
write.table(prob2, "//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Enero 2017//701-706//Probabilidad-Global-DatosNuevos-11029-11033.txt")


