# Predecir Perdida de Clientes con Arbol de Decision


# PASO 1:   Carga Package y Set de datos
# ---------------------------------------------------------------------------
library(C50)
library(rpart)
library(rpart.plot) 
data(churn); # carga tablas

Variables      <-c(4,7,16,19,17,20)               # variables elegidas
Entrenamiento  <-churnTrain[,Variables]           # tabla entrenamiento
names(Entrenamiento)
Test           <-churnTest [,Variables]           # tabla  Test


# PASO 2:   Crea Arbol de Decision
# ---------------------------------------------------------------------------
ModeloArbol<-rpart(churn ~ .,data=Entrenamiento,parms=list(split="information"))


# PASO 3:  Predice Desafiliación en datos de TEST
# ---------------------------------------------------------------------------
Prediccion <- predict(ModeloArbol, Test,type="class") # Prediccción en Test
MC         <- table(Test[, "churn"],Prediccion) # Matriz de Confusión


# PASO 4: Crea Grafico
# ---------------------------------------------------------------------------
rpart.plot(ModeloArbol, type=1, extra=100,cex = .7,
           box.col=c("gray99", "gray88")[ModeloArbol$frame$yval])

# VALIDACION CRUZADA

# PACKAGE Y DATA SET
# ----------------------------------------------------------------------------- 
library(C50) 
library(rpart) 
data(churn)
Variables  <- c(4, 7, 16, 19, 17, 20)  
datos      <- churnTrain[, Variables]  
datos      <- rbind(datos, churnTest[, Variables]) 
names(datos)


# FOLDS
# ------------------------------------------------------------------------------- 
set.seed(1)
Folds         <- 10            
datos$kfold   <- sample(1:Folds, nrow(datos), replace = T)


# MODELOS 
# -------------------------------------------------------------------------------- 
Iter   <- data.frame(iteracion = NULL, aciertos = NULL)
for (i in 1:Folds)
{
  Test          <- subset(datos, kfold  == i)
  Entrenamiento <- subset(datos, !kfold == i) 
  Modelo        <- rpart(churn ~ .,data = Entrenamiento)       
  Prediccion    <- predict(Modelo, Test, type = "class")  
  MC            <- table(Test[, "churn"],Prediccion)           
  Aciertos      <- MC[1, 1] / (MC[1, 1] + MC[2, 1])
  Iter          <- rbind(Iter, data.frame(Iter = i, acierto = Aciertos))  
}


# GRAFICO
# -------------------------------------------------------------------------------- 
promedio  <- format(mean(Iter$acierto, na.rm=TRUE)*100,digits = 4)
plot(Iter,type = "b", main = "% Prediccion en Cada Iteracion",  
     cex.axis = .7,cex.lab = .7,cex.main = .8, 
     xlab ="No. de Iteraciones", ylab="% Prediccion")
abline(h = mean(Iter$acierto), col = "blue", lty = 2)
legend("topright", legend = paste("Eficiencia de Prediccion =", promedio, "%"),
       col = "blue", lty = 2, lwd = 1, cex=.7, bg=NULL)



######DATOS REALES########
library(ggplot2)
dat<-read.csv2("C:/Users/ivecle01/Documents/Iván Echeverría/PAC-GENERAL.csv",head=T)
attach(dat)
names(dat)
dat2<-read.csv2("C:/Users/ivecle01/Documents/Iván Echeverría/PAC-GENERAL-TEST.csv",head=T)
attach(dat2)
names(dat2)

library(rpart)
library(rpart.plot)
df<-dat
names(df)
df2<-dat2
names(df2)
entrenamiento<-data.frame(df$Estado.Poliza,df$Sexo.Asociado, df$Capital.Asegurado)
names(entrenamiento)
attach(entrenamiento)
test<-data.frame(df2$Estado.Poliza,df2$Sexo.Asociado, df2$Capital.AseguradO)
names(test)
attach(test)

ModeloArbol<-rpart(df.Estado.Poliza ~ .,data=entrenamiento,parms=list(split="information"))
# PASO 3:  Predice Desafiliación en datos de TEST
# ---------------------------------------------------------------------------
Prediccion <- predict(ModeloArbol, test,type="class") # Prediccción en Test
MC         <- table(test[, "df2.Estado.Poliza"],Prediccion) # Matriz de Confusión

# PASO 4: Crea Grafico
# ---------------------------------------------------------------------------
rpart.plot(ModeloArbol, type=1, extra=100,cex = .7,
           box.col=c("gray99", "gray88")[ModeloArbol$frame$yval])


