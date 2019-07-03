
library(cluster)

Cobradores<-read.csv2("//nas//Datos//Liquidacion//Liquidacion 1//PAMELA//75%//1. Análisis 75% (Período Junio 2017)//Prueba T.csv",head=T)
str(Cobradores)
names(Cobradores)
attach(Cobradores)

[1] "Sucursal"                       "Cobrador"                       "Raz.n.Social"                  
[4] "Meses.Regularizados"            "Cobranza"                       "Mto..Recaudado"                
[7] "Mto..No.Recaudado"              "Mes.Prima"                      "X..Recaudaci.n"                
[10] "X..Recaudaci.n.T.1"             "X..Recaudaci.n.T.2"             "Tasa.No.Pago.Mes.prima"        
[13] "Cantidad.de.Asegurados.07.2017" "Producci.n"                     "Anulaci.n"                     
[16] "X0.a.3.meses"                   "X4.a.6.meses"                   "M.s.de.6.meses"                
[19] "X..0.a.3.meses"                 "X..4.a.6.meses"                 "X..M.s.de.6.meses"  

modelo<-glm(Cobranza~Mto..No.Recaudado+X..Recaudaci.n+X..Recaudaci.n.T.1+
            Cantidad.de.Asegurados.07.2017+Sucursal
            ,data = Cobradores ,family = "Gamma")

summary(modelo)

cor(modelo$fitted.values,Cobranza)^2
plot(modelo$fitted.values,Cobranza)
summary(modelo)


# BONDAD DE AJUSTE MODELO 1 #
pred <- predict(modelo, Cobradores,probability=T)
summary(pred)
summary(modelo1)
#table(data$UF.ARRIENDO, pred)
#qqplot(data$predict1)

plot(Cobranza,Mto..No.Recaudado)
cor(Cobranza,Mto..No.Recaudado)

step(modelo)


#Guardar probabilidades en un excel
write.table(pred, "//nas//Datos//Liquidacion//Liquidacion 1//PAMELA//75%//1. Análisis 75% (Período Junio 2017)//Prob Global.txt")

#Análisis multietiqueta#

install.packages("utiml") 
library(utiml)

ds <- create_holdout_partition(emotions) 
head(ds)
brmodel <- br(ds$train, "SVM", seed=123) 
prediction <- predict(brmodel, ds$test)

result <- multilabel_evaluate(ds$tes, prediction)

