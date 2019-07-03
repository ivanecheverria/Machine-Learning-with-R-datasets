library("riskr")
library(ggplot2)
library(InformationValue)
#data("predictions")

predictions<-read.csv2("//nas//Datos//Liquidacion//Liquidacion 1//Ivan Echeverría//Informe CobXAsegurado Mensual//Cob x Ase in R//BBDD rpart.csv",head=T)

head(predictions)
str(predictions)
Puntaje <- predictions$Clas..Cant..Asegurados

Churn <- predictions$Churn

ks(Churn, Puntaje)
## [1] 0.7157907

aucroc(Churn, Puntaje)
## [1] 0.9228714

gini(Churn, Puntaje)
## [1] 0.8457428

gg_perf(Churn, Puntaje)

gg_roc(Churn, Puntaje)
gg_gain(Churn, Puntaje)
gg_lift(Churn, Puntaje)


score <- round(predictions$Puntaje * 1000)

table_odds<-odds_table(Churn, Puntaje, nclass = 5) # default is (nclass =) 10 groups of equal size
df1<-data.frame(table_odds)

write.csv2(df1, "//nas/Datos/RespaldoPC/IVECLE01/8. Credit Scoring/credit_scoring_1.csv")
odds_table(Churn, Puntaje, breaks = c(0, 300, 700, 999))

#Matriz de confusión
Puntaje_cat <- ifelse(Puntaje < 500, 0, 1)

cm <- conf_matrix(Puntaje_cat, Churn)

cm$indicators


#cut Off

optimalCutoff(Churn, Puntaje) # returns cutoff that gives minimum misclassification error.
#[1] 0.34
optimalCutoff(Churn, Puntaje, optimiseFor="Both")  # returns cutoff that gives maximum of Youden's J Index
#[1] 0.46
sens_table <- optimalCutoff(Churn, Puntaje, optimiseFor="Both", returnDiagnostics=TRUE)$sensitivityTable

WOE(X=SimData$X.Cat, Y=SimData$Y.Binary)


#Tablas Bivariadas

fuga<-read.csv2("//nas//Datos//Liquidacion//Liquidacion 1//Ivan Echeverría//Informe CobXAsegurado Mensual//Cob x Ase in R//BBDD rpart 2.csv",head=T)

head(fuga)
names(fuga)

str(fuga)

ft(fuga$Grupo.Cobranza.Cobrador)
Cod.mes.activacion<-bt(fuga$Grupo.Cobranza.Cobrador, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Grupo.Cobranza.Cobrador.csv")

ft(fuga$Clas..Vigencia.Convenio)
Cod.mes.activacion<-bt(fuga$Clas..Vigencia.Convenio, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Clas..Vigencia.Convenio.csv")

ft(fuga$Comision.Cobranza)
Cod.mes.activacion<-bt(fuga$Comision.Cobranza, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Comision.Cobranza.csv")

ft(fuga$Medio.Magnetico)
Cod.mes.activacion<-bt(fuga$Medio.Magnetico, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Medio.Magnetico.csv")

ft(fuga$Categor.a)
Cod.mes.activacion<-bt(fuga$Categor.a, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Categor.a.csv")

ft(fuga$Paga)
Cod.mes.activacion<-bt(fuga$Paga, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Paga.csv")

ft(fuga$Condici.n.de.Pago)
Cod.mes.activacion<-bt(fuga$Condici.n.de.Pago, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Condici.n.de.Pago.csv")

ft(fuga$Morosidad)
Cod.mes.activacion<-bt(fuga$Morosidad, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Morosidad.csv")

ft(fuga$Suspende.Descuentos)
Cod.mes.activacion<-bt(fuga$Suspende.Descuentos, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Suspende.Descuentos.csv")

ft(fuga$Acepta.Producci.n)
Cod.mes.activacion<-bt(fuga$Acepta.Producci.n, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Acepta.Producci.n.csv")

ft(fuga$Marcado)
Cod.mes.activacion<-bt(fuga$Marcado, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Marcado.csv")

ft(fuga$Oficina.Centralizador)
Cod.mes.activacion<-bt(fuga$Oficina.Centralizador, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Oficina.Centralizador.csv")

ft(fuga$Sector)
Cod.mes.activacion<-bt(fuga$Sector, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Sector.csv")

ft(fuga$Clas..Tasa.recaudacion.)
Cod.mes.activacion<-bt(fuga$Clas..Tasa.recaudacion., fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Clas..Tasa.recaudacion..csv")

ft(fuga$Clas..Tasa.Asegurado)
Cod.mes.activacion<-bt(fuga$Clas..Tasa.Asegurado, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Clas..Tasa.Asegurado.csv")

ft(fuga$Clas..Cant..Asegurados)
Cod.mes.activacion<-bt(fuga$Clas..Cant..Asegurados, fuga$Estado.Oficial)
df1<-data.frame(Cod.mes.activacion)
write.csv2(df1, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual//Cob x Ase in R/Clas..Cant..Asegurados.csv")


library("ggplot2")

fuga$Tasa.Anulaci.n<- cut_interval(fuga$Tasa.Anulaci.n, 4)
gg_ba(fuga$Tasa.Anulaci.n, fuga$Estado.Oficial)
gg_ba2(fuga$Tasa.Anulaci.n, fuga$Estado.Oficial) + ggtitle("Estado Oficial")

fuga$Cuenta.Polizas<- cut_interval(fuga$Cuenta.Polizas, 2,include.lowest = TRUE)
head(fuga$Cuenta.Polizas)
gg_ba(fuga$Cuenta.Polizas, fuga$Churn)
gg_ba2(fuga$Cuenta.Polizas, fuga$Churn) + ggtitle("Cuenta pólizass")


#Ranking de variables predictivas
ranks <- pred_ranking(fuga, "Estado.Oficial",verbose = TRUE)
head(ranks)
dfranks<-data.frame(ranks)
write.csv2(dfranks, "//nas/Datos/Liquidacion//Liquidacion 1/Ivan Echeverría/Informe CobXAsegurado Mensual/Cob x Ase in R/ranks.csv")

