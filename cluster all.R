library(ggplot2)
library("klaR")
library("caret")
library(e1071)
library(kernlab)
library(reshape)
library(ggplot2)
library(scales)
library(zoo)

#Ejemplo clustering#
newiris <- iris
newiris$Species <- NULL
(kc <- kmeans(newiris, 3)) 
table(iris$Species, kc$cluster)
plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=2)


#Cluster Cobradores#
Cob<-read.csv2("//nas//Datos//Liquidacion//Liquidacion 1//Ivan Echeverría//Informe CobXAsegurado Mensual//Convenios-Vigentes-Kmeans.csv",head=T)
str(Cob)
rnames(Cob)
#[1] "Convenio"                                         
#[2] "Gestionar"                                        
#[3] "Raz.n.Social.1"                                   
#[4] "Raz.n.Social.2"                                   
#[5] "Ciudad"                                           
#[6] "Sucursal"                                         
#[7] "Sector"                                           
#[8] "Rubro"                                            
#[9] "Estado.del.Convenio"                              
#[10] "Tipo.Cobranza"                                    
#[11] "Vigencia.del.Convenio"                            
#[12] "Mes.Apertura"                                     
#[13] "Hoy"                                              
#[14] "Meses.Convenio"                                   
#[15] "A.os.Convenio"                                    
#[16] "Tendencia.de.Pagos"                               
#[17] "Estado.Cobranza"                                  
#[18] "Cantidad.de.Deudas"                               
#[19] "Fecha.de.Pago"                                    
#[20] "Usuario.Web.Empresa"                              
#[21] "éltima.Descarga.Planilla"                         
#[22] "Monto.Emitido"                                    
#[23] "Categor.a.por.Potencial"                          
#[24] "Trabajadores"                                     
#[25] "Porcentaje.de.Trabajadores.Asegurados"            
#[26] "Asegurados"                                       
#[27] "Variaci.n.Mes.Anterior"                           
#[28] "Ultimo.Mes.Venta"                                 
#[29] "Meses.Venta"                                      
#[30] "Productos.Contratados.AP"                         
#[31] "Productos.Contratados.C.Garantizado...Vida.Entera"
#[32] "Productos.Contratados.Familiares"                 
#[33] "Productos.Contratados.MIS.MAES"                   
#[34] "Productos.Contratados.Oncol.gico"                 
#[35] "Motivo.Anulaci.n.éltimo.A.o.Nø.Anulaci.n.No.Pago" 
#[36] "Motivo.Anulaci.n.éltimo.A.o.Nø.Renuncias"         
#[37] "Motivo.Anulaci.n.éltimo.A.o.Nø.Fallecimientos"    
#[38] "Rango.de.Remuneraci.n.Seg.n.nuestros.registros"   
#[39] "X..Hombres"                                       
#[40] "X..Mujeres"                                       
#[41] "BAG.Otorgados"                                    
#[42] "Asesor.Relacional"                                
#[43] "Tipo.Gesti.n.Agencia"                             
#[44] "Porcentaje.de.Recaudaci.n.Mes.Anterior"           
#[45] "Planillas.Devueltas..2016"                        
#[46] "Planillas.Devueltas2017"                          
#[47] "Itermediario.que.realiza.la..ltima.venta"         
#[48] "Tipo.de.Contrato"                                 
#[49] "Sucursal.1"

potencial<-read.csv2("//nas//Datos//Liquidacion//Liquidacion 1//Ivan Echeverría//Informe CobXAsegurado Mensual//Convenios-Vigentes-glm 2.csv",head=T)
str(potencial)
names(potencial)

#Análisis descriptivo General#
cor(potencial$Trabajadores,potencial$Asegurados)
plot(potencial$Trabajadores,potencial$Asegurados)


Antofagasta<-data.frame(potencial[potencial$Sucursal=="Antofagasta",])
Arica<-data.frame(potencial[potencial$Sucursal=="Arica",])
Concepcion<-data.frame(potencial[potencial$Sucursal=="Concepcion",])
Copiapo<-data.frame(potencial[potencial$Sucursal=="Copiapo",])
Iquique<-data.frame(potencial[potencial$Sucursal=="Iquique",])
La.Serena<-data.frame(potencial[potencial$Sucursal=="La Serena",])
Puerto.Montt<-data.frame(potencial[potencial$Sucursal=="Puerto Montt",])
Punta.Arenas<-data.frame(potencial[potencial$Sucursal=="Punta Arenas",])
Quilpue<-data.frame(potencial[potencial$Sucursal=="Quilpue",])
Rancagua<-data.frame(potencial[potencial$Sucursal=="Rancagua",])
Santiago<-data.frame(potencial[potencial$Sucursal=="Santiago",])
Talca<-data.frame(potencial[potencial$Sucursal=="Talca",])
Temuco<-data.frame(potencial[potencial$Sucursal=="Temuco",])
Valdivia<-data.frame(potencial[potencial$Sucursal=="Valdivia",])
Valparaíso<-data.frame(potencial[potencial$Sucursal=="Valparaiso",])


# Análisis por sucursal#
Valparaíso$Sucursal
names(Valparaíso)
qplot(Valparaíso$Sucursal, Valparaíso$Categoria.por.Potencial,geom="jitter")

#scatterplots for each combination of two factors 
library(lattice)
xyplot(Valparaíso$Trabajadores~Valparaíso$Asegurados|Valparaíso$Sucursal*Valparaíso$Categoria.por.Potencial, 
       main="Sucursal valparaíso", 
       ylab="Cantidad de Trabajadores", xlab="Cantidad de Asegurados",
       anel = function(x, y) {
         panel.grid(h = -1, v = 2)
         panel.xyplot(x, y)
         panel.loess(x, y, span=1)
       },
       aspect = "xy")


#modelo<-glm(Cob$Trabajadores~Cob$Asegurados, gaussian(link = "identity"))

with(potencial, table(potencial$Categor.a.por.Potencial, potencial$Sucursal))

with(potencial, do.call(rbind, tapply(potencial$Trabajadores,potencial$Sucursal, function(x) c(Promedio = mean(x), Desv.Estándar = sd(x)))))

library(nnet)
#ml$prog2 <- relevel(ml$prog, ref = "academic")
#Modelo#
potencial<-potencial[,-1]
names(potencial)
modelo1 <- glm(potencial$Estado.del.Convenio ~ ., data = potencial, family = binomial)

summary(modelo1)

m <- step(modelo1)

summary(m)
# BONDAD DE AJUSTE MODELO 1 #
pred1 <- predict(m, potencial,type="response")
summary(pred1)
summary(modelo1)
potencial$predict1<-pred1
#table(datos$UF.ARRIENDO, pred)
View(potencial)
qqplot(potencial$Asegurados, pred1)
(cor<-cor(potencial$Asegurados, pred1)^2)
head(potencial)
df1<-data.frame(pred1)
write.csv2(df1, "//nas//Datos//Liquidacion//Liquidacion 1//Ivan Echeverría//Informe CobXAsegurado Mensual//prob8.csv")
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
test$m1_score <- predict(m,type='response',potencial)
m1_pred <- prediction(potencial$Estado.del.Convenio,potencial$predict1)
m1_perf <- performance(m1_pred,"tpr","fpr")

# Plot precision/recall curve
m1_perf_precision <- performance(m1_pred, measure = "prec", x.measure = "rec")
plot(m1_perf_precision, main="m1 Logistic:Precision/recall curve")

#KS, Gini & AUC m1
m1_KS <- round(max(attr(m1_perf,'y.values')[[1]]-attr(m1_perf,'x.values')[[1]])*100, 2)
m1_AUROC <- round(performance(m1_pred, measure = "auc")@y.values[[1]]*100, 2)
m1_Gini <- (2*m1_AUROC - 100)
cat("AUROC: ",m1_AUROC,"\tKS: ", m1_KS, "\tGini:", m1_Gini, "\n")

df1<-data.frame(Cob$Trabajadores,Cob$Asegurados,Cob$A.os.Convenio,Cob$Categor.a.por.Potencial)
names(df1)
#$[1] "Cob.Trabajadores"  "Cob.Asegurados"    "Cob.A.os.Convenio"
df1$Cob.Categor.a.por.Potencial <- NULL
(kc <- kmeans(df1, 4)) 
table(Cob$Categor.a.por.Potencial, kc$cluster)
plot(df1[c("Cob.Trabajadores", "Cob.Asegurados")], col=kc$cluster)
points(kc$centers[,c("Cob.Trabajadores", "Cob.Asegurados")], col=1:4, pch=8, cex=2)


library(fpc)
pamk.result <- pamk(df1)
pamk.result$nc

# check clustering against actual species

table(pamk.result$pamobject$clustering, Cob$Categor.a.por.Potencial)
layout(matrix(c(1, 2), 1, 2)) # 2 graphs per page

plot(pamk.result$pamobject)

library(cluster)
# group into 4 clusters
pam.result <- pam(df1, 4)
table(pam.result$clustering, Cob$Categor.a.por.Potencial)

layout(matrix(c(1, 2), 1, 2)) # 2 graphs per page

plot(pam.result)

#Density-based Clustering of the iris data

library(fpc)

df2 <- df1[-4] # remove class tags

ds <- dbscan(df2, eps = 0.1, MinPts = 4)

# compare clusters with original class labels

table(ds$cluster, Cob$Categor.a.por.Potencial)
plot(ds, df2)
plot(ds, df2[c(1, 3)])
plotcluster(df2,ds$cluster)


# create a new dataset for labeling

set.seed(4356)
idx <- sample(1:nrow(df1), 1000)
# remove class labels
new.data <- df1[idx,-4]
# add random noise
new.data <- new.data + matrix(runif(10*4, min=0, max=0.2),
                              nrow=10, ncol=4)

# label new data
pred <- predict(ds, df2, new.data)
table(pred, Cob$Categor.a.por.Potencial[idx])

plot(iris2[c(1, 4)], col = 1 + ds$cluster)

points(new.data[c(1, 4)], pch = "+", col = 1 + pred, cex = 3)
