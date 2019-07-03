library(rpart)
library(party)
library(partykit)
library(MASS)
library(randomForest)
library(rpart.plot)
library(ROCR)
library(rattle)


fuga<-read.csv2("//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//Rechasos PAC//0-3 Meses-No Vigente.csv",head=T)
attach(fuga)
names(fuga)

#[1] "Capital.Asegurado"         "Motivo.Anulaci.n.Agrupado" "Nro..Plan"                
#[4] "Vige..Meses"               "Ciudad.Asociado"           "Provincia.Asociado"       
#[7] "Regi.n.Asociado"           "Nro..Oficina.Agente"       "Tipo.Contrato.Agente"     
#[10] "Estado.Contrato.Agente"    "Mnto.Comision.Pagada"      "Dia.Cargo"                
#[13] "Rango.Remuneracion"        "Nombre.Banco"              "Estado.P.liza"  

fuga<-fuga[fuga$Vige..Meses=="0-3 meses",]
summary(fuga)
dim(fuga)

fuga<-fuga[,-1]
names(fuga)

str(fuga)

#set.seed(127)

d <- sample(nrow(fuga), 0.7*nrow(fuga))

fuga_train <- fuga[d,] 
dim(fuga_train)
fuga_test  <- fuga[-d,]
dim(fuga_test)
#fuga_validacion <- fuga[15902:15946,] 
#dim(fuga_validacion)
prop.table(table(fuga_train$Estado.P.liza))
prop.table(table(fuga_test$Estado.P.liza))

fuga$Mnto.Comision.Pagada<-as.numeric(Mnto.Comision.Pagada)
str(fuga)
tree <- rpart(Estado.P.liza ~  ., data = fuga_train)#, control = rpart.control(cp = 0.0001))

printcp(tree)

bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

tree.pruned <- prune(tree, cp = bestcp)
tree.pruned <- prune(tree, cp = 0.0001 )

plotcp(tree)
summary(tree)
rsq.rpart(tree)



 # confusion matrix (training data)
conf.matrix <- table(fuga_train$Estado.P.liza, predict(tree.pruned, type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

(correctos <- sum(diag(conf.matrix)) / nrow(fuga_train) *100)

pred <- predict(tree, newdata=fuga_test, type="class")
library(caret)
confusionMatrix(pred, fuga_test$Estado.P.liza)

plot(tree.pruned)
text(tree.pruned, cex = 1.5, use.n = TRUE, xpd = TRUE)
prp(tree.pruned, faclen =0, cex = 0.9, extra = 1)

only_count <- function(x, labs, digits, varlen)
{
  paste(x$frame$n)
}

boxcols <- c("pink", "palegreen3")[tree.pruned$frame$yval]

par(xpd=TRUE)
prp(tree.pruned, faclen = -8, cex = 0.7, node.fun=only_count, box.col = boxcols)
legend("bottomleft", legend = c("No Potencial","Potencial"), fill = c("pink", "palegreen3"),
       title = "Grupos", cex = 0.5)


rpart.plot(tree.pruned, # middle graph
           #box.palette="Grays",
           branch.lty=8,cex = 0.8, shadow.col="gray",digits=2, faclen=100)

#fancyRpartPlot(tree.pruned)   


#probar el modelo con nuevos datos
rpart_predictions <- predict(object = tree,
                             newdata = fuga_test, 
                             type = "vector")
table<-table(rpart_predictions, fuga_test$Estado.Oficial)

(correctos <- sum(diag(table)) / nrow(fuga_test) *100)

rpart.tree<-as.party(tree.pruned)
plot(rpart.tree)
rpart.plot(tree)
# PREDICCION

predict.rpart <- predict(tree,fuga_test,type = "prob")[,2] #prob. clase=yes
predict.rocr  <- prediction(predict.rpart,fuga_test$Clas..Cant..Empleados)
perf.rocr     <- performance(predict.rocr,"tpr","fpr") #True y False postivie.rate


# GRAFICO CURVA ROC
auc <- as.numeric(performance(predict.rocr ,"auc")@y.values)
plot(perf.rocr,type='o', main = paste('Area Bajo la Curva =',round(auc,2)))  
abline(a=0, b= 1)



  
library(party)
library(partykit)
airct <- ctree(Clas..Cant..Asegurados ~ ., data = fuga_train, control = ctree_control(maxsurrogate = 10))
ctree_control(maxsurrogate = 3)

# simpler version of plot
plot(airct, type="simple",           # no terminal plots
     inner_panel=node_inner(airct,
                            abbreviate = F,            # short variable names
                            pval = T,                 # no p-values
                            id = T),                  # no id of node
     terminal_panel=node_terminal(airct, 
                                  abbreviate = F,
                                  digits = 1,                   # few digits on numbers
                                  fill = c("white"),            # make box white not grey
                                  id = T)
)

library(partykit) 
airct <- ctree(Clas..Cant..Asegurados ~ ., data = fuga)
class(airct)  # different class from before
# "constparty" "party"  
plot(airct)
plot(airct, gp = gpar(fontsize = 6),     # font size changed to 6
     inner_panel= node_inner,
     ip_args=list(
       abbreviate = F, 
       id = T)
)



########################################################3
library(party)
library(plyr)
model <- ctree(Clas..Cant..Asegurados ~ ., data = fuga_train)

plot(model, type="simple")

# predict new data
fuga_test$predClass = predict(model, newdata=fuga_test, type="response")    # obtain the class (0/1)
fuga_test$predProb = predict(model, newdata=fuga_test,type="prob") # obtain probability of class 1 (second element from the lists)
fuga_test$predNode = predict(model, newdata=fuga_test, type="node")   # obtain the predicted node (in case you need it)

table(fuga_test$predClass, fuga_test$Clas..Cant..Asegurados)  # everything is classified as 0

# pick a threshold of 0.2
#fuga_test$predClass2 = No
#fuga_test$predClass2[fuga_test$predProb >= 0.2] = Yes

#table(fuga_test$predClass2, fuga_test$Clas..Cant..Asegurados)  # you have some cases classified as 1

#Random Forest

library(randomForest)
fit <- randomForest(Clas..Cant..Asegurados ~ ., data=fuga)
print(fit) # view results 
importance(fit) # importance of each predictor
