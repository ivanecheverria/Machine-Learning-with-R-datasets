library(devtools)
library(httr) #solución error curlR de conexión
set_config(config(ssl_verifypeer = 0L)) # no considera autenticación
devtools::install_github("thomasp85/lime")

datos<-read.csv("C:/Users/ivecle01/Documents/Iván Echeverría/Complex machine learning models with LIME/2015.csv", head=T)
names(datos)
# configure multicore#
library(doParallel)
cl<-makeCluster(detectCores())
registerDoParallel(cl)

library(caret)

set.seed(42)
index<-createDataPartition(datos$Happiness.Score, p=0.7,list=FALSE)
train_data<-datos[index,]
test_data<-datos[-index,]

set.seed(42)
model_mlp<-caret::train(Happiness.Score ~ .,
                        data= train_data,
                        method="mlp",
                        trControl=trainControl(method="repeatedcv",
                                               numbers=10,
                                               repeats=5,
                                               verboseIter = FALSE))


#The Explain Function#

library(lime)

explain<-lime(train_data, mlp, bin_continuous = T, n_bins = 5, n_permutations = 1000)



