library(tm)
library(SnowballC)
library(wordcloud)
library(igraph)
library(fpc)
#install.packages("sm")
library(sm)
stopwords("spanish")
length(stopwords("spanish"))
library(ggplot2)
library(dplyr)
library(readr)
library(cluster)

#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "c")   

jeopQ <- read.csv2('//nas//Datos//Liquidacion//Liquidacion 1//Ivan E//OT rechazos PAC//Rechazos.csv', stringsAsFactors = FALSE)
names(jeopQ)

[1] "ID"                "P.liza"            "Tipo.Gesti.n"      "Rut.Contratante"  
[5] "DV"                "Rut.Asegurado"     "DV.1"              "Ciudad"           
[9] "Agencia.Asegurado" "Intermediario"     "Gesti.n"           "Encargado"        
[13] "Motivo.de.Rechazo" "OT"               "FECHA" 

jeopCorpus <- Corpus(VectorSource(jeopQ$Tipo.Gesti.n))
summary(jeopCorpus) 
inspect(jeopCorpus[2])

for(j in seq(jeopCorpus))   
{   
  jeopCorpus[[j]] <- gsub("/", " ", jeopCorpus[[j]])   
  jeopCorpus[[j]] <- gsub("@", " ", jeopCorpus[[j]])   
  jeopCorpus[[j]] <- gsub("\\|", " ", jeopCorpus[[j]])   
}   

jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
#jeopCorpus <- tm_map(jeopCorpus, tolower) 
jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
#jeopCorpus <- tm_map(jeopCorpus, stemDocument)
#jeopCorpus <- tm_map(jeopCorpus, removeNumbers)
jeopCorpus <- tm_map(jeopCorpus, stripWhitespace) ##Elimino los espacios dobles
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords("spanish"))
jeopCorpus <- tm_map(jeopCorpus, removeWords, c("MUTUAL", "HASTA","EDAD","PLAN", "MULT", "REAJ"))   

#Nube de palabras

wordcloud(jeopCorpus, min.freq = 4, random.order = F, colors = brewer.pal(name = "Dark2", n = 9))

#Frecuencia de palabras

nov_tdm <- TermDocumentMatrix(jeopCorpus)
nov_tdm
nov_mat <- as.matrix(nov_tdm)
dim(nov_mat)

nov_mat <- nov_mat %>% rowSums() %>% sort(decreasing = TRUE)
nov_mat <- data.frame(palabra = names(nov_mat), frec = nov_mat)

wordcloud(
  words = nov_mat$palabra, 
  freq = nov_mat$frec, 
  max.words = 80, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8)
)

nov_mat[1:20, ]

nov_mat[1:10, ] %>%
  ggplot(aes(palabra, frec)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = frec)) + 
  coord_flip() + 
  labs(title = "Diez palabras más frecuentes en Niebla",  x = "Palabras", y = "Número de usos")

#Asociaciones entre palabras

findAssocs(nov_tdm, terms = c("temuco", "antofagasta", "concepción", "santiago"), corlimit = .25)



nov_new <- removeSparseTerms(nov_tdm, sparse = .95)
nov_new <- nov_new %>% as.matrix()

nov_new <- nov_new / rowSums(nov_new)

nov_dist <- dist(nov_new, method = "euclidian")

nov_hclust <-  hclust(nov_dist, method = "ward.D")
plot(nov_hclust, main = "Dendrograma de Niebla - hclust", sub = "", xlab = "")
#rect.hclust(nov_hclust, k = 10, border="blue")

nov_agnes <- agnes(nov_dist, method = "average")
plot(nov_agnes, which.plots = 2, main = "Dendrograma de Niebla - Agnes", sub = "", xlab = "")
