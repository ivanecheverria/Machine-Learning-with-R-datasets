# cargar librerias
library(twitteR)
library(tm)
library(wordcloud)

# recolecta tweets de @
tweets = userTimeline("", 2000)

#recolecta hashtag
Tweets = searchTwitter("#TRUMP", n=200000, lang = "es")

# vuelca la informacion de los tweets a un data frame
df = twListToDF(Tweets)

# obtiene el texto de los tweets
txt = df$text

##### inicio limpieza de datos #####
# remueve retweets
txtclean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txt)
# remove @otragente
txtclean = gsub("@\\w+", "", txtclean)
# remueve simbolos de puntuación
txtclean = gsub("[[:punct:]]", "", txtclean)
# remove números
txtclean = gsub("[[:digit:]]", "", txtclean)
# remueve links
txtclean = gsub("http\\w+", "", txtclean)
##### fin limpieza de datos #####

# construye un corpus
corpus = Corpus(VectorSource(txtclean))

# convierte a minúsculas
corpus = tm_map(corpus, tolower)
# remueve palabras vacias (stopwords) en español
corpus = tm_map(corpus, removeWords, c(stopwords("spanish"), "presidencia_cl"))
# carga archivo de palabras vacias personalizada y lo convierte a ASCII
sw <- read.table("C:/Users/Iv�n/Documents/twitter/diccionario.txt")#,encoding="UTF-8")
sw <- readLines("C:/Users/Iv�n/Documents/twitter/diccionario.txt",encoding="UTF-8")
sw = iconv(sw, to="ASCII//TRANSLIT")
# remueve palabras vacias personalizada
corpus = tm_map(corpus, removeWords, sw)

# eliminamos algunas palabras que se repiten mucho y tienen baja relevancia
corpus  <- tm_map(corpus , removeWords, c("False","follow","client","<",">",
                                "/a","href", "rel", "twittwe",
                                "doscientos", "san", "bío", "alta", "alto",
                                "100", "113", "200", "300", "400", "450",
                                "2006", "2010", "2014", "2015","2016",
                                "2017", "2018", "marzo", "mayo", "ciento",
                                "cientos","mitad"))

# remove espacios en blanco extras
corpus = tm_map(corpus, stripWhitespace)

# crea una matriz de términos
tdm <- TermDocumentMatrix(corpus)

# convierte a una matriz
m = as.matrix(tdm)

# conteo de palabras en orden decreciente
wf <- sort(rowSums(m),decreasing=TRUE)

# crea un data frame con las palabras y sus frecuencias
dm <- data.frame(word = names(wf), freq=wf)

# grafica la nube de palabras (wordcloud)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
