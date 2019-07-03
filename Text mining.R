library(twitteR)
library(devtools)
library(SentimentAnalysis)
setup_twitter_oauth('MYBWCGeB9bDSnQDsvMCvTRt3G','C9zoTqWxgT8lGExyuTfCvveDskCQU3Ie2iDCbKQAuuKlXyBBFQ', 
                    access_token=	'1566727855-yAmWO9Ynp1yrezC3K6zDZdbtf920i1NoTPTVqRq', 
                    access_secret= 'NnZOuQRQzlSkA56pDO5UyJaaEFMgF25tz0NcjEXZweqqo')

tweets <- userTimeline("@ciepval", n = 3200)
(n.tweet <- length(tweets))

tweets<-searchTwitter("#TRUMP", lang="es", n=1000000)
#class(tweets)
#prob<-data.frame(as.character(tweets))
#write.csv2(tweets.df, "//nas//Datos//RespaldoPC//IVECLE01//1. Fugados 2016//2017//Fugados PAT Febrero 2017//Tweet.csv")
#Escribir en twitter:
tweet("big data")
tweets[1]



#mapa de seguidores
#source("http://biostat.jhsph.edu/~jleek/code/twitterMap.R")
#library(maps)
#twitterMap("mmlagoscc", fileName="twitterMap.pdf", nMax=1500)

# convert tweets to a data frame
tweets.df <- twListToDF(tweets)
dim(tweets.df)
names(tweets.df)


tweets.df$text
#[1] "text"          "favorited"     "favoriteCount" "replyToSN"     "created"      
#[6] "truncated"     "replyToSID"    "id"            "replyToUID"    "statusSource" 
#[11] "screenName"    "retweetCount"  "isRetweet"     "retweeted"     "longitude"    
#[16] "latitude" 

wordcloud(myCorpus,scale=c(4,.5),min.freq=3,max.words=Inf,
          random.order=TRUE, random.color=T, rot.per=.1,
          colors="black",ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)


for (i in c(1:2, 8814)) {
  cat(paste0("[", i, "] "))
  writeLines(strwrap(tweets.df$text[i], 60))
}

#text cleaning

tweets.df$text
utf8ToInt(tweets.df$text)
intToUtf8(tweets.df$text, multiple = FALSE)
library(tm)
# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets.df$text))
#myCorpus <- tm_map(myCorpus, content_transformer(tolower))
removeURL <- function(x) gsub("http[^[:space:]]*","", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords("SMART"))
# remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords,stopwords(""))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus,content_transformer(tolower))
myCorpus <- tm_map(myCorpus, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
library(SnowballC)
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)

# inspect the first 5 documents (tweets)
# inspect(myCorpus[1:5])
# The code below is used for to make text fit for paper width
for (i in c(1:2, 10000)) {
cat(paste0("[", i, "] "))
writeLines(strwrap(as.character(myCorpus[[i]]), 60))
}

# tm v0.5-10
# myCorpus <- tm_map(myCorpus, stemCompletion)
# tm v0.6
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpus)
myCorpus <- Corpus(VectorSource(myCorpus))

miningCases <- lapply(myCorpusCopy,
                     function(x) { grep(as.character(x), pattern = "\\<data")} )
sum(unlist(miningCases))

myCorpus <- tm_map(myCorpus, content_transformer(gsub),
                       pattern = "miner", replacement = "data")


tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(1, Inf)))
tdm

#Frequent Words and Associations

idx <- which(dimnames(tdm)$Terms == "r")
inspect(tdm[idx + (0:10), 101:110])

# inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 15))

library(ggplot2)
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 300)
library(ggplot2)
df <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
    xlab("Terms") + ylab("Count") + coord_flip()

# which words are associated with 'r'?
findAssocs(tdm, "r", 0.5)
# which words are associated with 'mining'?
findAssocs(tdm, "data", 0.5)

library(graph)
library(Rgraphviz)
plot(tdm, term = freq.terms, corThreshold = 0,5, weighting = T)

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)

#pal <- pal[-(1:4)]

# plot word cloud

library(wordcloud)
# colors
pal <- brewer.pal(8, "Dark2")
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 100,
          random.order = F, colors = pal)

# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D2") #ward.D2
plot(fit)
rect.hclust(fit,k=6)

m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 6 # number of clusters
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers

for (i in 1:k) {
   cat(paste("cluster ", i, ": ", sep = ""))
   s <- sort(kmeansResult$centers[i, ], decreasing = T)
   cat(names(s)[1:5], "\n")
   # print the tweets of every cluster
   # print(tweets[which(kmeansResult£cluster==i)])
}

#topic modelling

dtm <- as.DocumentTermMatrix(tdm)
library(topicmodels)
lda <- LDA(dtm, k = 8) # find 8 topics
(term <- terms(lda, 6)) # first 6 terms of every topic


# first topic identified for every document (tweet)
topic <- topics(lda, 1)
topics <- data.frame(date=tweets.df$created, topic)
qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")

