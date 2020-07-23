library(twitteR)
library(ROAuth)
cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

library(base64enc)
library(httpuv)

twitteR:::setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                              "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                              "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                              "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret
#registerTwitterOAuth(cred)

Tweets <-userTimeline('@Sachin_rt', n=3200,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Tweets.txt",row.names = F)
getwd()
setwd("F:/Data Sci/R Programming/Assignment/Text Mining")


##### Preprocessing of Data #####
raw_text <- TweetsDF
View(raw_text)
text <- raw_text$text
View(text)
## Clean data by removing retweet entities,etc.
txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",text)


## Clean data by removing people name
txt2 <- gsub("@\\w+","",txt1)


## Clean data by removing punctuations
txt3 <- gsub("[[:punct:]]","",txt2)

## Clean data by removing digits
txt4 <- gsub("[[:digit:]]","",txt3)

## Clean data by removing emojis
txt5 <- gsub("<.*>", "",txt4)

## Converting  remove emoji and bizarre signs
txt6 <- iconv(from = "latin1", to = "ASCII", sub="",txt5)

## Clean data by removing html links
txt7 <- gsub("http\\w+","",txt6)

write.csv(txt7,"Tweets1.csv")



library(tm)
library(wordcloud)
corpus <- Corpus(VectorSource(txt7))
# clean up the corpus using tm_map()
corpus_clean <- tm_map(corpus, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("English"))
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
## Clean data my removing html links
removeURL <- function(x) gsub("http[[:alnum:]]*",'',x)
corpus_clean <- tm_map(corpus_clean, content_transformer(removeURL))
inspect(corpus_clean[1:10])
## Senmatic Analysis
## Build a term-document matrix
dtm <- DocumentTermMatrix(corpus_clean)
rowTotals <- apply(dtm, 1, sum)
?apply

dtm.new   <- dtm[rowTotals > 0, ]
library(topicmodels)
lda <- LDA(dtm.new, 10)
term <- terms(lda, 20)
tops <- terms(lda)
tb <- table(names(tops), unlist(tops))
tb <- as.data.frame.matrix(tb)
tb
cls <- hclust(dist(tb), method = 'ward.D2') #ward is absolute distance
?hclust
par(family = "HiraKakuProN-W3")
plot(cls)
## Plot WordCloud
pal=brewer.pal(8,"Dark2")
wordcloud(corpus_clean,min.freq = 5,max.words =Inf,width=1000,height=1000,
          random.order=F,color=pal)


############# Sentimental Analysis ###############
library(syuzhet)
senti_text <- readLines("F:/Data Sci/R Programming/Assignment/Text Mining/Tweets1.csv")
senti_text

sv <- get_sentences(senti_text)
class(sv)
str(sv)
head(sv)

sentiment_vector <- get_sentiment(sv, method = "bing")
head(sentiment_vector)

afinn_sv <- get_sentiment(sv, method = "afinn")
head(afinn_sv)

nrc_vector <- get_sentiment(sv, method="nrc")
head(nrc_vector)

sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

# plot
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Score", ylab = "Emotional Valence")
abline(h = 0, col = "red")

# To extract the sentence with the most negative emotional valence
negative <- sv[which.min(sentiment_vector)]
negative

# To extract the most positive sentence
positive <- sv[which.max(sentiment_vector)]
positive

### Categorize each sentence by eight emotions
mysentiment <- get_nrc_sentiment(sv)
SentimentScores <- data.frame(colSums(mysentiment[,]))
View(SentimentScores)

## Extract sentence of sadness emotion
sad_items <- which(mysentiment$sadness > 0)
head(sv[sad_items])

## Visualization of Emotions
## Barplot
barplot(colSums(prop.table(mysentiment)),main="Sentiment Score based on Sachin's Tweets",
        las=2.5,ylab="Percentage Score",col=rainbow(10))
