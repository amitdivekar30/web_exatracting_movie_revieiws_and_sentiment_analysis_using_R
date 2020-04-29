


library(rvest)
library(magrittr)
library(XML)

joker<-NULL
url1<-"http://www.imdb.com/title/tt7286456/reviews?ref_=tt_ov_rt"
for(i in 0:5){
  url<-read_html(as.character(paste(url1,i,sep="")))
  ping<-url %>%
    html_nodes(".show-more__control") %>%
    html_text() 
  joker<-c(joker,ping)
}
joker
write.csv(joker,file="joker.csv")
getwd()
write.table(joker,"joker.txt")


#install.packages("tm")
library(tm)

#install.packages("slam")
library(slam)


#install.packages("topicmodels")
library(topicmodels)

x <- readLines("joker.txt")
x
length(x)

mydata.corpus <- Corpus(VectorSource(x))
#inspect(VCorpus(mydata.corpus))
mydata.corpus <- tm_map(mydata.corpus, removePunctuation)

stopwords<-readLines("stop.txt")
my_stopwords <- c(stopwords)
mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)

mydata.corpus <- tm_map(mydata.corpus, removeNumbers)

mydata.corpus <- tm_map(mydata.corpus, stripWhitespace)

## build a term-document matrix
mydata.dtm3 <- TermDocumentMatrix(mydata.corpus)
mydata.dtm3
#View(as.matrix(mydata.dtm3))
dim(mydata.dtm3)

# dtm <- as.DocumentTermMatrix(mydata.dtm3)
# dtm <- DocumentTermMatrix(mydata.corpus)
dtm <- t(mydata.dtm3)

rowTotals <- apply(dtm, 1, sum)
?apply


dtm.new   <- dtm[rowTotals > 0, ]
dim(dtm.new)

tdm<-t(dtm.new)

#####################wordcloud#####################################

#install.packages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)

matrix <- as.matrix(tdm) 

words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
View(df)
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))       




####################### Sentimet Analysis ##############################

#install.packages("syuzhet")
library("syuzhet")

text <- readLines("joker.txt")

s_v <- get_sentences(text)
class(s_v)
str(s_v)
head(s_v)

sentiment_vector <- get_sentiment(s_v, method = "bing")
head(sentiment_vector)

afinn_s_v <- get_sentiment(s_v, method = "afinn")

head(afinn_s_v)

nrc_vector <- get_sentiment(s_v, method="nrc")
head(nrc_vector)

sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

# plot
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

# To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

# and to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive

# more depth
poa_v <- text
poa_sent <- get_sentiment(poa_v, method="bing")
plot(
  poa_sent, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# percentage based figures
percent_vals <- get_percentage_values(poa_sent)

plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values <- get_transformed_values(
  poa_sent, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(s_v)
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment('grim')
# subset

sad_items <- which(nrc_data$sadness > 0)
head(s_v[sad_items])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:10]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)




