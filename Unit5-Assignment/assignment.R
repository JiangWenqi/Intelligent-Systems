# install.packages("tm")
# install.packages("SnowballC")
# install.packages("wordcloud")
# install.packages("syuzhet")
# Load
library("tm") # for text mining
library(tidyverse) # for data processing
library(plyr)
library("SnowballC") # for text stemming, reduces words to their root form
library("wordcloud") # word-cloud generator
library("syuzhet") # for sentiment analysis
library(udpipe) # tokenization, Parts of Speech Tagging, Lemmatization and Dependency
library(lattice) # for bar plot etc.

setwd("/Users/vinci/git/Intelligent-Systems/Unit5-Assignment")
getwd()

###################################################
################# Columns #########################
###################################################
# asin - ID of the product, like B000FA64PK
# helpful - helpfulness rating of the review - example: 2/3.
# overall - rating of the product.
# reviewText - text of the review (heading).
# reviewTime - time of the review (raw).
# reviewerID - ID of the reviewer, like A3SPTOKDG7WBLN
# reviewerName - name of the reviewer.
# summary - summary of the review (description).
# unixReviewTime - unix timestamp.


#################################################
######### 1. Data Pre-processing ################
#################################################
rawData <- read.csv('data/kindle_reviews.csv') %>%
  group_by(overall) %>%
  sample_frac(.01)

rawData <- data.frame(rawData)

rawData$index <- row.names(rawData)

rawData %>%
  group_by(reviewText) %>%
  summarize(n_reviews = n()) %>%
  mutate(pct = n_reviews / sum(n_reviews)) %>%
  arrange(-n_reviews) %>%
  top_n(10, n_reviews)



# group by overall column, select 1% rows randomly and drop null value
# preprocessedData <- rawData  %>%
#   drop_na(X, summary, reviewText, overall) %>%
#   select(X, summary, reviewText, overall)
#
# # Quick View of the preprocessedData
# head(preprocessedData)

#################################################
######### 2. Word Normalization #################
#################################################
# Load the data as a corpus
ReviewCorpus  <- Corpus(VectorSource(rawData$reviewText))

# Replacing "/", "@" and "|" with space
toSpace <-
  content_transformer(function (x , pattern)
    gsub(pattern, " ", x))
ReviewCorpus <- tm_map(ReviewCorpus, toSpace, "/")
ReviewCorpus <- tm_map(ReviewCorpus, toSpace, "@")
ReviewCorpus <- tm_map(ReviewCorpus, toSpace, "\\|")
# Convert the text to lower case
ReviewCorpus <- tm_map(ReviewCorpus, content_transformer(tolower))
# Remove punctuation
ReviewCorpus <- tm_map(ReviewCorpus, removePunctuation)
# Remove numbers
ReviewCorpus <- tm_map(ReviewCorpus, removeNumbers)
# Remove extra white spaces
ReviewCorpus <- tm_map(ReviewCorpus, stripWhitespace)
# Remove English common stop words
ReviewCorpus <-
  tm_map(ReviewCorpus, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stop words as a character vector
# ReviewCorpus <- tm_map(ReviewCorpus, removeWords, c("publisher", "s"))
# Text stemming - which reduces words to their root form
ReviewCorpus <- tm_map(ReviewCorpus, stemDocument)

normalWords <- ldply (ReviewCorpus, data.frame)
names(normalWords)[1] <- 'cleanReviewText'
normalWords$index <- row.names(normalWords)
head(normalWords$cleanReviewText)
preprocessedData <-
  merge(rawData, normalWords, by = "index", all = TRUE)



head(preprocessedData)

# -----------------------------------------------------------------
# show the most frequent terms and their frequencies in a bar plot.
# TDM
tdm <- TermDocumentMatrix(ReviewCorpus)
tdm <- as.matrix(tdm)
freq = rowSums(tdm)
freq.high = tail(sort(freq), n = 30)
freq.df = as.data.frame(freq.high)
freq.df$names <- rownames(freq.df)
ggplot(freq.df, aes(reorder(names, freq.high), freq.high)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Terms") +
  ylab("Frequency") +
  ggtitle("Term frequencies")



# -------------------------------------------------------------------------
# regular sentiment score using get_sentiment() function and method of your choice
syuzhet_vector <-
  get_sentiment(preprocessedData$cleanReviewText, method = "syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)
# bing
bing_vector <-
  get_sentiment(preprocessedData$cleanReviewText, method = "bing")
head(bing_vector)
summary(bing_vector)
#affin
afinn_vector <-
  get_sentiment(preprocessedData$cleanReviewText, method = "afinn")
head(afinn_vector)
summary(afinn_vector)
#compare the first row of each vector using sign function
rbind(sign(head(syuzhet_vector)),
      sign(head(bing_vector)),
      sign(head(afinn_vector)))
# run nrc sentiment analysis to return data frame with each row classified
# as one of the following emotions, rather than a score:
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust
# It also counts the number of positive and negative emotions found in each row
d <- get_nrc_sentiment(preprocessedData$cleanReviewText)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe

head (d, 10)
#transpose
td <- data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:9826]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2 <- td_new[1:8, ]
#Plot One - count of words associated with each sentiment
quickplot(
  sentiment,
  data = td_new2,
  weight = count,
  geom = "bar",
  fill = sentiment,
  ylab = "count"
) + ggtitle("Survey sentiments")

#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Text",
  xlab = "Percentage"
)
### Keywords

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model,
                  x = preprocessedData$cleanReviewText,
                  doc_id = preprocessedData$index)
x <- as.data.frame(x)
stats <- keywords_rake(
  x = x,
  term = "lemma",
  group = "doc_id",
  relevant = x$upos %in% c("NOUN", "ADJ")
)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(
  key ~ rake,
  data = head(subset(stats, freq > 3), 20),
  col = "cadetblue",
  main = "Keywords identified by RAKE",
  xlab = "Rake"
)
