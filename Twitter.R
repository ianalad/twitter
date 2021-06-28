###################################################################
########### Get tweets by keywords + sentiment analysis ###########
###################################################################

rm(list = ls())
library("rtweet")
library("tm")
library("RColorBrewer")
library("wordcloud")
library("ROAuth")
library("twitteR")
library('RCurl')
library("tidytext")

# Twitter Info
key = ""
secret = ""

#Twitter API info and call the whole object authenticate
authenticate <-  OAuthFactory$new(
  consumerKey = key,
  consumerSecret = secret,
  requestURL = 'https://api.twitter.com/oauth/request_token',
  accessURL = 'https://api.twitter.com/oauth/access_token',
  authURL = 'https://api.twitter.com/oauth/authorize'
)

# this will get to a Twitter Site where we can obtain the PIN
authenticate$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# insert the PIN from Twitter

library("httr")

access_token = ""
access_secret = ""

library("rtweet")

#Give the app name
twitter_token <-
  create_token(
    app = "",
    consumer_key = key,
    consumer_secret = secret,
    access_token,
    access_secret
  )
use_oauth_token(twitter_token)

#Obtain the required tweets by key words
tweets <- search_tweets(
  "disneyplus AND disney+",
  include_rts = FALSE,
  n = 2000,
  lang = "en",
  reply_to_status_id = NULL,
  until = "2019-09-19"
)

corpus <- Corpus(VectorSource(tweets$text))
clearCorpus <-
  tm_map(corpus, function(x)
    iconv(enc2utf8(x), sub = "byte"))

tdm <- TermDocumentMatrix(
  clearCorpus,
  control =
    list(
      removePunctuation = TRUE,
      stopwords = c("com", "https", stopwords("english")),
      removeNumbers = TRUE,
      tolower = TRUE
    )
)

m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
dm <- data.frame(word = names(word_freqs), freq = word_freqs)

#Wordcloud of most repeated words
wordcloud(
  dm$word,
  dm$freq,
  scale = c(3, .5),
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)

#Sentiment analysis of tweets
sentiment_analysis <- get_sentiments("afinn")
#'arg' should be one of “bing”, “afinn”, “loughran”, “nrc”

hist(
  sentiment_analysis$value,
  main = c("Sentiment Analysis of Tweets"),
  xlab = c("Sentiment Analysis Value")
)
