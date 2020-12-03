library(rtweet)
library(leaflet)
library(quanteda)
library(readr)

# COLLECT TWEETS
mytoken <- create_token(
  app = "", #app name here
  consumer_key = "", #consumer key here
  consumer_secret = "", #consumer secret here
  access_token = "", #access token here
  access_secret = "") #access secret here

tweets <- search_tweets("#SharpieGate", n = 10000, retryonratelimit = TRUE, token=mytoken) 

write_as_csv(tweets,"tweets.csv")

# GET GEO CODES
geocodes <- lat_lng(tweets)

geocodes <- geocodes[!is.na(geocodes$lat),] 

write_as_csv(geocodes,"geocodes.csv") #don't forget to copy this saved file to your app directory


?dfm()
?textplot_wordcloud()
