library(twitteR)
library(base64enc)
library(RCurl)
library(httr)
library(RJSONIO)
library(stringr)
library(dplyr)


api_key <- "wn7M23NbDoOAWogW3KpebX5Cq" # From dev.twitter.com
api_secret <- "cwynWugYkE3Dn63qgKapLgxNegEPJWfquiqWfibBeQTmRYWxvG" # From dev.twitter.com
token <- "370018889-n8oOECLBk89f19bvU9uUt2nlISAnLqF9cfczjTaf" # From dev.twitter.com
token_secret <- "blujMfPlQLI52RH3vvzUAJBvWgdzObSxWKOAwgPFyvlX6"
# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)
token <- get("oauth_token", twitteR:::oauth_cache)
token$cache()
set_config(config (ssl_verifypeer= 0L))
date_time = Sys.Date()
before_time = as.character.Date(date_time-1)
after_time =as.character.Date(date_time)

tweetdetails <- searchTwitter("etisalat -from:etisalat_9ja", n=5000, lang="en", since = before_time,until= after_time, geocode = '10,9, 200km') 
etisalat = twListToDF(tweetdetails)

etisalat$service_provider = "Etisalat"

#MTN
tweetdetails <- searchTwitter("mtn -from:mtnng", n=5000, lang="en", since = before_time,until= after_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
mtn = twListToDF(tweetdetails)
mtn$service_provider = "MTN"

#Airtel
tweetdetails <- searchTwitter("airtel -from:airtelnigeria", n=5000, lang="en", since = before_time,until= after_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
airtel = twListToDF(tweetdetails)
airtel$service_provider = "Airtel"

#Glo
tweetdetails <- searchTwitter("glo -from:gloworld", n=5000, lang="en", since = before_time,until= after_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
glo = twListToDF(tweetdetails)
glo$service_provider = "Glo"  

final_file = rbind(glo,etisalat,mtn,airtel)

telecoms_db = src_sqlite("C:/Users/rose.anwuri/Documents/TheArtandScienceofData/Telecoms tweets database",create = F)
db_insert_into(con = telecoms_db$con,table = "final_file",values = final_file)
