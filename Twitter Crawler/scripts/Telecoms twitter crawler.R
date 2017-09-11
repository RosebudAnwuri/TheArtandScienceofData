library(twitteR)
library(base64enc)
library(RCurl)
library(httr)
library(RJSONIO)
library(stringr)


api_key <- "UE0sCwrNmxHb8YL759R7SuLEc" # From dev.twitter.com
api_secret <- "C9OxWPmBAOwzQ6G4VXbCKeXd3XEHG5XvJjzTA1AVLoKbtwnpJy" # From dev.twitter.com
token <- "370018889-WKxIRFsc8OJhvdtW3BOOdgIy1qGco48d7QlUO0in" # From dev.twitter.com
token_secret <- "7Ah8qplWJf5ey4zB4IPTTBlypMCUenXnQsrCH7808UbRE" # From dev.twitter.com

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)
set_config(config (ssl_verifypeer= 0L))
date_time = Sys.Date()-1
date_time = as.character.Date(date_time)
tweetdetails <- searchTwitter("etisalat network -from:etisalat_9ja", n=1500, lang="en", since = date_time,until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
tweetData_network = twListToDF(tweetData)
tweetdetails <- searchTwitter("etisalat data -from:etisalat_9ja", n=1500, lang="en", since = "2016-05-03",until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
tweetData = append(tweetData, tweetdetails)

tweetData = twListToDF(tweetData)
tweetDataFinal = rbind(tweetData, tweetData_network)
tweetdetails <- searchTwitter("etisalat call -from:etisalat_9ja", n=1500, lang="en", since = "2016-05-03",until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
tweetData = twListToDF(tweetData)
etisalat = rbind(tweetData, tweetDataFinal)
etisalat$service_provider = "Etisalat"

#MTN
tweetdetails <- searchTwitter("mtn network -from:mtnng", n=1500, lang="en", since = date_time,until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
tweetData_network = twListToDF(tweetData)
tweetdetails <- searchTwitter("mtn data -from:mtnng", n=1500, lang="en", since = "2016-05-03",until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
tweetData = append(tweetData, tweetdetails)

tweetData = twListToDF(tweetData)
tweetDataFinal = rbind(tweetData, tweetData_network)
tweetdetails <- searchTwitter("mtn call -from:mtnng", n=1500, lang="en", since = "2016-05-03",until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
tweetData = twListToDF(tweetData)
mtn = rbind(tweetData, tweetDataFinal)
mtn$serviceProvider = "MTN"

#Airtel
tweetdetails <- searchTwitter("airtel network -from:airtelnigeria", n=1500, lang="en", since = date_time,until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
tweetData_network = twListToDF(tweetData)
tweetdetails <- searchTwitter("airtel data -from:airtelnigeria", n=1500, lang="en", since = "2016-05-03",until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
tweetData = append(tweetData, tweetdetails)

tweetData = twListToDF(tweetData)
tweetDataFinal = rbind(tweetData, tweetData_network)
tweetdetails <- searchTwitter("airtel call -from:airtelnigeria", n=1500, lang="en", since = "2016-05-03",until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
tweetData = twListToDF(tweetData)
airtel = rbind(tweetData, tweetDataFinal)
airtel$service_provider = "Airtel"
#Glo
tweetdetails <- searchTwitter("glo network -from:gloworld", n=1500, lang="en", since = date_time,until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
tweetData_network = twListToDF(tweetData)
tweetdetails <- searchTwitter("glo data -from:gloworld", n=1500, lang="en", since = "2016-05-03",until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
tweetData = append(tweetData, tweetdetails)

tweetData = twListToDF(tweetData)
tweetDataFinal = rbind(tweetData, tweetData_network)
tweetdetails <- searchTwitter("glo call -from:gloworld", n=1500, lang="en", since = "2016-05-03",until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
tweetData = twListToDF(tweetData)
glo = rbind(tweetData, tweetDataFinal)
glo$service_provider = "Glo"  

final_file = rbind(glo,etisalat,mtn,airtel)
write.csv(final_file,paste0("CMT DATA_"),Sys.Date(),".csv",row.names = F)