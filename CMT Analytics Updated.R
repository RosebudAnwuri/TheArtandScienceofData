library(twitteR)
library(base64enc)
library(RCurl)
library(httr)
library(RJSONIO)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)
library(tidytext)


api_key <- "UE0sCwrNmxHb8YL759R7SuLEc" # From dev.twitter.com
api_secret <- "C9OxWPmBAOwzQ6G4VXbCKeXd3XEHG5XvJjzTA1AVLoKbtwnpJy" # From dev.twitter.com
token <- "370018889-WKxIRFsc8OJhvdtW3BOOdgIy1qGco48d7QlUO0in" # From dev.twitter.com
token_secret <- "7Ah8qplWJf5ey4zB4IPTTBlypMCUenXnQsrCH7808UbRE" # From dev.twitter.com

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)
set_config(config (ssl_verifypeer= 0L))
date_time = Sys.Date()-10
tweetData = NULL
while (date_time <= Sys.Date()+1){
  date_time = as.character.Date(date_time)
  tweetdetails <- searchTwitter("etisalat network -from:etisalat_9ja", n=1500, lang="en", since = "2016-05-03",until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
  tweetData = append(tweetData, tweetdetails)
  date_time = as.Date(date_time) +1
  print(date_time)
  flush.console()
}

tweetData_network = twListToDF(tweetData)
 
date_time = Sys.Date()-10
tweetData = NULL
while (date_time <= Sys.Date()+1){
  date_time = as.character.Date(date_time)
  tweetdetails <- searchTwitter("etisalat data -from:etisalat_9ja", n=1500, lang="en", since = "2016-05-03",until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
  tweetData = append(tweetData, tweetdetails)
  date_time = as.Date(date_time) +1
  print(date_time)
  flush.console()
}
tweetData = twListToDF(tweetData)
tweetDataFinal = rbind(tweetData, tweetData_network)

date_time = Sys.Date()-10
tweetData = NULL
while (date_time <= Sys.Date()+1){
  date_time = as.character.Date(date_time)
  tweetdetails <- searchTwitter("etisalat call -from:etisalat_9ja", n=1500, lang="en", since = "2016-05-03",until= date_time, geocode = '10,9, 200km')# Transform tweets list into a data frame 
  tweetData = append(tweetData, tweetdetails)
  date_time = as.Date(date_time) +1
  print(date_time)
  flush.console()
}
tweetData = twListToDF(tweetData)
tweetDataFinal = rbind(tweetData, tweetDataFinal)
View(tweetDataFinal)

tweetDataFinal$text = as.character(tweetDataFinal$text)
#Just to make sure the twitter search only picked tweets that about mtn
tweetDataFinal = subset(tweetDataFinal,grepl('etisalat',text,ignore.case = T))

#We shouldn't be analyzing tweets that MTN actually tweeted right?
tweetDataFinal = subset(tweetDataFinal,!grepl('etisalat',screenName,ignore.case = T))

#So we have emoji's to deal with and the because they are beyond the UTF-8 they appear...well..weird
#One option is to remove them completely
#But I'd be losing a lot of information right? Because Emoji's show sentiment
#So I'd rather do some housekeeping and deal with this
#An awesome human compiled a list of emojis bytes and utf-8 characters and their descriptions on GitHub. Shout to Jessica Peterka-Bonetta!

emoticons = read.csv("https://raw.githubusercontent.com/today-is-a-good-day/Emoticons/master/emDict.csv",sep = ";")
emoticons$Native = NULL

#Ensuring the text is in the native R encoding
tweetDataFinal$text = enc2native(tweetDataFinal$text)

#Remove all the U+00 and leave the R encoding only
tweetDataFinal$text = tolower(str_replace_all(tweetDataFinal$text,"U\\+00",''))

#The function below find the first emoji in a tweet and puts it in a new column.
#The idea behind picking only one emoji comes from the assumption that one emoji out of the many that people put in a tweet is enough to find the sentiment of that tweet
#In fact, A lot of the time emojis are just repitions or are conveying the same emotion
extract_emojis = function(df){
  df$emoticon = ''
  pt = txtProgressBar(min = 0, max=nrow(df),initial = 0)
  for (i in 1:nrow(df)){
    if (str_count(df$text[i],"<e") >0){
      emoji_placeholder = "<e"
      k = str_locate(df$text[i],"<e")[2]
      while (emoji_placeholder %in% emoticons$R.encoding == F){
        emoji_placeholder = substr(df$text[i],start = str_locate(df$text[i],"<e")[1],  stop = k)
        k=k+1
        if (k > nchar(df$text[i])){
          break
        }
      }
      
      df$emoticon[i] = emoji_placeholder
    }
    
    setTxtProgressBar(pt,i)
  }
  
  return (df)
}

tweetDataFinal = extract_emojis(tweetDataFinal)

#Removing trailing and leading white space
tweetDataFinal$emoticon = gsub("^\\s+|\\s+$", "",tweetDataFinal$emoticon)

#By spot checking (really trial and error), I see that two emoticons do not exist in the emoticon csv, hence the loop searches to the end of the tweet
#and because it checks till the end of the tweet, it picks up some stuff like hashtags, links and the likes. This function removes everything after
#the last occurrence of ">" in the emoticon column
tweetDataFinal$emoticon=str_replace(tweetDataFinal$emoticon,"(?<=>)[^>]*$",'')

#Get the description of the emoticons by merging the emoticons table with the tweets dataframe, tweetDataFinal
tweetDataFinal = merge(tweetDataFinal,emoticons,by.x = "emoticon",by.y = "R.encoding",all.x = T)

  
#We see that some emoticons (two or three if we look carefully) have no description for them.
#Let's take a closer look at them

exploreUnknown = subset(tweetDataFinal,is.na(Description),emoticon!='')

#Nothing particularly strage asides that they are some repition of the same tweet by the same person tweeted at the same time
#This will be removed anyways.

#I'm going to remove mentions and @s in the tweet because it adds no value to our analysis
tweetDataFinal$text = str_replace_all(tweetDataFinal$text,'@\\S+',"")

#Also removing RT because that information is already captured in the column isRetweet
tweetDataFinal$text=str_replace_all(tweetDataFinal$text,'rt ','')

#Removing links from the tweets because again, there is no feasible way that this will make our analysis better
tweetDataFinal$text = gsub('http\\S+\\s*', "", tweetDataFinal$text)

#Let's not forget to remove the R encoding since we have extracted the sentiment we needed from it
tweetDataFinal$text = gsub("<.*>",'',tweetDataFinal$text)

#Now, It's safe to remove all other non aplha numeric characters
tweetDataFinal$text = gsub("[^0-9A-Za-z/// ]", "", tweetDataFinal$text, T)

#If you look carefully, there are some duplicate tweets from the exact same person at the same time. This happens sometimes on twitter when a person tweets once and
#for some network reasons, it is duplicated (but 6 times though? Weird.)
#Further a lot of duplicate tweets are probably promotional tweets 
#The first step is to get out all the tweets that are not retweets and the remove duplicates
notRetweets = subset(tweetDataFinal,isRetweet == F)
notRetweets = notRetweets[!duplicated(notRetweets["text"]),]

#I noticed something pretty weird with the retweeted tweets. I found duplicates of the same person retweeting the same thing within the same second
#Pretty dodgy eh? So I factored that in and removed the duplicated
Retweet = subset(tweetDataFinal,isRetweet == T)
Retweets = Retweet[!duplicated(Retweet[c("text","screenName")]),]

#Lets just put these two datasets together
tweetDataFinal = rbind(Retweets,notRetweets)


#######SPECIFIC TO MTN#########
#Are we done cleaning now? Not quite but we are almost there. Apparently people spell mountain as mtn.
#Natural Language. Sigh. So Just incase any of them pass through the network, call, data filter, let's remove them
tweetDataFinal = subset(tweetDataFinal,!grepl("dew",text))
tweetDataFinal = subset(tweetDataFinal,!grepl("bike",text))
#######SPECIFIC TO MTN#########

#######SPECIFIC TO GLO#########
tweetDataFinal = subset(tweetDataFinal,!grepl("glo up",text))
tweetDataFinal = subset(tweetDataFinal,!grepl("glo'd",text))
tweetDataFinal = subset(tweetDataFinal,!grepl("glo gang",text))
tweetDataFinal = subset(tweetDataFinal,!grepl("glo ing",text))
tweetDataFinal = subset(tweetDataFinal,!grepl("gloing",text))
tweetDataFinal = subset(tweetDataFinal,!grepl("glogang",text))
tweetDataFinal = subset(tweetDataFinal,!grepl("lil glo",text))
#######SPECIFIC TO MTN#########

#Now let's add the description of our emoticons to the tweets
#First, let's remove those pesky NAs in the description column and convert them to empty strings
tweetDataFinal$Description = apply(data.frame(tweetDataFinal$Description),1,function(x) ifelse(is.na(x),"",x))

#Now we are going to concatenate the tweets and emoji description
tweetDataFinal$text = paste(tweetDataFinal$text,tolower(tweetDataFinal$Description))

# I assume that because some description were empty we ould have some trailing spaces.
#Let's remove trailing and leading spaces one last time just incase.
tweetDataFinal$text = gsub("^\\s+|\\s+$", "",tweetDataFinal$text)

#And we are done! Now for some sentiment analysis!

#From the tidytext package, we would use the AFINN lexicon which scores word between -5 to 5 based on
#negative or positive sentiment
lexicon = sentiments %>% filter(lexicon == "AFINN")

#This function uses the AFINN lexicon we loop through the tweets and score them accordingly 
#It also takes into account when the word 'not' is in a sentence. If the word 'not' is there,
#I definitely need to look at the next word and check if that not + word exists in the lexicon
#If it does not I check if the second word alone exists in the lexicon and simply 
#reverse the sign of the score e.g "not great". great exists with a score of 3 and with this function
#not great would have a score of -3. Pretty awesome eh?
sentiment_score = function(sentence) {
  score = 0
  words = str_split(sentence, " ")
  words = unlist(words)
  for (i in 1:length(words)) {
    if (words[i] == "not") {
      word = paste("not", words[i + 1])
      word.split = unlist(str_split(word, " "))
      if (word %in% lexicon$word == T) {
        score = score + lexicon$score[lexicon$word == word]
        
      }
      else if (word.split[2] %in% lexicon$word == T) {
        score = score - lexicon$score[lexicon$word == word.split[2]]
        
      }
      
      
    }
    else if (i > 1 && words[i - 1] == "not")
      next
    
    else if (words[i] %in% lexicon$word == T) {
      score = score + lexicon$score[lexicon$word == words[i]]
      
    }
    
    
  }
  return (score)
  
}

#Let's apply this function to every row in the data. So much faster than a for loop!
tweetDataFinal$score=apply(data.frame(tweetDataFinal$text),1,sentiment_score)

#A Histogram of the scores
hist(tweetDataFinal$score[tweetDataFinal$score !=0])

table(tweetDataFinal$score[tweetDataFinal$score !=0]>0)
median(tweetDataFinal$score[tweetDataFinal$score !=0])
tweetDataFinal$Telecoms = rep("Etisalat",nrow(tweetDataFinal))
write.csv(tweetDataFinal,'etisalat.csv',row.names = F)



#Let's create a term-frequency document using the tm package by creating a corpus of all the words in our data
corpus = Corpus(VectorSource(tweetDataFinal$text[tweetDatafinal$score !=0]))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, content_transformer(stripWhitespace))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeWords, c("etisalat","9ja","etisalat_9ja","vodacom","nigeria","home","sleep","start","tweet","phone","aaaaay","face","datamustfall","datamustfal","network","data","call","amp"))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, removeWords, stop_words$word)
corpus = tm_map(corpus, removeWords, removePunctuation(stop_words$word))
corpus = tm_map(corpus, removePunctuation)

frequencies = DocumentTermMatrix(corpus)

frequencies.common = removeSparseTerms(frequencies,0.9996)

etisalatTweets = as.data.frame(as.matrix(frequencies.common))

termFreq = data.frame(word = colnames(etisalatTweets),frequency = colSums(etisalatTweets))

wordcloud(colnames(etisalatTweets), colSums(etisalatTweets), scale = c(4, 0.5),colors = 'darkgreen')

