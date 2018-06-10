library(jsonlite)
library(plyr)
library(dplyr)
library(tidyr)
library(devtools)
library(readr)
library(stringr)
library(tm)
library(SnowballC)
library(tidytext)
library(wordcloud)
library(httr)
#List of Years with Billboard data available
year_list=1950:2015

set_config(config(ssl_verifypeer = 0L))
get_billboard_data = function(year){
  df =fromJSON(paste0("https://raw.githubusercontent.com/kevinschaich/billboard-top-100-lyrics/master/data/years/",year,".json"))
df[,c("neg","neu","pos","compound")]=df$sentiment
df$sentiment=NULL
  return(df)
}

music_df=ldply(year_list,get_billboard_data)
music_df=music_df %>%
  select(-tags)
#install_github("tiagomendesdantas/Rspotify")
library(Rspotify)

keys <- spotifyOAuth(app_id="ASD","769ef3519e8444238fde9c8981c6371c","b17e4a7ca0b4426f9962645ba5c74a63")

#name of Spotify Features
features_name=c("id","danceability","energy","key","loudness","mode","speechiness",
                "acousticness","instrumentalness","liveness","valence","tempo",
                "duration_ms","time_signature","uri","analysis_url")

#Initiatilizing the features in the data
music_df[,c("id","danceability","energy","key","loudness","mode","speechiness",
         "acousticness","instrumentalness","liveness","valence","tempo",
         "duration_ms","time_signature","uri","analysis_url")]=0


get_spotify_features=function(track, artist){
  songs= try(searchTrack(paste0("track:",track ," artist:",artist),keys),silent = T)
  if (class(songs)=="try-error"){
    return(rep(0,length(features_name)))
  }
  else{
  song_id=songs[,"id"][1]
  features=getFeatures(song_id,keys)
  return(features)
  }
}

#Two options for this
#Option 1 Mapply: cleaner code but no intermediate data is saved 
#so if your internet suddenly goes off, you would have to start from scratch

#music_df[,17:32]=mapply(get_spotify_features,music_df$title,music_df$artist)

#Option 2 for loop: slower and less elegant but intermediate data is saved 
#so if your internet suddenly goes off, you can start from where it was aborted
for(i in 1:nrow(music_df)){
  music_df[i,17:32]=  get_spotify_features(track = music_df$title[i],music_df$artist[i])
  print(i)

}

#In order to avoid the code breaking where we cannot find an artist using Spotify's API
#We initialized all song features and now we want to see if we can get partial matches
#using first names because usually it is spelling error
failed_ids = which(music_df$id =="0")
get_spotify_features1=function(track, artist){
  artist=str_split(artist," ",simplify = T)[[1]]
  songs= try(searchTrack(paste0("track:",track ," artist:",artist,"*"),keys),silent = T)
  if (class(songs)=="try-error"){
    return(rep(0,length(features_name)))
  }
  else{
    song_id=songs[,"id"][1]
    features=getFeatures(song_id,keys)
    return(features)
  }
}

#Update the data for the failed IDs
for(i in failed_ids){
  music_df[i,17:32]=  get_spotify_features1(track = music_df$title[i],music_df$artist[i])
  print(i)
  
}

#Function to get spotify's featured and main artists in a song
get_featured_artists=function(track, artist){
  songs= try(searchTrack(paste0("track:",track ," artist:",artist),keys),silent = T)
  if (class(songs)=="try-error"){
    return(NA)
  }
  else{
    song_id=songs[,"id"][1]
    aboutSong=getTrack(song_id,keys)
    artists=aboutSong$artists
    artists=as.character(artists)
    return(artists)
  }
}

###Get the featured artists in each song###

#Initialize column
music_df$artist_with_features=""

#Update new column with function output using Option 2: for loop
for (i in 1:nrow(music_df)){
  music_df$artist_with_features[i]=get_featured_artists(music_df$title[i],(music_df$artist[i]))
}

#Take out the main artists for the "artists with features" column
remove_main_artist=function(main_artist,artists){
  regex_pattern=paste0("(?<=;).*",main_artist,";|;.*",main_artist,"|",main_artist,".?;")
  new_list=str_replace(artists,regex_pattern,"")
  return(new_list)
}
music_df$artist_with_features=remove_main_artist(music_df$artist,music_df$artist_with_features)

music_df$artist_with_features=ifelse(music_df$artist_with_features==music_df$artist,"",music_df$artist_with_features)

#Get Images of Artist
get_artist_image=function(artist){
  base_lang=Encoding(artist)
  base_lang=ifelse(base_lang=="unknown","UTF-8",base_lang)
  artist=iconv(artist,from=base_lang,to="ASCII//TRANSLIT")
  artists_tbl= try(searchArtist(artist,keys),silent = T)
  if (class(artists_tbl)=="try-error" ){ 
    #Picture not available image
    return("https://www.tabithaknowel.com/integrated/uploads/2017/05/noPhotoFound.png")
  }
  if (nrow(artists_tbl)==0){
    return("https://www.tabithaknowel.com/integrated/uploads/2017/05/noPhotoFound.png")
  }
  id=artists_tbl$id[1]
  req <- httr::GET(paste0("https://api.spotify.com/v1/artists/", 
                          id), httr::config(token = keys))
  json1 <- httr::content(req)
  no_of_images=length(json1$images)
  if(no_of_images <=0){
    return("https://www.tabithaknowel.com/integrated/uploads/2017/05/noPhotoFound.png")
  }
  image=json1$images[[1]]$url
  return(image)
}


artistImage=data.frame(artist=character(),image=character(), stringsAsFactors=FALSE)
unique_artists=unique(music_df$artist)
for (i in unique_artists[1:length(unique_artists)]){
  url=get_artist_image(i)
  artist_and_image=c(i,url)
  idx=which(unique_artists %in% i)
  artistImage[idx,]=artist_and_image
  
}

music_df=music_df %>%
  left_join(artistImage,by="artist")

#Create the decades in the data
music_df = music_df %>% mutate(year_bin= case_when(
  year<1960 ~"50s",
  year<1970 ~"60s",
  year<1980 ~"70s",
  year<1990 ~"80s",
  year<2000 ~"90s",
  year<2010 ~"00s",
  year>=2010 ~"10s"
))
#Using the semi-colon seperator, we would create each feature as its own column from
#Feature1 to Feature6. Feature6 because that's highest number of features for any
#song in this dataset
music_df_with_features=music_df %>%
  select(artist,title,artist_with_features)%>%
  separate(artist_with_features,paste0("Features",1:6),";")

#We will then gather this into an an "artist, featured artist" key-value pair
music_df_with_features=music_df_with_features%>%
  gather(ArtistFeatures,FeaturedArtists,Features1:Features6) %>%
  select(-ArtistFeatures)


#List of Genres as defined fron Kevin Schaic
genres = list("rock"= c("symphonic rock", "jazz-rock", "heartland rock", "rap rock", "garage rock", "folk-rock", "roots rock", "adult alternative pop rock", "rock roll", "punk rock", "arena rock", "pop-rock", "glam rock", "southern rock", "indie rock", "funk rock", "country rock", "piano rock", "art rock", "rockabilly", "acoustic rock", "progressive rock", "folk rock", "psychedelic rock", "rock & roll", "blues rock", "alternative rock", "rock and roll", "soft rock", "rock and indie", "hard rock", "pop/rock", "pop rock", "rock", "classic pop and rock", "psychedelic", "british psychedelia", "punk", "metal", "heavy metal"),
              "alternative/indie"= c("adult alternative pop rock", "alternative rock", "alternative metal", "alternative", "lo-fi indie", "indie", "indie folk", "indietronica", "indie pop", "indie rock", "rock and indie"),
              "electronic/dance"= c("dance and electronica", "electro house", "electronic", "electropop", "progressive house", "hip house", "house", "eurodance", "dancehall", "dance", "trap"),
              "soul"= c("psychedelic soul", "deep soul", "neo-soul", "neo soul", "southern soul", "smooth soul", "blue-eyed soul", "soul and reggae", "soul"),
              "classical/soundtrack"= c("classical", "orchestral", "film soundtrack", "composer"),
              "pop"= c("country-pop", "latin pop", "classical pop", "pop-metal", "orchestral pop", "instrumental pop", "indie pop", "sophisti-pop", "pop punk", "pop reggae", "britpop", "traditional pop", "power pop", "sunshine pop", "baroque pop", "synthpop", "art pop", "teen pop", "psychedelic pop", "folk pop", "country pop", "pop rap", "pop soul", "pop and chart", "dance-pop", "pop", "top 40"),
              "hip-hop"= c("conscious hip hop", "east coast hip hop", "hardcore hip hop", "west coast hip hop", "hiphop", "southern hip hop", "hip-hop", "hip hop", "hip hop rnb and dance hall", "gangsta rap", "rapper", "rap"),
              "rnb"=c("contemporary r b","rhythm and blues", "contemporary rnb", "contemporary r&b", "rnb", "rhythm & blues","r&b", "blues"),
              "disco"= c("disco"),
              "swing"=  c("swing"),
              "folk"= c("contemporary folk", "folk"),
              "country"= c("country rock", "country-pop", "country pop", "contemporary country", "country"),
              "jazz"= c("vocal jazz", "jazz", "jazz-rock"),
              "religious"= c("christian", "christmas music", "gospel"),
              "blues"= c("delta blues", "rock blues", "urban blues", "electric blues", "acoustic blues", "soul blues", "country blues", "jump blues", "classic rock. blues rock", "jazz and blues", "piano blues", "british blues", "british rhythm & blues", "rhythm and blues", "blues", "blues rock", "rhythm & blues"),
              "reggae"= c("reggae fusion", "roots reggae", "reggaeton", "pop reggae", "reggae", "soul and reggae"))


#Get Genre of an artist from Spotify
get_artist_genre=function(artist){
  base_lang=Encoding(artist)
  #Takes care of non-UTF characters
  base_lang=ifelse(base_lang=="unknown","UTF-8",base_lang)
  artist=iconv(artist,from=base_lang,to="ASCII//TRANSLIT")
  artists_tbl= try(searchArtist(artist,keys),silent = T)
  if (class(artists_tbl)=="try-error" ){
    return("")
  }
  if (nrow(artists_tbl)==0){
    return("")
  }
  lst1=artists_tbl$genre[1]
  genre_list=str_split(lst1,",")[[1]]
  num_list=NULL
  for (i in 1:length(genres)){
    chk= genre_list%in% genres[i][[1]]
    totchk=sum(chk)
    num_list=append(num_list,totchk)
    
  }
  if (sum(num_list)==0){
    return("")
  }
  idx=which(num_list == max(num_list))

  final_genre=names(genres)[idx]
  final_genre=paste0(final_genre,collapse = "&")
  return(final_genre)
}


#Get genre of main artist i.e. artist that owns the song
unique_main_artists=unique(music_df_with_features$artist)

main_genre_df=data.frame(artist=character(),genre=character())
for (i in unique_main_artists){
  gn=get_artist_genre(i)
  lt=list(i,gn)
  lt=data.frame(lt)
  names(lt)=c("artist","genre")
  main_genre_df=rbind(main_genre_df,lt)
}

#Like the features, seperate the genres into artist, genre key-value pairs from genre1
#to genre5. genre5 because five is the highest amount of genres one artist is affliated with
main_genre_df=main_genre_df %>% 
  separate(genre,paste0("genre",1:5),"&")

#Update the music features data with the genre for the main artists 
music_df_with_features=music_df_with_features%>%
  left_join(main_genre_df,by = "artist")%>%
  gather(xx,main_genre,genre1:genre5)
  
#Filter out rows with no genre information and take out the dummy key column xx created above
music_df_with_features=music_df_with_features%>%
  filter(!is.na(main_genre)) %>%
  select(-xx)

#Get Genres for featured artists
unique_featured_artists=unique(music_df_with_features$FeaturedArtists)

feat_genre_df=data.frame(artist=character(),genre=character(), stringsAsFactors=FALSE)

for (i in unique_featured_artists){
 i= str_replace_all(i,pattern = '[:punct:]',"")
  gn=get_artist_genre(i)
  lt=list(i,gn)
  lt=data.frame(lt)
  names(lt)=c("artist","genre")
  feat_genre_df=rbind(feat_genre_df,lt)
}

feat_genre_df=feat_genre_df %>% 
  separate(genre,paste0("genre",1:5),"&")

#Also make a key-value pair of main artist to featured artisr
music_df_with_features=music_df_with_features%>%
  left_join(feat_genre_df,by=c("FeaturedArtists"="artist")) %>%
  gather(xx,featured_genre,genre1:genre5)%>%
  filter(!is.na(featured_genre)) %>%
  select(-xx)

################################Topic Modelling######################################
library(topicmodels)
library(tidytext)
lyrics_words=music_df%>%
  unnest_tokens(word,lyrics) %>%
  select(word,year_bin,artist)

#Take out stop words
lyrics_words =lyrics_words %>%
  anti_join(stop_words)

lyrics_words=lyrics_words %>%
  group_by(word,year_bin) %>%
  summarise(count =n())

lyrics_dtm = lyrics_words %>%
  cast_dtm(year_bin,word,count)

#Topic Modeling
yearbin_lda <- LDA(lyrics_dtm, k = 2, control = list(seed = 1234))

yearbin_lda_tidy = tidy(yearbin_lda)

#select Top 5 terms for each topic
top_terms=yearbin_lda_tidy %>%
  group_by(topic)%>%
  top_n(5,beta)
  


library(ggplot2)
library(hrbrthemes)

#Plot Terms
theme_set(theme_bw())
top_terms$topic=paste("Topic", top_terms$topic)
top_terms %>%
  ggplot(aes(reorder(term,-beta), beta)) +
  geom_bar(stat = "identity",aes(fill=as.character(topic)))+
  scale_fill_manual(values = c('Topic 1' = "skyblue3","Topic 2" = "seagreen3"))+
  facet_wrap(~ topic, scales = "free")+
  theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))

#Get presence of each topic in each decade
year_gamma = tidy(yearbin_lda,matrix="gamma")
topics_df=year_gamma %>%
  arrange(-gamma)
  

write_csv(topics_df,"topic_dataset.csv")

##################CLUSTERING#############################
#Prepare the data

#Group artists by the median of all their song features
cols=c(1:2,4:8,9,10:14,16:28,31)

firstDf=music_df %>%
mutate(year_bin=as.character(year_bin))%>%
select(-lyrics,-title) %>%
select(cols)%>%
group_by(artist) %>%
summarise_if(is.numeric,median)

#Function to return mode of a vector
Mode <- function(x) {
ux <- unique(x)
ux[which.max(tabulate(match(x, ux)))]
}

#Group artists by the decade the exist in the most 
secondDf=music_df %>%
mutate(year_bin=as.character(year_bin))%>%
select(-lyrics,-title) %>%
group_by(artist) %>%
summarise(year_bin=Mode(year_bin)) %>%
as_tibble()

#Join both tables
artist_df=firstDf %>%
inner_join(secondDf,by="artist")

library(h2o)
h2o.init()


data = artist_df %>% as.h2o()

#Spilt frame into train and validation set
splits = h2o.splitFrame(data,ratios = 0.7,destination_frames = c("train", "valid"), seed = 1234)
train = h2o.getFrame("train")
val = h2o.getFrame("valid")

#Column indices for all song features
song_features=2:24

#create k-means model
knn_model=h2o.kmeans(train,song_features,validation_frame = val,k=2)

#Update full dataset with knn prediction
data$cluster=predict(knn_model,data)
data$year_bin=h2o.asfactor(data$year_bin)

#View tabular results
h2o.table(data$year_bin,data$cluster)

#Make H2O frame a dataframe and update cluster names
cluster_df=as.data.frame(data)
cluster_df=cluster_df %>%
  mutate(cluster=ifelse(cluster==0,"String Lover","Poetic"))

#Update music dataset
music_df=music_df%>%
  left_join((cluster_df%>%
              select(artist,cluster)),by="artist")

#Find centers between both clusters
ctrs = h2o.centers(knn_model)
ctrs = as.data.frame(ctrs)

#Calculate % difference between song features in each cluster
ctrs[3,]=abs((ctrs[2,]-ctrs[1,])/ctrs[1,])






######################
data = music_df %>%
  select(-lyrics,-title) %>%
  as.h2o()
splits = h2o.splitFrame(data,ratios = c(0.6,0.2),destination_frames = c("train", "valid", "test"), seed = 1234)
train = h2o.getFrame("train")
features=c(1:2,4:8,10:14,16:28)
test = h2o.getFrame("test")
val = h2o.getFrame("valid")
features=c(1:2,4:8,10:14,16:28)
knn_model=h2o.kmeans(train,features,validation_frame = val,k=7)
predict(knn_model,train)
data$cluster=predict(knn_model,train)

write_csv(music_df,"music_df.csv")
write_csv(music_df_with_features,"features_dataset.csv")
