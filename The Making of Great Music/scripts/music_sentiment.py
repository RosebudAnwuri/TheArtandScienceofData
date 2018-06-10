# -*- coding: utf-8 -*-
"""
Created on Fri Jan 19 17:18:58 2018

@author: rose.anwuri
"""

import pandas as pd
import matplotlib.pyplot as plt
import nltk
from nltk.corpus import stopwords
nltk.download()
music_df = pd.read_csv('https://raw.githubusercontent.com/walkerkq/musiclyrics/master/billboard_lyrics_1964-2015.csv')
music_df.head(n=5)
music_df['Lyrics']=music_df.Lyrics.fillna("")
from afinn import Afinn
afinn = Afinn()
music_df['sentiment_score']=map(afinn.score,music_df['Lyrics'])
music_df.head(n=5)
year_and_avg_sentiment=music_df[["Year","sentiment_score"]].groupby('Year').mean()
plt.plot( year_and_avg_sentiment.index,year_and_avg_sentiment.sentiment_score, label='linear')
plt.title("Sentiment of Popular Music between 1965-2015")
plt.show()
nrc_db = pd.read_table("https://raw.githubusercontent.com/mhbashari/NRC-Persian-Lexicon/master/NRC-emotion-lexicon-wordlevel-alphabetized-v0.92.txt",
                       names = ["Word", "Sentiment", "Exists"])
nrc_db.head(n=5)
nrc_db=nrc_db[nrc_db.Exists >0]
nrc_db.head(n=5)
sentiment=list(set(nrc_db.Sentiment))
for i in sentiment:
    music_df[i]=0

music_df.head(n=5)
stop_words=set(stopwords.words('english'))
def add_sentiment(lyrics):
    words=lyrics.split()
    words = set(words) - stop_words
    words = list(words)
    for i in words:
        sentiment=list(nrc_db[nrc_db.Word==i]["Sentiment"])
        if len(sentiment)>0:
            music_df.loc[music_df.Lyrics==lyrics,sentiment] +=1
        else:
            music_df.loc[music_df.Lyrics==lyrics,list(set(nrc_db.Sentiment))]+=0

for i in range(5100):
    add_sentiment(music_df.Lyrics[i])
    print(i)

import spotipy
client_credentials_manager = SpotifyClientCredentials()
from spotipy.oauth2 import SpotifyClientCredentials
client_credentials_manager = SpotifyClientCredentials(client_id="769ef3519e8444238fde9c8981c6371c",client_secret="b17e4a7ca0b4426f9962645ba5c74a63")
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)
from collections import OrderedDict
def get_spotify_features(track, artist):
    #Search for Spofity song ID 
    songs=sp.search(q='track:'+track+' '+'artist:'+artist+'*' , type='track')
    items = songs['tracks']['items']
    if len(items) ==0:
        return([0]*len(features))
    else:
        track = items[0]
        song_id = str(track["id"])
        #Use ID to get Song features
        track_features=sp.audio_features(song_id)
        if len(track_features[0]) <18:
            return([0]*len(features))
        else:
            features_to_df = np.array(track_features)[0]
            #Order Dictionary
            features_to_df = OrderedDict(features_to_df)
            #Get Dictionary values
            feature_values = features_to_df.values()
            return(feature_values)

music_df.loc[:,features]= music_df.loc[:,].apply(lambda row: pd.Series(get_spotify_features(row["Song"],row["artist_shortened"]),index=features) ,axis=1)
            
ind=np.linspace(0,5100,num=5101-1)
for i in ind:
    music_df.loc[i,features]=pd.Series(get_spotify_features(music_df.loc[i,"Song"],music_df.loc[i,"artist_shortened"]),index=features)
    print i

music_df = pd.read_csv('C:/Users/rose.anwuri/Documents/TheArtandScienceofData/Music Sentiment Analysis/data/music_data.csv')

def gather( df, key, value, cols ):
    id_vars = [ col for col in df.columns if col not in cols ]
    id_values = cols
    var_name = key
    value_name = value
    return pd.melt( df, id_vars, id_values, var_name, value_name )

music_df_gathered = gather(music_df,"Sentiment","Score",list(music_df.columns[9:19]))
music_df_gathered = gather(music_df,"audio_feature","feature_value",list(music_df_gathered.columns[10:28]))
