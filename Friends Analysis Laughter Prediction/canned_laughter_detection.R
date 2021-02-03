library(parsnip)
library(rsample)
library(tidyverse)
library(recipes)
library(ggthemr)
library(DALEX)
library(subtools) #devtools::install_github("fkeck/subtools")
library(httr)
library(jsonlite)
library(rvest)
library(stringdist)
laughter_transcribed <- read_csv('Laughter_Detection.csv')
laughter_transcribed=laughter_transcribed%>%
  rowwise()%>%
  mutate(seconds=paste0(seq(Start,End),collapse = ','))%>%
  separate_rows(seconds,sep = ',')%>%
  mutate(seconds=as.numeric(seconds))

season_one_audio=read_csv('season_one_audio.csv')
season_one_audio=season_one_audio%>%
  select(-X1,-`Unnamed: 0`)%>%
  mutate(seconds=seconds-1)

audio_training_data <- season_one_audio%>%
  filter(Episode=='E01')%>%
  mutate(seconds=seconds-1)%>%
  left_join(laughter_transcribed)%>%
  filter(seconds <max(laughter_transcribed$seconds))%>%
  mutate(label=ifelse(is.na(File),0,1))%>%
  select(-File,-Start,-End,-Length,-`Talk Over`,-seconds)

audio_training_data <- select_if(audio_training_data,is.numeric)


audio_training_data=audio_training_data%>%
  mutate(label=as.factor(label))

data_split <- initial_split(audio_training_data, prop = 0.7)

train=data_split %>%
  training() 

test=data_split %>%
  testing() 


model1=svm_poly( mode = "classification")%>%
  set_engine("kernlab") %>%
  parsnip::fit(label~., data = train)

model2=mlp( mode = "classification")%>%
  set_engine("nnet") %>%
  parsnip::fit(label~., data = train)

model3=rand_forest( mode = "classification")%>%
  set_engine("randomForest") %>%
  parsnip::fit(label~., data = train)


model4=boost_tree( mode = "classification")%>%
  set_engine("xgboost") %>%
  parsnip::fit(label~., data = train)

model5=logistic_reg( mode = "classification")%>%
  set_engine("glm") %>%
  parsnip::fit(label~., data = train)

model6=rand_forest( mode = "classification")%>%
  set_engine("ranger") %>%
  parsnip::fit(label~., data = train)


custom_predict_classif <- function(objectPred, set){
  as.data.frame(predict(objectPred, set, type = "prob"))[,2]
}

build_explainer <- function(model,test,label){
  explainer <- DALEX::explain(model, data=test, y=pull(test,label)%>%as.character()%>%as.numeric(), label =label, 
                              predict_function = custom_predict_classif, colorize = FALSE,verbose=FALSE)
  return(explainer)
  
}

get_weighted_precision <- function(model,data){
  data$prediction=predict(model,data)%>%pull()
  #data$prediction=ifelse(data$prediction>=0.5,'Uplift','Drop')
  model_name=model$spec$engine
  #f1_score=data%>%mutate_if(is.character,as.factor)%>%f_meas(revenue_success,prediction)%>%pull(.estimate)
  conf=table(data$label,data$prediction)
  res=conf[2,2]/(conf[2,2]+conf[1,2])
  recall=conf[2,2]/(conf[2,2]+conf[2,1])
  res1=(conf[2,2]+conf[1,1])/(conf[2,2]+conf[1,2]+conf[1,1]+conf[2,1])
  f1_score=2*((res*recall)/(res+recall))
  df=tibble(name=model_name,accuracy=100*res1,precision=100*res,f1_score=100*f1_score,recall=100*recall)
  return(df)
}


compare_explainers=pmap(list(
  model=list(model1,model2,model3,model4,model5,model6),
  test=list(test),
  label=c('svm_poly','nnet','rf','xgboost','glm','ranger')
),build_explainer)

do.call('plot',c(map(compare_explainers,DALEX::model_performance),list(geom='roc')))+
  ggtitle('AUC for Test')


test_results=map2_dfr(list(model1,model2,model3,model4,model5,model6),list(test),get_weighted_precision)

test_results%>%
  arrange(-f1_score)

canned_laughter_detection_model=model3
saveRDS(canned_laughter_detection_model,'canned_laughter_detection_model')


subtitles=list.files('S1')
subtitles=subtitles[str_detect(subtitles,'SAiNTS')]
subtitles=subtitles[c(1:15,17:23)]
subtitles[23:24]=c('Friends - 1x16 - The One With Two Parts (1).en.srt','Friends - 1x17 - The One With Two Parts (2).en.srt')
subtitles=sort(subtitles)
read_subtitles_custom<- function(file_path){
  path <- paste0('S1/',file_path)
  
  season_ep=str_extract(file_path,'\\dx\\d{2}')%>%str_split('x',simplify = T)%>%as.vector()
  season=as.numeric(season_ep[1])
  episode=as.numeric(season_ep[2])
  subs=read_subtitles(path)
  subs <- subs%>%
    mutate(season=season,
           epsiode=episode)
  return(subs)
  
}

subtitles_df=map_dfr(subtitles,read_subtitles_custom)

scripts_links=paste0('https://fangj.github.io/friends/season/01',sprintf('%02d', seq_len(24)),'.html')
read_scripts <- function(url){
  script_text=url%>%
    read_html%>%
    html_nodes('p[align="left"],p')%>%
    html_text()
  script_df=tibble(script_text)%>%
    filter(!str_detect(script_text,'Written by:') & !script_text %in% c('End','Opening Credits','Closing Credits','Commercial Break'))%>%
    mutate(scene=ifelse(str_detect(script_text,'^\\['),script_text,NA))%>%
    fill(scene,.direction = 'down')%>%
    filter(!str_detect(script_text,'^\\[|^\\(|^\\{'))%>%
    mutate(speaker=str_extract(script_text,'.*?:'))%>%
    mutate(script_text=str_remove_all(script_text,speaker)%>%str_trim())%>%
    mutate(addressee=str_extract(script_text,'Monica|Joey|Chandler|Phoebe|Ross|Rachel'),
           addressee=ifelse(is.na(addressee),'All',addressee))%>%
    filter(!is.na(script_text))%>%
    mutate(speaker=str_remove_all(speaker,':'))%>%
    mutate(script_text=str_replace_all(script_text,'\n',' '))
  season_ep=str_extract(url,'\\d{4}')
  season=str_sub(season_ep,1,2)%>%as.numeric()
  episode=str_sub(season_ep,3,4)%>%as.numeric()
  script_df=script_df%>%
    mutate(season=season,
           episode=episode)
  return(script_df)
}
scripts_df=map_dfr(scripts_links,read_scripts)

scripts_df=scripts_df%>%
  mutate(script_text=str_remove_all(script_text,'[:punct:]'))

subtitles_df=subtitles_df%>%
  mutate(Text_content=str_remove_all(Text_content,'[:punct:]'))
library(fuzzyjoin)

script_to_subtitle_match <- function(sub_df,script_df,row){
  window=seq(row-5,row+5)
  window=window[window>0]
  matched=sub_df%>%
    slice(window)%>%
    mutate(check=str_detect(script_df$script_text[row],Text_content))%>%
    filter(check)%>%
    mutate(
      scene=script_df$scene[row],
      speaker=script_df$speaker[row],
      addressee=script_df$addressee[row],
      script_text=script_df$script_text[row]
    )
  if(nrow(matched)==0){
    matched=sub_df%>%
      slice(window)%>%
      mutate(check=stringdist(script_df$script_text[row],Text_content,method = 'dl'))%>%
      top_n(1,desc(check))%>%
      mutate(
        scene=script_df$scene[row],
        speaker=script_df$speaker[row],
        addressee=script_df$addressee[row],
        script_text=script_df$script_text[row]
      )
  }
  return(matched)
  
 
}

final=tibble()
for(i in 1:24){
  print(paste('Running Episode',i))
  sub_df=subtitles_df%>%
    filter(epsiode==i)
  script_df=scripts_df%>%
    filter(episode==i)
  matched=map_dfr(seq_len(nrow(sub_df)),~script_to_subtitle_match(sub_df,script_df,.))
  final=final%>%bind_rows(matched)
}
season_one_text=final
season_one_text=season_one_text%>%
  separate(Timecode_in,c('hour','minute','second'),sep = ':',remove = F)%>%
  mutate_at(3:5,as.numeric)%>%
  rowwise()%>%
  mutate(Timecode_in=sum(hour*60,minute*60,second))%>%
  select(-hour,-minute,-second)%>%
  separate(Timecode_out,c('hour','minute','second'),sep = ':',remove = F)%>%
  mutate_at(4:6,as.numeric)%>%
  rowwise()%>%
  mutate(Timecode_out=sum(hour*60,minute*60,second))%>%
  select(-check,-hour,-minute,-second)

write_csv(season_one_audio,'season_one_audio.csv')
write_csv(season_one_text,'season_one_text.csv')
season_one_text=read_csv('season_one_text.csv')
season_one_audio=read_csv('season_one_audio.csv')
canned_laughter_detection_model=readRDS('canned_laughter_detection_model')
season_one_audio$laughter=predict(canned_laughter_detection_model,season_one_audio)%>%pull(.pred_class)
season_one_text=season_one_text%>%
  distinct(Timecode_in,Timecode_out,season,epsiode,scene,speaker,addressee,script_text)%>%
  group_by(epsiode,season,scene,speaker,addressee,script_text)%>%
  summarise(Timecode_in=min(Timecode_in),Timecode_out=max(Timecode_out))%>%
  ungroup()%>%
  arrange(epsiode,Timecode_in)
  
audio_text_join <- function(episode,seconds){
  audio=season_one_text %>% 
    filter(epsiode==episode)%>%
    mutate(diff=abs(seconds-Timecode_out))%>%
    top_n(1,desc(diff))%>%
    mutate(seconds=seconds)
  return(audio)
  
}
audio_text_dataset = tibble()

for(i in 1:24){
  ep_df=season_one_text%>%
    filter(epsiode==i)
  one_ep_test=map_dfr(seq_len(nrow(ep_df)),~audio_text_join(i,.))
  audio_text_dataset=audio_text_dataset%>%bind_rows(one_ep_test)

}





library(ggformula)
library(hrbrthemes)
library(magick)
library(grid)
library(extrafont)
font_import(pattern="GABRWFFR",paths = '/Users/rosebudanwuri/Downloads')
color_palette=c('#F7483E','#e7d509','#93AAC9','#F7483D','#D6D2CE','#C59A80','#FFEAD5','#E78D86')
theme_friends <- define_palette(
  swatch = color_palette, 
  background = 'white',
  text = 'black',
  gradient = c(lower = '#f27789', upper = '#31c2a4'),
  line = '#f3f4f8'
)
ggthemr(theme_friends)
chandler=image_read('chandler.png')%>%rasterGrob(interpolate=TRUE)
joey=image_read('joey.png')%>%rasterGrob(interpolate=TRUE)
monica=image_read('monica.png')%>%rasterGrob(interpolate=TRUE)
phoebe=image_read('phoebe.png')%>%rasterGrob(interpolate=TRUE)
rachel=image_read('rachel.png')%>%rasterGrob(interpolate=TRUE)
ross=image_read('ross.png')%>%rasterGrob(interpolate=TRUE)
p1=audio_text_dataset%>%
  left_join(season_one_audio%>%
              select(Season,Episode,seconds,laughter)%>%
              mutate(epsiode=str_extract(Episode,'\\d{2}')%>%as.numeric,
                     season=str_extract(Season,'\\d{2}')%>%as.numeric))%>%
  distinct(season,epsiode,script_text,laughter,speaker,addressee)%>%
  filter(laughter==1)%>%
  count(epsiode,speaker)%>%
  arrange(-n)%>%
  filter(speaker %in% c('Ross','Joey','Chandler','Monica','Phoebe','Rachel'))%>%
  ggplot(aes(epsiode,n,color=speaker))+
  geom_spline(size=1,spar = 0.5)+
  ggtitle('Who Was Funniest blahh')+
  theme_ipsum()+
  theme( panel.grid.major = element_blank(),plot.title = element_text(size=16, family="Gabriel Weiss' Friends Font"),legend.position="top",
         text=element_text(  family="Gabriel Weiss' Friends Font"), legend.key.width  = unit(1, "cm"))+
  guides(color=guide_legend(ncol=6))+
  scale_color_discrete(name = " ")


p2=audio_text_dataset%>%
  left_join(season_one_audio%>%
              select(Season,Episode,seconds,laughter)%>%
              mutate(epsiode=str_extract(Episode,'\\d{2}')%>%as.numeric,
                     season=str_extract(Season,'\\d{2}')%>%as.numeric))%>%
  distinct(season,epsiode,script_text,laughter,speaker,addressee)%>%
  filter(laughter==1)%>%
  count(speaker)%>%
  arrange(-n)%>%
  filter(speaker %in% c('Ross','Joey','Chandler','Monica','Phoebe','Rachel'))%>%
  ggplot(aes(reorder(speaker,n),n,fill=speaker))+
  geom_col(color='black')+
  coord_flip()+
  theme_ipsum()+
  theme( panel.grid.major = element_blank(),plot.title = element_text(size=16, family="Gabriel Weiss' Friends Font"),legend.position="top",
         text=element_text(  family="Gabriel Weiss' Friends Font"), legend.key.width  = unit(1, "cm"))+
  guides(fill=guide_legend(ncol=6))+
  scale_fill_discrete(name = " ")

library(patchwork)
p2/p1
audio_text_dataset%>%
  left_join(season_one_audio%>%
              select(Season,Episode,seconds,laughter)%>%
              mutate(epsiode=str_extract(Episode,'\\d{2}')%>%as.numeric,
                     season=str_extract(Season,'\\d{2}')%>%as.numeric))%>%
  distinct(season,epsiode,script_text,laughter,speaker,addressee)%>%
  filter(laughter==1)%>%
  count(speaker,addressee)%>%
  filter(addressee!='All')%>%
  arrange(-n)

audio_text_dataset%>%
  left_join(season_one_audio%>%
              select(Season,Episode,seconds,laughter)%>%
              mutate(epsiode=str_extract(Episode,'\\d{2}')%>%as.numeric,
                     season=str_extract(Season,'\\d{2}')%>%as.numeric))%>%
  distinct(season,epsiode,script_text,speaker,addressee)%>%
  count(speaker,name='total_dialogue')%>%
  arrange(-total_dialogue)%>%
  filter(speaker %in% c('Ross','Joey','Chandler','Monica','Phoebe','Rachel'))%>%
left_join(
  audio_text_dataset%>%
    left_join(season_one_audio%>%
                select(Season,Episode,seconds,laughter)%>%
                mutate(epsiode=str_extract(Episode,'\\d{2}')%>%as.numeric,
                       season=str_extract(Season,'\\d{2}')%>%as.numeric))%>%
    distinct(season,epsiode,script_text,laughter,speaker,addressee)%>%
    filter(laughter==1)%>%
    count(speaker,name='funny_dialogue')%>%
    arrange(-funny_dialogue)%>%
    filter(speaker %in% c('Ross','Joey','Chandler','Monica','Phoebe','Rachel')))%>%
  mutate(funny_ratio=100*funny_dialogue/total_dialogue)%>%
  arrange(-funny_ratio)
audio_text_dataset <- audio_text_dataset%>%
  left_join(season_one_audio%>%
              select(Season,Episode,seconds,laughter)%>%
              mutate(epsiode=str_extract(Episode,'\\d{2}')%>%as.numeric,
                     season=str_extract(Season,'\\d{2}')%>%as.numeric,
                     laughter=as.character(laughter)%>%as.numeric))%>%
  group_by(season,epsiode,script_text,speaker,addressee)%>%
  summarise(laughter=max(laughter),seconds=max(seconds))%>%
  arrange(epsiode,seconds)
write_csv(audio_text_dataset,'season_one_final.csv')
season_one_final=read_csv('season_one_final.csv')
season_one_final=season_one_final%>%
  mutate(word_count=str_count(script_text," ")+1)%>%
  filter(word_count>2)
write_csv(season_one_final,'season_one_final.csv')
