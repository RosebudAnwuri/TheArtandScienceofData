library(RSelenium)
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)
library(purrr)
library(lime)

#All Functions
get_historical_ranks = function(year){
  year_update = ifelse(year <10,paste0(0,year),year)
  url = paste0("http://stats.areppim.com/listes/list_billionairesx",year_update,"xwor.htm")
  tbl="http://stats.areppim.com/listes/list_billionairesx17xwor.htm" %>% 
    read_html() %>%
    html_node("body > table") %>%
    html_table(trim = T)
  names(tbl) = tbl[2,]
  tbl = tbl[3:nrow(tbl),]
  tbl = tbl[,-6]
  names(tbl) = c("Rank","Name","Citizenship","Age","Net Worth")
  tbl=tbl %>% mutate(Year= paste0(20,year_update))
}

joining_year = function(row){
  yr=names(full_list)[(which(is.na(full_list[row,3:13])==F))[1]+2]
  return(yr)
}

leavers = function(row){
  total =0
  for(i in 1:(length(row)-1)){
    if(is.na(as.character(row[i+1])) ==T && is.na(as.character(row[i])) ==F ){
      total = total+1
    }
  }
  return(total)
}

comers = function(row){
  total =0
  first_time = min(which(is.na(row)==F))
  
  if (first_time>=10){
    return(0)
  }
  else{
    for(i in first_time:(length(row)-1)){
      if(is.na(as.character(row[i+1])) ==F && is.na(as.character(row[i])) ==T ){
        total = total+1
      }
    }
  }
  return(total)
}

max_distance = function(row){
  lst=which(is.na(row)==F)
  lt = NULL
  if( length(lst)==1){
    return(0)
  }
  for (i in 1:(length(lst)-1)){
    h= lst[i+1]-(lst[i]+1)
    lt = append(lt,h)
  }
  return(max(lt))
}


last_year_on_list = function(row){
  years_onList=which(is.na(row)==F)
  year_left = names(full_list[,5:15])[max(years_onList)]
  return(year_left)
  
}
last_year_left = function(row){
  lst=NULL
  if (length(which(is.na(row)==T))==0){
    return ("")
  }
  else{
    for (i in 2:length(row)){
      if (is.na(row[i])==T&is.na(row[i-1])==F){
        lst = append(lst,i)
      }
    }
    year_left = names(full_list[,5:15])[max(lst)]
    return(year_left)
  }
}

#Make the names into URLs
names_to_URLs = function(name){
  name = as.character(name)
  name = str_trim(name)
  name=str_replace_all(name,"[^0-9A-Za-z\\- ]",'')
  name=str_replace_all(name,"\\s+",' ')
  name = str_replace_all(name," ","-")
  name=tolower(name)
  url = paste0("https://www.forbes.com/profile/",name,"/")
  return(url)
}

#Making a function to display name backwards e.g Joy Jones as Jones Joy 
#This will come in handy when getting the billionaire's info
backward_names = function(name){
  name = unlist(str_split(name," "))
  name = rev(name)
  name = paste(name,collapse = ' ')
  name = str_trim(name)
  name=names_to_URLs(name)
  return(name)
}


get_billionaire_info = function(names_of_billionaires){
  url=names_to_URLs(names_of_billionaires)
  data=try(url %>% 
             read_html %>% 
             html_nodes(".stats li") %>% 
             html_text(trim=T)
           ,silent = T)
  if(class(data) =="try-error"){
    url = backward_names(names_of_billionaires)
    data=try(url %>% 
               read_html %>% 
               html_nodes(".stats li") %>% 
               html_text(trim=T)
             ,silent = T)
  }
  sector = ifelse(length(data[grepl("Source of Wealth",data,T)])>0,data[grepl("Source of Wealth",data,T)],NA)
  education = ifelse(length(data[grepl("Education",data,T)])>0,data[grepl("Education",data,T)],NA)
  sector = str_replace_all(sector,".*\t|.*\n","")
  education = str_replace_all(education,".*\t|.*\n","")
  return(c(sector,education))
}

education_columns_creator=function(df){
  df$Self_Made = ifelse(grepl("self made",df$detail1,ignore.case = T),"Y","N")
  df$dropped_out = ifelse(grepl("drop out",df$detail2,ignore.case = T),"Y",ifelse(is.na(df$detail2),NA,"N"))
  df$bachelors_degree = ifelse(grepl("bachelor",df$detail2,ignore.case = T),"Y",ifelse(is.na(df$detail2),NA,"N"))
  df$masters_degree = ifelse(grepl("master of arts|master of science",df$detail2,ignore.case = T),"Y",ifelse(is.na(df$detail2),NA,"N"))
  df$MBA = ifelse(grepl("Master of Business Administration",df$detail2,ignore.case = T),"Y",ifelse(is.na(df$detail2),NA,"N"))
  df$phd_or_professional_degree = ifelse(grepl("doctor|llb",df$detail2,ignore.case = T),"Y",ifelse(is.na(df$detail2),NA,"N"))
  return(df)
}

#Extract Model ID from model
getID = function(model){
  ID=model@model_id
  return(ID)
}

#Function to check validation set accuracy
test_accuracy = function(model){
  table_accuracy=h2o.hit_ratio_table(model,valid = T)[1,2]
  return(table_accuracy)
}


model_type.H2OMultinomialModel = function(x, ...) {
  return("classification")
  
}

predict_model.H2OMultinomialModel = function(x, newdata, type, ...) {
  # Function performs prediction and returns dataframe with Response
  #
  # x is h2o model
  # newdata is data frame
  # type is only setup for data frame
  
  pred <- h2o.predict(x, as.h2o(newdata))
  
  # return classification probabilities only
  return(as.data.frame(pred[,-1]))
  
}

#List of billionaires from 2007-2017

years = 7:17


full_list = lapply(years,get_historical_ranks)
full_list = do.call("rbind",full_list)

#At this point had to some excel work to harmonize the names e.g. some year bill gates was mentioned as William Gates III
#This is where I spent most of my time
#Mark X for each billionaire in each year (this will come in handy later)
full_list = full_list %>% mutate(markings= "X")

#Three billionaires have exactly the same names - Wang Wei, Robert Miller and Jim Davis
full_list[9865,2] = "Jim Davis, new balance"
full_list[12711,2] ="Jim Davis, recruitment"
full_list[11497,2] = "Jim Davis, new balance"
full_list[11030,2] ="Jim Davis, recruitment"
full_list[9783,2] = "Wang Wei, delivery"
full_list[10981,2] = "Wang Wei, computer hardware"

#Use spread to make the year columns - the markings are useful here
full_list=full_list %>% 
  select(Name_updated,Citizenship,Year,markings) %>%
  spread(Year,markings)

#Create year billionaire joined

full_list$joining_year = unlist(lapply(1:nrow(full_list),joining_year))

#No of time billionaire has been on the list
full_list=full_list %>%
  mutate(no_of_times_on_list =rowSums(is.na(full_list[,3:13])==F))

#No of times billionaires left


full_list$no_of_times_left =apply(full_list[,3:13],1,leavers)

#No of times billionaires have comeback to the list



full_list$no_of_comebacks=apply(full_list[,3:13],1,comers)

#Longest time away from the list

full_list$longest_time_away = apply(full_list[,3:13],1,max_distance)

#Last year on list and last year off it

full_list$last_off_list = apply(full_list[,5:15],1,last_year_left)


#Make the names into URLs


#Making a function to display name backwards e.g Joy Jones as Jones Joy 
#This will come in handy when getting the billionaire's info

#Get the billionaires info


#Getting the columns for the details of billionaires set
full_list$detail1 =""
full_list$detail2 =""

#I opted for a for loop as opposed to the sexy apply function as this gives me the ability
#to store intermediate results
for (i in 1:nrow(full_list)){
  dat=get_billionaire_info(full_list$Name[i])
  #dat = list(dat)
  full_list$detail1[i]=dat[1]
  full_list$detail2[i]=dat[2]
  print(paste0(round((i/nrow(full_list))*100,2),"% done at ",i))
}

#Create columns for educational information

full_list = education_columns_creator(full_list)
full_list$detail1 = unlist(full_list$detail1)
full_list$detail2 = unlist(full_list$detail2)

#Data Munging complete! :)

#Clusters were made in Tableau using Sets and imported back into the csv file
#================================Machine Learning======================================#
library(h2o)
library(plyr)
library(dplyr)
library(purrr)
library(scales)
library(lime)
#Initialize H2o cluster
h2o.init()

dat1 = full_list %>%
  filter(Cluster !="The Newbie")

data = dat1 %>% 
        select(-Detail1,-Detail2) %>%
  as.h2o()
              
splits = h2o.splitFrame(data,ratios = c(0.6,0.2),destination_frames = c("train", "valid", "test"), seed = 1234)
train = h2o.getFrame("train")
val = h2o.getFrame("valid")
test = h2o.getFrame("test")



#Column index numbers
features=c(5,7,9,18,25,26,28)
  #c(4:11,18:19,25:26,28)
  
  #
#2,4:11,18:19,25:26,29
response=27

#Models we would like to train and test the accuracy
models = c("h2o.randomForest", "h2o.deeplearning" ,"h2o.gbm")
names_of_models = c("Random Forest", "Deep Learning" ,"GBM")

#The map function will invoke the functions in the "models" vector on the parameters below
list_of_models =invoke_map(models, x=features,y=response,training_frame =train,validation_frame = val) 


#Store IDs for retreival later
IDs = list_of_models %>%
  map_chr(getID)


#check accuracy on validation set
list_of_models %>%
  map_dbl(test_accuracy) %>%
  set_names(.,names_of_models)
#GBM performed best so let's compute that variable importance
model_gbm = h2o.getModel(IDs[3])
h2o.varimp_plot(h2o.getModel(IDs[3]))
#Store variable importance in csv for visualization
var_imp_rf=h2o.varimp(model_gbm)
write.csv(var_imp_rf,"Variable Importance GBM.csv",row.names = F)
model_gbm=h2o.loadModel("C:/Users/rose.anwuri/Documents/TheArtandScienceofData/Consitent Billionaire Guide/app/GBM_Model")
features_lime=model_gbm@parameters$x
train_lime = train[,c(features_lime,"Cluster")] %>% h2o.na_omit()
test_lime = val[,c(features_lime,"Cluster")]%>% h2o.na_omit()

predict_model(x = model_gbm, newdata =test_lime[,-9], type = 'raw') %>%
  tibble::as_tibble()
train_lime_df=as.data.frame(train_lime[,-9])
train_lime_df=train_lime_df[complete.cases(train_lime_df),]

explainer <- lime::lime(
  train_lime_df, 
  model          = model_gbm, 
  bin_continuous = FALSE)
test_lime_df = as.data.frame(test_lime) %>% filter(Cluster=="The Hustler") %>%as_tibble()
test_lime_df=test_lime_df[complete.cases(test_lime_df),]
explanation <- lime::explain(
  as.data.frame(test_lime_df[,-9]), 
  explainer    = explainer, 
  n_labels     = 1, 
  n_features   = 4,
  kernel_width = 0.5)
plot_features(explanation)
