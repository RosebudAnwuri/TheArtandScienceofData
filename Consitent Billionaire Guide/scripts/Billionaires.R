library(RSelenium)
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)
library(purrr)
library(lime)

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
