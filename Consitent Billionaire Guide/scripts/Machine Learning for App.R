library(h2o)
library(plyr)
library(dplyr)
library(purrr)
library(scales)

h2o.init()

#############################
##      Pre Processing     ##
#############################
dat = read.csv("C:/Users/rose.anwuri/OneDrive/TheArtandScienceofData/billionaire_data.csv")
dat1=dat %>%
  filter(Cluster !="The Newbie")
data = dat1 %>% 
  select(-Detail1,-Detail2) %>%
  as.h2o()

splits = h2o.splitFrame(data,ratios = c(0.6,0.2),destination_frames = c("train", "valid", "test"), seed = 1234)
train = h2o.getFrame("train")
val = h2o.getFrame("valid")
test = h2o.getFrame("test")
#After some feature selection, these are the final features
features=c(4,5,7,9,18,25,26,28)
response=27

#############################
##      Model Selection    ##
#############################

#Models we would like to train and test the accuracy
models = c("h2o.randomForest", "h2o.deeplearning" ,"h2o.gbm")
names_of_models = c("Random Forest", "Deep Learning" ,"GBM")

#############################
##Model Tuning and Validation 
##     in one step!        ##
#############################

#Select best model (already tuned) using the automl fuction
#Fix maximum number of models to be trained as 20
model_selection=h2o.automl(x=features,y=response,training_frame = train,validation_frame = val,max_models = 20,stopping_metric = "misclassification")

#Extract the best model
final_model = model_selection@leader

#Save Model to be loaded into Shiny App!
h2o.saveModel(final_model,"finalModel1")

