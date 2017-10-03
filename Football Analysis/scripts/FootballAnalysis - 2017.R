library(plyr)
library(dplyr)
library(stringr)
library(RSelenium)
library(rvest)
library(XML)
library(zoo)
library(h2o)
library(purrr)
library(caret)

options(stringsAsFactors = F)
#Get standard league table from skysports.com
Dat = NULL
for (i in 2009:2016){
teamStats = readHTMLTable(paste0('http://www.skysports.com/premier-league-table/',i))
names(teamStats) = NULL
teamStats = data.frame(teamStats)
teamStats$Year = i
Dat = rbind(Dat,teamStats)
print(i)
flush.console()
}

####Housekeeping!#####
Dat= subset(Dat, Year !=2006)
Dat$X. = NULL
Dat$Last.6 = NULL
Dat[,2:9] = apply(Dat[,2:9],2,function(x) as.integer(as.character(x)))
Dat$Team = factor(str_replace_all(as.character(Dat$Team),pattern = "[*]",''))
Dat$Team = str_trim(Dat$Team)

Dat1=Dat
Dat$rank = rep(1:20,length.out=320)

epl_db = ddply(Dat, ~Team,transform, W=roll_apply(W,2), 
               L=roll_apply(L,2),
               Cummulative.Pts=roll_apply(Pts,2), 
               Cummulative.rank=roll_apply(rank,2),
               GD=roll_apply(GD,2),
               D=roll_apply(D,2),
               F=roll_apply(F,2),
               A=roll_apply(A,2),
               Pts = lead(Pts),
               rank=lead(rank)) 

####Rendering whoscored.com website using a remote selenium driver and getting a data####

rD=rsDriver()
remDr = rD[["client"]]

#Whoscored's weird way of encoding their year variables. This 2009 - 2016
year_coding=c(1849,2458,2935,3389,3853,4311,5826,6335)

remDr$setImplicitWaitTimeout(30000)

team_statistics = ldply(year_coding,summary_gathering_pipeline)

team_statistics[,14:16] = apply(team_statistics[,14:16],2,percent_to_numberic)

defense_statistics = ldply(year_coding,defensive_gathering_pipeline)

offense_stat = ldply(year_coding,function(x) gathering_pipeline(type = "Offensive",x))
detailed_stat = ldply(year_coding,function(x) gathering_pipeline(type = "Detailed",x))
pass_stat     = ldply(year_coding,function(x) gathering_pipeline(type = "Pass Types",x))
card_stat     = ldply(year_coding,function(x) gathering_pipeline(type = "Card Situations",x))


remDr$close()

#Adding a Year column to the final table
team_statistics$Year = rep(2009:2016, each=20, length.out = 160)
defense_statistics$Year = rep(2009:2016, each=20, length.out = 160)
offense_stat$Year = rep(2009:2016, each=20, length.out = 160)
detailed_stat$Year = rep(2009:2016, each=20, length.out = 160)
pass_stat$Year = rep(2009:2016, each=20, length.out = 160)
card_stat$Year = rep(2009:2016, each=20, length.out = 160)
offense_stat$`Shots pg` = NULL
names(defense_statistics)[2] = "Shots.conceded.pg"
in_c = left_join(team_statistics,defense_statistics,by=c("Team","Year"))
in_c = left_join(in_c,offense_stat,by=c("Team","Year"))
in_c = left_join(in_c,pass_stat,by=c("Team","Year"))
epl_database = left_join(in_c,card_stat,by=c("Team","Year"))
epl_database=epl_database %>% 
  select(-matches("Rating"),-matches("\\X."),-X)
rm(in_c)
#Create cummulative means for all numerical variables
df_split=split(epl_database,epl_database$Team)
epl_data =ldply(names(df_split),function(x) cummean_fn(x,c(2:15,17:32),2))
epl_data$Team = str_trim(epl_data$Team)

####Housekeeping! Again! I am about to join two tables based on their team names. Hence, I have to ensure that they are of the same format####
epl_db$Team = as.character(epl_db$Team)
epl_db$Team[epl_db$Team == "Birmingham City"] = "Birmingham"
epl_db$Team[epl_db$Team == "Blackburn Rovers"] = "Blackburn"
epl_db$Team[epl_db$Team == "Bolton Wanderers"] = "Bolton"
epl_db$Team[epl_db$Team == "Cardiff City"] = "Cardiff"
epl_db$Team[epl_db$Team == "Norwich City"] = "Norwich"
epl_db$Team[epl_db$Team == "Swansea City"] = "Swansea"
epl_db$Team[epl_db$Team == "Stoke City"] = "Stoke"
epl_db$Team[epl_db$Team == "Tottenham Hotspur"] = "Tottenham"
epl_db$Team[epl_db$Team == "West Ham United"] = "West Ham"
epl_db$Team[epl_db$Team == "Wigan Athletic"] = "Wigan"
epl_db$Team[epl_db$Team == "Hull City"] = "Hull"
epl_db$Team[epl_db$Team == "Leicester City"] = "Leicester"

#Merge final tables from skysports and whoscored together and remove duplicate columns
epl_database = right_join(epl_db,epl_data,by = c("Year","Team"))
write.csv(epl_database,"Football Analysis/data/epl_data.csv",row.names = F)

#####Preprocessing#####
h2o.init()
model_data = epl_database %>%
  select(-Pl)

train = model_data %>% filter(Year<2016)   %>%
  as.h2o %>%
  h2o.na_omit()
test = model_data %>% filter(Year == 2016) %>%
  as.h2o 

train$Team = as.factor(train$Team)
test$Team = as.factor(test$Team)
cor=h2o.cor(model_data[,2:42] %>% as.h2o %>% h2o.na_omit)
row.names(cor) = colnames(cor)
cor$Pts = abs(cor$Pts)
features = c(2,4,7,11:13)
#7,13,6,11

response = 8

#####Model Selection########
models = c("h2o.glm","h2o.gbm","h2o.deeplearning","h2o.randomForest")

list_of_models = invoke_map(models,x=features,y=response,training_frame =train, validation_frame = test)

#Extract Model ID from model

#Store IDs for retreival later
IDs = list_of_models %>%
  map_chr(getID)

#######Model Validation########
list_of_models %>%
  map_dbl(test.r2) %>%
  set_names(.,models)

  list_of_models %>%
  map_dbl(test.rmse) %>%
  set_names(.,models)




#######Model Tuning#######

##GLM###
hyper_params =list(alpha=seq(0,1,0.1))


glm_Grid = h2o.grid(algorithm = "glm",
                    grid_id = "glm_tuning1",
                    hyper_params = hyper_params,
                    x=features,
                    y=response,training_frame = train,
                    validation_frame = test,
                    seed=5.388119e+18,lambda_search=T)


grid_sorted=h2o.getGrid("glm_tuning1",sort_by = "r2",decreasing = T)
rmse=ldply(grid_sorted@model_ids %>% unlist, function(x) h2o.getModel(x) %>% test.rmse)
cbind(alpha=grid_sorted@summary_table$alpha,rmse=rmse) %>% arrange(V1)

#GBM
hyper_params =list(learn_rate=seq(0.2,0.3,0.01),ntrees=seq(5,100,5),min_rows=1:10)
gbm_Grid = h2o.grid(algorithm = "gbm",
                    
                    hyper_params = hyper_params,
                    x=features,seed=8.973598e+18,
                    y=response,training_frame = train,
                    validation_frame = test
                    
                  )

grid_sorted=h2o.getGrid(gbm_Grid@grid_id,sort_by = "r2",decreasing = T)
rmse=ldply(grid_sorted@model_ids %>% unlist, function(x) h2o.getModel(x) %>% test.rmse)
cbind(val=grid_sorted@summary_table[,1],rmse=rmse) %>% arrange(V1) %>%top_n(-5)
model_gbm=h2o.getModel("gbm_grid7_model_0")
hyper_params =list(col_sample_rate_per_tree=seq(0.1,1,0.1),min_rows=1:10,ntrees=seq(5,100,5),col_sample_rate_change_per_level=seq(1,2,0.1))
rf_Grid = h2o.grid(algorithm = "randomForest",
                   grid_id = "rfD_tuning8",
                   hyper_params = hyper_params,
                   x=features,
                   y=response,training_frame = train,
                   validation_frame = test,
                   seed=-4.02063e+18
                   
                   )


rmse=ldply(grid_sorted@model_ids %>% unlist, function(x) h2o.getModel(x) %>% test.rmse)
cbind(val=grid_sorted@summary_table[,1],rmse=rmse) %>% arrange(V1) %>%top_n(-5)



#####Final Model######
#What if I told you all that tuning was of no use?
model_gbm=h2o.gbm(x=features,
                  y=response,training_frame = train,
                  validation_frame = test
                  )
test$predict = h2o.predict(model_gbm,test)
test.df = as.data.frame(test)
test.df$pred_rank = rank(-test.df$predict,ties.method = "first")
