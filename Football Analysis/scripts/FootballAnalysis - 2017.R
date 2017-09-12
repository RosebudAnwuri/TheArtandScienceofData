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
Dat$X. = NULL
Dat$Last.6 = NULL
Dat[,2:9] = apply(Dat[,2:9],2,function(x) as.integer(as.character(x)))
Dat$Team = factor(str_replace_all(as.character(Dat$Team),pattern = "[*]",''))
Dat$Team = str_trim(Dat$Team)

Dat1=Dat
Dat$rank = rep(1:20,length.out=160)



roll_apply = function(pts, no_of_lags){
  if(length(pts)<no_of_lags){
    return(cummean(pts))
  }
  else{
    return(c(pts[1],rollapply(pts,no_of_lags,mean)))
  }
}


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
summary_gathering_pipeline = function(number){
  remDr$navigate(paste0("https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/",number,"/England-Premier-League"))
  #Go to Team Statistics
  clicktype = remDr$findElement(using = "css selector", '#sub-navigation > ul > li:nth-child(3) > a')
  remDr$mouseMoveToLocation(webElement = clicktype)
  clicktype$click()
  doc = remDr$getPageSource()[[1]]
  current_doc = remDr$getPageSource()[[1]] %>%
    read_html(doc) %>% 
    html_table(fill=T)
    current_doc= current_doc[c(1,3,6)]
    team_stat= current_doc %>%
    Reduce(function(x,y) merge(x,y,by="Team"),.) %>%
    select(-R.x,-R.y,-R)
    return(team_stat)
  
}
remDr$setImplicitWaitTimeout(30000)

team_statistics = ldply(year_coding,summary_gathering_pipeline)
percent_to_numberic = function(percent){
  require(stringr)
  percent = str_replace(percent,"%","") %>%
    as.numeric
  return(percent/100)
  
}

team_statistics[,14:16] = apply(team_statistics[,14:16],2,percent_to_numberic)

defensive_gathering_pipeline = function(number){
  remDr$navigate(paste0("https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/",number,"/England-Premier-League"))
  #Go to Team Statistics
  clicktype = remDr$findElement(using = "css selector", '#sub-navigation > ul > li:nth-child(3) > a')
  remDr$mouseMoveToLocation(webElement = clicktype)
  clicktype$click()
  clicktype = remDr$findElement(using = "css selector", '#stage-team-stats-options > li:nth-child(2) > a')
  remDr$mouseMoveToLocation(webElement = clicktype)
  clicktype$click()
  Sys.sleep(2)
  doc = remDr$findElement(using = "css selector", '#statistics-team-table-defensive')
  doc =doc$getElementAttribute("innerHTML")[[1]] %>% 
    read_html %>% 
    html_table()
  defense_stat= doc[[1]] %>%
    select(-R)
  return(defense_stat)
  
}

defense_statistics = ldply(year_coding,defensive_gathering_pipeline)

gathering_pipeline = function(type,number){
  remDr$navigate(paste0("https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/",number,"/England-Premier-League"))
  #Go to Team Statistics
  clicktype = remDr$findElement(using = "css selector", '#sub-navigation > ul > li:nth-child(3) > a')
  remDr$mouseMoveToLocation(webElement = clicktype)
  clicktype$click()
  if(type=="Offensive"){
  clicktype = remDr$findElement(using = "css selector", '#stage-team-stats-options > li:nth-child(3) > a')
  remDr$mouseMoveToLocation(webElement = clicktype)
  clicktype$click()
  }
  else if(type=="Detailed"){
    clicktype = remDr$findElement(using = "css selector", '#stage-team-stats-options > li:nth-child(4) > a')
    remDr$mouseMoveToLocation(webElement = clicktype)
    clicktype$click()
  }
  else if(type=="Pass Types"){
    clicktype = remDr$findElement(using = "css selector", '#stage-situation-stats-options > li:nth-child(2) > a')
    remDr$mouseMoveToLocation(webElement = clicktype)
    clicktype$click()
  }
  else if(type=="Card Situations"){
    clicktype = remDr$findElement(using = "css selector", '#stage-situation-stats-options > li:nth-child(3) > a')
    remDr$mouseMoveToLocation(webElement = clicktype)
    clicktype$click()
  }
  Sys.sleep(2)
  if(type=="Offensive"){
  doc = remDr$findElement(using = "css selector", '#statistics-team-table-offensive')
  }
  else if(type=="Detailed"){
    doc = remDr$findElement(using = "css selector", '#top-team-stats-summary-grid')
  }
  else if(type=="Pass Types"){
    doc = remDr$findElement(using = "css selector", '#stage-passes-grid')
  }
  else if(type=="Card Situations"){
    doc = remDr$findElement(using = "css selector", '#stage-cards-grid')
  }
  doc =doc$getElementAttribute("outerHTML")[[1]] %>% 
    read_html %>% 
    html_table()
  stat= doc[[1]] %>%
    select(-R)
  return(stat)
  
}
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
team_statistics=read.csv("team_statistics.csv")
defense_statistics=read.csv("defense_statistics.csv")
offense_stat=read.csv("offense_stat.csv")
pass_stat=read.csv("pass_stat.csv")
card_stat=read.csv("card_stat.csv")
in_c = left_join(team_statistics,defense_statistics,by=c("Team","Year"))
in_c = left_join(in_c,offense_stat,by=c("Team","Year"))
in_c = left_join(in_c,pass_stat,by=c("Team","Year"))
epl_database = left_join(in_c,card_stat,by=c("Team","Year"))
epl_database=epl_database %>% 
  select(-matches("Rating"),-matches("\\X."),-X)
rm(in_c)
#Create cummulative means for all numerical variables
cummean_fn = function(df, no_of_cols,no_of_rolls){
  if(nrow(df_split[[df]])<no_of_rolls){
    df_split[[df]][,no_of_cols] = apply(df_split[[df]][,no_of_cols],2,cummean)
    
  }
  else{
    roll_sum = rollapply(df_split[[df]][,no_of_cols],no_of_rolls,mean)
    first_rows = apply(df_split[[df]][1:no_of_rolls-1,no_of_cols],2,cummean)
    
    
    df_split[[df]][,no_of_cols] = rbind(first_rows,roll_sum)
    
  }
  return(df_split[[df]])
}
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

netSpend=function(year){
  
  url=paste0("https://www.transfermarkt.co.uk/premier-league/transfers/wettbewerb/GB1/plus/?saison_id=",year,"&s_w=&leihe=0&leihe=1&intern=0&intern=1")
  Team=url %>% read_html %>% html_nodes(".table-header a") %>% html_text
  Team = Team[Team != ""]
  net_spend=url %>% read_html %>% html_nodes(".table-footer.footer-border span") %>% html_text
  df = cbind(Team,net_spend)
  #flush.console(year)
  return(df)
  
}
spend = ldply(years,netSpend)

#remove pound and million sign and convert to a number
spend = spend %>%
  mutate(net_spend=as.numeric(str_replace_all(net_spend,"£|m","")))

spend = spend %>%
  mutate(Year=rep(2009:2016,each=20))


#####Tiring and unsexy#####
spend$Team[spend$Team=="Arsenal FC"]="Arsenal"
spend$Team[spend$Team=="Chelsea FC"]="Chelsea"
spend$Team[spend$Team=="Fulham FC"]="Fulham"
spend$Team[spend$Team=="Stoke"]="Stoke City"
spend$Team[spend$Team=="Wigan"]="Wigan Athletic"
spend$Team[spend$Team=="West Ham"]="West Ham United"
spend$Team[spend$Team=="Sunderland AFC"]="Sunderland"
spend$Team[spend$Team=="Blackpool FC"]="Blackpool"
spend$Team[spend$Team=="Everton FC"]="Everton"
spend$Team[spend$Team=="Liverpool FC"]="Liverpool"
spend$Team[spend$Team=="Middlesbrough FC"]="Middlesbrough"
spend$Team[spend$Team=="Norwich"]="Norwich City"
spend$Team[spend$Team=="Swansea"]="Swansea City"
spend$Team[spend$Team=="Southampton FC"]="Southampton"
spend$Team[spend$Team=="Reading FC"]="Reading"
spend$Team[spend$Team=="Cardiff"]="Cardiff City"
spend$Team[spend$Team=="Hull"]="Hull City"
spend$Team[spend$Team=="Burnley FC"]="Burnley"
spend$Team[spend$Team=="Leicester"]="Leicester City"
spend$Team[spend$Team=="AFC Bournemouth"]="Bournemouth"
spend$Team[spend$Team=="Watford FC"]="Watford"
spend$Team[spend$Team=="Middlesbrough FC"]="Middlesbrough"
#Now, we have our data!

epl_database = epl_database %>%
  left_join(.,spend,by=c("Team","Year")) %>%
  select(-realYear)
#####Preprocessing#####
h2o.init()
model_data = epl_database %>%
  filter(!is.na(Pts)) %>%
  select(-Pl)

train = model_data %>% filter(Year<2014)   %>%
  as.h2o 
test = model_data %>% filter(Year == 2014) %>%
  as.h2o 

train$Team = as.factor(train$Team)
test$Team = as.factor(test$Team)
cor=h2o.cor(model_data[,2:42] %>% as.h2o %>% h2o.na_omit)
row.names(cor) = colnames(cor)
cor$Pts = abs(cor$Pts)
features = c(7,13,6,11)
#2,4,7,11:13
response = 8

model_gbm2= h2o.gbm(x=features,y=response,training_frame = train,validation_frame = test)

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

h2o.getModel(IDs[2]) %>% h2o.varimp_plot

glm_model = h2o.getModel(IDs[1])
#######Model Tuning#######



##GLM###
hyper_params =list(alpha=seq(0,1,0.1))


glm_Grid = h2o.grid(algorithm = "glm",
                    grid_id = "glm_tuning2",
                    hyper_params = hyper_params,
                    x=features,
                    y=response,training_frame = train,
                    validation_frame = test,
                    seed=5.388119e+18,lambda_search=F)

#0.8109321185122624
h2o.getGrid("glm_tuning2",sort_by = "r2",decreasing = T)
model_glm = h2o.getModel("glm_tuning2_model_5")
hyper_params =list(ntrees=seq(50,100,10))
gbm_Grid = h2o.grid(algorithm = "gbm",
                    grid_id = "gbm_grid2",
                    hyper_params = hyper_params,
                    x=features,seed=7.304183e+18,
                    y=response,training_frame = train,
                    validation_frame = test
                    #,ntrees = 23
                    #,max_depth=2,
                    #min_rows=1,
                    #learn_rate = 0.19
                    
                    )



grid_sorted=h2o.getGrid("gbm_grid2",sort_by = "r2",decreasing = T)
ldply(grid_sorted@model_ids %>% unlist, function(x) h2o.getModel(x) %>% test.rmse)
h2o.getModel("gbm_grid_model_0") %>% test.rmse
hyper_params =list(rho=seq(0.99,0.999,0.001))
dl_Grid = h2o.grid(algorithm = "deeplearning",
                    grid_id = "deepl6",
                    hyper_params = hyper_params,
                    x=features,
                    y=response,training_frame = train,
                    validation_frame = test,epochs=50,seed=-1.735309e+18,rho=0.993)


h2o.getGrid("deepl6",sort_by = "r2",decreasing = T)
dl_model=h2o.getModel("deepl5_model_13")
test$prediction = predict(dl_model,test)
test.df = as.data.frame(test)
hyper_params =list(col_sample_rate_per_tree=seq(0.1,1,0.1))
rf_Grid = h2o.grid(algorithm = "randomForest",
                   grid_id = "rfD_tuning8",
                   hyper_params = hyper_params,
                   x=features,
                   y=response,training_frame = train,
                   validation_frame = test,
                   seed=-4.02063e+18
                   ,min_rows=3
                   ,ntrees = 60
                   ,col_sample_rate_change_per_level=1.35
                   # ,col_sample_rate_per_tree  =0.3
                   )

#0.809985695215523
h2o.getGrid("rfD_tuning8",sort_by = "r2",decreasing = T)

best_model =h2o.getModel("rfD4_model_5") 



#####Final Model######
model_gbm= h2o.getModel(IDs[2])
test$predict = h2o.predict(model_gbm,test)
test.df = as.data.frame(test)
test.df$pred_rank = rank(-test.df$predict,ties.method = "first")
write.csv(test.df,"testpred3.csv")
