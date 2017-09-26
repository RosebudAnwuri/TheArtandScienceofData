#Apply moving average 

roll_apply = function(pts, no_of_lags){
  if(length(pts)<no_of_lags){
    return(cummean(pts))
  }
  else{
    return(c(pts[1],rollapply(pts,no_of_lags,mean)))
  }
}
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
percent_to_numberic = function(percent){
  require(stringr)
  percent = str_replace(percent,"%","") %>%
    as.numeric
  return(percent/100)
  
}
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

#RMSE on Test Set
test.rmse = function(model){
  require(caret)
  test$test_pred = predict(model,test)
  test.df = test %>% as.data.frame
  test.df$rank_pred=rank(-test.df$test_pred,ties.method = "min")
  rmse= RMSE(test.df$rank_pred,as.numeric(as.character(test.df$rank)),na.rm = T)
  return(rmse)
}

getID = function(model,newID){
  ID=model@model_id
  return(ID)
}