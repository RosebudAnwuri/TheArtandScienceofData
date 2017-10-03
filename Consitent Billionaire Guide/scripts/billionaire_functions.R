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
getID = function(model,newID){
  ID=model@model_id
  return(ID)
}

#Function to check validation set accuracy
test_accuracy = function(model){
  table_accuracy=h2o.hit_ratio_table(model,valid = T)[1,2]
  return(table_accuracy)
}


model_type.H2OMultinomialModel <- function(x, ...) {
  return("classification")
  
}

predict_model.H2OMultinomialModel <- function(x, newdata, type, ...) {
  # Function performs prediction and returns dataframe with Response
  #
  # x is h2o model
  # newdata is data frame
  # type is only setup for data frame
  
  pred <- h2o.predict(x, as.h2o(newdata))
  
  # return classification probabilities only
  return(as.data.frame(pred[,-1]))
  
}