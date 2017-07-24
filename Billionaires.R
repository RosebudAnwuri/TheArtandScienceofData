library(RSelenium)
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)

#List of billionaires from 2007-2017

years = 7:17

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
joining_year = function(row){
  yr=names(full_list)[(which(is.na(full_list[row,3:13])==F))[1]+2]
  return(yr)
}
full_list$joining_year = unlist(lapply(1:nrow(full_list),joining_year))

#No of time billionaire has been on the list
full_list=full_list %>%
  mutate(no_of_times_on_list =rowSums(is.na(full_list[,3:13])==F))

#No of times billionaires left
leavers = function(row){
  total =0
  for(i in 1:(length(row)-1)){
    if(is.na(as.character(row[i+1])) ==T && is.na(as.character(row[i])) ==F ){
      total = total+1
    }
  }
  return(total)
}

full_list$no_of_times_left =apply(full_list[,3:13],1,leavers)

#No of times billionaires have comeback to the list
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


full_list$no_of_comebacks=apply(full_list[,3:13],1,comers)

#Longest time away from the list
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

full_list$longest_time_away = apply(full_list[,3:13],1,max_distance)

#Last year on list and last year off it
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
full_list$last_off_list = apply(full_list[,5:15],1,last_year_left)


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

#Get the billionaires info

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
education_columns_creator=function(df){
  df$Self_Made = ifelse(grepl("self made",df$detail1,ignore.case = T),"Y","N")
  df$dropped_out = ifelse(grepl("drop out",df$detail2,ignore.case = T),"Y",ifelse(is.na(df$detail2),NA,"N"))
  df$bachelors_degree = ifelse(grepl("bachelor",df$detail2,ignore.case = T),"Y",ifelse(is.na(df$detail2),NA,"N"))
  df$masters_degree = ifelse(grepl("master of arts|master of science",df$detail2,ignore.case = T),"Y",ifelse(is.na(df$detail2),NA,"N"))
  df$MBA = ifelse(grepl("Master of Business Administration",df$detail2,ignore.case = T),"Y",ifelse(is.na(df$detail2),NA,"N"))
  df$phd_or_professional_degree = ifelse(grepl("doctor|llb",df$detail2,ignore.case = T),"Y",ifelse(is.na(df$detail2),NA,"N"))
  return(df)
}

full_list = new_columns_creator(full_list)
full_list$detail1 = unlist(full_list$detail1)
full_list$detail2 = unlist(full_list$detail2)

#Data Munging complete! :)
