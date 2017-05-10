library(RSelenium)
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(doParallel)
library(foreach)
library(pbapply)
library(readr)



#Billionaires data
rD=rsDriver()
remDr = rD[["client"]]

remDr$navigate("https://www.forbes.com/billionaires/list/#version:static")
remDr$setImplicitWaitTimeout(30000)
clicktype = remDr$findElement(using = "css selector", '#the_list')
doc = remDr$getPageSource()[[1]]
firstTable = htmlParse(remDr$getPageSource()[[1]])
tbl1=readHTMLTable(firstTable, as.data.frame = T)
billionaire_data = tbl1["the_list"]
billionaire_data = as.data.frame(billionaire_data)
billionaire_data = billionaire_data %>% filter(is.na(the_list.V2)==F)
billionaire_data = billionaire_data[,2:7]
remDr$navigate("https://billionaires-data-base.silk.co/explore")
clicktype = remDr$findElement(using = "css selector", '#canvas > div > div.content-box > div > div.queryviewer.widget.component.component-queryview.component-queryview-table > div.output > table')
doc = remDr$getPageSource()[[1]]
firstTable = htmlParse(remDr$getPageSource()[[1]])
tbl1=readHTMLTable(firstTable, as.data.frame = T)
sector_list = as.data.frame(tbl1[1])

nm=c("Rank"	,"Name"	,"Net Worth"	,"Age"	,"Source"	,"Country of Citizenship")
setNames(billionaire_data,nm)
billionaires = billionaire_data$Name[billionaire_data$`Country of Citizenship`=="United States"]
billionaires = as.character(billionaires)
billionaires = str_trim(billionaires)
billionaires=str_replace_all(billionaires,"[^0-9A-Za-z\\- ]",'')
billionaires=str_replace_all(billionaires,"\\s+",' ')
billionaires = str_replace_all(billionaires," ","-")
billionaires=tolower(billionaires)
rD=rsDriver()
remDr = rD[["client"]]
get_self_made_score = function(names_of_billionaires){
  
  url = paste0("https://www.forbes.com/profile/",names_of_billionaires,"/")
  remDr$navigate(url)
  remDr$setImplicitWaitTimeout(30000)
  clicktype = try(remDr$findElement(using = "css selector", 'body > div.content > div.stats-lists-container > div.stats > ul > li:nth-child(3) > div.value'),silent = T)
  i=1
  while (class(clicktype)=="try-error" & i <3){
    clicktype = try(remDr$findElement(using = "css selector", 'body > div.content > div.stats-lists-container > div.stats > ul > li:nth-child(3) > div.value'),silent = T)
    i=i+1
  }
  if (class(clicktype)=="try-error")(
    return(NA)
  )
  remDr$mouseMoveToLocation(webElement = clicktype)
  clicktype$click()
  score = clicktype$getElementAttribute("innerHTML")[[1]]
  return(score)
}


get_billionaire_info = function(names_of_billionaires){
  
  url = paste0("https://www.forbes.com/profile/",names_of_billionaires,"/")
  remDr$navigate(url)
  remDr$setImplicitWaitTimeout(30000)
  #If Self Made
  clicktype = try(remDr$findElement(using = "css selector", 'body > div.content > div.stats-lists-container > div.stats > ul > li:nth-child(2) > div.value'),silent = T)
  #i=1
  # while (class(clicktype)=="try-error" & i <3){
  #   clicktype = try(remDr$findElement(using = "css selector", 'body > div.content > div.stats-lists-container > div.stats > ul > li:nth-child(2) > div.value'),silent = T)
  #   i=i+1
  # }
  if (class(clicktype)=="try-error")(
    clicktype = try(remDr$findElement(using = "css selector", 'body > div.content > div.stats-lists-container > div.stats > ul > li:nth-child(2) > div.value'),silent = T)
    
  )
  if (class(clicktype)=="try-error")(
    clicktype = try(remDr$findElement(using = "css selector", 'body > div.content > div.stats-lists-container > div.stats > ul > li:nth-child(1) > div.value'),silent = T)
  )
  if (class(clicktype)=="try-error")(
    return(NA)
  )
  score = clicktype$getElementAttribute("innerHTML")[[1]]
  #If Drop Out
  clicktype = try(remDr$findElement(using = "css selector", 'body > div.content > div.stats-lists-container > div.stats > ul > li.education > div.value'),silent = T)
  # i=1
  # while (class(clicktype)=="try-error" & i <3){
  #   clicktype = try(remDr$findElement(using = "css selector", 'body > div.content > div.stats-lists-container > div.stats > ul > li.education > div.value'),silent = T)
  #   i=i+1
  # }
  if (class(clicktype)=="try-error")(
    return(NA)
  )
  
  education=clicktype$getElementAttribute("innerHTML")[[1]]
  return(c(score,education))
}

billionaire_info=pblapply(billionaires,get_billionaire_info)
billionaire_info=billionaire_info_copy
billionaire_info = do.call("rbind",billionaire_info)
billionaire_info = cbind(billionaires,billionaire_info)
billionaire_info = as.data.frame(billionaire_info)

#Creat columns from billionaire information
billionaire_info$Self_Made = ifelse(grepl("self made",billionaire_info$V2,ignore.case = T),"Y","N")
billionaire_info$dropped_out = ifelse(grepl("drop out",billionaire_info$V3,ignore.case = T),"Y",ifelse(is.na(billionaire_info$V3),NA,"N"))
billionaire_info$bachelors_degree = ifelse(grepl("bachelor",billionaire_info$V3,ignore.case = T),"Y",ifelse(is.na(billionaire_info$V3),NA,"N"))
billionaire_info$masters_degree = ifelse(grepl("master of arts|master of science",billionaire_info$V3,ignore.case = T),"Y",ifelse(is.na(billionaire_info$V3),NA,"N"))
billionaire_info$MBA = ifelse(grepl("Master of Business Administration",billionaire_info$V3,ignore.case = T),"Y",ifelse(is.na(billionaire_info$V3),NA,"N"))
billionaire_info$phd_or_professional_degree = ifelse(grepl("doctor|llb",billionaire_info$V3,ignore.case = T),"Y",ifelse(is.na(billionaire_info$V3),NA,"N"))
write.csv(billionaire_info,"Billionaire Data.csv",row.names = F)

#Add original name column
billionaire_info$Names = billionaire_data$Name[billionaire_data$`Country of Citizenship`=="United States"]
billionaire_info$Worth = billionaire_data$`Net Worth`[billionaire_data$`Country of Citizenship`=="United States"]
billionaire_info$Age = billionaire_data$Age[billionaire_data$`Country of Citizenship`=="United States"]
billionaire_info$Worth = str_replace(billionaire_info$Worth,"B",'')
billionaire_info$Worth = str_trim(billionaire_info$Worth)

#Get info for billionaires outside the US
billionaires_outside_us = billionaire_data$Name[billionaire_data$`Country of Citizenship`!="United States"]
billionaires_outside_us = as.character(billionaires_outside_us)
billionaires_outside_us = str_trim(billionaires_outside_us)
billionaires_outside_us=str_replace_all(billionaires_outside_us,"[^0-9A-Za-z\\- ]",'')
billionaires_outside_us=str_replace_all(billionaires_outside_us,"\\s+",' ')
billionaires_outside_us = str_replace_all(billionaires_outside_us," ","-")
billionaires_outside_us=tolower(billionaires_outside_us)

billionaires_info_others=pblapply(billionaires_outside_us,get_billionaire_info)
billionaires_info_others=list()
for (i in 673:length(billionaires_outside_us)){
  dat=get_billionaire_info(billionaires_outside_us[i])
  billionaires_info_others = append(billionaires_info_others,list(dat))
  print(paste0(round((i/length(billionaires_outside_us))*100,2),"% done at",i))
}

billionaires_info_others = do.call("rbind", billionaires_info_others)

billionaires_info_others = cbind(billionaires_outside_us,billionaires_info_others)
billionaires_info_others = as.data.frame(billionaires_info_others)

#Create columns for details

billionaires_info_others$Self_Made = ifelse(grepl("self made",billionaires_info_others$V2,ignore.case = T),"Y","N")
billionaires_info_others$dropped_out = ifelse(grepl("drop out",billionaires_info_others$V3,ignore.case = T),"Y",ifelse(is.na(billionaires_info_others$V3),NA,"N"))
billionaires_info_others$bachelors_degree = ifelse(grepl("bachelor",billionaires_info_others$V3,ignore.case = T),"Y",ifelse(is.na(billionaires_info_others$V3),NA,"N"))
billionaires_info_others$masters_degree = ifelse(grepl("master of arts|master of science",billionaires_info_others$V3,ignore.case = T),"Y",ifelse(is.na(billionaires_info_others$V3),NA,"N"))
billionaires_info_others$MBA = ifelse(grepl("Master of Business Administration",billionaires_info_others$V3,ignore.case = T),"Y",ifelse(is.na(billionaires_info_others$V3),NA,"N"))
billionaires_info_others$phd_or_professional_degree = ifelse(grepl("doctor|llb",billionaires_info_others$V3,ignore.case = T),"Y",ifelse(is.na(billionaires_info_others$V3),NA,"N"))

data_view = billionaire_data[billionaire_data$`Country of Citizenship`!="United States",]
data_view = data_view %>% arrange(Name)

#Add original columns
billionaires_info_others$Names = data_view$Name[data_view$`Country of Citizenship`!="United States"]
billionaires_info_others$Worth = data_view$`Net Worth`[data_view$`Country of Citizenship`!="United States"]
billionaires_info_others$Age = data_view$Age[data_view$`Country of Citizenship`!="United States"]
billionaires_info_others$Worth = str_replace(billionaires_info_others$Worth,"B",'')
billionaires_info_others$Worth = str_trim(billionaires_info_others$Worth)
write.csv(billionaires_info_others,"~/Data Analysis/Datasets/Billionaire Data 2.csv",row.names = F)

#Find out who has been leaving the billionaire scene
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

historical_ranks = lapply(years,get_historical_ranks)
historical_ranks = do.call("rbind",historical_ranks)
write.csv(historical_ranks,"~/Data Analysis/Datasets/historical ranks.csv")
historical_ranks = read_csv("~/Data Analysis/Datasets/historical ranks2.csv")
historical_ranks = historical_ranks %>% mutate(markings= "X")
historical_ranks[9865,2] = "Jim Davis, new balance"
historical_ranks[12711,2] ="Jim Davis, recruitment"
historical_ranks[11497,2] = "Jim Davis, new balance"
historical_ranks[11030,2] ="Jim Davis, recruitment"
historical_ranks[9783,2] = "Wang Wei, delivery"
historical_ranks[10981,2] = "Wang Wei, computer hardware"
join_year=historical_ranks %>% group_by(Name_updated,Citizenship) %>% summarise(joining_year=min(Year))
year_list=historical_ranks %>% 
  select(Name_updated,Citizenship,Year,markings) %>%
  spread(Year,markings) %>%
  mutate( joining_year=join_year$joining_year)



year_list = read_csv("year_list.csv")

joining_year = function(row){
  yr=names(year_list)[(which(is.na(year_list[row,3:13])==F))[1]+2]
  return(yr)
}
year_list$joining_year = unlist(lapply(1:nrow(year_list),joining_year))
year_list=year_list %>%
  mutate(no_of_times_on_list =rowSums(is.na(year_list[,3:13])==F))

leavers = function(row){
  total =0
  for(i in 1:(length(row)-1)){
    if(is.na(as.character(row[i+1])) ==T && is.na(as.character(row[i])) ==F ){
      total = total+1
    }
  }
  return(total)
}

year_list$no_of_times_left =apply(year_list[,3:13],1,leavers)

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


year_list$no_of_comebacks=apply(year_list[,3:13],1,comers)
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

year_list$longest_time_away = apply(year_list[,3:13],1,max_distance)
write_csv(year_list,"year_list.csv")

# #After some work in Excel e.g. joining data columns togther to the original billionaire info
# #I'd like to get data on the other guys from the past 10 years (1929 people)
# year_list = read_csv("year_list.csv")
# parsing_list = year_list %>% filter(is.na(`Not Available`) ==T)
# other_billionaires = parsing_list$Name
# other_billionaires = as.character(other_billionaires)
# other_billionaires = str_trim(other_billionaires)
# other_billionaires=str_replace_all(other_billionaires,"[^0-9A-Za-z\\- ]",'')
# other_billionaires=str_replace_all(other_billionaires,"\\s+",' ')
# other_billionaires = str_replace_all(other_billionaires," ","-")
# other_billionaires=tolower(other_billionaires)
# 
# other_details = list()
# for (i in 1672:length(other_billionaires)){
#   dat=get_billionaire_info(other_billionaires[i])
#   other_details = append(other_details,list(dat))
#   print(paste0(round((i/length(other_billionaires))*100,2),"% done at ",i))
# }
# 
# other_details = do.call("rbind",other_details)
# other_details = cbind(other_billionaires,other_details)                                                                  
# other_details = as.data.frame(other_details)

billionaire_data = read_csv("Billionaire Data.csv")

billionaire_data %>% 
  group_by(joining_year) %>% 
  summarise(Number_of_comers = n()) %>%
  filter(joining_year >2007) %>%
  ggplot(aes(joining_year,Number_of_comers)) +
  geom_line(aes(color="red")) 
  
  
billionaire_data %>%
  filter(joining_year >2007) %>%
  filter(`Country of Citezenship`!="United States") %>%
  group_by(joining_year,Sector) %>%
  summarise(number_of_people = n()) %>%
  top_n(1)
  
billionaire_data %>%
  group_by(`Country of Citezenship`,Sector) %>%
  summarise(number_of_people = n()) %>%
  filter(number_of_people >1) %>%
  top_n(1) %>%
  arrange(-number_of_people)

billionaire_data %>%
  filter(no_of_comebacks >=2) %>%
  nrow()
url="http://stats.areppim.com/listes/list_billionairesx17xwor.htm"
tbl = url %>% 
  read_html %>%
  html_node("body > table") %>%
  html_table()
tbl=setNames(tbl,c("Rank","Name","Worth","Age","Source of Wealth","Country of Citizenship"))
lower_billionaires = tbl1[1061:nrow(tbl1),]

names_to_URLs = function(name){
  name = as.character(name)
  name = str_trim(name)
  name=str_replace_all(name,"[^0-9A-Za-z\\- ]",'')
  name=str_replace_all(name,"\\s+",' ')
  name = str_replace_all(name," ","-")
  name=tolower(name)
  return(name)
}

get_billionaire_info = function(names_of_billionaires){
  names_of_billionaires=names_to_URLs(names_of_billionaires)
  url = paste0("https://www.forbes.com/profile/",names_of_billionaires,"/")
  data=try(url %>% 
             read_html %>% 
             html_nodes(".stats li") %>% 
             html_text(trim=T)
           ,silent = T)
  if(class(data) =="try-error"){
    sector = NA
    education = NA
  }
  sector = ifelse(length(data[grepl("Source of Wealth",data,T)])>0,data[grepl("Source of Wealth",data,T)],NA)
  education = ifelse(length(data[grepl("Education",data,T)])>0,data[grepl("Education",data,T)],NA)
  sector = str_replace_all(sector,".*\t|.*\n","")
  education = str_replace_all(education,".*\t|.*\n","")
  return(c(sector,education))
}
lower_billionaires$detail1=""
lower_billionaires$detail2=""
for (i in 3:nrow(lower_billionaires)){
  dat=get_billionaire_info(lower_billionaires$Name[i])
  #dat = list(dat)
  lower_billionaires$detail1[i]=dat[1]
  lower_billionaires$detail2[i]=dat[2]
  print(paste0(round((i/nrow(lower_billionaires))*100,2),"% done at ",i))
}
