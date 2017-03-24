library(rvest)
library(stringr)
library(dplyr)
library(doParallel)
library(foreach)


url1 <- "https://www.timeanddate.com/holidays/"

links <- url1 %>% read_html %>% html_nodes(".main-content-div a") %>% html_attr("href")
links <- links [4:231]
links <- paste0("https://www.timeanddate.com",links)
country <- str_replace_all(str_replace(links,"https://www.timeanddate.com/holidays/",""),"-"," ")
first_word_cap <- function(word){
  require(stringr)
  words <- unlist(str_split(word, " "))
  words <- lapply(words, function(x) str_replace(x,"//w",toupper(substr(x,1,1))))
  word <- paste(words, collapse = " ")
}
country <-unlist(lapply(country,first_word_cap))

no_of_holiday = function(x){
  require(rvest)
  require(dplyr)
  ifelse(is.list(try(x %>% 
                       read_html %>% 
                       html_node(".zebra") %>% 
                       html_table %>% 
                       filter (grepl("Holiday",`Holiday type`,ignore.case = T)),silent = T))
         ,nrow(x %>% read_html %>% html_node(".zebra") %>% html_table %>% filter (grepl("Holiday",`Holiday type`,ignore.case = T)))-1,0)
}
cores=detectCores()-1
cl = makeCluster(cores)
registerDoParallel(cl)
no_of_holidays = unlist(foreach(n=links,.packages=c("rvest","dplyr")) %dopar% no_of_holiday(x=n))
country_holidays <- cbind(data.frame(country),data.frame(no_of_holidays))
hist(country_holidays$no_of_holidays)

#Investigated outliers and relaized that some countires have county and regional holidays. 
#I wanted to ficus on more general holidays so I extracted countries that had a column stating what region the holiday was observed.
 outliers = function(x) {  
   if("Where it is observed" %in% names(x %>%
                                        read_html %>%
                                        html_node(".zebra") %>%
                                        html_table) == T){ 
     x
   }
 
}
outliers = unlist(foreach(n=links,.packages=c("rvest","dplyr")) %dopar% outliers(x=n))

#After extracting that, I extracted the new number of holidays.
updated_holidays= function (x){ 
  nrow(x %>% 
         read_html %>%
         html_node(".zebra") %>%
         html_table  %>%
         filter( grepl('holiday',`Holiday type`,ignore.case =T)) %>%
         filter(Encoding(`Where it is observed`) == "UTF-8"))
  }
updated_holidays = unlist(foreach(n=outliers,.packages=c("rvest","dplyr")) %dopar% updated_holidays(x=n))

#Housekeeping... getting the country names and putting them in a consistent format
outliers_country <- str_replace(outliers,"https://www.timeanddate.com/holidays/","")
outliers_country <- str_replace_all(outliers_country,"-"," ")
outliers_country <-unlist(lapply(outliers_country,first_word_cap))

#Updated the dataframe with the new holidays
country_holidays$no_of_holidays[country_holidays$country %in% outliers_country]=unlist(lapply(1:10,
  function(x) 
  country_holidays$no_of_holidays[country_holidays$country==outliers_country[x]]=updated_holidays[x])
  )
#remove countries with no holidays! 0_0 (Actually not countries. just the UN)
country_holidays = subset(country_holidays,no_of_holidays>0)

GDP.Data <- read.csv("~/Data Analysis/Datasets/GDP Data.csv")
###Merging the GDP data frame with the holiday dataframe
#At this point, I should point out that I really had to clean the country name formats outside R
#It would have been terribly cumbersome doing it any other way
country_holidays=rename(country_holidays,Country_Name = country)
country_data = merge(country_holidays,GDP.Data,by = "Country_Name",all.y = T)
country_data$no_of_holidays[210] = 9
country_data$no_of_holidays[213] = 13
country_data$no_of_holidays[214] = 10
country_data$no_of_holidays[219] = 15
country_data = country_data[complete.cases(country_data),]

#Health Spend
health_spend = read_csv("~/Data Analysis/Datasets/Health expenditure, total (current US).csv")

#Rehsaping the data to a better format
health_spend=health_spend %>% gather(Year, GDP,YR2007:YR2015)
health_spend$Year = str_replace(health_spend$Year,"YR",'')
avg_health_spend=health_spend %>% 
  filter(Year != 2015) %>% 
  group_by(Country_Name) %>% 
  summarise(Average_Total = mean(GDP))
country_data = merge(avg_health_spend,country_data,by = "Country_Name",all.y = T) 
country_data = country_data%>% select(Country_Name,Average,no_of_holidays,Avg_seven)

Health_expenditure_PPP <- read.csv("~/Data Analysis/Datasets/Health expenditure per capita, PPP.csv")
Health_expenditure_PPP=Health_expenditure_PPP %>% gather(Year, GDP,YR2007:YR2014)
Health_expenditure_PPP$Year = str_replace(Health_expenditure_PPP$Year,"YR",'')
Health_expenditure_PPP = rename(Health_expenditure_PPP,Country_Name=Country.Name)
avg_health_PPP= Health_expenditure_PPP %>% 
  filter(Year != 2015) %>% 
  group_by(Country_Name) %>% 
  summarise(Average_PPP = mean(GDP))

country_data = merge(avg_health_PPP,country_data,by = "Country_Name",all.y = T) 
country_data = country_data%>% select(Country_Name,Average,no_of_holidays,Avg_seven,Average_PPP)
