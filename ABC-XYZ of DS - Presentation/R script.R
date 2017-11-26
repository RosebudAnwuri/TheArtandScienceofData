#Install packages
install.packages("rvest")
install.packages("stringr")
install.packages("dplyr")
install.packages("plyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("caret")
install.packages("scales")
install.packages("gridExtra")

#Load Libraries
library(rvest)
library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)
library(tidyr)
library(caret)
library(scales)
library(gridExtra)

url <- "https://www.timeanddate.com/holidays/"

#Get all links for the countries
links <- url %>%
  read_html %>% 
  html_nodes(".main-content-div .row a") %>% 
  html_attr("href")

#inspect the head and tail
head(links)
tail(links)
#Remove the first two links
links <- links [3:length(links)]

#Let's concatenate the base the URL to the path in the links variable
links <- paste0("https://www.timeanddate.com",links)
links[1:10]

#Extract actual country names from the links
country <- stringr::str_replace_all(links,"https://www.timeanddate.com/holidays/","") %>%
  str_replace_all(.,"[-/]"," ") %>%
  stringr::str_trim()
country[1:10]

#Change from lower case to Title Case
country =stringr::str_to_title(country)
country[1:10]

#function to get holiday for each country
holiday_per_country = function(index){
  require(rvest)
  require(dplyr)
  holidays=try(links[index] %>% 
                 read_html %>% 
                 html_node(".zebra") %>% 
                 html_table %>% 
                 dplyr::filter((grepl(pattern = "Holiday",`Holiday Type`,ignore.case= T)==T))
               ,silent = T)
  if(is.list(holidays)){
    return(c(country[index],nrow(holidays)-1))
  }
  else{
    return(c(country[index],0))
  }
}

#Apply function to the our indices of links
holiday_df=plyr::ldply(1:length(links),holiday_per_country)
names(holiday_df) = c("Country","no_of_holidays")
holiday_df %>% dplyr::as_tibble()

#Inspect data to see its distribution
holiday_df$no_of_holidays = as.numeric(holiday_df$no_of_holidays)
ggplot(holiday_df,aes(no_of_holidays))+
  geom_histogram(fill="seagreen3",color="black")+
  theme_classic()

#Sort the data in descending order to find the top outliers
holiday_df %>%
  arrange(-no_of_holidays)

#Extract the countries that have the extra column "Where it is observed"
local_holidays = function(index) {  
  column_names=names(links[index] %>%
                       read_html %>%
                       html_node(".zebra") %>%
                       html_table) %>% str_to_lower()
  if("where it is observed" %in%column_names  == T){ 
    country[index]
  }
  
}
local_countries=lapply(1:length(links),local_holidays) %>% unlist
local_countries

#Extract the index numbers for each of these countries
indices = which(country %in% local_countries)

#function to extract the actual number of holidays for these 10 countries
updated_holidays= function (index){
  
  table=links[index] %>% 
    read_html %>%
    html_node(".zebra") %>%
    html_table 
  names(table)=names(table) %>%
    str_to_lower()
  updated_table = table %>%
    filter( (grepl('holiday',`holiday type`,ignore.case =T) & 
               `where it is observed` == "" & grepl('jewish|Hindu',`holiday type`,ignore.case =T)==F) | grepl('all',`where it is observed`,ignore.case =T))
  return(c(country[index],nrow(updated_table)))
}

#Apply function to indices
new_holiday = ldply(indices,updated_holidays)
names(new_holiday)=c("Country","no_of_holidays")

#Add indices as a column to the new_holiday dataframe
new_holiday=cbind(new_holiday,indices)
new_holiday

#update the number of holidays in the original dataframe with that of the new_holiday
#for only the 10 countries
for(i in indices){
  holiday_df$no_of_holidays[i]=new_holiday$no_of_holidays[new_holiday$indices==i]
  
}
holiday_df=holiday_df %>%
  mutate(no_of_holidays=as.numeric(no_of_holidays)) 
#Check for outliers again
holiday_df %>%
  arrange(-no_of_holidays)

#Update actual holidays for the top 4 outliers

holiday_df$no_of_holidays[holiday_df$Country=="Bangladesh"]=21
holiday_df$no_of_holidays[holiday_df$Country=="India"]=18
holiday_df$no_of_holidays[holiday_df$Country=="Pakistan"]=20
holiday_df$no_of_holidays[holiday_df$Country=="Malaysia"]=20

#Inspect again using our Histogram from before
ggplot(holiday_df,aes(no_of_holidays))+
  geom_histogram(fill="seagreen3",color="black")+
  theme_classic()

#Check for the zeros by sorting in ascending order
holiday_df %>%
  arrange(no_of_holidays)

#Take out the zeros
holiday_df=holiday_df %>%
  filter(no_of_holidays>0)

#Check for duplicates
holiday_df %>%
  group_by(Country) %>%
  dplyr::summarise(no_of_occurences=n()) %>%
  arrange(-no_of_occurences)

#Remove duplicates
holiday_df=holiday_df %>%
  distinct(Country,no_of_holidays)

#Now, let's look at Life Expectancy data from The World Bank
life_expectancy_df = read.csv("Life Expectancy.csv")

#Using tidyr's gather() function we would collapse all these life expectancies 
#into two columns i.e. A key-value format
life_expectancy_df = life_expectancy_df %>%
  gather(Year,life_expectancy,X2008..YR2008.:X2015..YR2015.) %>%
  select(-Country.Code)
life_expectancy_df

#Tidy up the columns
life_expectancy_df$Year=substr(life_expectancy_df$Year,2,5)
life_expectancy_df=life_expectancy_df %>%
  mutate(Year=as.integer(Year)) %>%
  mutate(life_expectancy=as.numeric(life_expectancy)) %>%
  mutate(Country.Name=as.character(Country.Name)) 

#Check for NAs
table(is.na(life_expectancy_df))

#Let's remove all the NA's with R's complete.cases()
life_expectancy_df=life_expectancy_df %>% 
  group_by(Country.Name) %>%
  dplyr::summarise(avg_life_expectancy=mean(life_expectancy))
life_expectancy_df

#Some exploration of the distribution of Life Expectancy
ggplot(life_expectancy_df,aes(avg_life_expectancy))+
  geom_histogram(fill="violetred3",color="black")+
  theme_classic()

#Let's investigate what countries are the lower end of the life expectancy
life_expectancy_df %>%
  arrange(avg_life_expectancy)

#Use inner join to join life expectancy anf the holiday
life_expectancy_df=life_expectancy_df %>%
  dplyr::rename(Country=Country.Name)
holiday_and_life = inner_join(holiday_df,life_expectancy_df,by="Country")
holiday_and_life

##############################MACHINE LEARNING###############################

#Data preprocessing
country_data = read.csv("country_data.csv")
set.seed(1)
idx=createDataPartition(country_data$avg_life_expectancy,p=0.7,list = F)
train=country_data[idx,]
test=country_data[-idx,]
country_data
paste("Number of Observations for train is:",nrow(train),"& Number of Observations for test is:",nrow(test))

#plot correlation charts
p1=ggplot(country_data,aes(no_of_holidays,avg_gdp))+
  geom_point(color="violetred3",size=2)+
  ggtitle("Holidays")+
  theme_classic()
p2=ggplot(country_data,aes(Population,avg_gdp))+
  geom_point(color="#e67e22",size=2)+
  ggtitle("Population")+
  theme_classic()
p3=ggplot(country_data,aes(Health.expenditure..private....of.GDP.,avg_gdp))+
  geom_point(color="#3498db",size=2)+
  ggtitle("Health Expenditure")+
  theme_classic()
p4=ggplot(country_data,aes(Labor.force..total,avg_gdp))+
  geom_point(color="#1abc9c",size=2)+
  ggtitle("Labor Force")+
  theme_classic()
p5=ggplot(country_data,aes(Unemployment_Rate,avg_gdp))+
  geom_point(color="#34495e",size=2)+
  ggtitle("Unemployment Rate")+
  theme_classic()
p6=ggplot(country_data,aes(avg_life_expectancy,avg_gdp))+
  geom_point(color="#e74c3c",size=2)+
  ggtitle("Average Life Expectancy")+
  theme_classic()
grid.arrange(p1,p2,p3,p4,p5,p6)

#change to log scale
p1=ggplot(country_data,aes(no_of_holidays,log(avg_gdp)))+
  geom_point(color="violetred3",size=2)+
  ggtitle("Holidays")+
  theme_classic()
p2=ggplot(country_data,aes(log(Population),log(avg_gdp)))+
  geom_point(color="#e67e22",size=2)+
  ggtitle("Population")+
  theme_classic()
p3=ggplot(country_data,aes(Health.expenditure..private....of.GDP.,log(avg_gdp)))+
  geom_point(color="#3498db",size=2)+
  ggtitle("Health Expenditure")+
  theme_classic()
p4=ggplot(country_data,aes(log(Labor.force..total),log(avg_gdp)))+
  geom_point(color="#1abc9c",size=2)+
  ggtitle("Labor Force")+
  theme_classic()
p5=ggplot(country_data,aes(Unemployment_Rate,log(avg_gdp)))+
  geom_point(color="#34495e",size=2)+
  ggtitle("Unemployment Rate")+
  theme_classic()
p6=ggplot(country_data,aes(avg_life_expectancy,log(avg_gdp)))+
  geom_point(color="#e74c3c",size=2)+
  ggtitle("Average Life Expectancy")+
  theme_classic()
grid.arrange(p1,p2,p3,p4,p5,p6)

#Model selection

models = c("lm","rf","rpart","gbm")

set.seed(7)
model_selection = function(method_name){
  if(method_name=="rf"){
    garbage <- capture.output(
      
      model <-train(x= train[,c(2,4:9)], y = train[,3], method = method_name,importance=T)
    )
  }
  else{
    garbage <- capture.output(
      
      model <-train(x= train[,c(2,4:9)], y = train[,3], method = method_name)
    )
    
  }
  return(model)
  
}

#function to compute r squared values on the test set
R2_metric = function(model){
  predTest=predict(model,newdata = test)
  R2 = R2(predTest,test$avg_gdp)
  return(c(model[["method"]],R2))
}

#apply model_selection to the list of models 
model_results = lapply(models,model_selection) 

#apply metric calculation to model_results
models_r2 = model_results%>%
  ldply(.,R2_metric)
names(models_r2) = c("model","r2")

#Check table
models_r2 %>%
  mutate(r2=as.numeric(r2)) %>%
  arrange(-r2)

#Check variable importance for randomeforest
varImp(model_results[[2]])[["importance"]] %>%
  as.data.frame() %>%
  mutate(variables=row.names(.)) %>%
  ggplot(.,aes(x=reorder(variables,Overall),y= Overall ))+
  geom_bar(stat = "identity",fill="#16a085",color="#16a085")+
  coord_flip()+
  theme_classic()+ 
  xlab("Variables")+
  ylab("Weighted Importance")+
  ggtitle("Random Forest Variable Importance")+
  geom_text(aes(label=paste0(round(Overall,1),"%"),hjust=ifelse(Overall<90,-0.1,1)))