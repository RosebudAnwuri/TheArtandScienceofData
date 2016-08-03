library(XML)
#Discipline

discTable = readHTMLTable('http://www.foxsports.com/soccer/stats?competition=1&season=20130&category=DISCIPLINE')


for (i in 1:11){
  print (i)
  flush.console()
  if (i == 1){
    discTable = readHTMLTable('http://www.foxsports.com/soccer/stats?competition=1&season=20120&category=DISCIPLINE')
    discTable = data.frame(discTable)
    names(discTable) = c('Name','GP','GS','MP','FS','FC','YC','RC','OFF','C','CK','PKG','PK')
    
    }
  else{
  tablereader = readHTMLTable(paste0('http://www.foxsports.com/soccer/stats?competition=1&season=20120&category=DISCIPLINE&pos=0&team=0&isOpp=0&splitType=0&sort=5&sortOrder=0&page=',i))
  tablereader = data.frame(tablereader)
  names(tablereader) = c('Name','GP','GS','MP','FS','FC','YC','RC','OFF','C','CK','PKG','PK')
  discTable = rbind(discTable,tablereader)}
}

discTable$Name = gsub('[[:digit:]]+', '', discTable$Name)
summary(duplicated(discTable[,1]))
discTable = discTable[!duplicated(discTable["Name"]),]

#Standard
StandardTable = readHTMLTable('http://www.foxsports.com/soccer/stats?competition=1&season=20120&category=STANDARD')
StandardTable = data.frame(StandardTable)
names(StandardTable) = c('Name','GP',	'GS',	'MP',	'G',	'A','SOG'	,'S'	,'YC',	'RC')

for (i in 2:11){
  print (i)
  flush.console()
  tablereader = readHTMLTable(paste0('http://www.foxsports.com/soccer/stats?competition=1&season=20120&category=STANDARD&pos=0&team=0&isOpp=0&splitType=0&sort=3&sortOrder=0&page=',i))
  tablereader = data.frame(tablereader)
  names(tablereader) = c('Name','GP',	'GS',	'MP',	'G',	'A','SOG'	,'S'	,'YC',	'RC')
  StandardTable = rbind(StandardTable,tablereader)
}
summary(duplicated(StandardTable[,1]))
StandardTable = StandardTable[!duplicated(StandardTable["Name"]),]
StandardTable$Name = gsub('[[:digit:]]+', '', StandardTable$Name)

#Control
controlTable = readHTMLTable('http://www.foxsports.com/soccer/stats?competition=1&season=20120&category=CONTROL')
controlTable = data.frame(controlTable)
names(controlTable) = c('Name','GP',	'GS','MP',	'TT',	'P',	'INT',	'BLK',	'GMB',	'TKL'	,'OFF'	,'C'	,'CK')

for (i in 2:11){
  print (i)
  flush.console()
  tablereader = readHTMLTable(paste0('http://www.foxsports.com/soccer/stats?competition=1&season=20120&category=CONTROL&pos=0&team=0&isOpp=0&splitType=0&sort=3&sortOrder=0&page=',i))
  tablereader = data.frame(tablereader)
  names(tablereader) = c('Name','GP',	'GS','MP',	'TT',	'P',	'INT',	'BLK',	'GMB',	'TKL'	,'OFF'	,'C'	,'CK')
  controlTable = rbind(controlTable,tablereader)
}
summary(duplicated(controlTable[,1]))
controlTable = controlTable[!duplicated(controlTable["Name"]),]
controlTable$Name = gsub('[[:digit:]]+', '', controlTable$Name)

#housekeeping
#extract the club names
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
controlTable$Club = substrRight(controlTable$Name,3)
StandardTable$Club = substrRight(StandardTable$Name,3)
discTable$Club = substrRight(discTable$Name,3)

#Remove the redundant surnames
controlTable$Name = sub(".*? (.+)", "\\1", controlTable$Name)
StandardTable$Name = sub(".*? (.+)", "\\1", StandardTable$Name)
discTable$Name = sub(".*? (.+)", "\\1", discTable$Name)

#Get the names in the correct format
controlTable$Name = sub(",.*", "", controlTable$Name)
StandardTable$Name = sub(",.*", "", StandardTable$Name)
discTable$Name = sub(",.*", "", discTable$Name)
library(sqldf)
controlTable = sqldf('select * from controlTable where GS is not null')
discTable = sqldf('select * from discTable where GS is not null')
StandardTable = sqldf('select * from StandardTable where GS is not null')

#Cos I'm lazy and three datasets exists I will sort and then join, I am quite sure all players exists in all
test = merge(controlTable,discTable, by = "Name", all = T)
test3 = merge(test,StandardTable,by = "Name",all = T)



#Remove redundant columns
library(stringr)
delCols = NULL
for (i in 1:ncol(test3)){
  if(str_count(names(test3)[i],'.x') >0){
    delCols = append(delCols,names(test3)[i])
  }
}
test3 <- test3[, !(colnames(test3) %in% delCols), drop=FALSE]

for (i in 1:ncol(test3)){
  if(is.factor(test3[,i])== T){
    test3[,i] = as.numeric(as.character(unlist(test3[,i])))
  }
}

test3$Club.x =NULL
test3$Club.y = NULL
dataset2 = test3
dataset2$Name =sub('.*\t','',dataset2$Name)

dataset2 =dataset2[rowSums(is.na(dataset2)) == 0,]
Dat = NULL
for (i in 2005:2015){
teamStats = readHTMLTable(paste0('http://www.skysports.com/premier-league-table/',i))
names(teamStats) = NULL
teamStats = data.frame(teamStats)
teamStats$Year = i
Dat = rbind(Dat,teamStats)
print(i)
flush.console()
}
Dat$X. = NULL
Dat$Last.6 = NULL
Dat[,2:9] = apply(Dat[,2:9],2,function(x) as.integer(as.character(x)))
library(stringr)
Dat$Team = factor(str_replace_all(as.character(Dat$Team),pattern = "[*]",''))
write.csv(Dat,"FootballData.csv",row.names = F)

library(DataCombine)
datasetLagged = slide(Dat,Var = "Pts",TimeVar = "Year",GroupVar = "Team",slideBy = +1)
names(datasetLagged)[ncol(datasetLagged)] = 'leadPoints'
Train = subset(datasetLagged,Year %in% 2005:2013)
Test = subset(datasetLagged,Year %in% 2014:2015)
RankingModel = lm(leadPoints~Pts*F, data =Train,na.action = na.omit)
summary(RankingModel)
Test$prediction = predict(RankingModel,newdata = Test)


library(dplyr)
rankedData1 = rankedData %>%
  group_by(Year)%>%
 mutate(rankings_pred = order(prediction, decreasing = T))
print.data.frame(rankedData)
