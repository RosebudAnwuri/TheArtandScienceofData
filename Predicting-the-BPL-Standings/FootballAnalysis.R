library(DataCombine)
library(rpart)
library(stringr)
library(RSelenium)
library(rvest)
library(XML)
library(digest)

options(stringsAsFactors = F)
#Get standard league table from skysports.com
Dat = NULL
for (i in 2009:2015){
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

#You see that as.character right there? it's probably the most useful life lesson in R
#When changing data types, it 100% much safer to convert to a character first.
#Trust me when I say I have learnt the hard way
Dat[,2:9] = apply(Dat[,2:9],2,function(x) as.integer(as.character(x)))
Dat$Team = factor(str_replace_all(as.character(Dat$Team),pattern = "[*]",''))

#Create a lead variable for the Premier League points 
datasetLagged = slide(Dat,Var = "Pts",TimeVar = "Year",GroupVar = "Team",slideBy = +1)
names(datasetLagged)[ncol(datasetLagged)] = 'leadPoints'


####Rendering whoscored.com website using a remote selenium driver and getting data####

startServer()
remDr = remoteDriver()
remDr$open(silent = F)
remDr$navigate("https://www.whoscored.com/Regions/252/Tournaments/2/Seasons/5826/Stages/12496/TeamStatistics/England-Premier-League-2015-2016")
remDr$setImplicitWaitTimeout(30000)
inTable = NULL
for (i in 2:8){
clicktype = remDr$findElement(using = "css selector", '#seasons')
remDr$mouseMoveToLocation(webElement = clicktype)
clicktype$click()
clicktype = remDr$findElement(using = "css selector", paste0('#seasons > option:nth-child(',i,')'))
remDr$mouseMoveToLocation(webElement = clicktype)
clicktype$click()
clicktype = remDr$findElement(using = "css selector", '#sub-navigation > ul:nth-child(1) > li:nth-child(3) > a:nth-child(1)')
remDr$mouseMoveToLocation(webElement = clicktype)
clicktype$click()
doc = remDr$getPageSource()[[1]]
current_doc = read_html(doc)
firstTable = htmlParse(remDr$getPageSource()[[1]])

#Housekeeping!
v=readHTMLTable(firstTable, as.data.frame = T)
v= v[c(1,3,6)]
names(v) = NULL
v=lapply(v, function(x){data.frame(x)})
testData =Reduce(function(x,y) merge(x,y,by="Team", all = T),v)
testData[,2:15] = apply(testData[,2:15],2,function(x) as.numeric(as.character(x)))
testData$RedCards = substring(as.character(testData$Discipline),nchar(as.character(testData$Discipline)))
testData$YellowCards = substring(as.character(testData$Discipline),1,nchar(as.character(testData$Discipline))-1)
testData$Discipline = NULL


#Defense Table
clicktype = remDr$findElement(using = "css selector", '#stage-team-stats-options > li:nth-child(2) > a:nth-child(1)')

remDr$mouseMoveToLocation(webElement = clicktype)
clicktype$click()
webElem = remDr$findElement(using = "css selector", '#statistics-team-table-defensive')

#Dealing with web can be a real pain especially living with place terrible internet
#This is area is for error handling. Whenever the table is not populated but instead is of the class try-error (which means it failed), we retry all the steps in getting that table till it works
#Crude I know, will keep on thinking of ways to refine this.
webElemtext = try(webElem$getElementAttribute("outerHTML")[[1]])
defenseTable = try(readHTMLTable(webElemtext, header = T,as.data.frame = T)[[1]])
while (class(defenseTable) == "try-error"){
  clicktype = remDr$findElement(using = "css selector", '#stage-team-stats-options > li:nth-child(2) > a:nth-child(1)')
  remDr$mouseMoveToLocation(webElement = clicktype)
  clicktype$click()
  webElem = remDr$findElement(using = "css selector", '#statistics-team-table-defensive')
  webElemtext = try(webElem$getElementAttribute("outerHTML")[[1]],silent = F)
  defenseTable = try(readHTMLTable(webElemtext, header = T,as.data.frame = T)[[1]])
}

defenseTable[,3:8] = apply(defenseTable[,3:8],2,function(x) as.numeric(as.character(x)))
bigTable = merge(testData,defenseTable,by = "Team",all = T)


#Offensive
clicktype1 = remDr$findElement(using = "css selector", '#stage-team-stats-options > li:nth-child(3) > a:nth-child(1)')
remDr$mouseMoveToLocation(webElement = clicktype1)
clicktype1$click()
webElem1 = remDr$findElement(using = "css selector", '#statistics-team-table-offensive')
webElemtext = try(webElem1$getElementAttribute("outerHTML")[[1]],silent = F)
offenseTable = try(readHTMLTable(webElemtext, header = T,as.data.frame = T)[[1]])
while(class(offenseTable) == "try-error"){
  clicktype1 = remDr$findElement(using = "css selector", '#stage-team-stats-options > li:nth-child(3) > a:nth-child(1)')
  remDr$mouseMoveToLocation(webElement = clicktype1)
  clicktype1$click()
  webElem1 = remDr$findElement(using = "css selector", '#statistics-team-table-offensive')
webElemtext = try(webElem1$getElementAttribute("outerHTML")[[1]], silent = F)
offenseTable = try(readHTMLTable(webElemtext, header = T,as.data.frame = T)[[1]])
}

offenseTable[,3:7] = apply(offenseTable[,3:7],2,function(x) as.numeric(as.character(x)))

bigTable = merge(bigTable,offenseTable,by = "Team",all = T)



#Detailed
clicktype3 = remDr$findElement(using = "css selector", '#stage-team-stats-options > li:nth-child(4) > a:nth-child(1)')
remDr$mouseMoveToLocation(webElement = clicktype3)
clicktype3$click()
webElem2 = remDr$findElement(using = "css selector", '#statistics-team-table-detailed')
webElemtext = try(webElem$getElementAttribute("outerHTML")[[1]])
detailedTable = try(readHTMLTable(webElemtext, header = T,as.data.frame = T)[[1]])
while (class(detailedTable) == "try-error"){
clicktype3 = remDr$findElement(using = "css selector", '#stage-team-stats-options > li:nth-child(4) > a:nth-child(1)')
remDr$mouseMoveToLocation(webElement = clicktype3)
clicktype3$click()
webElem2 = remDr$findElement(using = "css selector", '#statistics-team-table-detailed')
webElemtext = try(webElem$getElementAttribute("outerHTML")[[1]])
detailedTable = try(readHTMLTable(webElemtext, header = T,as.data.frame = T)[[1]])
}
bigTable = merge(bigTable,detailedTable,by = "Team",all = T)
inTable = rbind(inTable,bigTable)
print(i)
flush.console()
}
remDr$close()
browseURL("http://localhost:4444/selenium-server/driver/?cmd=shutDownSeleniumServer")

#Adding a Year column to the final table
inTable$Year = rep(2015:2009, each=20, length.out = 140)

####Housekeeping! Again! I am about to join two tables based on their team names. This means I have to ensure that they are of the same format####
inTable$Team[inTable$Team == "Birmingham City"] = "Birmingham"
inTable$Team[inTable$Team == "Blackburn Rovers"] = "Blackburn"
inTable$Team[inTable$Team == "Bolton Wanderers"] = "Bolton"
inTable$Team[inTable$Team == "Cardiff City"] = "Cardiff"
inTable$Team[inTable$Team == "Norwich City"] = "Norwich"
inTable$Team[inTable$Team == "Swansea City"] = "Swansea"
inTable$Team[inTable$Team == "Stoke City"] = "Stoke"
inTable$Team[inTable$Team == "Tottenham Hotspur"] = "Tottenham"
inTable$Team[inTable$Team == "West Ham United"] = "West Ham"
inTable$Team[inTable$Team == "Wigan Athletic"] = "Wigan"

#Removing trailing and leading spaces in the team names
inTable$Team = gsub("^\\s+|\\s+$", "",inTable$Team)

#Merge final tables from skysports and whoscored together and remove duplicate columns
Datasetfinal = merge(inTable,datasetLagged,by = c("Year","Team"),all = T)
Datasetfinal <- Datasetfinal[, !duplicated(colnames(Datasetfinal), fromLast = TRUE)] 

#Converting the percentages to numbers
Datasetfinal$Left.Side = as.numeric(str_replace(Datasetfinal$Left.Side,"%",""))/100
Datasetfinal$Middle.of.the.pitch = as.numeric(str_replace(Datasetfinal$Middle.of.the.pitch,"%",""))/100
Datasetfinal$Right.Side = as.numeric(str_replace(Datasetfinal$Right.Side,"%",""))/100

#Removing not so useful columns
#Very noob of me I know.
Datasetfinal$R.x = NULL
Datasetfinal$Rating.x = NULL
Datasetfinal$R.y = NULL
Datasetfinal$Rating.y = NULL
Datasetfinal$Pl = NULL

#Convert column types. Again.
Datasetfinal[,3:37] = apply(Datasetfinal[,3:37],2,function(x) as.numeric(as.character(x)))

#Now, we have our data!
