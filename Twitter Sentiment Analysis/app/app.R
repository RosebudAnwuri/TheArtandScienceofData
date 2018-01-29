library(shiny)
library(shinydashboard)
library(twitteR)
library(base64enc)
library(RCurl)
library(RJSONIO)
library(stringr)
library(tm)
library(SnowballC)
library(httr)
library(sqldf)
library(tidytext)
library(wordcloud)
library(shinythemes)
library(ggplot2)
library(colourpicker)
library(plyr)
library(dplyr)
library(wesanderson)
library(scales)
library(shinysky)
library(knitr)
library(markdown)
library(lubridate)
library(tidyr)
library(plotly)


api_key <- "UE0sCwrNmxHb8YL759R7SuLEc" # From dev.twitter.com
api_secret <- "C9OxWPmBAOwzQ6G4VXbCKeXd3XEHG5XvJjzTA1AVLoKbtwnpJy" # From dev.twitter.com
token <- "370018889-WKxIRFsc8OJhvdtW3BOOdgIy1qGco48d7QlUO0in" # From dev.twitter.com
token_secret <- "7Ah8qplWJf5ey4zB4IPTTBlypMCUenXnQsrCH7808UbRE" # From dev.twitter.com

busyIndicators <-
  function(text = "Calculation in progress..",
           img = "shinysky/busyIndicator/ajaxloaderq.gif",
           wait = 1000) {
    tagList(
      singleton(tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "busyIndicator.css")
      ))
      ,
      div(class = "shinysky-busy-indicator", p(text), img(src = img))
      ,
      tags$script(
        sprintf(
          "	setInterval(function(){
          if ($('html').hasClass('shiny-busy')) {
          setTimeout(function() {
          if ($('html').hasClass('shiny-busy') ) {
          $('div.shinysky-busy-indicator').show()
          }
          }, %d)
          } else {
          $('div.shinysky-busy-indicator').hide()
          }
  },100)
          ",
          wait
      )
      )
      )
    }

ui = dashboardPage(skin = "blue",
                   
  dashboardHeader(title = "Sentiment Analysis"),
  dashboardSidebar(sidebarMenu(
    menuItem("Getting Started",tabName = "Start", icon = icon("toggle-on") ),
    menuItem("Sentiment Scoring Model",tabName = "Table", icon = icon("calculator") ),
    menuItem("Word Cloud Creator",tabName = "wordCloud",icon = icon("cloud-download")),
    menuItem("About",tabName = "about",icon = icon("info"))
    
    
  )),
  dashboardBody(
   tabItems(
     tabItem(tabName = "Start",
             fluidPage( tags$style(HTML("

                                        
                                        .box.box-solid.box-info > .box-header {
                                        
                                        
                                        color: #fff;
                                        background-color: #01B8AA;
                                        }
                                        
                                        
                                        
                                        ")),
                        tags$style(HTML("
                                        .box.box-solid.box-info{
                                        border-bottom-color:#01B8AA;
                                        border-left-color:#01B8AA;
                                        border-right-color:#01B8AA;
                                        border-top-color:#01B8AA;
                                        }
                                        }
                                        
                                        ")),
               box(solidHeader = T,width = 12,
                   status = "info",
                   title=h1(strong('Getting Started with the Twitter Sentiment Analysis App'),style ="font-size: 22pt;text-align:center;vertical-align: middle"),
                   #br(),
                   h2("Hello There! Welcome to the Twitter Sentiment Analysis App! Now, you may ask what exactly does that mean? Well, it's simple! This app helps you understand how Twitter users feel about a certain topic. The great thing about this is that the topic can be anything of your choice. You can search for the sentiment towards a person like Obama, a place like Seychelles or even a movie like Mad Max!","This easy-to-use app has lots of interesting features and even lets you download the data it uses for all of its analysis. So the next time you want to get a review of a movie before heading to the cinemas, you can simply get on this app and get a 'Twitter Review' here! :-)",style= "font-size: 14pt"),
                   h2("I hope that by now you are as excited about the app as I am! I'd give a brief tutorial on how to get started with the app and create your first sentiment dashboard!",style= "font-size: 14pt"),
                   h2(strong("How do I get started?"),style= "font-size: 14pt"),
                   h2("There are two main sections:","the",em(' Sentiment Scoring Model'),"and the",em('Word Cloud Creator.'),"These sections work the same way and you can get your first analysis done in four simple steps.",style= "font-size: 14pt"),
                   h2("1. Type your search term (e.g. Hillary Clinton, Grey's Anatomy, Nigeria) into the search box.",style= "font-size: 14pt"),
                   h2("2. Choose a time period.",style= "font-size: 14pt"),
                   h2("3. Hit the search button.",style= "font-size: 14pt"),
                   h2("4. Go grab some coffee while the app does the heavy lifting for you.",style= "font-size: 14pt"),
                   h2("And voila! The dashboard comes to life with lots of useful information on how Twitter users feel about what you just searched on. That's it! That's all you have to do to get your first Twitter Sentiment Analysis. You can also search on tweets to a particular user or from a certain user. For more on different search terms, check that out",a("here.",href="https://dev.twitter.com/rest/public/search",target = "_blank"),"Simply use the search term the way it is specified in the link.",style= "font-size: 14pt"),
                   h2(strong("Second Question: I've created my first sentiment analysis dashboard, now what?"),style= "font-size: 14pt"),
                   h2("The next thing is to explore the dashboard! Let's focus on where most of the magic happens.",style= "font-size: 14pt"),
                   h2("1. In the",em('Twitter Trends'),"box in the", em('Sentiment Scoring Model'),"section, you would notice two panels.",em("Variables to Visualize"),"and",em("Level of Aggregation."),em("Variables to Visualize"), "visualizes different variables like Number of Tweets, Average Sentiment from each phone type (Status Source) etc. while",em("Level of Aggregation"),"chooses if you get the mean or sum for the sentiment score.",style= "font-size: 14pt"),
                   br(),
                   imageOutput('Panels'),
                   tags$head(tags$style(
                     type="text/css",
                     "#Panels img {max-width: 50%; width: 100%; height: auto}"
                   )),
                   br(),
                   h2("2.	Go ahead and click on the drop down arrow to choose a different variable to visualize in that box. You would have a different plot in that box.",style= "font-size: 14pt"),
                   br(),
                   imageOutput('Nextgraph'),
                   tags$head(tags$style(
                     type="text/css",
                     "#Nextgraph img {width: auto; height: 100%}"
                   )),
                   br(),
                   h2("Pretty cool eh?",style= "font-size: 14pt"),
                   h4("There are some other features on the dashboard like:",style= "font-size: 14pt"),
                   h2("1.	Excluding tweets with no sentiment from the", em('Sentiment Distribution'),"histogram.",style= "font-size: 14pt"),
                   h2("2.	Searching, filtering, sorting and downloading the full dataset from the",em('Sentiment Scoring Table.'),style= "font-size: 14pt"),
                   h2("3.	Changing the",em('Dashboard Theme'), "based on the",a("wesanderson palette.",href="https://epijim.uk/code-snippets/wes-anderson/",style= "font-size: 14pt; display: inline;",target = "_blank"),style= "font-size: 14pt"),
                   h2(strong("Third Question: How do I use the Advanced/Optional Search options?"),style= "font-size: 14pt"),
                   h2("Easy! Just follow the instructions on the", em("Search bar.") ,"Want to exclude a word/user? Just type it in to the relevant search box.",style= "font-size: 14pt"), 
                   h2("Want to exclude multiple words/users? Put them all in that box and separate with either spaces or commas.",style= "font-size: 14pt"), 
                   h2("You can also exclude users that have a certain word. Let's say you want to remove all tweets from users that have the word 'Beyonce' in their usernames. All you have to do is put this in the appropriate search box: %Beyonce%.",style= "font-size: 14pt"),
                   h2("A few important things to note:",style= "font-size: 14pt"),
                   h2("1.	Your internet connectivity has to be pretty reliable for you to use the app hitch-free.",style= "font-size: 14pt"),
                   h2("2.	The search time could be non-trivial for a myriad of reasons.",style= "font-size: 14pt"),
                   h3("a.	The Twitter search API searches through all Tweets all over the world. That might take some time.",style= "font-size: 14pt; text-indent:25px"),
                   h3("b.	If you search for highly used words/phrases like Donald Trump, it would take significantly longer than searching for something like the twitter sentiment towards the philosophical trends of the 13th Century.",style= "font-size: 14pt; text-indent:25px"),
                   h3("c.	Twitter search API is rate limited. This means that after a number of searches per day, Twitter begins to block further searches for a period of time. So if you see that your search is 'stuck' on a completion status for a while, you are probably being rate limited.",style= "font-size: 14pt; text-indent:25px"),
                   h2("It's pretty easy and fun to use. So dig in! I hope you enjoy it.",style= "font-size: 14pt"),
                   h2("Happy Twitter Sentimenting! :-)",style= "font-size: 14pt")
                   
                   
               )
             )),
     
     tabItem(tabName = "Table",
             fluidRow(
               box(title = strong("Search Bar"),height = 700,dateRangeInput("daterange1",label = h4("Select time range"),max = Sys.Date(),min = Sys.Date()-10,start = Sys.Date()-10,end=Sys.Date()), 
                   tags$head(
                 tags$link(rel = "stylesheet", type = "text/css", href = "animate.min.css")
               ),tags$head(
                 tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
               ),
                   textInput('search1',h4("Enter search keyword:"),value = ""),
               
                   #em(h4("Tip: To make your twitter search faster, shorten the search time range")),
               actionLink("hider","Show Advanced Search"),   
                div(actionButton(
                     "Sarch1",
                     div("SEARCH", icon("search"), style = "text-align:center;font-size:10pt;width: 150px;"),
                     styleclass = "success",
                     size = "mini",
                     css.class = "animated infinite pulse"
                   ),style="text-align:center;"),
                   solidHeader = T,status = "primary",
                   
                   div(id="advanced",style="display:none;",textInput('sarch5',h4("Optional: Would you like to exclude tweets from a specific user(s)? Enter username(s):"),value=""),
                   em(h5("If you are not sure of the full username or you want to remove all usernames with a certain keyword, simply put the text in this format %searchWord% e.g %etisalat% will remove all screenNames containing Etisalat")),
                   textInput('sarch6',h4('Optional: Would you like to exclude tweets with certain key words? Enter below:'),value = ""))
                   ,HTML("<script> $('#hider').click( function (event) {
                          var hide_link =document.getElementById('hider').innerHTML
                          if(hide_link=='Show Advanced Search'){
                              document.getElementById('hider').innerHTML='Hide Advanced Search';
                              document.getElementById('advanced').style.display='block';
                          }
                          if(hide_link=='Hide Advanced Search'){
                              document.getElementById('hider').innerHTML='Show Advanced Search';
                         document.getElementById('advanced').style.display='none';
                         }
                   })</script>"),
                   selectizeInput('palette1',h4("Select Dashboard Theme:"),choices = c("GrandBudapest","Moonrise1","Royal1","Moonrise2","Cavalcanti","Royal2","GrandBudapest2","Moonrise3","Chevalier","Zissou","FantasticFox","Darjeeling","Rushmore"),selected = "Rushmore"),
                   footer = 
                     em(h4('You can seperate the keywords by either spaces or commas.'))
                   
               ),
               
               box(title = strong("Twitter Trends"),height = 700,conditionalPanel("typeof output.severalPlots !== 'undefined'",box(width = 5,solidHeader = T,height = 100,selectizeInput('graphs',h4('Variables to visualize'),choices = c("Number of Tweets","score","statusSource","Hourly score"),selected = "score"))
                                                                                  , conditionalPanel("input.graphs == 'score'",box(width = 5,height = 100,solidHeader = T,selectizeInput('Aggregation',h4('Level of aggregation'),choices = c("sum","mean"),selected = "mean")))),conditionalPanel("input.graphs == 'statusSource'",
                                                                                                                                                                                                                                                                                                   box(checkboxInput('showSent',h4("Show Average Sentiment for each Channel"),value = F),solidHeader = T,width = 5,height = 100)),
                   div(plotlyOutput('severalPlots',height = "100%"),style="padding-top:25%"),solidHeader = T,status = "success"
                   ,busyIndicators(
                     text = h4("Running...", style = "font-size: 40px; font-family:Papyrus;"),
                     img = "Loading1.gif"
                   )),
               valueBoxOutput('tweetno',width = 3),
               valueBoxOutput("averageSentiment",width = 3),
               valueBoxOutput("users1",width = 3),
               valueBoxOutput("souRce",width = 3),
               box(title = strong("Sentiment Distribution"),height = 700,conditionalPanel("typeof output.distribution !== 'undefined'",checkboxInput('showz',h4("Exclude tweets with no sentiment?"),value = FALSE)),plotOutput("distribution"),solidHeader = T,status = "warning"),
               
             box(div(dataTableOutput('twitterTable'), style = "font-size: 90%; width: 90%"),height = 700,title = strong("Sentiment Scoring Table"),solidHeader = T,status = "info",conditionalPanel(condition = "typeof output.twitterTable !=='undefined'",downloadButton("downloadtable",label = "Download Full CSV"))
                 )
             
             
     )),
      tabItem(tabName = "wordCloud",
 fluidRow(tags$style(HTML("

                          
                          .box.box-solid.box-warning > .box-header {
                          
                          
                          color: #fff;
                          background-color: #F2C80F ;
                          }
                          
                          
                          
                          ")),
          tags$style(HTML("
                          .box.box-solid.box-warning{
                          border-bottom-color:#F2C80F;
                          border-left-color:#F2C80F;
                          border-right-color:#F2C80F;
                          border-top-color:#F2C80F;
                          }
                          }
                          
                          ")),tags$style(HTML("

                                              
                                              .box.box-solid.box-success > .box-header {
                                              
                                              
                                              color: #fff;
                                              background-color: #FE9666 ;
                                              }
                                              
                                              
                                              
                                              ")),
          tags$style(HTML("
                          .box.box-solid.box-success{
                          border-bottom-color:#FE9666;
                          border-left-color:#FE9666;
                          border-right-color:#FE9666;
                          border-top-color:#FE9666;
                          }
                          }
                          
                          ")),
      box(title=strong("Search Bar"),solidHeader = TRUE,status = "primary",dateRangeInput('daterange',h4('Select a Date Range'),max = Sys.Date(),min = Sys.Date()-10,start = Sys.Date()-10,end=Sys.Date()),
          textInput('search',h4('Enter your search term'),value = ""),
          #em(h4("Tip: To make your twitter search faster, shorten the search time range")),
         footer = selectizeInput('palette',h4("Select Dashboard Theme:"),choices = c("GrandBudapest","Moonrise1","Royal1","Moonrise2","Cavalcanti","Royal2","GrandBudapest2","Moonrise3","Chevalier","Zissou","FantasticFox","Darjeeling","Rushmore"),selected = "Rushmore"),
         h4(uiOutput('advButton')),
         conditionalPanel('output.labella== false',textInput('sarch2',h4("Would you like to exclude tweets from a specific user(s)? Enter username(s):"),value = ""),
                          textInput('sarch3',h4('Would you like to exclude tweets with certain key words? Enter below:'),value = ""),
                          textInput('sarch4',h4('Would you want to exclude certain words from your wordcloud? Enter below:'),value = ""),
                          em(h4('You can seperate the keywords by either spaces or commas'))),
         div(actionButton(
           "Sarch",
           div("SEARCH", icon("search"), style = "text-align:center;font-size:10pt;width: 150px;"),
           styleclass = "success",
           size = "mini",
           css.class = "animated infinite pulse"
         ),style="text-align:center;")
          ),
      box(title = strong("Word Cloud"), 
          plotOutput("Cloud",height = "490px"),
          textOutput("timer"),
          status = "success",solidHeader = T,
          height = 550),
       busyIndicator(text = "Creating Word Cloud...",wait = 1000)
      ,box(title = strong("Percentage Share of Word Sentiment"),solidHeader = T,status = "warning",plotlyOutput('topWords',height = "400px"),height = 500)
      
      )
 ),
 tabItem(tabName = "about",
         fluidPage( tags$style(HTML("

               
.box.box-solid.box-primary > .box-header {


                                    color: #fff;
                                    background-color: #689FB0 ;
                                    }


                                    
")),
tags$style(HTML("
.box.box-solid.box-primary{
border-bottom-color:#689FB0;
border-left-color:#689FB0;
border-right-color:#689FB0;
border-top-color:#689FB0;
}
   }
                                    
                                    ")),
           
           box(solidHeader = T,status = "primary",title =h2(strong("The Art and Science of Data"),style= "font-size: 22pt;text-align:center;"),br(),
             div("This Twitter Sentiment Application was humbly created by me (Rosebud Anwuri). If you like this and would like to see more of the stuff I work on, you can visit my blog here:",a("The Art and Science of Data.",style= "font-size: 16pt; display: inline;",href="http://theartandscienceofdata.wordpress.com",target="_blank"),"My blog is mostly focused on the application of Data Science to everyday life and culture. One of major aims of this blog is to to make Data Science more accessible, less technical and much more fun to a wider audience. Feel free to add a comment or drop me a message for questions or suggestions. Thank you!
",style= "font-size: 16pt") ,width = 12,imageOutput('testImage'),height=1000),
tags$head(tags$style(
  type="text/css",
  "#testImage img {max-width: 100%; width: 100%; height: auto}"
))
           
         ))

)
))




server = shinyServer(function(input, output, session){
  setup_twitter_oauth(api_key, api_secret, token, token_secret)
  token <- get("oauth_token", twitteR:::oauth_cache)
  token$cache()
  output$testImage = renderImage({list(src="www/Page2.PNG")},deleteFile = F)
  output$Panels = renderImage({list(src="www/Panels.PNG")},deleteFile = F)
  output$Nextgraph = renderImage({list(src="www/Nextgraph.PNG")},deleteFile = F)
  
  tablecreator= function(word){
    
    date_time = as.Date(input$daterange[1], format = "yyyy-mm-dd")
    tweetData = NULL
    withProgress(message = paste("Collecting",as.numeric(input$daterange[2]-input$daterange[1]+1),"day(s) worth of data..."),value = 1,{
      for (i in 0:as.numeric(input$daterange[2]-input$daterange[1]+1)){
          while (date_time <= input$daterange[2]){
            date_time = as.character.Date(date_time+1)
            
            tweetdetails <- searchTwitter(word, n=50, lang="en", since = paste(input$daterange[1]),until = date_time, geocode = '10,9, 200km')
            
            tweetData = append(tweetData, tweetdetails)
            i = i+1
            incProgress(1,message = paste0(round(100*i/as.numeric(input$daterange[2]-input$daterange[1]+1),0),"% Complete.."))
            
            date_time = as.Date(date_time)
          }
          
        
      }
    })
    tweetDataFinal = twListToDF(tweetData)
    tweetDataFinal$text = enc2native(tweetDataFinal$text)
    tweetDataFinal$text = gsub('http\\S+\\s*', "", tweetDataFinal$text)
    tweetDataFinal$text = str_replace_all(tweetDataFinal$text,'@\\S+',"")
    tweetDataFinal$text = str_replace_all(tweetDataFinal$text, "RT ", "")
    tweetDataFinal$text = gsub("<.*>",'',tweetDataFinal$text)
    tweetDataFinal$text = tolower(gsub("[^0-9A-Za-z/// ]", "", tweetDataFinal$text, T))
    tweetDataFinal = tweetDataFinal[!duplicated(tweetDataFinal["text"]),]
    if(input$sarch2 != ''){

      if(length(unlist(str_split(input$sarch2,",")))>1){
        worder4 = unlist(str_split(input$sarch2,","))
        for (i in worder4){
          tweetDataFinal = filter(tweetDataFinal, screenName != i)
        }
      }
      else if (length(unlist(str_split(input$sarch2," ")))>1) {
        worder4 = unlist(str_split(input$sarch2," "))
        for (i in worder4)
        {tweetDataFinal = filter(tweetDataFinal, screenName != i)
        }
      }
      else{
        tweetDataFinal = filter(tweetDataFinal, screenName != input$sarch2)
      }


    }
    if(input$sarch3 != ''){

        if(length(unlist(str_split(input$sarch3,",")))>1){
          worder1 = unlist(str_split(input$sarch3,","))
          for (i in worder1){
            tweetDataFinal = filter(tweetDataFinal, !grepl(i, text,ignore.case = T))
          }
        }
        else if (length(unlist(str_split(input$sarch3," ")))>1) {
          worder1 = unlist(str_split(input$sarch3," "))
          for (i in worder1)
          {tweetDataFinal = filter(tweetDataFinal, !grepl(i, text,ignore.case = T))
          }
        }
        else{
          tweetDataFinal = filter(tweetDataFinal, !grepl(input$sarch3, text,ignore.case = T))
        }


    }
    
    return(tweetDataFinal)
  }
  output$selection = renderUI({if (input$daterange[2]-input$daterange[1] <=3) return("score")
    else return("Hourly score")})
  
  tablecreator1= function(word){
    date_time = as.Date(input$daterange1[1], format = "yyyy-mm-dd")
    tweetData = NULL
    withProgress(message = paste("Collecting ",as.numeric(input$daterange1[2]-input$daterange1[1]+1),"day(s) worth of data..."),value = 1,{
      for (i in 0:as.numeric(input$daterange1[2]-input$daterange1[1])){
          while (date_time <= input$daterange1[2]){
            date_time = as.character.Date(date_time+1)
            
            tweetdetails <- searchTwitter(word, n=50, lang="en", since = paste(input$daterange1[1]),until=date_time,geocode = '10,9, 200km')
            
            tweetData = append(tweetData, tweetdetails)
            i = i+1
            incProgress(1,message = paste0(round(100*i/as.numeric(input$daterange1[2]-input$daterange1[1]+1),0),"% Complete.."))
            
             date_time = as.Date(date_time)
             print(date_time)
             flush.console()
             
          }
      }
    })
    tweetDataFinal = twListToDF(tweetData)
   # tweetDataFinal$text = enc2native(tweetDataFinal$text)
    tweetDataFinal$text = gsub('http\\S+\\s*', "", tweetDataFinal$text)
    tweetDataFinal$text = str_replace_all(tweetDataFinal$text,'@\\S+',"")
    tweetDataFinal$text = str_replace_all(tweetDataFinal$text, "RT ", "")
    tweetDataFinal$text = gsub("<.*>",'',tweetDataFinal$text)
    tweetDataFinal$text = tolower(gsub("[^0-9A-Za-z/// ]", "", tweetDataFinal$text, T))
    
    tweetDataFinal = tweetDataFinal[!duplicated(tweetDataFinal["text"]),]
    #tweetDataFinal = subset(tweetDataFinal,!grepl(input$search1,screenName,ignore.case = T))
    
    if(input$sarch5 !=''){
      
      
      if(str_count(input$sarch5,",")>0){
        sarch5 = gsub("^\\s+|\\s+$", "",input$sarch5)
        word_list = unlist(str_split(sarch5,","))
        word_list = unlist(lapply(word_list,function(x) gsub("^\\s+|\\s+$", "",x)))
        for (i in 1:length(word_list)){
          if (str_count(word_list[i],"%")>0){
            start = unlist(str_locate_all(word_list[i],"%"))[1]
            end = unlist(str_locate_all(word_list[i],"%"))[2]
            wordExtract = substring(word_list[i],start+1,end-1)
            tweetDataFinal = subset(tweetDataFinal,!grepl(wordExtract,screenName,ignore.case = T))
          }
          else {
          tweetDataFinal = subset(tweetDataFinal,screenName !=word_list[i])
          }
        }
      }
      else{
        sarch5 = gsub("^\\s+|\\s+$", "",input$sarch5)
        word_list = unlist(str_split(sarch5," "))
        word_list = unlist(lapply(word_list,function(x) gsub("^\\s+|\\s+$", "",x)))
        for (i in 1:length(word_list)){
          if (str_count(word_list[i],"%")>0){
            start = unlist(str_locate_all(word_list[i],"%"))[1]
            end = unlist(str_locate_all(word_list[i],"%"))[2]
            wordExtract = substring(word_list[i],start+1,end-1)
            tweetDataFinal = subset(tweetDataFinal,!grepl(wordExtract,screenName,ignore.case = T))
          }
          else{
          tweetDataFinal = subset(tweetDataFinal,screenName !=word_list[i])
          }
        }
      }
      }
    
    if(input$sarch6 != ''){
      if(str_count(input$sarch6,",")>0){
        word_list = unlist(str_split(input$sarch6,","))
        word_list = unlist(lapply(word_list,function(x) gsub("^\\s+|\\s+$", "",x)))
        for (i in word_list){
          tweetDataFinal= subset(tweetDataFinal,!grepl(i,text,ignore.case = T))
        }
      }
      else{
        word_list = unlist(str_split(input$sarch6," "))
        word_list = unlist(lapply(word_list,function(x) gsub("^\\s+|\\s+$", "",x)))
        for (i in word_list){
          tweetDataFinal= subset(tweetDataFinal,!grepl(i,text,ignore.case = T))
        }
      }
    }
    tweetDataFinal$score=apply(data.frame(tweetDataFinal$text),1,sentiment_score)
    tweetDataFinal$statusSource=gsub(pattern = "[^0-9A-Za-z/// ]",'',str_extract(tweetDataFinal$statusSource,">.*<"))
    return(tweetDataFinal)
    
  }
 
  sentiment_score = function(sentence) {
    lexicon = sentiments %>% filter(lexicon == "AFINN")
    lexicon$score[lexicon$word == 'irresponsible'] = -2
    search_words = unlist(str_split(tolower(input$search1),' '))
    search_words = unlist(lapply(search_words,function(x) gsub("^\\s+|\\s+$", "",x)))
    for (i in search_words){
      lexicon = subset(lexicon,word != i)
    }
    score = 0
    words = str_split(sentence, " ")
    words = unlist(words)
    for (i in 1:length(words)) {
      if (words[i] == "not") {
        word = paste("not", words[i + 1])
        word.split = unlist(str_split(word, " "))
        if (word %in% lexicon$word == T) {
          score = score + lexicon$score[lexicon$word == word]
          
        }
        else if (word.split[2] %in% lexicon$word == T) {
          score = score - lexicon$score[lexicon$word == word.split[2]]
          
        }
        
        
      }
      else if (i > 1 && words[i - 1] == "not")
        next
      
      else if (words[i] %in% lexicon$word == T) {
        score = score + lexicon$score[lexicon$word == words[i]]
        
      }
      
      
    }
    return (score)
    
  }
  
  
  cloudCreator = function(df){
    corpus = Corpus(VectorSource(df$text))
    corpus = tm_map(corpus, removeNumbers)
    corpus = tm_map(corpus, removeWords, c("amp",stopwords("english")))
    exc = unlist(str_split(tolower(input$search)," "))
    for (i in 1:length(exc)){
      corpus = tm_map(corpus, removeWords, exc[i])
    }
    corpus = tm_map(corpus, removeWords, stop_words$word)
    corpus = tm_map(corpus, removeWords, removePunctuation(stop_words$word))
    corpus = tm_map(corpus, removePunctuation)
    corpus = tm_map(corpus, content_transformer(stripWhitespace))
    corpus = tm_map(corpus, stemDocument)
    corpus = tm_map(corpus, content_transformer(tolower))
    
   
    if (input$sarch4 != ''){

      if(length(unlist(str_split(input$sarch4,",")))>1){
      worder = unlist(str_split(tolower(input$sarch4),","))
      for (i in worder){
        corpus = tm_map(corpus,removeWords,i)
      }
  }
      else if (length(unlist(str_split(input$sarch4," ")))>1) {
        worder =unlist(str_split(tolower(input$sarch4)," "))
        for (i in worder)
        {corpus = tm_map(corpus,removeWords,i)
        }
      }
      else{
        corpus = tm_map(corpus,removeWords,tolower(input$sarch4))
      }
    }
    corpus = tm_map(corpus, content_transformer(stripWhitespace))
    
    
    corpus = tm_map(corpus, PlainTextDocument)
    corpus <- Corpus(VectorSource(corpus))
    
    
    frequencies = DocumentTermMatrix(corpus)
    
    findFreqTerms(frequencies, lowfreq=20)
    
    wordTweet = as.data.frame(as.matrix(frequencies))
  
    return(wordTweet)
} 
  Clouding = eventReactive(input$Sarch,{cloudCreator(df=tablecreator(input$search))})
  output$Cloud = renderPlot({ wordcloud(colnames(Clouding()),colSums(Clouding()),colors = wes_palette(name = input$palette,type = "discrete"),max.words = 300,rot.per = 0,font = 2)})
  wordCloud = reactive({wordcloud(colnames(Clouding()),colSums(Clouding()),colors = wes_palette(name = input$palette,type = "discrete"),max.words  = 300,rot.per = 0,font = 2)})
  
 topWords = function(){
   x1= Clouding()%>%
     summarise_all(funs(sum)) %>%
     gather(Word,frequency) %>%
     inner_join(.,sentiments %>%
                 filter(lexicon=="nrc") %>%
                 rename(Word=word),by="Word") %>%
     select(-lexicon,-score) %>%
     group_by(sentiment) %>%
     summarise(total_sentiment=sum(frequency))
  
  colors=c('#0091ea','#e74c3c','#f06292','#00838f','#f1c40f','#ff9800','#00bfa5','rgba(236, 240, 241,1.0)','#7f8c8d','#2c3e50')
  return( 
    x1%>%
     plot_ly(labels=~sentiment,values=~total_sentiment,textinfo="percent", insidetextfont = list(color = '#FFFFFF'), marker = list(colors = colors,
                                                                                          line = list(color = '#FFFFFF', width = 1))) %>%
      add_pie(hole=0.6) %>%
      layout(showlegend=TRUE)
    

)

 }
 output$topWords = renderPlotly({topWords()})
  output$downloadtable <- downloadHandler(
    filename = function() { paste("Tweets on",input$search,'_',Sys.time(), '.csv', sep='') },
    content = function(file) {
      write.csv(tableReact(), file,row.names = F)
    }
  )
 
  
tableReact = eventReactive(input$Sarch1,{tablecreator1(input$search1)})
output$twitterTable =renderDataTable({
  tableReact() %>%
    mutate(text=substr(text,1,45)) %>%
    select(created,text,statusSource,screenName,score)},options = list(pageLength = 5))
             
output$averageSentiment =renderValueBox({if(mean(tableReact()$score)>0) return(valueBox(value=round(mean(tableReact()$score),2),subtitle = h5("Average Sentiment Score"),color = "green",icon = icon("thumbs-o-up"),width = 4))
                            else if(mean(tableReact()$score)<0) return(valueBox(round(mean(tableReact()$score),2),subtitle = "Average Sentiment Score",color = "red",icon = icon("thumbs-o-down"),width = 4))
  })
search_term = reactive({if(str_count(input$search1, ':') > 0){
  
  return(str_replace(input$search1,":",' '))
}
  else{
    return(paste("on",input$search1))
  }

})
output$users1 = renderValueBox({
  valueBox(value=nrow(tableReact()[!duplicated(tableReact()["screenName"]),]),subtitle = paste("People tweeting",search_term()),icon = icon("users"),width = 4,color = "yellow")

  })
output$tweetno = renderValueBox({
  valueBox(
  value=nrow(tableReact()),subtitle = paste("Tweets",search_term()),icon = icon("twitter"),width = 4)
  })
topphone = reactive({gsub('Twitter for ','',tableReact() %>% group_by(statusSource) %>% dplyr::summarise(scored = mean(score),N=n()) %>% arrange(-N) %>% top_n(1) %>% select(statusSource))
})
output$souRce = renderValueBox({valueBox(value = paste0(round(as.numeric(tableReact() %>% group_by(statusSource) %>% dplyr::summarise(scored = mean(score),N=n()) %>% arrange(-N) %>% top_n(1) %>% select(N)/nrow(tableReact()))*100,2),"%"),subtitle = paste0("Percent of people tweeting from ",topphone(),"s"),icon = icon("mobile"),color = "purple")})
output$distribution = renderPlot({if(input$showz == F) return(hist(tableReact()$score,main="Distribution of Sentiment",xlab = "Sentiment Score",col = wes_palette(input$palette1)[floor(runif(1,1,length(wes_palette(input$palette1))))] ,font = 14))
  hist(tableReact()$score[tableReact()$score != 0],main="Distribution of Sentiment",xlab = "Sentiment Score",col = wes_palette(input$palette1)[floor(runif(1,1,length(wes_palette(input$palette1))))],font = 14)
  })
severalPlot = function(){
  if(input$graphs == "Number of Tweets"){
    
    if (input$daterange1[2] - input$daterange1[1] +1 <= 3){
      return(
        ggplot(tableReact())+
          stat_count(aes(as.Date(created)),geom = "bar",fill = wes_palette(input$palette1)[floor(runif(1,1,length(wes_palette(input$palette1))))],size = 8)+
          theme_classic()+
          xlab("Date")+ylab(paste("Total", input$graphs))+
          ggtitle(paste('Trend of Total', input$graphs))+
          theme(text=element_text(size = 14))
      )
    }
    else {
    return( ggplot(tableReact())+
              stat_count(aes(as.Date(created)),geom = "line",size =1.1,color = wes_palette(input$palette1)[ceiling(runif(1,1,length(wes_palette(input$palette1))))])+
              theme_classic()+
              xlab("Date")+ylab(paste("Total", input$graphs))+
              ggtitle(paste('Trend of Total', input$graphs))+
              theme(text=element_text(size = 14)))
    }
  }
  else if(input$graphs == "Hourly score"){
    return(tableReact() %>%
      mutate(Hour = hour(ymd_hms(created))) %>%
      ggplot(aes(Hour,score))+
      stat_summary(geom = "line",fun.y ="mean",size=1.1,color = wes_palette(input$palette1)[floor(runif(1,1,length(wes_palette(input$palette1))))])+
      xlab('Hour of the day')+
      ylab('Average Sentiment Score')+
      theme_classic()+
      ggtitle("Average Sentiment Score per Hour")+
      theme(text=element_text(size = 14),panel.background = element_rect(colour = "white"),plot.background = element_rect(colour = "white"))
      
    )
  }
  else if (input$graphs == "statusSource"){
    
    if(input$showSent == T){
     return (tableReact() %>%
        group_by(statusSource) %>%
        dplyr::summarise(scored = mean(score),N=n()) %>%
        arrange(-N) %>%
        top_n(10) %>%
        mutate(Sentiment=ifelse(scored > 0,"Positive",ifelse(scored<0,"Negative","Neutral"))) %>%
        transform(statusSource = reorder(statusSource,-N)) %>%
        ggplot+
        stat_summary(aes(statusSource,N,fill = Sentiment),geom = "bar",fun.y = "sum")+
        geom_text(aes(x=statusSource,y=N,label=round(scored,2)))+
        theme_classic()+
        xlab("Source of Tweet")+ylab("Number of Tweets")+
        ggtitle(paste('Number of Tweets for each Channel'))+
        theme(text=element_text(size = 14),legend.title=element_blank(),axis.text.x = element_text(angle = 90, hjust = 1),panel.background = element_rect(colour = "white"),plot.background = element_rect(colour = "white"))+
        
        scale_fill_manual(values = c('Positive' = "seagreen3","Negative" = "firebrick3","Neutral"="ivory3"))
      )
    }
    else {
    return(tableReact() %>% 
             group_by(statusSource) %>%
             tally(sort = TRUE) %>%
             top_n(10) %>% 
             transform(statusSource = reorder(statusSource,-n)) %>%
             ggplot+
             stat_sum(aes(statusSource,n),geom = "bar",fill = wes_palette(input$palette1)[floor(runif(1,1,length(wes_palette(input$palette1))))],show.legend = F)+
             theme_classic()+
             xlab('Source of Tweet')+
             ylab('Number of Tweets')+
             ggtitle("Number of Tweets for each Channel")+
             theme(text=element_text(size = 14),axis.text.x = element_text(angle = 90, hjust = 1),panel.background = element_rect(colour = "white"),plot.background = element_rect(colour = "white"))
    )
    
    }
  }
  else if(input$graphs == 'score'){
    if (input$daterange1[2] - input$daterange1[1] +1 <= 3){
      return( ggplot(tableReact(),aes_string("as.Date(created)",input$graphs))+
                stat_summary(fun.y = input$Aggregation,geom = "bar",fill = wes_palette(input$palette1)[ceiling(runif(1,1,length(wes_palette(input$palette1))))])+
                theme_classic()+
                xlab("Date")+ylab(paste("Total", input$graphs))+
                ggtitle(paste('Trend of Total', input$graphs))+
                theme(text=element_text(size = 14)))
    }
    else {
   return(ggplot(tableReact(),aes_string("as.Date(created)",input$graphs))+
            stat_summary(fun.y = input$Aggregation,geom = "line",size =1.1,color = wes_palette(input$palette1)[floor(runif(1,1,length(wes_palette(input$palette1))))])+
            theme_classic()+
            xlab("Date")+ylab(paste("Average",input$graphs))+
            ggtitle(paste('Trend of Average',input$graphs))+
            theme(text=element_text(size = 14)))
    }
  }
  
}
  
  

output$severalPlots = renderPlotly({ggplotly(severalPlot(),tooltip="y")})
vars = reactiveValues(counter = 0)

label = reactive({
  if(vars$counter%%2 == 0)label = 'Show Advanced Search'
  else label = 'Hide Advanced Search'
  
})

output$advButton = renderUI({
  actionLink("click", label = label())
})
observeEvent(input$click,{
  vars$counter = vars$counter+1
}

)

output$labella = reactive({
  vars$counter%%2 == 0
  
})

outputOptions(output, "labella", suspendWhenHidden = FALSE)




  
})



shinyApp(ui = ui, server = server)