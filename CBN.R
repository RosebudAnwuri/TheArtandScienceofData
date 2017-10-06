library(h2o)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(plyr)
library(dplyr)
library(wesanderson)
library(scales)
library(lubridate)
#library(shinysky)
library(extrafont)
library(stringr)
h2o.init()

df = read.csv("C:/Users/rose.anwuri/OneDrive/TheArtandScienceofData/billionaire_data.csv")
dat = df %>% 
  filter(Cluster !="The Newbie") %>%
  filter(is.na(Age)==F)

#model = h2o.loadModel("C:\\Users\\rose.anwuri\\Documents\\TheArtandScienceofData\\DRF_model_R_1501438266398_1")
Countries=levels(dat$Country)
Sectors = levels(dat$Sector)
Relations = levels(dat$relation)
ui = shinyUI(
  navbarPage("Billion Dollar Questions",inverse = F,collapsible = T,fluid = T,
             theme=shinytheme("flatly"),
             tabPanel("What Type of Billionaire Are You?", icon = icon("money"),
                      sidebarLayout(position = 'left',
                                    sidebarPanel(id = "sidebar",
                                                 selectizeInput("Country",label = h4("What country are you from?",style ="font-size: 12pt;"),choices=Countries),
                                                 numericInput('Age',h4("How Old Are You?",style ="font-size: 12pt;"),value = 20,min = 12,max = 100),
                                                 selectizeInput("selfMade",h4("Do you have your own company (or plan to have one)?",style ="font-size: 12pt;"),choices = c("Yes","No")),
                                                 conditionalPanel("input.selfMade=='Yes'",selectizeInput("relation",h4("Choose one below that best describes your role in the business:",style ="font-size: 12pt;"),choices = Relations)),
                                                 conditionalPanel("input.selfMade=='No'",selectizeInput("buffer",h4("That's okay. Think of a business where you are likely to make money from. What best describes your role in that company?",style ="font-size: 12pt;"),choices = Relations)),
                                                 numericInput("founding_year",h4("When was/will this business (be) established?",style ="font-size: 12pt;"),value = 1999,min=1600,max=year(Sys.Date())+10),
                                                 selectizeInput("Sector",h4("What Sector is this business?",style ="font-size: 12pt;"),choices = Sectors),
                                                 selectizeInput("Bsc",h4("Do you have a Bachelor's Degree?",style ="font-size: 12pt;"),choices = c("Yes","No")),
                                                 tags$head(
                                                   tags$style(HTML('#run{border-radius: 0px;}'))
                                                 ),
                                                 div(actionButton("run",div("PREDICT",icon("flask"),style="text-align:center;font-size:10pt;"),styleclass = "success",size = "mini",css.class = "z-depth-5"),style="text-align:left;")
                                                 
                                                 
                                    ),
                                    mainPanel(position="right",
                                              br(),
                                              tags$head(
                                                tags$link(rel = "stylesheet", type = "text/css", href = "materialize.min.css")
                                              ),
                                              tags$head(tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/js/materialize.min.js")),
                                              
                                                              HTML(
                                                              '
                                                                 
 <div class="row">
                                                                   <div class="col s12 m12">
                                                                   <div class="card teal">
                                                                   <div class="card-content white-text">
                                                                   
                                                                   
                                                                   <p>Have you ever wondered what sort of billionaire you would end up as? A Consistent one, A Hustler or a Ghost?
                                                                   Learn a bit more about it on the link below and predict the one you could be with this simple app!</p>
                                                                   </div>
                                                                   <div style="background-color:#ffffff;">
                                                                   <div class="card-action">
                                                                   <div style="text-align:center;">
                                                                   <a style="color:#009688; font-family:Papyrus;" href="http://theartandscienceofdata.wordpress.com/blog" target"_blank">The Art and Science of Data</a>
                                                                   </div>
                                                                   </div>
                                                                   </div>
                                                                   
                                                                   </div>
                                                                   </div>
                                                                   </div>'),
                                              # busyIndicator(text = h4("Running Model...",style="font-size: 40px; font-family:Papyrus;"),img = "shinysky/busyIndicator/Loading3.gif"),
                                              
                                              textOutput("prefix_final"),
                                              div(imageOutput("image_final"), style="text-align: center;"),
                                              textOutput("prediction_final"),
                                              br(),
                                              tags$head(tags$style("#prefix_final{font-size: 40px; text-align:center;}")),
                                              tags$head(tags$style("#prediction_final{font-size: 40px; text-align:center;}")),
                                              conditionalPanel("typeof output.image_final !== 'undefined'",div(uiOutput("urlInput"),style="text-align:center;"))
                                              
                                    ))),
             tabPanel("About", icon = icon("info"),
             
             box(width = 12,title= h2(strong("The Art and Science of Data",img(src="Typing.gif",height=60)),style= "font-size: 36pt;text-align:center; font-family:Papyrus; color:#009688;"),
             br(),
             div("This application was humbly created by me (Rosebud Anwuri). It's purely for fun and in no way guarantees you'd become a billionaire (I'm sorry). If you like this and would like to see more of the stuff I work on, you can visit my blog here:",a("The Art and Science of Data.",style= "font-size: 16pt; display: inline;color:#009688;",href="http://theartandscienceofdata.wordpress.com",target="_blank"),"My blog is mostly focused on the application of Data Science to everyday life and culture to make it more accessible, less technical and much more fun to a wider audience. Feel free to add a comment or drop me a message for questions or suggestions. Thank you!",style= "font-size: 14pt; font-family:Helvetica;")
             ))
             ))






server = shinyServer(function(input, output, session){
  category_predictors = function(){
    self_made=ifelse(input$selfMade=="Yes","Y","N")
    year_born = year(Sys.Date())-input$Age
    age_at_start = ifelse(input$founding_year<year_born,0,input$founding_year-year(Sys.Date())+input$Age)
    bachelors_degree=ifelse(input$Bsc=="Yes","Y","N")
    Worth = round(runif(1,1,10),1)
    relationship = ifelse(input$selfMade=="Yes",input$relation,input$buffer)
    data_list = list(Worth,input$Country,self_made,bachelors_degree,input$Sector,
                     input$founding_year,relationship,age_at_start,input$Age)
    names(data_list)=model@parameters$x
    data_list =as.h2o(data_list)
    prediction = predict(model,data_list)$predict
    prediction = as.vector(prediction)
    prediction=sub("The","A",paste(prediction,"Billionaire"))
    
    return(prediction)
  }
  prediction_reactive= eventReactive(input$run, {
    paste(category_predictors())
  })
  output$prediction_final = renderText({prediction_reactive()})
  output$urlInput = renderUI({
    a(p(icon("twitter"),"Tweet It"),type="button",class = "waves-effect waves-light btn shiny-material-button z-depth-5" , 
      style = "fontweight:600; background-color: #4099FF; color: #FFFFFF;", target = "_blank",href=paste0(sprintf('https://twitter.com/intent/tweet?text=I+am+going+to+be+%s', 
                                                                                                                  str_replace_all(prediction_reactive()," ","+")),"!+Check+yours+out+at:&url=https%3A%2F%2Ftheartandscienceofdata.shinyapps.io%2FBillionDollarQuestions") )
  })
  
  
  result_prefix = function(prediction){
    prefix = ifelse(prediction=="A Consistent Billionaire","Congratulations! You Are:",
                    ifelse(prediction=="A Ghost Billionaire", "Oops! Looks like You're going be:",
                           "Hmmmm... Looks like You're going to be:"))
    return(prefix)
  }
  prefix_reactive= eventReactive(input$run, {
    result_prefix(prediction_reactive())
  })
  output$prefix_final = renderText({prefix_reactive()})
  
  image_per_prediction = function(prediction){
    image = ifelse(prediction=="A Consistent Billionaire","www/The Consistent.PNG",
                    ifelse(prediction=="A Ghost Billionaire", "www/The Ghost.PNG",
                           "www/The Hustler.PNG"))
    return(image)
  }
  
  image_reactive= eventReactive(input$run, {
    image_per_prediction(prediction_reactive())
  })
  output$image_final = renderImage({list(src=image_reactive())},deleteFile = F)
  
  
  output$niceGIF = renderImage({list(src="www/Typing.gif")},deleteFile = F)
  
})


shinyApp(ui = ui, server = server)