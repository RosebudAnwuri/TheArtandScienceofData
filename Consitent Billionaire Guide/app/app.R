library(shiny)
library(shinymaterial)
library(h2o)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(plyr)
library(dplyr)
library(wesanderson)
library(scales)
library(lubridate)
library(shinysky)
library(extrafont)
library(stringr)
h2o.init(strict_version_check = F)
df = read.csv("billionaire_data.csv")
dat = df %>% 
  filter(Cluster !="The Newbie") %>%
  filter(is.na(Age)==F)
model = h2o.loadModel("randomForestModel")
Countries=levels(dat$Country)
Sectors = levels(dat$Sector)
Relations = levels(dat$relation)
busyIndicators <- function(text = "Calculation in progress..",img = "shinysky/busyIndicator/ajaxloaderq.gif", wait=1000) {
  tagList(
    singleton(tags$head(
      tags$link(rel="stylesheet", type="text/css",href="busyIndicator.css")
    ))
    ,div(class="shinysky-busy-indicator",p(text),img(src=img))
    ,tags$script(sprintf(
      "	setInterval(function(){
      if ($('html').hasClass('shiny-busy')) {
      setTimeout(function() {
      if ($('html').hasClass('shiny-busy')) {
      $('div.shinysky-busy-indicator').show()
      }
      }, %d)  		    
      } else {
      $('div.shinysky-busy-indicator').hide()
      }
},100)
      ",wait)
    )
  )	
  }
ui <- material_page(title = "Billionaire Dollar Questions",nav_bar_color = "teal",background_color = "white",
                    material_tabs(color = "teal",
                      tabs = c(
                        "Predict Your Category" = "first_tab",
                        "About" = "second_tab"
                      )
                    ),
                    material_tab_content(tab_id = "first_tab",
                                         tags$br(),
                                         br(),
                                         material_row(
                                           material_column(width = 4,
                                           material_card(title="",depth=4,
                                           material_dropdown("Country",label = "What country are you from?",choices=Countries),
                                           material_number_box('Age',"How Old Are You?",initial_value = 20,min_value =  12,max_value =  100),
                                           material_dropdown(input_id = "selfMade",label = "Do you have your own company (or plan to have one)?",choices = c("Yes","No")),
                                           tags$br(),
                                           conditionalPanel("input.selfMade=='Yes'",material_dropdown("relation",label = "Choose one below that best describes your role in the business:",choices = Relations)),
                                           conditionalPanel("input.selfMade== 'No'",material_dropdown("buffer","That's okay. Think of a business where you are likely to make money from. What best describes your role in that company?",choices = Relations)),
                                           material_number_box("founding_year","When was/will this business (be) established?",initial_value = 1999,min_value=1600,max_value=year(Sys.Date())+10),
                                           material_dropdown("Sector","What Sector is this business?",choices = Sectors),
                                           material_radio_button("Bsc","Do you have a Bachelor's Degree?",choices = c("Yes","No")),
                                           br(),
                                           actionButton("run",div("PREDICT",icon("flask")),css.class = "z-depth-5")
                                           
                                             
                                           
                                           )),
                                           
                                                       material_column(width = 8,
                                                                      HTML('<div class="row">
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
                                                                         conditionalPanel(
                                                                           condition="!($('html').hasClass('shiny-busy'))",
                                                                           
                                                                         material_card(title="",
                                                                        textOutput("prefix_final"),
                                                                         div(imageOutput("image_final"), style="text-align: center;"),
                                                                         textOutput("prediction_final"),
                                                                         br(),
                                                                         tags$head(tags$style("#prefix_final{font-size: 40px; text-align:center; font-family:Papyrus;}")),
                                                                         tags$head(tags$style("#prediction_final{font-size: 40px; text-align:center;font-family:Papyrus;}")),
                                                                         div(uiOutput("urlInput"),style="text-align:center;"))),
                                                                      busyIndicators(text = h4("Running Model...",style="font-size: 40px; font-family:Papyrus;"),img = "Loading3.gif")
                                                                         
                                                           ))
                                         ),
                    material_tab_content("second_tab",
                                         
                                                       box(width = 12,title= h2(strong("The Art and Science of Data",img(src="Typing.gif",height=60)),style= "font-size: 36pt;text-align:center; font-family:Papyrus; color:#009688;"),
                                                           br(),
                                                           div("This application was humbly created by me (Rosebud Anwuri). It's purely for fun and in no way guarantees you'd become a billionaire (I'm sorry). If you like this and would like to see more of the stuff I work on, you can visit my blog here:",a("The Art and Science of Data.",style= "font-size: 16pt; display: inline;color:#009688;",href="http://theartandscienceofdata.wordpress.com",target="_blank"),"My blog is mostly focused on the application of Data Science to everyday life and culture to make it more accessible, less technical and much more fun to a wider audience. Feel free to add a comment or drop me a message for questions or suggestions. Thank you!",style= "font-size: 14pt; font-family:Segoe UI; color:#757575;")
                                                       ))
                    )

server <- function(input, output) {
  category_predictors = function(){
    self_made=ifelse(input$selfMade=="Yes","Y","N")
    year_born = year(Sys.Date())-input$Age
    age_at_start = ifelse(input$founding_year<year_born,0,input$founding_year-year(Sys.Date())+input$Age)
    bachelors_degree=ifelse(input$Bsc=="Yes","Y","N")
    Worth = round(runif(1,1,5),1)
    relationship = ifelse(input$selfMade==T,input$relation,input$buffer)
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
    category_predictors()
  })
  output$prediction_final = renderText({ 
    prediction_reactive()
    })
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
  output$prefix_final = renderText({
    prefix_reactive()
    })
  
  image_per_prediction = function(prediction){
    image = ifelse(prediction=="A Consistent Billionaire","www/The Consistent.PNG",
                   ifelse(prediction=="A Ghost Billionaire", "www/The Ghost.PNG",
                          "www/The Hustler.PNG"))
    return(image)
  }
  
  image_reactive= eventReactive(input$run, {
    image_per_prediction(prediction_reactive())
  })
  output$image_final = renderImage({ 
    list(src=image_reactive())},deleteFile = F)
  
  
  output$niceGIF = renderImage({list(src="www/Typing.gif")},deleteFile = F)
  
}
shinyApp(ui = ui, server = server)