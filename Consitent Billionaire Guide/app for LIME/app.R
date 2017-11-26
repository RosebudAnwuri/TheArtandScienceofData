library(h2o)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(shinythemes)
library(plyr)
library(dplyr)
library(scales)
library(lubridate)
library(shinysky)
library(stringr)
library(lime)
library(ggplot2)

h2o.init()
dat = read.csv("billionaire_data_for_ml.csv")
model = h2o.loadModel("GBM_Model")

explainer <- lime::lime(
  dat, 
  model          = model, 
  bin_continuous = T)

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
ui = shinyUI(
  navbarPage("Billion Dollar Questions",inverse = F,collapsible = T,fluid = T,
             theme=shinytheme("flatly"),
             tabPanel("What Type of Billionaire Are You?", icon = icon("money"),
                      sidebarLayout(position = 'left',
                                    
                                    sidebarPanel(id = "sidebar",
                                                 selectizeInput("Country",label = h4("What country are you from?",style ="font-size: 10pt;font-family: 'Open Sans';text-transform: uppercase;"),choices=Countries),
                                                 numericInput('Age',h4("How Old Are You?",style ="font-size: 10pt;font-family: 'Open Sans';text-transform: uppercase;"),value = 20,min = 12,max = 100),
                                                 selectizeInput("selfMade",h4("Do you have your own company (or plan to have one)?",style ="font-size: 10pt;font-family: 'Open Sans';text-transform: uppercase;"),choices = c("Yes","No")),
                                                 conditionalPanel("input.selfMade=='Yes'",selectizeInput("relation",h4("Choose one below that best describes your role in the business:",style ="font-size: 10pt;font-family: 'Open Sans';text-transform: uppercase;"),choices = Relations)),
                                                 conditionalPanel("input.selfMade=='No'",selectizeInput("buffer",h4("That's okay. Think of a business where you are likely to make money from. What best describes your role in that company?",style ="font-size: 10pt;font-family: 'Open Sans';text-transform: uppercase;"),choices = Relations)),
                                                 numericInput("founding_year",h4("When was/will this business (be) established?",style ="font-size: 10pt;font-family: 'Open Sans';text-transform: uppercase;"),value = 1999,min=1600,max=year(Sys.Date())+10),
                                                 selectizeInput("Sector",h4("What Sector is this business?",style ="font-size: 10pt;font-family: 'Open Sans';text-transform: uppercase;"),choices = Sectors),
                                                 selectizeInput("Bsc",h4("Do you have a Bachelor's Degree?",style ="font-size: 10pt;font-family: 'Open Sans';text-transform: uppercase;"),choices = c("Yes","No")),
                                                 selectizeInput("MBA",h4("Do you have an MBA?",style ="font-size: 10pt;font-family: 'Open Sans';text-transform: uppercase;"),choices = c("Yes","No")),
                                                 
                                                 tags$head(
                                                   tags$style(HTML('#run{border-radius: 0px;}'))
                                                 ),
                                                 div(conditionalPanel("!$('html').hasClass('shiny-busy')",actionButton("run",div("PREDICT",icon("flask"),style="text-align:center;font-size:10pt;width: 150px;"),styleclass = "success",size = "mini",css.class = "animated infinite rubberBand")),style="text-align:center;")
                                                 
                                                 
                                    ),
                                    mainPanel(position="left",
                                              
                                              tags$head(
                                                tags$style(HTML("
                                                                @import url('https://fonts.googleapis.com/css?family=Open+Sans:300,400');
                                                                
                                                                .navbar-default {
                                                                background-color: teal;
                                                                border-color: #E7E7E7;
                                                                }
                                                                .navbar-default .navbar-nav > li > a {
                                                                color: white;
                                                                }
                                                                .navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:hover, .navbar-default .navbar-nav > .active > a:focus {
                                                                background-color: #00897b;
                                                                color: white;
                                                                }
                                                                body > nav > div{
                                                                background-color: #009688;
                                                                }
                                                                body{
                                                                font-family:'Open Sans', sans-serif;
                                                                }
                                                        .selectize-input {border-radius:0px;
                                                        background-color: rgba(236, 240, 241,0) !important;
                                                            border-width:1px;
                                                                
                                                                border-top: none;
                                                                border-right: none;
                                                                border-left: none;
                                                                border-bottom-color: #546e7a;
                                                                border-radius: 0;}
                                                        .selectize-input.dropdown-active {border-radius:0px;
                                                                background-color: rgba(236, 240, 241,0);
                                                                border-width:1px;
                                                                border-top: none;
                                                                border-right: none;
                                                                border-left: none;
                                                                border-bottom-color: #546e7a;
                                                                border-radius: 0}
                                                    .selectize-dropdown {border-radius:0px;
                                                                   background-color: rgba(236, 240, 241,0);
                                                                   border-width:1px;
                                                                   border-top: none;
                                                                   border-right: none;
                                                                   border-left: none;
                                                                   border-bottom-color: #546e7a;
                                                                   border-radius: 0}           
                                                                
                                                                "))), tags$head(
                                                                  tags$link(rel = "stylesheet", type = "text/css", href = "animate.min.css")
                                                                ),                               
                                             bsModal( id="startupMessage",trigger='',size='large',HTML(
                                                '
                                                
                                                <div class="row animated wobble" style="padding-right:5%;padding-left:5%;">
                                                <div class="col s12 m12">
                                                <div style="background:#009688; color:white;font-size:12pt;font-weight:200;box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);">
                                                <div class="card-content white-text" style="padding:10px;">
                                                
                                                
                                                <p>Have you ever wondered what sort of billionaire you would end up as? A Consistent one, A Hustler or a Ghost?
                                                Learn a bit more about it on the link below and predict the one you could be with this simple app!</p>
                                                </div>
                                                <div style="background-color:#ffffff;">
                                                <div class="card-action">
                                                <a style="color:#00897b ; text-transform:uppercase;font-weight:400;font-size:3.7vh" href="http://theartandscienceofdata.wordpress.com/blog" target"_blank">The Art and Science of Data</a>
                                               
                                                </div>

                                                </div>
                                                
                                                </div>
                                                </div>
                                                </div>
<div class="animated wobble" style="text-align:center;"><button type="button"id="close-button" class="btn btn-default animated swing infinite" data-dismiss="modal" style="
    text-align: center; margin-top:20px;;width:150px;height:55px;border:none;border-radius:0;background-color:#00897b;
                                                ">GET STARTED!</button></div>')),HTML('<style>
.card-action {
                                                               box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
                                                               transition: all 0.3s cubic-bezier(.25,.8,.25,1);
                                                               height:50px;
                                                               text-align:center;
                                                               }
                                                               .card-action:hover {
                                                               box-shadow: 0 14px 28px rgba(0,0,0,0.25), 0 10px 10px rgba(0,0,0,0.22);
                                                               }
                                                               @media only screen and (max-width : 500px) {
                                                               img{
                                                               width:100%;
                                                               min-width:100%;
                                                               }
                                                               }

                                                               #run{
                                                               animation-duration:2s;
                                                               -moz-animation-duration: 2s;
                                                               -webkit-animation-duration: 2s;
                                                               -o-animation-duration:.2s;
                                                               -ms-animation-duration:.2s;
                                                               }
                                                               #urlInput > a{
                                                               animation-duration:2s;
                                                               -moz-animation-duration: 2s;
                                                               -webkit-animation-duration: 2s;
                                                               -o-animation-duration:.2s;
                                                               -ms-animation-duration:.2s;
                                                               }
#close-button{
                                                               animation-duration:2s;
                                                                                      -moz-animation-duration: 2s;
                                                                                      -webkit-animation-duration: 2s;
                                                                                      -o-animation-duration:.2s;
                                                                                      -ms-animation-duration:.2s;
                                                                                      }
.modal-content{
background: none;
    border: none;
                                                               box-shadow: none;
}
.modal-header{
border:none;
}
.modal .close{
color:white;
}
.modal-body{
display:none;
}
.modal-footer{
display:none;
}

                                                               </style>
                                                               
                                                               '),
                                              
                                              
                                              busyIndicators(text = h4("Running Model...",style="font-size: 40px; font-family:Papyrus;"),img = "Loading2.gif"),
                                              
                                             conditionalPanel("!$('html').hasClass('shiny-busy')",textOutput("prefix_final"),
                                              div(imageOutput("image_final"), style="text-align: center;"),
                                              textOutput("prediction_final")),
                                              br(),
                                              tags$head(tags$style("#prefix_final{
                                                                    font-size: 6vh; text-align:center;
                                                                    }
                                                                   #prediction_final{
                                                                   font-size: 6vh; text-align:center;
                                                                   }
                                                                   #sidebar{background-color: rgba(236, 240, 241,0.4);}
                                                                  form-group label{}
                                                                   .form-group{color: #90a4ae ;text-transform:uppercase;font-size:10pt;}
                                                                   #Age,#founding_year{background-color: rgba(236, 240, 241,0);
                                                                  border-width:1px;
  
                                                                   border-top: none;
                                                                   border-right: none;
                                                                   border-left: none;
                                                                   border-bottom-color: #546e7a;
                                                                   border-radius: 0}")),
                                              
                                             conditionalPanel("typeof output.image_final !== 'undefined' &&!$('html').hasClass('shiny-busy')", div(uiOutput("urlInput"),style="text-align:center;")),
                                             br(),
                                             
                                             conditionalPanel("!$('html').hasClass('shiny-busy')",
                                             plotOutput("limePlot"))
                                              
                                              ))),
             tabPanel("About", icon = icon("info"),
                      
                      box(width = 12,title= h2(strong("The Art and Science of Data",img(src="Typing.gif",height=60)),style= "font-size: 36pt;text-align:center; font-family:Papyrus; color:#009688;"),
                          br(),
                          div("This application was humbly created by me (Rosebud Anwuri). It's purely for fun and in no way guarantees you'd become a billionaire (I'm sorry). If you like this and would like to see more of the stuff I work on, you can visit my blog here:",a("The Art and Science of Data.",style= "font-size: 16pt; display: inline;color:#009688;",href="http://theartandscienceofdata.wordpress.com",target="_blank"),"My blog is mostly focused on the application of Data Science and aims to portray just how relevant Data Science can be in our everyday lives and how much fun it is! Feel free to add a comment or drop me a message for questions or suggestions. Thank you!",style= "font-size: 14pt; font-family:Helvetica;")
                      ))
             ))






server = shinyServer(function(input, output, session){
  toggleModal(session, "startupMessage", toggle = "open")
  
  feature_processing=function(){
    self_made=ifelse(input$selfMade=="Yes","Y","N")
    year_born = year(Sys.Date())-input$Age
    age_at_start = input$founding_year-year(Sys.Date())+input$Age
    bachelors_degree=ifelse(input$Bsc=="Yes","Y","N")
    MBA=ifelse(input$MBA=="Yes","Y","N")
    relationship = ifelse(input$selfMade=="Yes",input$relation,input$buffer)
    data_list = list(input$Country,self_made,bachelors_degree,MBA,input$Sector,
                     input$founding_year,relationship,age_at_start)
    names(data_list)=model@parameters$x
    return(data_list)
  }
  category_predictors = function(){
    
    df=feature_processing()
    data_list=as.h2o(df)
    prediction = predict(model,data_list)$predict
    prediction = as.vector(prediction)
    prediction=sub("The","A",paste(prediction,"Billionaire"))
    
    return(prediction)
  }
  lime_explainer=function(){
    df=feature_processing()
    df_lime = as.data.frame(df)
    explanation <- lime::explain(
      df_lime, 
      explainer    = explainer, 
      n_labels     = 1, 
      n_features   = 8,
      kernel_width = 0.5)
    
    return(plot_features(explanation)+
             labs(title="Feature Importance Visualisation",
           subtitle="Example: If Sector is Retail, the bar is green and the prediction is Ghost:\nIt simply means that Retail is one factor that supports you being classfied as a ghost.")+
             theme(text=element_text(size = 16)))
  }
  lime_plot=eventReactive(input$run, {
    lime_explainer()
  })
  output$limePlot = renderPlot({lime_plot()})
  prediction_reactive= eventReactive(input$run, {
    paste(category_predictors())
  })
  output$prediction_final = renderText({prediction_reactive()})
  output$urlInput = renderUI({
    a(p(icon("twitter"),"SHARE ON TWITTER",style="padding-top:5%"),type="button",class = "waves-effect waves-light btn shiny-material-button z-depth-5 animated infinite pulse" , 
      style = "fontweight:600; background-color: #4099FF; color: #FFFFFF; border-radius: 0;border-width: 0;padding:5px 10px", target = "_blank",href=paste0(sprintf('https://twitter.com/intent/tweet?text=I+am+going+to+be+%s', 
                                                                                                                  str_replace_all(prediction_reactive()," ","+")),"!+Check+yours+out+at:&url=https://goo.gl/jFsjbD") )
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