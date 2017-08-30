# ui.R
library(shiny)
library(shinythemes)
library(lubridate)

attendance = 0
time.stamp = ymd_hms(Sys.time())
time = hour(time.stamp) + minute(time.stamp)/60 + second(time.stamp)/3600
start.time = paste0(hour(time.stamp),":",minute(time.stamp))

shinyUI(
  navbarPage(title = "Seminar Survival", theme = shinytheme("slate"),
    tabPanel("Track Survival",
    # shinythemes::themeSelector(),
  
              sidebarPanel(
                p("Start time:", start.time),
                textInput("user_id", "User ID", "Enter a unique ID"),
                tags$p(actionButton("increment", "Put me in coach!")),
                tags$p(actionButton("decrement", "Not all who wander are lost... But I sure am!")),
                downloadButton('downloadData', 'Download'),
                hr(),
                h5("Instructions:"),
                p("1. Enter a unique user name"),
                p("2. When you begin understanding the seminar, press 'Put me in coach!'"),
                p("3. When you get lost, press 'Not all who wander are lost'"),
                p("4. If you begin understanding again, jump back in...Repeat!"),
                hr(),
                h5("Note:"),
                p("The complete data are available for ANYONE to download. If you don't want others to see 
                  your personal logins and logouts then leave the User ID blank or use a pseudonym."),
                p("Comments and questions should be confined to those that help the listener understand the presentation (e.g. 'What 
                  does the x-axis of this plot represent?'), or notes for the presenter's to help them improve in the future (e.g. 
                  `The material on slide 18 was confusing.')"),
                p("It is possible to click `Put me in' or `I'm lost' multiple times in a row. Please respect the integrity of the 
                  data and don't.")
              ),

              mainPanel(
                # alternates: "Boring conversation anyway."
                wellPanel(h5("Summary Statistics"), tableOutput('tab')),
                plotOutput("timePlot"),
                verbatimTextOutput("value"),
                tableOutput('tab.full'),
                
                hr(),
                
                wellPanel(
                fluidRow(
                  column(10,
                    textInput("txt", "Enter questions or comments below", width = "100%")
                  ),
                  column(1, br(), tags$p(actionButton("submit_comment", "Submit")))
                )),
                
                # verbatimTextOutput("comment_board", placeholder = T),
                wellPanel(verbatimTextOutput("comment_board", placeholder = T), style = "overflow-y:scroll; max-height: 300px"),
                
                hr(),
                plotOutput("kmPlot")
                
                )
    ),
  tabPanel("About",
           includeHTML("./about.html")
           )
  )
)
