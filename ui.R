# ui.R
library(shiny)

library(shinythemes)
library(lubridate)

attendance = 0
time.stamp = ymd_hms(Sys.time())
time = minute(time.stamp) + second(time.stamp)/60
start.time = paste0(hour(time.stamp),":",minute(time.stamp))

shinyUI(
  
  fluidPage(theme = shinytheme("spacelab"),
            # h1("Seminar Survival!"),
            
            sidebarLayout(
              
              sidebarPanel(
                p("Start time:", start.time),
                textInput("user_id", "User ID", "Enter a unique ID"),
                tags$p(actionButton("increment", "Put me in coach!")),
                tags$p(actionButton("decrement", "Not all who wander are lost... But I sure am!")),
                downloadButton('downloadData', 'Download'),
                h5("Instructions:"),
                p("1. Enter a unique user name"),
                p("2. When you begin understanding the seminar, press 'Put me in coach!'"),
                p("3. When you get lost, press 'Not all who wander are lost'"),
                p("4. If you begin understanding again, jump back in...Repeat!")
              ),
              
              mainPanel(
                # alternates: "Boring conversation anyway."
                # uiOutput("count"),
                # textAreaInput("caption", "Caption", "", width = "500px"),
                tableOutput('tab'),
                plotOutput("timePlot"),
                verbatimTextOutput("value"),
                tableOutput('tab.full'),
                plotOutput("kmPlot")
                )
            )
  )
  
)