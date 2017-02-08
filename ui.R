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
                tags$p(actionButton("increment", "Put me in coach!")),
                tags$p(actionButton("decrement", "Not all who wander are lost... But I sure am!")),
                downloadButton('downloadData', 'Download')
              ),
              
              mainPanel(
                # alternates: "Boring conversation anyway."
                # uiOutput("count"),
                tableOutput('tab'),
                plotOutput("timePlot"),
                plotOutput("kmPlot"),
                tableOutput('tab.full')
                )
            )
  )
  
)