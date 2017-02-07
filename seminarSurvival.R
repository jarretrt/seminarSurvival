library(shiny)
library(shinythemes)
library(lubridate)

attendance = 0
time.stamp = ymd_hms(Sys.time())
time = minute(time.stamp) + second(time.stamp)/60
start.time = paste0(hour(time.stamp),":",minute(time.stamp))

ui <- fluidPage(theme = shinytheme("sandstone"),
  h1("Seminar Survival!"),
  
  sidebarLayout(
    
    sidebarPanel(
      p("Start time:", start.time),
      tags$p(actionButton("increment", "Put me in coach!")),
      tags$p(actionButton("decrement", "Not all who wander are lost... But I sure am!"))
    ),
  
    mainPanel(
      # alternates: "Boring conversation anyway."
      # uiOutput("count"),
      tableOutput('tab'),
      plotOutput("timePlot")
    )
  )
)

server <- function(input, output) {
  
  # observe({input$increment
  observeEvent(input$increment, {
    
    attendance <<- c(attendance, tail(attendance,1)+1)
    
    time.stamp <<- ymd_hms(Sys.time())
    time <<- c(time, minute(time.stamp) + second(time.stamp)/60)
    output$timePlot <- renderPlot({
      plot(time, attendance, type = "s", xlim = c(min(time), max(time)+1))
      })
    
    max.participants <- max(attendance)
    current.participants <- tail(attendance,1)
    current.pct <- round((current.participants / max.participants)*100)
    first.dropout <- ifelse(sum(diff(attendance) < 1) >0 , time[min(which(diff(attendance) < 1))], 0)
    tab <- data.frame(rbind(max.participants,current.participants,current.pct,first.dropout), 
                      row.names = c("Maximum N","Current N", "Current percent", "Time of first dropout"))
    colnames(tab) <- " "
    output$tab <- renderTable(tab)
  })
  
  # observe({input$decrement
  observeEvent(input$decrement, {
    
    attendance <<- c(attendance, tail(attendance,1)-1)
    
    output$count <- renderText({
      paste0("i = ", tail(attendance,1))
    })
    
    time.stamp <<- ymd_hms(Sys.time())
    time <<- c(time, minute(time.stamp) + second(time.stamp)/60)
    
    output$timePlot <- renderPlot({
      plot(time, attendance, type = "s", xlim = c(min(time), max(time)+1))
    })
    
    max.participants <- max(attendance)
    current.participants <- tail(attendance,1)
    current.pct <- round((current.participants / max.participants)*100)
    first.dropout <- ifelse(sum(diff(attendance) < 1) >0 , time[min(which(diff(attendance) < 1))], 0)
    tab <- data.frame(rbind(max.participants,current.participants,current.pct,first.dropout), 
                      row.names = c("Maximum N","Current N", "Current percent", "Time of first dropout"))
    colnames(tab) <- " "
    output$tab <- renderTable(tab)
  })
  
}

runApp(list(ui = ui, server = server))
