library(shiny)
library(shinythemes)
library(lubridate)

attendance = 0
time.stamp = ymd_hms(Sys.time())
time = minute(time.stamp) + second(time.stamp)/60
start.time = paste0(hour(time.stamp),":",minute(time.stamp))

shinyServer(
  function(input, output, session) {
    
    observeEvent(input$increment, {
      attendance <<- c(attendance, tail(attendance,1)+1)
      
      time.stamp <<- ymd_hms(Sys.time())
      time <<- c(time, minute(time.stamp) + second(time.stamp)/60)
      
      max.participants <- max(attendance)
      current.participants <- tail(attendance,1)
      current.pct <- round((current.participants / max.participants)*100)
      first.dropout <- ifelse(sum(diff(attendance) < 1) >0 , time[min(which(diff(attendance) < 0))], 0)
      tab <- data.frame(rbind(max.participants,current.participants,current.pct,first.dropout), 
                        row.names = c("Maximum N","Current N", "Current percent", "Time of first dropout"))
      colnames(tab) <- " "
      tab <<- tab
      output$tab <- renderTable(tab)
    })
    
    observeEvent(input$decrement, {
      attendance <<- c(attendance, tail(attendance,1)-1)
      
      output$count <- renderText({
        paste0("i = ", tail(attendance,1))
      })
      
      time.stamp <<- ymd_hms(Sys.time())
      time <<- c(time, minute(time.stamp) + second(time.stamp)/60)
      
      # output$timePlot <- renderPlot({
      #   invalidateLater(2000, session)
      #   plot(time, attendance, type = "s", xlim = c(min(time), max(time)+1))
      # })
      
      max.participants <- max(attendance)
      current.participants <- tail(attendance,1)
      current.pct <- round((current.participants / max.participants)*100)
      first.dropout <- ifelse(sum(diff(attendance) < 1) >0 , time[min(which(diff(attendance) < 0))], 0)
      tab <- data.frame(rbind(max.participants,current.participants,current.pct,first.dropout), 
                        row.names = c("Maximum N","Current N", "Current percent", "Time of first dropout"))
      colnames(tab) <- " "
      tab <<- tab
      output$tab <- renderTable(tab)
    })
    
  
    output$timePlot <- renderPlot({
      invalidateLater(2000)
      attendance <<- c(attendance, tail(attendance,1))
      time.stamp <<- ymd_hms(Sys.time())
      time <<- c(time, minute(time.stamp) + second(time.stamp)/60)
      plot(time, attendance, type = "s", xlim = c(min(time), max(time)+1))
      
    })
    

    
    
  }
)