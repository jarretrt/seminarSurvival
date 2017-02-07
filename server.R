library(shiny)
library(shinythemes)
library(lubridate)

attendance = 0
time.stamp = ymd_hms(Sys.time())
time = hour(time.stamp) + minute(time.stamp)/60 + second(time.stamp)/3600
start.time = paste0(hour(time.stamp),":",minute(time.stamp))

tab.everyone = data.frame(session_id   = NULL,
                      surv_id = NULL,
                      comb_id = NULL,
                      time   = NULL,
                      awake    = NULL)

shinyServer(
  function(input, output, session) {
    #add session_id
    session_id <- as.numeric(Sys.time())
    rv <- reactiveValues(
      surv_id = 0,
      tab.full = data.frame(session_id   = session_id,
                            surv_id = 0,
                            comb_id = paste0(session_id, 0),
                            time   = time,
                            awake    = 0)
    )
    observeEvent(input$increment, {
      
      rv$surv_id <<- rv$surv_id + 1
      attendance <<- c(attendance, tail(attendance,1)+1)
      
      time.stamp <<- ymd_hms(Sys.time())
      time <<- c(time, hour(time.stamp) + minute(time.stamp)/60 + second(time.stamp)/3600)
      
      in.time <- hour(time.stamp) + minute(time.stamp)/60 + second(time.stamp)/3600
      
      max.participants <- max(attendance)
      current.participants <- tail(attendance,1)
      current.pct <- round((current.participants / max.participants)*100)
      first.dropout <- ifelse(sum(diff(attendance) < 1) >0 , time[min(which(diff(attendance) < 0))], 0)
      
      personal.tab <- data.frame(session_id = session_id, surv_id = isolate(rv$surv_id), comb_id = paste0(session_id, isolate(rv$surv_id)), time = in.time, awake = 1)
      rv$tab.full <<- rbind(rv$tab.full, personal.tab)
      tab.everyone <<- rbind(tab.everyone, rv$tab.full)
      
      tab <- data.frame(rbind(max.participants,current.participants,current.pct,first.dropout),
                        row.names = c("Maximum N","Current N", "Current percent", "Time of first dropout"))
      colnames(tab) <- " "
      tab <<- tab
      output$tab <- renderTable(tab)
      output$tab.full <- renderTable(rv$tab.full[min(which(rv$tab.full$surv_id ==1)):nrow(rv$tab.full),])
      
    })
    
    observeEvent(input$decrement, {
      attendance <<- c(attendance, tail(attendance,1)-1)
      
      output$count <- renderText({
        paste0("i = ", tail(attendance,1))
      })
      
      time.stamp <<- ymd_hms(Sys.time())
      
      time <<- c(time, hour(time.stamp) + minute(time.stamp)/60 + second(time.stamp)/3600)
      
      out.time <- (hour(time.stamp) + minute(time.stamp)/60 + second(time.stamp)/3600)
      
      max.participants <- max(attendance)
      current.participants <- tail(attendance,1)
      current.pct <- round((current.participants / max.participants)*100)
      first.dropout <- ifelse(sum(diff(attendance) < 1) >0 , time[min(which(diff(attendance) < 0))], 0)
      
      personal.tab <- data.frame(session_id = session_id, surv_id = isolate(rv$surv_id), comb_id = paste0(session_id, isolate(rv$surv_id)), time = out.time, awake = 0)
      rv$tab.full <<- rbind(rv$tab.full, personal.tab)
      tab.everyone <<- rbind(tab.everyone, rv$tab.full)
      
      tab <- data.frame(rbind(max.participants,current.participants,current.pct,first.dropout),
                        row.names = c("Maximum N","Current N", "Current percent", "Time of first dropout"))
      colnames(tab) <- " "
      tab <<- tab
      output$tab <- renderTable(tab)
      output$tab.full <- renderTable(rv$tab.full[min(which(rv$tab.full$surv_id ==1)):nrow(rv$tab.full),])
    })
    
  
    output$timePlot <- renderPlot({
      invalidateLater(2000)
      attendance <<- c(attendance, tail(attendance,1))
      time.stamp <<- ymd_hms(Sys.time())
      time <<- c(time, hour(time.stamp) + minute(time.stamp)/60 + second(time.stamp)/3600)
      plot(time, attendance, type = "s", xlim = c(min(time), max(time)+1/60))
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
  
    output$downloadData <- downloadHandler(
      filename = function() { 
        # paste(input$dataset, '.csv', sep='')
        paste0("seminarInfo_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(tab.everyone, file)
      }
    )
    
  }
)