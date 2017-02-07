library(shiny)
library(shinythemes)
library(lubridate)

#add session_id
session_id <- as.numeric(Sys.time())
#make data upload to dropbox
token <- readRDS("./ss-drop.rds")

attendance = 0
time.stamp = ymd_hms(Sys.time())
time = minute(time.stamp) + second(time.stamp)/60
start.time = paste0(hour(time.stamp),":",minute(time.stamp))
tab.full = NULL
shinyServer(
  function(input, output, session) {
    rv <- reactiveValues(
      tab.full = data.frame(session_id   = session_id,
                            time   = time,
                            awake    = 0)
    )
    observeEvent(input$increment, {
      attendance <<- c(attendance, tail(attendance,1)+1)
      
      time.stamp <<- ymd_hms(Sys.time())
      time <<- c(time, minute(time.stamp) + second(time.stamp)/60)
      
      in.time <- (minute(time.stamp) + second(time.stamp)/60)
      max.participants <- max(attendance)
      current.participants <- tail(attendance,1)
      current.pct <- round((current.participants / max.participants)*100)
      first.dropout <- ifelse(sum(diff(attendance) < 1) >0 , time[min(which(diff(attendance) < 0),na.rm=TRUE)], 0)
      tab <- data.frame(session_id = session_id, time = in.time, awake = 1)
      rv$tab.full <<- rbind(rv$tab.full, tab)
      output$tab <- renderTable(rv$tab.full[,-1])
    })
    

    observeEvent(input$decrement, {
      attendance <<- c(attendance, tail(attendance,1)-1)
      
      output$count <- renderText({
        paste0("i = ", tail(attendance,1))
      })
      
      time.stamp <<- ymd_hms(Sys.time())
      time <<- c(time, minute(time.stamp) + second(time.stamp)/60)
      
      out.time <- (minute(time.stamp) + second(time.stamp)/60)
      
      max.participants <- max(attendance)
      current.participants <- tail(attendance,1)
      current.pct <- round((current.participants / max.participants)*100)
      first.dropout <- ifelse(sum(diff(attendance) < 1) >0 , time[min(which(diff(attendance) < 0))], 0)
      tab <- data.frame(session_id = session_id, time = out.time, awake = 0)
      rv$tab.full <<- rbind(rv$tab.full, tab)
      output$tab <- renderTable(rv$tab.full[,-1])
    })
    
     output$timePlot <- renderPlot({
      invalidateLater(2000)
      attendance <<- c(attendance, tail(attendance,1))
      time.stamp <<- ymd_hms(Sys.time())
      time <<- c(time, minute(time.stamp) + second(time.stamp)/60)
      plot(time, attendance, type = "s", xlim = c(min(time), max(time)+1))
      
    })
    
     #output data
     file_path <- file.path(tempdir(), paste0(round(session_id),".csv"))
     readr::write_csv(isolate(rv$tab.full),file_path)    
     #upload to dropbox 
     rdrop2::drop_upload(file_path, "shiny/2016/seminarSurvival/", dtoken = token)
    
    
  }
)