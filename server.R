library(shiny)
library(shinythemes)
library(lubridate)
library(survival)

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
      
      personal.tab <- data.frame(session_id = session_id, 
                                 surv_id = isolate(rv$surv_id), 
                                 comb_id = paste0(session_id, isolate(rv$surv_id)), 
                                 time = in.time, 
                                 awake = 1)
      
      rv$tab.full <<- rbind(rv$tab.full, personal.tab)
      tab.everyone <<- rbind(tab.everyone, personal.tab)
      # tab.everyone <<- rv$tab.everyone
      
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
      if(!(length(which(diff(attendance)<0))==0)){
        first.dropout <- ifelse(sum(diff(attendance) < 1) >0 , time[min(which(diff(attendance) < 0))], 0)
      } else first.dropout <- NA  
      
      personal.tab <- data.frame(session_id = session_id, 
                                 surv_id = isolate(rv$surv_id), 
                                 comb_id = paste0(session_id, isolate(rv$surv_id)), 
                                 time = out.time, 
                                 awake = 0)
      
      rv$tab.full <<- rbind(rv$tab.full, personal.tab)
      tab.everyone <<- rbind(tab.everyone, personal.tab)
      # tab.everyone <<- rv$tab.everyone
      
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
      plot(time, attendance, type = "s", xlim = c(min(time), max(time)+1/120))
      max.participants <- max(attendance)
      current.participants <- tail(attendance,1)
      current.pct <- round((current.participants / max.participants)*100)
      if(!(length(which(diff(attendance)<0))==0)){
        first.dropout <- ifelse(sum(diff(attendance) < 1) >0 , time[min(which(diff(attendance) < 0))], 0)
      } else first.dropout <- NA  
      
      tab <- data.frame(rbind(max.participants,current.participants,current.pct,first.dropout), 
                        row.names = c("Maximum N","Current N", "Current percent", "Time of first dropout"))
      colnames(tab) <- " "
      tab <<- tab
      output$tab <- renderTable(tab)
      
    })
  
    output$downloadData <- downloadHandler(
      filename = function() { 
        paste0("seminarInfo_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(tab.everyone[!duplicated(tab.everyone[,c("comb_id", "surv_id", "awake")]),], file, row.names = F)
      }
    )
    
    observeEvent({
      input$decrement
      input$increment
      },{
      output$kmPlot <- renderPlot({
        if(sum(tab.everyone$awake == 0) > 0){
          censor.time.stamp <- ymd_hms(Sys.time())
          censor.time <- hour(censor.time.stamp) + minute(censor.time.stamp)/60 + second(censor.time.stamp)/3600
          tab.everyone.sub = tab.everyone[!duplicated(tab.everyone[,c("comb_id", "surv_id", "awake")]),]
          tab.everyone.sub$comb_id <- paste0(tab.everyone.sub$session_id, tab.everyone.sub$surv_id)
          tab.everyone.sub <- reshape(tab.everyone.sub, timevar = "awake", idvar = "comb_id", direction = "wide", v.names = c("time"))
          tab.everyone.sub$event <- 1*!is.na(tab.everyone.sub$time.0)
          tab.everyone.sub[is.na(tab.everyone.sub$time.0), "time.0"] <- censor.time
          tab.everyone.sub$time.dif = (tab.everyone.sub$time.0 - tab.everyone.sub$time.1)*60 # convert back to minutes
          
          KM <- survfit(Surv(time.dif, event) ~ 1,  type="kaplan-meier", conf.type="log", data=tab.everyone.sub)
          plot(KM, main = "Kaplan-Meier Survival Plot", xlab = "Minutes", ylab = "Survial") 
        }
      })
      })
    
    
    # tab.everyone <- read.csv("~/Desktop/seminarInfo_2017-02-07.csv")
    
    
  }
)