library(shiny)
library(shinythemes)
library(lubridate)
library(survival)
library(plyr)

attendance = 0
time.stamp = ymd_hms(Sys.time())
time = hour(time.stamp) + minute(time.stamp)/60 + second(time.stamp)/3600
start.time = paste0(hour(time.stamp),":",minute(time.stamp))

comment_board <- NULL

tab.everyone = data.frame(session_id   = NULL,
                          surv_id = NULL,
                          comb_id = NULL,
                          user_id = NULL,
                          time   = NULL,
                          awake    = NULL)

shinyServer(
  function(input, output, session) {
    #add session_id
    session_id <- as.numeric(Sys.time()) # gives a unique id associated with log in time
    rv <- reactiveValues(
      surv_id = 0,
      tab.full = data.frame(session_id   = session_id,
                            surv_id = 0,
                            comb_id = paste0(session_id, 0),
                            user_id = 0,
                            time   = time,
                            awake    = 0)
    )
    
    observeEvent(input$increment, {
      # if the user has already logged in, then give them their last survival id and increment by one
      # if(input$user_id %in% tab.everyone$user_id) rv$surv_id <<- tail(tab.everyone[tab.everyone$user_id == input$user_id, "surv_id"],1)
      rv$surv_id <<- rv$surv_id + 1
      attendance <<- c(attendance, tail(attendance,1)+1)

      time.stamp <<- ymd_hms(Sys.time())
      time <<- c(time, hour(time.stamp) + minute(time.stamp)/60 + second(time.stamp)/3600)

      in.time <- hour(time.stamp) + minute(time.stamp)/60 + second(time.stamp)/3600

      max.participants <- max(attendance)
      current.participants <- tail(attendance,1)
      current.pct <- round((current.participants / max.participants)*100)
      
      if(!(length(which(diff(attendance)<0))==0)){
        first.dropout <- ifelse(sum(diff(attendance) < 1) >0 , time[min(which(diff(attendance) < 0))], 0)
      } else first.dropout <- NA
      
      personal.tab <- data.frame(session_id = input$user_id,
                                 # session_id = session_id,
                                 surv_id = isolate(rv$surv_id),
                                 comb_id = paste0(session_id, isolate(rv$surv_id)),
                                 user_id = input$user_id,
                                 time = in.time,
                                 awake = 1)
      if(nrow(tab.everyone[grep(input$user_id, tab.everyone$user_id),]) != 0){
        personal.tab <- rbind(tab.everyone[grep(input$user_id, tab.everyone$user_id),], personal.tab)
      }  
      
      rv$tab.full  <<- unique(rbind(rv$tab.full, personal.tab))
      tab.everyone <<- unique(rbind(tab.everyone, personal.tab))

      tab <- data.frame(rbind(max.participants,current.participants,current.pct,first.dropout),
                        row.names = c("Maximum N","Current N", "Current percent", "Time of first dropout"))
      colnames(tab) <- " "
      tab <<- tab
      output$tab <- renderTable(tab, rownames = T)
      output$tab.full <- renderTable(rv$tab.full[min(which(rv$tab.full$surv_id ==1)):nrow(rv$tab.full),4:6])
    })

    observeEvent(input$decrement, {
      
      if(input$user_id %in% tab.everyone$user_id) rv$surv_id <<- tail(tab.everyone[tab.everyone$user_id == input$user_id, "surv_id"],1)
      
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


      personal.tab <- data.frame(session_id = input$user_id,
                               # session_id = session_id,
                                 surv_id = isolate(rv$surv_id),
                                 comb_id = paste0(session_id, isolate(rv$surv_id)),
                                 user_id =input$user_id,
                                 time = out.time,
                                 awake = 0)
      if(nrow(tab.everyone[grep(input$user_id, tab.everyone$user_id),]) != 0){
        personal.tab <- rbind(tab.everyone[grep(input$user_id, tab.everyone$user_id),], personal.tab)
      }

      rv$tab.full <<- unique(rbind(rv$tab.full, personal.tab))
      tab.everyone <<- unique(rbind(tab.everyone, personal.tab))

      tab <- data.frame(rbind(max.participants,current.participants,current.pct,first.dropout),
                        row.names = c("Maximum N","Current N", "Current percent", "Time of first dropout"))
      colnames(tab) <- " "
      tab <<- tab
      output$tab <- renderTable(tab, rownames = T)
      output$tab.full <- renderTable(rv$tab.full[min(which(rv$tab.full$surv_id ==1)):nrow(rv$tab.full),4:6])
    })


    output$timePlot <- renderPlot({
      invalidateLater(2000)
      attendance <<- c(attendance, tail(attendance,1))
      time.stamp <<- ymd_hms(Sys.time())
      time <<- c(time, hour(time.stamp) + minute(time.stamp)/60 + second(time.stamp)/3600)
      plot(time, attendance, type = "s", xlim = c(min(time), max(time)+1/120), xlab = "hour (24)", ylab = "cognitive attendance")
      max.participants <- max(attendance)
      current.participants <- tail(attendance,1)
      current.pct <- round((current.participants / max.participants)*100)
      if(!(length(which(diff(attendance)<0))==0)){
        first.dropout <- ifelse(sum(diff(attendance) < 1) >0 , time[min(which(diff(attendance) < 0))], 0)
      } else first.dropout <- NA

      tab <- data.frame(rbind(max.participants,current.participants,current.pct,first.dropout),
                        row.names = c("Maximum Attendance","Current Attendance", "Current percent", "Time of first dropout"))
      colnames(tab) <- " "
      tab <<- tab
      output$tab <- renderTable(tab, rownames = T)

    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("seminarInfo_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(tab.everyone[!duplicated(tab.everyone[,c("comb_id", "surv_id", "awake")]),], file, row.names = F)
        tab.everyone <-tab.everyone[!duplicated(tab.everyone[,c(1,2,3,5)]),]
        # tab.everyone$session_id <- c(as.character(tab.everyone$session_id), comment_board)
        tab.everyone$session_id <- as.character(tab.everyone$session_id)
        tab.everyone <- tab.everyone[,-which(names(tab.everyone) == "X")]
        # tab.everyone <- rbind.fill(tab.everyone, data.frame(session_id = comment_board))
        # tab.everyone <- rbind(tab.everyone, c(comment_board, rep(NA, times = 5)))
        write.csv(tab.everyone, file, row.names = F)
      }
    )

    observeEvent({
      input$decrement
      input$increment
    },{
      output$kmPlot <- renderPlot({
        invalidateLater(30000)
        if(sum(tab.everyone$awake == 0) > 0){
          
          # if(sum(tab.everyone.sub[tab.everyone.sub$comb_id %in% names(table(tab.everyone.sub$comb_id))[table(tab.everyone.sub$comb_id) > 1],"awake"] == 0)>0){
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
    
    
    comment_count <- reactiveValues(count = 0)
    observeEvent(input$submit_comment, comment_count$count <- comment_count$count + 1)
    
    observeEvent({
      # input$submit_comment
      comment_count$count
    },{
      if(comment_count$count > 0){
        user_comment <- paste0(input$user_id, ": ", input$txt)
        if(is.null(comment_board)) comment_board <<- user_comment
        else comment_board <<- paste(comment_board, user_comment, sep = "\n")
      }
    })
    
    
    output$comment_board <- renderText({
      invalidateLater(200)
      comment_board
      }) 
    

  }
)
