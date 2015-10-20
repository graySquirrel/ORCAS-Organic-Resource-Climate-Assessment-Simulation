library(shiny)
library(shinyTable)

shinyServer(function(input, output, session) {
    cachedTblFrom <- NULL
    cachedTblTo <- NULL
    
  validate <- function(tbl){
#     updateTableStyle(session, "tbl", "valid", 
#                      which(as.numeric(tbl$num2) < 50), 2)
#     updateTableStyle(session, "tbl", "warning", 
#                      which(as.numeric(tbl$num2) >= 50 & 
#                              as.numeric(tbl$num2) < 100), 2)
#     updateTableStyle(session, "tbl", "invalid", 
#                      which(as.numeric(tbl$num2) >= 100), 2)    
  }
  
  output$tblFrom <- renderHtable({
    if (is.null(input$tblFrom)){
      rows <- length(f1$type)
      acol <- rep(0,rows)
      tblFrom <- data.frame(Landfill = acol,AD = acol, 
                            Compost = acol, Animal.Feed = acol, 
                            row.names = f1$type)
      cachedTblFrom <<- tblFrom     
      return(tblFrom)
    } else{
      # Updates from client. The server has been made aware and can do some
      # validation or updates here, then send back the revised table. In this
      # case, we'll filter any number >= 100 in the first column.
      tblFrom <- input$tblFrom
      cachedTblFrom <<- tblFrom
      return(tblFrom)
    }
  })  
  
  output$tblTo <- renderHtable({
      if (is.null(input$tblTo)){
          rows <- length(f1$type)
          acol <- rep(0,rows)
          tblTo <- data.frame(Landfill = acol,AD = acol, 
                                Compost = acol, Animal.Feed = acol, 
                                row.names = f1$type)
          cachedtblTo <<- tblTo     
          return(tblTo)
      } else{
          # Updates from client. The server has been made aware and can do some
          # validation or updates here, then send back the revised table. In this
          # case, we'll filter any number >= 100 in the first column.
          tblTo <- input$tblTo
          
          # Any non-numeric data should be replaced with the cached data.
          #tbl[is.na(as.integer(as.character(tbl[,1]))),1] <- 
          #    as.character(cachedTbl[is.na(as.integer(as.character(tbl[,1]))),1])
          
          #validate(tbl)
          
          #tbl[as.integer(as.character(tbl[,1])) >= 100,1] <- 99
          cachedtblTo <<- tblTo
          return(tblTo)
      }
  })  
  
})