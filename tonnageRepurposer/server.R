library(shiny)
library(shinyTable)

scaleFacs <- read.csv(file="scaleFactors.csv",stringsAsFactors = FALSE,row.names = 1)

initTbl <- function() {
    rows <- length(scaleFacs[,1])
    cols <- 2 * length(scaleFacs[1,])
    cols <- cols+1
    o <- data.frame(matrix(rep(0,rows*cols),nrow=rows,ncol=cols))
    o[,1] <- rownames(scaleFacs)
    colnames(o) <- c("Feedstock","Old LF", "Old AD", 
                     "Old CM","Old AF",
                     "New LF","New AD","New CM","New AF")
    o
}
makeRes <- function(inTab) {
    o <- NULL
    if (! is.null(inTab)) {
        tin <- as.matrix(inTab[,2:5])
        tin2 <- as.double(tin)
        oldprod <- tin2*scaleFacs
        tin <- as.matrix(inTab[,6:9])
        tin2 <- as.double(tin)
        newprod <- tin2*scaleFacs
        oldSum <- rowSums(oldprod)
        newSum <- rowSums(newprod)
        oldTot <- sum(oldSum)
        newTot <- sum(newSum)
        totdf <- data.frame(oldTot, newTot)
        colnames(totdf) <- c("oldSum","newSum")
        rownames(totdf) <- c("Total")
        o <- data.frame(oldSum,newSum)
        o <- rbind(o, totdf)
    } else {
        rows <- length(scaleFacs[,1])
        cols <- 2
        o <- data.frame(matrix(rep(0,rows*cols),nrow=rows,ncol=cols))
        rownames(o) <- rownames(scaleFacs)
    }
    colnames(o) <- c("old GHG","new GHG")
    o
}
shinyServer(function(input, output, session) {
    #cachedTblFrom <- NULL
    #cachedTblTo <- NULL
    validate <- function(tbl){
    }
    
    #output$resTab <- renderTable(makeRes(input$tblTo))
    output$resPlot <- renderPlot({
        x <- makeRes(input$tblTo)
        counts <- t(as.matrix(x))
        print(counts)
        par(mar=c(12,4,1,1))
        barplot(height=counts,
                col=c("darkblue","red"),
                legend=rownames(counts),
                beside=TRUE,
                las=2)
        })
    
    output$tblTo <- renderHtable({
        if (is.null(input$tblTo)){
            tblTo <- initTbl()
            #print(tblTo)
            #cachedtblTo <<- tblTo     
            return(tblTo)
        } else{
            tblTo <- input$tblTo
            #print(tblTo)
            #tblTo <- cachedtblTo
            return(tblTo)
        }
    })  
    
})