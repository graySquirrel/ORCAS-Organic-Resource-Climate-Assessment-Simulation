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
    colnames(o) <- c("old GHG Kg CO2","new GHG Kg CO2")
    o
}

anyValids <- function(tbl) {
    if (length(which(as.numeric(rowSums(tbl[,1:4])) == as.numeric(rowSums(tbl[,5:8]))))==0) {
        return (NULL)
    }
    return (c(2:9))
}
anyWarnings <- function(tbl) {
    if (length(which(as.numeric(rowSums(tbl[,1:4])) != as.numeric(rowSums(tbl[,5:8]))))==0) {
        return (NULL)
    }
    return (c(2:9))
}

shinyServer(function(input, output, session) {
    #cachedTblFrom <- NULL
    #cachedTblTo <- NULL
    validate <- function(name,tbl){
        #updateTableStyle(session, "tblTo", "warning",3:5,2:3)
        t1 <- sapply(tbl[,2:9],as.numeric)
        updateTableStyle(session, "tblTo", "valid", 
                         which(as.numeric(rowSums(t1[,1:4])) == as.numeric(rowSums(t1[,5:8]))),
                         anyValids(t1)
        )
        updateTableStyle(session, "tblTo", "warning", 
                         which(as.numeric(rowSums(t1[,1:4])) != as.numeric(rowSums(t1[,5:8]))),
                         anyWarnings(t1)
        )
    }
    
    #output$resTab <- renderTable(makeRes(input$tblTo))
    output$resPlot <- renderPlot({
        x <- makeRes(input$tblTo)
        counts <- t(as.matrix(x))
        print(counts)
        par(mar=c(13,4,1,15))
        barplot(height=counts,
                col=c("darkblue","red"),
                legend.text=rownames(counts),
                args.legend=list(x = "right",inset=-.3),
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
            validate("tblTo", tblTo)
            return(tblTo)
        }
    })  
    
})