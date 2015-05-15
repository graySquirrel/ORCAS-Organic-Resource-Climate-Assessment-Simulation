#options(rgl.useNULL=TRUE)
library(shiny)
library(rgl)
library(shinyRGL)
source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 

shinyServer(function(input,output) {
    g1 <- GlobalFactors()
    
    output$surfplot <- renderPlot({ #renderWebGL({
#    output$surfplot <- renderWebGL({
    
        size <- 10
        TKNVec <- seq(from =580, to=5600,length.out=size)
        LoVec <-  seq(from = 15, to=413,length.out=size)
        TKNVecRep <- rep(TKNVec,each=size)
        LoVecRep  <- rep(LoVec,times=size)
        f1 <- Feedstock(type="GTW",
                        TS=17,
                        TVS=18,
                        Lo=LoVecRep,
                        TKN=TKNVecRep)
        res <- AnaerobicDigestionTreatmentPathway(f1, g1, 20,debug = F)
        dfres <- data.frame(LoVecRep,TKNVecRep,res)
        kgPerMT <- xtabs(res ~ LoVecRep+TKNVecRep, data=dfres)
        persp(LoVec,TKNVec,kgPerMT,theta=input$theta,phi=input$phi,shade=0.25,
              col="lightblue",ticktype="simple",
              xlab="Lo",ylab="TKN",zlab="kgCO2e/MT")
        #plot(LoVec)
        #axes3d()
        
    })
})