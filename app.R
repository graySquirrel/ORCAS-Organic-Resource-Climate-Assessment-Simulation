library(shiny)
source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
#source("treatmentLandApplication.R") 
source("treatmentLandfill.R")
source("treatmentcompost.R")
source("treatmentAnimalFeed.R")
source("parseGlobalFactors.R")
source("baselineFuncs.R")

#################################################################################
makeYforPlot1 <- function(s=NULL) {
    y1 <- massageDataforPlot(s$ADfstats$confDat, s$b$ADf$ADnetEmissions,"AD")
    y2 <- massageDataforPlot(s$LFstats$confDat, s$b$LF$LandfillNetEmissions,"LF")
    y3Special <- massageDataforPlot(s$CMpstats$confDat, s$b$CMb$final,"CMb")
    y5 <- massageDataforPlot(s$AFstats$confDat,s$b$AF$EMAnimalFeed,"AF")
    y <- rbind(y1,y2,y5,y3Special)
    # order by LF
    y$feedstock <- factor(y$feedstock, levels=y$feedstock[order(y2$Emissions)]) 
    y
}

ui <- fluidPage(
    # *Input() functions,
    titlePanel("Waste Treatment Simulator"),
    fluidRow(
        column(2,
               selectInput(inputId = "select", label = h3("Loading..."), 
                           choices = list("Please Wait..."), 
                           selected = 1)
        ),
        column(10,
               textOutput(outputId = "args"),
               plotOutput(outputId = "pathwaysChart", height = 600, width = 800),
               uiOutput(outputId = "sensitivitiesChart") # to plot multiple charts
        )
    )
)

server <- function(input, output, session) {
    AllStats <- calcAllStats(FSmemfile=NULL, GFmemfile=NULL)
    y = makeYforPlot1(AllStats)
    
    updateSelectInput(session, inputId="select", label = "Select feedstock", 
                      choices = as.character(unique(y$feedstock)))
    
    output$pathwaysChart <- renderPlot({
                makePathwaysPlot(doRanges = FALSE,#input$ranges,
                                 y[y$feedstock == input$select,],
                                 title = "Emissions",
                                 angle=0)
            }, height = 600, width = 800)
}

shinyApp(ui = ui, server = server)