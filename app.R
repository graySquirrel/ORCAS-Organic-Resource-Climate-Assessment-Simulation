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
    y1 <- massageDataforPlot(s$ADfstats$confDat, s$b$ADf$ADnetEmissions,"Anaerobic Digestion")
    y2 <- massageDataforPlot(s$LFstats$confDat, s$b$LF$LandfillNetEmissions,"Landfill")
    y3Special <- massageDataforPlot(s$CMpstats$confDat, s$b$CMb$final,"Compost")
    y5 <- massageDataforPlot(s$AFstats$confDat,s$b$AF$EMAnimalFeed,"Animal Feed")
    y <- rbind(y1,y2,y5,y3Special)
    # order by LF
    y$feedstock <- factor(y$feedstock, levels=y$feedstock[order(y2$Emissions)]) 
    y
}

AllStats <- calcAllStats(FSmemfile=NULL, GFmemfile=NULL)
y = makeYforPlot1(AllStats)

ui <- fluidPage(
    # *Input() functions,
    titlePanel("ORCAS:  Organic Resource Climate change impact Assessment Simulation"),
    helpText(a("Try ORCAS Tonnage repurposer", 
               href="https://ebner.shinyapps.io/ORCAStonnageRepurposer/",
               target="_blank")),
    fluidRow(
        column(2,
               selectizeInput(inputId = "select", label = h3("Loading..."), 
                              choices = list("Please Wait..."), 
                              selected = 1, width='400px',
                              multiple = TRUE, 
                              options = list(
                                  placeholder = 'select predefined feedstocks',
                                  onInitialize = I('function() { this.setValue(""); }')
                              )
               ),
               checkboxInput("checkbox", label = "Include Custom Input", value = FALSE),
               h6("Custom input starts by using a copy of MSWFW, and lets you adjust the below parameters"),
               htmlOutput("slider1"),
               htmlOutput("slider2"),
               htmlOutput("slider3")
        ),
        column(10,
               plotOutput(outputId = "pathwaysChart", height = 600, width = 800)
        )
    )
)

server <- function(input, output, session) {
    output$slider1 <- renderUI({
        if (input$checkbox == TRUE) {
            sliderInput("slider1", label = "TS", min = 0.001, 
                        max = 1, value = 0.3)
        } 
    })
    output$slider2 <- renderUI({
        if (input$checkbox == TRUE) {
            sliderInput("slider2", label = "Bo", min = 200, 
                        max = 500, value = 334)
        } 
    })
    output$slider3 <- renderUI({
        if (input$checkbox == TRUE) {
            sliderInput("slider3", label = "TKN", min = 500, 
                        max = 20000, value = 8900)
        } 
    })
#     output$value <- renderPrint({ 
#         if (input$checkbox == TRUE) {
#             paste(input$slider1,input$slider2,input$slider3,str(input$select))
#             }
#         })
    
    updateSelectInput(session, inputId="select", label = "Feedstock", 
                      choices = as.character(unique(y$feedstock)))
    
    output$pathwaysChart <- renderPlot({
        z <- y
        theInput <- input$select
        if (input$checkbox == TRUE) {
            #update theInput and z
            i <- read.csv(file="Feedstock.csv",sep = ",",stringsAsFactors=FALSE)
            j <- i[i$Feedstock == "MSWFW",]
            if (!is.null(input$slider1)) {
                j$TS <- input$slider1
                j$Bo <- input$slider2
                j$TKN <- input$slider3
            }
            j$Feedstock <- "Custom"
            o <- calcAllStats(FSmemfile=j, GFmemfile=NULL)
            df <- makeYforPlot1(o)
            theInput <- c("Custom", theInput)
            z <- rbind(y,df)
        }
        makePathwaysPlot(doRanges = FALSE,
                         z[z$feedstock %in% theInput,],
                         title = "Emissions (Kg CO2eq per metric ton of material)",
                         angle=0)
    }, height = 600, width = 800)
}

shinyApp(ui = ui, server = server)
