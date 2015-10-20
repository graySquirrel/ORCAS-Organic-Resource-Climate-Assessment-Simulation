library(shiny)
source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
source("treatmentLandfill.R")
source("treatmentcompost.R")
source("treatmentAnimalFeed.R")
source("parseGlobalFactors.R")
source("baselineFuncs.R")

df <- data.frame(c("AD_Digester_CH4Leaks","AD_MCFactor","AD_Digester_parasiticLoad"),
                 c("CH4 Leaks","utilization factor","parastic load"),
                 c("AD","AD","AD"),
                 c("ADstats","ADstats","ADstats"),
                 c("Food waste","Food waste","Food waste"),
                 stringsAsFactors = FALSE)
colnames(df) <- c("p","n1","n2","s","f")


ui <- fluidPage(
    # *Input() functions,
    titlePanel("Waste Treatment Simulator"),
    fluidRow(
        column(2,
               #actionButton("reset","Reset data"),
               checkboxInput(inputId = "ranges", label = "show uncertainty"),
               textInput(inputId = "filename", label = "feedstock file", 
                         value = "Feedstock.csv")
        ),
        column(10,
               # submitButton(text = "Calculate")
               # *Output() functions
               textOutput(outputId = "args"),
               plotOutput(outputId = "pathwaysChart", height = 600, width = 800),
               uiOutput(outputId = "sensitivitiesChart") # to plot multiple charts
        )
    )
)

server <- function(input, output) {
    AllStats <- calcAllStats(FSmemfile=NULL, GFmemfile=NULL)
    output$pathwaysChart <- renderPlot({
        createPathwaysPlot(s=AllStats,
                           doRanges = input$ranges)
    }, height = 600, width = 800)
    output$sensitivitiesChart <- renderUI({ 
        plot_output_list <- lapply(1:dim(df)[1], function(i){plotOutput(df[[i,1]])})
        do.call(tagList, plot_output_list)
        }) 
    for(i in 1:dim(df)[1]) {
        output[[df[[i,1]]]] <- renderPlot({
            plotFS(AllStats[[df[i,4]]],df[[i,5]],df[[i,1]],df[[i,2]],df[[i,3]])
        },width=400)
    }
}

shinyApp(ui = ui, server = server)