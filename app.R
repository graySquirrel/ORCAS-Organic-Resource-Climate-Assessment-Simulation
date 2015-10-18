library(shiny)
source("baselineFuncs.R")
foo <- NULL

ui <- fluidPage(
    # *Input() functions,
    titlePanel("Waste Treatment Simulator"),
    fluidRow(
        column(2,
               #actionButton("reset","Reset data"),
               checkboxInput(inputId = "ranges", label = "show uncertainty"),
               textInput(inputId = "filename", label = "feedstock file", 
                         value = "Feedstock.csv")
               #             checkboxInput(inputId = "CStorage", label = "include Carbon Storage", value = TRUE),
               #             checkboxGroupInput(inputId = "pathways",
               #                                label = "treatment pathways",
               #                                choices = c("AD"="AD",
               #                                            "Animal Feed"="AF",
               #                                            "Compost"="CM",
               #                                            "Landfill"="LF"), 
               #                                selected = c("AD","AF","CM")),
               #             radioButtons(inputId = "displacement", label = "choose displacement option",
               #                          choices = c("none" = "none",
               #                                      "Fertilizer displacement" = "FD",
               #                                      "Peat displacement" = "Peat",
               #                                      "Blended" = "Blended"))
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
    output$args <- renderText(
        {a<-paste("Range",input$ranges,", CStorage", input$CStorage)
        b <- foo
        print(paste(a,b))
        })
    output$pathwaysChart <- renderPlot({
        createPathwaysPlot(s=AllStats,
                           doRanges = input$ranges)
    }, height = 600, width = 800)
    output$sensitivitiesChart <- renderUI({ 
        plot_output_list <- list(
            plotOutput("CH4 Leaks, AD"),
            plotOutput("utilization Factor, AD")
            )
        do.call(tagList, plot_output_list)
        }) 
    output[["CH4 Leaks, AD"]] <- renderPlot({
        plotFactorSensitivity(AllStats$ADstats,"Food waste",
                               "AD_Digester_CH4Leaks","CH4 Leaks","AD") },
        width=400,height=300)
    output[["utilization Factor, AD"]] <- renderPlot({
        plotFactorSensitivity(AllStats$ADstats,"Food waste",
                              "AD_MCFactor","utilization Factor","AD")  },
        width=400,height=300)
    
    
    foo <- 1#reactive({input$filename})
}

shinyApp(ui = ui, server = server)