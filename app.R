library(shiny)
source("baselineFuncs.R")

ui <- fluidPage(
    # *Input() functions,
    titlePanel("Waste Treatment Simulator"),
    fluidRow(
       column(2,
            checkboxInput(inputId = "ranges", label = "show ranges"),
            checkboxInput(inputId = "CStorage", label = "include Carbon Storage", value = TRUE),
            checkboxGroupInput(inputId = "pathways",
                               label = "treatment pathways",
                               choices = c("AD"="AD",
                                           "Animal Feed"="AF",
                                           "Compost"="CM",
                                           "Landfill"="LF"), 
                               selected = c("AD","AF","CM")),
            radioButtons(inputId = "displacement", label = "choose displacement option",
                         choices = c("none" = "none",
                                     "Fertilizer displacement" = "FD",
                                     "Peat displacement" = "Peat",
                                     "Blended" = "Blended"))),
      column(10,
            # submitButton(text = "Calculate")
            # *Output() functions
            textOutput(outputId = "args"),
            plotOutput(outputId = "pathwaysChart")
        )
    )
)

server <- function(input, output) {
    o <- getBaselineResults()
    output$args <- renderText({paste("Range",input$ranges,", CStorage", input$CStorage)
        print(input$pathways)
                                    })
#     output$pathwaysChart <- renderPlot({createPathwaysPlot(
#         doRanges = input$ranges,
#         CS = input$CStorage,
#         paths = input$pathways,
#         disp = input$displacement)})
}

shinyApp(ui = ui, server = server)