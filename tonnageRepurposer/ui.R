library(shiny)
library(shinyTable)
#' Define UI for application that demonstrates a simple Handsontable
#' @author Jeff Allen \email{jeff@@trestletech.com}
# shinyUI(
#     pageWithSidebar(
#         # Application title
#         headerPanel("Move your waste!"),
#         sidebarPanel(
#             h3("Instructions")#,
#             #             tableOutput("resTab")
#         ),
#         mainPanel(
#             h3("Compare Scenarios.   Enter waste in MT."),
#             htable("tblTo", colHeaders="provided"),
#             h3("Results"),
#             tableOutput("resTab"),
#             plotOutput("resPlot")
#         )     
#     )
# )



shinyUI(fluidPage(
    headerPanel("Move your waste!"),
    fluidRow(
        column(2,
               h3("Instructions")
        ),
        column(6,
               h3("Compare Scenarios.   Enter waste in MT."),
               htable("tblTo", colHeaders="provided"),
               h3("Results"),
               tableOutput("resTab"),
               plotOutput("resPlot")
        )
    )
))