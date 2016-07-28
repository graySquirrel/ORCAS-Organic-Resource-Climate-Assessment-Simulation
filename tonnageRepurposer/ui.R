library(shiny)
library(shinyTable)

shinyUI(fluidPage(
    headerPanel("ORCAS Tonnage repurposer"),
    fluidRow(
        column(2,
               h3("Instructions"),
               h6("input tonnes of waste in old and new section and see how different the net emisisons are.")
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