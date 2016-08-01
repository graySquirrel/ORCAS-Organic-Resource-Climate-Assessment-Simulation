library(shiny)
library(shinyTable)

shinyUI(fluidPage(
    headerPanel("ORCAS Tonnage repurposer"),
    fluidRow(
        column(2,
               h3("Instructions"),
               h6("input tonnes of waste in old and new section and see how different the net emisisons are."),
               code("NOTE: rows highlighted in yellow mean that sums of old and new tonnage don't match")
        ),
        column(6,
               h3("Compare Scenarios.   Enter waste in MT."),
               htable("tblTo", colHeaders="provided"),
               h3("Results (in Units of Kilograms of CO2 equivalent)"),
               #tableOutput("resTab"),
               plotOutput("resPlot")
        )
    )
))