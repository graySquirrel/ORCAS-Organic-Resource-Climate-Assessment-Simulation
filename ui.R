#options(rgl.useNULL=TRUE)
library(shiny)
library(shinyRGL)

shinyUI(fluidPage(
    
    titlePanel("Anaerobic Digestion Treatment", windowTitle="AD pathway"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("theta",
                        "theta:",
                        min = 0,
                        max = 360,
                        value = 30),
            sliderInput("phi",
                        "phi:",
                        min = 0,
                        max = 360,
                        value = 30)
        ),
        
    mainPanel(
        plotOutput("surfplot") #webGLOutput("surfplot")
        #webGLOutput("surfplot")
        
    ))
))