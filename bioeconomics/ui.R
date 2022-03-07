#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Economics of Fishing"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "price",
                "Price per KG",
                min = 1e-3,
                max = 10,
                value = 1,
                pre = "$"
            ),
            sliderInput(
                "cost",
                "Cost per fishing day",
                min = 0,
                max = 1000,
                value = 30
            ),
            sliderInput(
                "ymax",
                "# Years",
                min = 1,
                max = 50,
                value = 1,
                animate = animationOptions(interval = 500, loop = TRUE, playButton = icon("play"), pauseButton = icon("pause"))
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(
            tabPanel("Open Access Dynamics" , plotOutput("open_access")),
            tabPanel("Equilibrium", plotOutput("eq_plot"))
            
        ))# close mainPanel
    )
))
