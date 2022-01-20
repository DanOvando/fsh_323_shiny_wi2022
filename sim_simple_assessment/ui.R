#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("A Simple Stock Assessment"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            withMathJax(includeMarkdown("simple_stock_assessment.md")),
            sliderInput("r",
                        "Intrinsic Growth Rate (r)",
                        min = 0,
                        max = 1,
                        value = 0.1, 
                        step = .01),
            sliderInput("k",
                        "Carrying Capacity (K)",
                        min = 1e-3,
                        max = 1500,
                        value = 50),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("data")
        )
    )
))
