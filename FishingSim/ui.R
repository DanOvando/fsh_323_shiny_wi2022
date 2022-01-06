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
    titlePanel("Fished Population Simulator"),
    sidebarLayout(
      sidebarPanel(
        "Adjust the sliders to change the level of fishing and the starting popultion",
        helpText("Set the number of fish caught every year"),
        sliderInput(
          "catch",
          "Catch Level:",
          min = 1,
          max = 20,
          value = 10
        ),
        helpText("Set the number of fish the population starts with"),
        sliderInput(
          "n.start",
          "Starting Population Size:",
          min = 1,
          max = 100,
          value = 50
        ),
        helpText("Set the production type. We'll what this means in class!"),
        radioButtons(
          "prodtype",
          "Production Type",
          c(
            "Constant Per-Capita Production"  =  "const",
            "Compensatory Per-Capita Production" = "compensatory"
          )
        )
      ),
      mainPanel(h4("Final catch, production, and population size"),
                plotOutput(outputId = "simplot"),
                br(),
                br(),
                br(),
                br(),
                h4("Trajcetory of catch, production, and population size"),
                plotOutput(outputId = "plot2"))
      )
    )
  )
  
  