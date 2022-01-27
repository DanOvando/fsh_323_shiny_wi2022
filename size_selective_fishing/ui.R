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
  titlePanel("Size-Selective Fishing"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("u",
                   "Fishing Mortality Rate (F):",
                   min = 0,
                   max = 1,
                   value = 0.0,
                   step = 0.025),
       radioButtons("fishing.method", "Fishing Method",
                    c(
                      "Fish Aggregating Device" =  "fad",
                      "School Sets" = "school",
                      "Dolphin Sets" = "dolphin",
                      "Longline" = "longline")),
       radioButtons("rec.type", "Recruitment",
                    c(
                      "Constant" = "const",
                      "Spawning biomass-dependent" = "var"
                    ))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("What is this?",h2("What is this?"), p("This app simulates the effects of different fishing gears and fishing mortality rates on a population of Yellowfin tuna (", em("Thunnus albacares"),")"),
                 h2("What do I do?"),
                 p("Adjust the sliders following the instructions in your lab assignment, and examine the results in each tab of the app."),
                 img(src = "900x600-yellowfin-tuna-noaa.jpg")),
        tabPanel("Catch and Biomass by Age", plotOutput("age_eq_plot")),
        tabPanel("Population and Catch Summary", tableOutput("eq_table"), plotOutput("eq_plot")),
        tabPanel("Recruitment", plotOutput("rec_plot"),helpText('The replacement line shows the number of recruits needed to "replace" the amount of spawning biomass (x axis) in the next generation.  Whenever recruitment is greater than the "replacement line", more recruits are produced than needed to replace the current generation of spawners so the population will grow.  When recruitment is less than the replacement line, too few recruits are produced to replace the current generation of spawners, so the population will decline'))
    ))
  )
))

