#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyalert)
library(shinyjs)
# Define UI for application that draws a histogram
#
#

parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel(
    "catch",
    sliderInput(
      "catch",
      "Catch:",
      min = 0,
      max = 6000,
      value = 4500,
      step = 10
    ),
    actionLink(
      "help2",
      label = "What does this do?",
      icon = icon("info-circle"),
      style = "color: #1faad7; font-size: 14px"
    )
  ),
  tabPanel(
    "f",
    sliderInput(
      "f",
      "Exploitaton Rate (F):",
      min = 0,
      max = .2,
      value = 0.1
    ),
    actionLink(
      "help3",
      label = "What does this do?",
      icon = icon("info-circle"),
      style = "color: #1faad7; font-size: 14px"
    )
  ),
  tabPanel(
    "esc",
    sliderInput("esc",
                "Escapement (x 1,000):",
                min = 20,
                max = 60,
                value = 30),
    actionLink("help4", label="What does this do?", icon=icon("info-circle"), style="color: #1faad7; font-size: 14px")
  )
) # close tabsetPanel

shinyUI(tagList(
  useShinyjs(),
  useShinyalert(),
  fluidPage(
    # Application title
    titlePanel("Harvest Strategies"),
    h2("What is this?"),
    h4("This app simulates fisheries across different harvest strategies"),
    h4("It follows the general Federal USA practice of defining 'overfished' as B/BMSY < 0.5"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "sigma",
          "Environmental Noise",
          min = 0,
          max = 0.6,
          value = 0,
          step = 0.05
        ),
        ########## Java addition ##################
        actionLink(
          "help1",
          label = "What does this do?",
          icon = icon("info-circle"),
          style = "color: #1faad7; font-size: 14px"
        ),
        hr(),
        sliderInput(
          "bstart",
          "Starting Biomass (x 1000)",
          min = 0,
          max = 100,
          value = 33
        ),
        hr(),
        radioButtons(
          "hr",
          "Harvest Strategy:",
          c(
            "Constant Catch" = "catch",
            "Constant Exploitation Rate" = "f",
            "Constant Escapement" = "esc"
          )
        ),
        parameter_tabs,
        hr(),
        actionButton("do.one", "Run Once"),
        actionButton("do", "Run Many Times and Summarize"),
        hr()
      ),
      
      # Show a plot of the generated distribution
      mainPanel(tabsetPanel(
        tabPanel("Single Model Run", plotOutput("simplot")),
        tabPanel("Summary of Many Runs", tableOutput("sumtable"),hr(), plotOutput("itplot"))
      ))
    )
  )
))
