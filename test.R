library(shiny)
library(waiter)

ui <- fluidPage(
  autoWaiter(),
  actionButton(
    "trigger",
    "Render"
  ),
  helpText("Note: while the data view will show only",br(),
           "the specified number of observations, the",
           "summary will be based on the full dataset."),
  plotOutput("plot"),
  plotOutput("plot2")
)

server <- function(input, output){
  output$plot <- renderPlot({
    input$trigger
    Sys.sleep(3)
    plot(cars)
  })
  
  output$plot2 <- renderPlot({
    input$trigger
    Sys.sleep(5)
    plot(runif(100))
  })
}

shinyApp(ui, server)