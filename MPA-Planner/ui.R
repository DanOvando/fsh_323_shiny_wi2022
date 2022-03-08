
library(shiny)
shinyUI(fluidPage(
    waiter::use_waiter(),
    # Application title
    titlePanel("MPA Design Platform"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            helpText("You can save MPA configurations generated on the input graph and upload them here"),
            fileInput("upload", "Upload MPA Design"),
            helpText("Once you have an MPA design you are happy with, click the button below to run simulations."),
            actionButton("simulate", "Simulate MPAs"),
            sliderInput("cons_weight","Manager Conservation Weighting", min = 0, max = 1, value = 0.33, step = 0.01)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Design MPA",
                         h1("Paint over cells to draw an MPA network"),
                         helpText("Once you're happy with an MPA network, click the 'simulate MPAs' button to the left",br(),
                                  "To clear the current MPA network, double-click on the graph"),
                         plotOutput("mpa",
                                    brush = brushOpts(id = "plot_brush", clip = FALSE), dblclick = "plot_reset")
                         ,
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         textOutput("progress"),
                         helpText("Click the download button to save your MPA network for future use"),
                         downloadButton("download_mpa", label = "Download MPA"),
                         h1("Unfished Adult Distribution"),
                         plotOutput("habitat"),
                         br(),
                         br(),
                         br(),
                         br(),
                         h1("Pre-MPA Conditions"),
                         plotOutput("history")),
                tabPanel("Conservation Results",
                         plotOutput("cons_results")),
                tabPanel("Fishery Results",
                         plotOutput("fish_results")),
                tabPanel("Results Summary", 
                         helpText("Click the download button to save the results of your MPA network for future use"),
                         downloadButton("download_obj", label = "Download Results"),
                         tableOutput("objectives"))
            ) # close tabset panel
    ) # close mainpanel
)# close sidebarlayout
)# close fluidpage
) #close shinyUI
