#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  observeEvent(input$hr, {
    updateTabsetPanel(inputId = "params", selected = input$hr)
  }) 
  
  # Help Text
  observeEvent(input$help1, {
    # Show a modal when the button is pressed
    shinyalert(title=NULL, 
               text=HTML("<b>
                        This influences how much environmental factors govern productivity. If set to 0, productivity is determined solely by the population size. Values >0 introduce variability, such that the productivity might be higher or lower than expected based on population size."), 
               closeOnClickOutside = TRUE, 
               html=TRUE, 
               animation=FALSE)
  })
  
  observeEvent(input$help2, {
    # Show a modal when the button is pressed
    shinyalert(title=NULL, 
               text=HTML("<b>
                         This sets the annual catch amount when the Constant Catch Harvest strategy is selected."), 
               closeOnClickOutside = TRUE, 
               html=TRUE, 
               animation=FALSE)
  })
  observeEvent(input$help3, {
    # Show a modal when the button is pressed
    shinyalert(title=NULL, 
               text=HTML("<b>
                         This sets the exploitation rate when the Constant Exploitation harvest strategy is selected."), 
               closeOnClickOutside = TRUE, 
               html=TRUE, 
               animation=FALSE)
  })
  observeEvent(input$help4, {
    # Show a modal when the button is pressed
    shinyalert(title=NULL, 
               text=HTML("<b>
                         This sets the escapement rate when the Constant Escapement harvest strategy is selected."), 
               closeOnClickOutside = TRUE, 
               html=TRUE, 
               animation=FALSE)
  })


  
 # vt.sys <- reactive({vt.sys <- rnorm(n = 51, mean = 0 - input$sigma^2/2, input$sigma)})
  run.single.pop <-function(hr, bstart, catch = 0, f=0, esc=0, sigma) {
    r <- 0.2
    K <- 100000
    msy <- r*K/4
    bmsy <- K/2
    fmsy <- r/2
    n.years <- 50
    years <- 0:n.years
    catches <- rep(NA, times = length(years))
    output <- rep(NA, times = length(years))
    output[1] <- bstart
    prod.func <- function(N, r, K) N * r * (1-N/K)
    cex.mult <- 1.25
    if (sigma > 0) vt <- rnorm(51, mean = - sigma^2/2, sd = sigma)
    if (sigma ==0) vt <- rep(0, length(years))
    
    if(hr == "catch") {
      catches[1] <- min(bstart, catch)
      for (i in 2:length(years)) {
        output[i] <- max(0,output[i-1] + prod.func(output[i-1], r,K)*exp(vt[i-1]) - catches[i-1])
        catches[i] <- min(catch, output[i])
      }
    }
    
    if(hr == "f") {
      catches[1] <- bstart * f
      for (i in 2:length(years)) {
        output[i] <- max(0,output[i-1] + prod.func(output[i-1], r,K)*exp(vt[i-1]) - catches[i-1])
        catches[i] <-f*output[i]
      }
    } 
    
    if(hr == "esc") {
      catches[1] <- max(0, bstart - esc)
      for (i in 2:length(years)) {
        output[i] <- max(0,output[i-1] + prod.func(output[i-1], r,K)*exp(vt[i-1]) - catches[i-1])
        catches[i] <-max(0, output[i] - esc)
      }
    } 
    library(viridis)
    # make top plot
    cols <- plasma(3)
    time.cols <- rev(plasma(length(years)))
    par(las = 1, mfrow = c(2,1), mar = c(4,4,1,1))
    plot(years, output/1000,
         type = "l",
         lwd = 2,
         col = cols[1],
         xlim <- c(0, 50),
         ylim <- c(0, K / 1000),
         xaxs = "i",
         yaxs = "i",
         xlab = "Year",
         ylab = "Population and Catch (x1,000)",
         cex.lab = cex.mult,
         cex.axis = cex.mult
    )
    lines(years, catches/1000,
          lwd =2,
          col = cols[2])
    legend("topright", legend = c("Population", "Catch"), lty = "solid", lwd = 2, col = cols[1:2])
    # make bottom plot
    blist <- seq(0, K, length.out = 100)
    prod <- prod.func(blist, r, K)
    harvest <- rep(NA, 100)
    if (hr == "catch") for (i in 1:100) harvest[i] <- min(catch, blist[i])
    if (hr == "f") harvest = blist * f
    if (hr == "esc") for (i in 1:100) harvest[i] <- max(0, blist[i] - esc)
    plot(blist / 1000, prod / 1000,
         type = "l",
         col = cols[1],
         lwd = 2,
         xlim = c(0, K/1000),
         ylim = c(0,15),
         xlab = "Population (x 1,000)",
         ylab = "Production and Catch (x 1,000)",
         xaxs = "i",
         yaxs = "i",
         cex.lab = cex.mult,
         cex.axis = cex.mult)
    
    lines(blist/ 1000, harvest / 1000,
          lwd = 2,
          col = cols[2])
    # plot actual production
    bt.minus <- output[-length(output)] / 1000
    pt.minus <- (prod.func(output, r, K) * exp(vt))[-length(output)] /1000
    bt <- output[-1] / 1000
    pt <- (prod.func(output, r, K) * exp(vt))[-1] /1000
    for (i in 1:(length(years)-1)) {
      segments(bt.minus[i], pt.minus[i], bt[i], pt[i],lwd = 2, col = time.cols[i])
    }
    
    points(output / 1000, prod.func(output, r, K) * exp(vt) /1000,
          pch = 21,
          bg = time.cols,
          col = time.cols)
    legend("topright", legend = c("Average Production", "Catch"), lty = "solid", lwd = 2, col = cols[1:2])
    
    # performance measures
    years.overfished <- length(which(output <= 0.25 * K))
    mean.biomass <- mean(output)
    mean.catch <- mean(catches)
    catch.var <- mean(abs(catches[-length(catches)] - catches[-1]))
    
    # Make Table
    text(x = 5, y = 14, pos = 4, paste("# Years overfished = ", round(years.overfished)))
    text(x = 5, y = 13, pos = 4, paste("Average Biomass = ", round(mean.biomass,0)))
    text(x = 5, y = 12, pos = 4, paste("Average Catch = ", round(mean.catch,0)))
    text(x = 5, y = 11, pos = 4, paste("Average Catch Variability = ", round(catch.var,0)))
  }
  
  run.many <-function(hr,bstart, catch = 0, f=0, esc=0, sigma) {
    r <- 0.2
    K <- 100000
    msy <- r*K/4
    bmsy <- K/2
    fmsy <- r/2
    
    n.years <- 50
    years <- 0:n.years
    years.overfished <- mean.catch <- mean.biomass <- catch.var <- rep(NA, times = 1000)
    
    for (j in 1:2000) {
      catches <- rep(NA, times = length(years))
      output <- rep(NA, times = length(years))
      output[1] <- bstart
      prod.func <- function(N, r, K) N * r * (1-N/K)
      if (sigma > 0) vt <- rnorm(n = length(years), mean = 0 - sigma^2/2, sigma)
      if (sigma ==0) vt <- rep(0, length(years))
      
      if(hr == "catch") {
        catches[1] <- min(bstart, catch)
        for (i in 2:length(years)) {
          output[i] <- max(0,output[i-1] + prod.func(output[i-1], r,K)*exp(vt[i-1]) - catches[i-1])
          catches[i] <- min(catch, output[i])
        }
      }
      
      if(hr == "f") {
        catches[1] <- bstart * f
        for (i in 2:length(years)) {
          output[i] <- max(0,output[i-1] + prod.func(output[i-1], r,K)*exp(vt[i-1]) - catches[i-1])
          catches[i] <-f*output[i]
        }
      } 
      
      if(hr == "esc") {
        catches[1] <- max(0, bstart - esc)
        for (i in 2:length(years)) {
          output[i] <- max(0,output[i-1] + prod.func(output[i-1], r,K)*exp(vt[i-1]) - catches[i-1])
          catches[i] <-max(0, output[i] - esc)
        }
      } 
      
      
      # performance measures
      years.overfished[j] <- length(which(output <= 0.25 * K))
      mean.biomass[j] <- mean(output)
      mean.catch[j] <- mean(catches)
      catch.var[j] <- mean(abs(catches[-length(catches)] - catches[-1]))
    }
    
    # Make Table
    sim.out <- as.data.frame(matrix(NA, nrow = 4, ncol = 2))
    names(sim.out) = c("Metric", "Summaries Across 2,000 simulations")
    sim.out[,1] <- c("# Years overfished", "Average Biomass", "Average Catch", "Average Catch Variability")
    sim.out[,2] <- c(as.character(round(mean(years.overfished),1)),
                           round(mean(mean.biomass),-1),
                           round(mean(mean.catch),-1),
                           round(mean(catch.var),0)
    )
    return(sim.out)
  }
  
  output$simplot <- renderPlot({run.single.pop(hr = input$hr,bstart = 1000*input$bstart, catch = input$catch, f = input$f, esc = 1000*input$esc, sigma = input$sigma)},
                               height = 700,
                               width = 460)
  output$sumtable <- renderTable({run.many(hr = input$hr, bstart = 1000*input$bstart, catch = input$catch, f = input$f, esc = 1000*input$esc, sigma = input$sigma)}
  )
  
  observeEvent(input$do.one, {
    output$simplot <- renderPlot({run.single.pop(hr = input$hr,bstart = 1000*input$bstart, catch = input$catch, f = input$f, esc = 1000*input$esc, sigma = input$sigma)},
                                 height = 700,
                                 width = 460)
    
      
    })
  observeEvent(input$do, {
    output$sumtable <- renderTable({run.many(hr = input$hr, bstart = 1000*input$bstart, catch = input$catch, f = input$f, esc = 1000*input$esc, sigma = input$sigma)})
                                 
    
  })
})
