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
shinyServer(function(input, output) {
  # make main function
  plot.fun <- function(fishing.pattern, fmax) {
    
    K <- 100
    r <- 0.4
    fmsy <- r/2
    msy <- 4* r/K
    bmsy <- K/2
    cex.mult = 1.75
    f.adj = 1 #fmsy / 0.3
  
    
    if (fishing.pattern ==1) fs <- rep(fmax * f.adj, 30) 
    if (fishing.pattern ==2) fs <- c(seq(0, fmax * f.adj, length.out = 10), rep(fmax * f.adj, 20))
    if (fishing.pattern ==3) fs <- c(seq(fmax, fmax / 2, length.out = 10), rep(fmax / 2 , 20))
    if (fishing.pattern ==4) fs <- c(seq(0, fmax , length.out = 10), rev(seq(fmax * f.adj/2, fmax, length.out = 10)), rep(fmax * f.adj /  2, 10))
    # make fishing pattern plot
    layout(matrix(c(1,2,2,3,3), nrow = 5, ncol = 1))
    par(las = 1, mar = c(4,5,1,1))
    plot(1:30, fs,
         type = "l",
         lwd = 3,
         xlab = "Year",
         ylab = "Fishing Mortality Rate",
         ylim = c(0, 0.8), 
         cex.lab = cex.mult,
         cex.axis = cex.mult,
         axes = T
    )
    # box()
    # axis(side = 1, at = c(0, 50, 100), cex.axis = cex.mult)
    # axis(side = 2, at = seq(0,1, by = 0.25), labels = seq(0,1, by = 0.25), cex.axis = cex.mult)
    
    catches <- output <- rep(NA, 30)
    
    output[1] <- K * input$init_dep / 100
    catches[1] <- fs[1] * output[1]
    
    n.loops <- length(output)
    for (i in 2:n.loops) {
      output[i] <-max(0, output[i - 1] * r * (1 - output[i - 1] / K) + output[i - 1] - catches[i-1] )
      catches[i] <- fs[i] * output[i]
    }
    library(viridis)
    cols <- rev(plasma(30))
    
   
    blist <- seq(0, K, length.out = 100)
    plot(blist, blist * r * (1 - blist /K),
         type = "l",
         col = "black",
         lwd = 2,
         xlim = c(0, 100),
         ylim = c(0, 25),
         xlab = "Population Biomass",
         ylab = "Production (line) and catches (points)",
         cex.lab = cex.mult,
         cex.axis = cex.mult
    )
    bt.minus<-output[-length(output)]
    c.minus <- catches[-length(catches)]
    bt <- output[-1]
    ct <- catches[-1]
    
    for (i in 1:(n.loops-1)) {
      segments(bt.minus[i], c.minus[i], bt[i], ct[i],lwd = 2, col = cols[i])
    }
    
    points(output, catches,
           pch = 21,
           bg = cols,
           cex = 2,
           col= cols)
    
    bt.minus <- bt.minus / bmsy
    bt <- bt/bmsy
    ft.minus <- fs[-length(fs)] / fmsy
    ft <- fs[-1] / fmsy
    
    plot(c(), c(),
         xlim <- c(0,2),
         ylim <- c(0,4),
         type = "l",
         lwd = 2,
         xlab = "B/BMSY",
         ylab =  "F/FMSY",
         cex.lab = cex.mult,
         cex.axis = cex.mult
    )
    abline (h = 1, lty = "dashed", lwd = 2)
    abline (v = 1, lty = "dashed", lwd = 2)
    
    for (i in 1:(n.loops-1)) {
      segments(bt.minus[i], ft.minus[i], bt[i], ft[i],lwd = 2, col = cols[i])
    }
    
    
    points(output / bmsy, fs / fmsy,
           pch = 21,
           bg = cols,
           cex = 2,
           col = cols)
  }
  
  plot.catches <- function(fishing.pattern, fmax) {
    K <- 100
    r <- 0.4
    fmsy <- r/2
    msy <- r * K / 4
    bmsy <- K/2
    cex.mult = 1.75
    f.adj = 1
    
    
    if (fishing.pattern ==1) fs <- rep(fmax * f.adj, 30) 
    if (fishing.pattern ==2) fs <- c(seq(0, fmax * f.adj, length.out = 10), rep(fmax * f.adj, 20))
    if (fishing.pattern ==3) fs <- c(seq(fmax, fmax/2 , length.out = 10), rep( fmax/2 , 20))
    if (fishing.pattern ==4) fs <- c(seq(0, fmax , length.out = 10), rev(seq(fmax * f.adj/2, fmax, length.out = 10)), rep(fmax * f.adj /  2, 10))
    
    # make fishing pattern plot
    
    par(las = 1, mar = c(4,5,1,1))
    layout(matrix(c(1,2,2,3,3), nrow = 5, ncol = 1))
    plot(1:30, fs ,
         type = "l",
         lwd = 3,
         xlab = "Year",
         ylab = "Fishing Mortality Rate",
         ylim = c(0, 0.8), 
         cex.lab = cex.mult,
         cex.axis = cex.mult,
         axes = T
    )
    # box()
    # axis(side = 1, at = c(0, 50, 100), cex.axis = cex.mult)
    # axis(side = 2, at = seq(0,1, by = 0.25), labels = seq(0,1, by = 0.25), cex.axis = cex.mult)
    
    catches <- output <- rep(NA, 30)
    
    output[1] <- K * input$init_dep / 100
    catches[1] <- fs[1] * output[1]
    
    n.loops <- length(output)
    for (i in 2:n.loops) {
      output[i] <-max(0, output[i - 1] * r * (1 - output[i - 1] / K) + output[i - 1] - catches[i-1] )
      catches[i] <- fs[i] * output[i]
    }
    library(viridis)
    cols <- rev(plasma(30))
    blist <- seq(0, K, length.out = 100)
    plot(blist, blist * r * (1 - blist /K),
         type = "l",
         col = "black",
         lwd = 2,
         xlim = c(0, 100),
         ylim = c(0, 25),
         xlab = "Population Biomass",
         ylab = "Production (line) and catches (points)",
         cex.lab = cex.mult,
         cex.axis = cex.mult
    )
    
    bt.minus<-output[-length(output)]
    c.minus <- catches[-length(catches)]
    bt <- output[-1]
    ct <- catches[-1]
    
    for (i in 1:(n.loops-1)) {
      segments(bt.minus[i], c.minus[i], bt[i], ct[i],lwd = 2, col = cols[i])
    }
    points(output, catches,
           pch = 21,
           bg = cols,
           cex = 2,
           col = cols)
  
  
    plot(1:30, output ,
         xlim <- c(0,30),
         ylim <- c(0,100),
         type = "l",
         col = "blue",
         lwd = 4,
         xlab = "Year",
         ylab = "Biomass (blue) and Catch (red)",
         cex.lab = cex.mult,
         cex.axis = cex.mult
    )
    lines(1:30, catches,
           lwd = 2,
           col = "red")

  }
  output$simplot <- renderPlot({plot.fun(input$fishing.pattern, input$fmax)},
                               width = 600,
                               height = 800)
  output$catchplot <- renderPlot({plot.catches(input$fishing.pattern, input$fmax)},
                               width = 600,
                               height = 800)
  
})
