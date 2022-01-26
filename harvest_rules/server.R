#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(patchwork)

theme_set(theme_minimal(base_size = 18) + theme(legend.position = "top"))

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
  run.single.pop <-function(hr, bstart, catch = 0, f=0, esc=0, sigma, n.years = 100) {
    r <- 0.2
    K <- 100000
    msy <- r*K/4
    bmsy <- K/2
    fmsy <- r/2
    years <- 0:n.years
    catches <- rep(NA, times = length(years))
    output <- rep(NA, times = length(years))
    output[1] <- bstart
    prod.func <- function(N, r, K) N * r * (1-N/K)
    cex.mult <- 1.25
    if (sigma > 0) vt <- rnorm(length(years), mean = - sigma^2/2, sd = sigma)
    if (sigma ==0) vt <- rep(0, length(years))
    
    if(hr == "catch") {
      catches[1] <- min(bstart, catch)
      for (i in 2:length(years)) {
        
        output[i] <- max(0,output[i-1] + prod.func(output[i-1], r,K)*exp(vt[i-1]) - catches[i-1])
        
        catches[i] <- min(catch, output[i])
      }
      
      production <- prod.func(output, r,K)*exp(vt)
      
    }
    
    if(hr == "f") {
      catches[1] <- bstart * f
      for (i in 2:length(years)) {
        output[i] <- max(0,output[i-1] + prod.func(output[i-1], r,K)*exp(vt[i-1]) - catches[i-1])
        catches[i] <-f*output[i]
      }
      
      production <- prod.func(output, r,K)*exp(vt)
      
    } 
    
    if(hr == "esc") {
      catches[1] <- max(0, bstart - esc)
      for (i in 2:length(years)) {
        output[i] <- max(0,output[i-1] + prod.func(output[i-1], r,K)*exp(vt[i-1]) - catches[i-1])
        catches[i] <-max(0, output[i] - esc)
      }
      
      production <- prod.func(output, r,K)*exp(vt)
      
    }
    
    
    
    blist <- seq(0, K, length.out = 100)
    prod <- prod.func(blist, r, K)
    harvest <- rep(NA, 100)
    if (hr == "catch") for (i in 1:100) harvest[i] <- min(catch, blist[i])
    if (hr == "f") harvest = blist * f
    if (hr == "esc") for (i in 1:100) harvest[i] <- max(0, blist[i] - esc)
    
    results <- tibble(Biomass = output, Production = production, Catch = catches) %>% 
      mutate(f = Catch / Biomass,
             `B/BMSY` = Biomass / bmsy,
             `F/FMSY` = f / fmsy,
             `Catch/MSY` = Catch  / msy,
             year = years)
    
    eq_results <- tibble(Biomass = blist, `Average Production` = prod, `Harvest Rule` = harvest) %>% 
      pivot_longer(-Biomass, names_to = "metric", values_to = "value")
    
  
    # make top plot
    
    
    long_results <- results %>% 
      pivot_longer(-year, names_to = "metric", values_to = "value")
    
    # performance measures
    years.overfished <- length(which(output <= 0.25 * K))
    p_overfished <- scales::percent(years.overfished / length(output))
    mean.biomass <- mean(output)
    mean.catch <- mean(catches)
    catch.var <- mean(abs(catches[-length(catches)] - catches[-1]))
    
    labels <- tibble(x = K * .3,
                label = c(paste0("% Years Overfished = ", p_overfished),
                paste0("Years Overfished = ", years.overfished),
                paste0("Mean Biomass = ", round(mean.biomass)),
                paste0("Mean Catch = ", round(mean.catch)),
                paste0("Mean Catch Variability = ", round(catch.var))
                )) %>% 
      mutate(y = seq(0.4 * msy, 0 * msy, length.out = nrow(.)))
    
    prod_breaks <- seq(0,1.5 * msy, by = 1000)
    
    prod_labs <- c(prod_breaks[1:(length(prod_breaks) -1 )], expression("" >= "1.5 x MSY"))

    
    trajectory_plot <- long_results %>% 
      filter(metric %in% c("Biomass", "Production","Catch")) %>% 
      ggplot(aes(year, value, color = metric)) + 
      geom_line(size = 2, show.legend = TRUE) + 
      scale_color_viridis_d(name = '') + 
      scale_x_continuous(name = "Year") + 
      scale_y_continuous(name = "") + 
      coord_cartesian(ylim = c(0, K))
      # facet_wrap(~metric, scales = "free_y", strip.position = "top")
    
    
    b_and_p <- results 

    
    production_plot <- ggplot() + 
      geom_vline(aes(xintercept = 0.25 * K),linetype = 2, color = "tomato") + 
      geom_line(data = eq_results, aes(Biomass, value, color = metric), size = 2) + 
      geom_path(data = results, aes(Biomass, Production), color = "lightgrey") +
      geom_point(data = results, aes(Biomass, Production, fill = year), shape = 21, size = 4, alpha = 0.9, color = "lightgrey") +
      scale_fill_viridis(name = "Year", option = "plasma", direction = -1, guide = guide_colorbar(frame.colour = "black", barwidth = unit (6, "lines"))) + 
      scale_color_manual(name = '', values = c("tomato", "black"))  +
      scale_x_continuous(name = "Population Biomass", limits = c(0,NA)) + 
      scale_y_continuous(name = "Biomass", breaks = prod_breaks, labels = prod_labs) + 
      geom_text(data = labels, aes(x,y, label = label),check_overlap = TRUE,
                hjust = "left") + 
      coord_cartesian(ylim = c(0,1.5 * msy))
  
  
    reference_trajectory = long_results %>% 
      filter(grepl("\\/",metric)) %>% 
      ggplot(aes(year, value, color = metric)) + 
      geom_line(size = 2) + 
      scale_color_discrete(name = '') + 
      coord_cartesian(ylim = c(0,NA)) + 
      scale_x_continuous(name = "Year") + 
      scale_y_continuous(name = "Reference Value")
    
    
    ylabs <- c(seq(0,3.5, by = 0.5), expression("" >= "4"))
    
    ybreaks <- seq(0,4, by = 0.5)
    
    kobe_plot <- results %>% 
      # filter((year %% 5) == 0) %>% 
      ggplot(aes(`B/BMSY`, pmin(4,`F/FMSY`), fill = year)) + 
      geom_vline(aes(xintercept = 0.5),linetype = 2, color = "tomato") + 
      geom_hline(aes(yintercept = 1), linetype = 2) + 
      geom_vline(aes(xintercept = 1), linetype = 2) +
      geom_path() + 
      geom_point(size = 4, shape = 21, alpha = 0.9, color = "lightgrey") + 
      scale_x_continuous(limits = c(0, 2.25)) + 
      scale_y_continuous(name = "F/FMSY", breaks =  ybreaks, labels = ylabs) +
      coord_cartesian(ylim = c(0, 4)) + 
      scale_fill_viridis(name = "Year", option = "plasma", direction = -1, guide = guide_colorbar(frame.colour = "black", barwidth = unit (10, "lines"))) 
      

    production_plot / trajectory_plot / kobe_plot / reference_trajectory

  }
  
  run.many <-function(hr,bstart, catch = 0, f=0, esc=0, sigma, n.years = 100) {
    r <- 0.2
    K <- 100000
    msy <- r*K/4
    bmsy <- K/2
    fmsy <- r/2
    its <- 200
    years <- 0:n.years
    
    years.overfished <- mean.catch <- mean.biomass <- catch.var <- rep(NA, times = its)
    
    bit <- matrix(NA, nrow =  length(years), ncol = its)
    
    cit <- matrix(NA, nrow =  length(years), ncol = its)
  
    a <- Sys.time()
    
    for (j in 1:its) {
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
      
      bit[,j] <- output
      
      cit[,j] <- catches
      
      # performance measures
      years.overfished[j] <- length(which(output <= 0.25 * K))
      mean.biomass[j] <- mean(output)
      mean.catch[j] <- mean(catches)
      catch.var[j] <- mean(abs(catches[-length(catches)] - catches[-1]))
    }
    
    d <- Sys.time() - a

    long_bit <- bit %>%
      as.data.frame() %>%
      mutate(year = years) %>%
      pivot_longer(
        -year,
        names_to = "iteration",
        values_to = "Population Biomass",
        names_prefix = "V",
        names_transform = list(iteration = as.integer)
      ) 
      
    long_cit <- cit %>%
      as.data.frame() %>%
      mutate(year = years) %>%
      pivot_longer(
        -year,
        names_to = "iteration",
        values_to = "Catch",
        names_prefix = "V",
        names_transform = list(iteration = as.integer)
      ) 
    
    
    long_things <- long_bit %>% 
      left_join(long_cit,by = c("year","iteration")) %>% 
      pivot_longer(-c(year, iteration))
    
    
    ref_line <- tibble(name = c("Population Biomass", "Catch"), y = c(K * .25, NA))
    
    
    sim_plot <- long_things %>% 
      ggplot(aes(year, value, color = name, group = interaction(name, iteration))) + 
      geom_hline(data = ref_line, aes(yintercept = y), color = "tomato", linetype = 2) +
      geom_line(show.legend = FALSE, alpha = 0.2) + 
      facet_wrap(~name, scales = "free_y") + 
      coord_cartesian(ylim = c(0,NA)) + 
      scale_x_continuous(name = "Year") + 
      scale_y_continuous("Biomass")
    
    
    # Make Table
    sim.out <- as.data.frame(matrix(NA, nrow = 4, ncol = 2))
    names(sim.out) = c("Metric", "Summaries Across 200 simulations")
    sim.out[,1] <- c("Mean # Years overfished", "Mean Biomass", "Mean Catch", "Mean Catch Variability")
    sim.out[,2] <- c(as.character(round(mean(years.overfished))),
                           round(mean(mean.biomass)),
                           round(mean(mean.catch)),
                           round(mean(catch.var))
    )
    return(list(sim_table = sim.out,
                sim_plot = sim_plot))
  }
  mult <- 1000
  output$simplot <- renderPlot({run.single.pop(hr = input$hr,bstart = mult*input$bstart, catch = input$catch, f = input$f, esc = mult*input$esc, sigma = input$sigma)},
                               height = 1500,
                               width = 600)
  output$sumtable <- renderTable({run.many(hr = input$hr, bstart = mult*input$bstart, catch = input$catch, f = input$f, esc = mult*input$esc, sigma = input$sigma)$sim_table}
  )
  
  output$itplot <- renderPlot({run.many(hr = input$hr, bstart = mult*input$bstart, catch = input$catch, f = input$f, esc = mult*input$esc, sigma = input$sigma)$sim_plot}
  )
  
  
  observeEvent(input$do.one, {
    output$simplot <- renderPlot({run.single.pop(hr = input$hr,bstart = mult*input$bstart, catch = input$catch, f = input$f, esc = mult*input$esc, sigma = input$sigma)},
                                 height = 1500,
                                 width = 600)
    
      
    })
  observeEvent(input$do, {

    output$sumtable <- renderTable({run.many(hr = input$hr, bstart = mult*input$bstart, catch = input$catch, f = input$f, esc = mult*input$esc, sigma = input$sigma)$sim_table}
    )
    
    output$itplot <- renderPlot({run.many(hr = input$hr, bstart = mult*input$bstart, catch = input$catch, f = input$f, esc = mult*input$esc, sigma = input$sigma)$sim_plot}
    )
                                 
    
  })
})
