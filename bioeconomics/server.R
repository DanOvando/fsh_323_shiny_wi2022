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
library(patchwork)
library(forcats)
library(viridis)
theme_set(theme_minimal(base_size = 14) + theme(legend.position = "top"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    r <- 0.42
    
    k <- 1000
    
    years <- 50
    
    msy <- r * k / 4
    
    q <- 0.1
    
    fmsy <- r / 2
    
    bmsy <- k / 2
    
    output$open_access <- renderPlot({

      price <- input$price
      
      cost <- input$cost
      
      ymax <- input$ymax
      
      emsy <- fmsy / q
      
      pk <- price * k - cost * emsy * 2
      
      delta <- 2 # increase in 
      
      theta <- (emsy * 2 * (delta - 1)) / pk
      
      # (emsy * 2 + theta * pk) / (emsy * 2)
      
      bio <- effort <- catch <- profit <- rep(NA, years)
    
      bio[1] <- k    
      
      effort[1] <- r / 2  / q
      
      for (y in 2:years){
          
          catch[y - 1] <- bio[y - 1] * min(1,q * effort[y - 1])
          
          profit[y-1] <- price * catch[y - 1] -  cost * effort[y - 1]
          
          bio[y] <- max(0,bio[y-1] + bio[y-1] * r * (1 - bio[y-1] / k) - catch[y-1])
          
          effort[y] <- max(0,effort[y - 1]  + theta * profit[y-1])
          
      }
      
      catch[years] <- bio[years] * min(1,q * effort[years])
      
      profit[years] <- price * catch[years] -  cost * effort[years]
      
      

      results <-
          tibble(
              catch = catch,
              profit = profit,
              biomass = bio,
              effort = effort,
              cost = effort * cost,
              revenue = catch * price,
              f = effort * q,
              b_bmsy = bio / bmsy,
              f_fmsy = f / fmsy,
              year = 1:years
          )
      
      fleet_plot <- results %>% 
          select(year, effort, profit) %>% 
          pivot_longer(-year) %>% 
          ggplot(aes(year, value, color = name)) + 
          geom_line()
          
      
      ybreaks <- seq(0,years, by = 10)
      
      ylabs <- ybreaks
      
      spiral_plot <- results %>%
          filter(year <= ymax) %>%
          ggplot(aes(profit, effort, color = year)) +
          geom_vline(aes(xintercept = 0), linetype = 2) +
          geom_path(size = 1.25) +
          scale_color_viridis(
              name = "Year",
              option = "plasma",
              direction = -1,
              guide = guide_colorbar(frame.colour = "black", barwidth = unit (10, "lines")),
              limits = c(0,years)
          ) +
          scale_x_continuous(
              name = "Profit",
              labels = scales::dollar,
              limits = c(min(results$profit), max(results$profit))
          ) +
          scale_y_continuous(name = "Fishing Days", limits = c(min(results$effort), max(results$effort)))
      
      kobe_plot <- results %>%
          filter(year <= ymax) %>%
          ggplot(aes(b_bmsy, f_fmsy, fill = year)) +
          geom_vline(aes(xintercept = 0.5), linetype = 2, color = "tomato") +
          geom_hline(aes(yintercept = 1), linetype = 2) +
          geom_vline(aes(xintercept = 1), linetype = 2) +
          geom_path() +
          geom_point(
              size = 4,
              shape = 21,
              alpha = 1,
              show.legend = FALSE
          ) +
          scale_x_continuous(name = "B/BMSY", limits = c(0, 2.25)) +
          scale_y_continuous(name = "F/FMSY") +
          coord_cartesian(ylim = c(0, 4)) +
          scale_fill_viridis(
              name = "Year",
              option = "plasma",
              direction = -1,
              guide = guide_colorbar(frame.colour = "black", barwidth = unit (10, "lines")),
              limits = c(0, years)
          )
      
      spiral_plot / kobe_plot

    },
    height = 700,
    width = 600)
    
    output$eq_plot <- renderPlot({
        
        price <- input$price
        
        cost <- input$cost
        
        fseq <- seq(0, 2, by = .01)
        
        bseq = 2 - fseq
        
        biomass = bseq * bmsy
        
        production <- biomass * r * (1 - biomass / k)
        
        f <- fseq * fmsy
        
        effort <- f / q
        
        out <-
            tibble(
                effort = effort,
                biomass = biomass,
                production = production,
                Revenue = production * price,
                Cost = effort * cost,
                Profit = Revenue - Cost
            )
        
        emey <- out$effort[which.max(out$Profit)]
        
        out %>% 
            select(effort, Cost, Revenue, Profit) %>% 
            pivot_longer(-effort) %>% 
            mutate(name = forcats::fct_relevel(name, c("Revenue","Cost","Profit"))) %>% 
            ggplot(aes(effort,value, color = name)) + 
            geom_vline(aes(xintercept = emey)) +
            geom_hline(aes(yintercept = 0), linetype = 2) +
            geom_line(size = 2) +
            scale_x_continuous(name = "Fishing Effort") + 
            scale_y_continuous(name = "$",labels = scales::dollar) + 
            scale_color_manual(values = c("steelblue", "tomato","black"), name = '')
        
  
        
    })

})
