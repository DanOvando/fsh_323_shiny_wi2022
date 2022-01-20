#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(viridis)

theme_set(theme_minimal(base_size = 18))
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  
    output$data <- renderPlot({
        
        set.seed(42)
        
        r <- 0.42
        
        k <- 323 * 2
        
        years <- 30
        
        b <- rep(0, years)
        
        b[1] <- k * 1
        
        b_hat <- rep(0, years)
        
        b_hat[1] <- input$k
        
        q <- 1
        
        f <- c(seq(0.01,r * .75, length.out = years / 2), seq(r/2,r/4, length.out = years / 2))
        
        effort <- f / q
        
        cv <- 0.2
        
        for (y in 2:years){
            
            b[y] <- max(0,b[y-1] + b[y-1] * r * (1 - b[y-1] / k) - b[y-1] * f[y-1]) * rlnorm(1,0,cv)
            
            b_hat[y] <- max(0,b_hat[y-1] + b_hat[y-1] * input$r * (1 - b_hat[y-1] / input$k) - b[y-1] * f[y-1])
            
        }
        
        catch <- b * f
        
        CPUE_hat <- b_hat * q
        out <- tibble(Biomass = b, Catch = catch, CPUE = catch / (effort), Year = 1:years) %>% 
            mutate(f_hat = catch / b_hat,
                   `F/FMSY` = f_hat / (input$r/2),
                   `B/BMSY` = b_hat / (input$k/2))
        
        
        catch_plot <- out %>% 
            ggplot(aes(Year, Catch)) + 
            geom_point(show.legend = FALSE, size = 4, shape = 21,fill = "darkgrey") + 
            scale_y_continuous(limits = c(0,NA)) + 
            labs(subtitle = "Observed Catch")
        
        cpue_plot <- out %>% 
            ggplot() + 
            geom_point(aes(Year, CPUE),show.legend = FALSE, size = 4, shape = 21,fill = "steelblue") + 
            geom_line(aes(Year,CPUE_hat, color = "Estimated CPUE"), size = 2) + 
            scale_y_continuous(limits = c(0,NA)) + 
            scale_color_manual(name = '', values = "tomato") + 
            theme(legend.position = "top") + 
            labs(title= "Observed (points) and Estimated (line) CPUE")
        
        status_plot <- out %>% 
            ggplot(aes(`B/BMSY`,`F/FMSY`)) + 
            geom_hline(aes(yintercept = 1)) + 
            geom_vline(aes(xintercept = 1))+
            geom_path(size = 1) + 
            geom_point(aes(fill = Year), size = 4,shape = 21) +
            scale_x_continuous(limits = c(0,2)) + 
            scale_y_continuous(limit = c(0,NA)) + 
            labs(subtitle = "Estimated B/BMSY and F/FMSY") + 
            scale_fill_viridis(option = "inferno", guide = guide_colorbar(frame.colour = "black",barwidth = unit(10, "lines")), direction = -1) + 
            theme(legend.position = "top")
        
        cpue_plot / (catch_plot | status_plot)
        

        
    },
    width = 700,
    height = 800)

})
