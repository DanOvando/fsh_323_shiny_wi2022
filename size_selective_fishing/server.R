#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(viridis)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
theme_set(theme_minimal(base_size = 14))
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   # load functions
  
  
  get.select <- function(fish.method) {
    agelist <- seq(0.5, 6, by = 0.1)
    if(fish.method=="fad") {
      selectivity <- c(0.176966891,
                       0.370161849,
                       0.738088715,
                       0.920765932,
                       1,
                       0.828057611,
                       0.589046512,
                       0.403249614,
                       0.313736282,
                       0.25938457,
                       0.232688308,
                       0.208520127,
                       0.180942353,
                       0.159122578,
                       0.16732155,
                       0.13602716,
                       0.105000686,
                       0.091326785,
                       0.076309287,
                       0.041141192,
                       0.036571745,
                       0.030418864,
                       0.02301416,
                       0.014427322,
                       0.003721111,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0)
      # Now smooth
      dat <- as.data.frame(cbind(agelist, selectivity))
      names(dat) <- c("age", "sel")
      smoothed <- loess(sel ~ age, data = dat, span = 0.15)
      selectivity <- smoothed$fitted / max(smoothed$fitted)
      
    }
    if(fish.method == "school") {
      selectivity <- c(0.047090052,
                       0.113762396,
                       0.253713983,
                       0.42684955,
                       0.62153945,
                       0.841925678,
                       0.91088413,
                       0.953007245,
                       1,
                       0.999832343,
                       0.910259528,
                       0.766005343,
                       0.618257347,
                       0.506795243,
                       0.437738711,
                       0.394356879,
                       0.348078913,
                       0.324964474,
                       0.320500576,
                       0.317705636,
                       0.287933237,
                       0.272703131,
                       0.258741218,
                       0.241653475,
                       0.218929046,
                       0.163635237,
                       0.101200736,
                       0.101552066,
                       0.100356092,
                       0.0978301,
                       0.092260339,
                       0.086044912,
                       0.075492773,
                       0.064824404,
                       0.037286229,
                       0.00880222,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0)
      
      dat <- as.data.frame(cbind(agelist, selectivity))
      names(dat) <- c("age", "sel")
      smoothed <- loess(sel ~ age, data = dat, span = 0.2)
      selectivity <- smoothed$fitted / max(smoothed$fitted)
      selectivity[agelist>4] <- 0
      selectivity[selectivity<0] <- 0
      
    }
    if(fish.method == "dolphin") {
      selectivity <- c(0.004193156,
                       0.009748905,
                       0.018924591,
                       0.038326875,
                       0.062251576,
                       0.086031572,
                       0.114372354,
                       0.154170873,
                       0.209695344,
                       0.283714314,
                       0.37493462,
                       0.423742938,
                       0.45420192,
                       0.486268,
                       0.508711802,
                       0.525045451,
                       0.598171832,
                       0.660993924,
                       0.6964466,
                       0.712180348,
                       0.742127975,
                       0.704323537,
                       0.686218682,
                       0.709921731,
                       0.746164421,
                       0.823663124,
                       0.917150971,
                       0.973809879,
                       1,
                       0.953624081,
                       0.912749306,
                       0.869601237,
                       0.832787858,
                       0.796144255,
                       0.783199129,
                       0.771582696,
                       0.703220961,
                       0.609481214,
                       0.519342926,
                       0.453891471,
                       0.388440017,
                       0.31964432,
                       0.228679814,
                       0.137715308,
                       0.046750802,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0,
                       0)
      dat <- as.data.frame(cbind(agelist, selectivity))
      names(dat) <- c("age", "sel")
      smoothed <- loess(sel ~ age, data = dat, span = 0.35)
      selectivity <- smoothed$fitted / max(smoothed$fitted)
      selectivity[agelist >=5] <- 0
    }
    if (fish.method == "longline") {
      a50 <- 2.5
      a95 <- 2.85
      selectivity <- ( 1 + exp(log(19)*(a50 - agelist)/(a95 - a50)))^(-1)
      
    }
    return(selectivity)
  }
  get.lh <- function() {
    agelist <- seq(0.5, 6.0, by = 0.1)
    lh <- matrix(c(
      0.5,	0.654668129,	0.00,	1.25, 
      0.6,	0.889292902,	0.00,	1.15, 
      0.7,	1.186955837,	0.00,	1.05, 
      0.8,	1.558218987,	0.00,	1.00, 
      0.9,	2.013907733,	0.00,	0.95, 
      1,	2.564820902,	0.00,	0.80 ,
      1.1,	3.221423549,	0.00,	0.80, 
      1.2,	3.993537626,	0.01,	0.80 ,
      1.3,	4.890045583,	0.02,	0.80 ,
      1.4,	5.918620714,	0.04,	0.80 ,
      1.5,	7.08549592,	0.09,	0.80 ,
      1.6,	8.395279834,	0.14,	0.80, 
      1.7,	9.850826173,	0.22,	0.80 ,
      1.8,	11.45315904,	0.30,	0.80 ,
      1.9,	13.20145392,	0.38,	0.80 ,
      2,	15.09307155,	0.46,	0.80 ,
      2.1,	17.12363957,	0.54,	0.80, 
      2.2,	19.28717547,	0.61,	0.80 ,
      2.3,	21.5762431,	0.67,	0.80 ,
      2.4,	23.98213469,	0.72,	0.80, 
      2.5,	26.49507013,	0.77,	0.80 ,
      2.6,	29.10440579,	0.80,	0.80 ,
      2.7,	31.79884561,	0.84,	0.80 ,
      2.8,	34.56664838,	0.86,	0.80 ,
      2.9,	37.39582573,	0.88,	0.80 ,
      3,	40.27432679,	0.90,	0.80 ,
      3.1,	43.1902063,	0.92,	0.80 ,
      3.2,	46.13177398,	0.93,	0.80, 
      3.3,	49.08772405,	0.94,	0.80 ,
      3.4,	52.0472442,	0.95,	0.80 ,
      3.5,	55.00010445,	0.95,	0.80, 
      3.6,	57.93672648,	0.96,	0.80 ,
      3.7,	60.84823474,	0.96,	0.80 ,
      3.8,	63.72649059,	0.97,	0.80 ,
      3.9,	66.56411141,	0.97,	0.80 ,
      4,	69.35447615,	0.97,	0.80 ,
      4.1,	72.09171939,	0.98,	0.80, 
      4.2,	74.77071552,	0.98,	0.80 ,
      4.3,	77.38705482,	0.98,	0.80 ,
      4.4,	79.93701304,	0.98,	0.80 ,
      4.5,	82.41751595,	0.98,	0.80 ,
      4.6,	84.82610022,	0.99,	0.80 ,
      4.7,	87.16087175,	0.99,	0.80 ,
      4.8,	89.42046259,	0.99,	0.80 ,
      4.9,	91.60398717,	0.99,	0.80 ,
      5,	93.71099881,	0.99,	0.80 ,
      5.1,	95.7414469,	0.99,	0.80 ,
      5.2,	97.69563543,	0.99,	0.80, 
      5.3,	99.57418307,	0.99,	0.80 ,
      5.4,	101.3779853,	0.99,	0.80 ,
      5.5,	103.1081787,	0.992307138,	0.80, 
      5.6,	104.7661074,	0.99269166,	0.80 ,
      5.7,	106.353292,	0.993038256,	0.80 ,
      5.8,	107.871401,	0.993351395,	0.80 ,
      5.9,	109.3222242,	0.993634932,	0.80, 
      6,	110.707649,	0.99389221,	0.80 ),
      nrow = length(agelist), ncol = 4, byrow = T)
    colnames(lh) <- c("age", "W", "pMat", "M")
    lh <- as.data.frame(lh)
    lh$G <- c(log(lh$W[-1] / lh$W[-nrow(lh)]),0)
    return(lh)
    
  }
  run.eq <- function(u, fishing.method, rec.type) {
    agelist <- seq(0.5, 6, by = 0.1)
    selectivity <- get.select(fishing.method)
    rzero  <-  500
    bha <-  957.99
    bhb <-  1.75631
    h <- 0.75
    dt <- 0.1
    lh <- get.lh()
    f <- -log(max(c(0.001, 1 - u)))
    ca <- z <- na <- rep(1, length(agelist))
    z <- (lh$M + f * selectivity)
    g <- lh$G
    la <- length(agelist)
    for (i in 2:la)
      na[i] <- na[i - 1] * exp(-z[i - 1] * dt)
    
    bpr <- na * lh$W
    
    sbpr <- sum(bpr * lh$pMat) * dt / 1000
    
    
    if (rec.type == "const")
      rinfty <- 500
    if (rec.type == "var")
      rinfty <- max(0, (bha * sbpr - 1) / (bhb * sbpr))
    ba <- rinfty * bpr / 1000 * (1 - exp(- (z - g) * dt)) / (z - g)
    ca <- ba * f*selectivity
    c_total <- sum(ca)
    return(output = list(ba = ba, ca = ca, c_total = c_total, sbpr= sbpr, rinfty = rinfty, rzero = rzero,n_at_a = na, w_at_a = lh$W))
  }
  plot_equil <- function(u, fishing.method, rec.type) {
    agelist <- seq(0.5, 6, by = 0.1)
    dt <- 0.1
    selectivity <- get.select(fishing.method)
    output <- run.eq(u, fishing.method, rec.type)
    unfished_output <- run.eq(0, fishing.method, rec.type)
    
    ba <- output$ba
    ca <- output$ca
    sbpr <- output$sbpr
    rinfty <- output$rinfty
    
    lh <- tibble(age = agelist, `Number of Tuna` = unfished_output$n_at_a, `Weight per Tuna` = unfished_output$w_at_a) %>% 
      mutate(`Total Biomass` = `Number of Tuna` *  `Weight per Tuna`) %>% 
      pivot_longer(-age)
    
    lh_plot <- lh %>% 
      ggplot(aes(age, value, fill = name)) + 
      geom_density(stat = "identity", show.legend = FALSE) + 
      facet_wrap(~name,scales = "free_y") + 
      scale_x_continuous(name = "Age (Years)", breaks = seq(min(agelist), max(agelist), by = 1), guide = guide_axis(n.dodge = 2)) + 
      scale_y_continuous(name = "Value") +
      theme(axis.text.x = element_text(size = 12))
  
    results <- tibble(age = agelist, biomass = ba, catch = ca, selectivity = selectivity)
    
    sel_plot <- results %>% 
      ggplot(aes(age, selectivity)) + 
      geom_hline(aes(yintercept = 0)) +
      geom_line(size = 2, color = "tomato") + 
      scale_x_continuous(name = "Age (Years)",breaks = seq(min(agelist), max(agelist), by = 1), labels = seq(min(agelist), max(agelist), by = 1)) + 
      scale_y_continuous(name = "Gear Selectivity")
    
    bio_plot <- results %>% 
      ggplot(aes(age, biomass)) + 
      geom_density(stat = "identity", fill = "steelblue") + 
      scale_x_continuous(name = "Age (Years)",breaks = seq(min(agelist), max(agelist), by = 1), labels = seq(min(agelist), max(agelist), by = 1)) + 
      scale_y_continuous(name = "Biomass Alive (million MT)", limits = c(0,0.45))
    
    catch_plot <- results %>%
      ggplot(aes(age, catch)) +
      geom_density(stat = "identity", fill = "tomato") +
      scale_x_continuous(
        name = "Age (Years)",
        breaks = seq(min(agelist), max(agelist), by = 1),
        labels = seq(min(agelist), max(agelist), by = 1)
      ) +
      scale_y_continuous(name = "Biomass Caught (million MT)", limits = c(0, 0.45))
    
    things <- sel_plot  /  bio_plot / catch_plot 
    
    out <- list(lh = lh_plot, things = things)
    return(out)
  }
  
  plot_recruit <- function(u, fish.method, rec.type) {
    unfished <- run.eq(0, fish.method, rec.type)
    ssb0 <- unfished$sbpr * unfished$rinfty
    output <- run.eq(u, fish.method, rec.type)
    sbpr <- output$sbpr
    
    sb.list <- seq(0, ssb0, length.out = 100)
    bha = 957.99
    bhb = 1.75631
    cols <- plasma(5)
    text.mult = 1.25
    if (rec.type == "const"){
      recs <- 500
    } else {
      recs <- sb.list * bha / (1 + bhb * sb.list)
      
    }

    rec_plot <- tibble(sb = sb.list, rec = recs) %>%
      ggplot() +
      geom_hline(aes(yintercept = 0)) +
      geom_line(aes(sb, rec, color = "Spawner-Recruit Curve"), size = 2) +
      geom_abline(
        aes(
          slope = 1 / sbpr,
          intercept = 0,
          color = "Replacement Line"
        ),
        linetype = 2,
        size = 1.5
      ) +
      scale_x_continuous(name = "Spawning biomass (million mt)",
                         limits = c(0, ssb0),
                         expand = expansion(mult = c(0, 0))) +
      scale_y_continuous(name = "Recruits (millions)", limits = c(0, 525)) +
      scale_color_manual(values = c("tomato", "aquamarine"), name = '') +
      theme(legend.position = "top")
    

return(rec_plot)
   
  }
  make_table <- function(u, fish.method, rec.type) {
    
    unfished <- run.eq(0, fish.method, rec.type)
    
    rzero <- unfished$rinfty
    m_output <- run.eq(u, fish.method, rec.type)
    dt <- 0.1
    rec <- m_output$rinfty
    ca <- sum(m_output$ca)
    ba <- sum(m_output$ba)
    sbpr <- m_output$sbpr* rec
    
    eq_data <- tibble(Biomass = ba / sum(unfished$ba), `Spawning Biomass` = m_output$sbpr / unfished$sbpr,
                        `Recruits` = rec /rzero)
    eq_plot <- eq_data %>% 
      pivot_longer(everything(), names_to = "metric", values_to = "value") %>% 
      ggplot() +
      geom_col(aes(metric,value, fill = value), alpha = 0.75) +
      geom_col(aes(metric,1), color = 'black', fill = 'transparent') + 
      scale_fill_gradient2(guide = "none", low = 'red1', mid = 'seashell', high = 'deepskyblue4', midpoint = 0.5, limits = c(0,1)) + 
      scale_x_discrete(name = "") + 
      scale_y_continuous(labels = scales::percent, name = "Percent of Unfished Level")
      
    table_dat <- as.data.frame(matrix(NA, nrow = 4, ncol =2))
    table_dat[,1]<-   c("Recuitment (millions)", "Catch (million mt)", "Total Biomass (million mt)", "Spawning Biomass (million mt)")
    table_dat[,2] <- c(round(rec,0), 
                       as.character(round(ca,3)), 
                       round(ba, 2), 
                       round(rec * sbpr/1000,2))
    names(table_dat) <- c("Metric", "Equilibrium Value")
    
  
    return(list(table_dat = table_dat,eq_plot = eq_plot ))
  }
  catch_vs_f <- function(fishing.method, rec.type) {
    u.list <- seq(0,1, length.out = 50)
    c.list <- rep(0, 50)
    for (i in 1:50) c.list[i] <- sum(run.eq(u.list[i], fishing.method, rec.type)$ca)
    plot(u.list, c.list,
         type = "l",
         lwd = 3,
         xlab = "Fishing Mortality Rate (F)",
         ylab = "Catch (million mt)"
    )
  }
    
  output$age_eq_plot <- renderPlot({plot_equil(input$u, input$fishing.method, input$rec.type)$things},
                                   width = 500,
                                   height = 600
                                  )
  
  output$lh <- renderPlot({plot_equil(input$u, input$fishing.method, input$rec.type)$lh},
                                   width = 600,
                                   height = 600
  )
  
  output$rec_plot <- renderPlot({plot_recruit(input$u, input$fishing.method, input$rec.type)},
                                width = 500,
                                height = 400
  )
  output$catch_plot <- renderPlot({catch_vs_f(input$fishing.method, input$rec.type)},
                                width = 500,
                                height = 400
  )
  output$eq_table <- renderTable({make_table(input$u, input$fishing.method, input$rec.type)$table_dat})
  
  output$eq_plot <- renderPlot({make_table(input$u, input$fishing.method, input$rec.type)$eq_plot})
  
})
