#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(marlin)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(glue)
library(waiter)
library(stringr)
theme_set(theme_minimal(base_size = 18))
options(dplyr.summarise.inform = FALSE)

shinyServer(function(input, output, session) {
    
    waiter <- waiter::Waiter$new()
    waiter$show()
    
    
    # see https://mastering-shiny.org/action-graphics.html for reactivity with clicking
    res <- resolution <- 20
    
    grid <- expand.grid(x = 1:res, y = 1:res)
    
    grid$patch <- 1:nrow(grid)
    
    grid$mpa <- FALSE
    
    selected <- reactiveVal(rep(FALSE, nrow(grid)))
    
    seasons <- 1
    
    years <- 50
    
    mpa_years <- 15
    
    tune_type <- "explt"
    
    steps <- years * seasons
    
    # for now make up some habitat
    

    yft_habitat <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
        mutate(habitat =  dnorm(x + rnorm(nrow(.)),14,8)) %>% 
        mutate(critter = "Yellowfin")
    
    mako_habitat <- expand_grid(x = 1:resolution, y = 1:resolution) %>%
        mutate(habitat =  dnorm(x + rnorm(nrow(.)),12,8)) %>% 
        mutate(critter = "Mako Shark")
    
    
    skj_habitat <-  expand_grid(x = 1:resolution, y = 1:resolution) %>%
        mutate(habitat = dnorm(y + rnorm(nrow(.)),17,8)) %>% 
        mutate(critter = "Skipjack")
    
    snap_habitat <-  expand_grid(x = 1:resolution, y = 1:resolution) %>%
        mutate(habitat =  dnorm(x * y, mean = 10,sd = 8)) %>% 
        mutate(critter = "Snapper")
    
    habs <- yft_habitat %>% 
        bind_rows(mako_habitat) %>% 
        bind_rows(skj_habitat) %>% 
        bind_rows(snap_habitat) %>% 
        group_by(critter) %>% 
        mutate(habitat = habitat / max(habitat))
    
    yft_habitat <- yft_habitat %>% 
        filter(critter == "Yellowfin") %>% 
        pivot_wider(names_from = "y", values_from = "habitat") %>% 
        select(-x,-critter) %>% 
        as.matrix()
    
    mako_habitat <- mako_habitat %>% 
        filter(critter == "Mako Shark") %>% 
        pivot_wider(names_from = "y", values_from = "habitat") %>% 
        select(-x,-critter) %>% 
        as.matrix()
    
    snap_habitat <- snap_habitat %>% 
        filter(critter == "Snapper") %>% 
        pivot_wider(names_from = "y", values_from = "habitat") %>% 
        select(-x,-critter) %>% 
        as.matrix()
    
    skj_habitat <- skj_habitat %>% 
        filter(critter == "Skipjack") %>% 
        pivot_wider(names_from = "y", values_from = "habitat") %>% 
        select(-x,-critter) %>% 
        as.matrix()
    
    
    # create a fauna object, which is a list of lists
    fauna <- 
        list(
            "Yellowfin" = create_critter(
                scientific_name = "Thunnus albacares",
                seasonal_habitat = yft_habitat, # pass habitat as lists
                recruit_habitat = yft_habitat,
                adult_movement = 0,# the mean number of patches moved by adults
                adult_movement_sigma = 10, # standard deviation of the number of patches moved by adults
                recruit_movement = 0,
                recruit_movement_sigma = 2,
                fished_depletion = .4, # desired equilibrium depletion with fishing (1 = unfished, 0 = extinct),
                rec_form = 3, # recruitment form, where 1 implies local recruitment
                seasons = seasons,
                init_explt = 0.3, 
                explt_type = "f",
                steepness = 0.8,
                r0 = 100000
            ),
            "Mako Shark" = create_critter(
                scientific_name = "Isurus oxyrinchus",
                seasonal_habitat = mako_habitat, # pass habitat as lists
                recruit_habitat = mako_habitat,
                adult_movement = 5,
                adult_movement_sigma = 3,
                fished_depletion = .3,
                rec_form = 1,
                burn_years = 200,
                seasons = seasons,
                init_explt = .09, 
                explt_type = "f",
                fec_form = "pups",
                pups = 2,
                steepness = 0.5,
                r0 = 1000
            ),
            "Skipjack" = create_critter(
                scientific_name = "Katsuwonus pelamis",
                seasonal_habitat = skj_habitat,
                adult_movement = 0,# the mean number of patches moved by adults
                adult_movement_sigma = 4, # standard deviation of the number of patches moved by adults
                recruit_movement = 0,
                recruit_movement_sigma = 10,
                fished_depletion = .6, # desired equilibrium depletion with fishing (1 = unfished, 0 = extinct),
                rec_form = 3, # recruitment form, where 1 implies local recruitment
                seasons = seasons,
                init_explt = 0.5, 
                explt_type = "f",
                steepness = 0.8,
                r0 = 200000
            ),
            "Snapper" = create_critter(
                scientific_name = "Lutjanus griseus",
                seasonal_habitat = snap_habitat,
                recruit_habitat = snap_habitat,
                adult_movement = 0,# the mean number of patches moved by adults
                adult_movement_sigma = 4, # standard deviation of the number of patches moved by adults
                recruit_movement = 0,
                recruit_movement_sigma = 5,
                fished_depletion = .6, # desired equilibrium depletion with fishing (1 = unfished, 0 = extinct),
                rec_form = 2, # recruitment form, where 1 implies local recruitment
                seasons = seasons,
                init_explt = 0.01, 
                explt_type = "f",
                steepness = 0.7,
                r0 = 50000,
                age_mature = 2.5,
                vbk = 0.4
            )
        )
    
    ssb0 <- purrr::map_df(fauna, "ssb0") %>% 
        pivot_longer(everything(),names_to = "critter", values_to = "ssb0")
    
    fleets <- list("longline" = create_fleet(
        list(
            `Yellowfin` = Metier$new(
                critter = fauna$`Yellowfin`,
                price = 100,
                # price per unit weight
                sel_form = "logistic",
                # selectivity form, one of logistic or dome
                sel_start = .3,
                # percentage of length at maturity that selectivity starts
                sel_delta = .1,
                # additional percentage of sel_start where selectivity asymptotes
                catchability = .01,
                # overwritten by tune_fleet but can be set manually here
                p_explt = 1
            ),
            `Mako Shark` = Metier$new(
                critter = fauna$`Mako Shark`,
                price = -1,
                sel_form = "logistic",
                sel_start = .25,
                sel_delta = .01,
                catchability = 0,
                p_explt = 1
            ),
            `Skipjack` = Metier$new(
                critter = fauna$Skipjack,
                price = 0,
                sel_form = "logistic",
                sel_start = 1,
                sel_delta = .01,
                catchability = 0,
                p_explt = .1
            ),
            `Snapper` = Metier$new(
                critter = fauna$Snapper,
                price = 0,
                sel_form = "logistic",
                sel_start = 100,
                sel_delta = .01,
                catchability = 0,
                p_explt = 0
            )
        ),
        mpa_response = "stay",
        base_effort = resolution ^ 2
    ),
    "purse-seine" = create_fleet(
        list(
            `Yellowfin` = Metier$new(
                critter = fauna$`Yellowfin`,
                price = 20,
                # price per unit weight
                sel_form = "logistic",
                # selectivity form, one of logistic or dome
                sel_start = .3,
                # percentage of length at maturity that selectivity starts
                sel_delta = .1,
                # additional percentage of sel_start where selectivity asymptotes
                catchability = .01,
                # overwritten by tune_fleet but can be set manually here
                p_explt = 0.5
            ),
            `Mako Shark` = Metier$new(
                critter = fauna$`Mako Shark`,
                price = 0,
                sel_form = "logistic",
                sel_start = 999,
                sel_delta = .01,
                catchability = 0,
                p_explt = 0
            ),
            `Skipjack` = Metier$new(
                critter = fauna$Skipjack,
                price = 10,
                sel_form = "logistic",
                sel_start = .75,
                sel_delta = .01,
                catchability = 0,
                p_explt = 1
            ),
            `Snapper` = Metier$new(
                critter = fauna$Snapper,
                price = 0,
                sel_form = "logistic",
                sel_start = 999,
                sel_delta = .01,
                catchability = 0,
                p_explt = 0
            )
        ),
        mpa_response = "stay",
        base_effort = resolution ^ 2
    ),
    "artisanal" = create_fleet(
        list(
            `Yellowfin` = Metier$new(
                critter = fauna$`Yellowfin`,
                price = 10,
                # price per unit weight
                sel_form = "logistic",
                # selectivity form, one of logistic or dome
                sel_start = .3,
                # percentage of length at maturity that selectivity starts
                sel_delta = .1,
                # additional percentage of sel_start where selectivity asymptotes
                catchability = .01,
                # overwritten by tune_fleet but can be set manually here
                p_explt = .1
            ),
            `Mako Shark` = Metier$new(
                critter = fauna$`Mako Shark`,
                price = 5,
                sel_form = "logistic",
                sel_start = .1,
                sel_delta = .01,
                catchability = 0,
                p_explt = 0
            ),
            `Skipjack` = Metier$new(
                critter = fauna$Skipjack,
                price = 0,
                sel_form = "logistic",
                sel_start = .1,
                sel_delta = .01,
                catchability = 0,
                p_explt = 0
            ),
            `Snapper` = Metier$new(
                critter = fauna$Snapper,
                price = 20,
                sel_form = "logistic",
                sel_start = 0.6,
                sel_delta = .01,
                catchability = 0,
                p_explt = 1
            )
        ),
        mpa_response = "stay",
        base_effort = resolution ^ 2 / 2
    ))
    
    
    fleets <- tune_fleets(fauna, fleets, tune_type = tune_type) # tunes the catchability by fleet to achieve target depletion
    
    starting_conditions <- simmar(fauna = fauna,
                  fleets = fleets,
                  years = years)
    # proc_baseline <- process_marlin(sim = starting_conditions, time_step = time_step, keep_age = FALSE)
    # browser()
    starting_conditions <- starting_conditions[[length(starting_conditions)]]
    # 
    baseline <- simmar(fauna = fauna,
                      fleets = fleets,
                      years = mpa_years,
                      initial_conditions = starting_conditions)
    
   proc_baseline <- process_marlin(sim = baseline, time_step = time_step, keep_age = FALSE)
    
   
   baseline_fleet_results <- proc_baseline$fleets %>% 
       group_by(year, fleet) %>% 
       summarise(catch = sum(catch, na.rm = TRUE),
                 revenue = sum(revenue, na.rm = TRUE)) %>% 
       ungroup() %>% 
       pivot_longer(c(-year,-fleet), values_to = "baseline_value") 

   baseline_bio_results <- proc_baseline$fauna %>% 
       group_by(year, critter) %>% 
       summarise(biomass = sum(b, na.rm = TRUE),
                 spawning_biomass = sum(ssb, na.rm = TRUE),
                 catch = sum(c, na.rm = TRUE)) %>% 
       ungroup() %>% 
       pivot_longer(c(-year,-critter), values_to = "baseline_value") %>% 
       left_join(ssb0, by = "critter") %>% 
       mutate(dep = baseline_value / ssb0)
  
    on.exit(waiter$hide())
   
   
    observeEvent(input$plot_brush, {
        brushed <- brushedPoints(grid, input$plot_brush, allRows = TRUE, xvar = "x", yvar = "y")$selected_
        selected(brushed | selected())
    })
    observeEvent(input$plot_reset, {
        selected(rep(FALSE, nrow(grid)))
    })
    
 
    data <- reactive({
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ","),
               validate("Invalid file; Please upload a .csv file")
        )
    })
    
    
    observeEvent(input$upload,{
        selected(data()$mpa)
    })
    
    output$history <- renderPlot({
        baseline_bio_results %>% 
            filter(name == "spawning_biomass", year == max(year)) %>% 
            ggplot(aes(reorder(critter, dep), dep, fill = critter)) + 
            geom_hline(aes(yintercept = 0)) +
            geom_col(show.legend = FALSE) + 
            scale_y_continuous(name = "Spawning Biomass / Unfished Spawning Biomass", limits = c(0, 1)) + 
            scale_x_discrete(name = '') + 
            theme(axis.title.y = element_text(size = 12))
    })
    
    
    output$habitat <- renderPlot({
        habs %>% 
            ggplot(aes(x,y, fill = habitat)) + 
            geom_tile() + 
            scale_fill_viridis_c(guide = "none") + 
            facet_wrap(~critter) + 
            labs(title = "Habitat") + 
            scale_x_continuous("Longitude-ish") + 
            scale_y_continuous("Latitude-ish") + 
            coord_cartesian(xlim = c(1, resolution), ylim = c(1, resolution), expand = TRUE)
            
    }, height = 500, width = 500
   ) # close plot habitat
    
    output$mpa <- renderPlot({
        
     grid$mpa <- selected()

        mpa_area <- grid %>% 
            ggplot(aes(x,y, fill = mpa)) + 
            geom_tile(color = "black") +
            scale_fill_discrete(name = "", labels = c("Fished","MPA")) + 
            theme(legend.position = "top") + 
            labs(title = glue::glue("{scales::percent(mean(grid$mpa), accuracy = 1)} MPA")) + 
            scale_x_continuous("Longitude-ish") + 
            scale_y_continuous("Latitude-ish") + 
            coord_cartesian(xlim = c(1, resolution), ylim = c(1, resolution), expand = TRUE)
            
            
       
        
        # mpa_bar <-  grid %>% 
        #     summarise(percent_mpa = mean(mpa)) %>% 
        #     ggplot() +
        #     geom_bar(aes("MPA Size",percent_mpa, fill = percent_mpa), stat = "identity") + 
        #     geom_hline(aes(yintercept = 0.3), linetype = 2, color = "red") +
        #     scale_y_continuous(limits = c(0,1), labels = scales::label_percent(accuracy = 1), name = "MPA Size") + 
        #     scale_x_discrete(name = '') +
        #     scale_fill_gradient2(low = "white", high = "tomato", mid = "steelblue", midpoint = 0.3, 
        #                          limits = c(0,1), guide = "none")
            
        
        # (mpa_bar | mpa_area) +   plot_layout(widths = c(1, 3))
        
        mpa_area
    }, height = 500, width = 500)
    
    output$download <- downloadHandler(
        filename = function() {
            paste0(scales::percent(mean(selected())),"_mpa.csv")
        },
        content = function(file) {
            vroom::vroom_write(grid %>% mutate(mpa = selected()), file, delim = ",")
        }
    )
    
    output$progress <- renderText({
        mpa_sim()
        "Done! Check tabs for results"
    })
    
    
    mpa_sim <- eventReactive(input$simulate,{
        # id <- showNotification("Reading data...", duration = NULL, closeButton = FALSE)
        # on.exit(removeNotification(id), add = TRUE)
        # 
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())
        
        grid$mpa <- selected()
 
        
        mpa_sim <- simmar(fauna = fauna,
                      fleets = fleets,
                      years = mpa_years,
                      mpas = list(locations = grid,
                                  mpa_year = 1),
                      initial_conditions = starting_conditions)
        
        
        proc_mpa <- process_marlin(sim = mpa_sim, time_step = time_step, keep_age = FALSE)
        
        
    })
    
    output$cons_results <- renderPlot({
        
        fsh_results <- mpa_sim()
        
        bio_results <- fsh_results$fauna %>% 
            group_by(year, critter) %>% 
            summarise(biomass = sum(b, na.rm = TRUE),
                      spawning_biomass = sum(ssb, na.rm = TRUE)) %>% 
            ungroup() %>% 
            pivot_longer(c(-year,-critter)) %>% 
            left_join(baseline_bio_results, by = c("year", "critter","name")) %>% 
            mutate(percent_change = value / baseline_value - 1,
                   dep = value / ssb0)
        
        
        bio_percent_plot <-  bio_results %>% 
            filter(name == "spawning_biomass") %>% 
            ggplot(aes(year, percent_change, fill = percent_change)) + 
            geom_hline(aes(yintercept = 0), linetype = 2) +
            geom_col(show.legend = FALSE,color = "black") +
            facet_wrap(~critter) + 
            scale_color_gradient2(midpoint = 0) +
            scale_y_continuous(labels = scales::label_percent(accuracy = .2), name = "Change in Spawning Biomass Caused by MPA") + 
            scale_x_continuous(name = "Year") +
            theme(axis.title.y = element_text(size = 12))
        
        
        bio_plot <-  bio_results %>% 
            filter(name == "spawning_biomass") %>% 
            ggplot(aes(year, dep)) + 
            geom_hline(aes(yintercept = 0), linetype = 2) +
            geom_line(size = 2) +
            facet_wrap(~critter) + 
            scale_y_continuous(name = "Spawning Biomass / Unfished Spawning Biomass", limits = c(0,1.1)) + 
            scale_x_continuous(name = "Year") +
            theme(axis.title.y = element_text(size = 12))
        
        
        space_plot <- marlin::plot_marlin(mpa_sim(), max_scale = FALSE, plot_type = "space")
        
        bio_percent_plot / bio_plot / space_plot
        
        
    },width = 600,height = 1200)
    
    output$fish_results <- renderPlot({
        
        fsh_results <- mpa_sim()

        fleet_results <- fsh_results$fleets %>% 
            group_by(year, fleet) %>% 
            summarise(catch = sum(catch, na.rm = TRUE),
                      revenue = sum(revenue, na.rm = TRUE)) %>% 
            ungroup() %>% 
            pivot_longer(c(-year,-fleet)) %>% 
            left_join(baseline_fleet_results, by = c("year", "fleet","name")) %>% 
            mutate(percent_change = value / baseline_value - 1)
            
        
       fleet_percent_plot <-  fleet_results %>% 
            ggplot(aes(year, percent_change, fill = percent_change)) + 
            geom_hline(aes(yintercept = 0), linetype = 2) +
            geom_col(show.legend = FALSE, color = "black") +
          scale_fill_gradient2(midpoint = 0, limits = c(-1, NA)) +
            facet_grid(name~fleet) + 
           scale_y_continuous(labels = scales::label_percent(accuracy = .2), name = "Change Caused by MPA") + 
           scale_x_continuous(name = "Year")
       # fleet_percent_plot <-  fleet_results %>% 
       #     ggplot(aes(year, percent_change)) + 
       #     geom_hline(aes(yintercept = 0), linetype = 2) +
       #     geom_col() +
       #     facet_grid(name~fleet) + 
       #     scale_y_continuous(labels = scales::label_percent(accuracy = .2), name = "Change Caused by MPA") + 
       #     scale_x_continuous(name = "Year")
       #     
       fleet_dist_plot <- fsh_results$fleets %>% 
         filter(year == max(year)) %>% 
         select(x,y,patch,contains("effort")) %>% 
         pivot_longer(contains("_effort"), names_to = "fleet") %>% 
         mutate(fleet = str_remove_all(fleet, "_effort")) %>% 
         group_by(fleet) %>% 
         mutate(value = value / max(value)) %>% 
         ungroup() %>% 
         ggplot(aes(x,y,fill = value)) + 
         geom_tile() +
         scale_fill_viridis_c(guide = "none") + 
         facet_wrap(~fleet) + 
         scale_x_continuous("Longitude-ish") + 
         scale_y_continuous("Latitude-ish") + 
         coord_cartesian(xlim = c(1, resolution), ylim = c(1, resolution), expand = TRUE) + 
         labs(title = "Fishing Effort")
       
       fleet_percent_plot / fleet_dist_plot

       
    },width = 600,height = 1000)
    
    output$objectives <- renderTable({
        
        fsh_results <- mpa_sim()
        
        ssb0 <- purrr::map_df(fauna, "ssb0") %>% 
            pivot_longer(everything(),names_to = "critter", values_to = "ssb0")
        
        fleet_eq_results <- fsh_results$fleets %>% 
            filter(year == max(year)) %>% 
            group_by(year, fleet) %>% 
            summarise(catch = sum(catch, na.rm = TRUE),
                      revenue = sum(revenue, na.rm = TRUE)) %>% 
            ungroup() %>% 
            pivot_longer(c(-year,-fleet)) %>% 
            left_join(baseline_fleet_results, by = c("year", "fleet","name")) %>% 
            mutate(percent_change = value / baseline_value - 1,
                   delta_value = value - baseline_value) %>% 
            group_by(fleet, name) %>% 
            summarise(percent_value = sum(delta_value) / sum(baseline_value))
        
        eq_bio_results <- fsh_results$fauna %>% 
            group_by(year, critter) %>% 
            summarise(spawning_biomass = sum(ssb, na.rm = TRUE)) %>% 
            ungroup() %>% 
            pivot_longer(c(-year,-critter)) %>% 
            left_join(baseline_bio_results, by = c("year", "critter","name")) %>% 
            mutate(dep = value / ssb0, baseline_dep = baseline_value / ssb0) %>% 
            filter(year == max(year)) %>% 
            mutate(percent_change = dep - baseline_dep) 
            
        
        # artisanal
        
        
        artis_econ_obj <- fleet_eq_results %>% 
            filter(fleet == "artisanal") %>% 
            ungroup() %>% 
            summarise(econ = sum(percent_value))
        
        artis_bio_obj <- eq_bio_results %>% 
            filter(critter %in% c("Snapper", "Mako Shark")) %>% 
            summarise(bio = sum(percent_change))
        
        artis_obj <- 0.75 * artis_econ_obj$econ + 0.25 * artis_bio_obj$bio
    
        
   # conservation 
   
        cons_obj <- eq_bio_results %>% 
            summarise(bio = sum(percent_change))
        
   # longline    
        
        longline_econ_obj <- fleet_eq_results %>% 
            filter(fleet == "longline", name == "revenue") %>% 
            ungroup() %>% 
            summarise(econ = sum(percent_value))
        
        longline_obj <- longline_econ_obj$econ
        
    
  # purse seine
          
        ps_econ_obj <- fleet_eq_results %>% 
            filter(fleet == "purse-seine", name == "revenue") %>% 
            ungroup() %>% 
            summarise(econ = sum(percent_value))
        
        ps_obj <- ps_econ_obj$econ
        
    # manager
        
    
        # max possible cons ~ 1.99
        # min possible econ ~ - 1
        # # fishery shutdown would mean conservation is about 2, all fishery is about -3
      

    alpha <-  (0 - (-3)) / (1.97 - (-3))
    
    # x * 2 + (1 - x) * -3
    
    manager_obj <-  alpha * cons_obj$bio +  (1 - alpha) * (longline_obj +  ps_obj + artis_obj) # weight total fishing interests and conservation interests equally 
    
    objectives <- tibble("Managers" = manager_obj,
                         "Conservationists" = cons_obj,
                         "Longline Fleet" = longline_obj,
                         "Purse-seine Fleet" = ps_obj, 
                         "Artisanal Fleet" = artis_obj)
    
    objectives
                  
    })
    
    
})
