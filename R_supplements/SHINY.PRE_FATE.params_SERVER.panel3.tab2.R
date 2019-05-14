
####################################################################

observeEvent(input$show.through_time, {
  if (input$show.through_time == "Abundance & coverage")
  {
    shinyjs::show("panel.evolutionCoverage")
    shinyjs::hide("panel.evolutionAbund")
    shinyjs::hide("panel.evolutionLight")
    shinyjs::hide("panel.evolutionSoil")
  } else if (input$show.through_time == "Abundance (PIXELS)")
  {
    shinyjs::hide("panel.evolutionCoverage")
    shinyjs::show("panel.evolutionAbund")
    shinyjs::hide("panel.evolutionLight")
    shinyjs::hide("panel.evolutionSoil")
  } else if (input$show.through_time == "Light (PIXELS)")
  {
    shinyjs::hide("panel.evolutionCoverage")
    shinyjs::hide("panel.evolutionAbund")
    shinyjs::show("panel.evolutionLight")
    shinyjs::hide("panel.evolutionSoil")
  } else if (input$show.through_time == "Soil (PIXELS)")
  {
    shinyjs::hide("panel.evolutionCoverage")
    shinyjs::hide("panel.evolutionAbund")
    shinyjs::hide("panel.evolutionLight")
    shinyjs::show("panel.evolutionSoil")
  }
})

####################################################################

observeEvent(input$create.evolutionCoverage, {
  
  withBusyIndicatorServer("create.evolutionCoverage", {
    path.init = getwd()
    setwd(get_path.folder())
    
    get_res = print_messages(as.expression(
      POST_FATE.graphic_evolutionCoverage(name.simulation = get_name.simul()
                                          , file.simulParam = input$graph.simulParam
                                          , no.years = input$graph.no.years
                                          , opt.abund_fixedScale = input$graph.opt.fixedScale
                                          , opt.ras_habitat = input$graph.opt.ras_habitat
                                          , opt.no_CPU = input$graph.opt.no_CPU
      )
    ))
    
    output$plot.evolutionCoverage1 = renderPlot({
      plot(get_res[[1]]$graph.spaceOccupancy)
    })
    
    output$plot.evolutionCoverage2 = renderPlot({
      plot(get_res[[1]]$graph.abundance)
    })
    
    setwd(path.init)
  })
})

####################################################################

observeEvent(input$create.evolutionAbund, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_evolutionAbund_pixels(name.simulation = get_name.simul()
                                            , file.simulParam = input$graph.simulParam
                                            , no.years = input$graph.no.years
                                            , opt.abund_fixedScale = input$graph.opt.fixedScale
                                            , opt.cells_ID = NULL
                                            , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  if(get_res)
  {
    vec_col = c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c'
                , '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00'
                , '#cab2d6', '#6a3d9a', '#ffff99', '#b15928')
    fun_col = colorRampPalette(vec_col)
    
    distriAbund = fread(get_last.createdFiles2(pattern_head = "POST_FATE_evolution_abundance_pixels_"
                                               , pattern_tail = ".csv$"))
    
    output$plot.evolutionAbund = renderPlot({
      
      ## Evolution of abundance
      pp = ggplot(distriAbund, aes_string(x = "YEAR", y = "Abund", color = "PFG")) +
        scale_color_manual("", values = fun_col(length(unique(distriAbund$PFG)))) +
        geom_line() +
        facet_grid("TYPE ~ ID", scales = ifelse(input$graph.opt.fixedScale, "fixed", "free_y")) +
        labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' abundance"),
             subtitle = paste0("For each PFG, the line represents the evolution through time of its abundance\n",
                               "for 5 randomly selected pixels within the studied area.\n")) +
        theme_fivethirtyeight() +
        theme(panel.background = element_rect(fill = "transparent", colour = NA)
              , plot.background = element_rect(fill = "transparent", colour = NA)
              , legend.background = element_rect(fill = "transparent", colour = NA)
              , legend.box.background = element_rect(fill = "transparent", colour = NA)
              , legend.key = element_rect(fill = "transparent", colour = NA))
      
      print(pp)
    })
  }
  setwd(path.init)
})

####################################################################

observeEvent(input$create.evolutionLight, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_evolutionLight_pixels(name.simulation = get_name.simul()
                                            , file.simulParam = input$graph.simulParam
                                            , no.years = input$graph.no.years
                                            , opt.abund_fixedScale = input$graph.opt.fixedScale
                                            , opt.cells_ID = NULL
                                            , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  if(get_res)
  {
    vec_col = c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c'
                , '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00'
                , '#cab2d6', '#6a3d9a', '#ffff99', '#b15928')
    fun_col = colorRampPalette(vec_col)
    
    distriAbund = fread(get_last.createdFiles2(pattern_head = "POST_FATE_evolution_light_pixels_"
                                               , pattern_tail = ".csv$"))
    
    output$plot.evolutionLight = renderPlot({
      
      no_strata = sub("STRATUM_", "", unique(distriAbund$STRATUM))
      no_strata = max(as.numeric(no_strata), na.rm = TRUE)
      no_strata = max(1, no_strata)
      
      ## Evolution of abundance
      pp = ggplot(distriAbund, aes_string(x = "YEAR", y = "Abund", color = "STRATUM")) +
        scale_color_manual("", values = fun_col(no_strata)) +
        geom_line() + ## lwd = 0.8 ?
        facet_grid("TYPE ~ ID", scales = ifelse(input$graph.opt.fixedScale, "fixed", "free_y")) +
        labs(x = "", y = "", title = paste0("GRAPH B : evolution of light resources"),
             subtitle = paste0("For each stratum, the line represents the evolution through time of its light resources\n",
                               "for 5 randomly selected pixels within the studied area.\n")) +
        theme_fivethirtyeight() +
        theme(panel.background = element_rect(fill = "transparent", colour = NA)
              , plot.background = element_rect(fill = "transparent", colour = NA)
              , legend.background = element_rect(fill = "transparent", colour = NA)
              , legend.box.background = element_rect(fill = "transparent", colour = NA)
              , legend.key = element_rect(fill = "transparent", colour = NA))
      
      print(pp)
    })
  }
  setwd(path.init)
})

####################################################################

observeEvent(input$create.evolutionSoil, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_evolutionSoil_pixels(name.simulation = get_name.simul()
                                           , file.simulParam = input$graph.simulParam
                                           , no.years = input$graph.no.years
                                           , opt.cells_ID = NULL
                                           , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  if(get_res)
  {
    vec_col = c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c'
                , '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00'
                , '#cab2d6', '#6a3d9a', '#ffff99', '#b15928')
    fun_col = colorRampPalette(vec_col)
    
    distriSoil = fread(get_last.createdFiles2(pattern_head = "POST_FATE_evolution_soil_pixels_"
                                              , pattern_tail = ".csv$"))
    
    # abund.file = get_last.createdFiles2(pattern_head = "POST_FATE_evolution_abundance_pixels_"
    #                                     , pattern_tail = ".csv$")
    # abund.file = NULL ## TODO
    # 
    addAbund = FALSE
    # if (!is.null(abund.file) && file.exists(abund.file))
    # {
    #   distriAbund = read.csv(abund.file, header = TRUE, sep = ",")
    #   distriSoil = merge(distriSoil, distriAbund, by = c("YEAR", "ID"), all.x = TRUE)
    #   distriSoil$PFG_presence = ifelse(distriSoil$Abund > 0, TRUE, FALSE)
    #   distriSoil = na.exclude(distriSoil)
    #   addAbund = TRUE
    # }
    
    output$plot.evolutionSoil = renderPlot({
      
      ## Evolution of abundance
      pp = ggplot(distriSoil, aes_string(x = "YEAR", y = "SOIL"))
      
      if (addAbund)
      {
        pp = pp +
          geom_line(aes_string(y = "Abund", color = "PFG"), lwd = 0.4) +
          scale_color_manual("", values = fun_col(length(unique(distriSoil$PFG)))) +
          facet_grid("TYPE ~ ID", scales = "fixed")
      } else
      {
        pp = pp +
          facet_grid(" ~ ID", scales = "fixed")
      }
      pp = pp +
        geom_line(lwd = 0.8) +
        labs(x = "", y = "", title = paste0("GRAPH B : evolution of soil resources"),
             subtitle = paste0("The line represents the evolution through time of the soil resources\n",
                               "for 5 randomly selected pixels within the studied area.\n")) +
        theme_fivethirtyeight() +
        theme(panel.background = element_rect(fill = "transparent", colour = NA)
              , plot.background = element_rect(fill = "transparent", colour = NA)
              , legend.background = element_rect(fill = "transparent", colour = NA)
              , legend.box.background = element_rect(fill = "transparent", colour = NA)
              , legend.key = element_rect(fill = "transparent", colour = NA))
      
      print(pp)
    })
  }
  setwd(path.init)
})

