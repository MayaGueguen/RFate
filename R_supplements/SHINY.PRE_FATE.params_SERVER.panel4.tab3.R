
####################################################################

observeEvent(input$show.specific_year, {
  if (input$show.specific_year == "PFG vs Habsuit")
  {
    shinyjs::show("panel.PFGvsHS")
    shinyjs::hide("panel.validationStat")
    shinyjs::hide("panel.PFGrichness")
    shinyjs::hide("panel.PFGcover")
    shinyjs::hide("panel.PFGlight")
    shinyjs::hide("panel.PFGsoil")
  } else if (input$show.specific_year == "Validation stat")
  {
    shinyjs::hide("panel.PFGvsHS")
    shinyjs::show("panel.validationStat")
    shinyjs::hide("panel.PFGrichness")
    shinyjs::hide("panel.PFGcover")
    shinyjs::hide("panel.PFGlight")
    shinyjs::hide("panel.PFGsoil")
  } else if (input$show.specific_year == "PFG richness")
  {
    shinyjs::hide("panel.PFGvsHS")
    shinyjs::hide("panel.validationStat")
    shinyjs::show("panel.PFGrichness")
    shinyjs::hide("panel.PFGcover")
    shinyjs::hide("panel.PFGlight")
    shinyjs::hide("panel.PFGsoil")
  } else if (input$show.specific_year == "PFG cover")
  {
    shinyjs::hide("panel.PFGvsHS")
    shinyjs::hide("panel.validationStat")
    shinyjs::hide("panel.PFGrichness")
    shinyjs::show("panel.PFGcover")
    shinyjs::hide("panel.PFGlight")
    shinyjs::hide("panel.PFGsoil")
  } else if (input$show.specific_year == "Light CWM (MAP)")
  {
    shinyjs::hide("panel.PFGvsHS")
    shinyjs::hide("panel.validationStat")
    shinyjs::hide("panel.PFGrichness")
    shinyjs::hide("panel.PFGcover")
    shinyjs::show("panel.PFGlight")
    shinyjs::hide("panel.PFGsoil")
  } else if (input$show.specific_year == "Soil CWM (MAP)")
  {
    shinyjs::hide("panel.PFGvsHS")
    shinyjs::hide("panel.validationStat")
    shinyjs::hide("panel.PFGrichness")
    shinyjs::hide("panel.PFGcover")
    shinyjs::hide("panel.PFGlight")
    shinyjs::show("panel.PFGsoil")
  }
})


####################################################################

output$UI.graph.mat.PFG.obs = renderUI({
  fileInput(inputId = "graph.mat.PFG.obs"
            , label = NULL
            , buttonLabel = param.style("mat.PFG.obs")
            , multiple = FALSE
            , width = "100%")
})

observeEvent(input$graph.mat.PFG.obs.delete, {
  output$UI.graph.mat.PFG.obs = renderUI({
    fileInput(inputId = "graph.mat.PFG.obs"
              , label = NULL
              , buttonLabel = param.style("mat.PFG.obs")
              , multiple = FALSE
              , width = "100%")
  })
})

####################################################################

output$UI.graph.opt.cover.obs = renderUI({
  fileInput(inputId = "graph.opt.cover.obs"
              , label = NULL
              , buttonLabel = param.style("opt.cover.obs")
              , multiple = FALSE
              , width = "100%")
})

observeEvent(input$graph.opt.cover.obs.delete, {
  output$UI.graph.opt.cover.obs = renderUI({
    fileInput(inputId = "graph.opt.cover.obs"
              , label = NULL
              , buttonLabel = param.style("opt.cover.obs")
              , multiple = FALSE
              , width = "100%")
  })
})
  
####################################################################

observeEvent(input$create.relativeAbund, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.relativeAbund(name.simulation = get_name.simul()
                            , file.simulParam = get_param.simul()
                            , year = as.numeric(input$graph.year)
                            , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  setwd(path.init)
})


####################################################################

observeEvent(input$create.PFGvsHS, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_mapPFGvsHS(name.simulation = get_name.simul()
                                 , file.simulParam = get_param.simul()
                                 , year = as.numeric(input$graph.year)
                                 , opt.strata = as.numeric(input$graph.strata_min)
                                 , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  # plot(get_res[[1]]$plot[[1]][[1]])
  
  output$plot.PFGvsHS = renderPlotly({
    plot(get_res[[1]]$plot[[1]][[1]])
  })
  
  # if(get_res)
  # {
    # col_vec = c('#6da34d', '#297373', '#58a4b0', '#5c4742', '#3f334d')
    # col_fun = colorRampPalette(col_vec)
    # 
    # distri.melt = fread(get_last.createdFiles2(pattern_head = "POST_FATE_evolution_spaceOccupancy_"
    #                                            , pattern_tail = ".csv$"))
    # 
    # distriAbund.melt = fread(get_last.createdFiles2(pattern_head = "POST_FATE_evolution_abundance_"
    #                                                 , pattern_tail = ".csv$"))
    # 
    # output$plot.evolutionCoverage1 = renderPlotly({
    #   
    #   ## Evolution of space occupation
    #   pp1 = ggplot(distri.melt, aes_string(x = "YEAR", y = "Abund * 100", color = "factor(HAB)")) +
    #     geom_line(lwd = 1) +
    #     facet_wrap("~ PFG") +
    #     scale_color_manual("Habitat", values = col_fun(length(unique(distri.melt$HAB)))) +
    #     labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' space occupation"),
    #          subtitle = paste0("For each PFG, the line represents the evolution through time of its space occupancy,\n",
    #                            "meaning the percentage of pixels in which the abundance of the species is greater than 0.\n")) +
    #     theme_fivethirtyeight() +
    #     theme(panel.background = element_rect(fill = "transparent", colour = NA)
    #           , plot.background = element_rect(fill = "transparent", colour = NA)
    #           , legend.background = element_rect(fill = "transparent", colour = NA)
    #           , legend.box.background = element_rect(fill = "transparent", colour = NA)
    #           , legend.key = element_rect(fill = "transparent", colour = NA))
    #   
    #   print(pp1)
    # })
  # }
  setwd(path.init)
})

####################################################################

observeEvent(input$create.validationStat, {

  path.init = getwd()
  setwd(get_path.folder())
  
  if (is.data.frame(input$graph.mat.PFG.obs))
  {
    graph.mat.PFG.obs = fread(input$graph.mat.PFG.obs$datapath)
  } else
  {
    graph.mat.PFG.obs = ""
  }
  if (is.data.frame(input$graph.opt.cover.obs))
  {
    graph.opt.cover.obs = input$graph.mat.PFG.obs$datapath
  } else
  {
    graph.opt.cover.obs = ""
  }
  

  get_res = print_messages(as.expression(
    POST_FATE.graphic_validationStatistics(name.simulation = get_name.simul()
                                           , file.simulParam = get_param.simul()
                                           , year = as.numeric(input$graph.year)
                                           , mat.PFG.obs = graph.mat.PFG.obs
                                           , opt.ras_habitat = graph.opt.cover.obs
                                           , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  output$plot.validationStat = renderPlot({
    plot(get_res[[1]]$plot[[1]][['ALL']])
  })
  
  setwd(path.init)
})

####################################################################

observeEvent(input$create.PFGrichness, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_mapPFGrichness(name.simulation = get_name.simul()
                                     , file.simulParam = get_param.simul()
                                     , year = as.numeric(input$graph.year)
                                     , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  output$plot.PFGrichness = renderPlotly({
    plot(get_res[[1]][[1]][[2]])
  })

  setwd(path.init)
})

####################################################################

observeEvent(input$create.PFGcover, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_mapPFGcover(name.simulation = get_name.simul()
                                 , file.simulParam = get_param.simul()
                                 , year = as.numeric(input$graph.year)
                                 , strata_min = as.numeric(input$graph.strata_min)
                                 , opt.no_CPU = input$graph.opt.no_CPU
                                 , opt.mat.cover.obs = NULL
                                 , opt.ras.cover.obs = NULL
    )
  ))
  
  output$plot.PFGcover = renderPlotly({
    plot(get_res[[1]]$plot[[1]])
  })
  
  setwd(path.init)
})
