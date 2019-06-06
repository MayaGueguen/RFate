
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
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_evolutionCoverage(name.simulation = get_name.simul()
                                        , file.simulParam = get_param.simul()
                                        , no.years = input$graph.no.years
                                        , opt.abund_fixedScale = input$graph.opt.fixedScale
                                        , opt.ras_habitat = input$graph.opt.ras_habitat
                                        , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  output$plot.evolutionCoverage1 = renderPlotly({
    plot(get_res[[1]]$graph.spaceOccupancy)
  })
  
  output$plot.evolutionCoverage2 = renderPlotly({
    plot(get_res[[1]]$graph.abundance)
  })
  
  setwd(path.init)
})


####################################################################

observeEvent(input$create.evolutionAbund, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_evolutionAbund_pixels(name.simulation = get_name.simul()
                                            , file.simulParam = get_param.simul()
                                            , no.years = input$graph.no.years
                                            , opt.abund_fixedScale = input$graph.opt.fixedScale
                                            , opt.cells_ID = NULL
                                            , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  output$plot.evolutionAbund = renderPlotly({
    plot(get_res[[1]]$plot)
  })
  
  setwd(path.init)
})

####################################################################

observeEvent(input$create.evolutionLight, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_evolutionLight_pixels(name.simulation = get_name.simul()
                                            , file.simulParam = get_param.simul()
                                            , no.years = input$graph.no.years
                                            , opt.abund_fixedScale = input$graph.opt.fixedScale
                                            , opt.cells_ID = NULL
                                            , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  output$plot.evolutionLight = renderPlotly({
    plot(get_res[[1]]$plot)
  })
  
  setwd(path.init)
})

####################################################################

observeEvent(input$create.evolutionSoil, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_evolutionSoil_pixels(name.simulation = get_name.simul()
                                           , file.simulParam = get_param.simul()
                                           , no.years = input$graph.no.years
                                           , opt.cells_ID = NULL
                                           , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  output$plot.evolutionSoil = renderPlotly({
    plot(get_res[[1]]$plot)
  })
  
  setwd(path.init)
})

