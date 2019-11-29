
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
  
  showModal(modalDialog(HTML(paste0("Creating relative PFG abundance maps with :
                                        <ul>
                                    <li><strong>folder :</strong> ", basename(get_name.simul()),"</li>
                                    <li><strong>simulation parameter file :</strong> ", basename(get_param.simul()), "</li>
                                    <li><strong>year :</strong> ", as.numeric(input$graph.year),"</li>
                                    <li><strong>opt.no_CPU :</strong> ", input$graph.opt.no_CPU,"</li>
                                    </ul>"))
                        , title = HTML("Relative PFG abundance maps (specific year)")
                        , footer = NULL))
  Sys.sleep(3)
  get_res = print_messages(as.expression(
    POST_FATE.relativeAbund(name.simulation = get_name.simul()
                            , file.simulParam = get_param.simul()
                            , year = as.numeric(input$graph.year)
                            , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  removeModal()

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
  
  showModal(modalDialog(HTML(paste0("Creating statistics for quality assessment (sensitivity, specificity, TSS, AUC) with :
                                        <ul>
                                    <li><strong>folder :</strong> ", basename(get_name.simul()),"</li>
                                    <li><strong>simulation parameter file :</strong> ", basename(get_param.simul()), "</li>
                                    <li><strong>year :</strong> ", as.numeric(input$graph.year),"</li>
                                    <li><strong>mat.PFG.obs :</strong> ", is.data.frame(graph.mat.PFG.obs),"</li>
                                    <li><strong>opt.ras_habitat :</strong> ", graph.opt.cover.obs,"</li>
                                    <li><strong>opt.no_CPU :</strong> ", input$graph.opt.no_CPU,"</li>
                                    </ul>"))
                        , title = HTML("Statistics for quality assessment (specific year)")
                        , footer = NULL))
  Sys.sleep(3)
  get_res = print_messages(as.expression(
    POST_FATE.graphic_validationStatistics(name.simulation = get_name.simul()
                                           , file.simulParam = get_param.simul()
                                           , year = as.numeric(input$graph.year)
                                           , mat.PFG.obs = graph.mat.PFG.obs
                                           , opt.ras_habitat = graph.opt.cover.obs
                                           , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  removeModal()
  
  output$plot.validationStat = renderPlot({
    plot(get_res[[1]]$plot[[1]][['ALL']])
  })
  
  shinyjs::hide("plot.PFGvsHS")
  shinyjs::show("plot.validationStat")
  shinyjs::hide("plot.PFGrichness")
  shinyjs::hide("plot.PFGcover")
  shinyjs::hide("plot.PFGlight")
  shinyjs::hide("plot.PFGsoil")
  
  setwd(path.init)
})

####################################################################

observeEvent(input$create.PFGvsHS, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  showModal(modalDialog(HTML(paste0("Creating maps of PFG habitat suitability vs simulated occurrences with :
                                        <ul>
                                    <li><strong>folder :</strong> ", basename(get_name.simul()),"</li>
                                    <li><strong>simulation parameter file :</strong> ", basename(get_param.simul()), "</li>
                                    <li><strong>year :</strong> ", as.numeric(input$graph.year),"</li>
                                    <li><strong>opt.strata :</strong> ", as.numeric(input$graph.strata_min),"</li>
                                    <li><strong>opt.no_CPU :</strong> ", input$graph.opt.no_CPU,"</li>
                                    </ul>"))
                        , title = HTML("Maps of PFG habitat suitability vs simulated occurrences (specific year)")
                        , footer = NULL))
  Sys.sleep(3)
  get_res = print_messages(as.expression(
    POST_FATE.graphic_mapPFGvsHS(name.simulation = get_name.simul()
                                 , file.simulParam = get_param.simul()
                                 , year = as.numeric(input$graph.year)
                                 , opt.strata = as.numeric(input$graph.strata_min)
                                 , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  removeModal()
  
  output$plot.PFGvsHS = renderPlotly({
    plot(get_res[[1]]$plot[[1]][[1]])
  })
  
  shinyjs::show("plot.PFGvsHS")
  shinyjs::hide("plot.validationStat")
  shinyjs::hide("plot.PFGrichness")
  shinyjs::hide("plot.PFGcover")
  shinyjs::hide("plot.PFGlight")
  shinyjs::hide("plot.PFGsoil")

  setwd(path.init)
})

####################################################################

observeEvent(input$create.PFGrichness, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  showModal(modalDialog(HTML(paste0("Creating map of PFG richness with :
                                        <ul>
                                    <li><strong>folder :</strong> ", basename(get_name.simul()),"</li>
                                    <li><strong>simulation parameter file :</strong> ", basename(get_param.simul()), "</li>
                                    <li><strong>year :</strong> ", as.numeric(input$graph.year),"</li>
                                    <li><strong>opt.no_CPU :</strong> ", input$graph.opt.no_CPU,"</li>
                                    </ul>"))
                        , title = HTML("Map of PFG richness (specific year)")
                        , footer = NULL))
  Sys.sleep(3)
  get_res = print_messages(as.expression(
    POST_FATE.graphic_mapPFGrichness(name.simulation = get_name.simul()
                                     , file.simulParam = get_param.simul()
                                     , year = as.numeric(input$graph.year)
                                     , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  removeModal()
  
  output$plot.PFGrichness = renderPlotly({
    plot(get_res[[1]][[1]][[2]])
  })
  
  shinyjs::hide("plot.PFGvsHS")
  shinyjs::hide("plot.validationStat")
  shinyjs::show("plot.PFGrichness")
  shinyjs::hide("plot.PFGcover")
  shinyjs::hide("plot.PFGlight")
  shinyjs::hide("plot.PFGsoil")

  setwd(path.init)
})

####################################################################

observeEvent(input$create.PFGcover, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  showModal(modalDialog(HTML(paste0("Creating map of PFG cover with :
                                        <ul>
                                    <li><strong>folder :</strong> ", basename(get_name.simul()),"</li>
                                    <li><strong>simulation parameter file :</strong> ", basename(get_param.simul()), "</li>
                                    <li><strong>year :</strong> ", as.numeric(input$graph.year),"</li>
                                    <li><strong>strata_min :</strong> ", as.numeric(input$graph.strata_min),"</li>
                                    <li><strong>opt.no_CPU :</strong> ", input$graph.opt.no_CPU,"</li>
                                    </ul>"))
                        , title = HTML("Map of PFG cover (specific year)")
                        , footer = NULL))
  Sys.sleep(3)
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
  removeModal()
  
  output$plot.PFGcover = renderPlotly({
    plot(get_res[[1]]$plot[[1]])
  })
  
  shinyjs::hide("plot.PFGvsHS")
  shinyjs::hide("plot.validationStat")
  shinyjs::hide("plot.PFGrichness")
  shinyjs::show("plot.PFGcover")
  shinyjs::hide("plot.PFGlight")
  shinyjs::hide("plot.PFGsoil")
  
  setwd(path.init)
})
