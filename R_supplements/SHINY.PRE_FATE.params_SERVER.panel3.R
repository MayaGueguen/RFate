

####################################################################

observeEvent(input$folder.simul, {
  if (input$folder.simul > 0) {
    path = choose.dir(default = readDirectoryInput(session, 'folder.simul'))
    updateDirectoryInput(session, 'folder.simul', value = path)
    
    names.simulParam = list.files(path = paste0(path, "/PARAM_SIMUL")
                                  , pattern = ".txt$"
                                  , all.files = FALSE
                                  , full.names = TRUE)
    if (length(names.simulParam) > 0)
    {
      updateSelectInput(session
                        , inputId = "graph.simulParam"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      shinyjs::enable("graph.simulParam")
      shinyjs::enable("show.evolutionCoverage")
      shinyjs::enable("show.evolutionAbund")
      shinyjs::enable("show.PFGvsHS")
      shinyjs::enable("show.validationStat")
      shinyjs::enable("show.PFGrichness")
      shinyjs::enable("show.PFGcover")
    } else
    {
      shinyjs::reset("graph.simulParam")
      shinyjs::disable("graph.simulParam")
      shinyjs::disable("show.evolutionCoverage")
      shinyjs::disable("show.evolutionAbund")
      shinyjs::disable("show.evolutionLight")
      shinyjs::disable("show.evolutionSoil")
      shinyjs::disable("show.PFGvsHS")
      shinyjs::disable("show.validationStat")
      shinyjs::disable("show.PFGrichness")
      shinyjs::disable("show.PFGcover")
      shinyjs::disable("show.PFGlight")
      shinyjs::disable("show.PFGsoil")
    }
    return(path)
  } else
  {
    shinyjs::reset("graph.simulParam")
    shinyjs::disable("graph.simulParam")
    shinyjs::disable("show.evolutionCoverage")
    shinyjs::disable("show.evolutionAbund")
    shinyjs::disable("show.evolutionLight")
    shinyjs::disable("show.evolutionSoil")
    shinyjs::disable("show.PFGvsHS")
    shinyjs::disable("show.validationStat")
    shinyjs::disable("show.PFGrichness")
    shinyjs::disable("show.PFGcover")
    shinyjs::disable("show.PFGlight")
    shinyjs::disable("show.PFGsoil")
  }
})

####################################################################

output$show.evolutionCoverage = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.evolutionCoverage"
                 , label = "Abundance & coverage"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

output$show.evolutionAbund = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.evolutionAbund"
                 , label = "Abundance (PIXELS)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

output$show.evolutionLight = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.evolutionLight"
                 , label = "Light (PIXELS)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

output$show.evolutionSoil = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.evolutionSoil"
                 , label = "Soil (PIXELS)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

####################################################################

output$show.PFGvsHS = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.PFGvsHS"
                 , label = "PFG vs Habsuit"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

output$show.validationStat = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.validationStat"
                 , label = "Validation statistics"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

output$show.PFGrichness = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.PFGrichness"
                 , label = "PFG richness"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

output$show.PFGcover = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.PFGcover"
                 , label = "PFG cover"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

output$show.PFGlight = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.PFGlight"
                 , label = "Light (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

output$show.PFGsoil = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.PFGsoil"
                 , label = "Soil (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

####################################################################

observeEvent(input$graph.simulParam, {
  if (nchar(input$graph.simulParam) > 0)
  {
    file.globalParam = .getParam(params.lines = input$graph.simulParam
                                 , flag = "GLOBAL_PARAMS"
                                 , flag.split = "^--.*--$"
                                 , is.num = FALSE)
    file.globalParam = paste0(dirname(sub("/PARAM_SIMUL", "", dirname(input$graph.simulParam)))
                              , "/", file.globalParam)
    
    ## -------------------------------------------------------------
    doLight = doSoil = FALSE
    if (file.exists(file.globalParam))
    {
      doLight = .getParam(params.lines = file.globalParam
                          , flag = "DO_LIGHT_COMPETITION"
                          , flag.split = " "
                          , is.num = TRUE)
      doSoil = .getParam(params.lines = file.globalParam
                         , flag = "DO_SOIL_COMPETITION"
                         , flag.split = " "
                         , is.num = TRUE)
    }
    
    if (doLight)
    {
      shinyjs::enable("show.evolutionLight")
      shinyjs::enable("show.PFGlight")
    } else
    {
      shinyjs::disable("show.evolutionLight")
      shinyjs::disable("show.PFGlight")
    }
    
    if (doSoil)
    {
      shinyjs::enable("show.evolutionSoil")
      shinyjs::enable("show.PFGsoil")
    } else
    {
      shinyjs::disable("show.evolutionSoil")
      shinyjs::disable("show.PFGsoil")
    }
    
    ## -------------------------------------------------------------
    dir.save = .getParam(params.lines = input$graph.simulParam
                                 , flag = "SAVE_DIR"
                                 , flag.split = "^--.*--$"
                                 , is.num = FALSE)
    dir.save = paste0(dirname(sub("/PARAM_SIMUL", "", dirname(input$graph.simulParam)))
                              , "/", dir.save)
    
    ## -------------------------------------------------------------    
    years.available = list.files(paste0(dir.save, "/ABUND_perPFG_allStrata"))
    years.available = sapply(sub("Abund_YEAR_", "", years.available)
                             , function(x) strsplit(as.character(x), "_")[[1]][1])
    years.available = rev(sort(unique(as.numeric(years.available))))
    
    if (length(years.available) > 0)
    {
      updateSelectInput(session
                        , inputId = "graph.year"
                        , choices = years.available
                        , selected = max(years.available))
      shinyjs::enable("graph.year")
    } else
    {
      shinyjs::reset("graph.year")
      shinyjs::disable("graph.year")
    }
    
    ## -------------------------------------------------------------    
    strata.available = list.files(paste0(dir.save, "/ABUND_perPFG_perStrata"))
    strata.available = sapply(sub(".*_STRATA_", "", strata.available)
                              , function(x) strsplit(as.character(x), "[.]")[[1]][1])
    strata.available = sort(unique(as.numeric(strata.available)))
    
    if (length(strata.available) > 0)
    {
      updateSelectInput(session
                        , inputId = "graph.strata_min"
                        , choices = strata.available
                        , selected = min(strata.available))
      shinyjs::enable("graph.strata_min")
    } else
    {
      shinyjs::reset("graph.strata_min")
      shinyjs::disable("graph.strata_min")
    }
  }
})
