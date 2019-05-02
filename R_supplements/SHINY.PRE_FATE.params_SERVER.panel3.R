

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
    } else
    {
      shinyjs::reset("graph.simulParam")
      shinyjs::disable("graph.simulParam")
      shinyjs::disable("show.evolutionCoverage")
      shinyjs::disable("show.evolutionAbund")
      shinyjs::disable("show.evolutionLight")
      shinyjs::disable("show.evolutionSoil")
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

observeEvent(input$graph.simulParam, {
  if (nchar(input$graph.simulParam) > 0)
  {
    file.globalParam = .getParam(params.lines = input$graph.simulParam
                                 , flag = "GLOBAL_PARAMS"
                                 , flag.split = "^--.*--$"
                                 , is.num = FALSE)
    file.globalParam = paste0(dirname(sub("/PARAM_SIMUL", "", dirname(input$graph.simulParam)))
                              , "/", file.globalParam)
    
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
    } else
    {
      shinyjs::disable("show.evolutionLight")
    }
    
    if (doSoil)
    {
      shinyjs::enable("show.evolutionSoil")
    } else
    {
      shinyjs::disable("show.evolutionSoil")
    }
  }
})
