
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
      shinyjs::enable("create.evolutionCoverage")
      shinyjs::enable("create.evolutionAbund")
      shinyjs::enable("create.PFGvsHS")
      shinyjs::enable("create.validationStat")
      shinyjs::enable("create.PFGrichness")
      shinyjs::enable("create.PFGcover")
    } else
    {
      shinyjs::reset("graph.simulParam")
      shinyjs::disable("graph.simulParam")
      shinyjs::disable("create.evolutionCoverage")
      shinyjs::disable("create.evolutionAbund")
      shinyjs::disable("create.evolutionLight")
      shinyjs::disable("create.evolutionSoil")
      shinyjs::disable("create.PFGvsHS")
      shinyjs::disable("create.validationStat")
      shinyjs::disable("create.PFGrichness")
      shinyjs::disable("create.PFGcover")
      shinyjs::disable("create.PFGlight")
      shinyjs::disable("create.PFGsoil")
    }
    return(path)
  } else
  {
    shinyjs::reset("graph.simulParam")
    shinyjs::disable("graph.simulParam")
    shinyjs::disable("create.evolutionCoverage")
    shinyjs::disable("create.evolutionAbund")
    shinyjs::disable("create.evolutionLight")
    shinyjs::disable("create.evolutionSoil")
    shinyjs::disable("create.PFGvsHS")
    shinyjs::disable("create.validationStat")
    shinyjs::disable("create.PFGrichness")
    shinyjs::disable("create.PFGcover")
    shinyjs::disable("create.PFGlight")
    shinyjs::disable("create.PFGsoil")
  }
})

####################################################################

get_path.simul = eventReactive(input$graph.simulParam, {
  return(sub("PARAM_SIMUL", "", dirname(input$graph.simulParam)))
})

get_name.simul = eventReactive(input$graph.simulParam, {
  return(basename(get_path.simul()))
})

get_path.folder = eventReactive(input$graph.simulParam, {
  return(dirname(get_path.simul()))
})

get_last.createdFiles1 = eventReactive(input$graph.simulParam, {
  system(command = paste0("ls -lat "
                          , get_path.simul()
                          , "/RESULTS/"
                          , " | awk '{print $9}'")
         , intern = TRUE)
})

get_last.createdFiles2 = function(pattern_head, pattern_tail)
{
  last.createdFiles = get_last.createdFiles1()
  last.createdFiles = last.createdFiles[grep(pattern = pattern_head, last.createdFiles)]
  last.createdFiles = last.createdFiles[grep(pattern = pattern_tail, last.createdFiles)]
  return(paste0(get_path.simul()
                , "/RESULTS/"
                , last.createdFiles[1]))
}

####################################################################

get_globalParam = eventReactive(input$graph.simulParam, {
  if (nchar(input$graph.simulParam) > 0)
  {
    shinyjs::enable("create.evolutionCoverage")
    shinyjs::enable("create.evolutionAbund")
    shinyjs::enable("create.PFGvsHS")
    shinyjs::enable("create.validationStat")
    shinyjs::enable("create.PFGrichness")
    shinyjs::enable("create.PFGcover")
    
    file.globalParam = .getParam(params.lines = input$graph.simulParam
                                 , flag = "GLOBAL_PARAMS"
                                 , flag.split = "^--.*--$"
                                 , is.num = FALSE)
    file.globalParam = paste0(get_path.folder(), "/", file.globalParam)
    file.globalParam
  } else
  {
    shinyjs::disable("create.evolutionCoverage")
    shinyjs::disable("create.evolutionAbund")
    shinyjs::disable("create.evolutionLight")
    shinyjs::disable("create.evolutionSoil")
    shinyjs::disable("create.PFGvsHS")
    shinyjs::disable("create.validationStat")
    shinyjs::disable("create.PFGrichness")
    shinyjs::disable("create.PFGcover")
    shinyjs::disable("create.PFGlight")
    shinyjs::disable("create.PFGsoil")
    return("")
  }
})

get_doLight = eventReactive(input$graph.simulParam, {
  file.globalParam = get_globalParam()
  doLight = FALSE
  if (file.exists(file.globalParam))
  {
    doLight = .getParam(params.lines = file.globalParam
                        , flag = "DO_LIGHT_COMPETITION"
                        , flag.split = " "
                        , is.num = TRUE)
  }
  doLight
})

get_doSoil = eventReactive(input$graph.simulParam, {
  file.globalParam = get_globalParam()
  doSoil = FALSE
  if (file.exists(file.globalParam))
  {
    doSoil = .getParam(params.lines = file.globalParam
                       , flag = "DO_SOIL_COMPETITION"
                       , flag.split = " "
                       , is.num = TRUE)
  }
  doSoil
})

get_dir.save = eventReactive(input$graph.simulParam, {
  if (nchar(input$graph.simulParam) > 0)
  {
    dir.save = .getParam(params.lines = input$graph.simulParam
                         , flag = "SAVE_DIR"
                         , flag.split = "^--.*--$"
                         , is.num = FALSE)
    dir.save = paste0(get_path.folder(), "/", dir.save)
    dir.save
  }
})

####################################################################

get_enableLightSoil = eventReactive(input$graph.simulParam, {
  if (get_doLight())
  {
    shinyjs::enable("create.evolutionLight")
    shinyjs::enable("create.PFGlight")
  } else
  {
    shinyjs::disable("create.evolutionLight")
    shinyjs::disable("create.PFGlight")
  }
  
  if (get_doSoil())
  {
    shinyjs::enable("create.evolutionSoil")
    shinyjs::enable("create.PFGsoil")
  } else
  {
    shinyjs::disable("create.evolutionSoil")
    shinyjs::disable("create.PFGsoil")
  }
})

observeEvent(input$graph.simulParam, {
  
  get_enableLightSoil()
    
  ## -------------------------------------------------------------    
  years.available = list.files(paste0(get_dir.save(), "/ABUND_perPFG_allStrata"))
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
  strata.available = list.files(paste0(get_dir.save(), "/ABUND_perPFG_perStrata"))
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
})
