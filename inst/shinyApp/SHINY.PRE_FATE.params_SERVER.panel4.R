
####################################################################

observeEvent(input$HELP.panel4, {
  introjs(session = session
          , options = list("nextLabel" = "Next"
                           , "prevLabel" = "Prev"
                           , "skipLabel" = "Close"
                           , steps = data.frame(element = c(paste0("#help4_", 1:2),"#main.panel")
                                                , intro = c("<p>A folder name with a typical <code>FATE-HD</code> organization, that can be created with the function 
                                                              <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.skeletonDirectory.html'>PRE_FATE.skeletonDirectory</a>.</p>
                                                              <p><strong>RESULTS</strong> folder should contain <code>FATE-HD</code> output maps.</p>"
                                                            , "<p><strong>ParamSimulation file</strong> : containing all links to the files created with the previous functions.<br>
                                                              It can be created with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_simulParameters.html'>PRE_FATE.params_simulParameters</a>.</p>
                                                              <p>Parameters will be extracted and used to produce output graphics.</p>"
                                                            , "<p><em>1. BROWSER</em></p>
                                                            <ul><li>Explore <strong>pre-existing graphics</strong> into the selected simulation folder</li></ul>
                                                            <p><em>2. Evolution of simulation through time</em></p>
                                                            <ul><li>Create graphics with abundances of PFG <strong>over several years</strong></li></ul>
                                                            <p><em>3. Specific year</em></p>
                                                            <ul><li>Create graphics with abundances of PFG <strong>for a specific year</strong> of simulation</li></ul>
                                                            "))
          )
  )
})


####################################################################

get_path.simul = eventReactive(input$graph.folder.simul, {
  if (input$graph.folder.simul > 0)
  {
    path = choose.dir(default = readDirectoryInput(session, 'graph.folder.simul'))
    updateDirectoryInput(session, 'graph.folder.simul', value = path)
    return(path)
  }
})

get_name.simul = eventReactive(input$graph.folder.simul, {
  return(basename(get_path.simul()))
})

get_path.folder = eventReactive(input$graph.folder.simul, {
  return(dirname(get_path.simul()))
})

get_param.simul = eventReactive(input$graph.simulParam, {
  return(paste0(get_path.simul(), "/PARAM_SIMUL/", input$graph.simulParam))
})

####################################################################

observeEvent(input$graph.folder.simul, {
  if (input$graph.folder.simul > 0)
  {
    print("yo1")
    names.simulParam = list.files(path = paste0(get_path.simul(), "/PARAM_SIMUL")
                                  , pattern = ".txt$"
                                  , all.files = FALSE
                                  , full.names = TRUE)
    names.simulParam = basename(names.simulParam)
    print("yo2")
    
    if (length(names.simulParam) > 0)
    {
      print("yo3")
      
      updateSelectInput(session
                        , inputId = "graph.simulParam"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      print("yo4")
      
      shinyjs::enable("graph.simulParam")
      shinyjs::enable("create.relativeAbund")
      shinyjs::enable("create.evolutionCoverage")
      shinyjs::enable("create.evolutionAbund")
      shinyjs::enable("create.PFGvsHS")
      shinyjs::enable("create.validationStat")
      shinyjs::enable("create.PFGrichness")
      shinyjs::enable("create.PFGcover")
      print("yo5")
      
    } else
    {
      shinyjs::reset("graph.simulParam")
      shinyjs::disable("graph.simulParam")
      shinyjs::disable("create.relativeAbund")
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
    print("yo6")
    
    update_browser.files()
    print("yo7")
    
  } else
  {
    shinyjs::reset("graph.simulParam")
    shinyjs::disable("graph.simulParam")
    shinyjs::disable("create.relativeAbund")
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

get_last.createdFiles1 = function(pattern_path)
{
  system(command = paste0("ls -lat "
                          , get_path.simul()
                          , "/RESULTS/"
                          , pattern_path
                          , " | awk '{print $9}'")
         , intern = TRUE)
}

get_last.createdFiles2 = function(pattern_path = "", pattern_head, pattern_tail)
{
  last.createdFiles = get_last.createdFiles1(pattern_path = pattern_path)
  last.createdFiles = last.createdFiles[grep(pattern = pattern_head, last.createdFiles)]
  last.createdFiles = last.createdFiles[grep(pattern = pattern_tail, last.createdFiles)]
  return(paste0(get_path.simul()
                , "/RESULTS/"
                , pattern_path
                , last.createdFiles[1]))
}

####################################################################

get_globalParam = eventReactive(input$graph.simulParam, {
  print("ya1")
  
  if (nchar(input$graph.simulParam) > 0)
  {
    print("ya2")
    
    shinyjs::enable("create.relativeAbund")
    shinyjs::enable("create.validationStat")
    shinyjs::enable("create.evolutionCoverage")
    shinyjs::enable("create.evolutionAbund")
    shinyjs::enable("create.PFGvsHS")
    shinyjs::enable("create.PFGrichness")
    shinyjs::enable("create.PFGcover")
    print("ya3")
    
    file.globalParam = .getParam(params.lines = get_param.simul()
                                 , flag = "GLOBAL_PARAMS"
                                 , flag.split = "^--.*--$"
                                 , is.num = FALSE)
    file.globalParam = paste0(get_path.folder(), "/", file.globalParam)
    file.globalParam
    print("ya4")
    
  } else
  {
    shinyjs::disable("create.relativeAbund")
    shinyjs::disable("create.validationStat")
    shinyjs::disable("create.evolutionCoverage")
    shinyjs::disable("create.evolutionAbund")
    shinyjs::disable("create.evolutionLight")
    shinyjs::disable("create.evolutionSoil")
    shinyjs::disable("create.PFGvsHS")
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
    dir.save = .getParam(params.lines = get_param.simul()
                         , flag = "SAVE_DIR"
                         , flag.split = "^--.*--$"
                         , is.num = FALSE)
    dir.save = paste0(get_path.folder(), "/", dir.save)
    dir.save
  }
})

get_lightFiles = eventReactive(input$graph.simulParam, {
  if (nchar(input$graph.simulParam) > 0 && get_doLight())
  {
    lightFiles = .getParam(params.lines = get_param.simul()
                           , flag = "PFG_LIGHT_PARAMS"
                           , flag.split = "^--.*--$"
                           , is.num = FALSE)
    lightFiles = paste0(get_path.folder(), "/", lightFiles)
    lightFiles
  } else
  {
    return("")
  }
})

get_soilFiles = eventReactive(input$graph.simulParam, {
  if (nchar(input$graph.simulParam) > 0 && get_doSoil())
  {
    soilFiles = .getParam(params.lines = get_param.simul()
                          , flag = "PFG_SOIL_PARAMS"
                          , flag.split = "^--.*--$"
                          , is.num = FALSE)
    soilFiles = paste0(get_path.folder(), "/", soilFiles)
    soilFiles
  } else
  {
    return("")
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

####################################################################

mat.PFG.succ = reactive({
  lightFiles = get_lightFiles()
  soilFiles = get_soilFiles()
  
  tabLight = tabSoil = data.frame(PFG = NA)
  
  no_PFG = length(which(nchar(lightFiles) > 0))
  if (no_PFG > 0)
  {
    tabLight = foreach(i = 1:no_PFG, .combine = "rbind") %do%
    {
      pfg = .getParam(params.lines = lightFiles[i]
                      , flag = "NAME"
                      , flag.split = " "
                      , is.num = FALSE)
      light = .getParam(params.lines = lightFiles[i]
                        , flag = "NAME"
                        , flag.split = " "
                        , is.num = FALSE)
      return(data.frame(PFG = pfg, light = light))
    }
  }
  
  no_PFG = length(which(nchar(soilFiles) > 0))
  if (no_PFG > 0)
  {
    tabSoil = foreach(i = 1:no_PFG, .combine = "rbind") %do%
    {
      pfg = .getParam(params.lines = soilFiles[i]
                      , flag = "NAME"
                      , flag.split = " "
                      , is.num = FALSE)
      soil = .getParam(params.lines = soilFiles[i]
                       , flag = "SOIL_CONTRIB"
                       , flag.split = " "
                       , is.num = TRUE)
      return(data.frame(PFG = pfg, soil_contrib = soil))
    }
  }
  
  return(na.exclude(merge(tabLight, tabSoil, by = "PFG", all = TRUE)))
})

####################################################################

observeEvent(input$graph.simulParam, {
  
  get_enableLightSoil()
  
  ## -------------------------------------------------------------    
  years.available = list.files(paste0(get_dir.save(), "/ABUND_perPFG_allStrata"))
  years.available = sapply(sub("Abund_YEAR_", "", years.available)
                           , function(x) strsplit(as.character(x), "_")[[1]][1])
  years.available = rev(sort(unique(as.numeric(as.character(years.available)))))
  
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
