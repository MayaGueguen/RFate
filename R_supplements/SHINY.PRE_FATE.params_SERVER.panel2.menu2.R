
####################################################################

observeEvent(input$set.strategy, {
  if (!is.null(input$set.strategy))
  {
    shinyjs::show("main.panel")
    shinyjs::enable("create.multiple_set")
    
    if (input$set.strategy == "From 1 folder, 1 simulation file")
    {
      shinyjs::hide("set.folder1.simulParam2")
      output$UI.set.folders.strat3 = renderUI({ br() })
      shinyjs::show("set.slider.1")
      shinyjs::show("set.slider.2")
      shinyjs::show("set.slider.3")
    } else if (input$set.strategy == "From 1 folder, 2 simulation files")
    {
      shinyjs::show("set.folder1.simulParam2")
      output$UI.set.folders.strat3 = renderUI({ br() })
      shinyjs::hide("set.slider.1")
      shinyjs::hide("set.slider.2")
      shinyjs::hide("set.slider.3")
      
    } else if (input$set.strategy == "From 2 folders, 2 simulation files")
    {
      shinyjs::hide("set.folder1.simulParam2")
      output$UI.set.folders.strat3 = renderUI({
        tagList(
          directoryInput(inputId = "set.folder2"
                         , label = param.style("Select the simulation folder :")
                         , value = '~')
          , shinyjs::disabled(
            selectInput(inputId = "set.folder2.simulParam2"
                        , label = param.style("Select the simulation parameters file :")
                        , choices = NULL
                        , selected = NULL
                        , multiple = F
                        , width = "100%")
          )
        )
      })
      shinyjs::hide("set.slider.1")
      shinyjs::hide("set.slider.2")
      shinyjs::hide("set.slider.3")
    }
  }
})


####################################################################

get_path.folder1 = eventReactive(input$set.folder1, {
  if (input$set.folder1 > 0)
  {
    path = choose.dir(default = readDirectoryInput(session, 'set.folder1'))
    updateDirectoryInput(session, 'set.folder1', value = path)
    return(path)
  }
})

get_path.folder2 = eventReactive(input$set.folder2, {
  if (input$set.folder2 > 0)
  {
    path = choose.dir(default = readDirectoryInput(session, 'set.folder2'))
    updateDirectoryInput(session, 'set.folder2', value = path)
    return(path)
  }
})

####################################################################

observeEvent(paste(input$set.folder1, input$set.strategy), {
  if (input$set.folder1 > 0)
  {
    names.simulParam = list.files(path = paste0(get_path.folder1(), "/PARAM_SIMUL")
                                  , pattern = ".txt$"
                                  , all.files = FALSE
                                  , full.names = TRUE)
    names.simulParam = basename(names.simulParam)
    if (length(names.simulParam) > 0)
    {
      updateSelectInput(session
                        , inputId = "set.folder1.simulParam1"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      
      updateSelectInput(session
                        , inputId = "set.folder1.simulParam2"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      
      shinyjs::enable("set.folder1.simulParam1")
      shinyjs::enable("set.folder1.simulParam2")
    } else
    {
      shinyjs::reset("set.folder1.simulParam1")
      shinyjs::reset("set.folder1.simulParam2")
      shinyjs::disable("set.folder1.simulParam1")
      shinyjs::disable("set.folder1.simulParam2")
    }
  } else
  {
    shinyjs::reset("set.folder1.simulParam1")
    shinyjs::reset("set.folder1.simulParam2")
    shinyjs::disable("set.folder1.simulParam1")
    shinyjs::disable("set.folder1.simulParam2")
  }
})

observeEvent(input$set.folder2, {
  if (input$set.folder2 > 0)
  {
    names.simulParam = list.files(path = paste0(get_path.folder2(), "/PARAM_SIMUL")
                                  , pattern = ".txt$"
                                  , all.files = FALSE
                                  , full.names = TRUE)
    names.simulParam = basename(names.simulParam)
    if (length(names.simulParam) > 0)
    {
      updateSelectInput(session
                        , inputId = "set.folder2.simulParam2"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      
      shinyjs::enable("set.folder2.simulParam2")
    } else
    {
      shinyjs::reset("set.folder2.simulParam2")
      shinyjs::disable("set.folder2.simulParam2")
    }
  } else
  {
    shinyjs::reset("set.folder2.simulParam2")
    shinyjs::disable("set.folder2.simulParam2")
  }
})


####################################################################

get_checked = eventReactive(lapply(grep(pattern = "^set.choices.",
                                        x = names(input),
                                        value = TRUE), function(x) input[[x]]),
                            {
                              return(list(input$set.choices.1
                                          , input$set.choices.2
                                          , input$set.choices.3
                                          , input$set.choices.4
                                          , input$set.choices.5
                                          , input$set.choices.6))
                            })

get_sliders = eventReactive(lapply(grep(pattern = "^set.slider.",
                                        x = names(input),
                                        value = TRUE), function(x) input[[x]]),
                            {
                              if (!is.null(input$set.strategy))
                              {
                                if (input$set.strategy == "From 1 folder, 1 simulation file")
                                {
                                  return(c(input$set.slider.1
                                           , input$set.slider.1
                                           , input$set.slider.3))
                                }
                              }
                            })

get_toSuppr = eventReactive(get_checked(), {
  
  GLOBAL.names.params = c("max_by_cohort" = "MAX_BY_COHORT"
                          , "max_abund_low" = "MAX_ABUND_LOW"
                          , "max_abund_medium" = "MAX_ABUND_MEDIUM"
                          , "max_abund_high" = "MAX_ABUND_HIGH"
                          , "ref_option" = "HABSUIT_OPTION"
                          , "seeding_duration" = "SEEDING_DURATION"
                          , "seeding_step" = "SEEDING_TIMESTEP"
                          , "seeding_input" = "SEEDING_INPUT"
                          , "mode_dispers" = "DISPERSAL_MODE"
                          , "light_thresh_medium" = "LIGHT_THRESH_MEDIUM"
                          , "light_thresh_low" = "LIGHT_THRESH_LOW"
                          , "strata_limits" = "NB_STRATUM")
  
  params.checked = get_checked()
  
  toSuppr = c("GLOBAL_PARAMS", "SAVE_DIR", "END_OF_FILE")
  if ("strata_limits" %in% params.checked)
  {
    toSuppr = c(toSuppr, "PFG_LIFE_HISTORY_PARAMS", "PFG_LIGHT_PARAMS")
  }
  for (i in params.checked)
  {
    toSuppr = c(toSuppr, as.vector(GLOBAL.names.params[i]))
  }
  
  return(toSuppr)
})

####################################################################

get_PARAMS = function(path_folder, file_simul, params)
{
  GLOBAL.names.params = c("max_by_cohort" = "MAX_BY_COHORT"
                          , "max_abund_low" = "MAX_ABUND_LOW"
                          , "max_abund_medium" = "MAX_ABUND_MEDIUM"
                          , "max_abund_high" = "MAX_ABUND_HIGH"
                          , "ref_option" = "HABSUIT_OPTION"
                          , "seeding_duration" = "SEEDING_DURATION"
                          , "seeding_step" = "SEEDING_TIMESTEP"
                          , "seeding_input" = "SEEDING_INPUT"
                          , "mode_dispers" = "DISPERSAL_MODE"
                          , "light_thresh_medium" = "LIGHT_THRESH_MEDIUM"
                          , "light_thresh_low" = "LIGHT_THRESH_LOW"
                          , "strata_limits" = "NB_STRATUM")
  
  ## GET FILE informations
  
  ## Simulation parameter file
  abs.simulParam = paste0(path_folder, "/PARAM_SIMUL/", file_simul)
  lines.simulParam = readLines(abs.simulParam)
  # params.simulParam = grep("^--.*--$", lines.simulParam, value = TRUE)
  # params.simulParam = gsub("--", "", params.simulParam)
  # params.simulParam.TOKEEP = params.simulParam[which(!(params.simulParam %in% get_toSuppr()))]
  ind = grep("^--.*--$", lines.simulParam)
  params.simulParam = lines.simulParam[ind]
  params.simulParam = gsub("--", "", params.simulParam)
  params.simulParam.TOKEEP = params.simulParam[which(!(params.simulParam %in% get_toSuppr()))]
  params.simulParam.TOKEEP = paste0("--", params.simulParam.TOKEEP, "--")
  toKeep = c()
  for (i in sapply(params.simulParam.TOKEEP, function(x) grep(x, lines.simulParam)))
  {
    toKeep = c(toKeep, lines.simulParam[i:(ind[which(ind == i) + 1] - 1)])
  }
  params.simulParam.TOKEEP = toKeep
  
  ## Global parameter file
  file.globalParam = .getParam(params.lines = abs.simulParam
                               , flag = "GLOBAL_PARAMS"
                               , flag.split = "^--.*--$"
                               , is.num = FALSE)
  file.globalParam = paste0(dirname(path_folder), "/", file.globalParam)
  
  lines.globalParam = readLines(file.globalParam)
  params.globalParam = as.vector(sapply(lines.globalParam, function(x) strsplit(as.character(x), " ")[[1]][1]))
  # params.globalParam = params.globalParam[which(params.globalParam != "##")]
  # params.globalParam.TOKEEP = params.globalParam[which(!(params.globalParam %in% get_toSuppr()))]
  params.globalParam.TOKEEP = lines.globalParam[which(!(params.globalParam %in% get_toSuppr()))]
  if (length(grep("##", params.globalParam.TOKEEP)) > 0)
  {
    params.globalParam.TOKEEP = params.globalParam.TOKEEP[-grep("##", params.globalParam.TOKEEP)]
  }
  
  for (i in unlist(params))
  {
    if (length(grep(GLOBAL.names.params[i], params.globalParam)) == 0)
    {
      for (y in 1:length(params))
      {
        if (length(params[[y]]) > 0)
        {
          toSuppr = c()
          for (x in 1:length(params[[y]]))
          {
            if (!is.null(params[[y]][x]) && params[[y]][x] == i)
            {
              toSuppr = c(toSuppr, x)
              shinyalert(type = "warning", text = paste0("The parameter '", i, "' is not defined in the global file :\n"
                                                         , basename(file.globalParam)
                                                         , "\n from the simulation file :\n"
                                                         , basename(abs.simulParam)
                                                         , "\n\nIt will not be considered."))
            }
          }
          if (length(toSuppr) > 0)
          {
            params[[y]] = params[[y]][-toSuppr]
          }
        }
      }
    }
  }
  
  ## Get parameters value
  PARAMS = lapply(params[1:3], function(y) {
    sapply(y, function(x) {
      if (!is.null(x))
      {
        return(.getParam(params.lines = file.globalParam
                         , flag = as.vector(GLOBAL.names.params[x])
                         , flag.split = " "
                         , is.num = TRUE))
      }
    })
  })
  
  return(list(PARAMS = PARAMS
              , TOKEEP.simul = params.simulParam.TOKEEP
              , TOKEEP.global = params.globalParam.TOKEEP))
}

####################################################################

get_ranges = eventReactive(paste(input$set.strategy
                                 , input$set.folder1
                                 , input$set.folder1.simulParam1
                                 , input$set.folder1.simulParam2
                                 , input$set.folder2
                                 , input$set.folder2.simulParam2
                                 , get_checked()
                                 , get_sliders()), {
  if (!is.null(input$set.strategy))
  {
    if (!is.null(get_path.folder1()) &&
        nchar(get_path.folder1()) > 0 &&
        dir.exists(get_path.folder1()))
    {
      if (!is.null(input$set.folder1.simulParam1) &&
          nchar(input$set.folder1.simulParam1) > 0 &&
          file.exists(paste0(get_path.folder1(), "/PARAM_SIMUL/", input$set.folder1.simulParam1)))
      {
        params.checked = get_checked()
        
        ## GET FILE 1 informations
        PARAMS1 = get_PARAMS(path_folder = get_path.folder1()
                            , file_simul = input$set.folder1.simulParam1
                            , params = params.checked)
        TOKEEP1.simul = PARAMS1$TOKEEP.simul
        TOKEEP1.global = PARAMS1$TOKEEP.global
        PARAMS1 = PARAMS1$PARAMS
        
        if (input$set.strategy == "From 1 folder, 1 simulation file")
        {
          ## ------------------------------------------------------------------------------------------ 
          ff = function()
          {
            lapply(1:length(PARAMS1), function(y) {
              if (length(PARAMS1[[y]]) > 0)
              {
                sapply(1:length(PARAMS1[[y]]), function(x) {
                  if (!is.null(PARAMS1[[y]][x]))
                  {
                    res = todo(x, y)
                    names(res) = names(PARAMS1[[y]][x])
                    return(res)
                  }
                })
              }
            })
          }
          
          params.sliders = get_sliders()
          todo = function(x, y) { return(as.vector(PARAMS1[[y]][x]) * params.sliders[y] / 100) }
          PARAMS.ecart = ff()
          todo = function(x, y) { return(as.vector(PARAMS1[[y]][x]) - PARAMS.ecart[[y]][x]) }
          PARAMS.min = ff()
          todo = function(x, y) { return(as.vector(PARAMS1[[y]][x]) + PARAMS.ecart[[y]][x]) }
          PARAMS.max = ff()

          PARAMS.range = rbind(unlist(PARAMS.min)
                               , unlist(PARAMS.max))
          rownames(PARAMS.range) = c("min", "max")
          return(list(PARAMS.range = PARAMS.range
                      , TOKEEP.global = TOKEEP1.global
                      , TOKEEP.simul = TOKEEP1.simul))
          
        } else if (input$set.strategy == "From 1 folder, 2 simulation files")
        {
          ## ------------------------------------------------------------------------------------------
          if (!is.null(input$set.folder1.simulParam2) &&
              nchar(input$set.folder1.simulParam2) > 0 &&
              file.exists(paste0(get_path.folder1(), "/PARAM_SIMUL/", input$set.folder1.simulParam2)))
          {
            if (input$set.folder1.simulParam1 == input$set.folder1.simulParam2)
            {
              shinyalert(type = "warning", text = paste0("You must select different simulation parameter files !"))
            } else
            {
              ## GET FILE 2 informations
              PARAMS2 = get_PARAMS(path_folder = get_path.folder1()
                                   , file_simul = input$set.folder1.simulParam2
                                   , params = params.checked)
              TOKEEP2.simul = PARAMS2$TOKEEP.simul
              TOKEEP2.global = PARAMS2$TOKEEP.global
              PARAMS2 = PARAMS2$PARAMS
              
              if (length(unlist(PARAMS1)) != length(unlist(PARAMS2)) ||
                  sum(names(unlist(PARAMS1)) == names(unlist(PARAMS2))) != length(unlist(PARAMS1)))
              {
                shinyalert(type = "warning", text = paste0("The files do not contain the same parameters to be evaluated.\n"
                                                           , "\n File 1 : '"
                                                           , paste0(names(unlist(PARAMS1)), collapse = "', '")
                                                           , "'\n File 2 : '"
                                                           , paste0(names(unlist(PARAMS2)), collapse = "', '")
                                                           , "'\n\nPlease check."))
              } else if (length(TOKEEP1.global) != length(TOKEEP2.global) ||
                         sum(TOKEEP1.global == TOKEEP2.global) != length(TOKEEP1.global))
              {
                shinyalert(type = "warning", text = paste0("The global files have different fixed parameter values."
                                                           , "\nPlease check."))
              } else if (length(TOKEEP1.simul) != length(TOKEEP2.simul) ||
                         sum(TOKEEP1.simul == TOKEEP2.simul) != length(TOKEEP1.simul))
              {
                shinyalert(type = "warning", text = paste0("The simulation files have different fixed parameter values."
                                                           , "\nPlease check."))
              } else
              {
                
                PARAMS.min = sapply(1:length(unlist(PARAMS1)), function(x) { min(c(unlist(PARAMS1)[x], unlist(PARAMS2)[x])) })
                PARAMS.max = sapply(1:length(unlist(PARAMS1)), function(x) { max(c(unlist(PARAMS1)[x], unlist(PARAMS2)[x])) })
                names(PARAMS.min) = names(PARAMS.max) = names(unlist(PARAMS1))
                
                PARAMS.range = rbind(unlist(PARAMS.min)
                                     , unlist(PARAMS.max))
                rownames(PARAMS.range) = c("min", "max")
                return(list(PARAMS.range = PARAMS.range
                            , TOKEEP.global = TOKEEP1.global
                            , TOKEEP.simul = TOKEEP1.simul))
              }
            }
          } else
          {
            shinyalert(type = "warning", text = paste0("The file '"
                                                       , paste0(get_path.folder1()
                                                                , "/PARAM_SIMUL/\n"
                                                                , input$set.folder1.simulParam2)
                                                       , "' does not exist !"))
          }
        } else if (input$set.strategy == "From 2 folders, 2 simulation files")
        {
          ## ------------------------------------------------------------------------------------------
          if (!is.null(get_path.folder2()) &&
              nchar(get_path.folder2()) > 0 &&
              dir.exists(get_path.folder2()))
          {
            if (!is.null(input$set.folder2.simulParam2) &&
                nchar(input$set.folder2.simulParam2) > 0 &&
                file.exists(paste0(get_path.folder2(), "/PARAM_SIMUL/", input$set.folder2.simulParam2)))
            {
              if (get_path.folder1() == get_path.folder2() &&
                  input$set.folder1.simulParam1 == input$set.folder2.simulParam2)
              {
                shinyalert(type = "warning", text = paste0("You must select different simulation parameter files !"))
              } else
              {
                ## GET FILE 2 informations
                PARAMS2 = get_PARAMS(path_folder = get_path.folder2()
                                     , file_simul = input$set.folder2.simulParam2
                                     , params = params.checked)
                TOKEEP2.simul = PARAMS2$TOKEEP.simul
                TOKEEP2.global = PARAMS2$TOKEEP.global
                PARAMS2 = PARAMS2$PARAMS
                
                if (length(unlist(PARAMS1)) != length(unlist(PARAMS2)) ||
                    sum(names(unlist(PARAMS1)) == names(unlist(PARAMS2))) != length(unlist(PARAMS1)))
                {
                  shinyalert(type = "warning", text = paste0("The files do not contain the same parameters to be evaluated.\n"
                                                             , "\n File 1 : '"
                                                             , paste0(names(unlist(PARAMS1)), collapse = "', '")
                                                             , "'\n File 2 : '"
                                                             , paste0(names(unlist(PARAMS2)), collapse = "', '")
                                                             , "'\n\nPlease check."))
                } else if (length(TOKEEP1.global) != length(TOKEEP2.global) ||
                           sum(TOKEEP1.global == TOKEEP2.global) != length(TOKEEP1.global))
                {
                  shinyalert(type = "warning", text = paste0("The global files have different fixed parameter values."
                                                             , "\nPlease check."))
                } else if (length(TOKEEP1.simul) != length(TOKEEP2.simul) ||
                           sum(TOKEEP1.simul == TOKEEP2.simul) != length(TOKEEP1.simul))
                {
                  shinyalert(type = "warning", text = paste0("The simulation files have different fixed parameter values."
                                                             , "\nPlease check."))
                } else
                {
                  
                  PARAMS.min = sapply(1:length(unlist(PARAMS1)), function(x) { min(c(unlist(PARAMS1)[x], unlist(PARAMS2)[x])) })
                  PARAMS.max = sapply(1:length(unlist(PARAMS1)), function(x) { max(c(unlist(PARAMS1)[x], unlist(PARAMS2)[x])) })
                  names(PARAMS.min) = names(PARAMS.max) = names(unlist(PARAMS1))
                  
                  PARAMS.range = rbind(unlist(PARAMS.min)
                                       , unlist(PARAMS.max))
                  rownames(PARAMS.range) = c("min", "max")
                  return(list(PARAMS.range = PARAMS.range
                              , TOKEEP.global = TOKEEP1.global
                              , TOKEEP.simul = TOKEEP1.simul))
                }
              }
            } else
            {
              shinyalert(type = "warning", text = paste0("The file '"
                                                         , paste0(get_path.folder2()
                                                                  , "/PARAM_SIMUL/\n"
                                                                  , input$set.folder2.simulParam2)
                                                         , "' does not exist !"))
            }
          } else
          {
            shinyalert(type = "warning", text = paste0("The folder '", get_path.folder2(), "' does not exist !"))
          }
        }
      } else
      {
        shinyalert(type = "warning", text = paste0("The file '"
                                                   , paste0(get_path.folder1()
                                                            , "/PARAM_SIMUL/\n"
                                                            , input$set.folder1.simulParam1)
                                                   , "' does not exist !"))
      }
    } else
    {
      shinyalert(type = "warning", text = paste0("The folder '", get_path.folder1(), "' does not exist !"))
    }
  }
})

####################################################################

observeEvent(input$create.multiple_set, {
  params.checked = get_checked()
  
  if (length(unlist(params.checked)) == 0)
  {
    shinyalert(type = "warning", text = "You must select some parameters to vary !")
  } else
  {
    params.ranges = get_ranges()
    
    print(params.checked)
    print(params.ranges)
    
    ## CREATE NEW FOLDER
    # PRE_FATE.skeletonDirectory(name.simulation = "FATE_simulation_MULTIPLE_SET")
    
  }
})

