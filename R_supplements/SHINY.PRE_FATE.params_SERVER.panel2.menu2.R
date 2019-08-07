
####################################################################

observeEvent(input$set.strategy, {
  if (!is.null(input$set.strategy))
  {
    shinyjs::show("main.panel")
    
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

# get_ranges = eventReactive(paste(get_checked(), get_sliders()), {
observeEvent(paste(get_checked(), get_sliders()), {
  
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
  params.sliders = get_sliders()
  
  if (!is.null(input$set.strategy))
  {
    print("uuuuuuuuuuuuu")
    
    if (!is.null(get_path.folder1()) &&
        nchar(get_path.folder1()) > 0 &&
        dir.exists(get_path.folder1()))
    {
      if (!is.null(input$set.folder1.simulParam1) &&
          nchar(input$set.folder1.simulParam1) > 0 &&
          file.exists(paste0(get_path.folder1(), "/PARAM_SIMUL/", input$set.folder1.simulParam1)))
      {
        ## GET FILE 1 informations
        
        ## Simulation parameter file
        abs.simulParam = paste0(get_path.folder1(), "/PARAM_SIMUL/", input$set.folder1.simulParam1)
        lines.simulParam = readLines(abs.simulParam)
        params.simulParam = grep("^--.*--$", lines.simulParam, value = TRUE)
        params.simulParam = gsub("--", "", params.simulParam)
        # params.simulParam.TOKEEP = params.simulParam[which(!(params.simulParam %in% toSuppr))]
        
        ## Global parameter file
        file.globalParam = .getParam(params.lines = abs.simulParam
                                     , flag = "GLOBAL_PARAMS"
                                     , flag.split = "^--.*--$"
                                     , is.num = FALSE)
        file.globalParam = paste0(dirname(get_path.folder1()), "/", file.globalParam)
        
        lines.globalParam = readLines(file.globalParam)
        params.globalParam = as.vector(sapply(lines.globalParam, function(x) strsplit(as.character(x), " ")[[1]][1]))
        params.globalParam = params.globalParam[which(params.globalParam != "##")]
        # params.globalParam.TOKEEP = params.globalParam[which(!(params.globalParam %in% toSuppr))]
        
        for (i in unlist(params.checked))
        {
          if (length(grep(GLOBAL.names.params[i], params.globalParam)) == 0)
          {
            for (y in 1:length(params.checked))
            {
              if (length(params.checked[[y]]) > 0)
              {
                toSuppr = c()
                for (x in 1:length(params.checked[[y]]))
                {
                  if (!is.null(params.checked[[y]][x]) && params.checked[[y]][x] == i)
                  {
                    toSuppr = c(toSuppr, x)
                    #   ## WARNING, le paramètre demandé à varier n'est pas défini
                  }
                }
                if (length(toSuppr) > 0)
                {
                  params.checked[[y]] = params.checked[[y]][-toSuppr]
                }
              }
            }
          }
        }

        ## Get parameters value
        PARAMS = lapply(params.checked[1:3], function(y) {
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
        
        if (input$set.strategy == "From 1 folder, 1 simulation file")
        {
          ## ------------------------------------------------------------------------------------------ 
          ff = function()
          {
            lapply(1:length(PARAMS), function(y) {
              if (length(PARAMS[[y]]) > 0)
              {
                sapply(1:length(PARAMS[[y]]), function(x) {
                  if (!is.null(PARAMS[[y]][x]))
                  {
                    res = todo(x, y)
                    names(res) = names(PARAMS[[y]][x])
                    return(res)
                  }
                })
              }
            })
          }
          todo = function(x, y) { return(as.vector(PARAMS[[y]][x]) * params.sliders[y] / 100) }
          PARAMS.ecart = ff()
          todo = function(x, y) { return(as.vector(PARAMS[[y]][x]) - PARAMS.ecart[[y]][x]) }
          PARAMS.min = ff()
          todo = function(x, y) { return(as.vector(PARAMS[[y]][x]) + PARAMS.ecart[[y]][x]) }
          PARAMS.max = ff()

          PARAMS.range = rbind(unlist(PARAMS.min)
                               , unlist(PARAMS.max))
          rownames(PARAMS.range) = c("min", "max")
          print(PARAMS.range)
          
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
              ## MESSAGE ERREUR
            } else
            {
              print("SCENARIO 2")
            }
          } else
          {
            shinyalert(type = "warning", text = paste0("The file '"
                                                       , paste0(get_path.folder1()
                                                                , "/PARAM_SIMUL/\n"
                                                                , input$set.folder1.simulParam2)
                                                       , "' does not exist !"))
            ## MESSAGE d'ERREUR folder1 simulParam2
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
                ## MESSAGE ERREUR
              } else
              {
                print("SCENARIO 3")
              }
            } else
            {
              shinyalert(type = "warning", text = paste0("The file '"
                                                         , paste0(get_path.folder2()
                                                                  , "/PARAM_SIMUL/\n"
                                                                  , input$set.folder2.simulParam2)
                                                         , "' does not exist !"))
              ## MESSAGE d'ERREUR folder2 simulParam2
            }
          } else
          {
            shinyalert(type = "warning", text = paste0("The folder '", get_path.folder2(), "' does not exist !"))
            ## MESSAGE d'ERREUR folder2
          }
        }
      } else
      {
        shinyalert(type = "warning", text = paste0("The file '"
                                                   , paste0(get_path.folder1()
                                                            , "/PARAM_SIMUL/\n"
                                                            , input$set.folder1.simulParam1)
                                                   , "' does not exist !"))
        ## MESSAGE d'ERREUR folder1 simulParam1
      }
    } else
    {
      shinyalert(type = "warning", text = paste0("The folder '", get_path.folder1(), "' does not exist !"))
      ## MESSAGE d'ERREUR folder1
    }
  }
})

