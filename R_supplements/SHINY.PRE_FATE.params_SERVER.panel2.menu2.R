
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
      shinyjs::show("set.slider.3")
      shinyjs::show("set.slider.5")
    } else if (input$set.strategy == "From 1 folder, 2 simulation files")
    {
      shinyjs::show("set.folder1.simulParam2")
      output$UI.set.folders.strat3 = renderUI({ br() })
      shinyjs::hide("set.slider.1")
      shinyjs::hide("set.slider.3")
      shinyjs::hide("set.slider.5")
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
      shinyjs::hide("set.slider.3")
      shinyjs::hide("set.slider.5")
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
                                           , 0
                                           , input$set.slider.3
                                           , 0
                                           , input$set.slider.5
                                           , 0))
                                }
                              }
                            })

# get_ranges = eventReactive(paste(get_checked(), get_sliders()), {
observeEvent(paste(get_checked(), get_sliders()), {
  if (!is.null(input$set.strategy))
  {
    print("uuuuuuuuuuuuu")
    print(input$set.folder1)
    print(nchar(input$set.folder1))
    print(get_path.folder1())
    if (!is.null(get_path.folder1()) &&
        # !is.na(get_path.folder1()) &&
        nchar(get_path.folder1()) > 0 &&
        dir.exists(get_path.folder1()))
    {
      if (!is.null(input$set.folder1.simulParam1) &&
          nchar(input$set.folder1.simulParam1) > 0 &&
          file.exists(paste0(get_path.folder1(), "/PARAM_SIMUL/", input$set.folder1.simulParam1)))
      {
        if (input$set.strategy == "From 1 folder, 1 simulation file")
        {
          print("SCENARIO 1")
        } else if (input$set.strategy == "From 1 folder, 2 simulation files")
        {
          if (!is.null(input$set.folder1.simulParam2) &&
              nchar(input$set.folder1.simulParam2) > 0 &&
              file.exists(paste0(get_path.folder1(), "/PARAM_SIMUL/", input$set.folder1.simulParam2)))
          { 
            print("SCENARIO 2")
          } else
          {
            shinyalert(type = "warning", text = paste0("The file '"
                                                       , paste0(get_path.folder1()
                                                                , "/PARAM_SIMUL/\n"
                                                                , input$set.folder1.simulParam2)
                                                       , "'does not exist !"))
            ## MESSAGE d'ERREUR folder1 simulParam2
          }
        } else if (input$set.strategy == "From 2 folders, 2 simulation files")
        {
          if (!is.null(get_path.folder2()) &&
              nchar(get_path.folder2()) > 0 &&
              dir.exists(get_path.folder2()))
          {
            if (!is.null(input$set.folder2.simulParam2) &&
                nchar(input$set.folder2.simulParam2) > 0 &&
                file.exists(paste0(get_path.folder2(), "/PARAM_SIMUL/", input$set.folder2.simulParam2)))
            { 
              print("SCENARIO 3")
            } else
            {
              shinyalert(type = "warning", text = paste0("The file '"
                                                         , paste0(get_path.folder2()
                                                                  , "/PARAM_SIMUL/\n"
                                                                  , input$set.folder2.simulParam2)
                                                         , "'does not exist !"))
              ## MESSAGE d'ERREUR folder2 simulParam2
            }
          } else
          {
            shinyalert(type = "warning", text = paste0("The folder '", get_path.folder2(), "'does not exist !"))
            ## MESSAGE d'ERREUR folder2
          }
        }
      } else
      {
        shinyalert(type = "warning", text = paste0("The file '"
                                                   , paste0(get_path.folder1()
                                                            , "/PARAM_SIMUL/\n"
                                                            , input$set.folder1.simulParam1)
                                                   , "'does not exist !"))
        ## MESSAGE d'ERREUR folder1 simulParam1
      }
    } else
    {
      shinyalert(type = "warning", text = paste0("The folder '", get_path.folder1(), "'does not exist !"))
      ## MESSAGE d'ERREUR folder1
    }
  }
})

