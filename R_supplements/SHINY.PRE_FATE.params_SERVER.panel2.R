
####################################################################

get_path.run = eventReactive(input$run.folder.simul, {
  if (input$run.folder.simul > 0)
  {
    path = choose.dir(default = readDirectoryInput(session, 'run.folder.simul'))
    updateDirectoryInput(session, 'run.folder.simul', value = path)
    return(path)
  }
})

####################################################################

observeEvent(input$run.folder.simul, {
  if (input$run.folder.simul > 0)
  {
    names.simulParam = list.files(path = paste0(get_path.run(), "/PARAM_SIMUL")
                                  , pattern = ".txt$"
                                  , all.files = FALSE
                                  , full.names = TRUE)
    names.simulParam = basename(names.simulParam)
    if (length(names.simulParam) > 0)
    {
      updateSelectInput(session
                        , inputId = "run.simulParam"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      shinyjs::enable("run.simulParam")
    } else
    {
      shinyjs::reset("run.simulParam")
      shinyjs::disable("run.simulParam")
    }
  } else
  {
    shinyjs::reset("run.simulParam")
    shinyjs::disable("run.simulParam")
  }
})

####################################################################

observeEvent(input$run.copy, {
  if (input$run.folder.simul > 0 && nchar(input$run.simulParam) > 0)
  {
    if(!is.null(input$run.executable))
    {
      system(paste0("scp -r ", get_path.run(), " ./"))
      system(paste0("scp ", input$run.executable$datapath, " FATE_executable.exe"))
    }
  }
})

####################################################################

observeEvent(input$run, {
  if (input$run.folder.simul > 0 && nchar(input$run.simulParam) > 0)
  {
    if (file.exists("FATE_executable.exe") &&
        dir.exists(basename(get_path.run())))
    {
      print("youhouuu")
    }
  }
})

