
####################################################################

observeEvent(input$run.folder.simul, {
  if (input$run.folder.simul > 0)
  {
    path = choose.dir(default = readDirectoryInput(session, 'run.folder.simul'))
    updateDirectoryInput(session, 'run.folder.simul', value = path)
    
    names.simulParam = list.files(path = paste0(path, "/PARAM_SIMUL")
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
    
    update_browser.files()
  } else
  {
    shinyjs::reset("run.simulParam")
    shinyjs::disable("run.simulParam")
  }
})

####################################################################
