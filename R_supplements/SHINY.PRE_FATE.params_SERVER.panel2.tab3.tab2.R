
####################################################################

output$UI.disp.PFG = renderUI({
  if (length(RV$names.PFG) == 0)
  {
    shinyjs::disabled(
      selectInput(inputId = "disp.PFG"
                  , label = NULL
                  , choices = RV$names.PFG
                  , selected = RV$names.PFG[1]
                  , multiple = F
                  , width = "100%")
    )
  } else
  {
    selectInput(inputId = "disp.PFG"
                , label = NULL
                , choices = RV$names.PFG
                , selected = RV$names.PFG[1]
                , multiple = F
                , width = "100%")
  }
})


####################################################################

output$mat.PFG.disp = renderTable({ RV$mat.PFG.disp })

observeEvent(input$add.PFG.disp, {
  RV$mat.PFG.disp <- rbind(RV$mat.PFG.disp
                           , data.frame(PFG = input$disp.PFG
                                        , MODE = as.numeric(input$disp.mode)
                                        , d50 = as.numeric(input$disp.d50)
                                        , d99 = as.numeric(input$disp.d99)
                                        , ldd = as.numeric(input$disp.ldd)))
})

observeEvent(input$delete.PFG.disp, {
  RV$mat.PFG.disp <- data.frame()
})

observeEvent(RV$mat.PFG.disp, {
  if (nrow(RV$mat.PFG.disp) > 0)
  {
    shinyjs::enable("create.disp")
  } else
  {
    shinyjs::disable("create.disp")
  }
})

####################################################################

output$created_table.disp = renderDataTable({
  path_folder = paste0(input$name.simul, "/DATA/PFGS/DISP/")
  return(get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE))
})

observeEvent(input$create.disp, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGdispersal(name.simulation = input$name.simul
                                   , mat.PFG.disp = RV$mat.PFG.disp
                                   , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/DISP/"))
    
    if (as.character(get_res) != "0")
    {
      output$created_table.disp = renderDataTable({
        path_folder = paste0(input$name.simul, "/DATA/PFGS/DISP/")
        return(get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE))
      })
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

