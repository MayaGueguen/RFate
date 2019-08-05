
####################################################################

output$UI.succ.PFG = renderUI({
  if (length(RV$names.PFG) == 0)
  {
    shinyjs::disabled(
      selectInput(inputId = "succ.PFG"
                  , label = NULL
                  , choices = RV$names.PFG
                  , selected = RV$names.PFG[1]
                  , multiple = F
                  , width = "100%")
    )
  } else
  {
    selectInput(inputId = "succ.PFG"
                , label = NULL
                , choices = RV$names.PFG
                , selected = RV$names.PFG[1]
                , multiple = F
                , width = "100%")
  }
})

####################################################################

output$mat.PFG.ALL = renderTable({ RV$mat.PFG.ALL })

observeEvent(input$add.PFG.succ, {
  RV$mat.PFG.ALL <- rbind(RV$mat.PFG.ALL
                          , data.frame(PFG = input$succ.PFG
                                       , type = input$succ.type
                                       , height = as.numeric(input$succ.height)
                                       , maturity = as.numeric(input$succ.maturity)
                                       , longevity = as.numeric(input$succ.longevity)
                                       , light = as.numeric(input$succ.light)
                          ))
})

observeEvent(input$delete.PFG.ALL, {
  RV$mat.PFG.ALL <- data.frame()
})

observeEvent(RV$mat.PFG.ALL, {
  if (nrow(RV$mat.PFG.ALL) > 0)
  {
    shinyjs::enable("create.succ")
    shinyjs::enable("create.light")
  } else
  {
    shinyjs::disable("create.succ")
    shinyjs::disable("create.light")
  }
})

####################################################################

output$created_table.succ = renderDataTable({
  path_folder = paste0(input$name.simul, "/DATA/PFGS/SUCC/")
  return(get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE))
})

observeEvent(input$create.succ, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGsuccession(name.simulation = input$name.simul
                                    , mat.PFG.succ = RV$mat.PFG.ALL[, c("PFG", "type", "height", "maturity", "longevity")]
                                    , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/SUCC/"))
    
    if (as.character(get_res) != "0")
    {
      output$created_table.succ = renderDataTable({
        path_folder = paste0(input$name.simul, "/DATA/PFGS/SUCC/")
        return(get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE))
      })
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

output$created_table.light = renderDataTable({
  path_folder = paste0(input$name.simul, "/DATA/PFGS/LIGHT/")
  return(get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE))
})

observeEvent(input$create.light, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGlight(name.simulation = input$name.simul
                               , mat.PFG.succ = RV$mat.PFG.ALL[, c("PFG", "type", "height", "maturity", "longevity", "light")]
                               , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/LIGHT/"))
    
    if (as.character(get_res) != "0")
    {
      output$created_table.light = renderDataTable({
        path_folder = paste0(input$name.simul, "/DATA/PFGS/LIGHT/")
        return(get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE))
      })
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

