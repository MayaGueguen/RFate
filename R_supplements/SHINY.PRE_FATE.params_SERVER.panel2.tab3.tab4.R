
####################################################################

output$UI.soil.PFG = renderUI({
  if (length(RV$names.PFG) == 0)
  {
    shinyjs::disabled(
      selectInput(inputId = "soil.PFG"
                  , label = NULL
                  , choices = RV$names.PFG
                  , selected = RV$names.PFG[1]
                  , multiple = F
                  , width = "100%")
    )
  } else
  {
    selectInput(inputId = "soil.PFG"
                , label = NULL
                , choices = RV$names.PFG
                , selected = RV$names.PFG[1]
                , multiple = F
                , width = "100%")
  }
})


####################################################################

output$mat.PFG.soil = renderTable({ RV$mat.PFG.soil })

observeEvent(input$add.PFG.soil, {
  RV$mat.PFG.soil <- rbind(RV$mat.PFG.soil
                           , data.frame(PFG = input$soil.PFG
                                        , type = input$soil.type
                                        , soil_contrib = as.numeric(input$soil.contrib)
                                        , soil_tol_min = as.numeric(input$soil.tol_min)
                                        , soil_tol_max = as.numeric(input$soil.tol_max)
                                        , lifeStage = rep(c("Germinant", "Immature", "Mature"), each = 3)
                                        , soilResources = rep(c("Low", "Medium", "High"), 3)
                                        , soil_tol = c(as.numeric(input$soil.Ge.L)
                                                       , as.numeric(input$soil.Ge.M)
                                                       , as.numeric(input$soil.Ge.H)
                                                       , as.numeric(input$soil.Im.L)
                                                       , as.numeric(input$soil.Im.M)
                                                       , as.numeric(input$soil.Im.H)
                                                       , as.numeric(input$soil.Ma.L)
                                                       , as.numeric(input$soil.Ma.M)
                                                       , as.numeric(input$soil.Ma.H))
                           ))
  
  shinyjs::enable("create.soil")
})

observeEvent(input$delete.PFG.soil, {
  RV$mat.PFG.soil <- data.frame()
  shinyjs::disable("create.soil")
})

####################################################################

output$created_table.soil = renderDataTable({
  path_folder = paste0(input$name.simul, "/DATA/PFGS/SOIL/")
  return(get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE))
})

observeEvent(input$create.soil, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGsoil(name.simulation = input$name.simul
                              , mat.PFG.soil = unique(RV$mat.PFG.soil[, c("PFG", "type", "soil_contrib", "soil_tol_min", "soil_tol_max")])
                              , mat.PFG.tol = RV$mat.PFG.soil[, c("PFG", "lifeStage", "soilResources", "soil_tol")]
                              , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/SOIL/"))
    
    if (as.character(get_res) != "0")
    {
      output$created_table.soil = renderDataTable({
        path_folder = paste0(input$name.simul, "/DATA/PFGS/SOIL/")
        return(get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE))
      })
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})
