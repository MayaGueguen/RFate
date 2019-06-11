
####################################################################

get_opt.folder.name = eventReactive(input$PFG.folder, {
  opt.folder.name = ifelse(nchar(input$PFG.folder) > 0
                           , gsub(" ", "_", input$PFG.folder)
                           , "")
  if (length(grep(" ", input$PFG.folder)) > 0)
  {
    showNotification("Spaces within opt.folder.name have been replaced by `_` !", type = "warning")
  }
  return(opt.folder.name)
})

####################################################################

output$names.PFG = renderText({
  HTML(paste0("<strong>PFG list :</strong> ", paste0(RV$names.PFG, collapse = " ; ")))
})

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

observeEvent(RV$names.PFG, {
  if (length(RV$names.PFG) > 0)
  {
    shinyjs::enable("succ.PFG")
    shinyjs::enable("add.PFG.succ")
    shinyjs::enable("disp.PFG")
    shinyjs::enable("add.PFG.disp")
    shinyjs::enable("soil.PFG")
    shinyjs::enable("add.PFG.soil")
  } else
  {
    shinyjs::disable("succ.PFG")
    shinyjs::disable("add.PFG.succ")
    shinyjs::disable("disp.PFG")
    shinyjs::disable("add.PFG.disp")
    shinyjs::disable("soil.PFG")
    shinyjs::disable("add.PFG.soil")
  }
})

observeEvent(input$name.PFG, {
  if (nchar(input$name.PFG) > 0)
  {
    shinyjs::enable("add.PFG.name")
  } else
  {
    shinyjs::disable("add.PFG.name")
  }
})

observeEvent(input$add.PFG.name, {
  if (input$name.PFG %in% RV$names.PFG)
  {
    shinyalert(type = "warning", text = "You must give different PFG names !")
    shinyjs::reset("name.PFG")
  } else
  {
    RV$names.PFG = c(RV$names.PFG, input$name.PFG)
    shinyjs::reset("name.PFG")
  }
})

observeEvent(input$delete.names.PFG, {
  RV$names.PFG = vector()
  
  shinyjs::reset("name.PFG")
  shinyjs::reset("succ.PFG")
  shinyjs::reset("disp.PFG")
  shinyjs::reset("soil.PFG")
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

####################################################################

output$UI.dist.grouping = renderUI({
  name.1st_col = ""
  if (input$dist.grouping == "by type")
  {
    name.1st_col = "H"
  } else if (length(RV$names.PFG) > 0)
  {
    name.1st_col = RV$names.PFG[1]
  }
  
  if (nchar(name.1st_col) > 0)
  {
    fluidRow(
      column(6
             , br()
             , fluidRow(
               column(4, HTML(""))
               , column(4, HTML(""))
               , column(4, HTML(paste0("<strong>", name.1st_col, "</strong>")))
             )
             , fluidRow(
               column(4, HTML("<strong> Stage 1</strong>"))
               , column(4, HTML("<strong>Killed</strong>"))
               , column(4
                        , selectInput(inputId = paste0("dist.1.kill.", name.1st_col)
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML(""))
               , column(4, HTML("<strong>Resprout</strong>"))
               , column(4
                        , selectInput(inputId = paste0("dist.1.resprout.", name.1st_col)
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML("<strong> Stage 2</strong>"))
               , column(4, HTML("<strong>Killed</strong>"))
               , column(4
                        , selectInput(inputId = paste0("dist.2.kill.", name.1st_col)
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML(""))
               , column(4, HTML("<strong>Resprout</strong>"))
               , column(4
                        , selectInput(inputId = paste0("dist.2.resprout.", name.1st_col)
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML("<strong> Stage 3</strong>"))
               , column(4, HTML("<strong>Killed</strong>"))
               , column(4
                        , selectInput(inputId = paste0("dist.3.kill.", name.1st_col)
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML(""))
               , column(4, HTML("<strong>Resprout</strong>"))
               , column(4
                        , selectInput(inputId = paste0("dist.3.resprout.", name.1st_col)
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML("<strong> Stage 4</strong>"))
               , column(4, HTML("<strong>Killed</strong>"))
               , column(4
                        , selectInput(inputId = paste0("dist.4.kill.", name.1st_col)
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML(""))
               , column(4, HTML("<strong>Resprout</strong>"))
               , column(4
                        , selectInput(inputId = paste0("dist.4.resprout.", name.1st_col)
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             ))
      , uiOutput(outputId = "UI.dist.grouping.BIS")
    )
  }
})
  
output$UI.dist.grouping.BIS = renderUI({
  name.2nd_col = vector()
  if (input$dist.grouping == "by type")
  {
    name.2nd_col = c("C", "P")
  } else if (length(RV$names.PFG) > 1)
  {
    name.2nd_col = RV$names.PFG[2:length(RV$names.PFG)]
  }
  
  if (length(name.2nd_col) > 0)
  {
    lapply(name.2nd_col, function(j) {
      column(2
             , br()
             , HTML(paste0("<strong>", j, "</strong>"))
             , lapply(as.vector(sapply(1:4, function(x) paste0("dist.", x, ".", c("kill", "resprout"), ".", j)))
                      , function(i) {
                        selectInput(inputId = i
                                    , label = NULL
                                    , choices = seq(0,100,10)
                                    , multiple = FALSE
                                    , width = "100%")
                      })
      )
    })
  }
})


####################################################################

output$mat.PFG.dist = renderTable({ RV$mat.PFG.dist })

observeEvent(input$dist.name, {
  if (nchar(input$dist.name) > 0)
  {
    shinyjs::enable("add.PFG.dist")
  } else
  {
    shinyjs::disable("add.PFG.dist")
  }
})

observeEvent(input$add.PFG.dist, {
  if (input$dist.grouping == "by type")
  {
    RV$mat.PFG.dist <- rbind(RV$mat.PFG.dist
                           , data.frame(name = input$dist.name
                                        , responseStage = 1:4
                                        , KilledIndiv_H = as.numeric(c(input$dist.1.kill.H
                                                                       , input$dist.2.kill.H
                                                                       , input$dist.3.kill.H
                                                                       , input$dist.4.kill.H)) / 10
                                        , KilledIndiv_C = as.numeric(c(input$dist.1.kill.C
                                                                       , input$dist.2.kill.C
                                                                       , input$dist.3.kill.C
                                                                       , input$dist.4.kill.C)) / 10
                                        , KilledIndiv_P = as.numeric(c(input$dist.1.kill.P
                                                                       , input$dist.2.kill.P
                                                                       , input$dist.3.kill.P
                                                                       , input$dist.4.kill.P)) / 10
                                        , ResproutIndiv_H = as.numeric(c(input$dist.1.resprout.H
                                                                         , input$dist.2.resprout.H
                                                                         , input$dist.3.resprout.H
                                                                         , input$dist.4.resprout.H)) / 10
                                        , ResproutIndiv_C = as.numeric(c(input$dist.1.resprout.C
                                                                         , input$dist.2.resprout.C
                                                                         , input$dist.3.resprout.C
                                                                         , input$dist.4.resprout.C)) / 10
                                        , ResproutIndiv_P = as.numeric(c(input$dist.1.resprout.P
                                                                         , input$dist.2.resprout.P
                                                                         , input$dist.3.resprout.P
                                                                         , input$dist.4.resprout.P)) / 10
                           ))
  } else
  {
    
  }
})

observeEvent(input$delete.PFG.dist, {
  RV$mat.PFG.dist <- data.frame()
})

observeEvent(RV$mat.PFG.dist, {
  if (nrow(RV$mat.PFG.dist) > 0)
  {
    shinyjs::enable("create.dist")
  } else
  {
    shinyjs::disable("create.dist")
  }
})

####################################################################

output$created_table.dist = renderDataTable({
  path_folder = paste0(input$name.simul, "/DATA/PFGS/DIST/")
  return(get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE))
})

observeEvent(input$create.dist, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGdisturbance(name.simulation = input$name.simul
                                     , mat.PFG.dist = RV$mat.PFG.dist
                                     , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/DIST/"))
    
    if (as.character(get_res) != "0")
    {
      output$created_table.dist = renderDataTable({
        path_folder = paste0(input$name.simul, "/DATA/PFGS/DIST/")
        return(get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE))
      })
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
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
