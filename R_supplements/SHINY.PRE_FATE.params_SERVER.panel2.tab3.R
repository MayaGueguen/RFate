
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

observeEvent(input$name.PFG, {
  if (nchar(input$name.PFG) > 0)
  {
    shinyjs::enable("add.PFG.name")
  } else
  {
    shinyjs::disable("add.PFG.name")
  }
  if (length(names.PFG) == 0)
  {
    output$names.PFG = renderText({ "PFG list : " })
  }
})

observeEvent(input$add.PFG.name, {
  if (input$name.PFG %in% names.PFG)
  {
    shinyalert(type = "warning", text = "You must give different PFG names !")
    shinyjs::reset("name.PFG")
  } else
  {
    names.PFG <<- c(names.PFG, input$name.PFG)
    output$names.PFG = renderText({
      paste0("PFG list : ", paste0(names.PFG, collapse = " "))
    })
    updateSelectInput(session
                      , inputId = "succ.PFG"
                      , choices = names.PFG
                      , selected = names.PFG[1])
    updateSelectInput(session
                      , inputId = "disp.PFG"
                      , choices = names.PFG
                      , selected = names.PFG[1])
    updateSelectInput(session
                      , inputId = "soil.PFG"
                      , choices = names.PFG
                      , selected = names.PFG[1])
    
    shinyjs::reset("name.PFG")
    shinyjs::enable("succ.PFG")
    shinyjs::enable("add.PFG.succ")
    shinyjs::enable("disp.PFG")
    shinyjs::enable("add.PFG.disp")
    shinyjs::enable("soil.PFG")
    shinyjs::enable("add.PFG.soil")
  }
})

observeEvent(input$delete.names.PFG, {
  names.PFG <<- c()
  output$names.PFG = renderText({ "PFG list : " })
  
  shinyjs::reset("name.PFG")
  shinyjs::reset("succ.PFG")
  shinyjs::reset("disp.PFG")
  shinyjs::reset("soil.PFG")
  shinyjs::disable("succ.PFG")
  shinyjs::disable("add.PFG.succ")
  shinyjs::disable("disp.PFG")
  shinyjs::disable("add.PFG.disp")
  shinyjs::disable("soil.PFG")
  shinyjs::disable("add.PFG.soil")
})

####################################################################

observeEvent(input$add.PFG.succ, {
  mat.PFG.ALL <<- rbind(mat.PFG.ALL
                        , data.frame(PFG = input$succ.PFG
                                     , type = input$succ.type
                                     , height = as.numeric(input$succ.height)
                                     , maturity = as.numeric(input$succ.maturity)
                                     , longevity = as.numeric(input$succ.longevity)
                                     , light = as.numeric(input$succ.light)
                        ))
  output$mat.PFG.ALL = renderTable({ mat.PFG.ALL })
  
  shinyjs::enable("create.succ")
  shinyjs::enable("create.light")
})

observeEvent(input$delete.PFG.ALL, {
  mat.PFG.ALL <<- data.frame()
  output$mat.PFG.ALL = renderTable({ mat.PFG.ALL })
  
  shinyjs::disable("create.succ")
  shinyjs::disable("create.light")
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
                                    , mat.PFG.succ = mat.PFG.ALL[, c("PFG", "type", "height", "maturity", "longevity")]
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
                               , mat.PFG.succ = mat.PFG.ALL[, c("PFG", "type", "height", "maturity", "longevity", "light")]
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

observeEvent(input$add.PFG.disp, {
  mat.PFG.disp <<- rbind(mat.PFG.disp
                         , data.frame(PFG = input$disp.PFG
                                      , MODE = as.numeric(input$disp.mode)
                                      , d50 = as.numeric(input$disp.d50)
                                      , d99 = as.numeric(input$disp.d99)
                                      , ldd = as.numeric(input$disp.ldd)))
  output$mat.PFG.disp = renderTable({ mat.PFG.disp })
  
  shinyjs::enable("create.disp")
})

observeEvent(input$delete.PFG.disp, {
  mat.PFG.disp <<- data.frame()
  output$mat.PFG.disp = renderTable({ mat.PFG.disp })
  
  shinyjs::disable("create.disp")
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
                                   , mat.PFG.disp = mat.PFG.disp
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
  if (input$dist.grouping == "by type")
  {
    fluidRow(
      column(6
             , br()
             , fluidRow(
               column(4, HTML(""))
               , column(4, HTML(""))
               , column(4, HTML("<strong>H</strong>"))
             )
             , fluidRow(
               column(4, HTML("<strong> Stage 1</strong>"))
               , column(4, HTML("<strong>Killed</strong>"))
               , column(4
                        , selectInput(inputId = "dist.1.kill.H"
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML(""))
               , column(4, HTML("<strong>Resprout</strong>"))
               , column(4
                        , selectInput(inputId = "dist.1.resprout.H"
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML("<strong> Stage 2</strong>"))
               , column(4, HTML("<strong>Killed</strong>"))
               , column(4
                        , selectInput(inputId = "dist.2.kill.H"
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML(""))
               , column(4, HTML("<strong>Resprout</strong>"))
               , column(4
                        , selectInput(inputId = "dist.2.resprout.H"
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML("<strong> Stage 3</strong>"))
               , column(4, HTML("<strong>Killed</strong>"))
               , column(4
                        , selectInput(inputId = "dist.3.kill.H"
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML(""))
               , column(4, HTML("<strong>Resprout</strong>"))
               , column(4
                        , selectInput(inputId = "dist.3.resprout.H"
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML("<strong> Stage 4</strong>"))
               , column(4, HTML("<strong>Killed</strong>"))
               , column(4
                        , selectInput(inputId = "dist.4.kill.H"
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             )
             , fluidRow(
               column(4, HTML(""))
               , column(4, HTML("<strong>Resprout</strong>"))
               , column(4
                        , selectInput(inputId = "dist.4.resprout.H"
                                      , label = NULL
                                      , choices = seq(0,100,10)
                                      , multiple = FALSE
                                      , width = "100%"))
             ))
      , column(2
               , br()
               , HTML("<strong>C</strong>")
               , selectInput(inputId = "dist.1.kill.C"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.1.resprout.C"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.2.kill.C"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.2.resprout.C"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.3.kill.C"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.3.resprout.C"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.4.kill.C"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.4.resprout.C"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
      )
      , column(2
               , br()
               , HTML("<strong>P</strong>")
               , selectInput(inputId = "dist.1.kill.P"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.1.resprout.P"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.2.kill.P"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.2.resprout.P"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.3.kill.P"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.3.resprout.P"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.4.kill.P"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
               , selectInput(inputId = "dist.4.resprout.P"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , multiple = FALSE
                             , width = "100%")
      )
    )
  } else
  {
    # names.PFG = list.files(path = paste0(input$name.simul, "/DATA/PFGS/SUCC/")
    #                        , pattern = "^SUCC_")
    # names.PFG = sub("^SUCC_", "", names.PFG)
    # names.PFG = sub(".txt$", "", names.PFG)
  }
})

####################################################################

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
    mat.PFG.dist <<- rbind(mat.PFG.dist
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
  output$mat.PFG.dist = renderTable({ mat.PFG.dist })
  
  shinyjs::enable("create.dist")
})

observeEvent(input$delete.PFG.dist, {
  mat.PFG.dist <<- data.frame()
  output$mat.PFG.dist = renderTable({ mat.PFG.dist })
  
  shinyjs::disable("create.dist")
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
                                     , mat.PFG.dist = mat.PFG.dist
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

observeEvent(input$add.PFG.soil, {
  mat.PFG.soil <<- rbind(mat.PFG.soil
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
  output$mat.PFG.soil = renderTable({ mat.PFG.soil })
  
  shinyjs::enable("create.soil")
})

observeEvent(input$delete.PFG.soil, {
  mat.PFG.soil <<- data.frame()
  output$mat.PFG.soil = renderTable({ mat.PFG.soil })
  
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
                              , mat.PFG.soil = unique(mat.PFG.soil[, c("PFG", "type", "soil_contrib", "soil_tol_min", "soil_tol_max")])
                              , mat.PFG.tol = mat.PFG.soil[, c("PFG", "lifeStage", "soilResources", "soil_tol")]
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
