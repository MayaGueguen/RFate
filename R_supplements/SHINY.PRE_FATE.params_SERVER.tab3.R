
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
    
    shinyjs::reset("name.PFG")
    shinyjs::enable("succ.PFG")
    shinyjs::enable("add.PFG.succ")
    shinyjs::enable("disp.PFG")
    shinyjs::enable("add.PFG.disp")
  }
})

observeEvent(input$delete.names.PFG, {
  names.PFG <<- c()
  output$names.PFG = renderText({ "PFG list : " })
  
  shinyjs::reset("name.PFG")
  shinyjs::disable("succ.PFG")
  shinyjs::disable("add.PFG.succ")
  shinyjs::disable("disp.PFG")
  shinyjs::disable("add.PFG.disp")
})

####################################################################

observeEvent(input$add.PFG.succ, {
  mat.PFG.succ <<- rbind(mat.PFG.succ
                         , data.frame(PFG = input$succ.PFG
                                      , type = input$succ.type
                                      , height = as.numeric(input$succ.height)
                                      , maturity = as.numeric(input$succ.maturity)
                                      , longevity = as.numeric(input$succ.longevity)
                         ))
  output$mat.PFG.succ = renderTable({ mat.PFG.succ })
  
  shinyjs::enable("create.succ")
})

observeEvent(input$delete.PFG.succ, {
  mat.PFG.succ <<- data.frame()
  output$mat.PFG.succ = renderTable({ mat.PFG.succ })
  
  shinyjs::disable("create.succ")
})

####################################################################

observeEvent(input$create.succ, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGsuccession(name.simulation = input$name.simul
                                    , mat.PFG.succ = mat.PFG.succ
      )
    ))
    
    if(get_res)
    {
      output$created_table.succ = renderDataTable({
        path_folder = paste0(input$name.simul, "/DATA/PFGS/SUCC/")
        return(get_files(path_folder))
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

observeEvent(input$create.disp, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGdispersal(name.simulation = input$name.simul
                                   , mat.PFG.disp = mat.PFG.disp
      )
    ))
    
    if(get_res)
    {
      output$created_table.disp = renderDataTable({
        path_folder = paste0(input$name.simul, "/DATA/PFGS/DISP/")
        return(get_files(path_folder))
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

observeEvent(input$create.dist, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGdisturbance(name.simulation = input$name.simul
                                     , mat.PFG.dist = mat.PFG.dist
      )
    ))
    
    if(get_res)
    {
      output$created_table.dist = renderDataTable({
        path_folder = paste0(input$name.simul, "/DATA/PFGS/DIST/")
        return(get_files(path_folder))
      })
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})


