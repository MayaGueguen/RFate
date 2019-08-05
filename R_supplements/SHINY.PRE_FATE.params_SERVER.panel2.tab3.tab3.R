
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
  if ((input$dist.grouping == "by type" && RV$compt.dist.by_pfg) ||
      (input$dist.grouping == "by PFG" && RV$compt.dist.by_type))
  {
    shinyalert(type = "warning", text = "You can not mix 'by type' and 'by PFG' !")
  } else
  {
    name.cols = ifelse(input$dist.grouping == "by type", list(c("H", "C", "P")), list(RV$names.PFG))
    if (length(name.cols) > 0)
    {
      res = data.frame(name = input$dist.name, responseStage = 1:4)
      for (group in name.cols)
      {
        eval(parse(text = paste0("res$KilledIndiv_", group, " = as.numeric(c("
                                 , paste0("input$dist.", 1:4, ".kill.", group, collapse = " , ")
                                 , ")) / 10"
        )))
        eval(parse(text = paste0("res$ResproutIndiv_", group, " = as.numeric(c("
                                 , paste0("input$dist.", 1:4, ".resprout.", group, collapse = " , ")
                                 , ")) / 10"
        )))
      }
      RV$mat.PFG.dist <- rbind(RV$mat.PFG.dist, res)
      
      if (input$dist.grouping == "by type")
      {
        RV$compt.dist.by_type <- TRUE
      } else
      {
        RV$compt.dist.by_pfg <- TRUE
      }
    }
  }
})

observeEvent(input$delete.PFG.dist, {
  RV$mat.PFG.dist <- data.frame()
  RV$compt.dist.by_type <- FALSE
  RV$compt.dist.by_pfg <- FALSE
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

