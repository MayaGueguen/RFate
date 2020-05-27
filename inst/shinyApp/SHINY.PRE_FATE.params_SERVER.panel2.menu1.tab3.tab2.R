
####################################################################

output$UI.light.PFG = renderUI({
  if (length(RV$names.PFG) == 0)
  {
    shinyjs::disabled(
      selectInput(inputId = "light.PFG"
                  , label = NULL
                  , choices = RV$names.PFG
                  , selected = RV$names.PFG[1]
                  , multiple = F
                  , width = "100%")
    )
  } else
  {
    selectInput(inputId = "light.PFG"
                , label = NULL
                , choices = RV$names.PFG[which(!(RV$names.PFG %in% RV$mat.PFG.light$PFG))]
                , selected = RV$names.PFG[which(!(RV$names.PFG %in% RV$mat.PFG.light$PFG))][1]
                , multiple = F
                , width = "100%")
  }
})

####################################################################

# observeEvent(input$light.light, {
#   if (input$light.light != ".")
#   {
#     updateSelectInput(session
#                       , inputId = "light.strategy_tol"
#                       , choices = c(".", "full_light", "pioneer", "ubiquist"
#                                     , "semi_shade", "undergrowth")
#                       , selected = ".")
#   }
# })
# 
# observeEvent(input$light.strategy_tol, {
#   if (input$light.strategy_tol != ".")
#   {
#     updateSelectInput(session
#                       , inputId = "light.light"
#                       , choices = c(".", 0:5)
#                       , selected = ".")
#   }
# })

####################################################################

output$UI.light.opt.ag = renderUI({
  if (input$light.opt.ag == "by strategy")
  {
    column(4, selectInput(inputId = "light.strategy_ag"
                          , label = NULL
                          , choices = c("light_lover", "indifferent", "shade_lover")
                          , selected = "indifferent"
                          , multiple = F
                          , width = "100%"))
  } else if (input$light.opt.ag == "user-defined")
  {
    fluidRow(
      column(4
             , HTML("<strong>Low</strong>")
             , selectInput(inputId = "light.Ge.L.act"
                           , label = NULL
                           , choices = seq(0,100,10)
                           , selected = 100
                           , multiple = FALSE
                           , width = "100%"))
      , column(4
               , HTML("<strong>Medium</strong>")
               , selectInput(inputId = "light.Ge.M.act"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , selected = 100
                             , multiple = FALSE
                             , width = "100%"))
      , column(4
               , HTML("<strong>High</strong>")
               , selectInput(inputId = "light.Ge.H.act"
                             , label = NULL
                             , choices = seq(0,100,10)
                             , selected = 100
                             , multiple = FALSE
                             , width = "100%"))
    )
  }
})

####################################################################

output$UI.light.opt.tol = renderUI({
  if (input$light.opt.tol == "by strategy")
  {
    column(4, selectInput(inputId = "light.strategy_tol"
                          , label = NULL
                          , choices = c("full_light", "pioneer", "ubiquist"
                                        , "semi_shade", "undergrowth")
                          , selected = "ubiquist"
                          , multiple = F
                          , width = "100%"))
  } else if (input$light.opt.tol == "user-defined")
  {
    name.1st_col = ""
    if (length(RV$names.PFG) > 0)
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
                 column(4, HTML("<strong> Germinant</strong>"))
                 , column(4, HTML("<strong>Low</strong>"))
                 , column(4
                          , selectInput(inputId = paste0("light.Ge.L.tol.", name.1st_col)
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML(""))
                 , column(4, HTML("<strong>Medium</strong>"))
                 , column(4
                          , selectInput(inputId = paste0("light.Ge.M.tol.", name.1st_col)
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML(""))
                 , column(4, HTML("<strong>High</strong>"))
                 , column(4
                          , selectInput(inputId = paste0("light.Ge.H.tol.", name.1st_col)
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML("<strong> Immature</strong>"))
                 , column(4, HTML("<strong>Low</strong>"))
                 , column(4
                          , selectInput(inputId = paste0("light.Im.L.tol.", name.1st_col)
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML(""))
                 , column(4, HTML("<strong>Medium</strong>"))
                 , column(4
                          , selectInput(inputId = paste0("light.Im.M.tol.", name.1st_col)
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML(""))
                 , column(4, HTML("<strong>High</strong>"))
                 , column(4
                          , selectInput(inputId = paste0("light.Im.H.tol.", name.1st_col)
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML("<strong> Mature</strong>"))
                 , column(4, HTML("<strong>Low</strong>"))
                 , column(4
                          , selectInput(inputId = paste0("light.Ma.L.tol.", name.1st_col)
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML(""))
                 , column(4, HTML("<strong>Medium</strong>"))
                 , column(4
                          , selectInput(inputId = paste0("light.Ma.M.tol.", name.1st_col)
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML(""))
                 , column(4, HTML("<strong>High</strong>"))
                 , column(4
                          , selectInput(inputId = paste0("light.Ma.H.tol.", name.1st_col)
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               
        )
        , uiOutput(outputId = "UI.light.opt.tol.BIS")
      ) ## END fluidRow
    }
  }
})

output$UI.light.opt.tol.BIS = renderUI({
  name.2nd_col = vector()
  if (length(RV$names.PFG) > 1)
  {
    name.2nd_col = RV$names.PFG[2:length(RV$names.PFG)]
  }
  
  if (length(name.2nd_col) > 0)
  {
    lapply(name.2nd_col, function(j) {
      column(2
             , br()
             , HTML(paste0("<strong>", j, "</strong>"))
             , lapply(as.vector(sapply(c("Ge", "Im", "Ma")
                                       , function(x) paste0("light.", x, ".", c("L", "M", "H"), ".", j)))
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

output$mat.PFG.light = renderTable({ RV$mat.PFG.light })

observeEvent(input$add.PFG.light, {
  req(input$light.PFG)
  RV$mat.PFG.light <- rbind(RV$mat.PFG.light
                            , data.frame(PFG = input$light.PFG
                                         , type = input$light.type
                                         , light_need = as.numeric(input$light.light)
                                         , strategy_ag = input$light.strategy_ag
                                         , strategy_tol = input$light.strategy_tol
                            ))
})

observeEvent(input$delete.PFG.light, {
  RV$mat.PFG.light <- data.frame()
})

observeEvent(RV$mat.PFG.light, {
  if (nrow(RV$mat.PFG.light) > 0)
  {
    shinyjs::enable("create.light")
  } else
  {
    shinyjs::disable("create.light")
  }
})


####################################################################

observeEvent(input$create.light, {
  if (input$create.skeleton > 0)
  {
    col.light = c("PFG", ifelse(length(which(RV$mat.PFG.light$strategy_ag == ".")) == nrow(RV$mat.PFG.light)
                                , "type", "strategy_ag"))
    if (length(which(RV$mat.PFG.light$strategy_tol == ".")) == nrow(RV$mat.PFG.light))
    {
      col.light = c(col.light, c("type", "light_need"))
      mat.tol = NULL
    } else
    {
      mat.tol = RV$mat.PFG.light[, c("PFG", "strategy_tol")]
    }
    
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGlight(name.simulation = input$name.simul
                               , mat.PFG.light = RV$mat.PFG.light[, unique(col.light)]
                               , mat.PFG.tol = mat.tol
                               , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/LIGHT/"))
    
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})


####################################################################

get_tab.light = eventReactive(paste(input$name.simul
                                    , input$create.light
                                    , RV$compt.light.nb), {
                                      if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                      {
                                        path_folder = paste0(input$name.simul, "/DATA/PFGS/LIGHT/")
                                        tab = get_files(path_folder, skip.no = 2, opt.sub_folder = TRUE)
                                        
                                        if (!is.null(tab) && ncol(tab) > 0)
                                        {
                                          RV$compt.light.nb = ncol(tab)
                                          RV$compt.light.files = colnames(tab)
                                          return(tab)
                                        }
                                      }
                                    })

output$UI.files.light = renderUI({
  tab = get_tab.light()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.light.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.light.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = HTML(paste(button.style, "margin-bottom: 3px;"))))
        , column(3
                 , actionButton(inputId = "delete.light.select"
                                , label = "Delete selected"
                                , icon = icon("trash-alt")
                                , width = "100%"
                                , style = HTML(paste(button.style, "margin-bottom: 3px;"))))
      ),
      hr(),
      fluidRow(
        column(10
               , lapply(1:ncol(tab)
                        , function(i) {
                          checkboxInput(inputId = paste0("check.light.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        # , column(2
        #          , lapply(1:ncol(tab)
        #                   , function(i) {
        #                     actionButton(inputId = paste0("upload.light.", colnames(tab)[i])
        #                                  , label = NULL
        #                                  , icon = icon("upload")
        #                                  , width = "100%"
        #                                  , style = HTML(paste(button.style, "margin-bottom: 3px;")))
        #                   })
        # )
      )
    )
  }
})

# observeEvent(RV$compt.light.nb, {
#   for (i in 1:RV$compt.light.nb)
#   {
#     observeEvent(input[[paste0("upload.light.", RV$compt.light.files[i])]], {
#       get_update.light(file.lightParam = paste0(input$name.simul
#                                                   , "/DATA/PFGS/LIGHT/"
#                                                   , RV$compt.light.files[i]))
#     })
#   }
# })


observeEvent(input$check.light.all, {
  for (col_tab in RV$compt.light.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.light.", col_tab)
                        , value = input$check.light.all)
  }
})

observeEvent(input$view.light.select, {
  output$created_table.light = renderDataTable({
    req(grep(pattern = "check.light.", x = names(input), value = TRUE))
    
    tab = get_tab.light()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.light.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
        {
          eval(parse(text = paste0("res = input$check.light.", colnames(tab)[i])))
          return(res)
        }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.light.select, {
  if (input$check.light.all)
  {
    col_toKeep = rep(TRUE,RV$compt.light.nb)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.light.nb, .combine = "c") %do%
    {
      eval(parse(text = paste0("res = input$check.light.", RV$compt.light.files[i])))
      return(res)
    }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.lightParam = RV$compt.light.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/PFGS/LIGHT/ \n")
                               , paste0(gsub("__", "/", file.saveParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.lightParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/PFGS/LIGHT/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/PFGS/LIGHT/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.light.", file.lightParam)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.light.", file.lightParam)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.light.nb = min(0, RV$compt.light.nb - sum(col_toKeep))
                 }
               })
  }
})


