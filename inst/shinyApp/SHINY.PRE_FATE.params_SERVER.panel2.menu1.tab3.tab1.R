
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
                , choices = RV$names.PFG[which(!(RV$names.PFG %in% RV$mat.PFG.ALL$PFG))]
                , selected = RV$names.PFG[which(!(RV$names.PFG %in% RV$mat.PFG.ALL$PFG))][1]
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


observeEvent(input$create.succ, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGsuccession(name.simulation = input$name.simul
                                    , mat.PFG.succ = RV$mat.PFG.ALL[, c("PFG", "type", "height", "maturity", "longevity")]
                                    , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/SUCC/"))
    
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

observeEvent(input$create.light, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGlight(name.simulation = input$name.simul
                               , mat.PFG.succ = RV$mat.PFG.ALL[, c("PFG", "type", "height", "maturity", "longevity", "light")]
                               , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/LIGHT/"))
    
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

get_tab.succ = eventReactive(paste(input$name.simul
                                     , input$create.succ
                                     , RV$compt.succ.nb), {
                                       if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                       {
                                         path_folder = paste0(input$name.simul, "/DATA/PFGS/SUCC/")
                                         tab = get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE)
                                         
                                         if (!is.null(tab) && ncol(tab) > 0)
                                         {
                                           RV$compt.succ.nb = ncol(tab)
                                           RV$compt.succ.files = colnames(tab)
                                           return(tab)
                                         }
                                       }
                                     })

output$UI.files.succ = renderUI({
  tab = get_tab.succ()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.succ.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.succ.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = HTML(paste(button.style, "margin-bottom: 3px;"))))
        , column(3
                 , actionButton(inputId = "delete.succ.select"
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
                          checkboxInput(inputId = paste0("check.succ.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        # , column(2
        #          , lapply(1:ncol(tab)
        #                   , function(i) {
        #                     actionButton(inputId = paste0("upload.succ.", colnames(tab)[i])
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

# observeEvent(RV$compt.succ.nb, {
#   for (i in 1:RV$compt.succ.nb)
#   {
#     observeEvent(input[[paste0("upload.succ.", RV$compt.succ.files[i])]], {
#       get_update.succ(file.succParam = paste0(input$name.simul
#                                                   , "/DATA/PFGS/SUCC/"
#                                                   , RV$compt.succ.files[i]))
#     })
#   }
# })


observeEvent(input$check.succ.all, {
  for (col_tab in RV$compt.succ.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.succ.", col_tab)
                        , value = input$check.succ.all)
  }
})

observeEvent(input$view.succ.select, {
  output$created_table.succ = renderDataTable({
    req(grep(pattern = "check.succ.", x = names(input), value = TRUE))
    
    tab = get_tab.succ()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.succ.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
        {
          eval(parse(text = paste0("res = input$check.succ.", colnames(tab)[i])))
          return(res)
        }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.succ.select, {
  if (input$check.succ.all)
  {
    col_toKeep = rep(TRUE,RV$compt.succ.nb)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.succ.nb, .combine = "c") %do%
    {
      eval(parse(text = paste0("res = input$check.succ.", RV$compt.succ.files[i])))
      return(res)
    }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.succParam = RV$compt.succ.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/PFGS/SUCC/ \n")
                               , paste0(gsub("__", "/", file.succParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.succParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/PFGS/SUCC/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/PFGS/SUCC/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.succ.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.succ.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.succ.nb = min(0, RV$compt.succ.nb - sum(col_toKeep))
                 }
               })
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


