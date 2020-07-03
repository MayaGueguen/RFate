
####################################################################

output$UI.dist.grouping = renderUI({
  max.stage = ifelse(req(input$DIST.no_sub), input$DIST.no_sub, 4)
  
  tagList(
    fluidRow(
      column(2, br())
      , lapply(1:max.stage, function(i)
      {
        column(trunc(10/max.stage), HTML(paste0("<strong>Stage ", i, "</strong>")))
      })
    )
    , fluidRow(
      column(2, br())
      , lapply(1:max.stage, function(i)
      {
        column(trunc(10/max.stage)
               , fluidRow(
                 column(6, HTML("<strong>Killed</strong>"), br(), br())
                 , column(6, HTML("<strong>Resprout</strong>"), br(), br())
               ))
      })
    )
    , fluidRow(uiOutput(outputId = "UI.dist.grouping.BIS"))
  )
})

output$UI.dist.grouping.BIS = renderUI({
  if (input$dist.grouping == "by type")
  {
    names.col = c("H", "C", "P")
  } else
  {
    names.col = RV$names.PFG
  }
  
  if (length(names.col) > 0)
  {
    max.stage = ifelse(req(input$DIST.no_sub), input$DIST.no_sub, 4)
    
    lapply(names.col, function(j)
    {
      fluidRow(
        column(2, HTML(paste0("<strong>", j, "</strong>")))
        , lapply(as.vector(sapply(1:max.stage, function(x) paste0("dist.", x, ".", c("kill", "resprout"), ".", j)))
                 , function(i) {
                   column(trunc(10/max.stage) / 2
                          , selectInput(inputId = i
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
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
      max.stage = ifelse(req(input$DIST.no_sub), input$DIST.no_sub, 4)
      res = data.frame(name = input$dist.name, responseStage = 1:max.stage)
      for (group in name.cols)
      {
        eval(parse(text = paste0("res$KilledIndiv_", group, " = as.numeric(c("
                                 , paste0("input$dist.", 1:max.stage, ".kill.", group, collapse = " , ")
                                 , ")) / 10"
        )))
        eval(parse(text = paste0("res$ResproutIndiv_", group, " = as.numeric(c("
                                 , paste0("input$dist.", 1:max.stage, ".resprout.", group, collapse = " , ")
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

observeEvent(input$create.dist, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGdisturbance(name.simulation = input$name.simul
                                     , mat.PFG.dist = RV$mat.PFG.dist
                                     , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/DIST/"))
    
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

get_tab.dist = eventReactive(paste(input$name.simul
                                     , input$create.dist
                                     , RV$compt.dist.nb), {
                                       if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                       {
                                         path_folder = paste0(input$name.simul, "/DATA/PFGS/DIST/")
                                         tab = get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE)
                                         
                                         if (!is.null(tab) && ncol(tab) > 0)
                                         {
                                           RV$compt.dist.nb = ncol(tab)
                                           RV$compt.dist.files = colnames(tab)
                                           return(tab)
                                         }
                                       }
                                     })

output$UI.files.dist = renderUI({
  tab = get_tab.dist()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.dist.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.dist.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = HTML(button.style.action)))
        , column(3
                 , actionButton(inputId = "delete.dist.select"
                                , label = "Delete selected"
                                , icon = icon("trash-alt")
                                , width = "100%"
                                , style = HTML(button.style.action)))
      ),
      hr(),
      fluidRow(
        column(10
               , lapply(1:ncol(tab)
                        , function(i) {
                          checkboxInput(inputId = paste0("check.dist.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        # , column(2
        #          , lapply(1:ncol(tab)
        #                   , function(i) {
        #                     actionButton(inputId = paste0("upload.dist.", colnames(tab)[i])
        #                                  , label = NULL
        #                                  , icon = icon("upload")
        #                                  , width = "100%"
        #                                  , style = HTML(button.style.action))
        #                   })
        # )
      )
    )
  }
})

# observeEvent(RV$compt.dist.nb, {
#   for (i in 1:RV$compt.dist.nb)
#   {
#     observeEvent(input[[paste0("upload.dist.", RV$compt.dist.files[i])]], {
#       get_update.dist(file.distParam = paste0(input$name.simul
#                                                   , "/DATA/PFGS/DIST/"
#                                                   , RV$compt.dist.files[i]))
#     })
#   }
# })


observeEvent(input$check.dist.all, {
  for (col_tab in RV$compt.dist.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.dist.", col_tab)
                        , value = input$check.dist.all)
  }
})

observeEvent(input$view.dist.select, {
  output$created_table.dist = renderDataTable({
    req(grep(pattern = "check.dist.", x = names(input), value = TRUE))
    
    tab = get_tab.dist()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.dist.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
        {
          eval(parse(text = paste0("res = input$check.dist.", colnames(tab)[i])))
          return(res)
        }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.dist.select, {
  if (input$check.dist.all)
  {
    col_toKeep = rep(TRUE,RV$compt.dist.nb)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.dist.nb, .combine = "c") %do%
    {
      eval(parse(text = paste0("res = input$check.dist.", RV$compt.dist.files[i])))
      return(res)
    }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.distParam = RV$compt.dist.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/PFGS/DIST/ \n")
                               , paste0(gsub("__", "/", file.distParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.distParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/PFGS/DIST/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/PFGS/DIST/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.dist.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.dist.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.dist.nb = min(0, RV$compt.dist.nb - sum(col_toKeep))
                 }
               })
  }
})

