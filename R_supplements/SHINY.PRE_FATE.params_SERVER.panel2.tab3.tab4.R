
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
    
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

get_tab.soil = eventReactive(paste(input$name.simul
                                     , input$create.soil
                                     , RV$compt.soil.nb), {
                                       if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                       {
                                         path_folder = paste0(input$name.simul, "/DATA/PFGS/SOIL/")
                                         tab = get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE)
                                         
                                         if (!is.null(tab) && ncol(tab) > 0)
                                         {
                                           RV$compt.soil.nb = ncol(tab)
                                           RV$compt.soil.files = colnames(tab)
                                           return(tab)
                                         }
                                       }
                                     })

output$UI.files.soil = renderUI({
  tab = get_tab.soil()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.soil.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.soil.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = HTML(paste(button.style, "margin-bottom: 3px;"))))
        , column(3
                 , actionButton(inputId = "delete.soil.select"
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
                          checkboxInput(inputId = paste0("check.soil.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        # , column(2
        #          , lapply(1:ncol(tab)
        #                   , function(i) {
        #                     actionButton(inputId = paste0("upload.soil.", colnames(tab)[i])
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

# observeEvent(RV$compt.soil.nb, {
#   for (i in 1:RV$compt.soil.nb)
#   {
#     observeEvent(input[[paste0("upload.soil.", RV$compt.soil.files[i])]], {
#       get_update.soil(file.soilParam = paste0(input$name.simul
#                                                   , "/DATA/PFGS/SOIL/"
#                                                   , RV$compt.soil.files[i]))
#     })
#   }
# })


observeEvent(input$check.soil.all, {
  for (col_tab in RV$compt.soil.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.soil.", col_tab)
                        , value = input$check.soil.all)
  }
})

observeEvent(input$view.soil.select, {
  output$created_table.soil = renderDataTable({
    req(grep(pattern = "check.soil.", x = names(input), value = TRUE))
    
    tab = get_tab.soil()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.soil.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
        {
          eval(parse(text = paste0("res = input$check.soil.", colnames(tab)[i])))
          return(res)
        }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.soil.select, {
  if (input$check.soil.all)
  {
    col_toKeep = rep(TRUE,RV$compt.soil.nb)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.soil.nb, .combine = "c") %do%
    {
      eval(parse(text = paste0("res = input$check.soil.", RV$compt.soil.files[i])))
      return(res)
    }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.soilParam = RV$compt.soil.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/PFGS/SOIL/ \n")
                               , paste0(gsub("__", "/", file.soilParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.soilParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/PFGS/SOIL/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/PFGS/SOIL/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.soil.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.soil.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.soil.nb = min(0, RV$compt.soil.nb - sum(col_toKeep))
                 }
               })
  }
})

