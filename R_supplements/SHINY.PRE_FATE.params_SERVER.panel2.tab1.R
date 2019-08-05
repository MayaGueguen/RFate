

####################################################################

output$UI.doHabSuitability = renderUI({
  if (input$doHabSuitability)
  {
    selectInput(inputId = "HABSUIT.ref_option"
                , label = ""
                , choices = c("(1) random", "(2) PFG specific")
                , selected = "(1) random"
                , multiple = FALSE
                , width = "100%")
  } 
})

####################################################################

output$UI.doDisturbances = renderUI({
  if (input$doDisturbances)
  {
    column(12
           , numericInput(inputId = "DIST.no"
                          , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>DIST.no</span>")
                          , min = 1
                          , value = 1
                          , width = "100%")
           , numericInput(inputId = "DIST.no_sub"
                          , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>DIST.no_sub</span>")
                          , min = 4
                          , max = 4
                          , value = 1
                          , width = "100%")
           , numericInput(inputId = "DIST.freq"
                          , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>DIST.freq</span>")
                          , min = 1
                          , value = 1
                          , width = "100%")
    )
  }
})

####################################################################

output$UI.doLight = renderUI({
  if (input$doLight)
  {
    column(12
           , numericInput(inputId = "LIGHT.thresh_medium"
                          , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>LIGHT.thresh_medium</span>")
                          , min = 1
                          , value = 1
                          , width = "100%")
           , numericInput(inputId = "LIGHT.thresh_low"
                          , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>LIGHT.thresh_low</span>")
                          , min = 4
                          , max = 4
                          , value = 1
                          , width = "100%")
    )
  }
})


####################################################################

observeEvent(input$create.global, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_globalParameters(name.simulation = input$name.simul
                                       , opt.no_CPU = input$opt.no_CPU
                                       , required.no_PFG = input$required.no_PFG
                                       , required.no_STRATA = input$required.no_STRATA
                                       , required.simul_duration = input$required.simul_duration
                                       , required.seeding_duration = input$required.seeding_duration
                                       , required.seeding_timestep = input$required.seeding_timestep
                                       , required.seeding_input = input$required.seeding_input
                                       , required.max_by_cohort = input$required.max_by_cohort
                                       , required.max_abund_low = input$required.max_abund_low
                                       , required.max_abund_medium = input$required.max_abund_medium
                                       , required.max_abund_high = input$required.max_abund_high
                                       , doDispersal = input$doDispersal
                                       , doHabSuitability = input$doHabSuitability
                                       , HABSUIT.ref_option = ifelse(input$HABSUIT.ref_option == "(1) random", 1, 2)
                                       , doLight = input$doLight
                                       , LIGHT.thresh_medium = input$LIGHT.thresh_medium
                                       , LIGHT.thresh_low = input$LIGHT.thresh_low
                                       , doSoil = input$doSoil
                                       , doDisturbances = input$doDisturbances
                                       , DIST.no = input$DIST.no
                                       , DIST.no_sub = input$DIST.no_sub
                                       , DIST.freq = rep(input$DIST.freq, input$DIST.no)
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/"))
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

get_tab.global = eventReactive(paste(input$name.simul
                                     , input$create.global
                                     , RV$compt.global.nb), {
                                       if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                       {
                                         path_folder = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/")
                                         tab = get_files(path_folder)
                                         
                                         if (!is.null(tab) && ncol(tab) > 0)
                                         {
                                           RV$compt.global.nb = ncol(tab)
                                           RV$compt.global.files = colnames(tab)
                                           return(tab)
                                         }
                                       }
                                     })

output$UI.files.global = renderUI({
  tab = get_tab.global()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.global.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.global.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = HTML(paste(button.style, "margin-bottom: 3px;"))))
        , column(3
                 , actionButton(inputId = "delete.global.select"
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
                          checkboxInput(inputId = paste0("check.global.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        , column(2
                 , lapply(1:ncol(tab)
                          , function(i) {
                            actionButton(inputId = paste0("upload.global.", colnames(tab)[i])
                                         , label = NULL
                                         , icon = icon("upload")
                                         , width = "100%"
                                         , style = HTML(paste(button.style, "margin-bottom: 3px;")))
                          })
        )
      )
    )
  }
})

observeEvent(RV$compt.global.nb, {
  for (i in 1:RV$compt.global.nb)
  {
    observeEvent(input[[paste0("upload.global.", RV$compt.global.files[i])]], {
      get_update.global(file.globalParam = paste0(input$name.simul
                                                  , "/DATA/GLOBAL_PARAMETERS/"
                                                  , RV$compt.global.files[i]))
    })
  }
})


observeEvent(input$check.global.all, {
  for (col_tab in RV$compt.global.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.global.", col_tab)
                        , value = input$check.global.all)
  }
})

observeEvent(input$view.global.select, {
  output$created_table.global = renderDataTable({
    req(grep(pattern = "check.global.", x = names(input), value = TRUE))
    
    tab = get_tab.global()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.global.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
        {
          eval(parse(text = paste0("res = input$check.global.", colnames(tab)[i])))
          return(res)
        }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.global.select, {
  if (input$check.global.all)
  {
    col_toKeep = rep(TRUE,RV$compt.global.nb)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.global.nb, .combine = "c") %do%
    {
      eval(parse(text = paste0("res = input$check.global.", RV$compt.global.files[i])))
      return(res)
    }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.globalParam = RV$compt.global.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/ \n")
                               , paste0(gsub("__", "/", file.saveParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.globalParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.global.", file.globalParam)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.global.", file.globalParam)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.global.nb = min(0, RV$compt.global.nb - sum(col_toKeep))
                 }
               })
  }
})

