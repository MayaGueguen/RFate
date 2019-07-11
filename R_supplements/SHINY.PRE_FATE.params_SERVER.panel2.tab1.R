

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

# get_tab.global = eventReactive(paste(input$name.simul, input$create.global), { ## update
# get_tab.global = eventReactive(input$name.simul, { ## init
# get_tab.global = eventReactive(lapply(grep(pattern = "name.simul|create.global|^delete.global.",
#                                            x = names(input),
#                                            value = TRUE), function(x) input[[x]]), {
get_tab.global = eventReactive(paste(input$name.simul, RV$compt.global.nb), { ## init
  cat("\n CREATE TAB \n")
  
  path_folder = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/")
  tab = get_files(path_folder)
  
  RV$compt.global.nb = ncol(tab)
  return(tab)
})

output$UI.files.global = renderUI({
  cat("\n CREATE GLOBAL BUTTONS \n")
  
  tab = get_tab.global()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    fluidRow(
      column(8
             , lapply(1:ncol(tab)
                      , function(i) {
                        checkboxInput(inputId = paste0("check.global.", colnames(tab)[i])
                                      , label = colnames(tab)[i]
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
      , column(2
               , lapply(1:ncol(tab)
                        , function(i) {
                          actionButton(inputId = paste0("delete.global.", colnames(tab)[i])
                                       , label = NULL
                                       , icon = icon("trash-alt")
                                       , width = "100%"
                                       , style = HTML(paste(button.style, "margin-bottom: 3px;")))
                        })
      )
    )
  }
})

output$created_table.global = renderDataTable({
  req(grep(pattern = "check.global.", x = names(input), value = TRUE))
  
  print("GETRENDER")
  cat("\n")
  
  tab = get_tab.global()
  tab = as.data.frame(tab)
  
  
  col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
  {
    eval(parse(text = paste0("res = input$check.global.", colnames(tab)[i])))
    return(res)
  }
  print(dim(tab))
  print(col_toKeep)
  
  return(tab[, which(col_toKeep == TRUE), drop = FALSE])
})
# })

# observeEvent(get_tab.global(), {
# observeEvent(paste(get_tab.global(), RV$compt.global.nb), {
observeEvent(lapply(grep(pattern = "^check.global.",
            x = names(input),
            value = TRUE), function(x) input[[x]]), {
  tab = get_tab.global()

  lapply(1:ncol(tab)
         , function(i)
         {
           print("AA")
           req(grep(pattern = "upload.global.", x = names(input), value = TRUE))
           print("aa")
           observeEvent(input[[paste0("upload.global.", colnames(tab)[i])]], {
             get_update.global(file.globalParam = paste0(input$name.simul
                                                         , "/DATA/GLOBAL_PARAMETERS/"
                                                         , colnames(tab)[i]))
           })
           print("BB")
           req(grep(pattern = "delete.global.", x = names(input), value = TRUE))
           print("bb")
           observeEvent(input[[paste0("delete.global.", colnames(tab)[i])]], {
             print(i)
             file.globalParam = paste0(input$name.simul
                                       , "/DATA/GLOBAL_PARAMETERS/"
                                       , colnames(tab)[i])
             print(file.globalParam)
             shinyalert(type = "warning"
                        , text = paste0("The simulation parameter file "
                                        , sub("GLOBAL_PARAMETERS/", "GLOBAL_PARAMETERS/\n", file.globalParam)
                                        , " will be removed !\n"
                                        , "Make sure this is what you want.")
                        , showCancelButton = TRUE
                        , showConfirmButton = TRUE
                        , callbackR = function(x)
                        {
                          if (x)
                          {
                            removeUI(selector = paste0("check.global.", colnames(tab)[i])
                                     , multiple = FALSE
                                     , immediate = TRUE)
                            removeUI(selector = paste0("upload.global.", colnames(tab)[i])
                                     , multiple = FALSE
                                     , immediate = TRUE)
                            removeUI(selector = paste0("delete.global.", colnames(tab)[i])
                                     , multiple = FALSE
                                     , immediate = TRUE)
                            print("notremoved")
                            file.remove(file.globalParam)
                            print("removed!")
                            RV$compt.global.nb = min(0, RV$compt.global.nb - 1)
                          }
                        })
           })
         })
})

# observeEvent(RV$compt.global.nb, {
#   # tab = get_tab.global()
#   
#   # if (!is.null(tab) && ncol(tab) > 0)
#   if (RV$compt.global.nb > 0)
#   {
#     cat("\n CREATE GLOBAL OBSERVE\n")
#     
#     # lapply(1:ncol(tab)
#     lapply(1:RV$compt.global.nb
#            , function(i)
#            {
#              observeEvent(input[[paste0("upload.global.", RV$compt.global.files[i])]], {
#                get_update.global(file.globalParam = paste0(input$name.simul
#                                                            , "/DATA/GLOBAL_PARAMETERS/"
#                                                            , RV$compt.global.files[i]))
#              })
#              
#              observeEvent(input[[paste0("delete.global.", RV$compt.global.files[i])]], {
#                print(i)
#                file.globalParam = paste0(input$name.simul
#                                          , "/DATA/GLOBAL_PARAMETERS/"
#                                          , RV$compt.global.files[i])
#                print(file.globalParam)
#                shinyalert(type = "warning"
#                           , text = paste0("The simulation parameter file "
#                                           , sub("GLOBAL_PARAMETERS/", "GLOBAL_PARAMETERS/\n", file.globalParam)
#                                           , " will be removed !\n"
#                                           , "Make sure this is what you want.")
#                           , showCancelButton = TRUE
#                           , showConfirmButton = TRUE
#                           , callbackR = function(x)
#                           {
#                             if (x)
#                             {
#                               file.remove(file.globalParam)
#                               RV$compt.global.nb = RV$compt.global.nb - 1
#                             }
#                           })
#              })
#            })
#     # }
#     
#     output$created_table.global = renderDataTable({
#       print("GETRENDER")
#       cat("\n")
#       
#       col_toKeep = foreach(i = 1:RV$compt.global.nb, .combine = "c") %do%
#       {
#         print(paste0("res = input$check.global.", RV$compt.global.files[i]))
#         eval(parse(text = paste0("res = input$check.global.", RV$compt.global.files[i])))
#         cat("res : ", res, "\n")
#         return(res)
#       }
#       # print(dim(tab))
#       print(RV$compt.global.nb)
#       print(col_toKeep)
#       # path_folder = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/")
#       # tab = get_files(path_folder)
#       # tab = 
#       
#       return(RV$compt.global.tab[, which(col_toKeep == TRUE), drop = FALSE])
#     })
#   }
# })
















# xxchange <- reactive({
#   # paste(input$name.simul, RV$compt.global.nb)
#   # paste(input$name.simul, input$create.global, RV$compt.global.nb)
#   eval(parse(text = paste0("paste(input$name.simul, input$create.global, "
#                            , paste0('input$delete.global.', 1:RV$compt.global.nb, collapse = ", ")
#                            , ")")))
#   # eval(parse(text = paste0("paste(input$name.simul, input$create.global, "
#   #                          , paste0('input$delete.global.', RV$compt.global.files, collapse = ", ")
#   #                          , ")")))
# })

# get_tab.global = eventReactive(paste(input$name.simul, input$create.global), {
#   cat("\n CREATE TAB \n")
#   
#   path_folder = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/")
#   tab = get_files(path_folder)
#   return(tab)
# })
# 
# # path_folder = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/")
# # RV$compt.global.tab = get_files(path_folder)
# 
# # get_tab.global = reactive({
# #   get_files(paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/"))
# # })
# 
# observeEvent(input$create.global, {
#   output$UI.files.global = renderUI({
#     cat("\n CREATE GLOBAL BUTTONS \n")
#     
#     # tab = get_tab.global()
#     path_folder = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/")
#     tab = get_files(path_folder)
#     
#     RV$compt.global.nb = ifelse(is.null(tab), 0, ncol(tab))
#     if (is.null(tab))
#     {
#       RV$compt.global.files = c()
#     } else
#     {
#       RV$compt.global.files = colnames(tab)
#     }
#     RV$compt.global.tab = as.data.frame(tab)
#     # tab = RV$compt.global.tab
#     print(head(RV$compt.global.tab))
#     
#     if (!is.null(tab) && ncol(tab) > 0)
#     {
#       removeUI(selector = "#check.global."
#                , multiple = TRUE
#                , immediate = TRUE)
#       removeUI(selector = "#upload.global."
#                , multiple = TRUE
#                , immediate = TRUE)
#       removeUI(selector = "#delete.global."
#                , multiple = TRUE
#                , immediate = TRUE)
#       print(names(input))
#       
#       fluidRow(
#         column(8
#                , lapply(1:ncol(tab)
#                         , function(i) {
#                           checkboxInput(inputId = paste0("check.global.", colnames(tab)[i])
#                                         , label = colnames(tab)[i]
#                                         , value = TRUE
#                                         , width = "100%")
#                         })
#         )
#         , column(2
#                  , lapply(1:ncol(tab)
#                           , function(i) {
#                             actionButton(inputId = paste0("upload.global.", colnames(tab)[i])
#                                          , label = NULL
#                                          , icon = icon("upload")
#                                          , width = "100%"
#                                          , style = HTML(paste(button.style, "margin-bottom: 3px;")))
#                           })
#         )
#         , column(2
#                  , lapply(1:ncol(tab)
#                           , function(i) {
#                             actionButton(inputId = paste0("delete.global.", colnames(tab)[i])
#                                          , label = NULL
#                                          , icon = icon("trash-alt")
#                                          , width = "100%"
#                                          , style = HTML(paste(button.style, "margin-bottom: 3px;")))
#                           })
#         )
#       )
#     }
#   })
# })
# 
# observeEvent(RV$compt.global.nb, {
#   # tab = get_tab.global()
#   
#   # if (!is.null(tab) && ncol(tab) > 0)
#   if (RV$compt.global.nb > 0)
#   {
#     cat("\n CREATE GLOBAL OBSERVE\n")
#     
#     # lapply(1:ncol(tab)
#     lapply(1:RV$compt.global.nb
#            , function(i)
#            {
#              observeEvent(input[[paste0("upload.global.", RV$compt.global.files[i])]], {
#                get_update.global(file.globalParam = paste0(input$name.simul
#                                                            , "/DATA/GLOBAL_PARAMETERS/"
#                                                            , RV$compt.global.files[i]))
#              })
#              
#              observeEvent(input[[paste0("delete.global.", RV$compt.global.files[i])]], {
#                print(i)
#                file.globalParam = paste0(input$name.simul
#                                          , "/DATA/GLOBAL_PARAMETERS/"
#                                          , RV$compt.global.files[i])
#                print(file.globalParam)
#                shinyalert(type = "warning"
#                           , text = paste0("The simulation parameter file "
#                                           , sub("GLOBAL_PARAMETERS/", "GLOBAL_PARAMETERS/\n", file.globalParam)
#                                           , " will be removed !\n"
#                                           , "Make sure this is what you want.")
#                           , showCancelButton = TRUE
#                           , showConfirmButton = TRUE
#                           , callbackR = function(x)
#                           {
#                             if (x)
#                             {
#                               file.remove(file.globalParam)
#                               RV$compt.global.nb = RV$compt.global.nb - 1
#                             }
#                           })
#              })
#            })
#   # }
#   
#   output$created_table.global = renderDataTable({
#     print("GETRENDER")
#     cat("\n")
#     
#       col_toKeep = foreach(i = 1:RV$compt.global.nb, .combine = "c") %do%
#       {
#         print(paste0("res = input$check.global.", RV$compt.global.files[i]))
#         eval(parse(text = paste0("res = input$check.global.", RV$compt.global.files[i])))
#         cat("res : ", res, "\n")
#         return(res)
#       }
#       # print(dim(tab))
#       print(RV$compt.global.nb)
#       print(col_toKeep)
#       # path_folder = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/")
#       # tab = get_files(path_folder)
#       # tab = 
#       
#       return(RV$compt.global.tab[, which(col_toKeep == TRUE), drop = FALSE])
#   })
#   }
# })


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
    
    # if (as.character(get_res) != "0")
    # {
    #   # print("youhouu")
    #   # print(RV$compt.global.nb)
    #   RV$compt.global.nb <- RV$compt.global.nb + 1
    #   # print(RV$compt.global.nb)
    # }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

