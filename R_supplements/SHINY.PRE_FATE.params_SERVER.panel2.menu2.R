
####################################################################

output$UI.set.folders = renderUI({
  if (!is.null(input$set.strategy))
  {
    if (input$set.strategy == "From 1 folder, 1 simulation file")
    {
      fluidRow(
        column(5
               , directoryInput(inputId = "set.folder1"
                                , label = param.style("Select the simulation folder :")
                                , value = '~')
               , shinyjs::disabled(
                 selectInput(inputId = "set.folder1.simulParam1"
                             , label = param.style("Select the simulation parameters file(s) :")
                             , choices = NULL
                             , selected = NULL
                             , multiple = F
                             , width = "100%")
               )
        )
        , column(5, br())
        , column(2
                 , br()
                 , actionButton(inputId = "HELP.panel2.menu2"
                                , label = "Need some help"
                                , icon = icon("question-circle", class = "icon-help")
                                , width = "100%"
                                , style = HTML(button.style.help)))
      ) ## END fluidRow
    } else if (input$set.strategy == "From 1 folder, 2 simulation files")
    {
      fluidRow(
        column(5
               , directoryInput(inputId = "set.folder1"
                                , label = param.style("Select the simulation folder :")
                                , value = '~')
               , shinyjs::disabled(
                 selectInput(inputId = "set.folder1.simulParam1"
                             , label = param.style("Select the simulation parameters file(s) :")
                             , choices = NULL
                             , selected = NULL
                             , multiple = F
                             , width = "100%")
               )
               , shinyjs::disabled(
                 selectInput(inputId = "set.folder1.simulParam2"
                             , label = NULL
                             , choices = NULL
                             , selected = NULL
                             , multiple = F
                             , width = "100%")
               )
        )
        , column(5, br())
        , column(2
                 , br()
                 , actionButton(inputId = "HELP.panel2.menu2"
                                , label = "Need some help"
                                , icon = icon("question-circle", class = "icon-help")
                                , width = "100%"
                                , style = HTML(button.style.help)))
      ) ## END fluidRow
    } else if (input$set.strategy == "From 2 folders, 2 simulation files")
    {
      fluidRow(
        column(5
               , directoryInput(inputId = "set.folder1"
                                , label = param.style("Select the simulation folder :")
                                , value = '~')
               , shinyjs::disabled(
                 selectInput(inputId = "set.folder1.simulParam1"
                             , label = param.style("Select the simulation parameters file(s) :")
                             , choices = NULL
                             , selected = NULL
                             , multiple = F
                             , width = "100%")
               )
        )
        , column(5
                 , directoryInput(inputId = "set.folder2"
                                  , label = param.style("Select the simulation folder :")
                                  , value = '~')
                 , shinyjs::disabled(
                   selectInput(inputId = "set.folder2.simulParam2"
                               , label = param.style("Select the simulation parameters file :")
                               , choices = NULL
                               , selected = NULL
                               , multiple = F
                               , width = "100%")
                 )
        )
        , column(2
                 , br()
                 , actionButton(inputId = "HELP.panel2.menu2"
                                , label = "Need some help"
                                , icon = icon("question-circle", class = "icon-help")
                                , width = "100%"
                                , style = HTML(button.style.help)))
      ) ## END fluidRow
    }
  }
})

####################################################################

output$UI.set.choices = renderUI({
  if (!is.null(input$set.strategy))
  {
    if (input$set.strategy == "From 1 folder, 1 simulation file")
    {
      wellPanel(id = "main.panel1",
                br(),
                fluidRow(
                  column(4
                         , checkboxGroupInput(inputId = "set.choices.1"
                                              , label = HTML("<i class='fa fa-heart'></i> global parameters")
                                              , choices = c("max_by_cohort"
                                                            , "max_abund_low"
                                                            , "max_abund_medium"
                                                            , "max_abund_high")
                                              , selected = NULL
                                              , width = "100%"
                         ) %>% helper(type = "inline"
                                      , title = "FATE-HD modules : CORE - Required and impacted parameters"
                                      , size = "l"
                                      , content = help.HTML(html.file = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html"
                                                            , target.anchor = '<h1'
                                                            , target.class = "CORE"
                                                            , web.address = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html#_core_module_(succession)")
                         )
                  )
                  , column(3, br())
                  , column(1, br())
                  , column(4
                           , checkboxGroupInput(inputId = "set.choices.2"
                                                , label = HTML("<i class='fa fa-globe'></i> habitat suitability")
                                                , choices = c("ref_option")
                                                , selected = NULL
                                                , width = "100%"
                           )
                  )
                ) %>% helper(type = "inline"
                             , title = "FATE-HD modules : HABITAT SUITABILITY - Required and impacted parameters"
                             , size = "l"
                             , content = help.HTML(html.file = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html"
                                                   , target.anchor = '<h1'
                                                   , target.class = "HABITAT SUITABILITY"
                                                   , web.address = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html#_habitat_suitability_module_")
                )
                , fluidRow(
                  br()
                  , column(4
                           , checkboxGroupInput(inputId = "set.choices.3"
                                                , label = HTML("<i class='fa fa-heart'></i> seeding")
                                                , choices = c("seeding_duration"
                                                              , "seeding_step"
                                                              , "seeding_input")
                                                , selected = NULL
                                                , width = "100%"
                           )
                  )
                  , column(3, br())
                  , column(1, br())
                  , column(4
                           , checkboxGroupInput(inputId = "set.choices.4"
                                                , label = HTML("<i class='fa fa-seedling'></i> dispersal")
                                                , choices = c("mode_dispers")
                                                , selected = NULL
                                                , width = "100%"
                           )
                  )
                ) %>% helper(type = "inline"
                             , title = "FATE-HD modules : DISPERSAL - Required and impacted parameters"
                             , size = "l"
                             , content = help.HTML(html.file = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html"
                                                   , target.anchor = '<h1'
                                                   , target.class = "DISPERSAL"
                                                   , web.address = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html#_dispersal_module_")
                )
                , fluidRow(
                  br()
                  , column(4
                           , checkboxGroupInput(inputId = "set.choices.5"
                                                , label = HTML("<i class='fa fa-sun'></i> global parameters")
                                                , choices = c("light_thresh_medium"
                                                              , "light_thresh_low")
                                                , selected = NULL
                                                , width = "100%"
                           )
                  )
                  , column(3, br())
                  , column(1, br())
                  , column(4
                           , checkboxGroupInput(inputId = "set.choices.6"
                                                , label = HTML("<i class='fa fa-sun'></i> height strata")
                                                , choices = c("strata_limits")
                                                , selected = NULL
                                                , width = "100%"
                           )
                  )
                ) %>% helper(type = "inline"
                             , title = "FATE-HD modules : LIGHT - Required and impacted parameters"
                             , size = "l"
                             , content = help.HTML(html.file = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html"
                                                   , target.anchor = '<h1'
                                                   , target.class = "LIGHT"
                                                   , web.address = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html#_light_module_")
                ) ## END fluidRow
      ) ## END main.panel1
    } else
    {
      wellPanel(id = "main.panel1",
                br(),
                fluidRow(
                  column(4
                         , checkboxGroupInput(inputId = "set.choices.1"
                                              , label = HTML("<i class='fa fa-heart'></i> global parameters")
                                              , choices = c("max_by_cohort"
                                                            , "max_abund_low"
                                                            , "max_abund_medium"
                                                            , "max_abund_high")
                                              , selected = NULL
                                              , width = "100%"
                         ) %>% helper(type = "inline"
                                      , title = "FATE-HD modules : CORE - Required and impacted parameters"
                                      , size = "l"
                                      , content = help.HTML(html.file = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html"
                                                            , target.anchor = '<h1'
                                                            , target.class = "CORE"
                                                            , web.address = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html#_core_module_(succession)")
                         )
                  )
                  , column(3
                           , sliderInput(inputId = "set.slider.1"
                                         , label = HTML("% of variation<br/><br/>")
                                         , min = 0
                                         , max = 100
                                         , value = 50
                                         , step = 5
                                         , round = TRUE
                                         , width = "100%"
                           )
                  )
                  , column(1, br())
                  , column(4
                           , checkboxGroupInput(inputId = "set.choices.2"
                                                , label = HTML("<i class='fa fa-globe'></i> habitat suitability")
                                                , choices = c("ref_option")
                                                , selected = NULL
                                                , width = "100%"
                           )
                  )
                ) %>% helper(type = "inline"
                             , title = "FATE-HD modules : HABITAT SUITABILITY - Required and impacted parameters"
                             , size = "l"
                             , content = help.HTML(html.file = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html"
                                                   , target.anchor = '<h1'
                                                   , target.class = "HABITAT SUITABILITY"
                                                   , web.address = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html#_habitat_suitability_module_")
                )
                , fluidRow(
                  br()
                  , column(4
                           , checkboxGroupInput(inputId = "set.choices.3"
                                                , label = HTML("<i class='fa fa-heart'></i> seeding")
                                                , choices = c("seeding_duration"
                                                              , "seeding_step"
                                                              , "seeding_input")
                                                , selected = NULL
                                                , width = "100%"
                           )
                  )
                  , column(3
                           , sliderInput(inputId = "set.slider.3"
                                         , label = HTML("<br/>")
                                         , min = 0
                                         , max = 100
                                         , value = 50
                                         , step = 5
                                         , round = TRUE
                                         , width = "100%"
                           )
                  )
                  , column(1, br())
                  , column(4
                           , checkboxGroupInput(inputId = "set.choices.4"
                                                , label = HTML("<i class='fa fa-seedling'></i> dispersal")
                                                , choices = c("mode_dispers")
                                                , selected = NULL
                                                , width = "100%"
                           )
                  )
                ) %>% helper(type = "inline"
                             , title = "FATE-HD modules : DISPERSAL - Required and impacted parameters"
                             , size = "l"
                             , content = help.HTML(html.file = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html"
                                                   , target.anchor = '<h1'
                                                   , target.class = "DISPERSAL"
                                                   , web.address = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html#_dispersal_module_")
                )
                , fluidRow(
                  br()
                  , column(4
                           , checkboxGroupInput(inputId = "set.choices.5"
                                                , label = HTML("<i class='fa fa-sun'></i> global parameters")
                                                , choices = c("light_thresh_medium"
                                                              , "light_thresh_low")
                                                , selected = NULL
                                                , width = "100%"
                           )
                  )
                  , column(3
                           , sliderInput(inputId = "set.slider.5"
                                         , label = HTML("<br/>")
                                         , min = 0
                                         , max = 100
                                         , value = 50
                                         , step = 5
                                         , round = TRUE
                                         , width = "100%"
                           )
                  )
                  , column(1, br())
                  , column(4
                           , checkboxGroupInput(inputId = "set.choices.6"
                                                , label = HTML("<i class='fa fa-sun'></i> height strata")
                                                , choices = c("strata_limits")
                                                , selected = NULL
                                                , width = "100%"
                           )
                  )
                ) %>% helper(type = "inline"
                             , title = "FATE-HD modules : LIGHT - Required and impacted parameters"
                             , size = "l"
                             , content = help.HTML(html.file = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html"
                                                   , target.anchor = '<h1'
                                                   , target.class = "LIGHT"
                                                   , web.address = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html#_light_module_")
                ) ## END fluidRow
      ) ## END main.panel1
    }
  }
})

####################################################################

get_path.folder1 = eventReactive(input$set.folder1, {
  if (input$set.folder1 > 0)
  {
    path = choose.dir(default = readDirectoryInput(session, 'set.folder1'))
    updateDirectoryInput(session, 'set.folder1', value = path)
    return(path)
  }
})

get_path.folder2 = eventReactive(input$set.folder2, {
  if (input$set.folder2 > 0)
  {
    path = choose.dir(default = readDirectoryInput(session, 'set.folder2'))
    updateDirectoryInput(session, 'set.folder2', value = path)
    return(path)
  }
})

####################################################################

observeEvent(input$set.folder1, {
  if (input$set.folder1 > 0)
  {
    names.simulParam = list.files(path = paste0(get_path.folder1(), "/PARAM_SIMUL")
                                  , pattern = ".txt$"
                                  , all.files = FALSE
                                  , full.names = TRUE)
    names.simulParam = basename(names.simulParam)
    if (length(names.simulParam) > 0)
    {
      updateSelectInput(session
                        , inputId = "set.folder1.simulParam1"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      
      updateSelectInput(session
                        , inputId = "set.folder1.simulParam2"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      
      shinyjs::enable("set.folder1.simulParam1")
      shinyjs::enable("set.folder1.simulParam2")
    } else
    {
      shinyjs::reset("set.folder1.simulParam1")
      shinyjs::reset("set.folder1.simulParam2")
      shinyjs::disable("set.folder1.simulParam1")
      shinyjs::disable("set.folder1.simulParam2")
    }
  } else
  {
    shinyjs::reset("set.folder1.simulParam1")
    shinyjs::reset("set.folder1.simulParam2")
    shinyjs::disable("set.folder1.simulParam1")
    shinyjs::disable("set.folder1.simulParam2")
  }
})

observeEvent(input$set.folder2, {
  if (input$set.folder2 > 0)
  {
    names.simulParam = list.files(path = paste0(get_path.folder2(), "/PARAM_SIMUL")
                                  , pattern = ".txt$"
                                  , all.files = FALSE
                                  , full.names = TRUE)
    names.simulParam = basename(names.simulParam)
    if (length(names.simulParam) > 0)
    {
      updateSelectInput(session
                        , inputId = "set.folder2.simulParam2"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      
      shinyjs::enable("set.folder2.simulParam2")
    } else
    {
      shinyjs::reset("set.folder2.simulParam2")
      shinyjs::disable("set.folder2.simulParam2")
    }
  } else
  {
    shinyjs::reset("set.folder2.simulParam2")
    shinyjs::disable("set.folder2.simulParam2")
  }
})


####################################################################

# observeEvent(input$run.copy, {
#   if (input$run.folder.simul > 0 && nchar(input$run.simulParam) > 0)
#   {
#     if(!is.null(input$run.executable))
#     {
#       showModal(modalDialog(HTML(paste0("Copying <em>", basename(get_path.run()), "</em> folder..."))
#                             , title = HTML("Run <code>FATE-HD</code> simulation")
#                             , footer = NULL))
#       system(paste0("scp -r ", get_path.run(), " ./"))
#       system(paste0("scp ", input$run.executable$datapath, " FATE_executable.exe"))
#       removeModal()
#     }
#   }
# })
# 
# ####################################################################
# 
# observeEvent(input$run, {
#   if (input$run.folder.simul > 0 && nchar(input$run.simulParam) > 0)
#   {
#     if (file.exists("FATE_executable.exe") &&
#         dir.exists(basename(get_path.run())))
#     {
#       showModal(modalDialog(HTML(paste0("Running simulation with :
#                                         <ul>
#                                         <li><strong>folder :</strong> ", basename(get_path.run()),"</li>
#                                         <li><strong>simulation parameter file :</strong> ", input$run.simulParam, "</li>
#                                         </ul>"))
#                             , title = HTML("Run <code>FATE-HD</code> simulation")
#                             , footer = NULL))
#       Sys.sleep(30)
#       removeModal()
#     }
#   }
# })
# 
