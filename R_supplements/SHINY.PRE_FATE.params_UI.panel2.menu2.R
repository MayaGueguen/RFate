
tabPanel(title =  HTML("<span class='panel_title'><i class='fa fa-clone'></i> Create multiple set</span>")
         , value = "panel3"
         
         , fluidRow(
           radioGroupButtons(inputId = "set.strategy"
                             , label = NULL
                             , choices = c("From 1 folder, 1 simulation file"
                                           , "From 1 folder, 2 simulation files"
                                           , "From 2 folders, 2 simulation files")
                             , selected = 0
                             , justified = TRUE
                             , status = "panelgraph"
                             , checkIcon = list(yes = icon("ok", lib = "glyphicon")
                                                , no = icon("remove", lib = "glyphicon"))
           )
         )
         
         , sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 12,
             style = HTML(paste0("border-width:0px; background-color:", help.color, "; margin-left:15px; margin-top:18px;")),
             withMathJax(),
             
             br(),
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
                      , shinyjs::hidden(
                        selectInput(inputId = "set.folder1.simulParam2"
                                    , label = NULL
                                    , choices = NULL
                                    , selected = NULL
                                    , multiple = F
                                    , width = "100%")
                      )
               )
               , column(5, uiOutput(outputId = "UI.set.folders.strat3")
               )
               , column(2
                        , br()
                        , actionButton(inputId = "HELP.panel2.menu2"
                                       , label = "Need some help"
                                       , icon = icon("question-circle", class = "icon-help")
                                       , width = "100%"
                                       , style = HTML(button.style.help)))
             ) ## END fluidRow
           ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 12
             , shinyjs::hidden(
               
               wellPanel(id = "main.panel",
                         style = "border-solid:solid; border-width:0px;",
                         fluidRow(
                           column(10
                                  , wellPanel(id = "main.panel1",
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
                                                         , shinyjs::hidden(
                                                           sliderInput(inputId = "set.slider.1"
                                                                       , label = HTML("% of variation<br/><br/>")
                                                                       , min = 0
                                                                       , max = 100
                                                                       , value = 50
                                                                       , step = 5
                                                                       , round = TRUE
                                                                       , width = "100%"
                                                           )
                                                         )
                                                )
                                                , column(1, br())
                                                , column(4
                                                         , checkboxGroupInput(inputId = "set.choices.4"
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
                                                         , checkboxGroupInput(inputId = "set.choices.2"
                                                                              , label = HTML("<i class='fa fa-heart'></i> seeding")
                                                                              , choices = c("seeding_duration"
                                                                                            , "seeding_step"
                                                                                            , "seeding_input")
                                                                              , selected = NULL
                                                                              , width = "100%"
                                                         )
                                                )
                                                , column(3
                                                         , shinyjs::hidden(
                                                           sliderInput(inputId = "set.slider.2"
                                                                       , label = HTML("<br/>")
                                                                       , min = 0
                                                                       , max = 100
                                                                       , value = 50
                                                                       , step = 5
                                                                       , round = TRUE
                                                                       , width = "100%"
                                                           )
                                                         )
                                                )
                                                , column(1, br())
                                                , column(4
                                                         , checkboxGroupInput(inputId = "set.choices.5"
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
                                                         , checkboxGroupInput(inputId = "set.choices.3"
                                                                              , label = HTML("<i class='fa fa-sun'></i> global parameters")
                                                                              , choices = c("light_thresh_medium"
                                                                                            , "light_thresh_low")
                                                                              , selected = NULL
                                                                              , width = "100%"
                                                         )
                                                )
                                                , column(3
                                                         , shinyjs::hidden(
                                                           sliderInput(inputId = "set.slider.3"
                                                                       , label = HTML("<br/>")
                                                                       , min = 0
                                                                       , max = 100
                                                                       , value = 50
                                                                       , step = 5
                                                                       , round = TRUE
                                                                       , width = "100%"
                                                           )
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
                           )
                           , column(2
                                    , wellPanel(id = "main.panel2",
                                                br(),
                                                numericInput(inputId = "set.num_simul"
                                                             , label = "Maximum number of simulation files"
                                                             , value = 3000
                                                             , min = 1000
                                                             , max = 10000
                                                             , step = 500
                                                             , width = "100%"
                                                )
                                                , br()
                                                , br()
                                                , shinyjs::disabled(
                                                  actionButton(inputId = "create.multiple_set"
                                                               , label = HTML("Create <br/>multiple set")
                                                               , icon = icon("play")
                                                               , width = "100%"
                                                               , style = HTML(button.style)
                                                  )
                                                )
                                    ) ## END main.panel2
                           )
                         )
               ) ## END main.panel
             )
           ) ## END mainPanel
         ) ## END sidebarLayout
) ## tabPanel
