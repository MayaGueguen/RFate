
tabPanel(title =  HTML("<span class='panel_title'><i class='fa fa-clone'></i> Create multiple set</span>")
         , value = "panel3"
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
                                    , label = param.style("Select the simulation parameters file :")
                                    , choices = NULL
                                    , selected = NULL
                                    , multiple = F
                                    , width = "100%")
                      )
                      , shinyjs::disabled(
                        selectInput(inputId = "set.folder1.simulParam2"
                                    , label = param.style("Select the simulation parameters file :")
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
           ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 12
             , wellPanel(id = "main.panel",
                         style = "border-solid:solid; border-width:0px;",
                         fluidRow(
                           column(10
                                  , wellPanel(id = "main.panel1",
                                              br(),
                                              fluidRow(
                                                column(4
                                                       , checkboxGroupInput(inputId = "sel.choices.1"
                                                                            , label = HTML("<i class='fa fa-heart'></i> global parameters")
                                                                            , choices = c("max_by_cohort"
                                                                                          , "max_abund_low"
                                                                                          , "max_abund_medium"
                                                                                          , "max_abund_high")
                                                                            , selected = NULL
                                                                            , width = "100%"
                                                       )
                                                )
                                                , column(3
                                                         , sliderInput(inputId = "sel.slider.1"
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
                                                         , checkboxGroupInput(inputId = "sel.choices.3"
                                                                              , label = HTML("<i class='fa fa-globe'></i> habitat suitability")
                                                                              , choices = c("ref_option")
                                                                              , selected = NULL
                                                                              , width = "100%"
                                                         )
                                                )
                                              )
                                              , fluidRow(
                                                br()
                                                , column(4
                                                         , checkboxGroupInput(inputId = "sel.choices.2"
                                                                              , label = HTML("<i class='fa fa-heart'></i> seeding")
                                                                              , choices = c("seeding_duration"
                                                                                            , "seeding_step"
                                                                                            , "seeding_input")
                                                                              , selected = NULL
                                                                              , width = "100%"
                                                         )
                                                )
                                                , column(3
                                                         , sliderInput(inputId = "sel.slider.2"
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
                                                         , checkboxGroupInput(inputId = "sel.choices.3"
                                                                              , label = HTML("<i class='fa fa-seedling'></i> dispersal")
                                                                              , choices = c("mode_dispers")
                                                                              , selected = NULL
                                                                              , width = "100%"
                                                         )
                                                )
                                              )
                                              , fluidRow(
                                                br()
                                                , column(4
                                                         , checkboxGroupInput(inputId = "sel.choices.4"
                                                                              , label = HTML("<i class='fa fa-sun'></i> global parameters")
                                                                              , choices = c("light_thresh_medium"
                                                                                            , "light_thresh_low")
                                                                              , selected = NULL
                                                                              , width = "100%"
                                                         )
                                                )
                                                , column(3
                                                         , sliderInput(inputId = "sel.slider.4"
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
                                                         , checkboxGroupInput(inputId = "sel.choices.3"
                                                                              , label = HTML("<i class='fa fa-sun'></i> height strata")
                                                                              , choices = c("strata_limits")
                                                                              , selected = NULL
                                                                              , width = "100%"
                                                         )
                                                )
                                              )
                                  )## END main.panel1
                           )
                         )
                         # ) %>% helper(type = "inline"
                         #              , title = "FATE-HD modules"
                         #              , size = "l"
                         #              , content = help.HTML(html.file = "https://mayagueguen.github.io/FATE-WEBSITE/1c_fate-hd_tutorial_MODULES.html"
                         #                                    , target.anchor = '<div '
                         #                                    , target.class = "#arguments")
             ) ## END main.panel
           ) ## END mainPanel
         ) ## END sidebarLayout
) ## tabPanel
