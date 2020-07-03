
tabPanel(title =  HTML("<span class='panel_title'><i class='fa fa-cogs'></i> Run simulation</span>")
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
                      , div(id = "help3_1"
                            , directoryInput(inputId = "run.folder.simul"
                                             , label = param.style("Select the simulation folder :")
                                             , value = '~')
                      )
               )
               , column(5
                        , div(id = "help3_2"
                              , shinyjs::disabled(
                                selectInput(inputId = "run.simulParam"
                                            , label = param.style("Select the simulation parameters file :")
                                            , choices = NULL
                                            , selected = NULL
                                            , multiple = F
                                            , width = "100%")
                              )
                        )
               )
               , column(2
                        , br()
                        , actionButton(inputId = "run"
                                       , label = "Run"
                                       , icon = icon("play-circle", class = "icon-help")
                                       , width = "100%"
                                       , style = HTML(button.style.help)
                        ) %>% helper(type = "inline"
                                     , title = "Run a FATE-HD simulation"
                                     , size = "l"
                                     , colour = "#e0dbd9"
                                     , content = help.full(param.name.vec = c("<hr/>"
                                                                              , "name.simulation"
                                                                              , "file.simulParam"
                                                                              , "name.FATE_executable")
                                                           , param.desc.vec = c("<hr/>"
                                                                                , "a <span style='font-family:Monospace;'>string</span> that corresponds to
                                                                                the main directory or simulation name of the <span style='font-family:Monospace;'>FATE-HD</span> simulation"
                                                                                , "a <span style='font-family:Monospace;'>string</span> that corresponds to
                                                                                the name of a parameter file that will be contained into the <span style='font-family:Monospace;'>PARAM_SIMUL</span>
                                                                                folder of the <span style='font-family:Monospace;'>FATE-HD</span> simulation"
                                                                                , "a <span style='font-family:Monospace;'>string</span> that corresponds to
                                                                                the file name of the <span style='font-family:Monospace;'>FATE-HD</span> executable"
                                                           )
                                                           )
                                     )
                                     )
                                     ) ## END fluidRow
             # , column(2
             #          , br()
             #          , actionButton(inputId = "run.copy"
             #                         , label = "Copy files"
             #                         , icon = icon("copy", class = "icon-help")
             #                         , width = "100%"
             #                         , style = HTML(button.style.help)
             #          ) %>% helper(type = "inline"
             #                       , title = "Copy simulation folder before running simulation"
             #                       , size = "l"
             #                       , colour = "#e0dbd9"
             #                       , content = help.full(param.name.vec = c("<hr/>"
             #                                                                , "name.simulation"
             #                                                                , "name.FATE_executable")
             #                                             , param.desc.vec = c("<hr/>"
             #                                                                  , "a <span style='font-family:Monospace;'>string</span> that corresponds to
             #                                                                  the main directory or simulation name of the <span style='font-family:Monospace;'>FATE-HD</span> simulation"
             #                                                                  , "a <span style='font-family:Monospace;'>string</span> that corresponds to
             #                                                                  the file name of the <span style='font-family:Monospace;'>FATE-HD</span> executable"
             #                                             )
             #                                             )
             #                       )
             #                       )
             # ) ## END fluidRow
             # , fluidRow(
             #   column(5
             #          , fileInput(inputId = "run.executable"
             #                      , label = param.style("Select the FATE executable file :")
             #                      , multiple = F
             #                      , width = "100%")
             #   )
             #   , column(5, br())
             #   , column(2
             #            , br()
             #            , actionButton(inputId = "run"
             #                           , label = "Run"
             #                           , icon = icon("play-circle", class = "icon-help")
             #                           , width = "100%"
             #                           , style = HTML(button.style.help)
             #            ) %>% helper(type = "inline"
             #                         , title = "Run a FATE-HD simulation"
             #                         , size = "l"
             #                         , colour = "#e0dbd9"
             #                         , content = help.full(param.name.vec = c("<hr/>"
             #                                                                  , "name.simulation"
             #                                                                  , "file.simulParam"
             #                                                                  , "name.FATE_executable")
             #                                               , param.desc.vec = c("<hr/>"
             #                                                                    , "a <span style='font-family:Monospace;'>string</span> that corresponds to
             #                                                                    the main directory or simulation name of the <span style='font-family:Monospace;'>FATE-HD</span> simulation"
             #                                                                    , "a <span style='font-family:Monospace;'>string</span> that corresponds to
             #                                                                    the name of a parameter file that will be contained into the <span style='font-family:Monospace;'>PARAM_SIMUL</span>
             #                                                                    folder of the <span style='font-family:Monospace;'>FATE-HD</span> simulation"
             #                                                                    , "a <span style='font-family:Monospace;'>string</span> that corresponds to
             #                                                                    the file name of the <span style='font-family:Monospace;'>FATE-HD</span> executable"
             #                                               )
             #                                               )
             #                         )
             #                         )
             #                         ) ## END fluidRow
                        ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 12,
             wellPanel(id = "main.panel",
                       style = "border-solid:solid; border-width:0px;",
                       fluidRow(
                         column(6
                                , wellPanel(id = "main.panel1",
                                            br(),
                                            HTML("<strong>ERRORS</strong>")
                                )
                         )
                         , column(6
                                  , wellPanel(id = "main.panel2",
                                              br(),
                                              HTML("<strong>OUTPUTS</strong>")
                                  )
                         )
                       )
             ) ## END wellPanel
           ) ## END mainPanel
           # ) %>% helper(type = "inline"
           #              , title = "Evaluate FATE-HD simulation outputs"
           #              , size = "l"
           #              , content = help.HTML(html.file = paste0(path.articles)
           #                                    , target.anchor = '<div '
           #                                    , target.class = "post_fate---evaluation-of-simulation")
               ) ## END sidebarLayout
             ) ## tabPanel
