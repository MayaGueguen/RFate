
tabPanel(title =  HTML("<span class='panel_title'><i class='fa fa-object-group'></i> Plant Functional Groups</span>")
         , value = "panel1"
         , sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 4,
             style = HTML(paste0("border-width:0px; background-color:", help.color, "; margin-left:15px; margin-top:18px;")),
             withMathJax(),
             
             br(),
             fluidRow(
               column(12
                      , actionButton(inputId = "HELP.panel1"
                                     , label = "Need some help"
                                     , icon = icon("question-circle", class = "icon-help")
                                     , width = "100%"
                                     , style = HTML(button.style.help))
                      , br()
                      , br()
                      , br()
                      , br()
                      , textInput(inputId = "PFG.zone_name"
                                  , label = param.style("Studied area")
                                  , value = "AREA_1"
                                  , width = "100%")
                      , br()
                      , fileInput(inputId = "PFG.mask"
                                  , label = param.style("Simulation mask")
                                  , multiple = FALSE
                                  , width = "100%")
                      , br()
                      , fluidRow(
                        column(6
                               , textInput(inputId = "PFG.extent_xmin"
                                           , label = param.style("xmin")
                                           , value = NULL
                                           , width = "100%")
                               , textInput(inputId = "PFG.extent_xmax"
                                           , label = param.style("xmax")
                                           , value = NULL
                                           , width = "100%")
                        )
                        , column(6
                                 , textInput(inputId = "PFG.extent_ymin"
                                             , label = param.style("ymin")
                                             , value = NULL
                                             , width = "100%")
                                 , textInput(inputId = "PFG.extent_ymax"
                                             , label = param.style("ymax")
                                             , value = NULL
                                             , width = "100%")
                        )
                      )
                      )
             ) ## END fluidRow
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
         #              , content = help.HTML(html.file = "docs/index.html"
         #                                    , target.anchor = '<div '
         #                                    , target.class = "post_fate---evaluation-of-simulation")
         ) ## END sidebarLayout
) ## tabPanel
