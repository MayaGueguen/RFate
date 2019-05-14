
tabPanel(title = HTML("<span class='tabPanel_title'>Specific year</span>")
         , value = "panel.specific_year"
         , fluidRow(
           column(3
                  , br()
                  , shinyjs::disabled(
                    selectInput(inputId = "graph.year"
                                , label = param.style("year(s)")
                                , choices = NULL
                                , selected = NULL
                                , multiple = FALSE
                                , width = "100%")
                  )
           )
           , column(3
                    , br()
                    , shinyjs::disabled(
                      selectInput(inputId = "graph.strata_min"
                                  , label = param.style("strata_min")
                                  , choices = NULL
                                  , selected = NULL
                                  , multiple = F
                                  , width = "100%")
                    )
           )
           , column(3
                    , br()
                    , numericInput(inputId = "graph.rel_abund_thresh"
                                   , label = param.style("opt.rel_abund_thresh")
                                   , value = 0.05
                                   , min = 0
                                   , max = 1
                                   , width = "100%")
           )
           , column(3
                    , br()
                    , numericInput(inputId = "graph.opt.no_CPU"
                                   , label = param.style("opt.no_CPU")
                                   , value = 1
                                   , min = 1
                                   , width = "100%")
           )
         )
         , fluidRow(
           column(3
                  , br()
                  , shinyjs::disabled(
                    actionButton(inputId = "create.relativeAbund"
                                 , label = "Run relative abund"
                                 , icon = icon("play")
                                 , width = "100%"
                                 , style = HTML(button.style)
                    ) %>% helper(type = "inline"
                                 , title = "Create maps of relative abundance"
                                 , size = "l"
                                 , content = help.HTML("docs/reference/POST_FATE.relativeAbund.html")
                    )
                  )
           )
           # , column(9
           #          , wellPanel(
           #            style = HTML(paste0("background-color: ", help.color, ";")),
           #            helpText(HTML("
           #                          <p><a href='https://mayagueguen.github.io/RFate/reference/POST_FATE.relativeAbund_presenceAbsence.html' target='_blank'>
           #                          See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
           #                          <table style='width:100%;'>
           #                          
           #                          </table>
           #                          "
           #            ))) ## END wellPanel
           #            )
         )
         , fluidRow(
           column(6
                  , br()
                  , fileInput(inputId = "graph.mat.PFG.obs"
                              , label = NULL
                              , buttonLabel = param.style("mat.PFG.obs")
                              , multiple = FALSE
                              , width = "100%")
           )
           # , column(4
           #          , br()
           #          , fileInput(inputId = "graph.mat.PFG.succ"
           #                      , label = NULL
           #                      , buttonLabel = param.style("mat.PFG.succ")
           #                      , multiple = FALSE
           #                      , width = "100%")
           # )
           , column(6
                    , br()
                    , fileInput(inputId = "graph.opt.cover.obs"
                                , label = NULL
                                , buttonLabel = param.style("opt.cover.obs")
                                , multiple = FALSE
                                , width = "100%")
                    # , fileInput(inputId = "graph.opt.light.obs"
                    #             , label = NULL
                    #             , buttonLabel = param.style("opt.light.obs")
                    #             , multiple = FALSE
                    #             , width = "100%")
                    # , fileInput(inputId = "graph.opt.soil.obs"
                    #             , label = NULL
                    #             , buttonLabel = param.style("opt.soil.obs")
                    #             , multiple = FALSE
                    #             , width = "100%")
           )
         )
         , fluidRow(
           column(3
                  , radioGroupButtons(inputId = "show.specific_year"
                                      , label = ""
                                      , choices = c("PFG vs Habsuit"
                                                    , "Validation stat"
                                                    , "PFG richness"
                                                    , "PFG cover"
                                                    , "Light CWM (MAP)"
                                                    , "Soil CWM (MAP)")
                                      , selected = 0
                                      , justified = TRUE
                                      , direction = "vertical"
                                      , status = "panelgraph"
                                      , checkIcon = list(yes = icon("ok", lib = "glyphicon")
                                                         , no = icon("remove", lib = "glyphicon"))
                  )
           )
           , column(9
                    , shinyjs::hidden(
                      fluidRow(
                        id = "panel.PFGvsHS"
                        , column(8
                                 , plotOutput(outputId = "plot.PFGvsHS", width = "100%", height = "600px")
                        )
                        , column(4
                                 , actionButton(inputId = "create.PFGvsHS"
                                                , label = "Run plot"
                                                , icon = icon("play")
                                                , width = "100%"
                                                , style = HTML(button.style)
                                 ) %>% helper(type = "inline"
                                              , title = "Plot maps of 0/1 predicted by FATE vs Habitat suitability"
                                              , size = "l"
                                              , content = help.HTML("docs/reference/POST_FATE.graphic_mapPFGvsHS.html")
                                 )
                        )
                      ))
                    , shinyjs::hidden(
                      fluidRow(
                        id = "panel.validationStat"
                        , column(8
                                 , plotOutput(outputId = "plot.validationStat", width = "100%", height = "600px")
                        )
                        , column(4
                                 , actionButton(inputId = "create.validationStat"
                                                , label = "Run plot"
                                                , icon = icon("play")
                                                , width = "100%"
                                                , style = HTML(button.style)
                                 ) %>% helper(type = "inline"
                                              , title = "Plot validation statistics and transform maps of abundances into 0/1"
                                              , size = "l"
                                              , content = help.HTML("docs/reference/POST_FATE.graphic_validationStatistics.html")
                                 )
                        )
                      ))
                    , shinyjs::hidden(
                      fluidRow(
                        id = "panel.PFGrichness"
                        , column(8
                                 , plotOutput(outputId = "plot.PFGrichness", width = "100%", height = "600px")
                        )
                        , column(4
                                 , actionButton(inputId = "create.PFGrichness"
                                                , label = "Run plot"
                                                , icon = icon("play")
                                                , width = "100%"
                                                , style = HTML(button.style)
                                 ) %>% helper(type = "inline"
                                              , title = "Plot map of PFG richness"
                                              , size = "l"
                                              , content = help.HTML("docs/reference/POST_FATE.graphic_mapPFGrichness.html")
                                 )
                        )
                      ))
                    , shinyjs::hidden(
                      fluidRow(
                        id = "panel.PFGcover"
                        , column(8
                                 , plotOutput(outputId = "plot.PFGcover", width = "100%", height = "600px")
                        )
                        , column(4
                                 , actionButton(inputId = "create.PFGcover"
                                                , label = "Run plot"
                                                , icon = icon("play")
                                                , width = "100%"
                                                , style = HTML(button.style)
                                 ) %>% helper(type = "inline"
                                              , title = "Plot map of PFG cover"
                                              , size = "l"
                                              , content = help.HTML("docs/reference/POST_FATE.graphic_mapPFGcover.html")
                                 )
                        )
                      ))
                    , shinyjs::hidden(
                      fluidRow(
                        id = "panel.PFGlight"
                        , column(8
                                 , plotOutput(outputId = "plot.PFGlight", width = "100%", height = "600px")
                        )
                        , column(4
                                 , actionButton(inputId = "create.PFGlight"
                                                , label = "Run plot"
                                                , icon = icon("play")
                                                , width = "100%"
                                                , style = HTML(button.style)
                                 ) %>% helper(type = "inline"
                                              , title = "Plot map of PFG light"
                                              , size = "l"
                                              , content = help.HTML("docs/reference/POST_FATE.graphic_mapPFGlight.html")
                                 )
                        )
                      ))
                    , shinyjs::hidden(
                      fluidRow(
                        id = "panel.PFGsoil"
                        , column(8
                                 , plotOutput(outputId = "plot.PFGsoil", width = "100%", height = "600px")
                        )
                        , column(4
                                 , actionButton(inputId = "create.PFGsoil"
                                                , label = "Run plot"
                                                , icon = icon("play")
                                                , width = "100%"
                                                , style = HTML(button.style)
                                 ) %>% helper(type = "inline"
                                              , title = "Plot map of PFG soil"
                                              , size = "l"
                                              , content = help.HTML("docs/reference/POST_FATE.graphic_mapPFGsoil.html")
                                 )
                        )
                      ))
           )
         )
) ## END tabPanel (Global parameters)
