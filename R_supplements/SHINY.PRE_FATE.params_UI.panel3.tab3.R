
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
                    , numericInput(inputId = "graph.opt.no_CPU"
                                   , label = param.style("opt.no_CPU")
                                   , value = 1
                                   , min = 1
                                   , width = "100%")
           )
           , column(3
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
         )
         , fluidRow(
           column(5
                  , br()
                  , uiOutput(outputId = "UI.graph.mat.PFG.obs")
           )
           , column(1
                    , br()
                    , actionButton(inputId = "graph.mat.PFG.obs.delete"
                                 , label = ""
                                 , icon = icon("broom")
                                 , width = "100%"
                                 , style = HTML(button.style)
                    )
           )
           # , column(4
           #          , br()
           #          , fileInput(inputId = "graph.mat.PFG.succ"
           #                      , label = NULL
           #                      , buttonLabel = param.style("mat.PFG.succ")
           #                      , multiple = FALSE
           #                      , width = "100%")
           # )
           , column(5
                    , br()
                    , uiOutput(outputId = "UI.graph.opt.cover.obs")
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
           , column(1
                    , br()
                    , actionButton(inputId = "graph.opt.cover.obs.delete"
                                   , label = ""
                                   , icon = icon("broom")
                                   , width = "100%"
                                   , style = HTML(button.style)
                    )
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
                                 , plotlyOutput(outputId = "plot.PFGvsHS", width = "100%", height = "600px")
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
                                 , plotlyOutput(outputId = "plot.PFGrichness", width = "100%", height = "600px")
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
                                 , plotlyOutput(outputId = "plot.PFGcover", width = "100%", height = "600px")
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
                                 , plotlyOutput(outputId = "plot.PFGlight", width = "100%", height = "600px")
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
                                 , plotlyOutput(outputId = "plot.PFGsoil", width = "100%", height = "600px")
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
