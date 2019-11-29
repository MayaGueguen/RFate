
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
         )
         , fluidRow(
           column(3, br()),
           column(3, br()),
           column(3, br()),
           column(3
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
                  , br()
                  , uiOutput(outputId = "UI.graph.mat.PFG.obs")
           )
           , column(1
                    , br()
                    , br()
                    , actionButton(inputId = "graph.mat.PFG.obs.delete"
                                   , label = ""
                                   , icon = icon("broom")
                                   , width = "100%"
                                   , style = HTML(button.style)
                    )
           )
           , column(5
                    , br()
                    , br()
                    , uiOutput(outputId = "UI.graph.opt.cover.obs")
           )
           , column(1
                    , br()
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
                  , actionButton(inputId = "create.validationStat"
                                 , label = "Run Validation statistics"
                                 , icon = icon("play")
                                 , width = "100%"
                                 , style = HTML(button.style)
                  ) %>% helper(type = "inline"
                               , title = "Plot validation statistics and transform maps of abundances into 0/1"
                               , size = "l"
                               , content = help.HTML("docs/reference/POST_FATE.graphic_validationStatistics.html")
                  )
                  , br()
                  , actionButton(inputId = "create.PFGvsHS"
                                 , label = "Run PFG vs HS"
                                 , icon = icon("play")
                                 , width = "100%"
                                 , style = HTML(button.style)
                  ) %>% helper(type = "inline"
                               , title = "Plot maps of 0/1 predicted by FATE vs Habitat suitability"
                               , size = "l"
                               , content = help.HTML("docs/reference/POST_FATE.graphic_mapPFGvsHS.html")
                  )
                  , br()
                  , actionButton(inputId = "create.PFGrichness"
                                 , label = "Run PFG richness"
                                 , icon = icon("play")
                                 , width = "100%"
                                 , style = HTML(button.style)
                  ) %>% helper(type = "inline"
                               , title = "Plot map of PFG richness"
                               , size = "l"
                               , content = help.HTML("docs/reference/POST_FATE.graphic_mapPFGrichness.html")
                  )
                  , br()
                  , actionButton(inputId = "create.PFGcover"
                                 , label = "Run PFG cover"
                                 , icon = icon("play")
                                 , width = "100%"
                                 , style = HTML(button.style)
                  ) %>% helper(type = "inline"
                               , title = "Plot map of PFG cover"
                               , size = "l"
                               , content = help.HTML("docs/reference/POST_FATE.graphic_mapPFGcover.html")
                  )
                  , br()
                  , actionButton(inputId = "create.PFGlight"
                                 , label = "Run PFG light"
                                 , icon = icon("play")
                                 , width = "100%"
                                 , style = HTML(button.style)
                  ) %>% helper(type = "inline"
                               , title = "Plot map of PFG light"
                               , size = "l"
                               , content = help.HTML("docs/reference/POST_FATE.graphic_mapPFGlight.html")
                  )
                  , br()
                  , actionButton(inputId = "create.PFGsoil"
                                 , label = "Run PFG soil"
                                 , icon = icon("play")
                                 , width = "100%"
                                 , style = HTML(button.style)
                  ) %>% helper(type = "inline"
                               , title = "Plot map of PFG soil"
                               , size = "l"
                               , content = help.HTML("docs/reference/POST_FATE.graphic_mapPFGsoil.html")
                  )
           )
           , column(9
                    , shinyjs::hidden(plotOutput(outputId = "plot.validationStat", width = "100%", height = "600px"))
                    , shinyjs::hidden(plotlyOutput(outputId = "plot.PFGvsHS", width = "100%", height = "600px"))
                    , shinyjs::hidden(plotlyOutput(outputId = "plot.PFGrichness", width = "100%", height = "600px"))
                    , shinyjs::hidden(plotlyOutput(outputId = "plot.PFGcover", width = "100%", height = "600px"))
                    , shinyjs::hidden(plotlyOutput(outputId = "plot.PFGlight", width = "100%", height = "600px"))
                    , shinyjs::hidden(plotlyOutput(outputId = "plot.PFGsoil", width = "100%", height = "600px"))
           )
         ) ## END fluidRow
) ## END tabPanel (Global parameters)
