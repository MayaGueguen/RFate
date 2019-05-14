
tabPanel(title = HTML("<span class='tabPanel_title'>Through time</span>")
         , value = "panel.through_time"
         , fluidRow(
           column(3
                  , br()
                  , numericInput(inputId = "graph.no.years"
                                 , label = param.style("no.years")
                                 , value = 10
                                 , min = 1
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
           , column(3
                    , br()
                    , checkboxInput(inputId = "graph.opt.fixedScale"
                                    , label = param.style("opt.fixedScale")
                                    , value = TRUE
                                    , width = "100%")
           )
           , column(3
                    , br()
                    , fileInput(inputId = "graph.opt.ras_habitat"
                                , label = param.style("opt.ras_habitat")
                                , multiple = FALSE
                                , width = "100%")
           )
         )
         , radioGroupButtons(inputId = "show.through_time"
                             , label = ""
                             , choices = c("Abundance & coverage"
                                           , "Abundance (PIXELS)"
                                           , "Light (PIXELS)"
                                           , "Soil (PIXELS)")
                             , selected = 0
                             , justified = TRUE
                             , status = "panelgraph"
                             , checkIcon = list(yes = icon("ok", lib = "glyphicon")
                                                , no = icon("remove", lib = "glyphicon"))
         )
         , fluidRow(
           br()
           , shinyjs::hidden(
             fluidRow(
               id = "panel.evolutionCoverage"
               , column(8
                        , plotOutput(outputId = "plot.evolutionCoverage1", width = "100%", height = "600px")
                        , plotOutput(outputId = "plot.evolutionCoverage2", width = "100%", height = "600px")
               )
               , column(4
                        , withBusyIndicatorUI(
                          actionButton(inputId = "create.evolutionCoverage"
                                       , label = "Run plot"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = HTML(button.style)
                          ) %>% helper(type = "inline"
                                       , title = "Plot evolution coverage"
                                       , size = "l"
                                       , content = help.HTML("docs/reference/POST_FATE.graphic_evolutionCoverage.html")
                          )
                        )
               )
             ))
           , shinyjs::hidden(
             fluidRow(
               id = "panel.evolutionAbund"
               , column(8
                        , plotOutput(outputId = "plot.evolutionAbund", width = "100%", height = "600px")
               )
               , column(4
                        , actionButton(inputId = "create.evolutionAbund"
                                       , label = "Run plot"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = HTML(button.style)
                        ) %>% helper(type = "inline"
                                     , title = "Plot evolution abund (PIXELS)"
                                     , size = "l"
                                     , content = help.HTML("docs/reference/POST_FATE.graphic_evolutionAbund_pixels.html")
                        )
               )
             ))
           , shinyjs::hidden(
             fluidRow(
               id = "panel.evolutionLight"
               , column(8
                        , plotOutput(outputId = "plot.evolutionLight", width = "100%", height = "600px")
               )
               , column(4
                        , actionButton(inputId = "create.evolutionLight"
                                       , label = "Run plot"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = HTML(button.style)
                        ) %>% helper(type = "inline"
                                     , title = "Plot evolution light (PIXELS)"
                                     , size = "l"
                                     , content = help.HTML("docs/reference/POST_FATE.graphic_evolutionLight_pixels.html")
                        )
                        , br()
                        , textOutput(outputId = "output.evolutionLight"))
             ))
           , shinyjs::hidden(
             fluidRow(
               id = "panel.evolutionSoil"
               , column(8
                        , plotOutput(outputId = "plot.evolutionSoil", width = "100%", height = "600px")
               )
               , column(4
                        , actionButton(inputId = "create.evolutionSoil"
                                       , label = "Run plot"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = HTML(button.style)
                        ) %>% helper(type = "inline"
                                     , title = "Plot evolution soil (PIXELS)"
                                     , size = "l"
                                     , content = help.HTML("docs/reference/POST_FATE.graphic_evolutionSoil_pixels.html")
                        )
               )
             ))
         )
) ## END tabPanel (Global parameters)
