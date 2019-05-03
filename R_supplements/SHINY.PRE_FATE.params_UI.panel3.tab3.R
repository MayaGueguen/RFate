
tabPanel(title = HTML("<p class='tabPanel_title'>Specific year</p>")
         , value = "panel.specific_year"
         , br()
         , wellPanel(
           style = HTML(paste0("background-color: ", help.color, ";")),
           helpText(HTML("
                         <p><a href='https://mayagueguen.github.io/RFate/reference/POST_FATE.relativeAbund_presenceAbsence.html' target='_blank'>
                         See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
                         <table style='width:100%;'>
                         
                         </table>
                         "
           ))) ## END wellPanel
         , fluidRow(
           column(3
                  , br()
                  , shinyjs::disabled(
                    selectInput(inputId = "graph.year"
                                , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>year(s)</span>")
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
                                  , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>strata_min</span>")
                                  , choices = NULL
                                  , selected = NULL
                                  , multiple = F
                                  , width = "100%")
                    )
           )
           , column(3
                    , br()
                    , numericInput(inputId = "graph.rel_abund_thresh"
                                   , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>opt.rel_abund_thresh</span>")
                                   , value = 0.05
                                   , min = 0
                                   , max = 1
                                   , width = "100%")
           )
           , column(3
                    , br()
                    , numericInput(inputId = "graph.opt.no_CPU"
                                   , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>opt.no_CPU</span>")
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
                                 , label = "Run relative & binary abund"
                                 , icon = icon("play")
                                 , width = "100%"
                                 , style = HTML(button.style)
                    )
                  )
           )
           , column(9
                    , wellPanel(
                      style = HTML(paste0("background-color: ", help.color, ";")),
                      helpText(HTML("
                         <p><a href='https://mayagueguen.github.io/RFate/reference/POST_FATE.relativeAbund_presenceAbsence.html' target='_blank'>
                         See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
                         <table style='width:100%;'>
                         
                         </table>
                         "
                      ))) ## END wellPanel
           )
         )
         , fluidRow(
           column(6
                  , br()
                  , fileInput(inputId = "graph.mat.PFG.obs"
                              , label = NULL
                              , buttonLabel = HTML("<span style = 'font-style: italic; font-weight: normal;'>mat.PFG.obs</span>")
                              , multiple = FALSE
                              , width = "100%")
           )
           # , column(4
           #          , br()
           #          , fileInput(inputId = "graph.mat.PFG.succ"
           #                      , label = NULL
           #                      , buttonLabel = HTML("<span style = 'font-style: italic; font-weight: normal;'>mat.PFG.succ</span>")
           #                      , multiple = FALSE
           #                      , width = "100%")
           # )
           , column(6
                    , br()
                    , fileInput(inputId = "graph.opt.cover.obs"
                                , label = NULL
                                , buttonLabel = HTML("<span style = 'font-style: italic; font-weight: normal;'>opt.cover.obs</span>")
                                , multiple = FALSE
                                , width = "100%")
                    # , fileInput(inputId = "graph.opt.light.obs"
                    #             , label = NULL
                    #             , buttonLabel = HTML("<span style = 'font-style: italic; font-weight: normal;'>opt.light.obs</span>")
                    #             , multiple = FALSE
                    #             , width = "100%")
                    # , fileInput(inputId = "graph.opt.soil.obs"
                    #             , label = NULL
                    #             , buttonLabel = HTML("<span style = 'font-style: italic; font-weight: normal;'>opt.soil.obs</span>")
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
                                 ))
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
                                 ))
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
                                 ))
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
                                 ))
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
                                 ))
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
                                 ))
                      ))
           )
         )
) ## END tabPanel (Global parameters)
