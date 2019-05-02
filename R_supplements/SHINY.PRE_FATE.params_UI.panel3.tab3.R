
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
                                , multiple = T
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
           column(4
                  , br()
                  , uiOutput(outputId = "show.PFGvsHS")
                  , br()
                  , uiOutput(outputId = "show.validationStat")
           )
           , column(4
                  , br()
                  , uiOutput(outputId = "show.PFGrichness")
                  , br()
                  , uiOutput(outputId = "show.PFGcover")
           )
           , column(4
                    , br()
                    , uiOutput(outputId = "show.PFGlight")
                    , br()
                    , uiOutput(outputId = "show.PFGsoil")
           )
         )
         , fluidRow(
           br()
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
) ## END tabPanel (Global parameters)
