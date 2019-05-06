
tabPanel(title = HTML("<span class='tabPanel_title'>PFG files</span>")
         , value = "create.PFG"
         , br()
         , wellPanel(
           fluidRow(
             column(1
                    , br()
                    , HTML("<strong>PFG</strong>")
             )
             , column(2
                      , br()
                      , textInput(inputId = "name.PFG"
                                  , label = NULL
                                  , width = "100%")
             )
             , column(2
                      , br()
                      , shinyjs::disabled(
                        actionButton(inputId = "add.PFG.name"
                                     , label = "Add PFG"
                                     , icon = icon("plus")
                                     , width = "100%"
                                     , style = HTML(button.style)
                        )
                      )
             )
             , column(5
                      , br()
                      , wellPanel(style = "overflow-x:scroll;"
                                  , textOutput(outputId = "names.PFG"))
             )
             , column(2
                      , br()
                      , actionButton(inputId = "delete.names.PFG"
                                     , label = NULL
                                     , icon = icon("trash")
                                     , style = HTML(button.style)
                      )
             )
           )
         ) ## END wellPanel
         , tabsetPanel(
           tabPanel(title = HTML("<p class='tabPanel_subtitle'>Succession - Light</p>")
                    , value = "panel.succ"
                    , fluidRow(
                      column(6
                             , br()
                             , shinyjs::disabled(
                               actionButton(inputId = "add.PFG.succ"
                                            , label = "Add PFG"
                                            , icon = icon("plus")
                                            , width = "100%"
                                            , style = HTML(button.style)
                               )
                             )
                      )
                      , column(6
                               , br()
                               , shinyjs::disabled(
                                 actionButton(inputId = "create.succ"
                                              , label = "Create PFG succession files"
                                              , icon = icon("file")
                                              , width = "100%"
                                              , style = HTML(button.style)
                                 ) %>% helper(type = "inline"
                                              , title = "Create PFG succession files"
                                              , size = "l"
                                              , content = help.full(param.web = "https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGsuccession.html"
                                                                    , param.name.vec = c("<hr/>"
                                                                                         , "type"
                                                                                         , "height"
                                                                                         , "maturity"
                                                                                         , "longevity")
                                                                    , param.desc.vec = c("<hr/>"
                                                                                         , "or life-form, based on Raunkier.
                                                                                         It should be either <span style='font-family:Monospace;'>H</span> (herbaceous),
                                                                                         <span style='font-family:Monospace;'>C</span> (chamaephyte) or <span style='font-family:Monospace;'>P</span> (phanerophyte)"
                                                                                         , "the maximum or average height that reach the PFG"
                                                                                         , "the age from which the PFG can reproduce"
                                                                                         , "the maximum or average lifespan of the PFG")
                                              ))
                               )
                               , br()
                               , br()
                               , shinyjs::disabled(
                                 actionButton(inputId = "create.light"
                                              , label = "Create PFG light files"
                                              , icon = icon("file")
                                              , width = "100%"
                                              , style = HTML(button.style)
                                 ) %>% helper(type = "inline"
                                              , title = "Create PFG light files"
                                              , size = "l"
                                              , content = help.full(param.web = "https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGsuccession.html"
                                                                    , param.name.vec = c("<hr/>"
                                                                                         , "light")
                                                                    , param.desc.vec = c("<hr/>"
                                                                                         , "a value between 0 and 5 corresponding to the light value of the PFG (e.g. from Flora Indicativa)")
                                              ))
                               )
                      )
                      )
                    , fluidRow(
                      column(2
                             , br()
                             , br()
                             , HTML("<strong>PFG</strong>")
                             , shinyjs::disabled(
                               selectInput(inputId = "succ.PFG"
                                           , label = NULL
                                           , choices = NULL
                                           , selected = NULL
                                           , multiple = F
                                           , width = "100%")
                             ))
                      , column(2
                               , br()
                               , br()
                               , HTML("<strong>type</strong>")
                               , selectInput(inputId = "succ.type"
                                             , label = NULL
                                             , choices = c("H", "C", "P")
                                             , selected = NULL
                                             , multiple = F
                                             , width = "100%"))
                      , column(2
                               , br()
                               , br()
                               , HTML("<strong>height</strong>")
                               , numericInput(inputId = "succ.height"
                                              , label = NULL
                                              , value = 0
                                              , min = 0
                                              , width = "100%"))
                      , column(2
                               , br()
                               , br()
                               , HTML("<strong>maturity</strong>")
                               , numericInput(inputId = "succ.maturity"
                                              , label = NULL
                                              , value = 0
                                              , min = 0
                                              , width = "100%"))
                      , column(2
                               , br()
                               , br()
                               , HTML("<strong>longevity</strong>")
                               , numericInput(inputId = "succ.longevity"
                                              , label = NULL
                                              , value = 0
                                              , min = 0
                                              , width = "100%"))
                      , column(2
                               , br()
                               , br()
                               , HTML("<strong>light</strong>")
                               , selectInput(inputId = "succ.light"
                                             , label = NULL
                                             , choices = 0:5
                                             , selected = NULL
                                             , multiple = F
                                             , width = "100%"))
                    )
                    , fluidRow(
                      column(11
                             , br()
                             , tableOutput(outputId = "mat.PFG.ALL"))
                      , column(1
                               , br()
                               , actionButton(inputId = "delete.PFG.ALL"
                                              , label = NULL
                                              , icon = icon("trash")
                                              , style = HTML(button.style)
                               )
                      )
                    )
                    , fluidRow(
                      br(),
                      wellPanel(style = "overflow-x:scroll;"
                                , dataTableOutput(outputId = "created_table.succ"))
                    )
                    , fluidRow(
                      br(),
                      wellPanel(style = "overflow-x:scroll;"
                                , dataTableOutput(outputId = "created_table.light"))
                    )
                               ) ## END tabPanel (succ)
           , tabPanel(title = HTML("<p class='tabPanel_subtitle'>Dispersal</p>")
                      , value = "panel.disp"
                      , fluidRow(
                        column(6
                               , br()
                               , shinyjs::disabled(
                                 actionButton(inputId = "add.PFG.disp"
                                              , label = "Add PFG"
                                              , icon = icon("plus")
                                              , width = "100%"
                                              , style = HTML(button.style)
                                 )
                               )
                        )
                        , column(6
                                 , br()
                                 , shinyjs::disabled(
                                   actionButton(inputId = "create.disp"
                                                , label = "Create PFG dispersal files"
                                                , icon = icon("file")
                                                , width = "100%"
                                                , style = HTML(button.style)
                                   ) %>% helper(type = "inline"
                                                , title = "Create PFG dispersal files"
                                                , size = "l"
                                                , content = help.full(param.web = "https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGdispersal.html"
                                                                      , param.name.vec = c("<hr/>"
                                                                                           , "MODE :"
                                                                                           , "[1] uniform kernel"
                                                                                           , "[2] exponential kernel"
                                                                                           , "[3] exponential kernel <br/>with probability"
                                                                                           , "d50"
                                                                                           , "d99"
                                                                                           , "ldd")
                                                                      , param.desc.vec = c("<hr/>"
                                                                                           , ""
                                                                                           , "homogeneous dispersal within the <span style='font-family:Monospace;'>d50</span>,
                                                                                           <span style='font-family:Monospace;'>d99</span> and <span style='font-family:Monospace;'>ldd</span> circles"
                                                                                           , "seeds are dispersed within each concentric circle according to a decreasing exponential density law (lambda = 1)"
                                                                                           , "seeds are dispersed within each concentric circle according to a decreasing exponential density law (lambda = 1) and a continuous decreasing probability with distance"
                                                                                           , "the distance at which 50% of seeds are dispersed"
                                                                                           , "the distance at which 49% of seeds are dispersed"
                                                                                           , "the long dispersal distance at which 1% of seeds are dispersed")
                                                ))
                                 )
                        )
                      )
                      , fluidRow(
                        column(2
                               , br()
                               , br()
                               , HTML("<strong>PFG</strong>")
                               , shinyjs::disabled(
                                 selectInput(inputId = "disp.PFG"
                                             , label = NULL
                                             , choices = NULL
                                             , selected = NULL
                                             , multiple = F
                                             , width = "100%")
                               ))
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>MODE</strong>")
                                 , selectInput(inputId = "disp.mode"
                                               , label = NULL
                                               , choices = 1:3
                                               , selected = NULL
                                               , multiple = F
                                               , width = "100%"))
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>d50</strong>")
                                 , numericInput(inputId = "disp.d50"
                                                , label = NULL
                                                , value = 0
                                                , min = 0
                                                , width = "100%"))
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>d99</strong>")
                                 , numericInput(inputId = "disp.d99"
                                                , label = NULL
                                                , value = 0
                                                , min = 0
                                                , width = "100%"))
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>ldd</strong>")
                                 , numericInput(inputId = "disp.ldd"
                                                , label = NULL
                                                , value = 0
                                                , min = 0
                                                , width = "100%"))
                      )
                      , fluidRow(
                        column(10
                               , br()
                               , tableOutput(outputId = "mat.PFG.disp"))
                        , column(2
                                 , br()
                                 , actionButton(inputId = "delete.PFG.disp"
                                                , label = NULL
                                                , icon = icon("trash")
                                                , style = HTML(button.style)
                                 )
                        )
                      )
                      , fluidRow(
                        br(),
                        wellPanel(style = "overflow-x:scroll;"
                                  , dataTableOutput(outputId = "created_table.disp"))
                      )
           ) ## END tabPanel (dispersal)
           , tabPanel(title = HTML("<p class='tabPanel_subtitle'>Disturbances</p>")
                      , value = "panel.dist"
                      , fluidRow(
                        column(6
                               , br()
                               , actionButton(inputId = "add.PFG.dist"
                                              , label = "Add disturbance"
                                              , icon = icon("plus")
                                              , width = "100%"
                                              , style = HTML(button.style)
                               )
                        )
                        , column(6
                                 , br()
                                 , shinyjs::disabled(
                                   actionButton(inputId = "create.dist"
                                                , label = "Create PFG disturbance files"
                                                , icon = icon("file")
                                                , width = "100%"
                                                , style = HTML(button.style)
                                   ) %>% helper(type = "inline"
                                                , title = "Create PFG disturbance files"
                                                , size = "l"
                                                , content = help.full(param.web = "https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGdisturbance.html"
                                                                      , param.name.vec = c("<hr/>"
                                                                                           , "dist.name"
                                                                                           , "dist.kill"
                                                                                           , "dist.resprout")
                                                                      , param.desc.vec = c("<hr/>"
                                                                                           , "the name of the perturbation"
                                                                                           , "mortality percentage that the perturbation will induced for each response stage"
                                                                                           , "resprouting percentage that the perturbation will induced for each response stage")
                                                ))
                                 )
                        )
                      )
                      , fluidRow(
                        column(4
                               , br()
                               , br()
                               , HTML("<strong>Disturbance</strong>")
                               , textInput(inputId = "dist.name"
                                           , label = NULL
                                           , width = "100%"))
                        , column(4
                                 , br()
                                 , br()
                                 , HTML("<strong>Disturbance</strong>")
                                 , radioButtons(inputId = "dist.grouping"
                                                , label = NULL
                                                , choices = c("by type", "by PFG")
                                                , selected = "by type"
                                                , width = "100%"))
                      )
                      ,fluidRow(
                        uiOutput(outputId = "UI.dist.grouping")
                      )
                      , fluidRow(
                        column(10
                               , br()
                               , wellPanel(style = "overflow-x:scroll;"
                                           , tableOutput(outputId = "mat.PFG.dist")))
                        , column(2
                                 , br()
                                 , actionButton(inputId = "delete.PFG.dist"
                                                , label = NULL
                                                , icon = icon("trash")
                                                , style = HTML(button.style)
                                 )
                        )
                      )
                      , fluidRow(
                        br(),
                        wellPanel(style = "overflow-x:scroll;"
                                  , dataTableOutput(outputId = "created_table.dist"))
                      )
           ) ## END tabPanel (disturbances)
           , tabPanel(title = HTML("<p class='tabPanel_subtitle'>Soil</p>")
                      , value = "panel.soil"
                      , fluidRow(
                        column(6
                               , br()
                               , shinyjs::disabled(
                                 actionButton(inputId = "add.PFG.soil"
                                              , label = "Add PFG"
                                              , icon = icon("plus")
                                              , width = "100%"
                                              , style = HTML(button.style)
                                 )
                               )
                        )
                        , column(6
                                 , br()
                                 , shinyjs::disabled(
                                   actionButton(inputId = "create.soil"
                                                , label = "Create PFG soil files"
                                                , icon = icon("file")
                                                , width = "100%"
                                                , style = HTML(button.style)
                                   ) %>% helper(type = "inline"
                                                , title = "Create PFG soil files"
                                                , size = "l"
                                                , content = help.full(param.web = "https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGsoil.html"
                                                                      , param.name.vec = c("<hr/>"
                                                                                           , "soil contrib"
                                                                                           , "soil min tolerance"
                                                                                           , "soil max tolerance")
                                                                      , param.desc.vec = c("<hr/>"
                                                                                           , "the contribution (influence) of the PFG on the nitrogen soil value of the pixel"
                                                                                           , "the lower value of nitrogen supported by the PFG"
                                                                                           , "the upper value of nitrogen supported by the PFG")
                                                ))
                                 )
                        )
                      )
                      , fluidRow(
                        column(2
                               , br()
                               , br()
                               , HTML("<strong>PFG</strong>")
                               , shinyjs::disabled(
                                 selectInput(inputId = "soil.PFG"
                                             , label = NULL
                                             , choices = NULL
                                             , selected = NULL
                                             , multiple = F
                                             , width = "100%")
                               ))
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>type</strong>")
                                 , selectInput(inputId = "soil.type"
                                               , label = NULL
                                               , choices = c("H", "C", "P")
                                               , selected = NULL
                                               , multiple = F
                                               , width = "100%"))
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>soil contribution</strong>")
                                 , numericInput(inputId = "soil.contrib"
                                                , label = NULL
                                                , value = 0
                                                , min = 0
                                                , max = 5
                                                , width = "100%"))
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>soil min tolerance</strong>")
                                 , numericInput(inputId = "soil.tol_min"
                                                , label = NULL
                                                , value = 0
                                                , min = 0
                                                , max = 5
                                                , width = "100%"))
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>soil max tolerance</strong>")
                                 , numericInput(inputId = "soil.tol_max"
                                                , label = NULL
                                                , value = 0
                                                , min = 0
                                                , max = 5
                                                , width = "100%"))
                      )
                      , fluidRow(
                        column(2, br())
                        , column(2
                                 , br()
                                 , br()
                                 , br()
                                 , br()
                                 , HTML("<strong>Germinant</strong>")
                                 , br()
                                 , br()
                                 , HTML("<strong>Immature</strong>")
                                 , br()
                                 , br()
                                 , HTML("<strong>Mature</strong>")
                        )
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>Low</strong>")
                                 , selectInput(inputId = "soil.Ge.L"
                                               , label = NULL
                                               , choices = seq(0,100,10)
                                               , multiple = FALSE
                                               , width = "100%")
                                 , selectInput(inputId = "soil.Im.L"
                                               , label = NULL
                                               , choices = seq(0,100,10)
                                               , multiple = FALSE
                                               , width = "100%")
                                 , selectInput(inputId = "soil.Ma.L"
                                               , label = NULL
                                               , choices = seq(0,100,10)
                                               , multiple = FALSE
                                               , width = "100%"))
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>Medium</strong>")
                                 , selectInput(inputId = "soil.Ge.M"
                                               , label = NULL
                                               , choices = seq(0,100,10)
                                               , multiple = FALSE
                                               , width = "100%")
                                 , selectInput(inputId = "soil.Im.M"
                                               , label = NULL
                                               , choices = seq(0,100,10)
                                               , multiple = FALSE
                                               , width = "100%")
                                 , selectInput(inputId = "soil.Ma.M"
                                               , label = NULL
                                               , choices = seq(0,100,10)
                                               , multiple = FALSE
                                               , width = "100%"))
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>High</strong>")
                                 , selectInput(inputId = "soil.Ge.H"
                                               , label = NULL
                                               , choices = seq(0,100,10)
                                               , multiple = FALSE
                                               , width = "100%")
                                 , selectInput(inputId = "soil.Im.H"
                                               , label = NULL
                                               , choices = seq(0,100,10)
                                               , multiple = FALSE
                                               , width = "100%")
                                 , selectInput(inputId = "soil.Ma.H"
                                               , label = NULL
                                               , choices = seq(0,100,10)
                                               , multiple = FALSE
                                               , width = "100%"))
                      )
                      , fluidRow(
                        column(10
                               , br()
                               , wellPanel(style = "overflow-x:scroll;"
                                           , tableOutput(outputId = "mat.PFG.soil")))
                        , column(2
                                 , br()
                                 , actionButton(inputId = "delete.PFG.soil"
                                                , label = NULL
                                                , icon = icon("trash")
                                                , style = HTML(button.style)
                                 )
                        )
                      )
                      , fluidRow(
                        br(),
                        wellPanel(style = "overflow-x:scroll;"
                                  , dataTableOutput(outputId = "created_table.soil"))
                      )
           ) ## END tabPanel (soil)
           ) ## END tabSetPanel
           ) ## END tabPanel (PFG files)
