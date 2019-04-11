
tabPanel(title = HTML("<p class='tabPanel_title'>PFG files</p>")
         , value = "create.PFG"
         , br()
         , fluidRow(
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
                                   , style = HTML(paste0("background-color: ", button.color, ";"))
                      )
                    )
           )
           , column(5
                    , br()
                    , wellPanel(textOutput(outputId = "names.PFG"))
           )
           , column(2
                    , br()
                    , actionButton(inputId = "delete.names.PFG"
                                   , label = NULL
                                   , icon = icon("trash")
                                   , style = HTML(paste0("background-color: ", button.color, ";"))
                    )
           )
         )
         , tabsetPanel(
           tabPanel(title = HTML("<p class='tabPanel_subtitle'>Succession</p>")
                    , value = "panel.succ"
                    , br()
                    , wellPanel(
                      style = HTML(paste0("background-color: ", button.color, ";")),
                      helpText(HTML("
                                    <p><a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGsuccession.html' target='_blank'>
                                    See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
                                    <table style='width:100%;'>
                                    <tr>
                                    <td style='width:30%;font-family:Monospace;vertical-align:top;'>type</td>
                                    <td style='width:70%;'>or life-form, based on Raunkier.
                                    It should be either <span style='font-family:Monospace;'>H</span> (herbaceous),
                                    <span style='font-family:Monospace;'>C</span> (chamaephyte) or <span style='font-family:Monospace;'>P</span> (phanerophyte) for now</td>
                                    </tr>
                                    <tr>
                                    <td style='width:30%;font-family:Monospace;vertical-align:top;'>height</td>
                                    <td style='width:70%;'>the maximum or average height that reach the PFG</td>
                                    </tr>
                                    <tr>
                                    <td style='width:30%;font-family:Monospace;vertical-align:top;'>maturity</td>
                                    <td style='width:70%;'>the age from which the PFG can reproduce</td>
                                    </tr>
                                    <tr>
                                    <td style='width:30%;font-family:Monospace;vertical-align:top;'>longevity</td>
                                    <td style='width:70%;'>the maximum or average lifespan of the PFG</td>
                                    </tr>
                                    <tr>
                                    <td style='width:30%;font-family:Monospace;vertical-align:top;'>light</td>
                                    <td style='width:70%;'>a value between 0 and 10 corresponding to the Ellenberg value of the PFG</td>
                                    </tr>
                                    </table>
                                    "
                      )))
                    , fluidRow(
                      column(6
                             , br()
                             , shinyjs::disabled(
                               actionButton(inputId = "add.PFG.succ"
                                            , label = "Add PFG"
                                            , icon = icon("plus")
                                            , width = "100%"
                                            , style = HTML(paste0("background-color: ", button.color, ";"))
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
                                              , style = HTML(paste0("background-color: ", button.color, ";"))
                                 )
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
                                             , choices = 0:10
                                             , selected = NULL
                                             , multiple = F
                                             , width = "100%"))
                    )
                    , fluidRow(
                      column(10
                             , br()
                             , tableOutput(outputId = "mat.PFG.succ"))
                      , column(2
                               , br()
                               , actionButton(inputId = "delete.PFG.succ"
                                              , label = NULL
                                              , icon = icon("trash")
                                              , style = HTML(paste0("background-color: ", button.color, ";"))
                               )
                      )
                    )
                    , fluidRow(
                      br(),
                      wellPanel(dataTableOutput(outputId = "created_table.succ"))
                    )
                      )
           , tabPanel(title = HTML("<p class='tabPanel_subtitle'>Dispersal</p>")
                      , value = "panel.disp"
                      , br()
                      , wellPanel(
                        style = HTML(paste0("background-color: ", button.color, ";")),
                        helpText(HTML("
                                      <p><a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGdispersal.html' target='_blank'>
                                      See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
                                      <table style='width:100%;'>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>MODE :</td>
                                      </tr>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>[1] uniform kernel</td>
                                      <td style='width:70%;'>homogeneous dispersal within the <span style='font-family:Monospace;'>d50</span>,
                                      <span style='font-family:Monospace;'>d99</span> and <span style='font-family:Monospace;'>ldd</span> circles</td>
                                      </tr>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>[2] exponential kernel</td>
                                      <td style='width:70%;'>seeds are dispersed within each concentric circle according to a decreasing exponential density law (lambda = 1)</td>
                                      </tr>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>[3] exponential kernel <br/>with probability</td>
                                      <td style='width:70%;'>seeds are dispersed within each concentric circle according to a decreasing exponential density law (lambda = 1) and a continuous decreasing probability with distance</td>
                                      </tr>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>d50</td>
                                      <td style='width:70%;'>the distance at which 50% of seeds are dispersed</td>
                                      </tr>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>d99</td>
                                      <td style='width:70%;'>the distance at which 49% of seeds are dispersed</td>
                                      </tr>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>ldd</td>
                                      <td style='width:70%;'>the long dispersal distance at which 1% of seeds are dispersed</td>
                                      </tr>
                                      </table>
                                      "
                        )))
                      , fluidRow(
                        column(6
                               , br()
                               , actionButton(inputId = "add.PFG.disp"
                                              , label = "Add PFG"
                                              , icon = icon("plus")
                                              , width = "100%"
                                              , style = HTML(paste0("background-color: ", button.color, ";"))
                               )
                        )
                        , column(6
                                 , br()
                                 , actionButton(inputId = "create.disp"
                                                , label = "Create PFG dispersal files"
                                                , icon = icon("file")
                                                , width = "100%"
                                                , style = HTML(paste0("background-color: ", button.color, ";"))
                                 )
                        )
                      )
                      , fluidRow(
                        column(2
                               , br()
                               , br()
                               , HTML("<strong>PFG</strong>")
                               , uiOutput(outputId = "UI.disp.PFG"))
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
                                                , style = HTML(paste0("background-color: ", button.color, ";"))
                                 )
                        )
                      )
                      , fluidRow(
                        br(),
                        wellPanel(dataTableOutput(outputId = "created_table.disp"))
                      )
                        )
           , tabPanel(title = HTML("<p class='tabPanel_subtitle'>Disturbances</p>")
                      , value = "panel.dist"
                      , br()
                      , wellPanel(
                        style = HTML(paste0("background-color: ", button.color, ";")),
                        helpText(HTML("
                                      <p><a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGdisturbance.html' target='_blank'>
                                      See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
                                      <table style='width:100%;'>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>dist.name</td>
                                      <td style='width:70%;'>the name of the perturbation</td>
                                      </tr>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>dist.kill</td>
                                      <td style='width:70%;'>mortality percentage that the perturbation will induced for each response stage</td>
                                      </tr>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>dist.resprout</td>
                                      <td style='width:70%;'>resprouting percentage that the perturbation will induced for each response stage</td>
                                      </tr>
                                      </table>
                                      "
                        )))
                      , fluidRow(
                        column(6
                               , br()
                               , actionButton(inputId = "add.PFG.dist"
                                              , label = "Add disturbance"
                                              , icon = icon("plus")
                                              , width = "100%"
                                              , style = HTML(paste0("background-color: ", button.color, ";"))
                               )
                        )
                        , column(6
                                 , br()
                                 , actionButton(inputId = "create.dist"
                                                , label = "Create PFG disturbance files"
                                                , icon = icon("file")
                                                , width = "100%"
                                                , style = HTML(paste0("background-color: ", button.color, ";"))
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
                               , tableOutput(outputId = "mat.PFG.dist"))
                        , column(2
                                 , br()
                                 , actionButton(inputId = "delete.PFG.dist"
                                                , label = NULL
                                                , icon = icon("trash")
                                                , style = HTML(paste0("background-color: ", button.color, ";"))
                                 )
                        )
                      )
                      , fluidRow(
                        br(),
                        wellPanel(dataTableOutput(outputId = "created_table.dist"))
                      )
                        )
                      ) ## END tabSetPanel
                      ) ## END tabPanel (PFG files)
