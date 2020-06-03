
tabPanel(title =  HTML("<span class='panel_title'><i class='fa fa-object-group'></i> Plant Functional Groups</span>")
         , value = "panel1"
         , sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 12,
             style = HTML(paste0("border-width:0px; background-color:", help.color, "; margin-left:15px; margin-top:18px;")),
             withMathJax(),
             
             br(),
             fluidRow(
               column(5
                      , div(id = "help1_2"
                            , fileInput(inputId = "species.observations"
                                        , label = NULL
                                        , buttonLabel = param.style("species.observations")
                                        , multiple = FALSE
                                        , accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")
                                        , width = "100%")
                      )
               )
               , column(5
                        , div(id = "help1_3"
                              , fileInput(inputId = "species.traits"
                                          , label = NULL
                                          , buttonLabel = param.style("species.traits")
                                          , multiple = FALSE
                                          , accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")
                                          , width = "100%")
                        )
               )
               , column(2
                        , actionButton(inputId = "HELP.panel1"
                                       , label = "Need some help"
                                       , icon = icon("question-circle", class = "icon-help")
                                       , width = "100%"
                                       , style = HTML(button.style.help)))
             ) ## END fluidRow
           ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 12,
             fluidRow(
               column(4,
                      wellPanel(id = "pfg.panel1",
                                style = "border-solid:solid; border-width:0px;",
                                tabsetPanel(
                                  source("SHINY.PRE_FATE.params_UI.panel1.tab1.R", local = TRUE)$value
                                  , source("SHINY.PRE_FATE.params_UI.panel1.tab2.R", local = TRUE)$value
                                  , source("SHINY.PRE_FATE.params_UI.panel1.tab3.R", local = TRUE)$value
                                ) ## END tabsetPanel
                      ) ## END wellPanel
               )
               , column(8,
                        wellPanel(id = "pfg.panel2",
                                  style = "border-solid:solid; border-width:0px;",
                                  tabsetPanel(
                                    tabPanel(title = HTML("<span class='tabPanel_title'>Graphics</span>")
                                               , value = "panel.graphics"
                                               , fluidRow(
                                                 column(11
                                                        , br()
                                                        , uiOutput(outputId = "UI.pfg.browser")
                                                 )
                                                 , column(1
                                                          , br()
                                                          , br()
                                                          , br()
                                                          , shinyjs::disabled(
                                                            actionButton(inputId = "pfg.go.left"
                                                                         , label = ""
                                                                         , icon = icon("arrow-circle-left", class = "icon-help")
                                                                         , width = "100%"
                                                                         , style = HTML(button.style.help))
                                                          )
                                                          , br()
                                                          , br()
                                                          , shinyjs::disabled(
                                                            actionButton(inputId = "pfg.go.right"
                                                                         , label = ""
                                                                         , icon = icon("arrow-circle-right", class = "icon-help")
                                                                         , width = "100%"
                                                                         , style = HTML(button.style.help))
                                                          )
                                                 )
                                               ) ## END fluidRow
                                    ) ## END tabPanel
                                    , tabPanel(title = HTML("<span class='tabPanel_title'>Observations</span>")
                                             , value = "panel.observations"
                                             , fluidRow(
                                               column(12
                                                      , wellPanel(style = HTML(paste0("border-width:0px; background-color:", help.color, "; margin-top:18px; overflow-y:scroll; max-height:400px;"))
                                                                  , shinyjs::hidden(
                                                                    dataTableOutput(outputId = "table.observations"
                                                                                    , width = "100%"
                                                                                    , height = "300px")
                                                                  )
                                                      )
                                               )
                                             ) ## END fluidRow
                                    ) ## END tabPanel
                                    , tabPanel(title = HTML("<span class='tabPanel_title'>Traits (species)</span>")
                                               , value = "panel.traits.sp"
                                               , fluidRow(
                                                 column(12
                                                        , wellPanel(style = HTML(paste0("border-width:0px; background-color:", help.color, "; margin-top:18px; overflow-y:scroll; max-height:400px;"))
                                                                    , shinyjs::hidden(
                                                                      dataTableOutput(outputId = "table.traits.sp"
                                                                                      , width = "100%"
                                                                                      , height = "600px")
                                                                    )
                                                        )
                                                 )
                                               ) ## END fluidRow
                                    ) ## END tabPanel
                                    , tabPanel(title = HTML("<span class='tabPanel_title'>Traits (PFG)</span>")
                                               , value = "panel.traits.pfg"
                                               , fluidRow(
                                                 column(12
                                                        , wellPanel(style = HTML(paste0("border-width:0px; background-color:", help.color, "; margin-top:18px; overflow-y:scroll; max-height:400px;"))
                                                                    , shinyjs::hidden(
                                                                      dataTableOutput(outputId = "table.traits.pfg"
                                                                                      , width = "100%"
                                                                                      , height = "600px")
                                                                    )
                                                        )
                                                 )
                                               ) ## END fluidRow
                                    ) ## END tabPanel
                                  ) ## END tabsetPanel
                        ) ## END wellPanel
               )
             ) ## END fluidRow
           ) %>% helper(type = "inline"
                        , title = "Create Plant Functional Group (PFG) for a FATE simulation"
                        , size = "l"
                        , content = help.HTML(html.file = "https://mayagueguen.github.io/RFate/articles/rfate_tutorial_1_PFG.html"
                                              , target.anchor = 'class="section level2"'
                                              , target.class = "what-are-the-key-steps-of-this-process")
           ) ## END mainPanel
         ) ## END sidebarLayout
) ## tabPanel
