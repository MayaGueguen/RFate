
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
               column(6,
                      wellPanel(id = "pfg.panel1",
                                style = "border-solid:solid; border-width:0px;",
                                tabsetPanel(
                                  source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab1.R", local = TRUE)$value
                                  , source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab2.R", local = TRUE)$value
                                  , source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab3.R", local = TRUE)$value
                                ) ## END tabsetPanel
                      ) ## END wellPanel
               )
               , column(6,
                        wellPanel(id = "pfg.panel2",
                                  style = "border-solid:solid; border-width:0px;"))
             ) ## END fluidRow
           ) %>% helper(type = "inline"
                        , title = "Create Plant Functional Group (PFG) for a FATE-HD simulation"
                        , size = "l"
                        , content = help.HTML(html.file = "docs/index.html"
                                              , target.anchor = 'class="section level2"'
                                              , target.class = "pre_fate---build-plant-functional-groups-pfg")
           ) ## END mainPanel
         ) ## END sidebarLayout
) ## tabPanel
