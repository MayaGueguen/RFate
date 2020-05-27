
tabPanel(title = HTML("<span class='panel_title'><i class='fa fa-chart-bar'></i> Simulation outputs & graphics</span>")
         , value = "panel4"
         , sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 12,
             style = HTML(paste0("border-width:0px; background-color:", help.color, "; margin-left:15px; margin-top:18px;")),
             withMathJax(),
             
             br(),
             fluidRow(
               column(5
                      , div(id = "help4_1"
                            , directoryInput(inputId = "graph.folder.simul"
                                             , label = param.style("Select the simulation folder :")
                                             , value = '~')
                      )
               )
               , column(5
                        , div(id = "help4_2"
                              , shinyjs::disabled(
                                selectInput(inputId = "graph.simulParam"
                                            , label = param.style("Select the simulation parameters file :")
                                            , choices = NULL
                                            , selected = NULL
                                            , multiple = F
                                            , width = "100%")
                              )
                        )
               )
               , column(2
                        , br()
                        , actionButton(inputId = "HELP.panel4"
                                       , label = "Need some help"
                                       , icon = icon("question-circle", class = "icon-help")
                                       , width = "100%"
                                       , style = HTML(button.style.help)))
             ) ## END fluidRow
           ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 12,
             wellPanel(id = "main.panel",
                       style = "border-solid:solid; border-width:0px;",
                       tabsetPanel(
                         source("SHINY.PRE_FATE.params_UI.panel4.tab1.R", local = TRUE)$value
                         , source("SHINY.PRE_FATE.params_UI.panel4.tab2.R", local = TRUE)$value
                         , source("SHINY.PRE_FATE.params_UI.panel4.tab3.R", local = TRUE)$value
                       ) ## END tabsetPanel
             ) ## END wellPanel
           ) ## END mainPanel
         # ) %>% helper(type = "inline"
         #              , title = "Evaluate FATE-HD simulation outputs"
         #              , size = "l"
         #              , content = help.HTML(html.file = "https://mayagueguen.github.io/RFate/articles/rfate_tutorial_3_graphics.html"
         #                                    , target.anchor = 'class="hidden name'
         #                                    , target.class = "rfate_tutorial_3_graphics.Rmd")
         ) ## END sidebarLayout
) ## tabPanel
