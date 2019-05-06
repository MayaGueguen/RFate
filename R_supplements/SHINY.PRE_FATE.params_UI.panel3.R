
tabPanel(title = HTML("<span class='panel_title'><i class='fa fa-chart-bar'></i> Simulation outputs & graphics</span>"),
         id = "step3",
         sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 12,
             style = "border-width:0px; background-color:#dee2e8; margin-left:15px; margin-top:18px;",
             withMathJax(),
             
             br(),
             fluidRow(
               column(5
                      , directoryInput(inputId = "folder.simul"
                                       , label = param.style("Select the simulation folder :")
                                       , value = '~')
               )
               , column(7
                        , shinyjs::disabled(
                          selectInput(inputId = "graph.simulParam"
                                      , label = param.style("Select the simulation parameters file :")
                                      , choices = NULL
                                      , selected = NULL
                                      , multiple = F
                                      , width = "100%")
                        )
               )
             ) ## END fluidRow
           ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 12,
             wellPanel(id = "main.panel",
                       style = "border-solid:solid; border-width:0px; border-color:#068f96;",
                       tabsetPanel(
                         source("R_supplements/SHINY.PRE_FATE.params_UI.panel3.tab1.R", local = TRUE)$value
                         , source("R_supplements/SHINY.PRE_FATE.params_UI.panel3.tab2.R", local = TRUE)$value
                         , source("R_supplements/SHINY.PRE_FATE.params_UI.panel3.tab3.R", local = TRUE)$value
                       ) ## END tabsetPanel
             ) ## END wellPanel
           ) ## END mainPanel
         ) ## END sidebarLayout
) ## tabPanel
