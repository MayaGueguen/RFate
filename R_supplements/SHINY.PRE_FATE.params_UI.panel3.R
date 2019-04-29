
tabPanel(title = HTML("<p class='panel_title'>C. Create simulation ouputs & graphics</p>"),
         sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 12,
             style = "border-width:0px; background-color:#dee2e8; margin-left:15px; margin-top:18px;",
             withMathJax(),
             
             br(),
             fluidRow(
               column(12
                      , HTML("<span style = 'font-style: italic; font-weight: normal;'>Select the simulation folder :</span>")
                      , br()
                      , directoryInput(inputId = "folder.simul"
                                       , label = ""
                                       , value = '~')
                      , br()
                      , br()
                      , HTML("<span style = 'font-style: italic; font-weight: normal;'>Select the simulation parameters file :</span>")
                      , br()
                      , br()
                      , shinyjs::disabled(
                        selectInput(inputId = "graph.simulParam"
                                    , label = NULL
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
             # shinyjs::hidden(
             wellPanel(id = "main.panel",
                       style = "border-solid:solid; border-width:0px; border-color:#068f96;",
                       tabsetPanel(
                         source("R_supplements/SHINY.PRE_FATE.params_UI.panel3.tab1.R", local = TRUE)$value,
                         source("R_supplements/SHINY.PRE_FATE.params_UI.panel3.tab2.R", local = TRUE)$value
                       ) ## END tabsetPanel
             ) ## END wellPanel
             # ) ## END hidden
           ) ## END mainPanel
         ) ## END sidebarLayout
)