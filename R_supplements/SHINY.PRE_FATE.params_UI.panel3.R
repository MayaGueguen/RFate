
tabPanel(title = HTML("<p class='panel_title'>C. Simulation outputs & graphics</p>"),
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
                                       , value = '~')# %>% helper(type = "inline"
                                                                 # , title = "Plot"
                                                                 # , content = c("This is a <b>plot</b>.",
                                                                 #               "This is on a new line."))
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
)