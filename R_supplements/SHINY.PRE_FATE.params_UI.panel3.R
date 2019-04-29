
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
                      # , shinyjs::hidden(
                      #   fileInput(inputId = "graph.simulParam"
                      #             , label = "Select simulation parameters file"
                      #             , multiple =  FALSE
                      #             # , buttonLabel = 
                      #             , width = "100%")
                      #             # , style = HTML(button.style)
                      # )
               )
             )
           ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 12,
             # shinyjs::hidden(
               wellPanel(id = "main.panel",
                         style = "border-solid:solid; border-width:0px; border-color:#068f96;",
                         tabsetPanel(
                           # source("R_supplements/SHINY.PRE_FATE.params_UI.tab31.R", local = TRUE)$value
                           # source("R_supplements/SHINY.PRE_FATE.params_UI.tab2.R", local = TRUE)$value,
                           # source("R_supplements/SHINY.PRE_FATE.params_UI.tab3.R", local = TRUE)$value,
                           # source("R_supplements/SHINY.PRE_FATE.params_UI.tab4.R", local = TRUE)$value
                         ) ## END tabsetPanel
               ) ## END wellPanel
             # ) ## END hidden
           ) ## END mainPanel
         ) ## END sidebarLayout
)