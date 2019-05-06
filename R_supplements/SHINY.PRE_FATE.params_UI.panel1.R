
tabPanel(title = HTML("<p class='panel_title'><i class='fa fa-folder-plus'></i> New</p>"),
        # icon = "question",
         id = "step1",
         sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 3,
             style = "border-width:0px; background-color:#dee2e8; margin-left:15px; margin-top:18px;",
             withMathJax(),
             
             br(),
             br(),
             br(),
             fluidRow(
               column(12
                      , textInput(inputId = "name.simul"
                                  , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>Enter the simulation name :</span>")
                                  , value = "FATE_simulation"
                                  , width = "100%")
               )
             ),
             fluidRow(
               column(12
                      , actionButton(inputId = "create.skeleton"
                                     , label = "Create folder"
                                     , icon = icon("folder")
                                     , width = "100%"
                                     , style = HTML(button.style)
                      )
               )
             ),
             br(),
             helpText(HTML("
                           <ul style='font-size:75%;padding-left:20px;'>
                           <li>DATA
                           <ul>
                           <li>GLOBAL PARAMETERS</li>
                           <li>MASK</li>
                           <li>SCENARIO</li>
                           <li>SAVE</li>
                           <li>PFGS :
                           <ul>
                           <li>SUCC</li>
                           <li>DISP</li>
                           <li>HABSUIT</li>
                           <li>LIGHT</li>
                           <li>SOIL</li>
                           <li>DIST</li>
                           </ul>
                           </li>
                           </ul>
                           </li>
                           <li>PARAM SIMUL</li>
                           <li>RESULTS</li>
                           </ul>
                           "
             )),
             br(),
             br(),
             fluidRow(
               column(12
                      , shinyjs::hidden(
                        actionButton(inputId = "create.simul"
                                     , label = HTML("Create Simulation <br/>parameters file")
                                     , icon = icon("file")
                                     , width = "100%"
                                     , style = HTML(button.style)
                        )
                      )
                      , br()
                      , br()
                      , shinyjs::hidden(
                        downloadButton(outputId = "FATE_simulation.zip"
                                       , label = "Download folder"
                                       , icon = icon("download")
                                       , width = "100%"
                                       , style = HTML(button.style)
                        )
                      )
                      , br()
                      , br()
                      , shinyjs::hidden(
                        actionButton(inputId = "refresh"
                                     , label = "Start new folder"
                                     , icon = icon("refresh")
                                     , width = "100%"
                                     , style = HTML(button.style)
                        )
                      )
               )
             )
             ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 9,
             shinyjs::hidden(
               wellPanel(id = "main.panel",
                         style = "border-solid:solid; border-width:0px; border-color:#068f96;",
                         tabsetPanel(
                           source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab1.R", local = TRUE)$value,
                           source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab2.R", local = TRUE)$value,
                           source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab3.R", local = TRUE)$value,
                           source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab4.R", local = TRUE)$value
                         ) ## END tabsetPanel
               ) ## END wellPanel
             ) ## END hidden
           ) ## END mainPanel
             ) ## END sidebarLayout
           ) ## END tabPanel