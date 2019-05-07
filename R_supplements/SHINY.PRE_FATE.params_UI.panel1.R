
introBox(
  tabPanel(title = HTML("<p class='panel_title'><i class='fa fa-folder-plus'></i> New</p>"),
           sidebarLayout(
             
             # Inputs
             sidebarPanel(
               width = 3,
               style = HTML(paste0("border-width:0px; background-color:", help.color, "; margin-left:15px; margin-top:18px;")),
               withMathJax(),
               
               fluidRow(
                 column(12
                        , actionButton(inputId = "help0"
                                       , label = "Need some help"
                                       , icon = icon("question-circle", class = "icon-help")
                                       , width = "100%"
                                       , style = HTML(button.style.help))
                        , br()
                        , br()
                        , br()
                        , br()
                        , introBox(
                          textInput(inputId = "name.simul"
                                    , label = param.style("Enter the simulation name :")
                                    , value = "FATE_simulation"
                                    , width = "100%")
                          , actionButton(inputId = "create.skeleton"
                                         , label = "Create folder"
                                         , icon = icon("folder")
                                         , width = "100%"
                                         , style = HTML(button.style))
                          , data.step = 2
                          , data.position = "bottom"
                          , data.intro = "
                          <strong>Step 1</strong><hr>
                          <p><code>PRE_FATE.skeletonDirectory</code> function creates a user-friendly directory tree to run a <code>FATE-HD</code> simulation.
                          <p>The tree structure is detailed below the button.</p>"
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
                             source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab1.R", local = TRUE)$value
                             , source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab2.R", local = TRUE)$value
                             , source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab3.R", local = TRUE)$value
                             , source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab4.R", local = TRUE)$value
                           ) ## END tabsetPanel
                 ) ## END wellPanel
               ) ## END hidden
             ) ## END mainPanel
               ) ## END sidebarLayout
                        ), ## END tabPanel
  data.step = 1,
  data.position = "auto",
  data.intro = "<p><code>FATE-HD</code> requires only one input parameter, which is a file containing
                        the names of parameter files, which may themselves contain parameters or other
                        file names. The point is : the user could give names of files stored everywhere
                        on a machine, and does not have to put them all in one same place.</p>
                          <p>But as this is more practical, this panel proposes a way to organize all
                        those files or parameter files that will or could be used by a <code>FATE-HD</code>
                          simulation.</p>"
               ) ## END introBox