
# introBox(
tabPanel(title = HTML("<p class='panel_title'><i class='fa fa-folder-plus'></i> New</p>"),
         sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 3,
             style = HTML(paste0("border-width:0px; background-color:", help.color, "; margin-left:15px; margin-top:18px; padding:0px;")),
             withMathJax(),
             
             introBox(
               wellPanel(
                 style = HTML(paste0("background-color:transparent; border:transparent; padding:20px;")),
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
                          , introBox(
                            shinyjs::disabled(
                            actionButton(inputId = "create.simul"
                                         , label = HTML("Create Simulation <br/>parameters file")
                                         , icon = icon("file")
                                         , width = "100%"
                                         , style = HTML(button.style)
                            )
                          ),
                          data.step = 4,
                          data.position = "auto",
                          data.intro = "
                            <p><em>3. Parameter management</em></p>
                            <ul>
                            <li>
                            <strong>ParamSimulation file</strong> : containing all links to the files created with the previous functions.<br>
                            This is the file that will be given as the only argument to the <code>FATE-HD</code> executable file into the command line.<br>
                            It can be created with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_simulParameters.html'>PRE_FATE.params_simulParameters</a>
                            </li>
                            </ul>"
                          ) ## END introBox
                          , br()
                          , br()
                          , introBox(
                            shinyjs::disabled(
                              downloadButton(outputId = "FATE_simulation.zip"
                                             , label = "Download folder"
                                             , icon = icon("download")
                                             , width = "100%"
                                             , style = HTML(button.style)
                              )
                            ),
                            data.step = 5,
                            data.position = "auto",
                            data.intro = "
                            <p>Download the complete simulation folder as an archive file (<code>FATE_simulation.zip</code>).</p>"
                          ) ## END introBox
                          , br()
                          , br()
                          , shinyjs::disabled(
                            actionButton(inputId = "refresh"
                                         , label = "Start new folder"
                                         , icon = icon("refresh")
                                         , width = "100%"
                                         , style = HTML(button.style)
                            )
                          )
                   )
                 )
                 ), ## END wellPanel
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
                 ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 9,
             shinyjs::hidden(
               wellPanel(id = "main.panel",
                         style = "border-solid:solid; border-width:0px; border-color:#068f96;",
                         introBox(
                           tabsetPanel(
                             source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab1.R", local = TRUE)$value
                             , source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab2.R", local = TRUE)$value
                             , source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab3.R", local = TRUE)$value
                             , source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.tab4.R", local = TRUE)$value
                           ), ## END tabsetPanel
                           data.step = 3,
                           data.position = "auto",
                           data.intro = "
                           <p><em>1. Simulation parameterization</em></p>
                           <ul>
                           <li>
                           <strong>Global parameters</strong> : related to the simulation definition<br>
                           (number of PFG and strata, simulation duration, computer resources, manage abundance values, modules loaded…)<br>
                           with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_globalParameters.html'>PRE_FATE.params_globalParameters</a>
                           </li>
                           <li>
                           <strong>Years to save abundance rasters and simulation outputs</strong> with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_saveYears.html'>PRE_FATE.params_saveYears</a>
                           </li>
                           <li>
                           <strong>Years and files to change rasters</strong> for the succession, habitat suitability or disturbance modules<br>
                           with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_changingYears.html'>PRE_FATE.params_changingYears</a>
                           </li>
                           </ul>
                           <p><em>2. For each PFG : behavior and characteristics</em></p>
                           <ul>
                           <li>
                           <strong>Succession files</strong> : related to the life history with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGsuccession.html'>PRE_FATE.params_PFGsuccession</a>
                           </li>
                           <li>
                           <strong>Dispersal files</strong> : related to the dispersal ability with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGdispersal.html'>PRE_FATE.params_PFGdispersal</a>
                           </li>
                           <li>
                           <strong>Light files</strong> : related to the light competition with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGlight.html'>PRE_FATE.params_PFGlight</a>
                           </li>
                           <li>
                           <strong>Soil files</strong> : related to the soil competition with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGsoil.html'>PRE_FATE.params_PFGsoil</a>
                           </li>
                           <li>
                           <strong>Disturbance files</strong> : related to the response to perturbations in terms of resprouting and mortality<br>
                           with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGdisturbance.html'>PRE_FATE.params_PFGdisturbance</a>
                           </li>
                           </ul>"
               ) ## END introBox
               ) ## END wellPanel
             ) ## END hidden
           ) ## END mainPanel
                          ) %>% helper(type = "inline"
                                       , title = "Create FATE-HD parameter files"
                                       , size = "l"
                                       , content = help.HTML(html.file = "docs/index.html"
                                                             , target.anchor = '<div id='
                                                             , target.class = c("the-different-type-of-parameters-and-flags"
                                                                                , "which-files-for-which-settings"))
                          ) ## END sidebarLayout
               ) ## END tabPanel
