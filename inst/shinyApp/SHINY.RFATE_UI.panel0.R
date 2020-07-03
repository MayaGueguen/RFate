
tabPanel(title =  HTML("<span class='panel_title'><i class='fa fa-home'></i></span>")
         , value = "panel0"
         , sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 12,
             style = HTML(paste0("border-width:0px; background-color:", help.color, "; margin-left:15px; margin-top:18px;")),
             withMathJax(),
             
             br(),
             fluidRow(
               column(8
                      , br()
                      , div(id = "help0_1"
                            , HTML("
                                   <p><code>FATE</code> is a <strong>spatially and temporally explicit 
                                   vegetation model</strong>. It uses <strong>plant functional groups 
                                   (PFG)</strong> and integrates important mechanisms driving vegetation 
                                   dynamics, structure and diversity, such as <strong>demographic cycle
                                   </strong>, obviously, but also <strong>seeds dispersal</strong>, 
                                   <strong>abiotic filtering</strong> or <strong>biotic interactions</strong> 
                                   (through the competition for resources like light availability or soil 
                                   suitability).</p>
                                   <p>If <strong>primary succession</strong> is the most obvious ecological 
                                   process that can be modelled with <code>FATE</code>, events related to 
                                   <strong>secondary succession</strong> can be represented as well using 
                                   the various <code>FATE</code> <strong>add-on modules</strong> : 
                                   disturbances (mowing, grazing, fire..), drought event, invasive species.</p>
                                   <p>As vegetation modelling can be challenging (data gathering, 
                                   parameterization, handling results…), <code>RFate</code> provides 
                                   <strong>user-friendly functions</strong> to go through the <strong>whole</strong> 
                                   <code>FATE</code> <strong>workflow</strong>.
                                   ")
                      )
                      , HTML("<hr><br/>")
               )
               , column(1, br())
               , column(3
                        , br()
                        , div(id = "help0_6"
                              , actionButton(inputId = "HELP.panel0"
                                             , label = "Need some help"
                                             , icon = icon("question-circle", class = "icon-help")
                                             , width = "100%"
                                             , style = HTML(button.style.help))
                        )
               )
             )
             , fluidRow(
               column(8
                      , br()
                      , div(id = "help0_2"
                            , HTML("
                                   <p><i class='fa fa-object-group' style='font-size:30px;'></i>&emsp;&emsp;
                                   <strong>STEP 1 : Creation of Plant Functional Groups</strong><br/></p>
                                   <p style='padding:0 0 0 40px;'>A plant functional group, or <strong>PFG</strong>, 
                                   is “<em>A set of representative species is classified based on key biological 
                                   characteristics, to determine groups of species sharing ecological 
                                   strategies</em>” 
                                   (<a href='http://j.boulangeat.free.fr/pdfs/Boulangeat2012_GCB_published.pdf' 
                                   title='Boulangeat, I., Philippe, P., Abdulhak, S., Douzet, R., Garraud, 
                                   L., Lavergne, S., Lavorel, S., Van Es J., Vittoz, P. and Thuiller, W. 
                                   Improving plant functional groups for dynamic models of biodiversity: 
                                   at the crossroad between functional and community ecology. 
                                   Global Change Biology, 18, 3464-3475.'>Boulangeat, 2012</a>). 
                                   PFGs are based on their distribution, physiological characteristics, 
                                   competition traits...</p>
                                   ")
                      )
                      # , HTML("<hr><br/>")
               )
               , column(1, br())
               , column(3
                        , br()
                        , div(id = "help0_7"
                              , actionButton(inputId = "web.RFate"
                                             , label = "Go to RFate website"
                                             , icon = icon("arrow-circle-right", class = "icon-help")
                                             , width = "100%"
                                             , style = HTML(button.style.help)
                                             , onclick ="window.open('https://mayagueguen.github.io/RFate/', '_blank')")
                        )
                        , br()
                        , br()
                        , downloadButton(outputId = "report"
                                         , label = "Generate report"
                                         , width = "100%"
                                         , style = HTML(button.style.help))
               )
             )
             , fluidRow(
               column(8
                      , div(id = "help0_3"
                            , HTML("
                                   <p><i class='fa fa-copy' style='font-size:30px;'></i>&emsp;&emsp;
                                   <strong>STEP 2 : Creation of simulation folder</strong><br/></p>
                                   <p style='padding:0 0 0 40px;'><code>FATE</code> requires a quite large number of 
                                   parameters, which are stored into <code>.txt</code> files, presented to and 
                                   recovered by the software. <br/>These <strong>parameters</strong> can be of 3 types :</p>
                                   <ol>
                                   <li>
                                   <strong>Filenames</strong>, to guide the application to other parameter files that should be read</li>
                                   <li>These filenames either correspond to :
                                   <ul>
                                   <li>other parameter files that contain <strong>values</strong> to be actually read and used</li>
                                   <li>
                                   <strong>raster</strong> files, with the extension <code>.tif</code> (lighter) or <code>.img</code>
                                   </li>
                                   </ul>
                                   </li>
                                   </ol>
                                   ")
                      )
                      , br()
                      , div(id = "help0_4"
                            , HTML("
                                   <p><i class='fa fa-cogs' style='font-size:30px;'></i>&emsp;&emsp;
                                   <strong>STEP 3 : Run a simulation</strong><br/></p>
                                   <p style='padding:0 0 0 40px;'>Give a simulation folder and a simulation parameters file, 
                                   and run your <code>FATE</code> simulation.
                                   ")
                      )
                      , br()
                      , div(id = "help0_5"
                            , HTML("
                                   <p><i class='fa fa-chart-bar' style='font-size:30px;'></i>&emsp;&emsp;
                                   <strong>STEP 4 : Creation of simulation outputs & graphics</strong><br/></p>
                                   <p style='padding:0 0 0 40px;'>Once a <code>FATE</code> simulation is done, 
                                   some post treatment panels and functions are available :</p>
                                   <ul>
                                   <li><strong>BROWSER :</strong> to visualize existing graphics</li>
                                   <li><strong>Through time :</strong> create tables and graphics for temporal evolution of PFG abundances ...</li>
                                   <li><strong>Specific year :</strong> create tables and spatial maps of basic patterns (richness, diversities ...) 
                                   for specific years</li>
                                   </ul>
                                   ")
                      )
               )
               , column(1, br())
               , column(3
                        , HTML("<br/><br/><br/><br/><br/>")
                        , HTML("<img src='https://mayagueguen.github.io/pictures/logo-leca.png' width='100%' />")
               )
             ) ## END fluidRow
           ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 12,
             wellPanel(id = "main.panel",
                       style = "border-solid:solid; border-width:0px;",
                       br()
             ) ## END wellPanel
           ) ## END mainPanel
         ) ## END sidebarLayout
) ## tabPanel
