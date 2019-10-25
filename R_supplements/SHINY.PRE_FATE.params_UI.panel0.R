
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
                                   <p><em>“<code>FATE-HD</code> is a dynamic landscape vegetation model that simulates interactions between plant modelling entities 
                                   (e.g. species or plant functional groups), their population dynamics and dispersal, whilst taking into account external drivers such 
                                   as disturbance regimes, and environmental variations. The model is built on past conceptual and technical developments 
                                   (Albert et al., 2008; Midgley et al., 2010) but has been entirely recoded in C++ and revisited.”</em> 
                                   (<a href='papers/Boulangeat_2014_GCB.pdf' title='Boulangeat, I., Georges, D., Thuiller, W., FATE-HD: A spatially and temporally explicit 
                                   integrated model for predicting vegetation structure and diversity at regional scale. Global Change Biology, 20, 2368–2378.'>Boulangeat, 2014</a>)</p>
                                   ")
                      )
                      , HTML("<hr><br/>")
               )
               , column(1, br())
               , column(3
                        , br()
                        , div(id = "help0_4"
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
                                   <p><strong>STEP 1 : Creation of Plant Functional Groups</strong><br/><br/></p>
                                   <div class='row' style='display:flex;'>
                                   <div class='column' style='flex:5%;'></div>
                                   <div class='column' style='flex:10%;'>
                                   <p><i class='fa fa-object-group' style='font-size:30px;'></i></p>
                                   </div>
                                   <div class='column' style='flex:85%;'>
                                   <p>“<em>The recurring suggestions are that models should explicitly (i) include spatiotemporal dynamics; (ii) consider multiple species 
                                   in interactions and (iii) account for the processes shaping biodiversity distribution.</em>”</p>
                                   </div>
                                   </div>
                                   <br/>
                                   <p><code>FATE-HD</code> is a “<em>a biodiversity model that meets this challenge at regional scale by combining phenomenological and 
                                   process-based approaches and using well-defined</em> <strong><em>plant</em> <em>functional</em> <em>group</em></strong>”. 
                                   (<a href='http://www.will.chez-alice.fr/pdf/BoulangeatGCB2014.pdf' title='Boulangeat, I., Georges, D., Thuiller, W., FATE-HD: A spatially 
                                   and temporally explicit integrated model for predicting vegetation structure and diversity at regional scale. Global Change Biology, 20, 2368–2378.'>
                                   Boulangeat, 2014</a>)</p>
                                   <br/>
                                   <p>A plant functional group, or <strong>PFG</strong>, is “<em>A set of representative species is classified based on key biological 
                                   characteristics, to determine groups of species sharing ecological strategies</em>” 
                                   (<a href='http://j.boulangeat.free.fr/pdfs/Boulangeat2012_GCB_published.pdf' 
                                   title='Boulangeat, I., Philippe, P., Abdulhak, S., Douzet, R., Garraud, L., Lavergne, S., Lavorel, S., Van Es J., Vittoz, P. and Thuiller, W. 
                                   Improving plant functional groups for dynamic models of biodiversity: at the crossroad between functional and community ecology. 
                                   Global Change Biology, 18, 3464-3475.'>Boulangeat, 2012</a>). PFGs are based on their distribution, physiological characteristics, competition traits...</p>
                                   ")
                      )
                      , HTML("<hr><br/>")
               )
               , column(1, br())
               , column(3
                        , br()
                        , div(id = "help0_5"
                              , actionButton(inputId = "web.FATE"
                                             , label = "Go to FATE website"
                                             , icon = icon("arrow-circle-right", class = "icon-help")
                                             , width = "100%"
                                             , style = HTML(button.style.help)
                                             , onclick ="window.open('https://mayagueguen.github.io/FATE-WEBSITE/', '_blank')")
                              , br()
                              , br()
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
                      , br()
                      , div(id = "help0_3"
                            , HTML("
                                   <p><strong>STEP 2 : Creation of simulation folder</strong><br/><br/></p>
                                   <div class='row' style='display:flex;'>
                                   <div class='column' style='flex:5%;'></div>
                                   <div class='column' style='flex:10%;'>
                                   <p><i class='fa fa-copy' style='font-size:30px;'></i></p>
                                   </div>
                                   <div class='column' style='flex:85%;'>
                                   <p><code>FATE-HD</code> requires a quite large number of parameters, which are stored into <code>.txt</code> files, presented to and 
                                   recovered by the software. These <strong>parameters</strong> can be of 3 types :</p>
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
                                   </div>
                                   </div>
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
