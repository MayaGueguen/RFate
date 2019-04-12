
tabPanel(title = HTML("<p class='tabPanel_title'>Scenario files</p>")
         , value = "create.scenario"
         , br()
         , wellPanel(
           style = HTML(paste0("background-color: ", help.color, ";")),
           helpText(HTML("
                         <p><a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_saveYears.html' target='_blank'>
                         See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
                         <table style='width:100%;'>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>years.maps</td>
                         <td style='width:70%;'>a <span style='font-family:Monospace;'>vector</span> of simulation years at which PFG abundance maps will be saved</td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>years.objects</td>
                         <td style='width:70%;'>a <span style='font-family:Monospace;'>vector</span> of simulation years at which FATE-HD simulation state will be saved</td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>opt.folder.name</td>
                         <td style='width:70%;'><em>(optional) a <span style='font-family:Monospace;'>string</span> that corresponds to the name of the folder that will 
                         be created into the <span style='font-family:Monospace;'>name.simulation/DATA/SAVE/</span> directory to store the results</em></td>
                         </tr>
                         </table>
                         "
           )))
         , fluidRow(
           column(6
                  , br()
                  , wellPanel(
                    HTML("<strong>Save maps ?</strong>")
                    , br()
                    , br()
                    , textInput(inputId = "save.maps.folder"
                                , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>opt.folder.name</span>")
                                , value = NULL
                                , width = "100%")
                    , br()
                    , br()
                    , numericInput(inputId = "save.maps.year1"
                                   , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>years.maps.start</span>")
                                   , value = 0
                                   , min = 0
                                   , width = "100%")
                    , numericInput(inputId = "save.maps.year2"
                                   , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>years.maps.end</span>")
                                   , value = 0
                                   , min = 0
                                   , width = "100%")
                    , numericInput(inputId = "save.maps.no"
                                   , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>years.maps.number</span>")
                                   , value = 0
                                   , min = 0
                                   , max = 100
                                   , step = 10
                                   , width = "100%")
                    , br()
                    , br()
                    , actionButton(inputId = "create.save.maps"
                                   , label = "Create SAVE maps files"
                                   , icon = icon("file")
                                   , width = "100%"
                                   , style = HTML(paste0("background-color: ", button.color, ";"))
                    )
                  )
           )
           , column(6
                    , br()
                    , wellPanel(
                      HTML("<strong>Save simulation ?</strong>")
                      , br()
                      , br()
                      , textInput(inputId = "save.objects.folder"
                                  , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>opt.folder.name</span>")
                                  , value = NULL
                                  , width = "100%")
                      , br()
                      , br()
                      , numericInput(inputId = "save.objects.year1"
                                     , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>years.objects</span>")
                                     , value = 0
                                     , min = 0
                                     , width = "100%")
                      , numericInput(inputId = "save.objects.year2"
                                     , label = NULL
                                     , value = 0
                                     , min = 0
                                     , width = "100%")
                      , numericInput(inputId = "save.objects.year3"
                                     , label = NULL
                                     , value = 0
                                     , min = 0
                                     , width = "100%")
                      , br()
                      , br()
                      , actionButton(inputId = "create.save.objects"
                                     , label = "Create SAVE objects files"
                                     , icon = icon("file")
                                     , width = "100%"
                                     , style = HTML(paste0("background-color: ", button.color, ";"))
                      )
                    )
           )
         )
         , fluidRow(
           br(),
           wellPanel(dataTableOutput(outputId = "created_table.save"))
         )
) ## END tabPanel (Scenario files)
