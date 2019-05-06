
tabPanel(title = HTML("<span class='tabPanel_title'>Scenario files</span>")
         , value = "create.scenario"
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
                                   , style = HTML(button.style)
                    )
                  ) %>% helper(type = "inline"
                               , title = "Create SAVE maps files"
                               , size = "l"
                               , content = help.full(param.web = "https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_saveYears.html"
                                                     , param.name.vec = c("<hr/>"
                                                                          , "years.maps"
                                                                          , "opt.folder.name")
                                                     , param.desc.vec = c("<hr/>"
                                                                          , "a <span style='font-family:Monospace;'>vector</span> of simulation years at which PFG abundance maps will be saved"
                                                                          , "<em>(optional) a <span style='font-family:Monospace;'>string</span> that corresponds to the name of the folder that will
                                                                          be created into the <span style='font-family:Monospace;'>name.simulation/DATA/SAVE/</span> directory to store the results</em>")
                                                     ))
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
                                     , style = HTML(button.style)
                      )
                    ) %>% helper(type = "inline"
                                 , title = "Create SAVE objects files"
                                 , size = "l"
                                 , content = help.full(param.web = "https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_saveYears.html"
                                                       , param.name.vec = c("<hr/>"
                                                                            , "years.objects"
                                                                            , "opt.folder.name")
                                                       , param.desc.vec = c("<hr/>"
                                                                            , "a <span style='font-family:Monospace;'>vector</span> of simulation years at which FATE-HD simulation state will be saved"
                                                                            , "<em>(optional) a <span style='font-family:Monospace;'>string</span> that corresponds to the name of the folder that will
                                                                          be created into the <span style='font-family:Monospace;'>name.simulation/DATA/SAVE/</span> directory to store the results</em>")
                                 ))
           )
           )
         , fluidRow(
           br(),
           wellPanel(style = "overflow-x:scroll;"
                     , dataTableOutput(outputId = "created_table.save"))
         )
         ) ## END tabPanel (Scenario files)
