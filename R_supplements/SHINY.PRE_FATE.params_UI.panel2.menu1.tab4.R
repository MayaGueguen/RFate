
tabPanel(title = HTML("<span class='tabPanel_title'>Raster files</span>")
         , value = "panel.raster"
         , sidebarLayout(
           sidebarPanel = NULL,
           mainPanel = mainPanel(
             width = 12,
             br(),
             tabsetPanel(
               tabPanel(title = HTML("<span class='tabPanel_subtitle'>Initial</span>")
                        , value = "panel.spatial.init"
                        , fluidRow(
                          column(4
                                 , br()
                                 , wellPanel(
                                   HTML("<strong>Simulation mask</strong>")
                                   , br()
                                   , br()
                                   , fileInput(inputId = "simul.mask"
                                               , label = param.style("name.mask")
                                               , multiple = FALSE
                                               , width = "100%"
                                   )
                                   , br()
                                   , br()
                                   , actionButton(inputId = "upload.mask"
                                                  , label = HTML("Upload <br/>simulation mask")
                                                  , icon = icon("upload")
                                                  , width = "100%"
                                                  , style = HTML(button.style)
                                   )
                                 ) %>% helper(type = "inline"
                                              , title = "Simulation mask"
                                              , size = "l"
                                              , content = help.full(param.name.vec = c("<hr/>"
                                                                                       , "simul.mask")
                                                                    , param.desc.vec = c("<hr/>"
                                                                                         , "a <span style='font-family:Monospace;'>string</span> that corresponds to
                                                                                         the file name of a raster mask, with either 0 or 1 within each pixel, 1 corresponding to the cells of the studied area in which the succession
                                                                                         module of the FATE-HD simulation will take place")
                                                                    ))
                                                                    )
                          , column(4
                                   , br()
                                   , wellPanel(
                                     HTML("<strong>Habitat suitability masks</strong>")
                                     , br()
                                     , br()
                                     , textInput(inputId = "habsuit.folder"
                                                 , label = param.style("habsuit.folder")
                                                 , width = "100%"
                                     )
                                     , fileInput(inputId = "habsuit.mask"
                                                 , label = param.style("habsuit.mask")
                                                 , multiple = TRUE
                                                 , width = "100%"
                                     )
                                     , br()
                                     , br()
                                     , actionButton(inputId = "upload.habsuit.mask"
                                                    , label = HTML("Upload habitat <br/>suitability masks")
                                                    , icon = icon("upload")
                                                    , width = "100%"
                                                    , style = HTML(button.style)
                                     )
                                   ) %>% helper(type = "inline"
                                                , title = "Habitat suitability masks"
                                                , size = "l"
                                                , content = help.full(param.name.vec = c("<hr/>"
                                                                                         , "habsuit.folder"
                                                                                         , "habsuit.mask")
                                                                      , param.desc.vec = c("<hr/>"
                                                                                           , "<em>(optional) a string that corresponds to the name of the folder
                                                                                           that will be created into the <span style='font-family:Monospace;'>name.simulation/DATA/PFGS/HABSUIT/</span> directory to store the habitat suitability maps"
                                                                                           , "one or several <span style='font-family:Monospace;'>string</span> corresponding to
                                                                                           the file name(s) of a raster mask, with values from 0 to 1 within each pixel, corresponding to the probability to find a specific PFG within this pixel")
                                                                      ))
                                                )
                          , column(4
                                   , br()
                                   , wellPanel(
                                     HTML("<strong>Disturbances masks</strong>")
                                     , br()
                                     , br()
                                     , fileInput(inputId = "dist.mask"
                                                 , label = param.style("dist.mask")
                                                 , multiple = TRUE
                                                 , width = "100%"
                                     )
                                     , br()
                                     , br()
                                     , actionButton(inputId = "upload.dist.mask"
                                                    , label = HTML("Upload <br/>disturbance masks")
                                                    , icon = icon("upload")
                                                    , width = "100%"
                                                    , style = HTML(button.style)
                                     )
                                   ) %>% helper(type = "inline"
                                                , title = "Disturbance masks"
                                                , size = "l"
                                                , content = help.full(param.name.vec = c("<hr/>"
                                                                                         , "dist.mask")
                                                                      , param.desc.vec = c("<hr/>"
                                                                                           , "one or several <span style='font-family:Monospace;'>string</span> corresponding to
                                                                                           the file name of a raster mask, with either 0 or 1 within each pixel, 1 corresponding to
                                                                                           the cells of the studied area in which the disturbance(s) will take place")
                                                                      ))
                                                                      )
                                                )
                          ) ## END tabPanel (Initial)
               , tabPanel(title = HTML("<span class='tabPanel_subtitle'>Changing</span>")
                          , value = "panel.spatial.changing"
                          , fluidRow(
                            column(6
                                   , br()
                                   , shinyjs::disabled(
                                     actionButton(inputId = "add.changing"
                                                  , label = "Add changing year"
                                                  , icon = icon("plus")
                                                  , width = "100%"
                                                  , style = HTML(button.style)
                                     )
                                   )
                            )
                            , column(6
                                     , br()
                                     , shinyjs::disabled(
                                       actionButton(inputId = "create.changing"
                                                    , label = "Create Scenario files"
                                                    , icon = icon("file")
                                                    , width = "100%"
                                                    , style = HTML(button.style)
                                       ) %>% helper(type = "inline"
                                                    , title = "Create Scenario files"
                                                    , size = "l"
                                                    , content = help.HTML("docs/reference/PRE_FATE.params_changingYears.html")
                                       )
                                     )
                            )
                          )
                          , fluidRow(
                            column(4
                                   , br()
                                   , br()
                                   , HTML("<em>opt.folder.name</em>")
                                   , textInput(inputId = "changing.folder"
                                               , label = NULL
                                               , width = "100%"))
                            , column(4
                                     , br()
                                     , br()
                                     , HTML("<strong>Type</strong>")
                                     , selectInput(inputId = "type.changing"
                                                   , label = NULL
                                                   , choices = c("MASK", "HS", "DIST")
                                                   , selected = "MASK"
                                                   , multiple = FALSE
                                                   , width = "100%"))
                            , column(4
                                     , br()
                                     , br()
                                     , br()
                                     , actionButton(inputId = "refresh.changing"
                                                    , label = "Get available files"
                                                    , icon = icon("refresh")
                                                    , width = "100%"
                                                    , style = HTML(button.style)
                                     ))
                          )
                          , fluidRow(
                            column(2
                                   , br()
                                   , HTML("<strong>Year</strong>")
                                   , numericInput(inputId = "changing.year"
                                                  , label = NULL
                                                  , value = 1
                                                  , min = 1
                                                  , width = "100%"))
                            , column(2
                                     , br()
                                     , HTML("<strong>Order</strong>")
                                     , numericInput(inputId = "changing.order"
                                                    , label = NULL
                                                    , value = 1
                                                    , min = 1
                                                    , width = "100%"))
                            , column(8
                                     , br()
                                     , HTML("<strong>File</strong>")
                                     , selectInput(inputId = "changing.file"
                                                   , label = NULL
                                                   , choices = NULL
                                                   , selected = NULL
                                                   , multiple = TRUE
                                                   , width = "100%")
                            )
                          )
                          , fluidRow(
                            column(10
                                   , br()
                                   , br()
                                   , wellPanel(style = "overflow-x:scroll;"
                                               , tableOutput(outputId = "mat.changing")))
                            , column(2
                                     , br()
                                     , actionButton(inputId = "delete.changing"
                                                    , label = NULL
                                                    , icon = icon("trash")
                                                    , style = HTML(button.style)
                                     )
                            )
                          )
                          , fluidRow(
                            br()
                            , br()
                            , br()
                            , br()
                            , column(12
                                     , wellPanel(style = HTML(paste0("border-width:0px; background-color:", help.color, "; margin-left:15px; margin-top:18px; overflow-y:scroll; max-height:250px;"))
                                                 , uiOutput(outputId = "UI.files.changing")))
                            , column(12
                                     , wellPanel(style = HTML(paste0("border-width:0px; background-color:", help.color, "; margin-left:15px; margin-top:18px; overflow-x:scroll;"))
                                                 , dataTableOutput(outputId = "created_table.changing"))
                            )
                          )
               ) ## END tabPanel (Changing)
                                   ) ## END tabsetPanel
                          ) ## END mainPanel
         ) ## END sidebarLayout
         ) ## END tabPanel (Raster files)
