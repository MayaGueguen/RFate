
tabPanel(title = HTML("<p class='tabPanel_title'>Raster files</p>")
         , value = "create.spatial"
         , br()
         , tabsetPanel(
           tabPanel(title = HTML("<p class='tabPanel_subtitle'>Initial</p>")
                    , value = "panel.spatial.init"
                    , br()
                    , wellPanel(
                      style = HTML(paste0("background-color: ", button.color, ";")),
                      helpText(HTML("
                                    <table style='width:100%;'>
                                    <tr>
                                    <td style='width:30%;font-family:Monospace;vertical-align:top;'>simul.mask</td>
                                    <td style='width:70%;'>a <span style='font-family:Monospace;'>string</span> that corresponds to
                                    the file name of a raster mask, with either 0 or 1 within each pixel, 1 corresponding to the cells of the studied area in which the succession
                                    module of the FATE-HD simulation will take place</td>
                                    </tr>
                                    <tr>
                                    <td style='width:30%;font-family:Monospace;vertical-align:top;'>habsuit.folder</td>
                                    <td style='width:70%;'><em>(optional) a string that corresponds to the name of the folder
                                    that will be created into the <span style='font-family:Monospace;'>name.simulation/DATA/SAVE/</span> directory to store the habitat suitability maps</td>
                                    </tr>
                                    <tr>
                                    <td style='width:30%;font-family:Monospace;vertical-align:top;'>habsuit.mask</td>
                                    <td style='width:70%;'>one or several <span style='font-family:Monospace;'>string</span> corresponding to
                                    the file name(s) of a raster mask, with values from 0 to 1 within each pixel, corresponding to the probability to find a specific PFG within this pixel</td>
                                    </tr>
                                    <tr>
                                    <td style='width:30%;font-family:Monospace;vertical-align:top;'>dist.mask</td>
                                    <td style='width:70%;'>one or several <span style='font-family:Monospace;'>string</span> corresponding to
                                    the file name of a raster mask, with either 0 or 1 within each pixel, 1 corresponding to the cells of the studied area in which the disturbance(s)
                                    will take place</td>
                                    </tr>
                                    </table>
                                    "
                      )))
                    , fluidRow(
                      column(4
                             , br()
                             , wellPanel(
                               HTML("<strong>Simulation mask</strong>")
                               , br()
                               , br()
                               , fileInput(inputId = "simul.mask"
                                           , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>name.mask</span>")
                                           , multiple = FALSE
                                           , width = "100%"
                               )
                               , br()
                               , br()
                               , actionButton(inputId = "upload.mask"
                                              , label = HTML("Upload <br/>simulation mask")
                                              , icon = icon("upload")
                                              , width = "100%"
                                              , style = HTML(paste0("background-color: ", button.color, ";"))
                               )
                             )
                      )
                      , column(4
                               , br()
                               , wellPanel(
                                 HTML("<strong>Habitat suitability masks</strong>")
                                 , br()
                                 , br()
                                 , textInput(inputId = "habsuit.folder"
                                             , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>habsuit.folder</span>")
                                             , width = "100%"
                                 )
                                 , fileInput(inputId = "habsuit.mask"
                                             , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>habsuit.mask</span>")
                                             , multiple = TRUE
                                             , width = "100%"
                                 )
                                 , br()
                                 , br()
                                 , actionButton(inputId = "upload.habsuit.mask"
                                                , label = HTML("Upload habitat <br/>suitability masks")
                                                , icon = icon("upload")
                                                , width = "100%"
                                                , style = HTML(paste0("background-color: ", button.color, ";"))
                                 )
                               )
                      )
                      , column(4
                               , br()
                               , wellPanel(
                                 HTML("<strong>Disturbances masks</strong>")
                                 , br()
                                 , br()
                                 , fileInput(inputId = "dist.mask"
                                             , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>dist.mask</span>")
                                             , multiple = TRUE
                                             , width = "100%"
                                 )
                                 , br()
                                 , br()
                                 , actionButton(inputId = "upload.dist.mask"
                                                , label = HTML("Upload <br/>disturbance masks")
                                                , icon = icon("upload")
                                                , width = "100%"
                                                , style = HTML(paste0("background-color: ", button.color, ";"))
                                 )
                               )
                      )
                    )
                      ) ## END tabPanel (Initial)
           , tabPanel(title = HTML("<p class='tabPanel_subtitle'>Changing</p>")
                      , value = "panel.spatial.changing"
                      , br()
                      , wellPanel(
                        style = HTML(paste0("background-color: ", button.color, ";")),
                        helpText(HTML("
                                      <table style='width:100%;'>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>opt.folder.name</td>
                                      <td style='width:70%;'><em>(optional) a string that corresponds to the name of the folder 
                                      that will be created into the <span style='font-family:Monospace;'>name.simulation/DATA/SCENARIO/</span> directory to store the results</em></td>
                                      </tr>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>type.changing</td>
                                      <td style='width:70%;'>a <span style='font-family:Monospace;'>string</span> to choose the concerned module :
                                      succession (MASK), habitat suitability (HS) or disturbances (DIST)</td>
                                      </tr>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>year.changing</td>
                                      <td style='width:70%;'>the simulation year at which the file will be changed</td>
                                      </tr>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>order.changing</td>
                                      <td style='width:70%;'>if several files given for a same year, to keep the order</td>
                                      </tr>
                                      <tr>
                                      <td style='width:30%;font-family:Monospace;vertical-align:top;'>file.changing</td>
                                      <td style='width:70%;'>the file name of the new file</td>
                                      </tr>
                                      </table>
                                      "
                        )))
                      , fluidRow(
                        column(6
                               , br()
                               , actionButton(inputId = "add.changing"
                                              , label = "Add changing year"
                                              , icon = icon("plus")
                                              , width = "100%"
                                              , style = HTML(paste0("background-color: ", button.color, ";"))
                               )
                        )
                        , column(6
                                 , br()
                                 , actionButton(inputId = "create.changing"
                                                , label = "Create Scenario files"
                                                , icon = icon("file")
                                                , width = "100%"
                                                , style = HTML(paste0("background-color: ", button.color, ";"))
                                 )
                        )
                      )
                      , fluidRow(
                        column(2
                               , br()
                               , br()
                               , HTML("<em>opt.folder.name</em>")
                               , textInput(inputId = "changing.folder"
                                           , label = NULL
                                           , width = "100%"))
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>Type</strong>")
                                 , selectInput(inputId = "type.changing"
                                               , label = NULL
                                               , choices = c("MASK", "HS", "DIST")
                                               , selected = "MASK"
                                               , multiple = FALSE
                                               , width = "100%"))
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>Year</strong>")
                                 , numericInput(inputId = "changing.year"
                                                , label = NULL
                                                , value = 1
                                                , min = 1
                                                , width = "100%"))
                        , column(2
                                 , br()
                                 , br()
                                 , HTML("<strong>Order</strong>")
                                 , numericInput(inputId = "changing.order"
                                                , label = NULL
                                                , value = 1
                                                , min = 1
                                                , width = "100%"))
                        , column(4
                                 , br()
                                 , br()
                                 , HTML("<strong>File</strong>")
                                 , textInput(inputId = "changing.file"
                                             , label = NULL
                                             , width = "100%"))
                        # , fileInput(inputId = "changing.file"
                        #             , label = NULL
                        #             , multiple = FALSE
                        #             , width = "100%"))
                      )
                      , fluidRow(
                        column(10
                               , br()
                               , tableOutput(outputId = "mat.changing"))
                        , column(2
                                 , br()
                                 , actionButton(inputId = "delete.changing"
                                                , label = NULL
                                                , icon = icon("trash")
                                                , style = HTML(paste0("background-color: ", button.color, ";"))
                                 )
                        )
                      )
                      , fluidRow(
                        br(),
                        wellPanel(dataTableOutput(outputId = "created_table.changing"))
                      )
                        ) ## END tabPanel (Changing)
                      ) ## END tabsetPanel
) ## END tabPanel (Raster files)
