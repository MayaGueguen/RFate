
tabPanel(title = HTML("<p class='tabPanel_title'>Through time...</p>")
         , value = "panel.through_time"
         , br()
         , wellPanel(
           style = HTML(paste0("background-color: ", help.color, ";")),
           helpText(HTML("
                         <p><a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_evolutionCoverage.html' target='_blank'>
                         See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
                         <table style='width:100%;'>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>no.years</td>
                         <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> corresponding to the number of simulation years that will be used to extract PFG abundance maps</td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>opt.fixedScale</td>
                         <td style='width:70%;'>default <span style='font-family:Monospace;'>TRUE</span>. If <span style='font-family:Monospace;'>FALSE</span>, the ordinate scale will be adapted for each PFG for the graphical representation of the evolution through time</td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>opt.no_CPU</td>
                         <td style='width:70%;'>default 1 <em>(optional). The number of resources that can be used to parallelize the unzip/zip of raster files</em></td>
                         </tr>
                         <tr><td><br/></td></tr>
                         
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>opt.ras_habitat</td>
                         <td style='width:70%;'>default <span style='font-family:Monospace;'>NULL</span> <em>(optional). A <span style='font-family:Monospace;'>string</span> that corresponds to the file name of a raster mask, with an <span style='font-family:Monospace;'>integer</span> value within each pixel, corresponding to a specific habitat</em></td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>opt.cells_ID</td>
                         <td style='width:70%;'>default <span style='font-family:Monospace;'>NULL</span> <em>(optional). The cells ID of the studied area for which PFG abundances will be extracted.</em></td>
                         </tr>
                         </table>
                         "
           ))) ## END wellPanel
         , fluidRow(
           column(3
                  , br()
                  , numericInput(inputId = "graph.no.years"
                                 , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>no.years</span>")
                                 , value = 10
                                 , min = 1
                                 , width = "100%")
           )
           , column(3
                  , br()
                  , numericInput(inputId = "graph.opt.no_CPU"
                                 , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>opt.no_CPU</span>")
                                 , value = 1
                                 , min = 1
                                 , width = "100%")
           )
           , column(3
                    , br()
                    , checkboxInput(inputId = "graph.opt.fixedScale"
                                    , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>opt.fixedScale</span>")
                                    , value = TRUE
                                    , width = "100%")
           )
           , column(3
                    , br()
                    , fileInput(inputId = "graph.opt.ras_habitat"
                                , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>opt.ras_habitat</span>")
                                , multiple = FALSE
                                , width = "100%")
           )
         )
         , fluidRow(
           column(3
                  , br()
                  , shinyjs::disabled(
                    actionButton(inputId = "show.evolutionCoverage"
                                 , label = "Abundance & coverage"
                                 , icon = icon("chart-bar")
                                 , width = "100%"
                                 , style = HTML(button.style)
                    )
                  )
           )
           , column(3
                  , br()
                  , shinyjs::disabled(
                    actionButton(inputId = "show.evolutionAbund"
                                 , label = "Abundance (PIXELS) "
                                 , icon = icon("chart-bar")
                                 , width = "100%"
                                 , style = HTML(button.style)
                    )
                  )
           )
           , column(3
                    , br()
                    , shinyjs::disabled(
                      actionButton(inputId = "show.evolutionLight"
                                   , label = "Light (PIXELS) "
                                   , icon = icon("chart-bar")
                                   , width = "100%"
                                   , style = HTML(button.style)
                      )
                    )
           )
           , column(3
                    , br()
                    , shinyjs::disabled(
                      actionButton(inputId = "show.evolutionSoil"
                                   , label = "Soil (PIXELS) "
                                   , icon = icon("chart-bar")
                                   , width = "100%"
                                   , style = HTML(button.style)
                      )
                    )
           )
         )
         , fluidRow(
           br()
           , shinyjs::hidden(
             fluidRow(
               id = "panel.evolutionCoverage"
               , column(8
                        , plotOutput(outputId = "plot.evolutionCoverage1", width = "100%", height = "600px")
                        , plotOutput(outputId = "plot.evolutionCoverage2", width = "100%", height = "600px")
               )
               , column(4
                        , actionButton(inputId = "create.evolutionCoverage"
                                       , label = "Run plot"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = HTML(button.style)
                        ))
             ))
           , shinyjs::hidden(
             fluidRow(
               id = "panel.evolutionAbund"
               , column(8
                        , plotOutput(outputId = "plot.evolutionAbund", width = "100%", height = "600px")
               )
               , column(4
                        , actionButton(inputId = "create.evolutionAbund"
                                       , label = "Run plot"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = HTML(button.style)
                        ))
             ))
           , shinyjs::hidden(
             fluidRow(
               id = "panel.evolutionLight"
               , column(8
                        , plotOutput(outputId = "plot.evolutionLight", width = "100%", height = "600px")
               )
               , column(4
                        , actionButton(inputId = "create.evolutionLight"
                                       , label = "Run plot"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = HTML(button.style)
                        ))
             ))
           , shinyjs::hidden(
             fluidRow(
               id = "panel.evolutionSoil"
               , column(8
                        , plotOutput(outputId = "plot.evolutionSoil", width = "100%", height = "600px")
               )
               , column(4
                        , actionButton(inputId = "create.evolutionSoil"
                                       , label = "Run plot"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = HTML(button.style)
                        ))
             ))
         )
) ## END tabPanel (Global parameters)
