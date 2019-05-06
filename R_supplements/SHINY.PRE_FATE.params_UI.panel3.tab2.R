
tabPanel(title = HTML("<p class='tabPanel_title'>Through time</p>")
         , value = "panel.through_time"
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
         , radioGroupButtons(inputId = "show.through_time"
                             , label = ""
                             , choices = c("Abundance & coverage"
                                           , "Abundance (PIXELS)"
                                           , "Light (PIXELS)"
                                           , "Soil (PIXELS)")
                             , selected = 0
                             , justified = TRUE
                             , status = "panelgraph"
                             , checkIcon = list(yes = icon("ok", lib = "glyphicon")
                                                , no = icon("remove", lib = "glyphicon"))
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
               , column(2
                        , withBusyIndicatorUI(
                          actionButton(inputId = "create.evolutionCoverage"
                                       , label = "Run plot"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = HTML(button.style)
                          ) %>% helper(type = "inline"
                                       , title = "Plot evolution coverage"
                                       , size = "l"
                                       , content = help.full(param.web = "https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_evolutionCoverage.html"
                                                             , param.name.vec = c("<hr/>"
                                                                                  , "no.years"
                                                                                  , "opt.fixedScale"
                                                                                  , "opt.no_CPU"
                                                                                  , "opt.ras_habitat")
                                                             , param.desc.vec = c("<hr/>"
                                                                                  , "an <span style='font-family:Monospace;'>integer</span> corresponding to the number of simulation years that will be used to extract PFG abundance maps"
                                                                                  , "default <span style='font-family:Monospace;'>TRUE</span>. If <span style='font-family:Monospace;'>FALSE</span>, the ordinate scale will be adapted for 
                                                                                  each PFG for the graphical representation of the evolution through time"
                                                                                  , "default 1 <em>(optional). The number of resources that can be used to parallelize the unzip/zip of raster files"
                                                                                  , "default <span style='font-family:Monospace;'>NULL</span> <em>(optional). A <span style='font-family:Monospace;'>string</span> that corresponds to the 
                                                                                  file name of a raster mask, with an <span style='font-family:Monospace;'>integer</span> value within each pixel, corresponding to a specific habitat</em>")
                                                             ))
                                       )
                        )
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
                        ) %>% helper(type = "inline"
                                     , title = "Plot evolution abund (PIXELS)"
                                     , size = "l"
                                     , content = help.full(param.web = "https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_evolutionAbund_pixels.html"
                                                           , param.name.vec = c("<hr/>"
                                                                                , "no.years"
                                                                                , "opt.fixedScale"
                                                                                , "opt.no_CPU"
                                                                                , "opt.cells_ID")
                                                           , param.desc.vec = c("<hr/>"
                                                                                , "an <span style='font-family:Monospace;'>integer</span> corresponding to the number of simulation years that will be used to extract PFG abundance maps"
                                                                                , "default <span style='font-family:Monospace;'>TRUE</span>. If <span style='font-family:Monospace;'>FALSE</span>, the ordinate scale will be adapted for 
                                                                                each PFG for the graphical representation of the evolution through time"
                                                                                , "default 1 <em>(optional). The number of resources that can be used to parallelize the unzip/zip of raster files"
                                                                                , "default <span style='font-family:Monospace;'>NULL</span> <em>(optional). The cells ID of the studied area for which PFG abundances will be extracted.</em>")
                                     ))
               )
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
                        ) %>% helper(type = "inline"
                                     , title = "Plot evolution light (PIXELS)"
                                     , size = "l"
                                     , content = help.full(param.web = "https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_evolutionLight_pixels.html"
                                                           , param.name.vec = c("<hr/>"
                                                                                , "no.years"
                                                                                , "opt.fixedScale"
                                                                                , "opt.no_CPU"
                                                                                , "opt.cells_ID")
                                                           , param.desc.vec = c("<hr/>"
                                                                                , "an <span style='font-family:Monospace;'>integer</span> corresponding to the number of simulation years that will be used to extract PFG abundance maps"
                                                                                , "default <span style='font-family:Monospace;'>TRUE</span>. If <span style='font-family:Monospace;'>FALSE</span>, the ordinate scale will be adapted for 
                                                                                each PFG for the graphical representation of the evolution through time"
                                                                                , "default 1 <em>(optional). The number of resources that can be used to parallelize the unzip/zip of raster files"
                                                                                , "default <span style='font-family:Monospace;'>NULL</span> <em>(optional). The cells ID of the studied area for which PFG abundances will be extracted.</em>")
                                     ))
                        , br()
                        , textOutput(outputId = "output.evolutionLight"))
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
                        ) %>% helper(type = "inline"
                                     , title = "Plot evolution soil (PIXELS)"
                                     , size = "l"
                                     , content = help.full(param.web = "https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_evolutionSoil_pixels.html"
                                                           , param.name.vec = c("<hr/>"
                                                                                , "no.years"
                                                                                , "opt.fixedScale"
                                                                                , "opt.no_CPU"
                                                                                , "opt.cells_ID")
                                                           , param.desc.vec = c("<hr/>"
                                                                                , "an <span style='font-family:Monospace;'>integer</span> corresponding to the number of simulation years that will be used to extract PFG abundance maps"
                                                                                , "default <span style='font-family:Monospace;'>TRUE</span>. If <span style='font-family:Monospace;'>FALSE</span>, the ordinate scale will be adapted for 
                                                                                  each PFG for the graphical representation of the evolution through time"
                                                                                , "default 1 <em>(optional). The number of resources that can be used to parallelize the unzip/zip of raster files"
                                                                                , "default <span style='font-family:Monospace;'>NULL</span> <em>(optional). The cells ID of the studied area for which PFG abundances will be extracted.</em>")
                                     ))
               )
             ))
           )
           ) ## END tabPanel (Global parameters)
