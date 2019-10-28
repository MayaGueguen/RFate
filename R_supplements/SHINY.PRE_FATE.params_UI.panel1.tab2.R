
tabPanel(title = HTML("<span class='tabPanel_title'>Pairwise distance</span>")
         , value = "panel.distance"
         , fluidRow(
           column(12
                  , br()
                  , radioButtons(inputId = "choice.dominant"
                                 , label = param.style("Use dominant species...")
                                 , choices = c("from dominant selection"
                                               , "from file")
                                 , selected = "from dominant selection"
                                 , inline = TRUE
                                 , width = "100%")
                  , br()
                  , uiOutput(outputId = "UI.species.selected")
                  , fileInput(inputId = "species.niche.distance"
                              , label = NULL
                              , buttonLabel = param.style("species.niche.distance")
                              , multiple = FALSE
                              , accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv",
                                           ".RData")
                              , width = "100%")
           )
         ) ## END fluidRow
         , fluidRow(
           column(6
                  , sliderInput(inputId = "opt.max.percent.NA"
                                , label = param.style("opt.max.percent.NA")
                                , min = 0
                                , max = 1
                                , value = 0
                                , step = 0.05
                                , width = "100%")
           )
           , column(6
                    , sliderInput(inputId = "opt.max.percent.similarSpecies"
                                  , label = param.style("opt.max.percent.similarSpecies")
                                  , min = 0
                                  , max = 1
                                  , value = 0.25
                                  , step = 0.05
                                  , width = "100%")
                    
           )
         ) ## END fluidRow
         , fluidRow(
           column(6
                  , br()
                  , numericInput(inputId = "opt.min.sd"
                                 , label = param.style("opt.min.sd")
                                 , min = 0
                                 , value = 0.5
                                 , width = "100%")
           )
           , column(6, br())
         ) ## END fluidRow
         , fluidRow(
           column(6, br())
           , column(6
                    , br()
                    , actionButton(inputId = "compute.distance"
                                   , label = "Compute pairwise distance"
                                   , icon = icon("ruler")
                                   , width = "100%"
                                   , style = HTML(button.style)
                    ) %>% helper(type = "inline"
                                 , title = "Computation of species pairwise distance"
                                 , size = "l"
                                 , content = help.HTML("docs/reference/PRE_FATE.speciesDistance.html")
                    )
           )
         ) ## END fluidRow
) ## END tabPanel (Pairwise distance)
