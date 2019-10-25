
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
                  , uiOutput(outputId = "UI.selected.species")
                  , br()
                  , dataTableOutput(outputId = "species.traits.table")
                  , br()
                  , uiOutput(outputId = "UI.selectedTraits")
                  , uiOutput(outputId = "UI.selectedGroup")
                  # , selectInput(inputId = "pfg.traits"
                  #               , label = param.style("Traits to be used")
                  #               , choices = NULL
                  #               , selected = NULL
                  #               , multiple = TRUE
                  #               , width = "100%")
                  , br()
                  , sliderInput(inputId = "traits.threshold"
                                , label = param.style("traits.threshold")
                                , min = 0
                                , max = 1
                                , value = 1
                                , step = 0.05
                                , width = "100%")
                  , br()
                  , fileInput(inputId = "species.niche.distance"
                              , label = NULL
                              , buttonLabel = param.style("species.niche.distance")
                              , multiple = FALSE
                              , width = "100%")
           )
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
