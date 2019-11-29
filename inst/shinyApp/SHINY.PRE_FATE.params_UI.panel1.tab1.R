
tabPanel(title = HTML("<span class='tabPanel_title'>1. Dominant species</span>")
         , value = "panel.dominant"
         , fluidRow(
           column(6, br())
           , column(6
                    , br()
                    , checkboxInput(inputId = "doHabitatSelection"
                                    , label = param.style("doHabitatSelection")
                                    , value = FALSE
                                    , width = "100%")
           )
         ) ## END fluidRow
         , fluidRow(
           column(6
                  , br()
                  , sliderInput(inputId = "selectionRule.quanti"
                                , label = param.style("quanti")
                                , min = 0
                                , max = 1
                                , value = 0.8
                                , step = 0.05
                                , width = "100%")
                  , numericInput(inputId = "selectionRule.min_mean_abund"
                                 , label = param.style("min_mean_abund")
                                 , min = 0
                                 , value = 10
                                 , step = 1
                                 , width = "100%")
                  , numericInput(inputId = "selectionRule.min_no_abund_over25"
                                 , label = param.style("min_no_abund_over25")
                                 , min = 0
                                 , value = 10
                                 , step = 1
                                 , width = "100%")
           )
           , column(6
                    , br()
                    , uiOutput(outputId = "UI.doHabitatSelection")
           )
         ) ## END fluidRow
         , fluidRow(
           column(12
                  , br()
                  , actionButton(inputId = "select.dominant"
                                 , label = "Select dominant species"
                                 , icon = icon("list")
                                 , width = "100%"
                                 , style = HTML(button.style)
                  ) %>% helper(type = "inline"
                               , title = "Selection of dominant species"
                               , size = "l"
                               , content = help.HTML("docs/reference/PRE_FATE.selectDominant.html")
                  )
           )
         ) ## END fluidRow
) ## END tabPanel (Dominant species)
