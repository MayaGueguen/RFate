
tabPanel(title = HTML("<span class='tabPanel_title'>BROWSER</span>")
         , value = "panel.browser"
         , fluidRow(
           column(2
                  , br()
                  , checkboxInput(inputId = "browser.abundance"
                                  , label = "Abundance"
                                  , value = TRUE
                                  , width = "100%")
           )
           , column(2
                    , br()
                    , checkboxInput(inputId = "browser.validation"
                                    , label = "Validation"
                                    , value = TRUE
                                    , width = "100%")
           )
           , column(2
                    , br()
                    , checkboxInput(inputId = "browser.richness"
                                    , label = "Richness"
                                    , value = TRUE
                                    , width = "100%")
           )
           , column(2
                    , br()
                    , checkboxInput(inputId = "browser.cover"
                                    , label = "Cover"
                                    , value = TRUE
                                    , width = "100%")
           )
           , column(2
                    , br()
                    , checkboxInput(inputId = "browser.light"
                                    , label = "Light"
                                    , value = TRUE
                                    , width = "100%")
           )
           , column(2
                    , br()
                    , checkboxInput(inputId = "browser.soil"
                                    , label = "Soil"
                                    , value = TRUE
                                    , width = "100%")
           )
           
         )
         , fluidRow(
           column(12
                  , br()
                  , shinyjs::disabled(
                    selectInput(inputId = "browser.files"
                                , label = param.style("Select the graphic to display :")
                                , choices = NULL
                                , selected = NULL
                                , multiple = F
                                , width = "100%")
                  )
           )
         )
         , fluidRow(
           column(11
                    , br()
                    , uiOutput(outputId = "UI.plot.browser")
           )
           , column(1
                    , br()
                    , br()
                    , br()
                    , shinyjs::disabled(
                      actionButton(inputId = "go.left"
                                   , label = ""
                                   , icon = icon("arrow-circle-left", class = "icon-help")
                                   , width = "100%"
                                   , style = HTML(button.style.help))
                    )
                    , br()
                    , br()
                    , shinyjs::disabled(
                      actionButton(inputId = "go.right"
                                   , label = ""
                                   , icon = icon("arrow-circle-right", class = "icon-help")
                                   , width = "100%"
                                   , style = HTML(button.style.help))
                    )
           )
         )
) ## END tabPanel (Global parameters)
