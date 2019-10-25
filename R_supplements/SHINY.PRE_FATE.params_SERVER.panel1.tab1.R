
####################################################################

output$UI.doHabitatSelection = renderUI({
  if (input$doHabitatSelection)
  {
    tagList(
      sliderInput(inputId = "selectionRule.min_percent_habitat"
                  , label = param.style("selectionRule.min_percent_habitat")
                  , min = 0
                  , max = 1
                  , value = 0.05
                  , step = 0.05
                  , width = "100%")
      , numericInput(inputId = "selectionRule.min_no_habitat"
                     , label = param.style("selectionRule.min_no_habitat")
                     , min = 0
                     , value = 0.05
                     , step = 1
                     , width = "100%")
    )
  }
})
