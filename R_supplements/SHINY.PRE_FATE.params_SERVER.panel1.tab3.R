
####################################################################

output$UI.species.distance = renderUI({
  if (input$choice.distance == "from file")
  {
    fileInput(inputId = "species.distance"
              , label = NULL
              , buttonLabel = param.style("species.distance")
              , multiple = FALSE
              , width = "100%")
  }
})
