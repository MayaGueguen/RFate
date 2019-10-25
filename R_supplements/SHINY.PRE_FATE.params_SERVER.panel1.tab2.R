
####################################################################

output$UI.selected.species = renderUI({
  if (input$choice.dominant == "from file")
  {
    fileInput(inputId = "selected.species"
              , label = NULL
              , buttonLabel = param.style("selected.species")
              , multiple = FALSE
              , width = "100%")
  }
})
