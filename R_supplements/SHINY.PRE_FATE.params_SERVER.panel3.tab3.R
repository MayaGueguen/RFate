
####################################################################

observeEvent(input$show.specific_year, {
  if (input$show.specific_year == "PFG vs Habsuit")
  {
    shinyjs::show("panel.PFGvsHS")
    shinyjs::hide("panel.validationStat")
    shinyjs::hide("panel.PFGrichness")
    shinyjs::hide("panel.PFGcover")
    shinyjs::hide("panel.PFGlight")
    shinyjs::hide("panel.PFGsoil")
  } else if (input$show.specific_year == "Validation stat")
  {
    shinyjs::hide("panel.PFGvsHS")
    shinyjs::show("panel.validationStat")
    shinyjs::hide("panel.PFGrichness")
    shinyjs::hide("panel.PFGcover")
    shinyjs::hide("panel.PFGlight")
    shinyjs::hide("panel.PFGsoil")
  } else if (input$show.specific_year == "PFG richness")
  {
    shinyjs::hide("panel.PFGvsHS")
    shinyjs::hide("panel.validationStat")
    shinyjs::show("panel.PFGrichness")
    shinyjs::hide("panel.PFGcover")
    shinyjs::hide("panel.PFGlight")
    shinyjs::hide("panel.PFGsoil")
  } else if (input$show.specific_year == "PFG cover")
  {
    shinyjs::hide("panel.PFGvsHS")
    shinyjs::hide("panel.validationStat")
    shinyjs::hide("panel.PFGrichness")
    shinyjs::show("panel.PFGcover")
    shinyjs::hide("panel.PFGlight")
    shinyjs::hide("panel.PFGsoil")
  } else if (input$show.specific_year == "Light CWM (MAP)")
  {
    shinyjs::hide("panel.PFGvsHS")
    shinyjs::hide("panel.validationStat")
    shinyjs::hide("panel.PFGrichness")
    shinyjs::hide("panel.PFGcover")
    shinyjs::show("panel.PFGlight")
    shinyjs::hide("panel.PFGsoil")
  } else if (input$show.specific_year == "Soil CWM (MAP)")
  {
    shinyjs::hide("panel.PFGvsHS")
    shinyjs::hide("panel.validationStat")
    shinyjs::hide("panel.PFGrichness")
    shinyjs::hide("panel.PFGcover")
    shinyjs::hide("panel.PFGlight")
    shinyjs::show("panel.PFGsoil")
  }
})
