
####################################################################

output$show.PFGvsHS = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.PFGvsHS"
                 , label = "PFG vs Habsuit"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

output$show.validationStat = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.validationStat"
                 , label = "Validation statistics"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

output$show.PFGrichness = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.PFGrichness"
                 , label = "PFG richness"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

output$show.PFGcover = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.PFGcover"
                 , label = "PFG cover"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

output$show.PFGlight = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.PFGlight"
                 , label = "Light (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})

output$show.PFGsoil = renderUI({
  shinyjs::disabled(
    actionButton(inputId = "show.PFGsoil"
                 , label = "Soil (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  )
})


####################################################################
## PFGvsHS

observeEvent(input$show.PFGvsHS, {
  shinyjs::show("panel.PFGvsHS")
  shinyjs::hide("panel.validationStat")
  shinyjs::hide("panel.PFGrichness")
  shinyjs::hide("panel.PFGcover")
  shinyjs::hide("panel.PFGlight")
  shinyjs::hide("panel.PFGsoil")
  
  output$show.PFGvsHS = renderUI({
      actionButton(inputId = "show.PFGvsHS"
                   , label = "PFG vs Habsuit"
                   , icon = icon("chart-bar")
                   , width = "100%"
                   , style = HTML(panel.style.hover))
  })
  output$show.validationStat = renderUI({
      actionButton(inputId = "show.validationStat"
                   , label = "Validation statistics"
                   , icon = icon("chart-bar")
                   , width = "100%"
                   , style = HTML(panel.style))
  })
  output$show.PFGrichness = renderUI({
      actionButton(inputId = "show.PFGrichness"
                   , label = "PFG richness"
                   , icon = icon("chart-bar")
                   , width = "100%"
                   , style = HTML(panel.style))
  })
  output$show.PFGcover = renderUI({
      actionButton(inputId = "show.PFGcover"
                   , label = "PFG cover"
                   , icon = icon("chart-bar")
                   , width = "100%"
                   , style = HTML(panel.style))
  })
  output$show.PFGlight = renderUI({
      actionButton(inputId = "show.PFGlight"
                   , label = "Light (MAP)"
                   , icon = icon("chart-bar")
                   , width = "100%"
                   , style = HTML(panel.style))
  })
  output$show.PFGsoil = renderUI({
      actionButton(inputId = "show.PFGsoil"
                   , label = "Soil (MAP)"
                   , icon = icon("chart-bar")
                   , width = "100%"
                   , style = HTML(panel.style))
  })
})

####################################################################
## validationStat

observeEvent(input$show.validationStat, {
  shinyjs::hide("panel.PFGvsHS")
  shinyjs::show("panel.validationStat")
  shinyjs::hide("panel.PFGrichness")
  shinyjs::hide("panel.PFGcover")
  shinyjs::hide("panel.PFGlight")
  shinyjs::hide("panel.PFGsoil")
  
  output$show.PFGvsHS = renderUI({
    actionButton(inputId = "show.PFGvsHS"
                 , label = "PFG vs Habsuit"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.validationStat = renderUI({
    actionButton(inputId = "show.validationStat"
                 , label = "Validation statistics"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style.hover))
  })
  output$show.PFGrichness = renderUI({
    actionButton(inputId = "show.PFGrichness"
                 , label = "PFG richness"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGcover = renderUI({
    actionButton(inputId = "show.PFGcover"
                 , label = "PFG cover"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGlight = renderUI({
    actionButton(inputId = "show.PFGlight"
                 , label = "Light (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGsoil = renderUI({
    actionButton(inputId = "show.PFGsoil"
                 , label = "Soil (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
})


####################################################################
## PFGrichness

observeEvent(input$show.PFGrichness, {
  shinyjs::hide("panel.PFGvsHS")
  shinyjs::hide("panel.validationStat")
  shinyjs::show("panel.PFGrichness")
  shinyjs::hide("panel.PFGcover")
  shinyjs::hide("panel.PFGlight")
  shinyjs::hide("panel.PFGsoil")
  
  output$show.PFGvsHS = renderUI({
    actionButton(inputId = "show.PFGvsHS"
                 , label = "PFG vs Habsuit"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.validationStat = renderUI({
    actionButton(inputId = "show.validationStat"
                 , label = "Validation statistics"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGrichness = renderUI({
    actionButton(inputId = "show.PFGrichness"
                 , label = "PFG richness"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style.hover))
  })
  output$show.PFGcover = renderUI({
    actionButton(inputId = "show.PFGcover"
                 , label = "PFG cover"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGlight = renderUI({
    actionButton(inputId = "show.PFGlight"
                 , label = "Light (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGsoil = renderUI({
    actionButton(inputId = "show.PFGsoil"
                 , label = "Soil (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
})

####################################################################
## PFGcover

observeEvent(input$show.PFGcover, {
  shinyjs::hide("panel.PFGvsHS")
  shinyjs::hide("panel.validationStat")
  shinyjs::hide("panel.PFGrichness")
  shinyjs::show("panel.PFGcover")
  shinyjs::hide("panel.PFGlight")
  shinyjs::hide("panel.PFGsoil")
  
  output$show.PFGvsHS = renderUI({
    actionButton(inputId = "show.PFGvsHS"
                 , label = "PFG vs Habsuit"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.validationStat = renderUI({
    actionButton(inputId = "show.validationStat"
                 , label = "Validation statistics"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGrichness = renderUI({
    actionButton(inputId = "show.PFGrichness"
                 , label = "PFG richness"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGcover = renderUI({
    actionButton(inputId = "show.PFGcover"
                 , label = "PFG cover"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style.hover))
  })
  output$show.PFGlight = renderUI({
    actionButton(inputId = "show.PFGlight"
                 , label = "Light (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGsoil = renderUI({
    actionButton(inputId = "show.PFGsoil"
                 , label = "Soil (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
})

####################################################################
## PFGlight

observeEvent(input$show.PFGlight, {
  shinyjs::hide("panel.PFGvsHS")
  shinyjs::hide("panel.validationStat")
  shinyjs::hide("panel.PFGrichness")
  shinyjs::hide("panel.PFGcover")
  shinyjs::show("panel.PFGlight")
  shinyjs::hide("panel.PFGsoil")
  
  output$show.PFGvsHS = renderUI({
    actionButton(inputId = "show.PFGvsHS"
                 , label = "PFG vs Habsuit"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.validationStat = renderUI({
    actionButton(inputId = "show.validationStat"
                 , label = "Validation statistics"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGrichness = renderUI({
    actionButton(inputId = "show.PFGrichness"
                 , label = "PFG richness"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGcover = renderUI({
    actionButton(inputId = "show.PFGcover"
                 , label = "PFG cover"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGlight = renderUI({
    actionButton(inputId = "show.PFGlight"
                 , label = "Light (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style.hover))
  })
  output$show.PFGsoil = renderUI({
    actionButton(inputId = "show.PFGsoil"
                 , label = "Soil (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
})

####################################################################
## PFGsoil

observeEvent(input$show.PFGsoil, {
  shinyjs::hide("panel.PFGvsHS")
  shinyjs::hide("panel.validationStat")
  shinyjs::hide("panel.PFGrichness")
  shinyjs::hide("panel.PFGcover")
  shinyjs::hide("panel.PFGlight")
  shinyjs::show("panel.PFGsoil")
  
  output$show.PFGvsHS = renderUI({
    actionButton(inputId = "show.PFGvsHS"
                 , label = "PFG vs Habsuit"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.validationStat = renderUI({
    actionButton(inputId = "show.validationStat"
                 , label = "Validation statistics"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGrichness = renderUI({
    actionButton(inputId = "show.PFGrichness"
                 , label = "PFG richness"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGcover = renderUI({
    actionButton(inputId = "show.PFGcover"
                 , label = "PFG cover"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGlight = renderUI({
    actionButton(inputId = "show.PFGlight"
                 , label = "Light (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style))
  })
  output$show.PFGsoil = renderUI({
    actionButton(inputId = "show.PFGsoil"
                 , label = "Soil (MAP)"
                 , icon = icon("chart-bar")
                 , width = "100%"
                 , style = HTML(panel.style.hover))
  })
})
