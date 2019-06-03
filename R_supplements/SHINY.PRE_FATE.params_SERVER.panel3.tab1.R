
####################################################################

get_names.files = eventReactive(input$folder.simul, {
  names.files = list.files(path = paste0(get_path.simul(), "/RESULTS")
                           , pattern = ".pdf$"
                           , all.files = FALSE
                           , full.names = TRUE)
  names.files = basename(names.files)
  return(names.files)
})

update_graph.files = function()
{
  names.files = get_names.files()
  if (length(names.files) > 0)
  {
    ind.toSuppr = get_browser.abundance()
    ind.toSuppr = c(ind.toSuppr, get_browser.validation())
    ind.toSuppr = c(ind.toSuppr, get_browser.richness())
    ind.toSuppr = c(ind.toSuppr, get_browser.cover())
    ind.toSuppr = c(ind.toSuppr, get_browser.light())
    ind.toSuppr = c(ind.toSuppr, get_browser.soil())
    if (length(ind.toSuppr) > 0)
    {
      names.files = names.files[-ind.toSuppr]
    }
    
    updateSelectInput(session
                      , inputId = "graph.files"
                      , choices = names.files
                      , selected = names.files[1])
    shinyjs::enable("graph.files")
  } else
  {
    shinyjs::disable("graph.files")
  }
}

####################################################################

get_browser.abundance = eventReactive(input$browser.abundance, {
  if (!input$browser.abundance)
  {
    return(grep("abundance|spaceOccupancy", get_names.files()))
  } 
})

get_browser.validation = eventReactive(input$browser.validation, {
  if (!input$browser.validation)
  {
    return(grep("validation|PFGvsHS", get_names.files()))
  } 
})

get_browser.richness = eventReactive(input$browser.richness, {
  if (!input$browser.richness)
  {
    return(grep("richness", get_names.files()))
  } 
})

get_browser.cover = eventReactive(input$browser.cover, {
  if (!input$browser.cover)
  {
    return(grep("cover", get_names.files()))
  } 
})

get_browser.light = eventReactive(input$browser.light, {
  if (!input$browser.light)
  {
    return(grep("light", get_names.files()))
  } 
})

get_browser.soil = eventReactive(input$browser.soil, {
  if (!input$browser.soil)
  {
    return(grep("soil", get_names.files()))
  } 
})


####################################################################

observeEvent(input$browser.abundance, { update_graph.files() })
observeEvent(input$browser.validation, { update_graph.files() })
observeEvent(input$browser.richness, { update_graph.files() })
observeEvent(input$browser.cover, { update_graph.files() })
observeEvent(input$browser.light, { update_graph.files() })
observeEvent(input$browser.soil, { update_graph.files() })


