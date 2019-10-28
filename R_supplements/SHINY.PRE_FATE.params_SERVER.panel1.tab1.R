
####################################################################

output$UI.doHabitatSelection = renderUI({
  if (input$doHabitatSelection)
  {
    tagList(
      sliderInput(inputId = "selectionRule.min_percent_habitat"
                  , label = param.style("min_percent_habitat")
                  , min = 0
                  , max = 1
                  , value = 0.05
                  , step = 0.05
                  , width = "100%")
      , numericInput(inputId = "selectionRule.min_no_habitat"
                     , label = param.style("min_no_habitat")
                     , min = 0
                     , value = 0.05
                     , step = 1
                     , width = "100%")
    )
  }
})

####################################################################

get_obs = eventReactive(list(input$species.observations, input$select.dominant), {
  if (is.data.frame(input$species.observations))
  {
    if (extension(input$species.observations$name) %in% c(".txt", ".csv"))
    {
      sp.obs = fread(input$species.observations$datapath)
      return(sp.obs)
    } else
    {
      shinyalert(type = "warning", text = "You must provide a text file for the species.observations !")
      return(NULL)
    }
  } else
  {
    shinyalert(type = "warning", text = "You must provide a text file for the species.observations !")
    return(NULL)
  }
})

####################################################################

output$table.observations = renderDataTable({
  sp.obs = get_obs()
  if (!is.null(sp.obs))
  {
    print(head(sp.obs))
    return(sp.obs)
  }
})

####################################################################

observeEvent(input$select.dominant, {
  
  sp.obs = get_obs()
  if (!is.null(sp.obs))
  {
    selectionRule.min_percent_habitat = NULL
    if (!is.null(input$selectionRule.min_percent_habitat)){
      selectionRule.min_percent_habitat = as.numeric(input$selectionRule.min_percent_habitat)
    }
    selectionRule.min_no_habitat = NULL
    if (!is.null(input$selectionRule.min_no_habitat)){
      selectionRule.min_no_habitat = as.numeric(input$selectionRule.min_no_habitat)
    }
    
    get_res = print_messages(as.expression(
      PRE_FATE.selectDominant(mat.site.species.abund = sp.occ
                              , selectionRule.quanti = as.numeric(input$selectionRule.quanti)
                              , selectionRule.min_mean_abund = as.numeric(input$selectionRule.min_mean_abund)
                              , selectionRule.min_no_abund_over25 = as.numeric(input$selectionRule.min_no_abund_over25)
                              , doHabitatSelection = input$doHabitatSelection
                              , selectionRule.min_percent_habitat = selectionRule.min_percent_habitat
                              , selectionRule.min_no_habitat = selectionRule.min_no_habitat
      )
    ))
  } else
  {
    shinyalert(type = "warning", text = "You must provide a text file for the species.observations !")
  }
})
