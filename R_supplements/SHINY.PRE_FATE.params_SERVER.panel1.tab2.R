
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

####################################################################

observeEvent(input$compute.distance, {
  
  if (is.data.frame(input$species.traits))
  {
    if (extension(input$species.traits$name) %in% c(".txt", ".csv"))
    {
      sp.traits = fread(input$species.traits$datapath)
    } else
    {
      shinyalert(type = "warning", text = "You must provide a text file for the species.traits !")
    }
    
    if (is.data.frame(input$species.niche.distance))
    {
      if (extension(input$species.niche.distance$name) %in% c(".txt", ".csv"))
      {
        sp.niche = fread(input$species.niche.distance$datapath)
      } else if (extension(input$species.niche.distance$name) == ".RData")
      {
        sp.niche = get(load(input$species.niche.distance$datapath))
      } else
      {
        shinyalert(type = "warning", text = "You must provide a text or a RData file for the species.niche.distance !")
      }
      
      print(dim(sp.traits))
      print(head(sp.traits))
      print(dim(sp.niche))
      print(head(sp.niche))
      
      
      get_res = print_messages(as.expression(
        PRE_FATE.speciesDistance(mat.species.traits = sp.traits
                                 , mat.species.overlap = sp.niche
                                 , opt.max.percent.NA = input$traits.threshold
                                 , opt.max.percent.similarSpecies = 0.25
                                 , opt.min.sd = 0.3
        )
      ))
    } else
    {
      shinyalert(type = "warning", text = "You must provide a text or a RData file for the species.niche.distance !")
    }
  } else
  {
    shinyalert(type = "warning", text = "You must provide a text file for the species.traits !")
  }
})
