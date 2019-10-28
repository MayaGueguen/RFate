
####################################################################

output$UI.species.selected = renderUI({
  if (input$choice.dominant == "from file")
  {
    fileInput(inputId = "species.selected"
              , label = NULL
              , buttonLabel = param.style("species.selected")
              , multiple = FALSE
              , width = "100%")
  }
})

####################################################################

get_traits = eventReactive(list(input$species.traits, input$compute.distance), {
  if (is.data.frame(input$species.traits))
  {
    if (extension(input$species.traits$name) %in% c(".txt", ".csv"))
    {
      sp.traits = fread(input$species.traits$datapath)
      
      if (length(which(colnames(sp.traits) == "species")) == 1)
      {
        return(sp.traits)
      } else
      {
        shinyalert(type = "warning", text = "The species.traits must contain a column named 'species' !")
        return(NULL)
      }
    } else
    {
      shinyalert(type = "warning", text = "You must provide a text file for the species.traits !")
    }
  } else
  {
    shinyalert(type = "warning", text = "You must provide a text file for the species.traits !")
  }
})


get_dom = eventReactive(list(input$choice.dominant, input$compute.distance), {
  end_filename = ""
  if (input$choice.dominant == "from file")
  {
    print(input$species.selected)
    if (is.data.frame(input$species.selected))
    {
      if (extension(input$species.selected$name) %in% c(".txt", ".csv"))
      {
        end_filename = input$species.selected$datapath
      } else
      {
        shinyalert(type = "warning", text = "You must provide a text file for the species.selected !")
      }
    } else
    {
      shinyalert(type = "warning", text = "You must provide a text file for the species.selected !")
    }
  } else
  {
    end_filename = paste0(c(input$selectionRule.quanti
                            , input$selectionRule.min_mean_abund
                            , input$selectionRule.min_no_abund_over25)
                          , collapse = "_")
    if(input$doHabitatSelection)
    {
      end_filename = paste0(c(end_filename
                              , input$selectionRule.min_percent_habitat
                              , input$selectionRule.min_no_habitat)
                            , collapse = "_")
    }
    end_filename = paste0("PRE_FATE_DOMINANT_species_selected_SPECIES_ONLY_"
                          , end_filename
                          , ".csv")
  }
  if (file.exists(end_filename))
  {
    sp.select = fread(end_filename)
    if (ncol(sp.select) > 1)
    {
      warning(paste0("The file ", input$species.selected$name, " contains ", ncol(sp.select)
                     , " columns. Only the first, which should contain dominant species, will be used."))
    }
    
    sp.dom = as.data.frame(sp.select)[, 1]
    return(sp.dom)
  } else
  {
    shinyalert(type = "warning", text = "You must run the selection of dominant species or provide a text file for species.selected !")
    return(NULL)
  }
})

####################################################################

output$table.traits = renderDataTable({
  sp.traits = get_traits()
  if (!is.null(sp.traits))
  {
    print(head(sp.traits))
    return(sp.traits)
  }
})

####################################################################

observeEvent(input$compute.distance, {
  
  ## GET species traits
  sp.traits = get_traits()
  if (!is.null(sp.traits))
  {
    ## GET selected dominant species
    sp.dom = get_dom()
    if (!is.null(sp.dom))
    {
      if (length(which(sp.traits$species %in% sp.dom)) > 0)
      {
        sp.traits = sp.traits[which(sp.traits$species %in% sp.dom), ]
        
        ## GET species niche distance
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
          
          if (length(which(colnames(sp.niche) %in% sp.dom)) > 0)
          {
            
            get_res = print_messages(as.expression(
              PRE_FATE.speciesDistance(mat.species.traits = sp.traits
                                       , mat.species.overlap = sp.niche
                                       , opt.max.percent.NA = as.numeric(input$opt.max.percent.NA)
                                       , opt.max.percent.similarSpecies = as.numeric(input$opt.max.percent.similarSpecies)
                                       , opt.min.sd = as.numeric(input$opt.min.sd)
              )
            ))
          } else
          {
            shinyalert(type = "warning", text = "Species names between species.niche.distance and dominant species do not match !")
          }
        } else
        {
          shinyalert(type = "warning", text = "You must provide a text or a RData file for the species.niche.distance !")
        }
      } else
      {
        print("yuuu")
        shinyalert(type = "warning", text = "Species names between species.traits and dominant species do not match !")
      }
    }
  }
})
