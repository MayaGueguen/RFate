
####################################################################

output$UI.species.distance = renderUI({
  if (input$choice.distance == "from file")
  {
    fileInput(inputId = "species.distance"
              , label = NULL
              , buttonLabel = param.style("species.distance")
              , multiple = TRUE
              , width = "100%")
  }
})

####################################################################

output$UI.nb.clusters = renderUI({
  sp.clust = get_CLUST1()
  if (!is.null(sp.clust))
  {
    group_names = names(sp.clust$clust.dendograms)
    lapply(group_names, function(i) {
      sliderInput(inputId = paste0("no.clust_", i)
                  , label = param.style(paste0("no.clust_", i))
                  , min = 2
                  , max = max(sp.clust$clust.evaluation$nb.cluster[which(sp.clust$clust.evaluation$group == i)])
                  , value = 2
                  , step = 1
                  , round = TRUE
                  , width = "100%"
      )
    })
  }
})

####################################################################

get_dist = eventReactive(list(input$choice.distance, input$clustering.step1), {
  end_filename = ""
  if (input$choice.distance == "from file")
  {
    if (!is.null(input$species.distance) && is.data.frame(input$species.distance))
    {
      if (sum(sapply(input$species.distance$name, extension) %in% c(".txt", ".csv")) == nrow(input$species.distance))
      {
        end_filename = input$species.distance$datapath
      } else
      {
        if (length(which(RV$pfg.graph == "clust1")) > 0)
        {
          RV$pfg.graph <- RV$pfg.graph[-which(RV$pfg.graph == "clust1")]
        }
        shinyjs::disable("clustering.step2")
        shinyjs::disable("clustering.step3")
        shinyalert(type = "warning", text = "You must provide a text file (.txt or .csv) for the species.distance !")
      }
    } else
    {
      if (length(which(RV$pfg.graph == "clust1")) > 0)
      {
        RV$pfg.graph <- RV$pfg.graph[-which(RV$pfg.graph == "clust1")]
      }
      shinyjs::disable("clustering.step2")
      shinyjs::disable("clustering.step3")
      shinyalert(type = "warning", text = "You must provide a text file (.txt or .csv) for the species.distance !")
    }
  } else
  {
    end_filename = list.files(pattern = "^PRE_FATE_DOMINANT_species_distance_")
  }
  if (end_filename != "" && length(end_filename) > 0)
  {
    sp.dist = foreach(fi = end_filename) %do%
      {
        sp.di = fread(fi, header = TRUE, drop = 1)
        if (ncol(sp.di) == 0 || nrow(sp.di) == 0 || ncol(sp.di) != nrow(sp.di))
        {
          if (length(which(RV$pfg.graph == "clust1")) > 0)
          {
            RV$pfg.graph <- RV$pfg.graph[-which(RV$pfg.graph == "clust1")]
          }
          shinyjs::disable("clustering.step2")
          shinyjs::disable("clustering.step3")
          shinyalert(type = "warning", text = paste0("The file n°", which(end_filename == fi), " contains ", ncol(sp.di)
                                                     , " columns and "
                                                     , nrow(sp.di), " lines. It should be a square matrix (pairwise distances)."))
          return(NULL)
          
        } else
        {
          sp.di = as.dist(sp.di)
          return(sp.di)
        }
      }
    if (sum(sapply(sp.dist, is.null)) == 0)
    {
      names(sp.dist) = sub("^PRE_FATE_DOMINANT_species_distance_", "", sub(".csv$", "", end_filename))
      return(sp.dist)
    }
  } else
  {
    if (length(which(RV$pfg.graph == "clust1")) > 0)
    {
      RV$pfg.graph <- RV$pfg.graph[-which(RV$pfg.graph == "clust1")]
    }
    shinyjs::disable("clustering.step2")
    shinyjs::disable("clustering.step3")
    shinyalert(type = "warning", text = "You must run the selection of dominant species or provide a text file (.txt or .csv) for species.distance !")
    return(NULL)
  }
})

####################################################################

get_CLUST1 = eventReactive(input$clustering.step1, {
  
  ## GET species distance
  sp.dist = get_dist()
  if (!is.null(sp.dist))
  {
    get_res = print_messages(as.expression(
      PRE_FATE.speciesClustering_step1(mat.species.DIST = sp.dist)
    ))
    
    shinyjs::enable("clustering.step2")
    return(get_res)
  }
})

####################################################################

get_CLUST2 = eventReactive(input$clustering.step2, {
  
  ## GET species distance
  sp.dist = get_dist()
  if (!is.null(sp.dist))
  {
    ## GET species clusters
    sp.clust = get_CLUST1()
    if (!is.null(sp.clust))
    {
      no.clusters = foreach(i = names(sp.clust$clust.dendograms), .combine = "c") %do%
        {
          input[[paste0("no.clust_", i)]]
        }
      get_res = print_messages(as.expression(
        PRE_FATE.speciesClustering_step2(clust.dendograms = sp.clust$clust.dendograms
                                         , no.clusters = no.clusters
                                         , mat.species.DIST = sp.dist)
      ))
      
      shinyjs::enable("clustering.step3")
      return(get_res)
    }
  }
})

####################################################################

get_CLUST3 = eventReactive(input$clustering.step3, {
  
  ## GET species traits
  sp.traits = get_traits()
  if (!is.null(sp.traits))
  {
    ## GET species determ
    sp.determ = get_CLUST2()
    if (!is.null(sp.determ))
    {
      # no.clusters = foreach(i = names(sp.clust$clust.dendograms), .combine = "c") %do%
      #   {
      #     input[[paste0("no.clust_", i)]]
      #   }
      get_res = print_messages(as.expression(
        PRE_FATE.speciesClustering_step3(mat.species.traits = sp.pfg.traits)
      ))
      
      # shinyjs::enable("clustering.step3")
      return(get_res)
    }
  }
})
