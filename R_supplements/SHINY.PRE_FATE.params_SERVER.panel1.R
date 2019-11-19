
####################################################################

observeEvent(input$HELP.panel1, {
  introjs(session = session
          , options = list("nextLabel" = "Next"
                           , "prevLabel" = "Prev"
                           , "skipLabel" = "Close"
                           , steps = data.frame(element = c("#pfg.panel1", paste0("#help1_", 2:3), "#pfg.panel2")
                                                , intro = c("Functional group building is made in 3 steps :
                                                            <ul>
                                                            <li>the selection of dominant species within the studied area
                                                            </li>
                                                            <li>the computation of functional distance (traits, climatic niche...) between each pair of selected species
                                                            </li>
                                                            <li>the division into clusters, and the calculation of traits for each group obtained
                                                            </li>
                                                            </ul>
                                                            "
                                                            , "Species observations must be given by sites. 
                                                            Colnames must be <em>species</em>, <em>sites</em> and <em>abund</em> (presence/absence data are allowed). 
                                                            An optional column <em>habitat</em> can be given if selection of dominant is to be done also by type of habitat."
                                                            , "Traits must be given for as many species (dominant) as possible.
                                                            Grouping information can be given as well to perform the clustering for different species assemblages (e.g. life form)."
                                                            , "Intermediate graphics allow the user to adjust the parameters at each step."))
          )
  )
})

####################################################################


observeEvent(input$select.dominant, {
  RV$pfg.graph <- c(RV$pfg.graph, "dom") 
})
observeEvent(input$compute.distance, {
  print("BABAHABABABAB")
  RV$pfg.graph <- c(RV$pfg.graph, "dist") 
})
observeEvent(input$clustering.step1, {
  RV$pfg.graph <- c(RV$pfg.graph, "clust1") 
})
observeEvent(input$clustering.step2, {
  RV$pfg.graph <- c(RV$pfg.graph, "clust2") 
})
observeEvent(input$clustering.step3, {
  RV$pfg.graph <- c(RV$pfg.graph, "clust3") 
})


observeEvent(list(input$select.dominant
                  , input$compute.distance
                  , input$clustering.step1
                  , input$clustering.step2
                  , input$clustering.step3
), {
  
  print(RV$pfg.graph)
  levels_graph.type = unique(RV$pfg.graph)
  levels_graph.type = factor(levels_graph.type, c("dom", "dist", "clust1", "clust2", "clust3"))
  levels_graph.type = sort(levels_graph.type)
  pp = foreach(graph.type = levels_graph.type, .combine ="c") %do%
    {
      graph.type = as.character(graph.type)
      tab = switch(graph.type
                   , dom = {
                     fread(paste0("PRE_FATE_DOMINANT_species_selected_COMPLETE_TABLE_"
                                  , get_dom_param()
                                  , ".csv"), stringsAsFactors = FALSE)
                   }
                   , dist = {
                     get_DIST()
                   })
      if (!is.null(tab) && length(tab) > 1)
      {
        opt.abund_fixedScale = FALSE
        
        # ## pixels
        # vec_col = c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c'
        #             , '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00'
        #             , '#cab2d6', '#6a3d9a', '#ffff99', '#b15928')
        # fun_col = colorRampPalette(vec_col)
        # 
        # ## abund
        # col_vec = c('#6da34d', '#297373', '#58a4b0', '#5c4742', '#3f334d')
        # col_fun = colorRampPalette(col_vec)
        
        colRamp = colorRampPalette(c('#8e0152','#c51b7d','#de77ae','#7fbc41','#4d9221','#276419'))
        
        variables.labeller = c("stat.no_sites_recorded" = "stat.no_sites_recorded"
                               , "Fake1" = "", "Fake2" = ""
                               , "stat.abund_median" = "stat.abund_median"
                               , "stat.abund_mean" = "stat.abund_mean"
                               , "stat.abund_max" = "stat.abund_max"
                               , "stat.no_sites_abund" = "stat.no_sites_abund"
                               , "stat.no_sites_abund_max" = "stat.no_sites_abund_max"
                               , "stat.no_sites_abund_over25" = "stat.no_sites_abund_over25"
        )
        
        pp = switch(graph.type
                    ## ---------------------------------------------------------------------------------------------------------- ##
                    , dom = {
                      mat.plot = melt(tab, id.vars = c("species","SELECTION"))
                      mat.plot = rbind(mat.plot, data.frame(species = "SP.ghost", SELECTION = NA, 
                                                            variable = c("Fake1","Fake2"), value = NA))
                      mat.plot$variable = factor(mat.plot$variable, c("stat.no_sites_recorded"
                                                                      , "Fake1", "Fake2"
                                                                      , "stat.abund_median"
                                                                      , "stat.abund_mean"
                                                                      , "stat.abund_max"
                                                                      , "stat.no_sites_abund"
                                                                      , "stat.no_sites_abund_max"
                                                                      , "stat.no_sites_abund_over25"
                      ))
                      mat.plot$SELECTION = sub("all_","global + habitat ",mat.plot$SELECTION)
                      mat.plot$SELECTION = sub("all","global",mat.plot$SELECTION)
                      mat.plot$limit = NA
                      # mat.plot$limit[which(mat.plot$variable == "stat.no_sites_recorded")] = quant
                      mat.plot$limit[which(mat.plot$variable == "stat.abund_mean")] = input$selectionRule.min_mean_abund
                      mat.plot$limit[which(mat.plot$variable == "stat.no_sites_abund_over25")] = input$selectionRule.min_no_abund_over25
                      
                      if(length(na.exclude(unique(mat.plot$SELECTION))) == 1) {
                        colos = "grey35"
                      } else {
                        colos = colRamp(length(na.exclude(unique(mat.plot$SELECTION))))
                      }
                      
                      pp2 = ggplot(mat.plot, aes_string(x = "value", fill = "SELECTION")) +
                        geom_histogram(position = "dodge", na.rm = TRUE) +
                        geom_vline(aes_string(xintercept = "limit"), lwd = 1, color = "#fb6a4a", na.rm = TRUE) +
                        scale_fill_manual("Species selected in dataset :", values = colos) +
                        facet_wrap("variable", scales="free", labeller = as_labeller(variables.labeller)) +
                        labs(x="", y = "", title = paste0("STEP 2 : selected dominant species (", length(unique(mat.plot$species)) - 1,")"),
                             subtitle = paste0("Criteria used for each species (highlighted with colored vertical lines) :\n"
                                               , "Total number of presences >= quantile( ", input$selectionRule.quanti * 100, "% )\n"
                                               , "Mean abundance >= ", input$selectionRule.min_mean_abund, "\n"
                                               , "Number of releves with (abundance > 25) >= ", input$selectionRule.min_no_abund_over25
                                               , "\n")) +
                        .getGraphics_theme() +
                        theme(legend.position = c(0.7, 1),
                              legend.title = element_text(size=10),
                              legend.direction = "vertical")
                      return(list(pp2))
                    }
                    ## ---------------------------------------------------------------------------------------------------------- ##
                    , dist = {
                      pp = foreach(x = names(tab)) %do%
                        {
                          hc = hclust(tab[[x]])
                          pp = ggdendrogram(hc, rotate = TRUE) +
                            labs(title = paste0("Hierarchical clustering based on species distance "
                                                , ifelse(length(names(tab)) > 1
                                                         , paste0("(group ", x, ")")
                                                         , "")))
                          return(pp)
                        }
                    }
                    ## ---------------------------------------------------------------------------------------------------------- ##
        )
      }
    }
  
  if (is.ggplot(pp) || (is.list(pp) && length(pp) > 0))
  {
    print("isggplot")
    print(is.ggplot(pp))
    print(str(pp))
    
    if (is.ggplot(pp))
    {
      RV$compt.browser.pfg <- 1
      RV$compt.browser.pfg.max <- 1
      
      shinyjs::hide("pfg.go.left")
      shinyjs::hide("pfg.go.right")
      
      output$UI.pfg.browser = renderUI({
        plotOutput(outputId = "pfg.browser", width = "100%", height = "600px")
      })
      
      output$pfg.browser = renderPlot({ pp })
    } else
    {
      shinyjs::show("pfg.go.left")
      shinyjs::show("pfg.go.right")
      shinyjs::disable("pfg.go.left")
      shinyjs::disable("pfg.go.right")
      if (length(pp) > 1)
      {
        shinyjs::enable("pfg.go.right")
      }
      
      output$UI.pfg.browser = renderUI({
        lapply(1:length(pp), function(i) {
          plotOutput(outputId = paste0("pfg.browser_", i), width = "100%", height = "600px")
        })
      })
      
      lapply(1:length(pp), function(i) {
        output[[paste0("pfg.browser_", i)]] = renderPlot({ plot(pp[[i]]) })
      })
      
      RV$compt.browser.pfg.max <- length(pp)
      RV$compt.browser.pfg <- 1
      
      all.plot = 1:RV$compt.browser.pfg.max
      shinyjs::show(paste0("pfg.browser_", RV$compt.browser.pfg))
      if (RV$compt.browser.pfg.max > 1)
      {
        for (i in all.plot[-RV$compt.browser.pfg])
        {
          shinyjs::hide(paste0("pfg.browser_", i))
        }
      }
    }
  }
})

####################################################################

observeEvent(RV$compt.browser.pfg, {
  all.plot = 1:RV$compt.browser.pfg.max
  shinyjs::show(paste0("pfg.browser_", RV$compt.browser.pfg))
  if (RV$compt.browser.pfg.max > 1)
  {
    for (i in all.plot[-RV$compt.browser.pfg])
    {
      shinyjs::hide(paste0("pfg.browser_", i))
    }
  }
})

observeEvent(input$pfg.go.left, {
  if (input$pfg.go.left > 0)
  {
    shinyjs::enable("pfg.go.right")
    RV$compt.browser.pfg <- RV$compt.browser.pfg - 1
    if (RV$compt.browser.pfg == 1)
    {
      shinyjs::disable("pfg.go.left")
    }
  }
})

observeEvent(input$pfg.go.right, {
  if (input$pfg.go.right > 0)
  {
    shinyjs::enable("pfg.go.left")
    RV$compt.browser.pfg <- RV$compt.browser.pfg + 1
    if (RV$compt.browser.pfg == RV$compt.browser.pfg.max)
    {
      shinyjs::disable("pfg.go.right")
    }
  }
})



