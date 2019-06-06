
####################################################################

get_names.files = eventReactive(input$graph.folder.simul, {
  names.files1 = list.files(path = paste0(get_path.simul(), "/RESULTS")
                            , pattern = ".csv$"
                            , all.files = FALSE
                            , full.names = TRUE)
  names.files1 = basename(names.files1)
  
  
  names.files2 = list.files(path = paste0(get_path.simul(), "/RESULTS")
                            , pattern = "map_PFG"
                            , all.files = FALSE
                            , full.names = TRUE)
  dir.files2 = sub(".*SIMUL_V", "SIMUL_V", names.files2)
  dir.files2 = sub(".pdf$", "", dir.files2)
  
  names.files3 = list.files(path = paste0(get_path.simul(), "/RESULTS/", dir.files2)
                            , pattern = "^PFGrichness|^PFGcover"
                            , all.files = FALSE
                            , full.names = TRUE)
  names.files3 = paste0(basename(dirname(names.files3)), "/", basename(names.files3))
  
  names.files = c(names.files1, names.files3)
  names.files = sub(".csv$|.pdf$|.tif$", "", names.files)
  return(names.files)
})

update_browser.files = function()
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
                      , inputId = "browser.files"
                      , choices = names.files
                      , selected = names.files[1])
    shinyjs::enable("browser.files")
  } else
  {
    shinyjs::disable("browser.files")
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

observeEvent(input$browser.abundance, { update_browser.files() })
observeEvent(input$browser.validation, { update_browser.files() })
observeEvent(input$browser.richness, { update_browser.files() })
observeEvent(input$browser.cover, { update_browser.files() })
observeEvent(input$browser.light, { update_browser.files() })
observeEvent(input$browser.soil, { update_browser.files() })

####################################################################

get_graph.type = eventReactive(input$browser.files, {
  if (nchar(input$browser.files) > 0)
  {
    if (length(grep("evolution_abundance_pixels", input$browser.files)) > 0)
    {
      return("abund.pixels")
    } else if (length(grep("evolution_light_pixels", input$browser.files)) > 0)
    {
      return("light.pixels")
    } else if (length(grep("evolution_soil_pixels", input$browser.files)) > 0)
    {
      return("soil.pixels")
    } else if (length(grep("evolution_abundance", input$browser.files)) > 0)
    {
      return("abund")
    } else if (length(grep("evolution_spaceOccupancy", input$browser.files)) > 0)
    {
      return("spaceOccupancy")
    } else if (length(grep("VALIDATION_STATISTICS", input$browser.files)) > 0)
    {
      return("validation")
    } else if (length(grep("PFGrichness", input$browser.files)) > 0)
    {
      return("richness")
    } else if (length(grep("PFGcover", input$browser.files)) > 0)
    {
      return("cover")
    } else if (length(grep("PFGlight", input$browser.files)) > 0)
    {
      return("light")
    } else if (length(grep("PFGsoil", input$browser.files)) > 0)
    {
      return("soil")
    } else if (length(grep("PFGvsHS", input$browser.files)) > 0)
    {
      return("PFGvsHS")
    } else
    {
      return(NA)
    }
  }
})

observeEvent(input$browser.files, {
  if (nchar(input$browser.files) > 0)
  {
    graph.type = get_graph.type()
    if (!is.na(graph.type))
    {
      if (graph.type %in% c("richness", "cover"))
      {
        ras = raster(paste0(get_path.simul(), "/RESULTS/", input$browser.files, ".tif"))
        print(ras)
      } else
      {
        tab = fread(paste0(get_path.simul(), "/RESULTS/", input$browser.files, ".csv"))
        print(head(tab))
      }
      print(graph.type)
      
      opt.abund_fixedScale = FALSE
      
      ## pixels
      vec_col = c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c'
                  , '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00'
                  , '#cab2d6', '#6a3d9a', '#ffff99', '#b15928')
      fun_col = colorRampPalette(vec_col)
      
      ## abund
      col_vec = c('#6da34d', '#297373', '#58a4b0', '#5c4742', '#3f334d')
      col_fun = colorRampPalette(col_vec)
      
      pp = switch(graph.type
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , abund.pixels = {
                    pp = ggplot(tab, aes_string(x = "YEAR", y = "Abund", color = "PFG")) +
                      scale_color_manual("", values = fun_col(length(unique(tab$PFG)))) +
                      geom_line() +
                      facet_grid("TYPE ~ ID", scales = ifelse(opt.abund_fixedScale, "fixed", "free_y")) +
                      labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' abundance"),
                           subtitle = paste0("For each PFG, the line represents the evolution through time of its abundance\n",
                                             "for 5 randomly selected pixels within the studied area.\n")) +
                      .getGraphics_theme()
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , light.pixels = {
                    pp = ggplot(tab, aes_string(x = "YEAR", y = "Abund", color = "STRATUM")) +
                      scale_color_manual("", values = fun_col(max(sub("STRATUM_", "", tab$STRATUM)))) +
                      geom_line() +
                      facet_grid("TYPE ~ ID", scales = ifelse(opt.abund_fixedScale, "fixed", "free_y")) +
                      labs(x = "", y = "", title = paste0("GRAPH B : evolution of light resources"),
                           subtitle = paste0("For each stratum, the line represents the evolution through time of its light resources\n",
                                             "for 5 randomly selected pixels within the studied area.\n")) +
                      .getGraphics_theme()
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , soil.pixels = {
                    pp = ggplot(tab, aes_string(x = "YEAR", y = "SOIL"))
                    
                    if ("Abund" %in% colnames(tab))
                    {
                      pp = pp +
                        geom_line(aes_string(y = "Abund", color = "PFG"), lwd = 0.4) +
                        scale_color_manual("", values = fun_col(length(unique(tab$PFG)))) +
                        facet_grid("TYPE ~ ID", scales = "fixed")
                    } else
                    {
                      pp = pp +
                        facet_grid(" ~ ID", scales = "fixed")
                    }
                    pp = pp +
                      geom_line(lwd = 0.8) +
                      labs(x = "", y = "", title = paste0("GRAPH B : evolution of soil resources"),
                           subtitle = paste0("The line represents the evolution through time of the soil resources\n",
                                             "for 5 randomly selected pixels within the studied area.\n")) +
                      .getGraphics_theme()
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , spaceOccupancy = {
                    pp = ggplot(tab, aes_string(x = "YEAR", y = "Abund * 100", color = "factor(HAB)")) +
                      geom_line(lwd = 1) +
                      facet_wrap("~ PFG") +
                      scale_color_manual("Habitat", values = col_fun(length(unique(tab$HAB)))) +
                      labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' space occupation"),
                           subtitle = paste0("For each PFG, the line represents the evolution through time of its space occupancy,\n",
                                             "meaning the percentage of pixels in which the abundance of the species is greater than 0.\n")) +
                      .getGraphics_theme()
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , abund = {
                    pp = ggplot(tab, aes_string(x = "YEAR", y = "Abund", color = "HAB")) +
                      geom_line(lwd = 1) +
                      facet_wrap("~ PFG", scales = ifelse(opt.abund_fixedScale, "fixed", "free_y")) +
                      scale_color_manual("Habitat", values = col_fun(length(unique(tab$HAB)))) +
                      labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' abundance"),
                           subtitle = paste0("For each PFG, the line represents the evolution through time of its abundance\n",
                                             "over the whole studied area, meaning the sum of its abundances in every pixel.\n")) +
                      .getGraphics_theme()
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , validation = {
                    ## prepare the plot ------------------------------------------------------------
                    tab = melt(tab, id.vars = c("PFG", "HAB", "AUC.sd", "sensitivity.sd", "specificity.sd"))
                    tab$variable = factor(tab$variable, c("sensitivity", "TSS", "specificity", "AUC"))
                    
                    tab$AUC.sd[which(tab$variable != "AUC")] = NA
                    tab$sensitivity.sd[which(tab$variable != "sensitivity")] = NA
                    tab$specificity.sd[which(tab$variable != "specificity")] = NA
                    
                    tab$hline = 0.5
                    tab$hline[which(tab$variable == "AUC")] = 0.8
                    tab$hline[which(tab$variable == "TSS")] = 0.4
                    
                    hab_names = sort(unique(tab$HAB))
                    
                    ## produce the plot ------------------------------------------------------------
                    # plot_list.hab = foreach(habi = hab_names) %do%
                    # {
                    #   cat("\n > Preparing for habitat ", habi)
                    #   mat.plot = tab[which(tab$HAB == habi), ]
                    #   
                    #   ## 1. get the legend
                    #   pp = ggplot(mat.plot, aes_string(x = "PFG", y = "value", fill = "value")) +
                    #     scale_fill_gradientn(""
                    #                          , colors = brewer.pal(9, "RdYlGn")
                    #                          , breaks = seq(0, 1, 0.2)
                    #                          , limits = c(0, 1)) +
                    #     geom_bar(stat = "identity", na.rm = TRUE) +
                    #     ylim(0, 1) +
                    #     .getGraphics_theme() +
                    #     theme(legend.key.width = unit(2, "lines"))
                    #   
                    #   pp_leg = suppressWarnings(get_legend(pp))
                    #   
                    #   ## 2. get one plot for the title and for each statistic
                    #   pp_list = foreach(vari = c("all", "sensitivity", "specificity", "TSS", "AUC")) %do%
                    #   {
                    #     if (vari == "all"){
                    #       pp = ggplot(mat.plot, aes_string(x = "PFG", y = "value", fill = "value")) +
                    #         labs(x = "", y = "", title = paste0("GRAPH F : validation statistics - Simulation year : ", y, " - Habitat ", habi),
                    #              subtitle = paste0("Sensitivity (or specificity) measures the proportion of actual positives (or negatives) that are correctly identified as such.\n"
                    #                                , "True skill statistic (TSS) values of -1 indicate predictive abilities of not better than a random model,\n"
                    #                                , "0 indicates an indiscriminate model and +1 a perfect model.\n"
                    #                                , "AUC corresponds to the area under the ROC curve (Receiver Operating Characteristic).\n")) +
                    #         .getGraphics_theme() +
                    #         theme(panel.grid = element_blank()
                    #               , axis.text = element_blank())
                    #     } else
                    #     {
                    #       if (vari == "sensitivity") subti = "Sensitivity - True positive rate"
                    #       if (vari == "specificity") subti = "Specificity - True negative rate"
                    #       if (vari == "TSS") subti = "True Skill Statistic (TSS)"
                    #       if (vari == "AUC") subti = "Area Under Curve (AUC)"
                    #       pp = ggplot(mat.plot[which(mat.plot$variable == vari), ]
                    #                   , aes_string(x = "PFG", y = "value", fill = "value")) +
                    #         scale_fill_gradientn(guide = F
                    #                              , colors = brewer.pal(9, "RdYlGn")
                    #                              , breaks = seq(0, 1, 0.2)
                    #                              , limits = c(0, 1)) +
                    #         scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.08)) +
                    #         geom_bar(stat = "identity", na.rm = TRUE) +
                    #         geom_hline(aes_string(yintercept = "hline"), lty = 2, color = "grey30") +
                    #         geom_errorbar(aes(ymin = value - sensitivity.sd, ymax = value + sensitivity.sd), color = "grey30", na.rm = TRUE) +
                    #         geom_errorbar(aes(ymin = value - specificity.sd, ymax = value + specificity.sd), color = "grey30", na.rm = TRUE) +
                    #         geom_errorbar(aes(ymin = value - AUC.sd, ymax = value + AUC.sd), color = "grey30", na.rm = TRUE) +
                    #         annotate(geom = "text", x = no_PFG / 2, y = 1.05, label = subti, size = 4) +
                    #         .getGraphics_theme() +
                    #         theme(axis.text.x = element_text(angle = 90))
                    #       
                    #       pp = suppressWarnings(ggMarginal(pp, type = "boxplot", margins = "y", size = 7))
                    #     }
                    #     
                    #     return(pp)
                    #   }
                    #   
                    #   ## 3. gather everything
                    #   pp_list[[6]] = pp_leg
                    #   return(grid.arrange(grobs = pp_list
                    #                       , layout_matrix = matrix(c(1,1,2,3,2,3,4,5,4,5,6,6), ncol = 2, byrow = TRUE)
                    #                       , newpage = ifelse(y == years[1], FALSE, TRUE)))
                    # } ## END loop on hab_names
                    # names(plot_list.hab) = hab_names
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , richness = {
                    ras.pts = as.data.frame(rasterToPoints(ras))
                    colnames(ras.pts) = c("X", "Y", "NB")
                    print(head(ras.pts))
                    
                    pp = ggplot(ras.pts, aes_string(x = "X", y = "Y", fill = "NB")) +
                      scale_fill_gradientn("Number of PFG"
                                           , colors = viridis_pal()(max(ras.pts$NB))
                                           , breaks = seq(1, max(ras.pts$NB), 2)) +
                      coord_equal() +
                      geom_raster() +
                      labs(x = "", y = ""
                           , title = paste0("GRAPH D : map of PFG richness - Simulation year : "
                                            , strsplit(sub(".*YEAR_", "", input$browser.files), "_")[[1]][1])
                           , subtitle = paste0("For each pixel and stratum, first relative abundances are calculated, "
                                               , "then transformed into binary values :\n"
                                               , "1 if the PFG abundance represents more than 5 % "
                                               , "of the pixel abundance, 0 otherwise.\n"
                                               , "If the PFG is present in one stratum, then it is considered present within the pixel.\n"
                                               , "Finally, simulated PFG occurrences are summed.\n")) +
                      .getGraphics_theme() +
                      theme(axis.text = element_blank()
                            , legend.key.width = unit(2, "lines"))
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , cover = {
                    ras.pts = as.data.frame(rasterToPoints(ras))
                    colnames(ras.pts) = c("X", "Y", "COVER")
                    print(head(ras.pts))
                    
                    pp = ggplot(ras.pts, aes_string(x = "X", y = "Y", fill = "COVER")) +
                      scale_fill_gradientn("Abundance (%)"
                                           , colors = brewer.pal(9, "Greens")
                                           , limits = c(0, 1)
                                           , breaks = seq(0, 1, 0.2)
                                           , labels = seq(0, 100, 20)) +
                      coord_equal() +
                      geom_raster() +
                      labs(x = "", y = "", title = paste0("GRAPH E : map of PFG cover - Simulation year : "
                                                          , strsplit(sub(".*YEAR_", "", input$browser.files), "_")[[1]][1]),
                           subtitle = paste0("For each pixel, PFG abundances from strata "
                                             , strsplit(sub(".*STRATA_", "", input$browser.files), "_")[[1]][1]
                                             , " to "
                                             , sub(".tif", "", tail(strsplit(input$browser.files, "_")[[1]], 1))
                                             , " are summed,\n"
                                             , "then transformed into relative values by dividing by the maximum abundance obtained.\n")) +
                      .getGraphics_theme() +
                      theme(axis.text = element_blank()
                            , legend.key.width = unit(2, "lines"))
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , light = {
                    ras.pts = as.data.frame(rasterToPoints(ras_light))
                    colnames(ras.pts) = c("X", "Y", "LIGHT")
                    print(head(ras.pts))
                    
                    pp = ggplot(ras.pts, aes_string(x = "X", y = "Y", fill = "LIGHT")) +
                      scale_fill_gradientn("Light (Landolt)"
                                           , colors = (brewer.pal(9, "Oranges"))) +
                      coord_equal() +
                      geom_raster() +
                      labs(x = "", y = "", title = paste0("GRAPH E : map of light CWM - Simulation year : "
                                                          , strsplit(sub(".*YEAR_", "", input$browser.files), "_")[[1]][1]),
                           subtitle = paste0("For each pixel, PFG abundances from strata "
                                             , strsplit(sub(".*STRATA_", "", input$browser.files), "_")[[1]][1]
                                             , " to "
                                             , sub(".tif", "", tail(strsplit(input$browser.files, "_")[[1]], 1))
                                             , " are summed,\n"
                                             , "then transformed into relative values by dividing by the maximum abundance obtained.\n"
                                             , "Community Weighted Mean is then calculated with observed values of light\n"
                                             , "(Landolt - Flora Indicativa) for each PFG.")) +
                      .getGraphics_theme() +
                      theme(axis.text = element_blank()
                            , legend.key.width = unit(2, "lines"))
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , soil = {
                    ras.pts = as.data.frame(rasterToPoints(ras_light))
                    colnames(ras.pts) = c("X", "Y", "SOIL")
                    print(head(ras.pts))
                    
                    pp = ggplot(ras.pts, aes_string(x = "X", y = "Y", fill = "SOIL")) +
                      scale_fill_gradientn("Soil (Landolt)"
                                           , colors = (brewer.pal(9, "Oranges"))) +
                      coord_equal() +
                      geom_raster() +
                      labs(x = "", y = "", title = paste0("GRAPH E : map of soil CWM - Simulation year : "
                                                          , strsplit(sub(".*YEAR_", "", input$browser.files), "_")[[1]][1]),
                           subtitle = paste0("For each pixel, PFG abundances from strata "
                                             , strsplit(sub(".*STRATA_", "", input$browser.files), "_")[[1]][1]
                                             , " to "
                                             , sub(".tif", "", tail(strsplit(input$browser.files, "_")[[1]], 1))
                                             , " are summed,\n"
                                             , "then transformed into relative values by dividing by the maximum abundance obtained.\n"
                                             , "Community Weighted Mean is then calculated with observed values of soil\n"
                                             , "(Landolt - Flora Indicativa) for each PFG.")) +
                      .getGraphics_theme() +
                      theme(axis.text = element_blank()
                            , legend.key.width = unit(2, "lines"))
                  }
      )
      
      output$plot.browser = renderPlotly({ ggplotly(pp) })
    }
  }
})
