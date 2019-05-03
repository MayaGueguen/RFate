
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

####################################################################

observeEvent(input$create.relativeAbund, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.relativeAbund_presenceAbsence(name.simulation = get_name.simul()
                                            , file.simulParam = input$graph.simulParam
                                            , year = as.numeric(input$graph.year)
                                            , strata_min = as.numeric(input$graph.strata_min)
                                            , rel.abund.threshold = input$graph.rel_abund_thresh
                                            , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  setwd(path.init)
})


####################################################################

observeEvent(input$create.PFGvsHS, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_mapPFGvsHS(name.simulation = get_name.simul()
                                 , file.simulParam = input$graph.simulParam
                                 , year = input$graph.year
                                 , opt.strata = input$opt.strata_min
                                 , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  if(get_res)
  {
    # col_vec = c('#6da34d', '#297373', '#58a4b0', '#5c4742', '#3f334d')
    # col_fun = colorRampPalette(col_vec)
    # 
    # distri.melt = fread(get_last.createdFiles2(pattern_head = "POST_FATE_evolution_spaceOccupancy_"
    #                                            , pattern_tail = ".csv$"))
    # 
    # distriAbund.melt = fread(get_last.createdFiles2(pattern_head = "POST_FATE_evolution_abundance_"
    #                                                 , pattern_tail = ".csv$"))
    # 
    # output$plot.evolutionCoverage1 = renderPlot({
    #   
    #   ## Evolution of space occupation
    #   pp1 = ggplot(distri.melt, aes_string(x = "YEAR", y = "Abund * 100", color = "factor(HAB)")) +
    #     geom_line(lwd = 1) +
    #     facet_wrap("~ PFG") +
    #     scale_color_manual("Habitat", values = col_fun(length(unique(distri.melt$HAB)))) +
    #     labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' space occupation"),
    #          subtitle = paste0("For each PFG, the line represents the evolution through time of its space occupancy,\n",
    #                            "meaning the percentage of pixels in which the abundance of the species is greater than 0.\n")) +
    #     theme_fivethirtyeight() +
    #     theme(panel.background = element_rect(fill = "transparent", colour = NA)
    #           , plot.background = element_rect(fill = "transparent", colour = NA)
    #           , legend.background = element_rect(fill = "transparent", colour = NA)
    #           , legend.box.background = element_rect(fill = "transparent", colour = NA)
    #           , legend.key = element_rect(fill = "transparent", colour = NA))
    #   
    #   print(pp1)
    # })
  }
  setwd(path.init)
})

####################################################################

observeEvent(input$create.validationStat, {

  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_validationStatistics(name.simulation = get_name.simul()
                                           , file.simulParam = input$graph.simulParam
                                           , year = as.numeric(input$graph.year)
                                           , mat.PFG.obs = input$graph.mat.PFG.obs
                                           , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  if(get_res)
  {
    mat.valid = fread(get_last.createdFiles2(pattern_head = "POST_FATE_prediction_VALIDATION_STATISTICS_"
                                               , pattern_tail = ".csv$"))

    mat.valid = melt(mat.valid, id.vars = c("PFG", "AUC.sd", "sensitivity.sd", "specificity.sd"))
    mat.valid$variable = factor(mat.valid$variable, c("sensitivity", "TSS", "specificity", "AUC"))
    
    mat.valid$AUC.sd[which(mat.valid$variable != "AUC")] = NA
    mat.valid$sensitivity.sd[which(mat.valid$variable != "sensitivity")] = NA
    mat.valid$specificity.sd[which(mat.valid$variable != "specificity")] = NA
    
    mat.valid_list[[y]] = mat.valid
    
    mat.valid$hline = 0.5
    mat.valid$hline[which(mat.valid$variable == "AUC")] = 0.8
    mat.valid$hline[which(mat.valid$variable == "TSS")] = 0.4
    
    output$plot.validationStat = renderPlot({

      ## produce the plot ------------------------------------------------------------
      ## 1. get the legend
      pp = ggplot(mat.valid, aes_string(x = "PFG", y = "value", fill = "value")) +
        scale_fill_gradientn(""
                             , colors = brewer.pal(9, "RdYlGn")
                             , breaks = seq(0, 1, 0.2)
                             , limits = c(0, 1)) +
        geom_bar(stat = "identity") +
        ylim(0, 1) +
        theme_fivethirtyeight() +
        theme(legend.key.width = unit(2, "lines")
              , panel.background = element_rect(fill = "transparent", colour = NA)
              , plot.background = element_rect(fill = "transparent", colour = NA)
              , legend.background = element_rect(fill = "transparent", colour = NA)
              , legend.box.background = element_rect(fill = "transparent", colour = NA)
              , legend.key = element_rect(fill = "transparent", colour = NA))
      pp_leg = get_legend(pp)
      
      ## 2. get one plot for the title and for each statistic
      pp_list = foreach(vari = c("all", "sensitivity", "specificity", "TSS", "AUC")) %do%
      {
        if (vari == "all"){
          pp = ggplot(mat.valid, aes_string(x = "PFG", y = "value", fill = "value")) +
            labs(x = "", y = "", title = paste0("GRAPH F : validation statistics - Simulation year : ", y),
                 subtitle = paste0("Sensitivity (or specificity) measures the proportion of actual positives (or negatives) that are correctly identified as such.\n"
                                   , "True skill statistic (TSS) values of -1 indicate predictive abilities of not better than a random model,\n"
                                   , "0 indicates an indiscriminate model and +1 a perfect model.\n"
                                   , "AUC corresponds to the area under the ROC curve (Receiver Operating Characteristic).\n")) +
            theme_fivethirtyeight() +
            theme(panel.background = element_rect(fill = "transparent", colour = NA)
                  , panel.grid = element_blank()
                  , axis.text = element_blank()
                  , plot.background = element_rect(fill = "transparent", colour = NA)
                  , legend.background = element_rect(fill = "transparent", colour = NA)
                  , legend.box.background = element_rect(fill = "transparent", colour = NA)
                  , legend.key = element_rect(fill = "transparent", colour = NA))
        } else {
          if (vari == "sensitivity") subti = "Sensitivity - True positive rate"
          if (vari == "specificity") subti = "Specificity - True negative rate"
          if (vari == "TSS") subti = "True Skill Statistic (TSS)"
          if (vari == "AUC") subti = "Area Under Curve (AUC)"
          pp = ggplot(mat.valid[which(mat.valid$variable == vari), ]
                      , aes_string(x = "PFG", y = "value", fill = "value")) +
            scale_fill_gradientn(guide = F
                                 , colors = brewer.pal(9, "RdYlGn")
                                 , breaks = seq(0, 1, 0.2)
                                 , limits = c(0, 1)) +
            scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.08)) +
            geom_bar(stat = "identity") +
            geom_hline(aes_string(yintercept = "hline"), lty = 2, color = "grey30") +
            geom_errorbar(aes(ymin = value - sensitivity.sd, ymax = value + sensitivity.sd), color = "grey30") +
            geom_errorbar(aes(ymin = value - specificity.sd, ymax = value + specificity.sd), color = "grey30") +
            geom_errorbar(aes(ymin = value - AUC.sd, ymax = value + AUC.sd), color = "grey30") +
            annotate(geom = "text", x = no_PFG / 2, y = 1.05, label = subti, size = 4) +
            theme_fivethirtyeight() +
            theme(panel.background = element_rect(fill = "transparent", colour = NA)
                  , plot.background = element_rect(fill = "transparent", colour = NA)
                  , legend.background = element_rect(fill = "transparent", colour = NA)
                  , legend.box.background = element_rect(fill = "transparent", colour = NA)
                  , legend.key = element_rect(fill = "transparent", colour = NA)
                  , axis.text.x = element_text(angle = 90))
          
          pp = ggMarginal(pp, type = "boxplot", margins = "y", size = 7)
        }
        
        return(pp)
      }
      
      ## 3. gather everything
      pp_list[[6]] = pp_leg
      grid.arrange(grobs = pp_list
                   , layout_matrix = matrix(c(1,1,2,3,2,3,4,5,4,5,6,6), ncol = 2, byrow = TRUE)
                   , newpage = ifelse(y == years[1], FALSE, TRUE))
    })
  }
  setwd(path.init)
})

####################################################################

observeEvent(input$create.PFGrichness, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_mapPFGrichness(name.simulation = get_name.simul()
                                     , file.simulParam = input$graph.simulParam
                                     , year = as.numeric(input$graph.year)
                                     , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  if(get_res)
  {
    ras_TOT = raster(get_last.createdFiles2(pattern_path = paste0(basename(get_dir.save()), "/")
                                            , pattern_head = "PFGrichness_YEAR_"
                                            , pattern_tail = ".tif$"))
    
    ras.pts = as.data.frame(rasterToPoints(ras_TOT))
    colnames(ras.pts) = c("X", "Y", "NB")
    
    output$plot.PFGrichness = renderPlot({
      
      ## Map of PFG richness
      pp = ggplot(ras.pts, aes_string(x = "X", y = "Y", fill = "NB")) +
        scale_fill_gradientn("Number of PFG"
                             , colors = viridis_pal()(max(ras.pts$NB))
                             , breaks = seq(1, max(ras.pts$NB), 2)) +
        coord_equal() +
        geom_raster() +
        labs(x = "", y = ""
             , title = paste0("GRAPH D : map of PFG richness - Simulation year : ", as.numeric(input$graph.year))
             , subtitle = paste0("For each pixel and stratum, first relative abundances are calculated, "
                                 , "then transformed into binary values :\n"
                                 , "1 if the PFG abundance represents more than 5 % "
                                 , "of the pixel abundance, 0 otherwise.\n"
                                 , "If the PFG is present in one stratum, then it is considered present within the pixel.\n"
                                 , "Finally, simulated PFG occurrences are summed.\n")) +
        theme_fivethirtyeight() +
        theme(axis.text = element_blank()
              , legend.key.width = unit(2, "lines")
              , panel.background = element_rect(fill = "transparent", colour = NA)
              , plot.background = element_rect(fill = "transparent", colour = NA)
              , legend.background = element_rect(fill = "transparent", colour = NA)
              , legend.box.background = element_rect(fill = "transparent", colour = NA)
              , legend.key = element_rect(fill = "transparent", colour = NA))

      print(pp)
    })
  }
  setwd(path.init)
})


