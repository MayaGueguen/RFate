
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
                   , dist = { get_DIST() }
                   , clust1 = { get_CLUST1() }
                   , clust2 = { get_CLUST2() }
                   , clust3 = { get_CLUST3() }
      )
      if (graph.type == "clust2"){
        sp.dist = get_dist()
      }
      if (graph.type == "clust3"){
        sp.pfg.traits = get_sp.pfg.traits()
      }
      
      if (!is.null(tab) && length(tab) > 1)
      {
        opt.abund_fixedScale = FALSE
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
                    , clust1 = {
                      clust.evaluation = tab$clust.evaluation
                      group_names = names(tab$clust.dendograms)
                      
                      ## Find number of cluster which give optimal variable values
                      combi = expand.grid(group = group_names, variable = unique(clust.evaluation$variable))
                      
                      group = variable = NULL
                      clust.evaluation.optim = foreach(group = combi$group, variable = combi$variable) %do% {
                        tmp = clust.evaluation[which(clust.evaluation$group == group & clust.evaluation$variable == variable),]
                        if(variable == "mVI")
                        {
                          optim = unique(sort(tmp$value, decreasing = F))[1:3]
                          ind.optim = which(tmp$value %in% optim) 
                        } else {
                          optim = unique(sort(tmp$value, decreasing = T))[1:3]
                          ind.optim = which(tmp$value %in% optim)
                        }
                        optim.clust = tmp$nb.cluster[ind.optim]
                        optim.val = tmp$value[ind.optim]
                        return(data.frame(group, variable, optim.clust, optim.val))
                      }
                      clust.evaluation.optim = do.call(rbind, clust.evaluation.optim)
                      
                      ## GRAPHICAL REPRESENTATION
                      
                      pp2 = ggplot(clust.evaluation, aes_string(x = "nb.cluster", y = "value")) +
                        facet_grid("variable ~ group", scales = "free") +
                        geom_line() + geom_point() +
                        geom_vline(data = clust.evaluation.optim, aes_string(xintercept = "optim.clust", color = "group"), lwd = 4, alpha = 0.3) +
                        scale_color_manual(guide = F, values = colRamp(length(group_names))) +
                        labs(x = "", y = "", title = "STEP B : Choice of number of clusters",
                             subtitle = paste0("Evolution of clustering evaluation variables with the number of clusters in each group.\n",
                                               "All values except that of mVI must be maximized (check function's help for more details about the measures).\n",
                                               "The number of clusters with values among the 3 best are highlighted.")) +
                        theme_fivethirtyeight() +
                        theme(panel.background = element_rect(fill = "transparent", colour = NA)
                              , plot.background = element_rect(fill = "transparent", colour = NA)
                              , legend.background = element_rect(fill = "transparent", colour = NA)
                              , legend.box.background = element_rect(fill = "transparent", colour = NA)
                              , legend.key = element_rect(fill = "transparent", colour = NA))
                      
                      return(list(pp2))
                    }
                    ## ---------------------------------------------------------------------------------------------------------- ##
                    , clust2 = {
                      determ = tab$determ.all
                      colLev = levels(interaction(determ$toSuppr, determ$group))
                      group_names = unique(determ$group)
                      
                      pp3 = ggplot(determ, aes_string(x = "pfg", y = "sp.mean.dist"
                                                      , color = interaction(determ$toSuppr, determ$group)
                                                      , shape = ("toSuppr"))) +
                        scale_color_manual(guide = F, values = colRamp(length(colLev))) +
                        scale_shape_manual(guide = F, values = c("0" = 20, "1" = 8)) +
                        geom_errorbar(aes_string(ymin = "allSp.min", ymax = "allSp.max"), color = "darkblue") +
                        geom_point(position = "jitter") +
                        geom_point(aes_string(y = "allSp.mean"), pch = 18, lwd = 5, color = "darkblue") +
                        facet_grid("~ group", scales = "free_x") +
                        labs(x = "", y = "Mean distance to other species", title = "STEP C : Removal of distant species",
                             subtitle = paste0("Only species whose mean distance to other species is included in the distribution\n",
                                               "of all PFG's species mean distances to other species are kept.\n",
                                               "Species indicated with * will be removed from PFGs.\n",
                                               "Non-represented PFG might be one-species-only.")) +
                        theme_fivethirtyeight() +
                        theme(axis.ticks.x = element_blank()
                              , panel.background = element_rect(fill = "transparent", colour = NA)
                              , plot.background = element_rect(fill = "transparent", colour = NA)
                              , legend.background = element_rect(fill = "transparent", colour = NA)
                              , legend.box.background = element_rect(fill = "transparent", colour = NA)
                              , legend.key = element_rect(fill = "transparent", colour = NA)
                              , axis.text.x = element_text(angle = 90))
                      
                      ## GRAPHICAL REPRESENTATION 2
                      ## Compute Principal Coordinates Analysis (PCO) for the determinantes of each group
                      ## to see the position in "distance space" (overlap + traits) between species and groups
                      
                      pp4_list = foreach(group = group_names) %do%
                        {
                          tmp = determ[which(determ$group == group),]
                          mat = sp.dist[[group]]
                          mat = quasieuclid(as.dist(mat))
                          
                          PCO = dudi.pco(mat, scannf = FALSE, nf = 3) ## PCO
                          PCO.li = PCO$li
                          PCO.li$det = ifelse(rownames(PCO.li) %in% tmp$sp[which(tmp$toSuppr == 1)], 0, 1)
                          PCO.li$det = factor(PCO.li$det, c(0, 1))
                          PCO.li$PFG = determ[rownames(PCO.li), "pfg"]
                          
                          inert = inertia.dudi(PCO)$tot.inertia
                          inert = c(inert$`cum(%)`[1]
                                    , inert$`cum(%)`[2] - inert$`cum(%)`[1]
                                    , inert$`cum(%)`[3] - inert$`cum(%)`[2])
                          
                          PCO.li.ELL = .getELLIPSE(xy = PCO.li[, c("A1", "A2")], fac = PCO.li$PFG)
                          PCO.li.ELL.det = .getELLIPSE(xy = PCO.li[which(PCO.li$det == 1), c("A1", "A2")]
                                                       , fac = PCO.li$PFG[which(PCO.li$det == 1)])
                          
                          pp4 = ggplot(PCO.li, aes_string(x = "A1", y = "A2", color = "PFG")) +
                            geom_hline(yintercept = 0, color = "grey30", lwd = 1) +
                            geom_vline(xintercept = 0, color = "grey30", lwd = 1) +
                            geom_point(aes_string(shape = "det", size = "det"), alpha = 0.5) +
                            geom_path(data = PCO.li.ELL, aes_string(x = "x", y = "y"), lty = 2) +
                            geom_path(data = PCO.li.ELL.det, aes_string(x = "x", y = "y")) +
                            geom_label_repel(data = unique(PCO.li.ELL[, c("xlabel", "ylabel", "PFG")])
                                             , aes_string(x = "xlabel", y = "ylabel", label = "PFG")) +
                            scale_shape_manual(guide = F, values = c("0" = 8, "1" = 20)) +
                            scale_size_manual(guide = F, values = c("0" = 3, "1" = 1)) +
                            scale_color_discrete(guide = F) +
                            labs(x = paste0("\n1st axis = ", round(inert[1], 1), "% of inertia")
                                 , y = paste0("2nd axis = ", round(inert[2], 1), "% of inertia\n")
                                 , title = paste0("STEP C : Removal of distant species : group ", group),
                                 subtitle = paste0("Only species whose mean distance to other species is included in the distribution\n",
                                                   "of all PFG's species mean distances to other species are kept.\n",
                                                   "Species indicated with * will be removed from PFGs.\n",
                                                   "Inertia ellipse are represented, with (dashed) and without (solid) non-determinant species.")) +
                            theme_fivethirtyeight() +
                            theme(panel.background = element_rect(fill = "transparent", colour = NA)
                                  , plot.background = element_rect(fill = "transparent", colour = NA)
                                  , legend.background = element_rect(fill = "transparent", colour = NA)
                                  , legend.box.background = element_rect(fill = "transparent", colour = NA)
                                  , legend.key = element_rect(fill = "transparent", colour = NA)
                                  , axis.title = element_text(inherit.blank = FALSE))
                          
                          return(pp4)
                        }
                      names(pp4_list) = group_names
                      return(c(list(pp3), pp4_list))
                    }
                    ## ---------------------------------------------------------------------------------------------------------- ##
                    , clust3 = {
                      mat.traits.pfg = tab
                      mat.species.traits = sp.pfg.traits
                      melt.vars = c("PFG")
                      if ("type" %in% colnames(tab)) {
                        melt.vars = c("PFG", "type")
                      }
                      
                      mat.species.traits.melt = melt(mat.species.traits, id.vars = c("species", melt.vars))
                      mat.species.traits.melt$value = as.numeric(as.character(mat.species.traits.melt$value))
                      
                      mat.traits.pfg.melt = melt(mat.traits.pfg, id.vars = melt.vars)
                      mat.traits.pfg.melt$value = as.numeric(as.character(mat.traits.pfg.melt$value))
                      
                      #################################################################################################
                      ## Longevity - Maturity graph
                      if ("longevity" %in% colnames(tab) && "maturity" %in% colnames(tab))
                      {
                        cat("\n >> Longevity and maturity...")
                        
                        ind.keep.sp = which(mat.species.traits.melt$variable %in% c("longevity", "maturity"))
                        ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("longevity", "maturity"))
                        
                        pp.1 = ggplot(mat.species.traits.melt[ind.keep.sp,]
                                      , aes_string(x = "PFG", y = "value", fill = "variable")) +
                          geom_boxplot(color = "grey60") +
                          geom_segment(data = mat.traits.pfg
                                       , aes_string(x = "PFG"
                                                    , xend = "PFG"
                                                    , y = "maturity"
                                                    , yend = "longevity")
                                       , color = "#525252"
                                       , lwd = 1
                                       , inherit.aes = FALSE) +
                          geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
                                     , aes_string(x = "PFG"
                                                  , y = "value"
                                                  , color = "variable")
                                     , size = 2
                                     , inherit.aes = FALSE) +
                          scale_y_log10() +
                          scale_fill_manual(guide = FALSE
                                            , values = c("longevity" = "#ffffff"
                                                         , "maturity" = "#ffffff")) +
                          scale_color_manual(""
                                             , values = c("longevity" = "#377eb8"
                                                          , "maturity" = "#ff7f00")) +
                          labs(x = "", y = "", title = "STEP D : Computation of PFG traits values : longevity & maturity"
                               , subtitle = paste0("PFG traits values are calculated as the average of the PFG determinant species traits values.\n"
                                                   , "If the trait is factorial or categorical, median value is taken.\n"
                                                   , "Light-grey boxplot represent determinant species values.\n"
                                                   , "Colored points represent the PFG calculated values.\n\n"
                                                   , "If there is no values for longevity within one PFG, and some maturity values are available,\n"
                                                   , "some values are inferred as maturity * 2.\n"
                                                   , "If there is no values for maturity within one PFG, and some longevity values are available,\n"
                                                   , "some values are inferred as longevity / 2.\n")) +
                          theme_fivethirtyeight() +
                          theme(panel.background = element_rect(fill = "transparent", colour = NA)
                                , plot.background = element_rect(fill = "transparent", colour = NA)
                                , legend.background = element_rect(fill = "transparent", colour = NA)
                                , legend.box.background = element_rect(fill = "transparent", colour = NA)
                                , legend.key = element_rect(fill = "transparent", colour = NA))
                        
                      } else
                      {
                        pp.1 = NULL
                      }
                      
                      #################################################################################################
                      ## Soil contribution - tolerance
                      if ("soil" %in% colnames(tab))
                      {
                        cat("\n >> Soil contribution and tolerance...")
                        
                        ind.keep.sp = which(mat.species.traits.melt$variable %in% c("soil_contrib", "soil_tol_min", "soil_tol_max"))
                        ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("soil_contrib", "soil_tol_min", "soil_tol_max"))
                        
                        pp.2 = ggplot(mat.species.traits.melt[ind.keep.sp,]
                                      , aes_string(x = "PFG"
                                                   , y = "value"
                                                   , fill = "factor(variable
                                               , c('soil_tol_min'
                                                   , 'soil_contrib'
                                                   , 'soil_tol_max'))")) +
                          geom_boxplot(color = "grey60") +
                          geom_linerange(data = mat.traits.pfg
                                         , aes_string(x = "as.numeric(PFG) + 0.5"
                                                      , ymin = "soil_tol_min"
                                                      , ymax = "soil_tol_max")
                                         , color = "#525252"
                                         , lwd = 1
                                         , inherit.aes = FALSE) +
                          geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
                                     , aes_string(x = "as.numeric(PFG) + 0.5"
                                                  , y = "value"
                                                  , color = "factor(variable
                                               , c('soil_tol_min'
                                                   , 'soil_contrib'
                                                   , 'soil_tol_max'))")
                                     , size = 2
                                     , inherit.aes = FALSE) +
                          scale_y_continuous(breaks = 1:max(as.numeric(as.character(mat.traits.pfg$soil_tol_max)), na.rm = TRUE)) +
                          scale_fill_manual(guide = FALSE
                                            , values = c("soil_tol_min" = "#ffffff"
                                                         , "soil_contrib" = "#ffffff"
                                                         , "soil_tol_max" = "#ffffff")) +
                          scale_color_manual(""
                                             , values = c("soil_tol_min" = "#ec7014"
                                                          , "soil_contrib" = "#b15928"
                                                          , "soil_tol_max" = "#cb181d")) +
                          labs(x = "", y = "", title = "STEP D : Computation of PFG traits values : soil contribution & tolerance"
                               , subtitle = paste0("PFG traits values are calculated as the average of the PFG determinant species traits values.\n"
                                                   , "If the trait is factorial or categorical, median value is taken.\n"
                                                   , "Light-grey boxplot represent determinant species values.\n"
                                                   , "Colored points represent the PFG calculated values.\n\n")) +
                          theme_fivethirtyeight() +
                          theme(panel.background = element_rect(fill = "transparent", colour = NA)
                                , plot.background = element_rect(fill = "transparent", colour = NA)
                                , legend.background = element_rect(fill = "transparent", colour = NA)
                                , legend.box.background = element_rect(fill = "transparent", colour = NA)
                                , legend.key = element_rect(fill = "transparent", colour = NA))
                        
                      } else
                      {
                        pp.2 = NULL
                      }
                      
                      #################################################################################################
                      ## Height - Light
                      if ("height" %in% colnames(tab) && "light" %in% colnames(tab))
                      {
                        cat("\n >> Height and light...")
                        
                        ## Height panel
                        ind.keep.sp = which(mat.species.traits.melt$variable %in% c("height"))
                        ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("height"))
                        
                        pp.3.1 = ggplot(mat.species.traits.melt[ind.keep.sp,]
                                        , aes_string(x = "PFG", y = "value")) +
                          geom_boxplot(color = "grey60") +
                          geom_point(data = mat.traits.pfg.melt[ind.keep.pfg,]
                                     , aes_string(x = "PFG"
                                                  , y = "value"
                                                  , color = "variable")
                                     , size = 2
                                     , inherit.aes = FALSE) +
                          scale_color_manual(guide = F
                                             , values = c("height" = "#238b45"
                                                          , "light" = "#1d91c0")) +
                          scale_y_continuous(breaks = c(1, 10, 100, 1000, 5000)
                                             , trans = "log") +
                          labs(x = "", y = "", title = "STEP D : Computation of PFG traits values : height & light"
                               , subtitle = paste0("PFG traits values are calculated as the average of the PFG determinant species traits values.\n"
                                                   , "If the trait is factorial or categorical, median value is taken.\n"
                                                   , "Light-grey boxplot represent determinant species values.\n"
                                                   , "Colored points represent the PFG calculated values.\n\n")) +
                          theme_fivethirtyeight() +
                          theme(axis.text.x = element_blank()
                                , panel.background = element_rect(fill = "transparent", colour = NA)
                                , plot.background = element_rect(fill = "transparent", colour = NA)
                                , legend.background = element_rect(fill = "transparent", colour = NA)
                                , legend.box.background = element_rect(fill = "transparent", colour = NA)
                                , legend.key = element_rect(fill = "transparent", colour = NA))
                        
                        ## Light panel
                        ind.keep.sp = which(mat.species.traits.melt$variable %in% c("light"))
                        ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("light"))
                        
                        pp.3.2 = ggplot(mat.species.traits.melt[ind.keep.sp,]
                                        , aes_string(x = "PFG", y = "value")) +
                          geom_boxplot(color = "grey60") +
                          geom_point(data = mat.traits.pfg.melt[ind.keep.pfg,]
                                     , aes_string(x = "PFG"
                                                  , y = "value"
                                                  , color = "factor(variable, c('height', 'light'))")
                                     , size = 2
                                     , inherit.aes = FALSE) +
                          scale_color_manual(""
                                             , values = c("height" = "#238b45"
                                                          , "light" = "#1d91c0")
                                             , drop = FALSE) +
                          scale_y_continuous(breaks = 1:max(as.numeric(as.character(mat.traits.pfg$light)), na.rm = TRUE)) +
                          labs(x = "", y = "") +
                          theme_fivethirtyeight() +
                          theme(axis.text.y = element_text(margin = margin(t = 0
                                                                           , r = 2
                                                                           , b = 0
                                                                           , l = 0
                                                                           , unit = "lines"))
                                , panel.background = element_rect(fill = "transparent", colour = NA)
                                , plot.background = element_rect(fill = "transparent", colour = NA)
                                , legend.background = element_rect(fill = "transparent", colour = NA)
                                , legend.box.background = element_rect(fill = "transparent", colour = NA)
                                , legend.key = element_rect(fill = "transparent", colour = NA))
                        
                        pp.3 = list(pp.3.1, pp.3.2)
                        
                      } else
                      {
                        pp.3 = NULL
                      }
                      
                      #################################################################################################
                      ## Dispersal graph
                      if ("dispersal" %in% colnames(tab))
                      {
                        cat("\n >> Dispersal...")
                        
                        ind.keep.sp = which(mat.species.traits.melt$variable %in% c("dispersal"))
                        ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("dispersal"))
                        
                        pp.4 = ggplot(mat.species.traits.melt[ind.keep.sp,]
                                      , aes_string(x = "PFG", y = "value")) +
                          geom_boxplot(color = "grey60") +
                          geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
                                     , aes_string(x = "PFG"
                                                  , y = "value"
                                                  , color = "variable")
                                     , size = 2
                                     , inherit.aes = FALSE) +
                          scale_y_continuous(breaks = 1:max(as.numeric(as.character(mat.traits.pfg$dispersal)), na.rm = TRUE)) +
                          scale_color_manual("", values = c("dispersal" = "#88419d")) +
                          labs(x = "", y = "", title = "STEP D : Computation of PFG traits values : dispersal"
                               , subtitle = paste0("PFG traits values are calculated as the average of the PFG determinant species traits values.\n"
                                                   , "If the trait is factorial or categorical, median value is taken.\n"
                                                   , "Light-grey boxplot represent determinant species values.\n"
                                                   , "Colored points represent the PFG calculated values.\n\n")) +
                          theme_fivethirtyeight() +
                          theme(panel.background = element_rect(fill = "transparent", colour = NA)
                                , plot.background = element_rect(fill = "transparent", colour = NA)
                                , legend.background = element_rect(fill = "transparent", colour = NA)
                                , legend.box.background = element_rect(fill = "transparent", colour = NA)
                                , legend.key = element_rect(fill = "transparent", colour = NA))
                        
                      } else
                      {
                        pp.4 = NULL
                      }
                      
                      #################################################################################################
                      ## Palatability graph
                      if ("palatability" %in% colnames(tab))
                      {
                        cat("\n >> Palatability...")
                        
                        ind.keep.sp = which(mat.species.traits.melt$variable %in% c("palatability"))
                        ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("palatability"))
                        
                        pp.5 = ggplot(mat.species.traits.melt[ind.keep.sp,]
                                      , aes_string(x = "PFG", y = "value")) +
                          geom_boxplot(color = "grey60") +
                          geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
                                     , aes_string(x = "PFG"
                                                  , y = "value"
                                                  , color = "variable")
                                     , size = 2
                                     , inherit.aes = FALSE) +
                          scale_y_continuous(breaks = 1:max(as.numeric(as.character(mat.traits.pfg$palatability)), na.rm = TRUE)) +
                          scale_color_manual("", values = c("palatability" = "#02818a")) +
                          labs(x = "", y = "", title = "STEP D : Computation of PFG traits values : palatability"
                               , subtitle = paste0("PFG traits values are calculated as the average of the PFG determinant species traits values.\n"
                                                   , "If the trait is factorial or categorical, median value is taken.\n"
                                                   , "Light-grey boxplot represent determinant species values.\n"
                                                   , "Colored points represent the PFG calculated values.\n\n")) +
                          theme_fivethirtyeight() +
                          theme(panel.background = element_rect(fill = "transparent", colour = NA)
                                , plot.background = element_rect(fill = "transparent", colour = NA)
                                , legend.background = element_rect(fill = "transparent", colour = NA)
                                , legend.box.background = element_rect(fill = "transparent", colour = NA)
                                , legend.key = element_rect(fill = "transparent", colour = NA))
                        
                      } else
                      {
                        pp.5 = NULL
                      }
                      
                      #################################################################################################
                      
                      pp_list = list()
                      if (sum(unlist(lapply(list(pp.1, pp.2, pp.3, pp.4, pp.5), is.null))) < 5)
                      {
                        if (!is.null(pp.1)) pp_list[[length(pp_list) + 1]] = pp.1
                        if (!is.null(pp.2)) pp_list[[length(pp_list) + 1]] = pp.2
                        if (!is.null(pp.3)) pp_list[[length(pp_list) + 1]] = grid.arrange(grobs = pp.3
                                                                                          , layout_matrix = matrix(c(1, 1, 1, 2, 2)
                                                                                                                   , ncol = 1
                                                                                                                   , byrow = TRUE))
                        if (!is.null(pp.4)) pp_list[[length(pp_list) + 1]] = pp.4
                        if (!is.null(pp.5)) pp_list[[length(pp_list) + 1]] = pp.5
                      }
                      return(pp_list)
                    }
        )
      }
    }
  
  if (is.ggplot(pp) || (is.list(pp) && length(pp) > 0))
  {
    # if (is.ggplot(pp))
    # {
    #   RV$compt.browser.pfg <- 1
    #   RV$compt.browser.pfg.max <- 1
    #   
    #   shinyjs::hide("pfg.go.left")
    #   shinyjs::hide("pfg.go.right")
    #   
    #   output$UI.pfg.browser = renderUI({
    #     plotOutput(outputId = "pfg.browser", width = "100%", height = "600px")
    #   })
    #   
    #   output$pfg.browser = renderPlot({ pp })
    # } else
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
      
      # for(i in 1:length(pp)){
      #   shinyjs::hideElement(paste0("pfg.browser_", i))
      # }
      
      RV$compt.browser.pfg.max <- length(pp)
      RV$compt.browser.pfg <- 1
      
      print("HEHOOO")

    }
  }
})

####################################################################

# observe({
#   for (i in 1:RV$compt.browser.pfg.max)
#   {
#   shinyjs::toggleState(paste0("pfg.browser_", i), !is.null(input[[paste0("pfg.browser_", i)]]) && i != RV$compt.browser.pfg)
#   }
# })

# ttt  =reactive({
#   lapply(1:RV$compt.browser.pfg.max, function(i) {
#   input[[paste0("pfg.browser_", i)]]
#   })
# })

# update_graphs = eventReactive(list(RV$compt.browser.pfg
#                                    , RV$compt.browser.pfg.max), {
  # req(grep(pattern = "pfg.browser_", x = names(input), value = TRUE))

observeEvent(list(RV$compt.browser.pfg, RV$compt.browser.pfg.max), {
  
  print("YOUHUUUUUUUUUUUUU")
  print(RV$compt.browser.pfg)
  print(RV$compt.browser.pfg.max)
  
  for (i in 1:RV$compt.browser.pfg.max)
  {
    if (i == RV$compt.browser.pfg)
    {
      shinyjs::show(paste0("pfg.browser_", i))
    } else
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



