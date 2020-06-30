
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


observeEvent(RV$pfg.graph, {

  levels_graph.type = unique(RV$pfg.graph)
  levels_graph.type = factor(levels_graph.type, c("dom", "dist", "clust1", "clust2", "clust3"))
  levels_graph.type = sort(levels_graph.type)
  pp = foreach(graph.type = levels_graph.type, .combine ="c") %do%
    {
      graph.type = as.character(graph.type)
      tab = switch(graph.type
                   , dom = {
                     fi = paste0("PRE_FATE_DOMINANT_TABLE_complete"
                                 , get_dom_param()
                                 , ".csv")
                     if (file.exists(fi))
                     {
                       fread(fi, stringsAsFactors = FALSE)
                     }
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
        colRamp = colorRampPalette(c('#8e0152','#c51b7d','#de77ae','#7fbc41','#4d9221','#276419'))
        
        pp = switch(graph.type
                    ## ---------------------------------------------------------------------------------------------------------- ##
                    , dom = {
                      ## Get selected species and corresponding observations
                      RULES.sel = tab[which(tab$SELECTION != "Not selected"), ]
                      
                      ## Transform results of selection rules into distance matrix
                      mat.dist = RULES.sel[, -which(colnames(RULES.sel) %in% c("species", "SELECTION"))]
                      mat.dist = as.matrix(mat.dist)
                      mat.dist[] = as.numeric(mat.dist[])
                      mat.dist = as.dist(gowdis(mat.dist))
                      if (!is.euclid(mat.dist)) mat.dist = quasieuclid(mat.dist)
                      
                      ## Transform distance matrix into PCO -----------------------------------
                      PCO = dudi.pco(mat.dist, scannf = FALSE, nf = 3) ## PCO
                      
                      if (ncol(PCO$li) > 1)
                      {
                        PCO.li = PCO$li
                        PCO.li$PFG = factor(RULES.sel$SELECTION)
                        PCO.li$selected = RULES.sel$A1
                        ind_sel = which(PCO.li$selected == TRUE)
                        
                        ## GET inertia values
                        inert = inertia.dudi(PCO)$tot.inertia
                        inert = c(inert$`cum(%)`[1]
                                  , inert$`cum(%)`[2] - inert$`cum(%)`[1]
                                  , inert$`cum(%)`[3] - inert$`cum(%)`[2])
                        
                        ## --------------------------------------------------------------------
                        num.axis = 2:length(which(!is.na(inert)))
                        pp_pco.list = foreach(i.axis = num.axis) %do%
                        {
                          ## GET ellipses when A1 = TRUE
                          PCO.ELL = .getELLIPSE(xy = PCO.li[ind_sel, c("A1", paste0("A", i.axis))]
                                                , fac = PCO.li$PFG[ind_sel])
                          PCO.ELL$selected = TRUE
                          PCO.pts = merge(PCO.li[ind_sel, ]
                                          , unique(PCO.ELL[, c("xlabel", "ylabel", "PFG")])
                                          , by = "PFG")
                          PCO.pts$selected = TRUE
                          labels.ELL = unique(PCO.ELL[, c("xlabel", "ylabel", "PFG", "selected")])
                          
                          ## GET ellipses when A1 = FALSE
                          if (length(unique(PCO.li$PFG[-ind_sel])) > 1)
                          {
                            tmp.ELL = .getELLIPSE(xy = PCO.li[-ind_sel, c("A1", paste0("A", i.axis))]
                                                  , fac = PCO.li$PFG[-ind_sel])
                            tmp.ELL$selected = FALSE
                            tmp.pts = merge(PCO.li[-ind_sel, ]
                                            , unique(tmp.ELL[, c("xlabel", "ylabel", "PFG")])
                                            , by = "PFG")
                            tmp.pts$selected = FALSE
                            
                            PCO.ELL = rbind(PCO.ELL, tmp.ELL)
                            PCO.pts = rbind(PCO.pts, tmp.pts)
                          }
                          
                          
                          pp_pco = ggplot(PCO.li, aes_string(x = "A1"
                                                             , y = paste0("A", i.axis)
                                                             , color = "PFG"
                                                             , alpha = "as.numeric(selected)"
                                                             , linetype = "selected")) +
                            geom_point() +
                            geom_hline(yintercept = 0, color = "grey30", lwd = 1) +
                            geom_vline(xintercept = 0, color = "grey30", lwd = 1) +
                            geom_segment(data = PCO.pts, aes_string(xend = "xlabel"
                                                                    , yend = "ylabel"
                                                                    , size = "selected")) +
                            geom_path(data = PCO.ELL, aes_string(x = "x", y = "y", size = "selected")) +
                            geom_label_repel(data = labels.ELL, aes_string(x = "xlabel"
                                                                           , y = "ylabel"
                                                                           , label = "PFG")) +
                            scale_y_continuous(position = "right", labels = NULL
                                               , sec.axis = sec_axis(~ . + 0)) +
                            scale_color_manual(guide = FALSE, values = pal_col) +
                            scale_alpha(guide = FALSE, range = c(0.2, 1)) +
                            scale_linetype_manual(guide = FALSE, values = c("TRUE" = 1, "FALSE" = 2)) +
                            scale_size_manual(guide = FALSE, values = c("TRUE" = 0.8, "FALSE" = 0.5)) +
                            labs(x = paste0("\nAXIS 1 = ", round(inert[1], 1), "% of inertia")
                                 , y = paste0("AXIS ", i.axis, " = ", round(inert[i.axis], 1), "% of inertia\n")
                                 , title = "STEP 2 : Selected dominant species"
                                 , subtitle = paste0("Colors highlight the rules of selection.\n"
                                                     , "Species not meeting any criteria or only A1 have been removed.\n"
                                                     , "Priority has been set to A2, B1 and B2 rules, rather than C. \n"
                                                     , "Hence, species selected according to A2, B1 and/or B2 can also meet criterion C\n"
                                                     , "while species selected according to C do not meet any of the three criteria.\n"
                                                     , "Species selected according to one (or more) criterion but not meeting criterion A1 are transparent."
                                                     , "\n")) +
                            .getGraphics_theme() +
                            theme(axis.title = element_text(size = 12))
                          
                          return(pp_pco)
                        }
                        names(pp_pco.list) = paste0("Axis1_Axis", num.axis)
                        pp_pco.list = pp_pco.list[names(pp_pco.list)[which(sapply(pp_pco.list, is.null) == FALSE)]]
                        return(pp_pco.list)
                      }
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
                      combi = expand.grid(GROUP = group_names
                                          , variable = unique(clust.evaluation$variable))
                      
                      clust.evaluation.optim = foreach(group = combi$GROUP, variable = combi$variable) %do%
                      {
                        tmp = clust.evaluation[which(clust.evaluation$GROUP == group &
                                                       clust.evaluation$variable == variable),]
                        if(variable == "mVI")
                        {
                          optim = unique(sort(tmp$value, decreasing = F))[1:3]
                          ind.optim = which(tmp$value %in% optim)
                        } else {
                          optim = unique(sort(tmp$value, decreasing = T))[1:3]
                          ind.optim = which(tmp$value %in% optim)
                        }
                        optim.clust = tmp$no.clusters[ind.optim]
                        optim.val = tmp$value[ind.optim]
                        return(data.frame(GROUP = group
                                          , variable
                                          , optim.clust
                                          , optim.val
                                          , stringsAsFactors = FALSE))
                      }
                      clust.evaluation.optim = do.call(rbind, clust.evaluation.optim)
                      
                      ## GRAPHICAL REPRESENTATION
                      
                      pp2 = ggplot(clust.evaluation, aes_string(x = "no.clusters", y = "value")) +
                        facet_grid("variable ~ GROUP", scales = "free") +
                        geom_line() +
                        geom_point() +
                        geom_vline(data = clust.evaluation.optim
                                   , aes_string(xintercept = "optim.clust", color = "group")
                                   , lwd = 4
                                   , alpha = 0.3) +
                        scale_color_manual(guide = F, values = colRamp(length(group_names))) +
                        labs(x = "", y = ""
                             , title = "STEP B : Choice of number of clusters"
                             , subtitle = paste0("Evolution of clustering evaluation variables with "
                                                 , "the number of clusters in each group.\n"
                                                 , "All values except that of mVI must be maximized "
                                                 , "(check function's help for more details about the measures).\n"
                                                 , "The number of clusters with values among the 3 best are highlighted.")) +
                        .getGraphics_theme()
                      
                      return(list(pp2))
                    }
                    ## ---------------------------------------------------------------------------------------------------------- ##
                    , clust2 = {
                      determ = tab$determ.all
                      colLev = levels(interaction(determ$toSuppr, determ$group))
                      group_names = unique(determ$group)
                      
                      pp3 = ggplot(determ, aes_string(x = "pfg", y = "sp.mean.dist"
                                                      , color = interaction(determ$DETERMINANT, determ$GROUP)
                                                      , shape = "DETERMINANT")) +
                        scale_color_manual(guide = F, values = colRamp(length(colLev))) +
                        scale_shape_manual(guide = F, values = c("0" = 20, "1" = 8)) +
                        geom_errorbar(aes_string(ymin = "allSp.min", ymax = "allSp.max")
                                      , color = "darkblue") +
                        geom_point(position = "jitter") +
                        geom_point(aes_string(y = "allSp.mean")
                                   , pch = 18
                                   , lwd = 5
                                   , color = "darkblue") +
                        facet_grid("~ GROUP", scales = "free_x") +
                        labs(x = "", y = "Mean distance to other species"
                             , title = "STEP C : Removal of distant species"
                             , subtitle = paste0("Only species whose mean distance to other species "
                                                 , "is included in the distribution\n"
                                                 , "of all PFG's species mean distances to other species are kept.\n"
                                                 , "Species indicated with * will be removed from PFGs.\n"
                                                 , "Non-represented PFG might be one-species-only.")) +
                        .getGraphics_theme() +
                        theme(axis.ticks.x = element_blank()
                              , axis.text.x = element_text(angle = 90))
                      
                      ## GRAPHICAL REPRESENTATION 2
                      ## Compute Principal Coordinates Analysis (PCO) for the determinantes of each group
                      ## to see the position in "distance space" (overlap + traits) between species and groups
                      
                      pp4_list = foreach(group = group_names) %do%
                      {
                        ## Transform results of clustering into distance matrix
                        tmp = determ[which(determ$GROUP == group),]
                        mat = mat.species.DIST[[group]]
                        mat = quasieuclid(as.dist(mat))
                        
                        ## Transform distance matrix into PCO
                        PCO = dudi.pco(mat, scannf = FALSE, nf = 3) ## PCO
                        PCO.li = PCO$li
                        PCO.li$det = ifelse(rownames(PCO.li) %in% tmp$sp[which(tmp$DETERMINANT == FALSE)], 0, 1)
                        PCO.li$det = factor(PCO.li$det, c(0, 1))
                        PCO.li$PFG = clust.groups[rownames(PCO.li)]
                        
                        ## GET inertia values
                        inert = inertia.dudi(PCO)$tot.inertia
                        inert = c(inert$`cum(%)`[1]
                                  , inert$`cum(%)`[2] - inert$`cum(%)`[1]
                                  , inert$`cum(%)`[3] - inert$`cum(%)`[2])
                        
                        ## GET ellipses
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
                               , title = paste0("STEP C : Removal of distant species : group ", group)
                               , subtitle = paste0("Only species whose mean distance to other species "
                                                   , "is included in the distribution\n"
                                                   , "of all PFG's species mean distances to other species are kept.\n"
                                                   , "Species indicated with * will be removed from PFGs.\n"
                                                   , "Inertia ellipse are represented, with (dashed) and "
                                                   , "without (solid) non-determinant species.")) +
                          .getGraphics_theme() +
                          theme(axis.title = element_text(inherit.blank = FALSE))
                        
                        plot(pp4)
                        return(pp4)
                      }
                      names(pp4_list) = group_names
                      return(c(list(pp3), pp4_list))
                    }
                    ## ---------------------------------------------------------------------------------------------------------- ##
                    , clust3 = {
                      mat.traits.pfg = as.data.frame(tab)
                      mat.species.traits = as.data.frame(sp.pfg.traits)
                      melt.vars = c("PFG")
                      if ("type" %in% colnames(tab)) {
                        melt.vars = c("PFG", "type")
                      }
                      
                      mat.traits.melt = mat.traits
                      for (i in ind.factor) mat.traits.melt[, i] = as.numeric(as.factor(mat.traits.melt[, i]))
                      mat.traits.melt = melt(mat.traits.melt, id.vars = c("species", "PFG"))
                      mat.traits.melt$variable = as.character(mat.traits.melt$variable)
                      mat.traits.melt$variable = factor(mat.traits.melt$variable, names_traits.factor)
                      
                      mat.traits.pfg.melt = mat.traits.pfg
                      for (i in ind.factor) mat.traits.pfg.melt[, i] = as.numeric(as.factor(mat.traits.pfg.melt[, i]))
                      mat.traits.pfg.melt = melt(mat.traits.pfg.melt, id.vars = c("no.species", "PFG"))
                      mat.traits.pfg.melt$variable = as.character(mat.traits.pfg.melt$variable)
                      mat.traits.pfg.melt$variable = factor(mat.traits.pfg.melt$variable, names_traits.factor)
                      
                      #############################################################################
                      
                      pp.i = function(i.trait, i.title, i.color
                                      , i.segment = NULL, i.facet = FALSE)
                      {
                        ind.keep.sp = which(mat.traits.melt$variable %in% i.trait)
                        ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% i.trait)
                        
                        
                        pp.i = ggplot(mat.traits.melt[ind.keep.sp,]
                                      , aes_string(x = "PFG", y = "value", fill = "variable")) +
                          geom_boxplot(color = "grey60", na.rm = TRUE)
                        
                        if (!is.null(i.segment)) {
                          pp.i = pp.i +
                            geom_segment(data = mat.traits.pfg
                                         , aes_string(x = "PFG"
                                                      , xend = "PFG"
                                                      , y = i.segment[1]
                                                      , yend = i.segment[2])
                                         , color = "#525252"
                                         , lwd = 1
                                         , inherit.aes = FALSE)
                        }
                        
                        if (i.facet) {
                          pp.i = pp.i + facet_wrap(~ variable, ncol = 1, scales = "free_y")
                        }
                        
                        pp.i = pp.i +
                          geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
                                     , aes_string(x = "PFG"
                                                  , y = "value"
                                                  , color = "variable")
                                     , size = 2
                                     , inherit.aes = FALSE) +
                          scale_color_manual("", values = i.color) +
                          scale_fill_manual(guide = FALSE, values = rep("#FFFFFF", length(i.trait))) +
                          labs(x = "", y = ""
                               , title = paste0("STEP D : Computation of PFG traits values : ", i.title)
                               , subtitle = paste0("PFG traits values are calculated as the average of "
                                                   , "the PFG determinant species traits values.\n"
                                                   , "If the trait is factorial or categorical, median value is taken.\n"
                                                   , "Light-grey boxplot represent determinant species values.\n"
                                                   , "Colored points represent the PFG calculated values.\n\n")) +
                          .getGraphics_theme()
                        
                        return(pp.i)
                      }
                      
                      pp_list = list()
                      
                      #############################################################################
                      if (isThere.longevity && isThere.maturity)
                      {
                        cat("\n> Longevity and maturity...")
                        pp_list$maturity_longevity = pp.i(i.trait = c("longevity", "maturity")
                                                          , i.title = "longevity & maturity"
                                                          , i.color = c("longevity" = "#377eb8", "maturity" = "#ff7f00")
                                                          , i.segment = c("longevity", "maturity"))
                        pp_list$maturity_longevity = suppressMessages(pp_list$maturity_longevity +
                                                                        scale_y_log10())
                        plot(pp_list$maturity_longevity)
                        
                        indSuppr = which(names_traits.factor %in% c("maturity", "longevity"))
                        names_traits.factor = names_traits.factor[-indSuppr]
                      }
                      
                      #############################################################################
                      if (isThere.height && isThere.light)
                      {
                        cat("\n> Height and light...")
                        pp_list$height_light = pp.i(i.trait = c("height", "light")
                                                    , i.title = "height & light"
                                                    , i.color = c("height" = "#238b45", "light" = "#1d91c0")
                                                    , i.facet = TRUE)
                        pp_list$height_light = suppressMessages(pp_list$height_light + scale_y_log10())
                        pp_list$height_light = pp_list$height_light + theme(strip.text = element_blank())
                        plot(pp_list$height_light)
                        
                        indSuppr = which(names_traits.factor %in% c("height", "light"))
                        names_traits.factor = names_traits.factor[-indSuppr]
                      }
                      
                      #############################################################################
                      if ((isThere.soil_contrib && isThere.soil_tolerance) ||
                          (isThere.soil_contrib && isThere.soil_tol_min && isThere.soil_tol_max))
                      {
                        cat("\n> Soil contribution and tolerance...")
                        pp_list$soil = pp.i(i.trait = c("soil_tol_min", "soil_contrib", "soil_tol_max")
                                            , i.title = "soil contribution & tolerance"
                                            , i.color = c("soil_tol_min" = "#ec7014"
                                                          , "soil_contrib" = "#b15928"
                                                          , "soil_tol_max" = "#cb181d")
                                            , i.segment = c("soil_tol_min", "soil_tol_max"))
                        plot(pp_list$soil)
                        
                        indSuppr = which(names_traits.factor %in% c("soil_tol_min", "soil_contrib"
                                                                    , "soil_tol_max", "soil_tolerance"))
                        names_traits.factor = names_traits.factor[-indSuppr]
                        
                      }
                      
                      #############################################################################
                      if (length(names_traits.factor) > 0)
                      {
                        for(i.trait in names_traits.factor)
                        {
                          cat(paste0("\n> ", i.trait, "..."))
                          pp_list[[i.trait]] = pp.i(i.trait = i.trait
                                                    , i.title = i.trait
                                                    , i.color = "#02818a")
                          plot(pp_list[[i.trait]])
                          
                        }
                      }
                      
                      #############################################################################
                      return(pp_list)
                    }
        )
      }
    }
  
  if (is.ggplot(pp) || (is.list(pp) && length(pp) > 0))
  {
    shinyjs::show("pfg.go.left")
    shinyjs::show("pfg.go.right")
    shinyjs::disable("pfg.go.left")
    shinyjs::disable("pfg.go.right")
    if (length(pp) > 1)
    {
      shinyjs::enable("pfg.go.left")
    }
    
    output$UI.pfg.browser = renderUI({
      lapply(1:length(pp), function(i) {
        if (i == length(pp)){
          plotOutput(outputId = paste0("pfg.browser_", i), width = "100%", height = "600px")
        } else {
          shinyjs::hidden(plotOutput(outputId = paste0("pfg.browser_", i), width = "100%", height = "600px"))
        }
      })
    })
    lapply(1:length(pp), function(i) {
      output[[paste0("pfg.browser_", i)]] = renderPlot({ plot(pp[[i]]) })
    })

    RV$compt.browser.pfg.max <- length(pp)
    RV$compt.browser.pfg <- length(pp)
  }
})

####################################################################

observeEvent(input$pfg.go.left, {
  if (input$pfg.go.left > 0)
  {
    shinyjs::enable("pfg.go.right")
    RV$compt.browser.pfg <- RV$compt.browser.pfg - 1
    if (RV$compt.browser.pfg == 1)
    {
      shinyjs::disable("pfg.go.left")
    }
    shinyjs::toggle(paste0("pfg.browser_", RV$compt.browser.pfg + 1))
    shinyjs::toggle(paste0("pfg.browser_", RV$compt.browser.pfg))
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
    shinyjs::toggle(paste0("pfg.browser_", RV$compt.browser.pfg - 1))
    shinyjs::toggle(paste0("pfg.browser_", RV$compt.browser.pfg))
  }
})



