

####################################################################

# output$plot.evolutionCoverage = renderPlot({
plots_1_2 = observeEvent(input$create.evolutionCoverage, {
  
  path.init = getwd()
  path.folder = dirname(sub("PARAM_SIMUL", "", dirname(input$graph.simulParam)))
  setwd(path.folder)
  
  name.simul = basename(sub("PARAM_SIMUL", "", dirname(input$graph.simulParam)))
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_evolutionCoverage(name.simulation = name.simul
                                        , file.simulParam = input$graph.simulParam
                                        , no.years = input$graph.no.years
                                        , opt.abund_fixedScale = input$graph.opt.fixedScale
                                        , opt.ras_habitat = input$graph.opt.ras_habitat
                                        , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  if(get_res)
  {
    dir.save = .getParam(params.lines = input$graph.simulParam
                         , flag = "SAVE_DIR"
                         , flag.split = "^--.*--$"
                         , is.num = FALSE)
    
    col_vec = c('#6da34d', '#297373', '#58a4b0', '#5c4742', '#3f334d')
    col_fun = colorRampPalette(col_vec)
    
    distri.melt = fread(paste0(sub("PARAM_SIMUL", "", dirname(input$graph.simulParam))
                               , "/RESULTS/POST_FATE_evolution_spaceOccupancy_"
                               , basename(dir.save)
                               , ".csv"))
    
    distriAbund.melt = fread(paste0(sub("PARAM_SIMUL", "", dirname(input$graph.simulParam))
                                    , "/RESULTS/POST_FATE_evolution_abundance_"
                                    , basename(dir.save)
                                    , ".csv"))
    
    output$plot.evolutionCoverage1 = renderPlot({
      
      ## Evolution of space occupation
      pp1 = ggplot(distri.melt, aes_string(x = "YEAR", y = "Abund * 100", color = "factor(HAB)")) +
        geom_line(lwd = 1) +
        facet_wrap("~ PFG") +
        scale_color_manual("Habitat", values = col_fun(length(unique(distri.melt$HAB)))) +
        labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' space occupation"),
             subtitle = paste0("For each PFG, the line represents the evolution through time of its space occupancy,\n",
                               "meaning the percentage of pixels in which the abundance of the species is greater than 0.\n")) +
        theme_fivethirtyeight() +
        theme(panel.background = element_rect(fill = "transparent", colour = NA)
              , plot.background = element_rect(fill = "transparent", colour = NA)
              , legend.background = element_rect(fill = "transparent", colour = NA)
              , legend.box.background = element_rect(fill = "transparent", colour = NA)
              , legend.key = element_rect(fill = "transparent", colour = NA))
      
      print(pp1)
    })
    
    output$plot.evolutionCoverage2 = renderPlot({
      
      ## Evolution of abundance
      pp2 = ggplot(distriAbund.melt, aes_string(x = "YEAR", y = "Abund", color = "HAB")) +
        geom_line(lwd = 1) +
        facet_wrap("~ PFG", scales = ifelse(input$graph.opt.fixedScale, "fixed", "free_y")) +
        scale_color_manual("Habitat", values = col_fun(length(unique(distri.melt$HAB)))) +
        labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' abundance"),
             subtitle = paste0("For each PFG, the line represents the evolution through time of its abundance\n",
                               "over the whole studied area, meaning the sum of its abundances in every pixel.\n")) +
        theme_fivethirtyeight() +
        theme(panel.background = element_rect(fill = "transparent", colour = NA)
              , plot.background = element_rect(fill = "transparent", colour = NA)
              , legend.background = element_rect(fill = "transparent", colour = NA)
              , legend.box.background = element_rect(fill = "transparent", colour = NA)
              , legend.key = element_rect(fill = "transparent", colour = NA))
      
      print(pp2)
    })
  }
  setwd(path.init)
})

# output$plot.evolutionCoverage = renderPlot({
#   print(plots_1_2[[1]])
# })
