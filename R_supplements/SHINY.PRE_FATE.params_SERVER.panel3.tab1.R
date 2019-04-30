

####################################################################

get_path.simul = eventReactive(input$graph.simulParam, {
  return(sub("PARAM_SIMUL", "", dirname(input$graph.simulParam)))
})

get_name.simul = eventReactive(input$graph.simulParam, {
  return(basename(get_path.simul()))
})

get_path.folder = eventReactive(input$graph.simulParam, {
  return(dirname(get_path.simul()))
})


get_last.createdFiles1 = eventReactive(input$graph.simulParam, {
  system(command = paste0("ls -lat "
                          , get_path.simul()
                          , "/RESULTS/"
                          , " | awk '{print $9}'")
         , intern = TRUE)
})

get_last.createdFiles2 = function(pattern_head, pattern_tail)
{
  last.createdFiles = get_last.createdFiles1()
  last.createdFiles = last.createdFiles[grep(pattern = pattern_head, last.createdFiles)]
  last.createdFiles = last.createdFiles[grep(pattern = pattern_tail, last.createdFiles)]
  return(last.createdFiles[1])
}


####################################################################

observeEvent(input$show.evolutionCoverage, {
  shinyjs::show("panel.evolutionCoverage")
  shinyjs::hide("panel.evolutionAbund")
  shinyjs::hide("panel.evolutionLight")
  shinyjs::hide("panel.evolutionSoil")
})

observeEvent(input$create.evolutionCoverage, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_evolutionCoverage(name.simulation = get_name.simul()
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
    
    distri.melt = fread(paste0(get_path.simul()
                               , "/RESULTS/POST_FATE_evolution_spaceOccupancy_"
                               , basename(dir.save)
                               , ".csv"))
    
    distriAbund.melt = fread(paste0(get_path.simul()
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

####################################################################

observeEvent(input$show.evolutionAbund, {
  shinyjs::hide("panel.evolutionCoverage")
  shinyjs::show("panel.evolutionAbund")
  shinyjs::hide("panel.evolutionLight")
  shinyjs::hide("panel.evolutionSoil")
})

observeEvent(input$create.evolutionAbund, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  get_res = print_messages(as.expression(
    POST_FATE.graphic_evolutionAbund_pixels(name.simulation = get_name.simul()
                                            , file.simulParam = input$graph.simulParam
                                            , no.years = input$graph.no.years
                                            , opt.abund_fixedScale = input$graph.opt.fixedScale
                                            , opt.cells_ID = NULL
                                            , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  
  if(get_res)
  {
    vec_col = c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c'
                , '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00'
                , '#cab2d6', '#6a3d9a', '#ffff99', '#b15928')
    fun_col = colorRampPalette(vec_col)
    
    # last.createdFiles = system(command = paste0("ls -lat "
    #                                             , get_path.simul()
    #                                             , "/RESULTS/"
    #                                             , " | awk '{print $9}'")
    #                            , intern = TRUE)
    # last.createdFiles = last.createdFiles[grep("POST_FATE_evolution_abundance_pixels_", last.createdFiles)]
    # last.createdFiles = last.createdFiles[grep(".csv$", last.createdFiles)]
    
    distriAbund = fread(paste0(get_path.simul()
                               , "/RESULTS/"
                               , get_last.createdFiles2(pattern_head = "POST_FATE_evolution_abundance_pixels_"
                                                        , pattern_tail = ".csv$")))
    
    output$plot.evolutionAbund = renderPlot({
      
      ## Evolution of abundance
      pp = ggplot(distriAbund, aes_string(x = "YEAR", y = "Abund", color = "PFG")) +
        scale_color_manual("", values = fun_col(length(unique(distriAbund$PFG)))) +
        geom_line() +
        facet_grid("TYPE ~ ID", scales = ifelse(input$graph.opt.fixedScale, "fixed", "free_y")) +
        labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' abundance"),
             subtitle = paste0("For each PFG, the line represents the evolution through time of its abundance\n",
                               "for 5 randomly selected pixels within the studied area.\n")) +
        theme_fivethirtyeight() +
        theme(panel.background = element_rect(fill = "transparent", colour = NA)
              , plot.background = element_rect(fill = "transparent", colour = NA)
              , legend.background = element_rect(fill = "transparent", colour = NA)
              , legend.box.background = element_rect(fill = "transparent", colour = NA)
              , legend.key = element_rect(fill = "transparent", colour = NA))
      
      print(pp)
    })
  }
  setwd(path.init)
})

####################################################################

observeEvent(input$show.evolutionLight, {
  shinyjs::hide("panel.evolutionCoverage")
  shinyjs::hide("panel.evolutionAbund")
  shinyjs::show("panel.evolutionLight")
  shinyjs::hide("panel.evolutionSoil")
})

####################################################################

observeEvent(input$show.evolutionSoil, {
  shinyjs::hide("panel.evolutionCoverage")
  shinyjs::hide("panel.evolutionAbund")
  shinyjs::hide("panel.evolutionLight")
  shinyjs::show("panel.evolutionSoil")
})

