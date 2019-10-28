
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

observeEvent(list(input$select.dominant
                  , input$compute.distance
                  , input$clustering.step1
                  , input$clustering.step2
                  , input$clustering.step3
), {
  # if (nchar(input$browser.files) > 0)
  # {
  graph.type = "light.pixels"
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
                ggplot(data = data.frame(x = 1:10, y = runif(10)), aes(x=x, y=y)) +
                  geom_point(color = "blue")
                # pp = ggplot(tab, aes_string(x = "YEAR", y = "Abund", color = "PFG")) +
                #   scale_color_manual("", values = fun_col(length(unique(tab$PFG)))) +
                #   geom_line() +
                #   facet_grid("TYPE ~ ID", scales = ifelse(opt.abund_fixedScale, "fixed", "free_y")) +
                #   labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' abundance"),
                #        subtitle = paste0("For each PFG, the line represents the evolution through time of its abundance\n",
                #                          "for 5 randomly selected pixels within the studied area.\n")) +
                #   .getGraphics_theme()
              }
              ## ---------------------------------------------------------------------------------------------------------- ##
              , light.pixels = {
                ggplot(data = data.frame(x = 1:10, y = runif(10)), aes(x=x, y=y)) +
                  geom_point(color = "red")
                # pp = ggplot(tab, aes_string(x = "YEAR", y = "Abund", color = "STRATUM")) +
                #   scale_color_manual("", values = fun_col(max(sub("STRATUM_", "", tab$STRATUM)))) +
                #   geom_line() +
                #   facet_grid("TYPE ~ ID", scales = ifelse(opt.abund_fixedScale, "fixed", "free_y")) +
                #   labs(x = "", y = "", title = paste0("GRAPH B : evolution of light resources"),
                #        subtitle = paste0("For each stratum, the line represents the evolution through time of its light resources\n",
                #                          "for 5 randomly selected pixels within the studied area.\n")) +
                #   .getGraphics_theme()
              }
              ## ---------------------------------------------------------------------------------------------------------- ##
  )
  
  if (is.ggplot(pp))
  {
    RV$compt.browser.pfg<- 1
    RV$compt.browser.pfg.max <- 1
    
    shinyjs::hide("pfg.go.left")
    shinyjs::hide("pfg.go.right")
    
    output$UI.pfg.browser = renderUI({
      plotlyOutput(outputId = "plot.browser", width = "100%", height = "600px")
    })
    
    output$plot.browser = renderPlotly({ ggplotly(pp) })
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
    RV$compt.browser.pfg<- 1
    
    all.plot = 1:RV$compt.browser.pfg.max
    shinyjs::show(paste0("pfg.browser_", RV$compt.browser.pfg))
    if (RV$compt.browser.pfg.max > 1)
    {
      for (i in all.plot[-RV$compt.browser])
      {
        shinyjs::hide(paste0("pfg.browser_", i))
      }
    }
  }
  # }
})

####################################################################

observeEvent(RV$compt.browser, {
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
    RV$compt.browser.pfg<- RV$compt.browser.pfg- 1
    if (RV$compt.browser.pfg== 1)
    {
      shinyjs::disable("pfg.go.left")
    }
  }
})

observeEvent(input$pfg.go.right, {
  if (input$pfg.go.right > 0)
  {
    shinyjs::enable("pfg.go.left")
    RV$compt.browser.pfg<- RV$compt.browser.pfg+ 1
    if (RV$compt.browser.pfg== RV$compt.browser.pfg.max)
    {
      shinyjs::disable("pfg.go.right")
    }
  }
})



