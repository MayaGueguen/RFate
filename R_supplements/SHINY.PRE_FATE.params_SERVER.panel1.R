
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

