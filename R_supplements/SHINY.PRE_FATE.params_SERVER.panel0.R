
####################################################################

observeEvent(input$HELP.panel0, {
  introjs(session = session
          , options = list("nextLabel" = "Next"
                           , "prevLabel" = "Prev"
                           , "skipLabel" = "Close"
                           , steps = data.frame(element = paste0("#help0_", 1:5)
                                                , intro = c("FATE presentation blabla"
                                                            , "PFG"
                                                            , "Simulations parameters"
                                                            , "Help"
                                                            , "websites"))
          )
  )
})

