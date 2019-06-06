
####################################################################

observeEvent(input$HELP.panel1, {
  introjs(session = session
          , options = list("nextLabel" = "Next"
                           , "prevLabel" = "Prev"
                           , "skipLabel" = "Close"
                           , steps = data.frame(element = paste0("#help1_", 1:5)
                                                , intro = c("", "", "", "", ""))
          )
  )
})

