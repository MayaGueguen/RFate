

####################################################################

observeEvent(input$create.skeleton, {
  print_messages(as.expression(
    PRE_FATE.skeletonDirectory(name.simulation = input$name.simul)
  ))
  
  shinyjs::show("main.panel")
  shinyjs::show("create.simul")
  shinyjs::show("FATE_simulation.zip")
  shinyjs::show("refresh")
})

####################################################################

observeEvent(input$create.simul, {
  if (input$create.skeleton > 0)
  {
    mask.file = list.files(path = paste0(input$name.simul, "/DATA/MASK")
                           , pattern = "^MASK_")
    if (input$upload.mask > 0 && length(mask.file) > 0)
    {
      get_res = print_messages(as.expression(
        PRE_FATE.params_simulParameters(name.simulation = input$name.simul
                                        , name.mask = mask.file
        )
      ))
    } else
    {
      shinyalert(type = "warning", text = "You must upload a simulation mask first !")
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

output$FATE_simulation.zip = downloadHandler(
  filename = function(){
    paste0(input$name.simul, "_", Sys.Date(), ".zip")
  },
  content = function(file){
    zip(zipfile = file, input$name.simul)
    file.copy(file, file)
  },
  contentType = "application/zip"
)

####################################################################

observeEvent(input$refresh, {
  system(command = paste0("rm -r ", input$name.simul))
  shinyjs::hide("main.panel")
  shinyjs::hide("create.simul")
  shinyjs::hide("FATE_simulation.zip")
  shinyjs::hide("refresh")
})

