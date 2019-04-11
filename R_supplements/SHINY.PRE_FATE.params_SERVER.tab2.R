
####################################################################

observeEvent(input$create.save.maps, {
  if (input$create.skeleton > 0)
  {
    opt.folder.name = ifelse(nchar(input$save.maps.folder) > 0, input$save.maps.folder, "")
    get_res = print_messages(as.expression(
      PRE_FATE.params_saveYears(name.simulation = input$name.simul
                                , years.maps = round(seq(input$save.maps.year1
                                                         , input$save.maps.year2
                                                         , length.out = input$save.maps.no))
                                , years.objects = NULL
                                , opt.folder.name = opt.folder.name
      )
    ))
    
    if(get_res)
    {
      output$created_table.save = renderDataTable({
        path_folder = paste0(input$name.simul, "/DATA/SAVE/", opt.folder.name)
        if (nchar(opt.folder.name) > 0) path_folder = paste0(path_folder, "/")
        return(get_files(path_folder, skip.no = 0))
      })
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})


observeEvent(input$create.save.objects, {
  if (input$create.skeleton > 0)
  {
    opt.folder.name = ifelse(nchar(input$save.objects.folder) > 0, input$save.objects.folder, "")
    get_res = print_messages(as.expression(
      PRE_FATE.params_saveYears(name.simulation = input$name.simul
                                , years.maps = NULL
                                , years.objects = c(input$save.objects.year1
                                                    , input$save.objects.year2
                                                    , input$save.objects.year3)
                                , opt.folder.name = opt.folder.name
      )
    ))
    
    if(get_res)
    {
      output$created_table.save = renderDataTable({
        path_folder = paste0(input$name.simul, "/DATA/SAVE/", opt.folder.name)
        if (nchar(opt.folder.name) > 0) path_folder = paste0(path_folder, "/")
        return(get_files(path_folder, skip.no = 0))
      })
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})
