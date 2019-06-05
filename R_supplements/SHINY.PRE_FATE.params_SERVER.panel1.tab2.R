
####################################################################

output$created_table.save = renderDataTable({
  path_folder = paste0(input$name.simul, "/DATA/SAVE/")
  return(get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE))
})

observeEvent(input$create.save.maps, {
  if (input$create.skeleton > 0)
  {
    if (is.numeric(input$save.maps.year1) &&
        is.numeric(input$save.maps.year2) &&
        is.numeric(input$save.maps.no))
    {
      if (input$save.maps.no > 0)
      {
        years.maps = round(seq(input$save.maps.year1
                               , input$save.maps.year2
                               , length.out = input$save.maps.no))
        years.maps = years.maps[which(years.maps > 0)]
        years.maps = sort(unique(years.maps))
        if (length(years.maps) > 0)
        {
          opt.folder.name = ifelse(nchar(input$save.maps.folder) > 0
                                   , gsub(" ", "_", input$save.maps.folder)
                                   , "")
          if (length(grep(" ", input$save.maps.folder)) > 0)
          {
            showNotification("Spaces within opt.folder.name have been replaced by `_` !", type = "warning")
          }
          
          get_res = print_messages(as.expression(
            PRE_FATE.params_saveYears(name.simulation = input$name.simul
                                      , years.maps = years.maps
                                      , years.objects = NULL
                                      , opt.folder.name = opt.folder.name
            )
          ), cut_pattern = paste0(input$name.simul, "/DATA/SAVE/"))
          
          if (as.character(get_res) != "0")
          {
            output$created_table.save = renderDataTable({
              path_folder = paste0(input$name.simul, "/DATA/SAVE/")
              return(get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE))
            })
          }
        } else
        {
          shinyalert(type = "warning", text = "No years have been selected !")
        }
      } else
      {
        shinyalert(type = "warning", text = "You must give a positive number of years !")
      }
    } else
    {
      shinyalert(type = "warning", text = "You must give numeric values !")
    } 
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

observeEvent(input$create.save.objects, {
  if (input$create.skeleton > 0)
  {
    cond1 = ifelse(!is.na(input$save.objects.year1), input$save.objects.year1 > 0, FALSE)
    cond2 = ifelse(!is.na(input$save.objects.year2), input$save.objects.year2 > 0, FALSE)
    cond3 = ifelse(!is.na(input$save.objects.year3), input$save.objects.year3 > 0, FALSE)
    if (cond1 || cond2 || cond3)
    {
      years.objects = c(input$save.objects.year1
                        , input$save.objects.year2
                        , input$save.objects.year3)
      years.objects = years.objects[which(years.objects > 0)]
      years.objects = sort(years.objects)
      
      opt.folder.name = ifelse(nchar(input$save.objects.folder) > 0
                               , gsub(" ", "_", input$save.objects.folder)
                               , "")
      if (length(grep(" ", input$save.objects.folder)) > 0)
      {
        showNotification("Spaces within opt.folder.name have been replaced by `_` !", type = "warning")
      }
      
      get_res = print_messages(as.expression(
        PRE_FATE.params_saveYears(name.simulation = input$name.simul
                                  , years.maps = NULL
                                  , years.objects = years.objects
                                  , opt.folder.name = opt.folder.name
        )
      ), cut_pattern = paste0(input$name.simul, "/DATA/SAVE/"))
      
      if (as.character(get_res) != "0")
      {
        output$created_table.save = renderDataTable({
          path_folder = paste0(input$name.simul, "/DATA/SAVE/")
          return(get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE))
        })
      }
    } else
    {
      shinyalert(type = "warning", text = "No years have been selected !")
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})
