
####################################################################

observeEvent(input$upload.mask, {
  if (input$create.skeleton > 0)
  {
    if(!is.null(input$simul.mask))
    {
      file1 = input$simul.mask$datapath
      file2 = input$simul.mask$name
      file2 = paste0("MASK_", file2)
      file2 = paste0(input$name.simul, "/DATA/MASK/", file2)
      get_res = file.copy(from = file1, to = file2)
      
      if(get_res)
      {
        shinyalert(type = "success", text = paste0("The file ", input$simul.mask$name
                                                   , " has been correctly uploaded and renamed as "
                                                   , file2, " !"))
      } else
      {
        shinyalert(type = "error", text = "Oops. Something went wrong !")
      }
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

observeEvent(input$upload.habsuit.mask, {
  if (input$create.skeleton > 0)
  {
    if(!is.null(input$habsuit.mask))
    {
      if (nchar(input$habsuit.folder) > 0)
      {
        dir.create(paste0(input$name.simul, "/DATA/PFGS/HABSUIT/", input$habsuit.folder))
        opt.folder.name = paste0(input$habsuit.folder, "/")
      } else
      {
        opt.folder.name = ""
      }
      
      file1 = input$habsuit.mask$datapath
      file2 = input$habsuit.mask$name
      file2 = paste0("HABSUIT_", file2)
      file2 = paste0(input$name.simul, "/DATA/PFGS/HABSUIT/", opt.folder.name, file2)
      get_res = file.copy(from = file1, to = file2)
      
      if(get_res)
      {
        shinyalert(type = "success", text = paste0("The file ", input$habsuit.mask$name
                                                   , " has been correctly uploaded and renamed as "
                                                   , file2, " !"))
      } else
      {
        shinyalert(type = "error", text = "Oops. Something went wrong !")
      }
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})
####################################################################

observeEvent(input$upload.dist.mask, {
  if (input$create.skeleton > 0)
  {
    if(!is.null(input$dist.mask))
    {
      file1 = input$dist.mask$datapath
      file2 = input$dist.mask$name
      file2 = paste0("DIST_", file2)
      file2 = paste0(input$name.simul, "/DATA/MASK/", file2)
      get_res = file.copy(from = file1, to = file2)
      
      if(get_res)
      {
        shinyalert(type = "success", text = paste0("The file ", input$dist.mask$name
                                                   , " has been correctly uploaded and renamed as "
                                                   , file2, " !"))
      } else
      {
        shinyalert(type = "error", text = "Oops. Something went wrong !")
      }
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

observeEvent(input$add.changing, {
  # if(!is.null(input$changing.file))
  # {
  mat.changing <<- rbind(mat.changing
                         , data.frame(opt.folder.name = input$changing.folder
                                      , type.changing = input$type.changing
                                      , year = input$changing.year
                                      , order = input$changing.order
                                      , file.name = input$changing.file))
  # , file.name = input$changing.file$name))
  output$mat.changing = renderTable({ mat.changing })
  # }
})

observeEvent(input$delete.changing, {
  mat.changing <<- data.frame()
  output$mat.changing = renderTable({ mat.changing })
})

####################################################################

observeEvent(input$create.changing, {
  if (input$create.skeleton > 0)
  {
    if (nrow(mat.changing) > 0)
    {
      mat.changing.split = split(mat.changing, list(mat.changing$type.changing, mat.changing$opt.folder.name))
      print(length(mat.changing.split))
      for(i in 1:length(mat.changing.split))
      {
        print(i)
        tab = mat.changing.split[[i]]
        tab$opt.folder.name = as.character(tab$opt.folder.name)
        tab$file.name = as.character(tab$file.name)
        print(nrow(tab))
        print(tab)
        print(tab$opt.folder.name)
        if (nrow(tab) > 0 && sum(nchar(tab$file.name) == 0) == 0)
        {
          if (nchar(tab$opt.folder.name) > 0)
          {
            dir.create(paste0(input$name.simul, "/DATA/scenario/", tab$opt.folder.name))
          }
          get_res = print_messages(as.expression(
            PRE_FATE.params_changingYears(name.simulation = input$name.simul
                                          , type.changing = tab$type.changing
                                          , mat.changing = data.frame(year = tab$year
                                                                      , order = tab$year
                                                                      , file.name = tab$file.name)
                                          , opt.folder.name = tab$opt.folder.name
            )
          ))
          
          if(get_res)
          {
            output$created_table.changing = renderDataTable({
              path_folder = paste0(input$name.simul, "/DATA/SCENARIO/")
              return(get_files(path_folder, skip.no = 0))
            })
          }
        }
      }
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})
