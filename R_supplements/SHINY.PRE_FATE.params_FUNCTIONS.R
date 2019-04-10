

###################################################################################################################################

get_messages = function(out_info)
{
  ind_empty = which(nchar(out_info) == 0)
  ind_Warning = grep("^Warning", out_info)
  ind_Warning_mess = vector()
  for(ind in ind_Warning){
    if (length(ind_empty) > 0)
    {
      i = ind + 1
      next_empty = FALSE
      while(!next_empty)
      {
        if (i %in% ind_empty){
          next_empty = TRUE
        } else {
          i = i + 1
        }
      }
    } else
    {
      i = length(out_info) + 1
    }
    ind_Warning_mess = c(ind_Warning_mess, (ind+1):(i-1))
  }
  ind_mess = 1:length(out_info)
  ind_mess = ind_mess[-c(ind_empty, ind_Warning, ind_Warning_mess)]
  return(list(warning = out_info[ind_Warning_mess]
              , message = out_info[ind_mess]))
}

###################################################################################################################################

print_messages = function(fun)
{
  out_fun = tryCatch(
    capture.output(type = "message", expr = { fun })
    , error = function(e) { e })
  if (inherits(out_fun, "simpleError"))
  {
    shinyalert(type = "error", text = out_fun$message)
    return(0)
  } else
  {
    out_info = get_messages(out_fun)
    if (length(out_info$warning) > 0)
    {
      showNotification(out_info$warning, type = "warning")
    }
    if (length(out_info$message) > 0)
    {
      shinyalert(type = "success", text = out_info$message)
    }
    return(1)
  }
}

###################################################################################################################################

get_files = function(path_folder, skip.no = 2)
{
  tab_names = list.files(path = path_folder, include.dirs = FALSE)
  tab = foreach(tab_name = tab_names, .combine = "cbind") %do%
  {
    fread(file = paste0(path_folder, tab_name), header = FALSE, skip = skip.no, sep = "\t")
  }
  colnames(tab) = tab_names
  return(tab)
}
