
###################################################################################################################################

theme.color = "#3a7da8"
navbar.color = "#e0dbd9"
navbar.color.text = "#8c8582"

button.color = "rgba(96, 129, 150, 0.5)"
button.style = paste0("background-color: ", button.color, "; border-width:0px;")
button.style.help = paste0("color:#FFFFFF; font-family: 'Londrina Solid', cursive; font-size: 18px; background-color: rgba(10, 58, 135, 0.8); border-width:0px;")

panel.style = paste0("color:#FFFFFF; background-color:", button.color, "; border-width:0px;")
panel.style.hover = paste0("color:#FFFFFF; background-color:", theme.color, "; border-width:0px;")

help.color = "#dee2e8"

param.style = function(param.text)
{
  return(HTML(paste0("<span style = 'font-style: italic; font-weight: normal;'>"
                     , param.text
                     , "</span>")))
}

help.web = function(web.address)
{
  return(paste0("<a href='"
                , web.address
                , "' target='_blank'>See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a>"))
}

help.param.name = function(param.text)
{
  return(paste0("<tr><td style='width:30%;font-weight:bold;font-family:Monospace;vertical-align:top;'>"
                , param.text
                , "</td>"))
}
help.param.desc = function(param.text)
{
  return(paste0("<td style='width:70%;'>"
                , param.text
                , "</td></tr>"))
}
help.full = function(param.web = NULL, param.name.vec, param.desc.vec)
{
  TEXT.full = foreach(i = 1:length(param.name.vec), .combine = "c") %do%
  {
    text.name = help.param.name(param.name.vec[i])
    text.desc = help.param.desc(param.desc.vec[i])
    return(paste0(text.name, text.desc))
  }
  TEXT.full = paste0(TEXT.full, collapse = "")

  TEXT = paste0("<table style='width:100%;'>"
                , TEXT.full
                , "</table>")
  if (!is.null(param.web))
  {
    TEXT = paste0(help.web(param.web), TEXT)
  }
  return(HTML(TEXT))
}




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

print_messages = function(fun, cut_pattern = "STUPID")
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
      shinyalert(type = "success", text = sub(cut_pattern, paste0(cut_pattern, " "), out_info$message))
    }
    return(1)
  }
}

###################################################################################################################################

get_files = function(path_folder, skip.no = 2, opt.sub_folder = FALSE)
{
  tab_names = list.files(path = path_folder
                         , include.dirs = FALSE
                         , full.names = TRUE
                         , recursive = opt.sub_folder)
  if (length(tab_names) > 0)
  {
    tab = foreach(tab_name = tab_names) %do%
    {
      fread(file = tab_name, header = FALSE, skip = skip.no, sep = "\t")
    }
    if (length(tab) > 1)
    {
      nrows = sapply(tab, nrow)
      nrow_max = max(nrows)
      if (length(which(nrows < nrow_max)) > 0)
      {
        for (i in which(nrows < nrow_max))
        {
          tab[[i]] = rbind(tab[[i]], data.frame(V1 = rep("", nrow_max - nrows[i])))
        }
      }
      tab = do.call(cbind, tab)
    } else
    {
      tab = tab[[1]]
    }
    tab_names = sub("//", "/", tab_names)
    tab_names = sub(path_folder, "", tab_names)
    colnames(tab) = tab_names
    return(tab)
  }
}

###################################################################################################################################

.getParam = function(params.lines
                     , flag
                     , flag.split
                     , is.num = TRUE
){
  

  param.name = params.lines
  params.lines = readLines(params.lines)

  if(flag.split == " "){
    value.line = grep(flag, params.lines, value = TRUE) #params.lines[ind.flag]
    value.line = unlist(strsplit(value.line, split = flag.split))[-1]
  } else {
    ind.flag.split = grep(flag.split, params.lines)
    ind.flag = grep(paste0("--", flag, "--"), params.lines)
    if (length(ind.flag) == 0)
    {
      stop(paste0("Wrong type of data!\n `flag` (", flag, ") is not found within `params.lines` (", param.name, ")"))
    }
    ind.start = which(ind.flag.split == ind.flag)
    if (ind.flag.split[ind.start + 1] == ind.start + 1)
    {
      stop(paste0("Wrong type of data!\n `flag` (", flag, ") does not contain any value"))
    }
    
    ind1 = (ind.flag.split[ind.start] + 1)
    ind2 = ifelse(length(ind.flag.split) == 1
                  , max(length(params.lines), ind1)
                  , ind.flag.split[ind.start + 1] - 1)
    value.line = params.lines[ind1:ind2]
    value.line = as.character(value.line)
  }
  if(is.num){
    value.line = as.numeric(value.line)
  }
  return(value.line)
}


