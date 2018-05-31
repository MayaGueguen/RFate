##' #@export
## @importFrom utils install.packages

.createParams = function(params.file, params.list, separator = " ")
{
  if (missing(params.file) ||
      is.na(params.file) ||
      is.null(length(params.file)) ||
      !is.character(params.file) ||
      nchar(params.file) == 0)
  {
    stop("Wrong type of data!\n `params.file` must contain a character value of length > 0")
  } else
  {
    if (tail(strsplit(params.file, "[.]")[[1]], 1) != "txt")
    {
      stop("Wrong type of data!\n `params.file` must be a file name with .txt extension")
    }
    if (!dir.exists(dirname(params.file)))
    {
      stop("Wrong name file given!\n `params.file` directory does not exist")
    }
    if (file.exists(params.file))
    {
      warning("`params.file` already exists. It will be replaced.")
    }
  }
  if (missing(params.list) || !is.list(params.list))
  {
    stop("Wrong type of data!\n `params.list` must be a list")
  } else
  {
    if (is.null(names(params.list)))
    {
      stop("Wrong type of data!\n `params.list` must be a list with non-null names")
    }
  }
  

  
  text.to.paste = sapply(1:length(params.list), function(x){
    res = paste0(params.list[[x]], collapse = separator)
    res = paste0(names(params.list)[x], separator, res, "\n")
  })
  text.to.paste = paste0(text.to.paste, collapse = "")
  text.to.paste = paste0("## File automatically generated \n"
                         , "## Date : ", date(), "\n"
                         , text.to.paste)
  
  cat(text.to.paste, sep = "", file = params.file, append = FALSE)
  message(paste0("\n The parameter file ", params.file, " has been successfully created !\n"))
}
