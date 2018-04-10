##' #@export
## @importFrom utils install.packages

.createParams = function(params.file, params.list)
{
  if (nchar(params.file) == 0) {
    stop("Wrong type of data!\n `params.file` must be a string of length > 0")
  }
  if (tail(strsplit(params.file, "[.]")[[1]], 1) != "txt") {
    stop("Wrong type of data!\n `params.file` must be a file name with .txt extension")
  }
  if (!is.list(params.list)) {
    stop("Wrong type of data!\n `params.list` must be a list")
  }
  if (is.null(names(params.list))) {
    stop("Wrong type of data!\n `params.list` must be a list with non-null names")
  }
  if (!dir.exists(dirname(params.file))){
    stop("Wrong name file given!\n `params.file` directory does not exist")
  }

  
  text.to.paste = sapply(1:length(params.list), function(x){
    res = paste0(params.list[[x]], collapse = " ")
    res = paste0(names(params.list)[x], " ", res, "\n")
  })
  text.to.paste = paste0(text.to.paste, collapse = "")
  text.to.paste = paste0("## File automatically generated \n"
                         , "## Date : ", date(), "\n"
                         , text.to.paste)
  
  cat(text.to.paste, sep = "", file = params.file, append = FALSE)
  cat("\n The parameter file ", params.file, "has been successfully created !\n")
  cat("\n")
}
