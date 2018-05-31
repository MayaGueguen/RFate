##' #@export
## @importFrom utils install.packages

.getParam = function(params.lines
                     , flag
                     , flag.split
                     , is.num = TRUE
){
  
  if (missing(params.lines) ||
      is.na(params.lines) ||
      is.null(length(params.lines)) ||
      !is.character(params.lines) ||
      length(params.lines) == 0)
  {
    stop("Wrong type of data!\n `params.lines` must contain a character value of length > 0")
  } else
  {
    if (!file.exists(params.lines))
    {
      stop("Wrong type of data!\n `params.lines` file does not exist")
    }
  }
  if (missing(flag) ||
      is.na(flag) ||
      !is.character(flag) ||
      nchar(flag) == 0)
  {
    stop("Wrong type of data!\n `flag` must be a string of length > 0")
  }
  if (missing(flag.split) ||
      is.na(flag.split) ||
      !is.character(flag.split) ||
      !(flag.split %in% c(" ", "^--.*--$")))
  {
    stop("Wrong type of data!\n `flag.split` must be either ` ` or `^--.*--$`")
  }
  if (!is.logical(is.num))
  {
    stop("Wrong type of data!\n `is.num` must be logical")
  }
  
  params.lines = readLines(params.lines)
  if (length(grep(flag, params.lines)) == 0){
    stop("Wrong type of data!\n `flag` is not found within `params.lines`")
  }
  if (length(grep(flag.split, params.lines)) == 0){
    stop("Wrong type of data!\n `flag.split` is not found within `params.lines`")
  }


  if(flag.split == " "){
    value.line = grep(flag, params.lines, value = TRUE) #params.lines[ind.flag]
    value.line = unlist(strsplit(value.line, split = flag.split))[-1]
  } else {
    ind.flag.split = grep(flag.split, params.lines)
    ind.flag = grep(flag, params.lines)
    ind.start = which(ind.flag.split == ind.flag)
    
    ind1 = (ind.flag.split[ind.start] + 1)
    ind2 = ifelse(length(ind.flag.split) == 1, max(length(params.lines), ind1), ind1 - 1)
    value.line = params.lines[ind1:ind2]
    value.line = as.character(value.line)
  }
  if(is.num){
    value.line = as.numeric(value.line)
  }
  return(value.line)
}

