##' #@export
## @importFrom utils install.packages

.getParam = function(params.lines
                     , flag
                     , flag.split
                     , is.num = TRUE
){
  
  if (is.null(length(params.lines)) || length(params.lines) == 0){
    stop("Wrong type of data!\n `params.lines` must be of length > 0")
  }
  if (nchar(flag) == 0) {
    stop("Wrong type of data!\n `flag` must be a string of length > 0")
  }
  if (!(flag.split %in% c(" ", "^--.*--$"))){
    stop("Wrong type of data!\n `flag.split` must be either ` ` or `^--.*--$`")
  }
  if (!is.logical(is.num)){
    stop("Wrong type of data!\n `is.num` must be logical")
  }
  if (length(grep(flag, params.lines)) == 0){
    stop("Wrong type of data!\n `flag` is not found within `params.lines`")
  }


  if(is.num){
    value.line = grep(flag, params.lines, value = TRUE) #params.lines[ind.flag]
    value.line = unlist(strsplit(value.line, split = flag.split))[-1]
    value.line = as.numeric(value.line)
  } else {
    ind.flag.split = grep(flag.split, params.lines)
    ind.flag = grep(flag, params.lines)
    ind.start = which(ind.flag.split == ind.flag)
    
    value.line = params.lines[(ind.flag.split[ind.start] + 1):(ind.flag.split[ind.start + 1] - 1)]
    value.line = as.character(value.line)
  }
  return(value.line)
}

