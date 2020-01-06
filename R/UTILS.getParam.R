### HEADER #####################################################################
##' @title Extract parameter value(s) from a parameter file
##' 
##' @name .getParam
##'
##' @author Maya Guéguen
##' 
##' @description This function extracts from a text file the value(s) of a 
##' given parameter.
##' 
##' @param params.lines a \code{string} that corresponds to the 
##' name of the file from which to extract the parameter value
##' @param flag a \code{string} that corresponds to the parameter
##' name to be extracted and that must be present into the
##' \code{param.lines} file
##' @param flag.split either " " or "^--.*--$", depending on the
##' type of parameter file
##' @param is.num default \code{TRUE}. If \code{TRUE}, the 
##' extracted parameter is considered to be \code{numeric} and 
##' will be processed as such
##'
## END OF HEADER ###############################################################


.getParam = function(params.lines
                     , flag
                     , flag.split
                     , is.num = TRUE
){
  
  if (.testParam_notChar(params.lines))
  {
    .stopMessage_beChar("params.lines")
  } else
  {
    .testParam_existFile(params.lines)
  }
  if (.testParam_notChar(flag) ||
      nchar(flag) == 0)
  {
    .stopMessage_beChar("flag")
  }
  if (missing(flag.split) ||
      is.na(flag.split) ||
      !is.character(flag.split) ||
      !(flag.split %in% c(" ", "^--.*--$")))
  {
    .stopMessage_content("flag.split", c(" ", "^--.*--$"))
  }
  if (!is.logical(is.num))
  {
    stop("Wrong type of data!\n `is.num` must be logical")
  }
  
  param.name = params.lines
  params.lines = readLines(params.lines)
  if (flag.split == "^--.*--$")
  {
    if (length(grep("--END_OF_FILE--", params.lines)) == 0)
    {
      stop(paste0("Wrong type of data!\n `flag` (--END_OF_FILE--) is not found within `params.lines` (", param.name, ")"))
    }
  }
  if (length(grep(flag.split, params.lines)) <= ifelse(flag.split == "^--.*--$", 1, 0)){
    stop(paste0("Wrong type of data!\n `flag.split` (", flag.split, ") is not found within `params.lines` (", param.name, ")"))
  }
  if (length(grep(flag, params.lines)) == 0){
    stop(paste0("Wrong type of data!\n `flag` (", flag, ") is not found within `params.lines` (", param.name, ")"))
  }
  
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

