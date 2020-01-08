### HEADER #####################################################################
##' @title Replace parameter value(s) from a parameter file
##' 
##' @name .setParam
##'
##' @author Maya Guéguen
##' 
##' @description This function finds in a text file the value(s) of a 
##' given parameter, and replace it with new value(s).
##' 
##' @param params.lines a \code{string} that corresponds to the 
##' name of the file from which to extract the parameter value
##' @param flag a \code{string} that corresponds to the parameter
##' name to be extracted and that must be present into the
##' \code{param.lines} file
##' @param flag.split either " " or "^--.*--$", depending on the
##' type of parameter file
##' @param value a \code{string} or a \code{numeric} value (it can also be 
##' a \code{vector}) containing the new value of the parameter to be changed
##'
## END OF HEADER ###############################################################


.setParam = function(params.lines
                     , flag
                     , flag.split
                     , value
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
  if (.testParam_notDef(value) || nchar(value) == 0)
  {
    stop("No data given!\n (missing `value` information)")
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
    value.line = grep(flag, params.lines, value = TRUE)
    params.lines = sub(value.line
                       , paste(flag, paste0(value, collapse = flag.split), sep = flag.split)
                       , params.lines)
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
    if (length(ind1:ind2) == length(value)){
      params.lines[ind1:ind2] = value
    } else {
      stop(paste0("Wrong dimension(s) of data!\n `value` does not have the same number of elements ("
                  , length(value), ") than `flag` (", flag, ", ", length(ind1:ind2), ")"))
    }
  }
  cat(params.lines, sep = "\n", file = param.name, append = FALSE)
  message(paste0("\n The parameter file ", param.name, " has been successfully corrected !\n"))
}

