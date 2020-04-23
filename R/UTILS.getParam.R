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
##' @param params.lines a \code{string} that corresponds to the name of the 
##' file from which to extract the parameter value
##' @param flag a \code{string} that corresponds to the parameter name to be 
##' extracted and that must be present into the \code{param.lines} file
##' @param flag.split either "\code{ }" or "\code{^--.*--$}", depending on the 
##' type of parameter file
##' @param is.num default \code{TRUE}. If \code{TRUE}, the extracted parameter 
##' is considered to be \code{numeric} and will be processed as such
##' 
##' 
##' @return A \code{vector} containing one or more values of type \code{string} 
##' or \code{numeric} depending on the value of\code{is.num} parameter.
##' 
##' @examples 
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create a Global_parameters file
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
##'                                  , required.no_PFG = 6
##'                                  , required.no_strata = 5
##'                                  , required.simul_duration = 100
##'                                  , required.seeding_duration = c(10,50)
##'                                  , required.seeding_timestep = 1
##'                                  , required.seeding_input = 100
##'                                  , required.max_abund_low = 30000
##'                                  , required.max_abund_medium = 50000
##'                                  , required.max_abund_high = 90000)
##'                                  
##' ## Extract number of PFG
##' .getParam(params.lines = "FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt"
##'           , flag = "NO_PFG"
##'           , flag.split = " "
##'           , is.num = TRUE)
##' 
##' 
##' ## ----------------------------------------------------------------------------------------- ##
##'                                 
##' ## Load example data
##'           
##' 
##' @export
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
    value.line = grep(paste0(flag, " "), params.lines, value = TRUE)
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

