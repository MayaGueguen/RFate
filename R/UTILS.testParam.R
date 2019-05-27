
.testParam_notDef = function(param)
{
  if (missing(param) ||
      (length(param) == 1 && is.na(param)) ||
      # sum(is.na(param)) > 0 ||
      is.null(param) ||
      length(param) == 0)
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}

#################################################################################################
.testParam_notChar = function(param)
{
  if (.testParam_notDef(param) ||
      !is.character(param) ||
      sum(nchar(param) == 0, na.rm = TRUE) > 0)
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}

.testParam_existFile = function(param)
{
  if (.testParam_notChar(param) ||
      sum(!file.exists(param), na.rm = TRUE) > 0)
  {
    .stopMessage_existFile(param)
  }
}

.testParam_existFolder = function(param1, param2)
{
  if (.testParam_notChar(param1) ||
      !dir.exists(paste0(param1, "/", param2)))
  {
    .stopMessage_existFolders(deparse(substitute(param1)), param2)
  }
}

#################################################################################################
.testParam_notNum = function(param)
{
  if (.testParam_notDef(param) ||
      !is.numeric(param))
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}

#################################################################################################
.testParam_notDf = function(param)
{
  if (missing(param) ||
      !is.data.frame(param))
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}

#################################################################################################
.testParam_notInChar = function(param, inList)
{
  if (.testParam_notDef(param) ||
      sum(!(param %in% inList), na.rm = TRUE) > 0)
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}

#################################################################################################
.testParam_notInClass = function(param, inList)
{
  if (missing(param) ||
      is.null(param) ||
      length(param) == 0 ||
      sum(!(class(param) %in% inList), na.rm = TRUE) > 0)
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}

