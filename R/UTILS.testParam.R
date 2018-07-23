
.testParam_notDef = function(param)
{
  if (missing(param) ||
      is.na(param) ||
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
      !is.character(param))
  {
    return(TRUE)
  } else
  {
    return(FALSE)
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
      !(param %in% inList))
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
      !(class(param) %in% inList))
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}

