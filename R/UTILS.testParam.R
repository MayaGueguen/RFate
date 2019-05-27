
globalVariables(
  c(
    "AUC.sd",
    "PFG",
    "SCENARIO.DIST",
    "SCENARIO.HS",
    "SCENARIO.MASK",
    "abs.simulParam",
    "cutoff",
    "di.mod",
    "dir.output.perPFG.allStrata",
    "dir.output.perPFG.allStrata.BIN",
    "dir.output.perPFG.allStrata.REL",
    "dir.output.perPFG.perStrata",
    "dir.output.perPFG.perStrata.BIN",
    "dir.save",
    "file.mask",
    "files.PFG.DISP",
    "files.PFG.DIST",
    "files.PFG.LIGHT",
    "files.PFG.SOIL",
    "files.PFG.SUCC",
    "i",
    "ind_1_mask",
    "no_1_mask",
    "no_PFG",
    "pfg",
    "ras.mask",
    "sensitivity.sd",
    "specificity.sd",
    "st",
    "tab",
    "thresh",
    "tr",
    "value",
    "vari",
    "xx",
    "xy.1",
    "y"
  )
)

#################################################################################################
.testParam_notDef = function(param)
{
  if (missing(param) ||
      (length(param) == 1 && is.na(param)) ||
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

