

.stopMessage_existFile = function(param)
{
  stop(paste0("Wrong name file given!\n `", param, "` does not exist"))
}

.stopMessage_existFolders = function(param1, param2)
{
  stop(paste0("Wrong name folder given!\n `", param1, "` does not exist or does not contain a ", param2, " folder"))
}

.stopMessage_beDataframe = function(param)
{
  stop(paste0("Wrong type of data!\n `", param, "` must be a data.frame"))
}

.stopMessage_beInteger = function(param)
{
  stop(paste0("Wrong type of data!\n `", param, "` must be an integer > 0"))
}

.stopMessage_beChar = function(param)
{
  stop(paste0("Wrong type of data!\n `", param, "` must contain a character value of length > 0"))
}

#################################################################################################

.stopMessage_numRowCol = function(param1, param2)
{
  stop(paste0("Wrong dimension(s) of data!\n `", param1
              , "` does not have the appropriate number of rows (>0) or columns ("
              , paste0(param2, collapse = ", "), ")"))
}


.stopMessage_columnNames = function(param1, param2)
{
  if (length(param2) == 1)
  {
    end_message = param2
  } else
  {
    end_message = paste0(paste0(param2[-length(param2)], collapse = "`, `")
                         , "` and `", param2[length(param2)])
  }
  stop(paste0("Wrong type of data!\n Column names of `", param1
              , "` must be `", end_message, "`"))
}


.stopMessage_columnNumeric = function(param1, param2)
{
  if (length(param2) == 1)
  {
    end_message = param2
  } else
  {
    end_message = paste0(paste0(param2[-length(param2)], collapse = "`, `")
                         , "` and `", param2[length(param2)])
  }
  stop(paste0("Wrong type of data!\n Columns `", end_message
                , "` of `", param1, "` must contain numeric values"))
}

.stopMessage_columnNoNA = function(param1, param2)
{
  if (length(param2) == 1)
  {
    end_message = param2
  } else
  {
    end_message = paste0(paste0(param2[-length(param2)], collapse = "`, `")
                         , "` and `", param2[length(param2)])
  }
  stop(paste0("Wrong type of data!\n Columns `", end_message
              , "` of `", param1, "` must not contain NA values"))
}

.stopMessage_columnBetween = function(param1, param2, val1, val2)
{
  if (length(param2) == 1)
  {
    end_message = param2
  } else
  {
    end_message = paste0(paste0(param2[-length(param2)], collapse = "`, `")
                         , "` and `", param2[length(param2)])
  }
  stop(paste0("Wrong type of data!\n Columns `", end_message
              , "` of `", param1, "` must contain values between ", val1, " and ", val2))
}


#################################################################################################

.stopMessage_content = function(param1, param2)
{
  if (length(param2) == 1)
  {
    end_message = param2
  } else
  {
    end_message = paste0(paste0(param2[-length(param2)], collapse = "`, `")
                         , "` or `", param2[length(param2)])
  }
  stop(paste0("Wrong type of data!\n `", param1, "` must be either `", end_message, "`"))
}

