### HEADER #####################################################################
##' @title Load a dataset
##' 
##' @name .loadData
##'
##' @author Maya Guéguen
##' 
##' @description This function loads one of the available data sets.
##' 
##' @param data.name a \code{string} that corresponds to the 
##' name of the dataset that will be loaded
##'
##' @export
##'
##' @importFrom utils download.file
##'
## END OF HEADER ###############################################################

.loadData = function(data.name)
{
  if (.testParam_notInChar(data.name, c("PNE_PFG", "PNE_PARAM", "PNE_RESULTS"))) {
    .stopMessage_content("data.name", c("PNE_PFG", "PNE_PARAM", "PNE_RESULTS"))
  } else {
    cat("\n > Loading `",data.name,"` dataset...\n")
    download.file(url = paste0("https://raw.githubusercontent.com/MayaGueguen/RFate/master/data-raw/DATASET_PNE/"
                               , data.name, ".RData")
                  , destfile = paste0(data.name, ".RData")
                  , method = "wget")
    if (!file.exists(paste0(data.name, ".RData")))
    {
      stop(paste0("Download of `",data.name,"` dataset failed!"))
    } else
    {
      load(paste0("./", data.name, ".RData"))
      message(paste0("Download of `",data.name,"` dataset succeeded!"))
    }
  }
}

