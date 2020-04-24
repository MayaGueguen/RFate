### HEADER #####################################################################
##' @title Compress (\code{.tif}, \code{.img}) or decompress (\code{.gz}) files 
##' contained in results folder
##' 
##' @name .unzip_ALL
##' @aliases .unzip
##' @aliases .zip_ALL
##' @aliases .zip
##' 
##' @usage
##' .unzip_ALL(folder_name, no_cores)
##' .unzip(folder_name, list_files, no_cores)
##' .zip_ALL(folder_name, no_cores)
##' .zip(folder_name, list_files, no_cores)
##'
##' @description These functions compress (\code{.tif}, \code{.img}) or 
##' decompress (\code{.gz}) files contained in a given folder.
##' 
##' @param folder_name a \code{string} corresponding to the directory to be 
##' scanned
##' @param list_files a \code{vector} containing filenames to be compress or 
##' decompress, in order not to scan the whole given directory
##' @param no_cores an \code{integer} corresponding to the number of computing 
##' resources that can be used to parallelize the (de)compression
##' 
##' @importFrom parallel mclapply
##'
## END OF HEADER ###############################################################

NULL

##' @export

.unzip_ALL = function(folder_name, no_cores)
{
  cat("\n UNZIP RASTER FILES from repository ", folder_name, "...\n")
  if (no_cores > 1 && .getOS() == "windows")
  {
    no_cores = 1
    warning("Parallelisation is not available for Windows. Sorry.")
  }
  list_files = list.files(folder_name, pattern = ".gz$", full.names = T)
  mclapply(list_files, function(x)
    system(paste0("gunzip ", x), ignore.stderr = T), mc.cores = no_cores)
  cat(" Done!\n")
}



##' @export

.unzip = function(folder_name, list_files, no_cores)
{
  cat("\n UNZIP RASTER FILES from repository ", folder_name, "...\n")
  if (no_cores > 1 && .getOS() == "windows")
  {
    no_cores = 1
    warning("Parallelisation is not available for Windows. Sorry.")
  }
  mclapply(list_files, function(x)
    system(paste0("gunzip ", x), ignore.stderr = T), mc.cores = no_cores)
  cat(" Done!\n")
}



##' @export

.zip_ALL = function(folder_name, no_cores)
{
  cat("\n ZIP RASTER FILES from repository ", folder_name, "...\n")
  if (no_cores > 1 && .getOS() == "windows")
  {
    no_cores = 1
    warning("Parallelisation is not available for Windows. Sorry.")
  }
  list_files = list.files(folder_name, pattern = ".tif$|.img$", full.names = T)
  mclapply(list_files, function(x)
    system(paste0("gzip -9 ", x), ignore.stderr = T), mc.cores = no_cores)
  cat(" Done!\n")
}



##' @export

.zip = function(folder_name, list_files, no_cores)
{
  cat("\n ZIP RASTER FILES from repository ", folder_name, "...\n")
  if (no_cores > 1 && .getOS() == "windows")
  {
    no_cores = 1
    warning("Parallelisation is not available for Windows. Sorry.")
  }
  mclapply(list_files, function(x)
    system(paste0("gzip -9 ", x), ignore.stderr = T), mc.cores = no_cores)
  cat(" Done!\n")
}
