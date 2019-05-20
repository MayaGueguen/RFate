##' @importFrom parallel mclapply

.unzip_ALL = function(folder_name, nb_cores)
{
  cat("\n UNZIP RASTER FILES from repository ", folder_name, "...\n")
  list_files = list.files(folder_name, pattern = ".gz$", full.names = T)
  mclapply(list_files, function(x)
    system(paste0("gunzip ", x), ignore.stderr = T), mc.cores = nb_cores)
  cat("\n Done!\n")
}

.unzip = function(folder_name, list_files, nb_cores)
{
  cat("\n UNZIP RASTER FILES from repository ", folder_name, "...\n")
  mclapply(list_files, function(x)
    system(paste0("gunzip ", x), ignore.stderr = T), mc.cores = nb_cores)
  cat("\n Done!\n")
}

.zip_ALL = function(folder_name, nb_cores)
{
  cat("\n ZIP RASTER FILES from repository ", folder_name, "...\n")
  list_files = list.files(folder_name, pattern = ".tif$|.img$", full.names = T)
  mclapply(list_files, function(x)
    system(paste0("gzip -9 ", x), ignore.stderr = T), mc.cores = nb_cores)
  cat("\n Done!\n")
}

.zip = function(folder_name, list_files, nb_cores)
{
  cat("\n ZIP RASTER FILES from repository ", folder_name, "...\n")
  mclapply(list_files, function(x)
    system(paste0("gzip -9 ", x), ignore.stderr = T), mc.cores = nb_cores)
  cat("\n Done!\n")
}
