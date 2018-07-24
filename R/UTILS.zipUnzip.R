##' @importFrom parallel mclapply

.unzip = function(folder_name, nb_cores)
{
  cat("\n UNZIP RASTER FILES from repository ", folder_name, "...\n")
  list_files = list.files(folder_name, full.names = T)
  mclapply(list_files, function(x)
    system(paste0("gunzip ", x)), mc.cores = nb_cores)
  cat("\n Done!\n")
}

.zip = function(folder_name, nb_cores)
{
  cat("\n ZIP RASTER FILES from repository ", folder_name, "...\n")
  list_files = list.files(folder_name, full.names = T)
  mclapply(list_files, function(x)
    system(paste0("gzip -9 ", x)), mc.cores = nb_cores)
  cat("\n Done!\n")
}

