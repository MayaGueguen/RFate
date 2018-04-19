##' #@export
##' @importFrom utils install.packages

.loadPackage = function(package.name)
{
  if (missing(package.name) || is.na(package.name) || length(package.name) == 0){
    stop("No data given!\n (missing `package.name`)")
  } else if (!is.character(package.name)){
    stop("Wrong type of data!\n `package.name` must contain a character value")
  } else {
    load.package = requireNamespace(package.name)
    if (!load.package)
    {
      cat("\n > Installing `",package.name,"` package...\n")
      install.packages(package.name)
      load.package = requireNamespace(package.name)
      if (!load.package)
      {
        stop(paste0("Installation of `",package.name,"` package failed!"))
      } else
      {
        message(paste0("Installation of `",package.name,"` package succeeded!"))
      }
    }
  }
}
