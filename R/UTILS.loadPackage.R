##' #@export
##' @importFrom utils install.packages

.loadPackage = function(package.name)
{
  load.package = requireNamespace(package.name) ## niche.overlap
  if (!load.package)
  {
    cat("\n > Installing `",package.name,"` package...\n")
    install.packages(package.name)
    load.package = requireNamespace(package.name)
    if (!load.package)
    {
      stop(paste0("Installation of `",package.name,"` package failed!"))
    }
  }
}
