### HEADER #####################################################################
##' @title Upscale all raster maps of a \code{FATE-HD} simulation folder
##' 
##' @name .upscaleMaps
##'
##' @author Maya Guéguen
##' 
##' @description This function scans all the raster files within a 
##' \code{FATE-HD} simulation folder and upscale them to the specified 
##' resolution
##' 
##' @param name.simulation a \code{string} that corresponds to the main
##' directory or simulation name of the \code{FATE-HD} simulation
##' @param resolution an \code{integer} that corresponds to the new 
##' resolution to upscale all the maps
##' 
##' @importFrom raster raster projectRaster writeRaster
##'
## END OF HEADER ###############################################################


.upscaleMaps = function(name.simulation
                       , resolution
                       
){
  .testParam_existFolder(name.simulation, "")
  name.simulation = sub("/$", "", name.simulation)
  
  if (.testParam_notNum(resolution) ||
      sum(resolution <= 0) > 0){
    .stopMessage_beInteger("resolution")
  }
  
  all.files = list.files(path = paste0(name.simulation, "/DATA")
                         , pattern = ".tif$"
                         , full.names = TRUE
                         , recursive = TRUE
                         , include.dirs = FALSE)
  if (length(all.files) == 0){
    all.files = list.files(path = paste0(name.simulation, "/DATA")
                           , pattern = ".img$"
                           , full.names = TRUE
                           , recursive = TRUE
                           , include.dirs = FALSE)
    if (length(all.files) == 0){
      all.files = list.files(path = paste0(name.simulation, "/DATA")
                             , pattern = ".asc$"
                             , full.names = TRUE
                             , recursive = TRUE
                             , include.dirs = FALSE)
      if (length(all.files) == 0){
        stop(paste0("Missing data!\n The folder ", name.simulation, "/DATA does not contain adequate files (.tif, .img or .asc)"))
      }
    }
  }
  
  for (fi in all.files)
  {
    ras = raster(fi)
    old.res = unique(res(ras))
    old.proj = projection(ras)
    if (old.res <= resolution)
    {
      if (!is.na(old.proj))
      {
        proj.method = "bilinear"
        if (sum(unique(ras[]) %in% c(0,1)) == 2 || length(unique(ras[])) < 10){
          proj.method = "ngb"
        }
        ras.new = projectRaster(from = ras
                                , res = resolution
                                , crs = old.proj
                                , method = proj.method)
        writeRaster(ras.new, filename = fi, overwrite = TRUE)
        message(paste0("\n The raster file ", fi, " has been successfully upscaled !"))
      } else
      {
        warning(paste0("\n The raster file ", fi, " does not contain projection information. Please check."))
      }
    } else
    {
      warning(paste0("\n The raster file ", fi, " has a coarser resolution (", old.res
                     , ") than the one resquested (", resolution, "). Please check."))
    }
  }
}

