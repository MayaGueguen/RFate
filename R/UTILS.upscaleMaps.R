### HEADER #####################################################################
##' @title Upscale / crop all raster maps of a \code{FATE} simulation folder
##' 
##' @name .upscaleMaps
##' 
##' @usage
##' .upscaleMaps(name.simulation, resolution)
##' .cropMaps(name.simulation, extent)
##'
##' @author Maya Guéguen
##' 
##' @description These functions scan all the raster files within a 
##' \code{FATE} simulation folder and upscale / crop them to the specified 
##' resolution / extent.
##' 
##' @param name.simulation a \code{string} that corresponds to the main 
##' directory or simulation name of the \code{FATE} simulation
##' @param resolution an \code{integer} that corresponds to the new resolution 
##' to upscale all the maps
##' @param extent a \code{vector} of 4 \code{numeric} values that corresponds 
##' to the new extent to crop all the maps
##' 
##' 
##' @examples 
##'
##' ## Load example data
##' PNE_PARAM = .loadData("PNE_PARAM")
##' 
##' 
##' ## Create a skeleton folder
##' PRE_FATE.skeletonDirectory(name.simulation = "FATE_PNE")
##' 
##' ## Create simulation masks
##' library(raster)
##' writeRaster(PNE_PARAM$masks$maskEcrins
##'             , file = "FATE_PNE/DATA/MASK/mask.tif"
##'             , overwrite = TRUE)
##' writeRaster(PNE_PARAM$masks$noDisturb
##'             , file = "FATE_PNE/DATA/MASK/noDisturb.tif"
##'             , overwrite = TRUE)
##' writeRaster(PNE_PARAM$HS_0
##'             , file = paste0("FATE_PNE/DATA/PFGS/HABSUIT/HS_0_PFG_"
##'                             , 1:nlayers(PNE_PARAM$HS_0), ".tif")
##'             , overwrite = TRUE
##'             , bylayer = TRUE)
##' 
##' 
##' ## Upscale all simulation maps
##' ras1 = raster("FATE_PNE/DATA/MASK/mask.tif")
##' ras2 = raster("FATE_PNE/DATA/PFGS/HABSUIT/HS_0_PFG_3.tif")
##' print(ras1)
##' print(ras2)
##' 
##' .upscaleMaps(name.simulation = "FATE_PNE"
##'              , resolution = 250)
##' 
##' ras1 = raster("FATE_PNE/DATA/MASK/mask.tif")
##' ras2 = raster("FATE_PNE/DATA/PFGS/HABSUIT/HS_0_PFG_3.tif")
##' print(ras1)
##' print(ras2)
##' 
##' ## Crop all simulation maps
##' .cropMaps(name.simulation = "FATE_PNE"
##'           , extent = c(875576, 930000, 1946124, 2000000))
##' 
##' ras1 = raster("FATE_PNE/DATA/MASK/mask.tif")
##' ras2 = raster("FATE_PNE/DATA/PFGS/HABSUIT/HS_0_PFG_3.tif")
##' print(ras1)
##' print(ras2)
##' 
##' 
##' @importFrom raster raster projectRaster writeRaster 
##' res projection extent crop
##'
## END OF HEADER ###############################################################

NULL

##' @export

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
                                , method = proj.method
                                , filename = fi
                                , overwrite = TRUE)
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


##' @export

.cropMaps = function(name.simulation
                    , extent
                    
){
  .testParam_existFolder(name.simulation, "")
  name.simulation = sub("/$", "", name.simulation)
  
  if (.testParam_notNum(extent) ||
      length(extent) != 4){
    stop(paste0("Wrong type of data!\n `extent` must contain 4 numeric values"))
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
    old.proj = projection(ras)
    if (!is.na(old.proj))
    {
      ras.new = crop(x = ras, y = extent, filename = fi, overwrite = TRUE)
      message(paste0("\n The raster file ", fi, " has been successfully cropped !"))
    } else
    {
      warning(paste0("\n The raster file ", fi, " does not contain projection information. Please check."))
    }
  }
}

