### HEADER #####################################################################
##' @title Create relative abundance maps for each Plant Functional
##' Group \cr for one (or several) specific year of a \code{FATE-HD} simulation
##' 
##' @name POST_FATE.relativeAbund
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce raster maps of PFG simulated
##' relative abundances for one (or several) specific \code{FATE-HD} 
##' simulation year.
##'              
##' @param name.simulation a \code{string} that corresponds to the main
##' directory or simulation name of the \code{FATE-HD} simulation
##' @param file.simulParam a \code{string} that corresponds to the name of a
##' parameter file that will be contained into the \code{PARAM_SIMUL} folder
##' of the \code{FATE-HD} simulation
##' @param year an \code{integer} corresponding to the simulation year(s) that 
##' will be used to extract PFG abundance maps
##' @param opt.no_CPU default 1 (\emph{optional}). The number of resources that 
##' can be used to parallelize the \code{unzip/zip} of raster files
##' 
##' 
##' @details 
##' 
##' This function allows one to obtain, for a specific \code{FATE-HD} simulation
##' and a specific parameter file within this simulation, raster maps of PFG
##' relative abundance. \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved
##' from the results folder \code{ABUND_perPFG_perStrata} and unzipped.
##' Informations extracted lead to the production of the same number of raster
##' before the maps are compressed again :
##' 
##' \enumerate{
##'   \item{for each selected simulation year(s), \strong{relative abundances} 
##'   within each stratum are calculated :
##'   \deqn{\frac{\text{Abund } PFG_i \text{ } Stratum_j}
##'   {\Sigma \text{ Abund } PFG_{all} \text{ } Stratum_j}}}
##' }
##' 
##' 
##' @return One result folder is created :
##' \describe{
##'   \item{\file{ABUND_REL_perPFG \cr_allStrata}}{containing relative abundance 
##'   raster maps for each PFG across all strata}
##' }
##' 
##' 
##' @keywords FATE, outputs, relative abundance
##' 
##' @seealso \code{\link{POST_FATE.graphic_evolutionCoverage}},
##' \code{\link{POST_FATE.graphic_mapPFGvsHS}},
##' \code{\link{POST_FATE.graphic_mapPFGrichness}},
##' \code{\link{POST_FATE.graphic_mapPFGcover}},
##' \code{\link{POST_FATE.graphic_validationStatistics}}
##' 
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
##'                                         , file.simulParam = "Simul_parameters_V1.txt"
##'                                         , year = 850
##'                                         , opt.no_CPU = 1)
##'                                     
##' POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
##'                                         , file.simulParam = "Simul_parameters_V1.txt"
##'                                         , year = c(850, 950)
##'                                         , opt.no_CPU = 1)
##' }
##'                                     
##'                                     
##' 
##' @export
##' 
##' @importFrom foreach foreach
##' @importFrom raster raster stack as.data.frame
##' rasterToPoints writeRaster
##'
## END OF HEADER ###############################################################


POST_FATE.relativeAbund = function(
  name.simulation
  , file.simulParam = NULL
  , year
  , opt.no_CPU = 1
){
  .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation, "RESULTS/")
  .testParam_existFolder(name.simulation, "DATA/")
  name.simulation = sub("/", "", name.simulation)
  
  if (.testParam_notDef(file.simulParam) || nchar(file.simulParam) == 0)
  {
    abs.simulParams = list.files(paste0(name.simulation, "/PARAM_SIMUL/"))
    if (length(abs.simulParams) == 0)
    {
      stop(paste0("Missing data!\n The folder ", name.simulation, "/PARAM_SIMUL/ does not contain adequate files"))
    }
    abs.simulParams = paste0(name.simulation, "/PARAM_SIMUL/", abs.simulParams)
  } else
  {
    file.simulParam = basename(file.simulParam)
    abs.simulParams = paste0(name.simulation, "/PARAM_SIMUL/", file.simulParam)
    .testParam_existFile(abs.simulParams)
  }
  if (.testParam_notNum(year))
  {
    .stopMessage_beInteger("year")
  }

  #################################################################################################
  
  for (abs.simulParam in abs.simulParams)
  {
    
    cat("\n ############## GRAPHIC POST FATE ############## \n")
    cat("\n Simulation name : ", name.simulation)
    cat("\n Simulation file : ", abs.simulParam)
    cat("\n")
    
    ## Get results directories -----------------------------------------------------
    # .getGraphics_results(name.simulation, abs.simulParam)
    # 
    # ## Get number of PFGs ----------------------------------------------------------
    # ## Get PFG names ---------------------------------------------------------------
    # .getGraphics_PFG(name.simulation, abs.simulParam)
    dir.save = .getParam(params.lines = abs.simulParam
                         , flag = "SAVE_DIR"
                         , flag.split = "^--.*--$"
                         , is.num = FALSE)
    .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/"))

    dir.output.perPFG.allStrata = paste0(name.simulation, "/RESULTS/", basename(dir.save), "/ABUND_perPFG_allStrata/")
    .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/ABUND_perPFG_allStrata/"))

    dir.output.perPFG.allStrata.REL = paste0(name.simulation, "/RESULTS/", basename(dir.save), "/ABUND_REL_perPFG_allStrata/")
    if (!dir.exists(dir.output.perPFG.allStrata.REL))
    {
      dir.create(path = dir.output.perPFG.allStrata.REL)
    }
    
    ## Get number of PFGs ----------------------------------------------------------
    file.globalParam = .getParam(params.lines = abs.simulParam
                                 , flag = "GLOBAL_PARAMS"
                                 , flag.split = "^--.*--$"
                                 , is.num = FALSE)
    no_PFG = .getParam(params.lines = file.globalParam
                       , flag = "NB_FG"
                       , flag.split = " "
                       , is.num = TRUE)
    if (length(no_PFG) == 0 || .testParam_notNum(no_PFG))
    {
      stop(paste0("Missing data!\n The number of PFG (NB_FG) within ", file.globalParam, " does not contain any value"))
    }

    ## Get PFG names ---------------------------------------------------------------
    PFG = .getParam(params.lines = abs.simulParam
                    , flag = "PFG_LIFE_HISTORY_PARAMS"
                    , flag.split = "^--.*--$"
                    , is.num = FALSE)
    pattern = ".*SUCC_"
    PFG = sub(".txt", "", sub(pattern, "", PFG))
    if (length(PFG) != no_PFG)
    {
      stop(paste0("Missing data!\n The number of PFG (NB_FG) within ", file.globalParam
                  , " is different from the number of PFG files contained in ", name.simulation, "/DATA/PFGS/SUCC/"))
    }
    
    ## Get raster mask -------------------------------------------------------------
    file.mask = .getParam(params.lines = abs.simulParam
                          , flag = "MASK"
                          , flag.split = "^--.*--$"
                          , is.num = FALSE)
    .testParam_existFile(file.mask)
    
    ras.mask = raster(file.mask)
    ras.mask[which(ras.mask[] == 0)] = NA
    
    ## Get list of arrays and extract years of simulation --------------------------
    years = sort(unique(as.numeric(year)))
    no_years = length(years)
    
    ## UNZIP the raster saved ------------------------------------------------------
    raster.perPFG.allStrata = grep(paste0("Abund_YEAR_", years, "_", collapse = "|")
                                   , list.files(dir.output.perPFG.allStrata, full.names = TRUE)
                                   , value = TRUE)
    if (length(raster.perPFG.allStrata) == 0)
    {
      stop(paste0("Missing data!\n The folder ", dir.output.perPFG.allStrata, " does not contain adequate files"))
    }

    .unzip(folder_name = dir.output.perPFG.allStrata
           , list_files = raster.perPFG.allStrata
           , nb_cores = opt.no_CPU)
    
    
    ## get the data inside the rasters ---------------------------------------------
    cat("\n GETTING RELATIVE ABUNDANCES for year")
    for (y in years)
    {
      cat(" ", y)
      
      file_name = paste0(dir.output.perPFG.allStrata,
                         "Abund_YEAR_",
                         y,
                         "_",
                         PFG,
                         "_STRATA_all")
      if (length(which(file.exists(paste0(file_name, ".tif")))) > 0)
      {
        file_name = paste0(file_name, ".tif")
      } else if (length(which(file.exists(paste0(file_name, ".img")))) > 0)
      {
        file_name = paste0(file_name, ".img")
      } else if (length(which(file.exists(paste0(file_name, ".asc")))) > 0)
      {
        file_name = paste0(file_name, ".asc")
      }
      if (length(which(file.exists(file_name))) == 0)
      {
        stop(paste0("Missing data!\n The names of PFG extracted from files within ", name.simulation, "/DATA/PFGS/SUCC/ : "
                    , paste0("\n", PFG, collapse = "\n")
                    , "\n is different from the files contained in ", dir.output.perPFG.allStrata
                    , "They should be : "
                    , paste0("\n", file_name, collapse = "\n")))
      }
      gp = PFG[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      if (length(file_name) > 0)
      {
        ras = stack(file_name) * ras.mask
        ras_REL = ras / sum(ras)
        names(ras_REL) = gp
        
        new_name = paste0(dir.output.perPFG.allStrata.REL
                          , "Abund_relative_YEAR_"
                          , y
                          , "_"
                          , names(ras_REL)
                          , "_STRATA_all.tif")
        
        writeRaster(x = ras_REL
                    , filename = new_name
                    , overwrite = TRUE
                    , bylayer = TRUE)
      }
    } ## end loop on years
    
    ## ZIP the raster saved ------------------------------------------------------
    .zip(folder_name = dir.output.perPFG.allStrata, nb_cores = opt.no_CPU)
    
  }
}

