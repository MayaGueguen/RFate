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
##' ## ----------------------------------------------------------------------------------------- ##
##' ## Load example data
##' PNE_PARAM = .loadData("PNE_PARAM")
##' PNE_RESULTS = .loadData("PNE_RESULTS")
##' 
##' ## PNE_PARAM$succ_light : data.frame
##' ## PNE_PARAM$strata_limits : vector
##' ## PNE_PARAM$disp : data.frame
##' ## PNE_PARAM$dist : data.frame
##' ## PNE_PARAM$global : vector
##' ## PNE_PARAM$masks : rasterStack
##' ## PNE_RESULTS$abund_str.equilibrium : rasterStack
##' 
##' ## Create a skeleton folder
##' PRE_FATE.skeletonDirectory(name.simulation = "FATE_PNE")
##' 
##' ## Create PFG succession parameter files : predefined of strata limits
##' tab = PNE_PARAM$succ_light[, c("PFG", "type", "height", "maturity", "longevity")]
##' PRE_FATE.params_PFGsuccession(name.simulation = "FATE_PNE"
##'                               , mat.PFG.succ = tab
##'                               , strata.limits = PNE_PARAM$strata_limits
##'                               , strata.limits_reduce = FALSE)
##' 
##' ## Create PFG light parameter files : predefined of strata limits
##' tab = PNE_PARAM$succ_light[, c("PFG", "type", "height", "maturity", "longevity", "light")]
##' PRE_FATE.params_PFGlight(name.simulation = "FATE_PNE"
##'                          , mat.PFG.succ = tab
##'                          , strata.limits = PNE_PARAM$strata_limits
##'                          , strata.limits_reduce = FALSE)
##' 
##' ## Create PFG dispersal parameter files
##' PRE_FATE.params_PFGdispersal(name.simulation = "FATE_PNE"
##'                              , mat.PFG.disp = PNE_PARAM$disp)
##' 
##' ## Create PFG disturbance parameter files
##' PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_PNE"
##'                                , mat.PFG.dist = PNE_PARAM$dist)
##' 
##' ## Create a Global_parameters file
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_PNE"
##'                                  , required.no_PFG = PNE_PARAM$global["NB_FG"]
##'                                  , required.no_STRATA = PNE_PARAM$global["NB_STRATUM"]
##'                                  , required.simul_duration = PNE_PARAM$global["SIMULATION_DURATION"]
##'                                  , required.seeding_duration = PNE_PARAM$global["SEEDING_DURATION"]
##'                                  , required.seeding_timestep = PNE_PARAM$global["SEEDING_TIMESTEP"]
##'                                  , required.seeding_input = PNE_PARAM$global["SEEDING_INPUT"]
##'                                  , required.max_by_cohort = PNE_PARAM$global["MAX_BY_COHORT"]
##'                                  , required.max_abund_low = PNE_PARAM$global["MAX_ABUND_LOW"]
##'                                  , required.max_abund_medium = PNE_PARAM$global["MAX_ABUND_MEDIUM"]
##'                                  , required.max_abund_high = PNE_PARAM$global["MAX_ABUND_HIGH"]
##'                                  , doLight = TRUE
##'                                  , LIGHT.thresh_medium = PNE_PARAM$global["LIGHT.thresh_medium"]
##'                                  , LIGHT.thresh_low = PNE_PARAM$global["LIGHT.thresh_low"]
##'                                  , doDispersal = TRUE
##'                                  , DISPERSAL.mode = PNE_PARAM$global["DISPERSAL.mode"]
##'                                  , doHabSuitability = TRUE
##'                                  , HABSUIT.ref_option = PNE_PARAM$global["HABSUIT.ref_option"]
##'                                  , doDisturbances = TRUE
##'                                  , DIST.no = PNE_PARAM$global["DIST.no"]
##'                                  , DIST.no_sub = PNE_PARAM$global["DIST.no_sub"]
##'                                  , DIST.freq = rep(PNE_PARAM$global["DIST.freq"]
##'                                                    , PNE_PARAM$global["DIST.no"])
##' )
##' 
##' ## Create simulation masks
##' library(raster)
##' writeRaster(PNE_PARAM$masks$maskEcrins
##'             , file = "FATE_PNE/DATA/MASK/mask.tif"
##'             , overwrite = TRUE)
##' writeRaster(PNE_PARAM$masks$noDisturb
##'             , file = "FATE_PNE/DATA/MASK/noDisturb.tif"
##'             , overwrite = TRUE)
##' 
##' ## Create simulation parameters file
##' PRE_FATE.params_simulParameters(name.simulation = "FATE_PNE"
##'                                 , name.mask = "mask.tif"
##'                                 , name.dist = "noDisturb.tif")
##' 
##' ## Create results folders
##' name.folder = "FATE_PNE"
##' name.simul = "SIMUL_V1"
##' dir1 = paste0(name.folder, "/RESULTS/", name.simul, "/ABUND_perPFG_allStrata")
##' dir2 = paste0(name.folder, "/RESULTS/", name.simul, "/ABUND_perPFG_perStrata")
##' 
##' dir.create(dir1, recursive = TRUE)
##' dir.create(dir2, recursive = TRUE)
##' 
##' ## Create results files
##' PFG.names = PNE_PARAM$succ_light$PFG
##' for (pfg in PFG.names)
##' {
##'   ind = grep(pfg, names(PNE_RESULTS$abund_str.equilibrium))
##'   stk = PNE_RESULTS$abund_str.equilibrium[[ind]] * 1
##'   ras = sum(stk)
##'   writeRaster(ras
##'               , filename = paste0(dir1, "/Abund_YEAR_800_", pfg, "_STRATA_all.tif")
##'               , overwrite = TRUE)
##' }
##' 
##' ## Create relative abundance maps
##' POST_FATE.relativeAbund(name.simulation = name.folder
##'                                         , file.simulParam = "Simul_parameters_V1.txt"
##'                                         , year = 800
##'                                         , opt.no_CPU = 1)
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
  name.simulation = sub("/$", "", name.simulation)
  
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
    .getGraphics_results(name.simulation  = name.simulation
                         , abs.simulParam = abs.simulParam)

    ## Get number of PFGs ----------------------------------------------------------
    ## Get PFG names ---------------------------------------------------------------
    .getGraphics_PFG(name.simulation  = name.simulation
                     , abs.simulParam = abs.simulParam)
    
    ## Get raster mask -------------------------------------------------------------
    .getGraphics_mask(name.simulation  = name.simulation
                      , abs.simulParam = abs.simulParam)
    
    
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
    .zip(folder_name = dir.output.perPFG.allStrata
         , list_files = raster.perPFG.allStrata
         , nb_cores = opt.no_CPU)
    
  }
}

