### HEADER #####################################################################
##' @title Create tables of pixel temporal evolution of PFG abundances (and 
##' light and soil resources if activated) for a \code{FATE-HD} simulation
##' 
##' @name POST_FATE.temporalEvolution
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce from 1 to 3 tables 
##' containing pixel temporal evolution of PFG abundances as well as light and 
##' soil resources if those modules were activated in a \code{FATE-HD} 
##' simulation.
##' 
##'              
##' @param name.simulation a \code{string} that corresponds to the main 
##' directory or simulation name of the \code{FATE-HD} simulation
##' @param file.simulParam a \code{string} that corresponds to the name of a 
##' parameter file that will be contained into the \code{PARAM_SIMUL} folder 
##' of the \code{FATE-HD} simulation
##' @param no.years an \code{integer} corresponding to the number of simulation 
##' years that will be used to extract PFG abundance / light / soil maps
##' @param opt.ras_habitat default \code{NULL} (\emph{optional}). A \code{string} that 
##' corresponds to the file name of a raster mask, with an \code{integer} value 
##' within each pixel, corresponding to a specific habitat
##' @param opt.no_CPU default \code{1} (\emph{optional}). The number of 
##' resources that can be used to parallelize the \code{unzip/zip} of raster 
##' files
##' 
##' 
##' @details 
##' 
##' This function allows one to obtain, for a specific \code{FATE-HD} 
##' simulation and a specific parameter file within this simulation, one to 
##' three preanalytical tables that can then be used to create graphics.
##' \cr \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved 
##' from the results folder \code{ABUND_perPFG_allStrata} and unzipped.
##' Informations extracted lead to the production of one table before the maps 
##' are compressed again :
##' 
##' \itemize{
##'   \item{the value of \strong{abundance for each Plant Functional Group} 
##'   for each selected simulation year(s) in every pixel in which the PFG is 
##'   present for at least one of the selected simulation year(s) \cr \cr
##'   }
##' }
##' 
##' IF the \code{LIGHT} module was activated (see 
##' \code{\link{PRE_FATE.params_globalParameters}}), for each height stratum 
##' and each selected simulation year, raster maps are retrieved from the 
##' results folder \code{LIGHT} and unzipped.
##' Informations extracted lead to the production of one table before the maps 
##' are compressed again :
##' 
##' \itemize{
##'   \item{the value of \strong{light resources for each height stratum} for 
##'   each selected simulation year(s) in every pixel \cr \cr
##'   }
##' }
##' 
##' IF the \code{SOIL} module was activated (see 
##' \code{\link{PRE_FATE.params_globalParameters}}), for each selected 
##' simulation year, raster maps are retrieved from the results folder 
##' \code{SOIL} and unzipped.
##' Informations extracted lead to the production of one table before the maps 
##' are compressed again :
##' 
##' \itemize{
##'   \item{the value of \strong{soil resources} for each selected simulation 
##'   year(s) in every pixel \cr \cr
##'   }
##' }
##' 
##' If a raster mask for habitat has been provided, the tables will also 
##' contain information about the pixel habitat. \cr \cr
##' 
##' 
##' \strong{These \code{.csv} files can then be used by other functions} :
##' 
##' \itemize{
##'   \item to produce graphics of temporal evolution of modelled abundances 
##'   and space occupancy at the whole area level \cr (see 
##'   \code{\link{POST_FATE.graphic_evolutionCoverage}})
##'   \item to produce graphics of temporal evolution of modelled abundances 
##'   and / or resources at the pixel level \cr (see 
##'   \code{\link{POST_FATE.graphic_evolutionPixels}})
##' }
##' 
##' 
##' 
##' @return A \code{list} containing three \code{data.frame} objects with the 
##' following columns :
##' \describe{
##'   \item{\code{PFG}}{the concerned Plant Functional Group (for abundance)}
##'   \item{\code{STRATUM}}{the concerned height stratum (for LIGHT)}
##'   \item{\code{ID}}{the concerned pixel}
##'   \item{\code{X, Y}}{the coordinates of the concerned pixel}
##'   \item{\code{HAB}}{the habitat of the concerned pixel}
##'   \item{\emph{years}}{values of the corresponding object for each 
##'   selected simulation year(s)}
##' }
##' 
##' One to three \code{POST_FATE_TABLE_PIXEL_evolution_[...].csv} files are created : 
##' \describe{
##'   \item{\file{abundance}}{always}
##'   \item{\file{light}}{\emph{if LIGHT module was activated}}
##'   \item{\file{soil}}{\emph{if SOIL module was activated}}
##' }
##' 
##' 
##' @keywords FATE, outputs, temporal evolution
##' 
##' @seealso \code{\link{PRE_FATE.params_globalParameters}},
##' \code{\link{POST_FATE.graphic_evolutionCoverage}},
##' \code{\link{POST_FATE.graphic_evolutionPixels}}
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.temporalEvolution(name.simulation = "FATE_simulation"
##'                                        , file.simulParam = "Simul_parameters_V1.txt"
##'                                        , opt.no_CPU = 1)
##'                                     
##' POST_FATE.temporalEvolution(name.simulation = "FATE_simulation"
##'                                        , file.simulParam = "Simul_parameters_V1.txt"
##'                                        , no.years = 50
##'                                        , opt.no_CPU = 1)
##' }
##'                                                         
##'                                                         
##'                                                         
##' ## ----------------------------------------------------------------------------------------- ##
##' ## Load example data
##' PNE_PFG = .loadData("PNE_PFG")
##' PNE_PARAM = .loadData("PNE_PARAM")
##' PNE_RESULTS = .loadData("PNE_RESULTS")
##' 
##' ## PNE_PFG$PFG.observations : data.frame
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
##' dir3 = paste0(name.folder, "/RESULTS/", name.simul, "/LIGHT")
##' dir4 = paste0(name.folder, "/RESULTS/", name.simul, "/SOIL")
##' 
##' dir.create(dir1, recursive = TRUE)
##' dir.create(dir2, recursive = TRUE)
##' dir.create(dir3, recursive = TRUE)
##' dir.create(dir4, recursive = TRUE)
##' 
##' ## Create results files
##' PFG.names = PNE_PARAM$succ_light$PFG
##' PFG.short = sapply(PFG.names, function(x) strsplit(x, "_")[[1]][1])
##' for (pfg in PFG.names)
##' {
##'   ind = grep(pfg, names(PNE_RESULTS$abund_str.equilibrium))
##'   stk = PNE_RESULTS$abund_str.equilibrium[[ind]]
##'   ras = sum(stk)
##'   writeRaster(ras
##'               , filename = paste0(dir1, "/Abund_YEAR_800_", pfg, "_STRATA_all.tif")
##'               , overwrite = TRUE)
##'               
##'   for (ye in seq(100, 700, 100))
##'   {
##'     file.copy(from = paste0(dir1, "/Abund_YEAR_800_", pfg, "_STRATA_all.tif")
##'               , to = paste0(dir1, "/Abund_YEAR_", ye, "_", pfg, "_STRATA_all.tif"))
##'   }
##' }
##' 
##' 
##' ## Create temporal table
##' 
##' tempEvol = POST_FATE.temporalEvolution(name.simulation = "FATE_PNE"
##'                                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                                     , opt.no_CPU = 1)
##' 
##' str(tempEvol$`FATE_PNE/PARAM_SIMUL/Simul_parameters_V1.txt`)
##'                                                                          
##' 
##' @export
##' 
##' @importFrom foreach foreach
##' @importFrom data.table rbindlist fwrite
##' @importFrom raster raster stack as.data.frame cellFromXY
##' @importFrom doParallel registerDoParallel
##'
##'
## END OF HEADER ###############################################################


POST_FATE.temporalEvolution = function(
  name.simulation
  , file.simulParam = NULL
  , no.years = 10
  , opt.ras_habitat = NULL
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
  if (!.testParam_notDef(opt.ras_habitat))
  {
    if (nchar(opt.ras_habitat) > 0)
    {
      .testParam_existFile(opt.ras_habitat)
      ras.habitat = raster(opt.ras_habitat)
    }
  }
  
  #################################################################################################
  
  res = foreach (abs.simulParam = abs.simulParams) %do%
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
      
      ## Get habitat information -----------------------------------------------------
      if (exists("ras.habitat"))
      {
        ras.habitat = ras.habitat * ras.mask
      }
      
      ## Get list of arrays and extract years of simulation --------------------------
      raster.perPFG.allStrata = grep("Abund_", list.files(dir.output.perPFG.allStrata), value = TRUE)
      if (length(raster.perPFG.allStrata) == 0)
      {
        stop(paste0("Missing data!\n The folder ", dir.output.perPFG.allStrata, " does not contain adequate files"))
      }
      years = sapply(sub("Abund_YEAR_", "", raster.perPFG.allStrata)
                     , function(x) strsplit(as.character(x), "_")[[1]][1])
      years = sort(unique(as.numeric(years)))
      years = years[round(seq(1, length(years), length.out = min(no.years, length(years))))]
      no_years = length(years)
      
      ## UNZIP the raster saved ------------------------------------------------------
      .unzip_ALL(folder_name = dir.output.perPFG.allStrata, nb_cores = opt.no_CPU)
      if (doLight) .unzip_ALL(folder_name = dir.output.light, nb_cores = opt.no_CPU)
      if (doSoil) .unzip_ALL(folder_name = dir.output.soil, nb_cores = opt.no_CPU)
      
      ## get the data inside the rasters ---------------------------------------------
      cat("\n GETTING ABUNDANCE for pfg")
      if (opt.no_CPU > 1)
      {
        registerDoParallel(cores = opt.no_CPU)
      }
      tabAbund.list = foreach (pfg = PFG) %dopar%
        {
          cat(" ", pfg)
          file_name = paste0(dir.output.perPFG.allStrata,
                             "Abund_YEAR_",
                             years,
                             "_",
                             pfg,
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
          # if (length(which(file.exists(file_name))) == 0)
          # {
          #   stop(paste0("Missing data!\n The names of PFG extracted from files within ", name.simulation, "/DATA/PFGS/SUCC/ : "
          #               , paste0("\n", PFG, collapse = "\n")
          #               , "\n is different from the files contained in ", dir.output.perPFG.allStrata
          #               , "\n They should be : "
          #               , paste0("\n", file_name, collapse = "\n")))
          # }
          ye = years[which(file.exists(file_name))]
          file_name = file_name[which(file.exists(file_name))]
          
          if (length(file_name) > 0)
          {
            ras = stack(file_name) * ras.mask
            ras.df = rasterToPoints(ras)
            ras.df = as.data.frame(ras.df)
            colnames(ras.df) = c("X", "Y", ye)
            ID.abund = rowSums(ras.df[, 3:ncol(ras.df)])
            ras.df = ras.df[which(ID.abund > 0), ]
            ras.df$ID = cellFromXY(ras.mask, ras.df[, c("X", "Y")])
            ras.df$PFG = pfg
            
            if (exists("ras.habitat"))
            {
              ras.df$HAB = cellFromXY(ras.habitat, ras.df[, c("X", "Y")])
            } else
            {
              ras.df$HAB = "ALL"
            }
            ras.df = ras.df[, c("PFG", "ID", "X", "Y", "HAB", ye)]
            
            return(ras.df)
          }
        } ## END loop on PFG
      cat("\n")
      
      tabAbund = rbindlist(tabAbund.list, fill = TRUE)
      tabAbund = as.data.frame(tabAbund)
      
      fwrite(tabAbund
             , file = paste0(name.simulation
                             , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_"
                             , basename(dir.save)
                             , ".csv")
             , row.names = FALSE)
      
      
      ## get the data inside the rasters ---------------------------------------------
      if (doLight)
      {
        cat("\n GETTING LIGHT for stratum")
        if (opt.no_CPU > 1)
        {
          registerDoParallel(cores = opt.no_CPU)
        }
        tabLight.list = foreach (str = c(1:no_STRATA)-1) %dopar%
          {
            cat(" ", str)
            file_name = paste0(dir.output.light,
                               "Light_Resources_YEAR_",
                               years,
                               "_STRATA_",
                               str)
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
            ye = years[which(file.exists(file_name))]
            file_name = file_name[which(file.exists(file_name))]
            
            if (length(file_name) > 0)
            {
              ras = stack(file_name) * ras.mask
              ras.df = rasterToPoints(ras)
              ras.df = as.data.frame(ras.df)
              colnames(ras.df) = c("X", "Y", ye)
              ras.df$ID = cellFromXY(ras.mask, ras.df[, c("X", "Y")])
              ras.df$STRATUM = str
              
              if (exists("ras.habitat"))
              {
                ras.df$HAB = cellFromXY(ras.habitat, ras.df[, c("X", "Y")])
              } else
              {
                ras.df$HAB = "ALL"
              }
              ras.df = ras.df[, c("STRATUM", "ID", "X", "Y", "HAB", ye)]
              
              return(ras.df)
            }
          } ## END loop on STRATUM
        cat("\n")
        
        tabLight = rbindlist(tabLight.list, fill = TRUE)
        tabLight = as.data.frame(tabLight)
        
        fwrite(tabLight
               , file = paste0(name.simulation
                               , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_light_"
                               , basename(dir.save)
                               , ".csv")
               , row.names = FALSE)
      } else
      {
        tabLight = NA
      } ## END loop for light
      
      
      ## get the data inside the rasters ---------------------------------------------
      if (doSoil)
      {
        cat("\n GETTING SOIL")
        file_name = paste0(dir.output.soil,
                           "Soil_Resources_YEAR_",
                           years)
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
        ye = years[which(file.exists(file_name))]
        file_name = file_name[which(file.exists(file_name))]
        
        if (length(file_name) > 0)
        {
          ras = stack(file_name) * ras.mask
          ras.df = rasterToPoints(ras)
          ras.df = as.data.frame(ras.df)
          colnames(ras.df) = c("X", "Y", ye)
          ras.df$ID = cellFromXY(ras.mask, ras.df[, c("X", "Y")])
          
          if (exists("ras.habitat"))
          {
            ras.df$HAB = cellFromXY(ras.habitat, ras.df[, c("X", "Y")])
          } else
          {
            ras.df$HAB = "ALL"
          }
          tabSoil = ras.df[, c("ID", "X", "Y", "HAB", ye)]
          
          fwrite(tabSoil
                 , file = paste0(name.simulation
                                 , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_soil_"
                                 , basename(dir.save)
                                 , ".csv")
                 , row.names = FALSE)
        }
      } else
      {
        tabSoil = NA
      } ## END loop for soil
      cat("\n")
      
      
      ## ZIP the raster saved ------------------------------------------------------
      .zip_ALL(folder_name = dir.output.perPFG.allStrata, nb_cores= opt.no_CPU)
      if (doLight) .zip_ALL(folder_name = dir.output.light, nb_cores = opt.no_CPU)
      if (doSoil) .zip_ALL(folder_name = dir.output.soil, nb_cores = opt.no_CPU)
      
      cat("\n> Done!\n")
      cat("\n")
      
      message(paste0("\n The output files \n"
                     , " > POST_FATE_TABLE_PIXEL_evolution_abundance_"
                     , basename(dir.save)
                     , ".csv \n"
                     , ifelse(doLight
                            , paste0(" > POST_FATE_TABLE_PIXEL_evolution_light_"
                                     , basename(dir.save)
                                     , ".csv \n")
                            , "")
                     , ifelse(doSoil
                            , paste0(" > POST_FATE_TABLE_PIXEL_evolution_soil_"
                                     , basename(dir.save)
                                     , ".csv \n")
                            , "")
                     , "have been successfully created !\n"))
      
      return(list(tab.abundance = tabAbund
                  , tab.light = tabLight
                  , tab.soil = tabSoil))
    } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}
