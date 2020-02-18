### HEADER #####################################################################
##' @title Create a graphical representation of several statistics for each PFG 
##' to asses the quality of the model 
##'  \cr for one (or several) specific year of a \code{FATE-HD} simulation
##' 
##' @name POST_FATE.temporalAbund
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce a graphical representation
##' of several statistics (sensitivity, specificity, TSS, AUC) for quality
##' assessment for one (or several) specific \code{FATE-HD} simulation year.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param file.simulParam a \code{string} that corresponds to the name of a
##' parameter file that will be contained into the \code{PARAM_SIMUL} folder
##' of the \code{FATE-HD} simulation
##' @param year an \code{integer} corresponding to the simulation year(s) that 
##' will be used to extract PFG binary maps
##' @param mat.PFG.obs a \code{data.frame} with 4 columns : PFG, X, Y, obs
##' @param opt.ras_habitat default NULL (\emph{optional}). A \code{string} that
##' corresponds to the file name of a raster mask, with an \code{integer} value
##' within each pixel, corresponding to a specific habitat
##' @param opt.no_CPU default 1 (\emph{optional}). The number of resources that 
##' can be used to parallelize the \code{unzip/zip} of raster files
##' @param opt.doPlot default TRUE (\emph{optional}). If TRUE, plot(s) will be
##' processed, otherwise only the calculation and reorganization of outputs
##' will occur, be saved and returned.
##' 
##' 
##' @details 
##' 
##' This function allows one to obtain, for a specific \code{FATE-HD} simulation
##' and a specific parameter file within this simulation, one preanalytical
##' graphic. \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved
##' from the results folder \code{ABUND_REL_perPFG_allStrata} and unzipped.
##' Informations extracted lead to the production of presence/absence maps and 
##' one graphic before the maps are compressed again :
##' 
##' \itemize{
##'   \item{the value of \strong{several statistics for the predictive quality
##'   of the model for each Plant Functional Group} and for each selected
##'   simulation year(s)
##'   }
##' }
##' 
##' Observation records (presences and absences) are required for each PFG 
##' within the \code{mat.PFG.obs} object :
##' 
##' \describe{
##'   \item{\code{PFG}}{the concerned Plant Functional Group}
##'   \item{\code{X} and \code{Y}}{the coordinates of each observations,
##'   matching with the projection of the mask of \code{name.simulation}}
##'   \item{\code{obs}}{either 0 or 1 to indicate presences or absences}
##' }
##' 
##' If a raster mask for habitat has been provided, the graphics will be also
##' done per habitat.
##' 
##' 
##' 
##' @return A \code{data.frame} with the following columns :
##' \describe{
##'   \item{\code{PFG}}{the concerned Plant Functional Group}
##'   \item{\code{AUC.sd}}{standard deviation of the AUC values}
##'   \item{\code{sensitivity.sd}}{standard deviation of the sensitivity values}
##'   \item{\code{specificity.sd}}{standard deviation of the specificity values}
##'   \item{\code{variable}}{name of the calculated statistic among 'sensitivity',
##'   'specificity', 'TSS' and 'AUC'}
##'   \item{\code{value}}{value of the corresponding statistic}
##' }
##' 
##' Two folders are created :
##' \describe{
##'   \item{\file{BIN_perPFG \cr_allStrata}}{containing presence / absence  
##'   raster maps for each PFG across all strata}
##'   \item{\file{BIN_perPFG \cr_perStrata}}{containing presence / absence  
##'   raster maps for each PFG for each stratum}
##' }
##' 
##' One \code{POST_FATE_[...].pdf} file is created : 
##' \describe{
##'   \item{\file{GRAPHIC_C \cr validationStatistics}}{to assess the modeling 
##'   quality of each PFG based on given observations within the studied area}
##' }
##' 
##' 
##' @keywords FATE, outputs, binary, area under curve, sensitivity, specificity,
##' true skill statistic
##' 
##' @seealso \code{\link{POST_FATE.relativeAbund}}
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.temporalAbund(name.simulation = "FATE_simulation"
##'                                        , file.simulParam = "Simul_parameters_V1.txt"
##'                                        , year = 850
##'                                        , mat.PFG.obs = 
##'                                        , opt.no_CPU = 1)
##'                                     
##' POST_FATE.temporalAbund(name.simulation = "FATE_simulation"
##'                                        , file.simulParam = "Simul_parameters_V1.txt"
##'                                        , year = c(850, 950)
##'                                        , mat.PFG.obs = 
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
##' PFG.short = sapply(PFG.names, function(x) strsplit(x, "_")[[1]][1])
##' for (pfg in PFG.names)
##' {
##'   ind = grep(pfg, names(PNE_RESULTS$abund_str.equilibrium))
##'   stk = PNE_RESULTS$abund_str.equilibrium[[ind]]
##'   writeRaster(stk
##'               , filename = paste0(dir2, "/Abund_YEAR_800_", pfg, "_STRATA_"
##'                                   , sub(".*str", "", names(stk)), ".tif")
##'               , overwrite = TRUE
##'               , bylayer = TRUE)
##'   ras = sum(stk)
##'   writeRaster(ras
##'               , filename = paste0(dir1, "/Abund_YEAR_800_", pfg, "_STRATA_all.tif")
##'               , overwrite = TRUE)
##' }
##' 
##' ## Create relative abundance maps
##' POST_FATE.relativeAbund(name.simulation = "FATE_PNE"
##'                         , file.simulParam = "Simul_parameters_V1.txt"
##'                         , year = 800
##'                         , opt.no_CPU = 1)
##' 
##' ## Create binary maps
##' library(reshape2)
##' tab = PNE_PFG$PFG.observations
##' tab = melt(tab, id.vars = c("sites", "X", "Y"))
##' colnames(tab) = c("sites", "X", "Y", "PFG", "obs")
##' tab = tab[, c("PFG", "X", "Y", "obs")]
##' tab = tab[which(tab$PFG != "Others"), ]
##' tab$PFG = sapply(tab$PFG, function(x) names(PFG.short)[which(PFG.short == x)])
##' tab$obs = ifelse(tab$obs > 0, 1, 0)
##' str(tab)
##' 
##' validStats = POST_FATE.temporalAbund(name.simulation = "FATE_PNE"
##'                                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                                     , year = 800
##'                                                     , mat.PFG.obs = tab
##'                                                     , opt.no_CPU = 1)
##' 
##' str(validStats$`FATE_PNE/PARAM_SIMUL/Simul_parameters_V1.txt`$tab$`800`)
##' plot(validStats$`FATE_PNE/PARAM_SIMUL/Simul_parameters_V1.txt`$plot$`800`$ALL)
##'                                                                          
##' 
##' @export
##' 
##' @importFrom foreach foreach
##' @importFrom data.table fwrite
##' @importFrom raster raster stack as.data.frame cellFromXY
##'
##'
## END OF HEADER ###############################################################


POST_FATE.temporalAbund = function(
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
      tabAbund.list = foreach (pfg = PFG) %do%
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
                             , "/RESULTS/POST_FATE_evolution_abundance_PIXEL_"
                             , basename(dir.save)
                             , ".csv")
             , row.names = FALSE)
      
      
      ## get the data inside the rasters ---------------------------------------------
      if (doLight)
      {
        cat("\n GETTING LIGHT for stratum")
        tabLight.list = foreach (str = c(1:no_STRATA)-1) %do%
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
                               , "/RESULTS/POST_FATE_evolution_light_PIXEL_"
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
                                 , "/RESULTS/POST_FATE_evolution_soil_PIXEL_"
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
                     , " > POST_FATE_evolution_abundance_PIXEL_"
                     , basename(dir.save)
                     , ".csv \n"
                     , ifelse(doLight
                            , paste0(" > POST_FATE_evolution_light_PIXEL_"
                                     , basename(dir.save)
                                     , ".csv \n")
                            , "")
                     , ifelse(doSoil
                            , paste0(" > POST_FATE_evolution_soil_PIXEL_"
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
