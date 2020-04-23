### HEADER #####################################################################
##' @title Create binary maps for each Plant Functional Group for one (or 
##' several) specific year of a \code{FATE} simulation
##' 
##' @name POST_FATE.binaryMaps
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce raster maps of PFG presence 
##' / absence for one (or several) specific \code{FATE} simulation year.
##' 
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param years an \code{integer}, or a \code{vector} of \code{integer}, 
##' corresponding to the simulation year(s) that will be used to extract PFG 
##' abundance maps
##' @param method an \code{integer} to choose the transformation method : \cr 
##' \code{1} (relative abundance) or \code{2} (optimizing TSS) (see 
##' \code{\href{POST_FATE.binaryMaps#details}{Details}})
##' @param method1.threshold default \code{0.05}. \cr If \code{method = 1}, 
##' minimum relative abundance required for each PFG to be considered as present 
##' in the concerned pixel 
##' @param method2.cutoff default \code{NULL}. \cr If \code{method = 2}, a 
##' \code{data.frame} with 3 columns : \code{year}, \code{PFG}, \code{cutoff} \cr
##' (see \code{\href{POST_FATE.binaryMaps#details}{Details}})
##' @param opt.no_CPU (\emph{optional}) default \code{1}. \cr The number of 
##' resources that can be used to parallelize the \code{unzip/zip} of raster 
##' files
##' 
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE} simulation and 
##' a specific parameter file within this simulation, \strong{raster maps of PFG 
##' presence / absence}. \cr \cr
##' 
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved 
##' from the results folder \code{ABUND_REL_perPFG_allStrata} and lead to the 
##' production of as many maps as those found :
##' 
##' \describe{
##'   \item{\code{1} relative abundance}{relative abundance maps are transformed 
##'   into binary maps according to the threshold given by 
##'   \code{method1.threshold} : 
##'   \deqn{abund\_rel_{\text{ PFG}_i} > \text{method1.threshold} \;\; 
##'   \Leftrightarrow \;\; 1}}
##'   \item{\code{2} optimizing TSS}{relative abundance maps are transformed 
##'   into binary maps according to the \code{\href{.getCutoff}{cutoff}} found 
##'   with the \code{\link{POST_FATE.graphic_validationStatistics}} function : 
##'   \deqn{abund\_rel_{\text{ PFG}_i} > \text{method2.cutoff}_{\text{ PFG}_i} \;\; 
##'   \Leftrightarrow \;\; 1}}
##' }
##' 
##' Binary maps per stratum are obtained by multiplying raster maps from 
##' \code{ABUND_perPFG_perStrata} folder by corresponding raster maps from 
##' \code{BIN_perPFG_allStrata} folder.
##' 
##' \strong{It requires} that the \code{\link{POST_FATE.relativeAbund}} 
##' function has been run and that the folder \code{ABUND_REL_perPFG_allStrata} 
##' exists. \cr If \code{method = 2}, it requires that the 
##' \code{\link{POST_FATE.graphic_validationStatistics}} function has been run. 
##' \cr \cr
##' 
##' \strong{These binary \code{raster} files can then be used by other 
##' functions} :
##' 
##' \itemize{
##'   \item to produce graphics of \emph{PFG modelled presence} \code{vs} 
##'   \emph{PFG Habitat Suitability} maps \cr (see 
##'   \code{\link{POST_FATE.graphic_mapPFGvsHS}})
##' }
##' 
##' 
##' @return 
##' Two folders are created :
##' \describe{
##'   \item{\file{BIN_perPFG \cr_allStrata}}{containing presence / absence 
##'   raster maps for each PFG across all strata}
##'   \item{\file{BIN_perPFG \cr_perStrata}}{containing presence / absence 
##'   raster maps for each PFG for each stratum}
##' }
##' 
##' 
##' @keywords FATE, outputs, binary
##' 
##' @seealso \code{\link{POST_FATE.relativeAbund}},
##' \code{\link{POST_FATE.graphic_validationStatistics}}
##' \code{\link{.getCutoff}},
##' \code{\link{POST_FATE.graphic_mapPFGvsHS}}
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
##'                                        , file.simulParam = "Simul_parameters_V1.txt"
##'                                        , year = 850
##'                                        , mat.PFG.obs = 
##'                                        , opt.no_CPU = 1)
##'                                     
##' POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
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
##' validStats = POST_FATE.binaryMaps(name.simulation = "FATE_PNE"
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
##' @importFrom foreach foreach %do%
##' @importFrom raster stack writeRaster
##'
## END OF HEADER ###############################################################


POST_FATE.binaryMaps = function(
  name.simulation
  , file.simulParam = NULL
  , years
  , method
  , method1.threshold = 0.05
  , method2.cutoff = NULL
  , opt.no_CPU = 1
){
  
  #############################################################################
  
  ## CHECK parameter name.simulation
  .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation, "RESULTS/")
  .testParam_existFolder(name.simulation, "DATA/")
  name.simulation = sub("/", "", name.simulation)
  ## CHECK parameter file.simulParam
  abs.simulParams = .getParam_abs.simulParams(file.simulParam, name.simulation)
  ## CHECK parameter years
  .testParam_notInteger.m("years", years)
  ## CHECK parameter method
  .testParam_notInValues.m("method", method, c(1, 2))
  ## CHECK parameter method1.threshold
  if (method == 1)
  {
    .testParam_notBetween.m("method1.threshold", method1.threshold, 0, 1)
  }
  ## CHECK parameter method2.cutoff
  if (method == 2)
  {
    if (is.null(method2.cutoff))
    {
      valid.files = list.files(paste0(name.simulation, "/RESULTS/")
                               , pattern = paste0("POST_FATE_TABLE_YEAR_"
                                                  , years, "_validationStatistics"
                                                  , collapse = "|")
                               , full.names = FALSE)
      if (length(valid.files) == 0)
      {
        stop(paste0("Missing data!\n The folder ", name.simulation
                    , "/RESULTS/ does not contain adequate files "
                    , "(starting by `POST_FATE_TABLE_YEAR_[...]_validationStatistics`)"))
      }
    } else if (is.data.frame(method2.cutoff))
    {
      if (nrow(method2.cutoff) == 0 || ncol(method2.cutoff) != 3)
      {
        .stopMessage_numRowCol("method2.cutoff", c("year", "PFG", "cutoff"))
      } else
      {
        if (.testParam_notColnames(method2.cutoff, c("year", "PFG", "cutoff"))){
          .stopMessage_columnNames("method2.cutoff", c("year", "PFG", "cutoff"))
        }
      }
      mat.cutoff = method2.cutoff
    } else
    {
      stop("Wrong type of data!\n `method2.cutoff` must be either NULL or a data.frame")
    }
  }
  
  
  #############################################################################
  
  res = foreach (abs.simulParam = abs.simulParams) %do%
  {
    
    cat("\n ############## GRAPHIC POST FATE ############## \n")
    cat("\n Simulation name : ", name.simulation)
    cat("\n Simulation file : ", abs.simulParam)
    cat("\n")
    
    ## Get results directories ------------------------------------------------
    .getGraphics_results(name.simulation  = name.simulation
                         , abs.simulParam = abs.simulParam)
    
    ## Get number of PFGs -----------------------------------------------------
    ## Get PFG names ----------------------------------------------------------
    .getGraphics_PFG(name.simulation  = name.simulation
                     , abs.simulParam = abs.simulParam)
    
    ## Get raster mask --------------------------------------------------------
    .getGraphics_mask(name.simulation  = name.simulation
                      , abs.simulParam = abs.simulParam)
    
    ## Get list of arrays and extract years of simulation ---------------------
    years = sort(unique(as.numeric(years)))
    no_years = length(years)
    
    if (method == 1)
    {
      mat.cutoff = expand.grid(year = years
                               , PFG = PFG
                               , cutoff = method1.threshold
                               , stringsAsFactors = FALSE)
    } else if (exists("valid.files"))
    {
      valid.files = valid.files[grep(basename(dir.save), valid.files)]
      if (length(valid.files) > 0)
      {
        mat.cutoff = foreach(fi = valid.files, .combine = "rbind") %do%
        {
          ye = sub("_validationStatistics.*", "", fi)
          ye = sub("POST_FATE_TABLE_YEAR_", "", ye)
          tab = fread(paste0(name.simulation, "/RESULTS/", fi))
          tab = unique(tab[, c("PFG", "cutoff")])
          tab$year = as.numeric(ye)
          return(tab)
        }
      } else
      {
        stop(paste0("Missing data!\n The folder ", name.simulation
                    , "/RESULTS/ does not contain adequate files "
                    , "(starting by `POST_FATE_TABLE_YEAR_[...]_validationStatistics` "
                    , "and corresponding to `", basename(dir.save), "` folder)"))
      }
    }
    
    ## UNZIP the raster saved -------------------------------------------------
    raster.perPFG.perStrata = .getRasterNames(years, "perStrata", "ABUND")
    .unzip(folder_name = dir.output.perPFG.perStrata
           , list_files = raster.perPFG.perStrata
           , nb_cores = opt.no_CPU)
    
    
    
    ## get the data inside the rasters ----------------------------------------
    cat("\n GETTING PRESENCE/ABSENCE maps for")
    for (y in years)
    {
      cat("\n > year", y)
      
      file_name = paste0(dir.output.perPFG.allStrata.REL
                         , "Abund_relative_YEAR_"
                         , y
                         , "_"
                         , PFG
                         , "_STRATA_all.tif")
      gp = PFG[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      if (length(file_name) > 0)
      {
        ras = stack(file_name) * ras.mask
        names(ras) = gp
        
        for (fg in gp)
        {
          ## Produce binary maps ------------------------------------
          ## ALL STRATA
          new_name = paste0(dir.output.perPFG.allStrata.BIN
                            , "Binary_YEAR_"
                            , y
                            , "_"
                            , fg
                            , "_STRATA_all.tif")
          ind.cutoff = which(mat.cutoff$year == y & mat.cutoff$PFG == fg)
          if (length(ind.cutoff) > 0)
          {
            cutoff = mat.cutoff$cutoff[ind.cutoff]
            ras.bin = ras[[fg]]
            ras.bin[] = ifelse(ras.bin[] >= cutoff, 1, 0)
            writeRaster(x = ras.bin
                        , filename = new_name
                        , overwrite = TRUE)
            
            ## SEPARATED STRATA
            prev_names = list.files(path = dir.output.perPFG.perStrata
                                    , pattern = paste0("Abund_YEAR_"
                                                       , y
                                                       , "_"
                                                       , fg
                                                       , "_STRATA")
                                    , full.names = TRUE)
            prev_names = prev_names[grep(".tif$", prev_names)]
            if (length(prev_names) > 0)
            {
              new_names = sub(dir.output.perPFG.perStrata
                              , dir.output.perPFG.perStrata.BIN
                              , prev_names)
              new_names = sub("Abund_YEAR_", "Binary_YEAR_", new_names)
              ras.bin.str = stack(prev_names)
              ras.bin.str = ras.bin.str * ras.bin
              writeRaster(x = ras.bin.str
                          , filename = new_names
                          , overwrite = TRUE
                          , bylayer = TRUE)
              
              message(paste0("\n The output files \n"
                             , paste0(" > ", basename(new_names), " \n"
                                      , collapse = "")
                             , "have been successfully created !\n"))
            }
          } else
          {
            warning(paste0("Missing data!\n No cutoff for year ", y, ", PFG ", fg
                           , ".\n No binary maps will be produced!"))
          }
        } ## END loop on PFG
      } ## END condition file_name
    } ## END loop on years
    cat("\n")
    
    
    ## ZIP the raster saved ---------------------------------------------------
    .zip(folder_name = dir.output.perPFG.perStrata
         , list_files = raster.perPFG.perStrata
         , nb_cores = opt.no_CPU)
    
  } ## END loop on abs.simulParams
}
