### HEADER #####################################################################
##' @title Create a graphical representation of the evolution of PFG coverage
##' and abundance through time for a \code{FATE-HD} simulation
##' 
##' @name POST_FATE.graphic_evolutionCoverage
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce two graphical 
##' representations for a \code{FATE-HD} simulation : 1) the evolution through
##' time of the space occupation of each PFG ; 2) the evolution through time
##' of the abundance of each PFG. These graphics represent both the evolution 
##' over the whole area. 
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE-HD} simulation
##' @param file.simulParam default \code{NULL}. A \code{string} corresponding to 
##' the name of a parameter file that will be contained into the 
##' \code{PARAM_SIMUL} folder of the \code{FATE-HD} simulation
##' @param opt.abund_fixedScale default \code{TRUE}. If \code{FALSE}, the 
##' ordinate scale will be adapted for each PFG for the graphical representation 
##' of the  evolution of abundances through time
##' @param opt.doPlot (\emph{optional}) default \code{TRUE}. \cr If TRUE, 
##' plot(s) will be processed, otherwise only the calculation and reorganization 
##' of outputs will occur, be saved and returned
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE-HD} simulation and 
##' a specific parameter file within this simulation, two preanalytical 
##' graphics : 
##' 
##' \itemize{
##'   \item{the evolution of \strong{space occupancy} of each plant functional 
##'   group through simulation time, \cr with \emph{space occupancy} 
##'   representing the percentage of pixels within the mask of studied area 
##'   where the PFG is present
##'   }
##'   \item{the evolution of \strong{total abundance} of each plant functional 
##'   group through simulation time, \cr with \emph{total abundance} being the 
##'   sum over the whole studied area of the PFG abundances (\code{FATE-HD} 
##'   \emph{arbitrary unit})
##'   }
##' }
##' 
##' If the information has been provided (see 
##' \code{\link{POST_FATE.temporalEvolution}}), the graphics will be also done 
##' per habitat. \cr \cr
##' 
##' \strong{It requires} that the \code{\link{POST_FATE.temporalEvolution}} 
##' function has been run and that the file 
##' \code{POST_FATE_TABLE_PIXEL_evolution_abundance.csv} exists.
##' 
##' 
##' 
##' @return A \code{list} containing two \code{data.frame} objects with the 
##' following columns, and two \code{ggplot2} objects :
##' 
##' \describe{
##'   \item{tab.spaceOccupancy}{
##'     \describe{
##'       \item{\code{PFG}}{concerned plant functional group (for abundance)}
##'       \item{\code{HAB}}{concerned habitat}
##'       \item{\emph{year}}{concerned simulation year}
##'       \item{\code{spaceOccupancy}}{number of occupied pixels divided by the 
##'       total number of pixels within the studied area}
##'     }
##'   }
##'   \item{tab.totalAbundance}{
##'     \describe{
##'       \item{\code{PFG}}{concerned plant functional group (for abundance)}
##'       \item{\code{HAB}}{concerned habitat}
##'       \item{\emph{year}}{concerned simulation year}
##'       \item{\code{totalAbundance}}{total abundance over all the pixels 
##'       within the studied area}
##'     }
##'   }
##'   \item{graph.spaceOccupancy}{\code{ggplot2} object, representing the 
##'   evolution of each PFG space occupancy}
##'   \item{graph.totalAbundance}{\code{ggplot2} object, representing the 
##'   evolution of each PFG total abundance \cr \cr}
##' }
##' 
##' 
##' Two \code{POST_FATE_TABLE_ZONE_evolution_[...].csv} files are created : 
##' \describe{
##'   \item{\file{spaceOccupancy}}{always, containing \code{tab.spaceOccupancy}}
##'   \item{\file{totalAbundance}}{always, containing \code{tab.totalAbundance}}
##' }
##' 
##' 
##' Two \code{POST_FATE_[...].pdf} files are created : 
##' \describe{
##'   \item{\file{GRAPHIC_A \cr spaceOccupancy}}{to visualize for each PFG the 
##'   evolution of its occupation of the studied area through simulation time}
##'   \item{\file{GRAPHIC_A \cr totalAbundance}}{to visualize for each PFG the 
##'   evolution of its abundance within the whole studied area through 
##'   simulation time}
##' }
##' 
##' 
##' @keywords FATE, outputs, abundance through time
##' 
##' @seealso \code{\link{POST_FATE.temporalEvolution}}
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt")
##'                                     
##' POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                     , opt.abund_fixedScale = FALSE)
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
##' tempEvol = POST_FATE.temporalEvolution(name.simulation = "FATE_PNE"
##'                                        , file.simulParam = "Simul_parameters_V1.txt"
##'                                        , opt.no_CPU = 1)
##' 
##' ## Create evolution coverage and space occupancy graphics
##' graph = POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_PNE"
##'                                             , file.simulParam = "Simul_parameters_V1.txt")
##' str(graph)
##' 
##' 
##' @export
##' 
##' @importFrom utils write.csv
##' @importFrom data.table fread
##' 
##' @importFrom ggplot2 ggplot ggsave aes_string 
##' geom_line 
##' scale_color_manual
##' facet_wrap labs theme element_text element_blank
##' @importFrom ggthemes theme_fivethirtyeight
##'
## END OF HEADER ###############################################################



POST_FATE.graphic_evolutionCoverage = function(
  name.simulation
  , file.simulParam = NULL
  , opt.abund_fixedScale = TRUE
  , opt.doPlot = TRUE
){
  
  #############################################################################
  
  ## CHECK parameter name.simulation
  .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation, "RESULTS/")
  .testParam_existFolder(name.simulation, "DATA/")
  name.simulation = sub("/", "", name.simulation)
  ## CHECK parameter file.simulParam
  abs.simulParams = .getParam_abs.simulParams(file.simulParam, name.simulation)
  
  
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
    
    ## Get the abundance table ------------------------------------------------
    file.abundance = paste0(name.simulation
                            , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_"
                            , basename(dir.save)
                            , ".csv")
    .testParam_existFile(file.abundance)
    tab.totalAbundance = fread(file.abundance)
    tab.totalAbundance = as.data.frame(tab.totalAbundance, stringAsFactors = FALSE)
    
    years = colnames(tab.totalAbundance)
    years = years[which(!(years %in% c("PFG", "ID.pixel", "X", "Y", "HAB")))]
    years = as.numeric(years)
    
    hab_names = unique(tab.totalAbundance$HAB)
    no_hab = length(hab_names)
    
    cat("\n Number of years : ", length(years))
    cat("\n Number of habitat : ", no_hab)
    cat("\n")
    
    ## Transform the data inside the table ------------------------------------
    cat("\n GETTING COVERAGE and ABUNDANCE over the whole area...")
    
    tab.totalAbundance.split = split(tab.totalAbundance
                                     , list(tab.totalAbundance$PFG
                                            , tab.totalAbundance$HAB))
    distriAbund.melt = foreach(i = 1:length(tab.totalAbundance.split)
                               , .combine = "rbind"
    ) %do%
    {
      pfg = strsplit(names(tab.totalAbundance.split)[i], "[.]")[[1]][1]
      hab = strsplit(names(tab.totalAbundance.split)[i], "[.]")[[1]][2]
      tab = tab.totalAbundance.split[[i]]
      if (nrow(tab) > 0)
      {
        tab = tab[, as.character(years), drop = FALSE]
        
        return(data.frame(PFG = pfg, HAB = hab, YEAR = years
                          , totalAbundance = colSums(tab, na.rm = TRUE)
                          , stringsAsFactors = FALSE)) 
      }
    }
    
    distri.melt = foreach(i = 1:length(tab.totalAbundance.split)
                          , .combine = "rbind"
    ) %do%
    {
      pfg = strsplit(names(tab.totalAbundance.split)[i], "[.]")[[1]][1]
      hab = strsplit(names(tab.totalAbundance.split)[i], "[.]")[[1]][2]
      tab = tab.totalAbundance.split[[i]]
      if (nrow(tab) > 0)
      {
        tab = tab[, as.character(years), drop = FALSE]
        tab = as.matrix(tab)
        tab = apply(tab, 2, function(x) length(which(x > 0)))
        
        return(data.frame(PFG = pfg, HAB = hab, YEAR = years
                          , spaceOccupancy = tab / no_1_mask
                          , stringsAsFactors = FALSE))
      }
    }
    cat("\n")
    
    write.csv(distri.melt
              , file = paste0(name.simulation
                              , "/RESULTS/POST_FATE_TABLE_ZONE_evolution_spaceOccupancy_"
                              , basename(dir.save)
                              , ".csv")
              , row.names = TRUE)
    
    write.csv(distriAbund.melt
              , file = paste0(name.simulation
                              , "/RESULTS/POST_FATE_TABLE_ZONE_evolution_totalAbundance_"
                              , basename(dir.save)
                              , ".csv")
              , row.names = TRUE)
    
    message(paste0("\n The output files \n"
                   , " > POST_FATE_TABLE_ZONE_evolution_spaceOccupancy_"
                   , basename(dir.save)
                   , ".csv \n"
                   , " > POST_FATE_TABLE_ZONE_evolution_totalAbundance_"
                   , basename(dir.save)
                   , ".csv \n"
                   , "have been successfully created !\n"))
    
    
    ## produce the plot -------------------------------------------------------
    if (opt.doPlot)
    {
      cat("\n PRODUCING PLOT(S)...")
      col_vec = c('#6da34d', '#297373', '#58a4b0', '#5c4742', '#3f334d')
      col_fun = colorRampPalette(col_vec)
      
      ## Evolution of space occupation
      pp1 = ggplot(distri.melt, aes_string(x = "YEAR"
                                           , y = "spaceOccupancy * 100"
                                           , color = "factor(HAB)")) +
        geom_line(lwd = 1) +
        facet_wrap("~ PFG") +
        scale_color_manual("Habitat", values = col_fun(no_hab)) +
        labs(x = "", y = ""
             , title = paste0("GRAPH A : evolution of species' space occupation")
             , subtitle = paste0("For each PFG, the line represents the "
                                 , "evolution through time of its space "
                                 , "occupancy,\n meaning the percentage of "
                                 , "pixels in which the abundance of the "
                                 , "species is greater than 0.\n")) +
        .getGraphics_theme()
      ggsave(filename = paste0(name.simulation
                               , "/RESULTS/POST_FATE_GRAPHIC_A_evolution_spaceOccupancy_"
                               , basename(dir.save), ".pdf")
             , plot = pp1, width = 10, height = 8)
      
      ## Evolution of abundance
      pp2 = ggplot(distriAbund.melt, aes_string(x = "YEAR"
                                                , y = "totalAbundance"
                                                , color = "HAB")) +
        geom_line(lwd = 1) +
        facet_wrap("~ PFG", scales = ifelse(opt.abund_fixedScale, "fixed", "free_y")) +
        scale_color_manual("Habitat", values = col_fun(no_hab)) +
        labs(x = "", y = ""
             , title = paste0("GRAPH A : evolution of species' abundance")
             , subtitle = paste0("For each PFG, the line represents the "
                                 , "evolution through time of its abundance\n"
                                 , "over the whole studied area, meaning the "
                                 , "sum of its abundances in every pixel.\n")) +
        .getGraphics_theme()
      ggsave(filename = paste0(name.simulation
                               , "/RESULTS/POST_FATE_GRAPHIC_A_evolution_abundance_"
                               , basename(dir.save), ".pdf")
             , plot = pp2, width = 10, height = 8)
    } else
    {
      pp1 = pp2 = NULL
    } ## END opt.doPlot
    
    
    ## ------------------------------------------------------------------------
    
    cat("\n> Done!\n")
    cat("\n")
    
    return(list(tab.spaceOccupancy = distri.melt
                , tab.totalAbundance = distriAbund.melt
                , graph.spaceOccupancy = pp1
                , graph.totalAbundance = pp2))
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

