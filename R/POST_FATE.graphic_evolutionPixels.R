### HEADER #####################################################################
##' @title Create a graphical representation of the evolution of PFG abundance 
##' through time for 5 (or more) pixels of a \code{FATE} simulation
##' 
##' @name POST_FATE.graphic_evolutionPixels
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce one graphical representation 
##' for a \code{FATE} simulation : the evolution through time of the 
##' abundance of each PFG for 5 (or more) randomly selected cells of the studied 
##' area.
##' 
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param opt.cells_ID (\emph{optional}) default \code{NULL}. \cr The cells ID 
##' of the studied area for which PFG abundances will be extracted
##' @param opt.abund_fixedScale default \code{TRUE}. If \code{FALSE}, the 
##' ordinate scale will be adapted for each PFG for the graphical representation 
##' of the  evolution of abundances through time
##' @param opt.doPlot (\emph{optional}) default \code{TRUE}. \cr If \code{TRUE}, 
##' plot(s) will be processed, otherwise only the calculation and reorganization 
##' of outputs will occur, be saved and returned
##' 
##' 
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE} simulation and 
##' a specific parameter file within this simulation, one preanalytical graphic :
##' 
##' \itemize{
##'   \item{the evolution of \strong{abundance} of each Plant Functional Group 
##'   through simulation time, within 5 (or more) randomly selected pixels of 
##'   the studied area (\code{FATE} \emph{arbitrary unit})
##'   }
##'   \item{\strong{if light was activated} (see 
##'   \code{\link{PRE_FATE.params_globalParameters}}), \cr evolution of 
##'   \strong{light resources} within the selected pixels is also represented 
##'   (\emph{\code{1}: Low, \code{2}: Medium, \code{3}: High})
##'   }
##'   \item{\strong{if soil was activated} (see 
##'   \code{\link{PRE_FATE.params_globalParameters}}), \cr evolution of 
##'   \strong{soil resources} within the selected pixels is also represented 
##'   (user-defined scale) \cr \cr
##'   }
##' }
##' 
##' \strong{It requires} that the \code{\link{POST_FATE.temporalEvolution}} 
##' function has been run and that the file 
##' \code{POST_FATE_TABLE_PIXEL_evolution_abundance.csv} exists (as well as the 
##' \code{POST_FATE_TABLE_PIXEL_evolution_light.csv} and 
##' \code{POST_FATE_TABLE_PIXEL_evolution_soil.csv} files if those modules were 
##' activated).
##' 
##' 
##' @return A \code{list} containing one \code{data.frame} object with the 
##' following columns, and one \code{ggplot2} object :
##' 
##' \describe{
##'   \item{tab}{
##'     \describe{
##'       \item{\code{TYPE}}{concerned information (either '\code{light}', 
##'       '\code{abundance}' or '\code{soil}')}
##'       \item{\code{GROUP}}{concerned entity (either 
##'       '\code{STRATUM_[...]}', PFG name or '\code{soil}')}
##'       \item{\code{ID.pixel}}{number of the concerned pixel}
##'       \item{\code{HAB}}{habitat of the concerned pixel}
##'       \item{\code{YEAR}}{concerned simulation year}
##'       \item{\code{value}}{concerned value extracted from \code{.csv} files 
##'       produced by \code{\link{POST_FATE.temporalEvolution}}}
##'     }
##'   }
##'   \item{plot}{\code{ggplot2} object, representing the evolution of each PFG 
##'   abundance, \emph{and light and soil resources if those modules were 
##'   activated} \cr \cr}
##' }
##' 
##' 
##' One \code{POST_FATE_TABLE_PIXEL_evolution_pixels_[...].csv} file is created : 
##' \describe{
##'   \item{\emph{pixels ids}}{always, containing the \code{data.frame} detailed 
##'   above}
##' }
##' 
##' 
##' One \code{POST_FATE_[...].pdf} file is created : 
##' \describe{
##'   \item{\file{GRAPHIC_A \cr pixels}}{to visualize for each PFG the evolution 
##'   of its abundance within each selected pixel through simulation time}
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
##' POST_FATE.graphic_evolutionPixels(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt")
##'                                     
##' POST_FATE.graphic_evolutionPixels(name.simulation = "FATE_simulation"
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
##'   writeRaster(ras
##'               , filename = paste0(dir3, "/Light_Resources_YEAR_800_STRATA_0.tif")
##'               , overwrite = TRUE)
##'               
##'   for (ye in seq(100, 700, 100))
##'   {
##'     file.copy(from = paste0(dir1, "/Abund_YEAR_800_", pfg, "_STRATA_all.tif")
##'               , to = paste0(dir1, "/Abund_YEAR_", ye, "_", pfg, "_STRATA_all.tif"))
##'               
##'     file.copy(from = paste0(dir3, "/Light_Resources_YEAR_800_STRATA_0.tif")
##'               , to = paste0(dir3, "/Light_Resources_YEAR_", ye, "_STRATA_0.tif"))
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
##' graph = POST_FATE.graphic_evolutionPixels(name.simulation = "FATE_PNE"
##'                                           , file.simulParam = "Simul_parameters_V1.txt")
##' str(graph)                                  
##' 
##' @export
##' 
##' @importFrom utils write.csv
##' @importFrom grDevices colorRampPalette
##' @importFrom reshape2 melt
##' @importFrom foreach foreach %do%
##' 
##' @importFrom ggplot2 ggplot ggsave aes_string 
##' geom_line geom_area
##' scale_color_manual scale_fill_manual
##' facet_grid labs theme element_text element_blank
##' @importFrom ggthemes theme_fivethirtyeight
##'
## END OF HEADER ###############################################################


POST_FATE.graphic_evolutionPixels = function(
  name.simulation
  , file.simulParam = NULL
  , opt.abund_fixedScale = TRUE
  , opt.cells_ID = NULL
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
    tab.abundance = fread(file.abundance)
    tab.abundance = as.data.frame(tab.abundance)
    tab.abundance$TYPE = "abundance"
    colnames(tab.abundance)[which(colnames(tab.abundance) == "PFG")] = "GROUP"
    
    years = colnames(tab.abundance)
    years = years[which(!(years %in% c("TYPE", "GROUP", "ID.pixel", "X", "Y", "HAB")))]
    years = as.numeric(years)
    no_years = length(years)
    
    strata = paste0("Stratum ", (no_STRATA - 1):0)
    
    ## Get resources tables ---------------------------------------------------
    if (doLight)
    {
      file.light = paste0(name.simulation
                          , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_light_"
                          , basename(dir.save)
                          , ".csv")
      .testParam_existFile(file.light)
      tab.light = fread(file.light)
      tab.light = as.data.frame(tab.light)
      tab.light$STRATUM = paste0("Stratum ", tab.light$STRATUM)
      tab.light$TYPE = "light"
      colnames(tab.light)[which(colnames(tab.light) == "STRATUM")] = "GROUP"
      
      tab.abundance = rbind(tab.abundance, tab.light)
    }
    
    if (doSoil)
    {
      file.soil = paste0(name.simulation
                         , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_soil_"
                         , basename(dir.save)
                         , ".csv")
      .testParam_existFile(file.soil)
      tab.soil = fread(file.soil)
      tab.soil = as.data.frame(tab.soil)
      tab.soil$GROUP = "soil"
      tab.soil$TYPE = "soil"
      
      tab.abundance = rbind(tab.abundance, tab.soil)
    }
    
    
    ## Get concerned cells id -------------------------------------------------
    IDS = sample(unique(tab.abundance$ID.pixel), 5)
    if (!is.null(opt.cells_ID))
    {
      if (sum(opt.cells_ID %in% ind_1_mask) == length(opt.cells_ID))
      {
        IDS = opt.cells_ID
      } else
      {
        warning(paste0("The values given in `opt.cells_ID` do not match "
                       , "with any cells of the studied area \n"
                       , "(obtained from the raster file `"
                       , file.mask
                       , "`)\n"
                       , "They will be replaced by randomly selected cells."))
      }
    }
    
    cat("\n Number of years : ", no_years)
    cat("\n Selected years : ", years)
    cat("\n Selected cells : ", IDS)
    cat("\n")
    
    ## Transform the data inside the table ------------------------------------
    distriAbund = tab.abundance[which(tab.abundance$ID.pixel %in% IDS), , drop = FALSE]
    distriAbund = distriAbund[, c("TYPE", "GROUP", "ID.pixel", "HAB", as.character(years))]
    distriAbund = melt(distriAbund, id.vars = c("TYPE", "GROUP", "ID.pixel", "HAB"))
    colnames(distriAbund) = c("TYPE", "GROUP", "ID.pixel", "HAB", "YEAR", "value")
    distriAbund$YEAR = as.numeric(as.character(distriAbund$YEAR))
    distriAbund$TYPE = factor(distriAbund$TYPE, c("light", "abundance", "soil"))
    distriAbund$GROUP = factor(distriAbund$GROUP, c(strata, PFG, "soil"))
    
    write.csv(distriAbund
              , file = paste0(name.simulation
                              , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_pixels_"
                              , ifelse(length(IDS) <= 5
                                       , paste0(IDS, collapse = "_")
                                       , length(IDS))
                              , "_"
                              , basename(dir.save)
                              , ".csv")
              , row.names = FALSE)
    
    message(paste0("\n The output file \n"
                   , " > POST_FATE_TABLE_PIXEL_evolution_pixels_"
                   , ifelse(length(IDS) <= 5
                            , paste0(IDS, collapse = "_")
                            , length(IDS))
                   , "_"
                   , basename(dir.save)
                   , ".csv \n"
                   , "has been successfully created !\n"))
    
    ## produce the plot -------------------------------------------------------
    if (opt.doPlot)
    {
      cat("\n PRODUCING PLOT...")
      vec_col1 = c('#0077BB', '#33BBEE', '#009988', '#EE7733', '#CC3311', '#EE3377')
      val_col1 = c(rep(rgb(1,1,1,1), no_STRATA)
                   , colorRampPalette(vec_col1)(no_PFG)
                   , "grey30")
      names(val_col1) = c(strata, PFG, "soil")
      
      vec_col2 = c('#FEC44F', '#FB9A29', '#EC7014', '#CC4C02', '#993404', '#662506')
      val_col2 = colorRampPalette(vec_col2)(no_STRATA)
      names(val_col2) = strata
      
      
      pp = ggplot(distriAbund, aes_string(x = "YEAR", y = "value")) +
        geom_line(data = distriAbund[which(distriAbund$TYPE == "soil"),]
                  , color = "grey30"
                  , lwd = 0.7) +
        geom_line(data = distriAbund[which(distriAbund$TYPE == "abundance"),]
                  , aes_string(color = "GROUP")
                  , lwd = 0.7) +
        scale_color_manual("", values = val_col1) +
        geom_area(data = distriAbund[which(distriAbund$TYPE == "light"),]
                  , aes_string(fill = "GROUP")
                  , position = "identity", alpha= 0.4) +
        scale_fill_manual("", values = val_col2) +
        facet_grid("TYPE ~ ID.pixel"
                   , scales = ifelse(opt.abund_fixedScale, "fixed", "free_y")) +
        labs(x = "", y = ""
             , title = paste0("GRAPH A : evolution of species' abundance")
             , subtitle = paste0("For each PFG, the line represents the "
                                 , "evolution through time of its abundance\n"
                                 , "for 5 randomly selected pixels within the "
                                 , "studied area.\n")) +
        .getGraphics_theme()
      
      ggsave(filename = paste0(name.simulation
                               , "/RESULTS/POST_FATE_GRAPHIC_A_evolution_pixels_"
                               , ifelse(length(IDS) <= 5
                                        , paste0(IDS, collapse = "_")
                                        , length(IDS))
                               , "_"
                               , basename(dir.save)
                               , ".pdf")
             , plot = pp, width = 10, height = 8)
    } else
    {
      pp = NULL
    } ## END opt.doPlot
    
    
    ## ------------------------------------------------------------------------
    
    cat("\n> Done!\n")
    cat("\n")
    
    return(list(tab = distriAbund, plot = pp))
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

