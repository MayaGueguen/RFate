### HEADER #####################################################################
##' @title Create a map of the community weighted mean (CWM) of light
##'  \cr for one (or several) specific year of a \code{FATE-HD} simulation
##' 
##' @name POST_FATE.graphic_mapPFGlight
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce a raster map of community
##' weighted mean (CWM) of light for one (or several) specific \code{FATE-HD}
##' simulation year.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param file.simulParam a \code{string} that corresponds to the name of a
##' parameter file that will be contained into the \code{PARAM_SIMUL} folder
##' of the \code{FATE-HD} simulation
##' @param year an \code{integer} corresponding to the simulation year(s) that 
##' will be used to extract PFG abundance and binary maps
##' @param strata_min an \code{integer} corresponding to the lowest stratum from
##' which PFG abundances are summed up to the highest stratum
##' @param mat.PFG.succ a \code{data.frame} with 2 columns : PFG, light
##' @param opt.mat.light.obs default NULL (\emph{optional}). A \code{data.frame}
##' with 3 columns : X, Y, obs
##' @param opt.ras.light.obs default NULL (\emph{optional}). A \code{string} that
##' corresponds to the file name of a raster containing observed values for 
##' community weighted mean of light
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
##' from the results folders \code{ABUND_perPFG_perStrata} and 
##' \code{BIN_perPFG_perStrata} and unzipped.
##' Informations extracted lead to the production of one graphic before the
##' maps are compressed again :
##' 
##' \itemize{
##'   \item{the map of \strong{light Community Weighted Mean} for each selected
##'   simulation year(s), representing the simulated value of light (Landolt)
##'   within each pixel above a height threshold
##'   }
##' }
##' 
##' 
##' 
##' @return One \code{POST_FATE_[...].pdf} file is created : 
##' \describe{
##'   \item{\file{GRAPHIC_B \cr PFGlight}}{to visualize the light CWM
##'   within the studied area}
##' }
##' 
##' A raster file is created into simulation results folder.
##' 
##' @keywords FATE, outputs, relative abundance, light, community weighted mean,
##' 
##' @seealso \code{\link{POST_FATE.relativeAbund}}, 
##' \code{\link{POST_FATE.graphic_validationStatistics}}
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.graphic_mapPFGlight(name.simulation = "FATE_simulation"
##'                               , file.simulParam = "Simul_parameters_V1.txt"
##'                               , year = 850
##'                               , strata_min = 1
##'                               , mat.PFG.succ = data.frame(PFG = paste0("PFG", 1:3)
##'                                                           , light = c(5, 2, 3))
##'                               , opt.no_CPU = 1)
##'                                     
##' POST_FATE.graphic_mapPFGlight(name.simulation = "FATE_simulation"
##'                               , file.simulParam = "Simul_parameters_V1.txt"
##'                               , year = c(850, 950)
##'                               , strata_min = 1
##'                               , mat.PFG.succ = data.frame(PFG = paste0("PFG", 1:3)
##'                                                           , light = c(5, 2, 3))
##'                               , opt.no_CPU = 1)
##' }
##'                                     
##'                                     
##' 
##' @export
##' 
##' @importFrom foreach foreach
##' @importFrom raster raster stack as.data.frame
##' rasterToPoints xyFromCell
##' @importFrom grid unit
##' 
##' @importFrom ggplot2 ggplot aes aes_string ggsave
##' geom_raster element_blank coord_equal
##' scale_fill_gradientn labs theme element_rect
##' @importFrom ggthemes theme_fivethirtyeight
##' @importFrom RColorBrewer brewer.pal
##' @importFrom grDevices pdf
##'
## END OF HEADER ###############################################################


POST_FATE.graphic_mapPFGlight = function(
  name.simulation
  , file.simulParam = NULL
  , year
  , strata_min = 1
  , mat.PFG.succ
  , opt.mat.light.obs = NULL
  , opt.ras.light.obs = NULL
  , opt.no_CPU = 1
  , opt.doPlot = TRUE
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
  if (.testParam_notNum(strata_min))
  {
    .stopMessage_beInteger("strata_min")
  }
  if (.testParam_notDf(mat.PFG.succ))
  {
    .stopMessage_beDataframe("mat.PFG.succ")
  }
  if (nrow(mat.PFG.succ) == 0 || ncol(mat.PFG.succ) != 2)
  {
    .stopMessage_numRowCol("mat.PFG.succ", c("PFG", "light"))
  }
  if (ncol(mat.PFG.succ) == 2)
  {
    if (sum(colnames(mat.PFG.succ) == c("PFG", "light")) == 2)
    {
      mat.PFG.succ = mat.PFG.succ[ , c("PFG", "light")]
    } else {
      .stopMessage_columnNames("mat.PFG.succ", c("PFG", "light"))
    }
  }
  mat.PFG.succ$PFG = as.character(mat.PFG.succ$PFG)
  if (length(which(is.na(mat.PFG.succ$PFG))) > 0 ||
      length(unique(mat.PFG.succ$PFG)) < nrow(mat.PFG.succ)){
    stop("Wrong type of data!\n Column `PFG` of `mat.PFG.succ` must contain different values")
  }
  if (.testParam_notChar(mat.PFG.succ$PFG))
  {
    .stopMessage_beChar("mat.PFG.succ$PFG")
  }
  if (!is.numeric(mat.PFG.succ$light))
  {
    .stopMessage_columnNumeric("mat.PFG.succ", c("light"))
  }
  if (length(which(is.na(mat.PFG.succ$light))) > 0)
  {
    .stopMessage_columnNoNA("mat.PFG.succ", c("light"))
  }
  if (!.testParam_notDef(opt.mat.light.obs))
  {
    if (.testParam_notDf(opt.mat.light.obs))
    {
      .stopMessage_beDataframe("opt.mat.light.obs")
    } else
    {
      if (nrow(opt.mat.light.obs) == 0 || ncol(opt.mat.light.obs) != 3)
      {
        .stopMessage_numRowCol("opt.mat.light.obs", c("X", "Y", "obs"))
      }
      if (ncol(opt.mat.light.obs) == 3)
      {
        if (sum(colnames(opt.mat.light.obs) == c("X", "Y", "obs")) == 3)
        {
          opt.mat.light.obs = opt.mat.light.obs[ , c("X", "Y", "obs")]
        } else
        {
          .stopMessage_columnNames("opt.mat.light.obs", c("X", "Y", "obs"))
        }
      }
      if (!is.numeric(opt.mat.light.obs$X) ||
          !is.numeric(opt.mat.light.obs$Y) ||
          !is.numeric(opt.mat.light.obs$obs)) {
        .stopMessage_columnNumeric("opt.mat.light.obs", c("X", "Y", "obs"))
      }
      if (length(which(is.na(opt.mat.light.obs$X))) > 0 ||
          length(which(is.na(opt.mat.light.obs$Y))) > 0 ||
          length(which(is.na(opt.mat.light.obs$obs))) > 0) {
        opt.mat.light.obs = na.exclude(opt.mat.light.obs)
      }
    }
  } else if (!.testParam_notDef(opt.ras.light.obs))
  {
    if (nchar(opt.ras.light.obs) > 0)
    {
      .testParam_existFile(opt.ras.light.obs)
      ras.light = raster(opt.ras.light.obs)
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
    
    ## Get list of arrays and extract years of simulation --------------------------
    years = sort(unique(as.numeric(year)))
    no_years = length(years)
    raster.perPFG.perStrata = grep(paste0("Abund_YEAR_", years, "_", collapse = "|")
                                   , list.files(dir.output.perPFG.perStrata), value = TRUE)
    
    strata = sapply(sub(".*_STRATA_", "", raster.perPFG.perStrata)
                    , function(x) strsplit(as.character(x), "[.]")[[1]][1])
    strata = sort(unique(as.numeric(strata)))
    no_strata = max(strata)
    if (!(no_strata > 0) || is.infinite(no_strata) | .testParam_notDef(no_strata))
    {
      stop(paste0("Missing data!\n The folder ", dir.output.perPFG.perStrata, " does not contain adequate files",
                  " (number of strata null or no strata files found)"))
    }
    if (no_strata < strata_min)
    {
      stop(paste0("Wrong data given!\n `strata_min` is superior to maximum strata found (", no_strata, ")"))
    }
    
    cat("\n Number of strata : ", no_strata)
    cat("\n Selected strata : ", strata_min:no_strata)
    cat("\n")
    
    raster.perPFG.perStrata = raster.perPFG.perStrata[grep(paste0("_STRATA_", strata_min:no_strata, collapse = "|")
                                                           , raster.perPFG.perStrata)]
    if (length(raster.perPFG.perStrata) == 0)
    {
      stop(paste0("Missing data!\n The folder ", dir.output.perPFG.perStrata, " does not contain adequate files"))
    }
    
    ## UNZIP the raster saved ------------------------------------------------------
    combi = expand.grid(year = years, stratum = strata_min:no_strata)
    raster.perPFG.perStrata = foreach(y = combi$year, st = combi$stratum, .combine = "c") %do%
    {
      paste0(dir.output.perPFG.perStrata,
             "Abund_YEAR_",
             y,
             "_",
             PFG,
             "_STRATA_",
             st,
             ".tif.gz")
    }
    .unzip(folder_name = dir.output.perPFG.perStrata
           , list_files = raster.perPFG.perStrata
           , nb_cores = opt.no_CPU)
    
    
    ## get the data inside the rasters ---------------------------------------------
    cat("\n GETTING LIGHT for")
    plot_list = foreach (y = years) %do%
    {
      cat("\n > year", y)
      
      cat("\n PFG ")
      ras_TOT.list = foreach (pfg = PFG) %do%
      {
        cat(" ", pfg)
        
        ## Abundance maps
        file_name = paste0(dir.output.perPFG.perStrata,
                           "Abund_YEAR_",
                           y,
                           "_",
                           pfg)
        file_name = as.vector(sapply(file_name, function(x) paste0(x,
                                                                   "_STRATA_",
                                                                   strata_min:no_strata,
                                                                   ".tif")))
        gp_st = paste0(pfg, "_STRATA_", strata_min:no_strata)
        gp_st = gp_st[which(file.exists(file_name))]
        file_name = file_name[which(file.exists(file_name))]
        
        ## Binary map
        bin_name = paste0(dir.output.perPFG.allStrata.BIN
                          , "Binary_YEAR_"
                          , y
                          , "_"
                          , pfg
                          , "_STRATA_all.tif")
        
        if (length(file_name) > 0)
        {
          ## Binary map
          if (file.exists(bin_name))
          {
            ras.BIN = stack(bin_name) * ras.mask
            names(ras.BIN) = pfg
          } else
          {
            ras.BIN = ras.mask
            names(ras.BIN) = pfg
            warning(paste0("Missing data!\n No binary map for PFG ", pfg
                           , "\n No abundance filtering for this PFG."
                           , "\n Binary are created with the POST_FATE.graphic_validationStatistics function. Please check!"))
          }

          ## Abundance maps
          ras = stack(file_name) * ras.mask
          ras = ras * ras.BIN
          ras_TOT = ras
          if (nlayers(ras) > 1)
          {
            ras_TOT = sum(ras, na.rm = TRUE)
          }
          
          return(ras_TOT)
        }
      } ## END ras_TOT.list
      
      names(ras_TOT.list) = PFG
      ras_TOT.list = ras_TOT.list[!sapply(ras_TOT.list, is.null)]
      ras_TOT.list = stack(ras_TOT.list)
      ras_REL.list = ras_TOT.list / sum(ras_TOT.list)
      names(ras_REL.list) = names(ras_TOT.list)
      
      for (i in 1:nlayers(ras_REL.list))
      {
        i_light = mat.PFG.succ$light[which(mat.PFG.succ$PFG == names(ras_REL.list)[i])]
        if (is.null(i_light) || length(i_light) == 0)
        {
          stop(paste0("Missing data!\n `mat.PFG.succ` does not contain light value for the PFG `"
                      , names(ras_REL.list)[i]
                      , "`. Please check."))
        }
        if (nlayers(ras_REL.list) == 1)
        {
          ras_REL.list = ras_REL.list * i_light
        } else
        {
          ras_REL.list[[names(ras_REL.list)[i]]] = ras_REL.list[[names(ras_REL.list)[i]]] * i_light
        }
      }
      
      ras_light = sum(ras_REL.list)# / nlayers(ras_REL.list)
      ras.pts = as.data.frame(rasterToPoints(ras_light))
      colnames(ras.pts) = c("X", "Y", "LIGHT")
      
      output.name = paste0(name.simulation
                           , "/RESULTS/"
                           , basename(dir.save)
                           , "/PFGlight_YEAR_"
                           , y
                           , "_STRATA_"
                           , strata_min
                           , "_"
                           , no_strata
                           , ".tif")
      writeRaster(ras_light
                  , filename = output.name
                  , overwrite = TRUE)
      
      message(paste0("\n The output file \n"
                     , " > ", output.name, " \n"
                     , "has been successfully created !\n"))
      
      ## Observed light maps ------------------------------------------------------------
      if (!is.null(opt.mat.light.obs))
      {
        opt.mat.light.obs$ID = cellFromXY(ras.mask, opt.mat.light.obs[, c("X", "Y")])
      } else if (exists("ras.light"))
      {
        ras.light = ras.light * ras.mask
        opt.mat.light.obs = data.frame(ID = cellFromXY(ras.light, xy.1))
        opt.mat.light.obs$obs = ras.light[opt.mat.light.obs$ID]
      }
      
      if (!is.null(opt.mat.light.obs))
      {
        opt.mat.light.sim = ras.pts
        colnames(opt.mat.light.sim) = c("X", "Y", "sim")
        opt.mat.light.sim$ID = cellFromXY(ras.mask, opt.mat.light.sim[, c("X", "Y")])
        
        mat.light = merge(opt.mat.light.obs[, c("ID", "obs")]
                          , opt.mat.light.sim[, c("ID", "sim")]
                          , by = "ID")
        mat.light = na.exclude(mat.light)
        
        ## calculate evaluation statistics ---------------------------------------------
        # getEval = function(xx, mat)
        # {
        #   mat$sim = ifelse(mat$sim < xx, 0, 1)
        #   mat.conf = cmx(mat)
        #   auc = auc(mat)
        #   sens = sensitivity(mat.conf)
        #   spec = specificity(mat.conf)
        #   TSS = sens$sensitivity + spec$specificity - 1
        #   CCR = sum(diag(mat.conf)) / sum(mat.conf) ## Correct Classification Rate
        #   return(data.frame(thresh = xx, auc, sens, spec, TSS, CCR))
        # }
        # 
        # EVAL.cover = foreach(xx = seq(0, 1, 0.1), .combine = "rbind") %do% { getEval(xx, mat = mat.light) }
        # EVAL.cover.melt = melt(EVAL.cover, id.vars = "thresh")
        # 
        # write.csv(EVAL.cover
        #           , file = paste0(name.simulation
        #                           , "/RESULTS/POST_FATE_PFGcover_VALIDATION_STATISTICS"
        #                           , basename(dir.save)
        #                           , ".csv")
        #           , row.names = TRUE)
        # 
        # message(paste0("\n The output file POST_FATE_PFGcover_VALIDATION_STATISTICS"
        #                , basename(dir.save)
        #                , ".csv has been successfully created !\n"))
        
        ## produce the plot ------------------------------------------------------------
        # tab = EVAL.cover.melt[which(EVAL.cover.melt$variable %in% c("AUC", "TSS", "CCR")), ]
        # tab.max.auc = EVAL.cover.melt[which(EVAL.cover.melt$variable == "AUC"), ]
        # tab.max.auc = tab.max.auc[which.max(tab.max.auc$value), ]
        # tab.max.tss = EVAL.cover.melt[which(EVAL.cover.melt$variable == "TSS"), ]
        # tab.max.tss = tab.max.tss[which.max(tab.max.tss$value), ]
        # tab.max.ccr = EVAL.cover.melt[which(EVAL.cover.melt$variable == "CCR"), ]
        # tab.max.ccr = tab.max.ccr[which.max(tab.max.ccr$value), ]
        # 
        # pp = ggplot(tab, aes(x = thresh, y = value)) +
        #   geom_vline(data = tab.max.auc, aes(xintercept = thresh)
        #              , color = "brown", lwd = 5, alpha = 0.5) +
        #   geom_vline(data = tab.max.tss, aes(xintercept = thresh)
        #              , color = "brown", lwd = 5, alpha = 0.5) +
        #   geom_vline(data = tab.max.ccr, aes(xintercept = thresh)
        #              , color = "brown", lwd = 5, alpha = 0.5) +
        #   geom_line(color = "grey60", lwd = 1) +
        #   geom_point() +
        #   facet_wrap(~ variable, scales = "free_y") +
        #   labs(x = "", y = "", title = paste0("GRAPH E : validation statistics of PFG cover - Simulation year : ", y)
        #        , subtitle = paste0("Correct classification rate (CCR) measures the proportion of actual positives (or negatives) that are correctly identified as such.\n"
        #                            , "True skill statistic (TSS) values of -1 indicate predictive abilities of not better than a random model,\n"
        #                            , "0 indicates an indiscriminate model and +1 a perfect model.\n"
        #                            , "AUC corresponds to the area under the ROC curve (Receiver Operating Characteristic).\n\n"
        #                            , "Statistics are calculated for different thresholds for converting coverage to binary values (x-axis).\n")) +
        # .getGraphics_theme() +
        #   theme(axis.text = element_blank()
        #         , legend.key.width = unit(2, "lines"))
        # plot(pp)
      }
      
      ## produce the plot ------------------------------------------------------------
      if (opt.doPlot)
      {
        cat("\n PRODUCING PLOT...")
        
        ## Map of light CWM
        pp = ggplot(ras.pts, aes_string(x = "X", y = "Y", fill = "LIGHT")) +
          scale_fill_gradientn("Light (Landolt)"
                               , colors = (brewer.pal(9, "Oranges"))) +
          # , breaks = seq(1, 5, 0.2)
          # , labels = seq(1, 5, 0.2)) +
          coord_equal() +
          geom_raster() +
          labs(x = "", y = "", title = paste0("GRAPH E : map of light CWM - Simulation year : ", y),
               subtitle = paste0("For each pixel, PFG abundances from strata "
                                 , strata_min, " to ", no_strata, " are summed,\n"
                                 , "then transformed into relative values by dividing by the maximum abundance obtained.\n"
                                 , "Community Weighted Mean is then calculated with observed values of light\n"
                                 , "(Landolt - Flora Indicativa) for each PFG.")) +
          .getGraphics_theme() +
          theme(axis.text = element_blank()
                , legend.key.width = unit(2, "lines"))

        } ## END opt.doPlot
      
      return(list(raster = ras_light, plot = pp))
    } ## END loop on years
    names(plot_list) = years
    
    ## SAVE plots into file ------------------------------------------------------
    if (opt.doPlot && !is.null(plot_list[[1]]))
    {
      pdf(file = paste0(name.simulation, "/RESULTS/POST_FATE_GRAPHIC_C_map_PFGlight_", basename(dir.save), ".pdf")
          , width = 12, height = 10)
      for (y in years)
      {
        plot(plot_list[[as.character(y)]][[2]])
      }
      dev.off()
    }
    
    ## ZIP the raster saved ------------------------------------------------------
    .zip(folder_name = dir.output.perPFG.perStrata
         , list_files = raster.perPFG.perStrata
         , nb_cores = opt.no_CPU)
    
    return(plot_list)
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

