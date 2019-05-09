### HEADER #####################################################################
##' @title Create a graphical representation of several statistics for each PFG 
##' to asses the quality of the model 
##'  \cr for one (or several) specific year of a \code{FATE-HD} simulation
##' 
##' @name POST_FATE.graphic_validationStatistics
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
##' @param opt.no_CPU default 1 (\emph{optional}). The number of resources that 
##' can be used to parallelize the \code{unzip/zip} of raster files
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
##' POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
##'                                        , file.simulParam = "Simul_parameters_V1.txt"
##'                                        , year = 850
##'                                        , opt.no_CPU = 1)
##'                                     
##' POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
##'                                        , file.simulParam = "Simul_parameters_V1.txt"
##'                                        , year = c(850, 950)
##'                                        , opt.no_CPU = 1)
##' }
##'                                     
##'                                     
##' 
##' @export
##' 
##' @importFrom foreach foreach
##' @importFrom reshape2 melt
##' @importFrom raster raster stack as.data.frame cellFromXY
##' @importFrom grid unit
##' @importFrom Hmisc somers2
##'
##' @importFrom ggplot2 ggplot aes aes_string geom_raster geom_bar
##' geom_hline geom_errorbar scale_fill_gradientn facet_wrap
##' ylim labs theme annotate element_rect element_blank
##' scale_y_continuous element_text
##' @importFrom ggthemes theme_fivethirtyeight
##' @importFrom ggExtra ggMarginal
##' @importFrom gridExtra grid.arrange
##' @importFrom cowplot get_legend
##' @importFrom RColorBrewer brewer.pal
##' @importFrom grDevices pdf
##' 
##' @importFrom PresenceAbsence sensitivity specificity auc cmx
##'
## END OF HEADER ###############################################################


POST_FATE.graphic_validationStatistics = function(
  name.simulation
  , file.simulParam = NULL
  , year
  , mat.PFG.obs
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
  if (.testParam_notDf(mat.PFG.obs))
  {
    .stopMessage_beDataframe("mat.PFG.obs")
  } else
  {
    if (nrow(mat.PFG.obs) == 0 || ncol(mat.PFG.obs) != 4)
    {
      .stopMessage_numRowCol("mat.PFG.obs", c("PFG", "X", "Y", "obs"))
    }
    if (ncol(mat.PFG.obs) == 4)
    {
      if (sum(colnames(mat.PFG.obs) == c("PFG", "X", "Y", "obs")) == 4)
      {
        mat.PFG.obs = mat.PFG.obs[ , c("PFG", "X", "Y", "obs")]
      } else
      {
        .stopMessage_columnNames("mat.PFG.obs", c("PFG", "X", "Y", "obs"))
      }
    }
    if (.testParam_notChar(mat.PFG.obs$PFG))
    {
      .stopMessage_beChar("mat.PFG.obs$PFG")
    }
    if (!is.numeric(mat.PFG.obs$X) ||
        !is.numeric(mat.PFG.obs$Y) ||
        !is.numeric(mat.PFG.obs$obs)) {
      .stopMessage_columnNumeric("mat.PFG.obs", c("X", "Y", "obs"))
    }
    if (length(which(is.na(mat.PFG.obs$PFG))) > 0 ||
        length(which(is.na(mat.PFG.obs$X))) > 0 ||
        length(which(is.na(mat.PFG.obs$Y))) > 0 ||
        length(which(is.na(mat.PFG.obs$obs))) > 0) {
      mat.PFG.obs = na.exclude(mat.PFG.obs)
    }
    if (sum(mat.PFG.obs$obs %in% c(0,1)) < nrow(mat.PFG.obs)){
      stop("Wrong type of data!\n Column `obs` of `mat.PFG.obs` must contain either 0 or 1")
    }
  }
  #################################################################################################
  
  for (abs.simulParam in abs.simulParams)
  {
    
    cat("\n ############## GRAPHIC POST FATE ############## \n")
    cat("\n Simulation name : ", name.simulation)
    cat("\n Simulation file : ", abs.simulParam)
    cat("\n")
    
    ## Get results directories -----------------------------------------------------
    dir.save = .getParam(params.lines = abs.simulParam
                         , flag = "SAVE_DIR"
                         , flag.split = "^--.*--$"
                         , is.num = FALSE)
    .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/"))
    
    dir.output.perPFG.allStrata.REL = paste0(name.simulation, "/RESULTS/", basename(dir.save), "/ABUND_REL_perPFG_allStrata/")
    .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/ABUND_REL_perPFG_allStrata/"))
    
    dir.output.perPFG.allStrata.BIN = paste0(name.simulation, "/RESULTS/", basename(dir.save), "/BIN_perPFG_allStrata/")
    if (!dir.exists(dir.output.perPFG.allStrata.BIN))
    {
      dir.create(path = dir.output.perPFG.allStrata.BIN)
    }
    
    dir.output.perPFG.perStrata = paste0(name.simulation, "/RESULTS/", basename(dir.save), "/ABUND_perPFG_perStrata/")
    .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/ABUND_perPFG_perStrata/"))
    
    dir.output.perPFG.perStrata.BIN = paste0(name.simulation, "/RESULTS/", basename(dir.save), "/BIN_perPFG_perStrata/")
    if (!dir.exists(dir.output.perPFG.perStrata.BIN))
    {
      dir.create(path = dir.output.perPFG.perStrata.BIN)
    }
    
    ## Get list of arrays and extract years of simulation --------------------------
    years = sort(unique(as.numeric(year)))
    no_years = length(years)
    raster.perPFG.allStrata = grep(paste0("Abund_relative_YEAR_", years, "_", collapse = "|")
                                   , list.files(dir.output.perPFG.allStrata.REL), value = TRUE)
    if (length(raster.perPFG.allStrata) == 0)
    {
      stop(paste0("Missing data!\n The folder ", dir.output.perPFG.allStrata.REL, " does not contain adequate files"))
    }
    raster.perPFG.perStrata = grep(paste0("Abund_YEAR_", years, "_", collapse = "|")
                                   , list.files(dir.output.perPFG.perStrata, full.names = TRUE)
                                   , value = TRUE)
    if (length(raster.perPFG.perStrata) == 0)
    {
      stop(paste0("Missing data!\n The folder ", dir.output.perPFG.perStrata, " does not contain adequate files"))
    }
    .unzip(folder_name = dir.output.perPFG.perStrata
           , list_files = raster.perPFG.perStrata
           , nb_cores = opt.no_CPU)
    
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
    mat.PFG.obs = mat.PFG.obs[which(mat.PFG.obs$PFG %in% PFG), ]
    if (nrow(mat.PFG.obs) == 0)
    {
      stop(paste0("Missing data!\n The names of PFG within `mat.PFG.obs`"
                  , " is different from the names of PFG contained from "
                  , name.simulation, "/DATA/PFGS/SUCC/"))
    }
    
    ## Get raster mask -------------------------------------------------------------
    file.mask = .getParam(params.lines = abs.simulParam
                          , flag = "MASK"
                          , flag.split = "^--.*--$"
                          , is.num = FALSE)
    .testParam_existFile(file.mask)
    
    ras.mask = raster(file.mask)
    ras.mask[which(ras.mask[] == 0)] = NA
    
    
    ## get the data inside the rasters ---------------------------------------------
    cat("\n GETTING STATISTICS and PRESENCE/ABSENCE maps for")
    mat.valid_list = list(length(years))
    plot_list = list(length(years))
    for (y in years)
    {
      cat("\n > year", y)
      
      file_name = paste0(dir.output.perPFG.allStrata.REL,
                         "Abund_relative_YEAR_",
                         y,
                         "_",
                         PFG,
                         "_STRATA_all.tif")
      gp = PFG[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      if (length(file_name) > 0)
      {
        ras = stack(file_name) * ras.mask
        names(ras) = gp
        
        mat.PFG.obs.split = split(mat.PFG.obs, mat.PFG.obs$PFG)
        
        cat("\n PFG ")
        mat.valid = foreach(i = 1:length(mat.PFG.obs.split), .combine = "rbind") %do%
        {
          fg = names(mat.PFG.obs.split)[i]
          mat = mat.PFG.obs.split[[i]]
          
          cat(" ", fg)
          if (fg %in% gp)
          {
            mat$ID = as.numeric(as.factor(paste0(mat$X, "_", mat$Y)))
            
            mat = cbind(mat, fg = ras[[fg]][cellFromXY(ras, mat[, c("X", "Y")])])
            mat = mat[, c("ID", "obs", "fg")]
            mat = na.exclude(mat)
            if (nrow(mat) == 0)
            {
              warning(paste0("Missing data!\n No simulation values for PFG ", fg))
              return(data.frame(PFG = fg
                                , AUC = NA, AUC.sd = NA
                                , sensitivity = NA, sensitivity.sd = NA
                                , specificity = NA, specificity.sd = NA
                                , TSS = NA))
            } else
            {
              # auc = unname(somers2(x = mat[, "fg"], y = mat[, "obs"])["C"])
              cutoff = .getCutoff(Obs = mat[, "obs"], Fit = mat[, "fg"])
              mat.bin = mat
              mat.bin$fg = ifelse(mat.bin$fg >= cutoff$Cut, 1, 0)
              mat.conf = cmx(mat.bin[, c("ID", "obs", "fg")])
              sens = sensitivity(mat.conf)
              spec = specificity(mat.conf)
              TSS = sens$sensitivity + spec$specificity - 1
              auc = auc(mat.bin[, c("ID", "obs", "fg")])
              
              ## ALL STRATA
              new_name = paste0(dir.output.perPFG.allStrata.BIN
                                , "Binary_YEAR_"
                                , y
                                , "_"
                                , fg
                                , "_STRATA_all.tif")
              if (!file.exists(new_name))
              {
                ras.bin = ras[[fg]]
                ras.bin[] = ifelse(ras.bin[] >= cutoff$Cut, 1, 0)
                
                writeRaster(x = ras.bin
                            , filename = new_name
                            , overwrite = TRUE)
              }
              
              ## SEPARATED STRATA
              prev_names = list.files(path = dir.output.perPFG.perStrata
                                      , pattern = paste0("Abund_YEAR_",
                                                         y,
                                                         "_",
                                                         fg,
                                                         "_STRATA")
                                      , full.names = TRUE)
              prev_names = prev_names[grep(".tif$", prev_names)]
              if (length(prev_names) > 0)
              {
                for (prev_name in prev_names)
                {
                  new_name = sub(dir.output.perPFG.perStrata
                                 , dir.output.perPFG.perStrata.BIN
                                 , prev_name)
                  new_name = sub("Abund_YEAR_", "Binary_YEAR_", new_name)
                  if (!file.exists(new_name))
                  {
                    ras.bin = raster(prev_name)
                    ras.bin[] = ifelse(ras.bin[] >= cutoff$Cut, 1, 0)
                    
                    writeRaster(x = ras.bin
                                , filename = new_name
                                , overwrite = TRUE)
                  }
                }
              }
              
              return(data.frame(PFG = fg, auc, sens, spec, TSS))
            }
          } else
          {
            warning(paste0("Missing data!\n No simulation values for PFG ", fg))
            return(data.frame(PFG = fg
                              , AUC = NA, AUC.sd = NA
                              , sensitivity = NA, sensitivity.sd = NA
                              , specificity = NA, specificity.sd = NA
                              , TSS = NA))
          }
        }
        write.csv(mat.valid
                  , file = paste0(name.simulation
                                  , "/RESULTS/POST_FATE_prediction_YEAR_"
                                  , y
                                  , "_VALIDATION_STATISTICS_"
                                  , basename(dir.save)
                                  , ".csv")
                  , row.names = TRUE)
        
        message(paste0("\n The output file POST_FATE_prediction_YEAR_"
                       , y
                       , "_VALIDATION_STATISTICS_"
                       , basename(dir.save)
                       , ".csv has been successfully created !\n"))
        
        
        mat.valid = melt(mat.valid, id.vars = c("PFG", "AUC.sd", "sensitivity.sd", "specificity.sd"))
        mat.valid$variable = factor(mat.valid$variable, c("sensitivity", "TSS", "specificity", "AUC"))
        
        mat.valid$AUC.sd[which(mat.valid$variable != "AUC")] = NA
        mat.valid$sensitivity.sd[which(mat.valid$variable != "sensitivity")] = NA
        mat.valid$specificity.sd[which(mat.valid$variable != "specificity")] = NA
        
        mat.valid_list[[y]] = mat.valid
        
        mat.valid$hline = 0.5
        mat.valid$hline[which(mat.valid$variable == "AUC")] = 0.8
        mat.valid$hline[which(mat.valid$variable == "TSS")] = 0.4
        
        ## produce the plot ------------------------------------------------------------
        ## 1. get the legend
        pp = ggplot(mat.valid, aes_string(x = "PFG", y = "value", fill = "value")) +
          scale_fill_gradientn(""
                               , colors = brewer.pal(9, "RdYlGn")
                               , breaks = seq(0, 1, 0.2)
                               , limits = c(0, 1)) +
          geom_bar(stat = "identity") +
          ylim(0, 1) +
          theme_fivethirtyeight() +
          theme(legend.key.width = unit(2, "lines")
                , panel.background = element_rect(fill = "transparent", colour = NA)
                , plot.background = element_rect(fill = "transparent", colour = NA)
                , legend.background = element_rect(fill = "transparent", colour = NA)
                , legend.box.background = element_rect(fill = "transparent", colour = NA)
                , legend.key = element_rect(fill = "transparent", colour = NA))
        pp_leg = get_legend(pp)
        
        ## 2. get one plot for the title and for each statistic
        pp_list = foreach(vari = c("all", "sensitivity", "specificity", "TSS", "AUC")) %do%
        {
          if (vari == "all"){
            pp = ggplot(mat.valid, aes_string(x = "PFG", y = "value", fill = "value")) +
              labs(x = "", y = "", title = paste0("GRAPH F : validation statistics - Simulation year : ", y),
                   subtitle = paste0("Sensitivity (or specificity) measures the proportion of actual positives (or negatives) that are correctly identified as such.\n"
                                     , "True skill statistic (TSS) values of -1 indicate predictive abilities of not better than a random model,\n"
                                     , "0 indicates an indiscriminate model and +1 a perfect model.\n"
                                     , "AUC corresponds to the area under the ROC curve (Receiver Operating Characteristic).\n")) +
              theme_fivethirtyeight() +
              theme(panel.background = element_rect(fill = "transparent", colour = NA)
                    , panel.grid = element_blank()
                    , axis.text = element_blank()
                    , plot.background = element_rect(fill = "transparent", colour = NA)
                    , legend.background = element_rect(fill = "transparent", colour = NA)
                    , legend.box.background = element_rect(fill = "transparent", colour = NA)
                    , legend.key = element_rect(fill = "transparent", colour = NA))
          } else {
            if (vari == "sensitivity") subti = "Sensitivity - True positive rate"
            if (vari == "specificity") subti = "Specificity - True negative rate"
            if (vari == "TSS") subti = "True Skill Statistic (TSS)"
            if (vari == "AUC") subti = "Area Under Curve (AUC)"
            pp = ggplot(mat.valid[which(mat.valid$variable == vari), ]
                        , aes_string(x = "PFG", y = "value", fill = "value")) +
              scale_fill_gradientn(guide = F
                                   , colors = brewer.pal(9, "RdYlGn")
                                   , breaks = seq(0, 1, 0.2)
                                   , limits = c(0, 1)) +
              scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.08)) +
              geom_bar(stat = "identity") +
              geom_hline(aes_string(yintercept = "hline"), lty = 2, color = "grey30") +
              geom_errorbar(aes(ymin = value - sensitivity.sd, ymax = value + sensitivity.sd), color = "grey30") +
              geom_errorbar(aes(ymin = value - specificity.sd, ymax = value + specificity.sd), color = "grey30") +
              geom_errorbar(aes(ymin = value - AUC.sd, ymax = value + AUC.sd), color = "grey30") +
              annotate(geom = "text", x = no_PFG / 2, y = 1.05, label = subti, size = 4) +
              theme_fivethirtyeight() +
              theme(panel.background = element_rect(fill = "transparent", colour = NA)
                    , plot.background = element_rect(fill = "transparent", colour = NA)
                    , legend.background = element_rect(fill = "transparent", colour = NA)
                    , legend.box.background = element_rect(fill = "transparent", colour = NA)
                    , legend.key = element_rect(fill = "transparent", colour = NA)
                    , axis.text.x = element_text(angle = 90))
            
            pp = ggMarginal(pp, type = "boxplot", margins = "y", size = 7)
          }
          
          return(pp)
        }
        
        ## 3. gather everything
        pp_list[[6]] = pp_leg
        plot_list[[y]] = grid.arrange(grobs = pp_list
                                      , layout_matrix = matrix(c(1,1,2,3,2,3,4,5,4,5,6,6), ncol = 2, byrow = TRUE)
                                      , newpage = ifelse(y == years[1], FALSE, TRUE))
        
      }
    } ## end loop on years
    
    pdf(file = paste0(name.simulation, "/RESULTS/POST_FATE_GRAPHIC_C_validationStatistics_", basename(dir.save), ".pdf")
        , width = 12, height = 10)
    for (y in years)
    {
      plot(plot_list[[y]])
    }
    dev.off()
    
    ## ZIP the raster saved ------------------------------------------------------
    .zip(folder_name = dir.output.perPFG.perStrata, nb_cores = opt.no_CPU)
    
    return(list(tab = mat.valid_list, plot = plot_list))
  }
}

