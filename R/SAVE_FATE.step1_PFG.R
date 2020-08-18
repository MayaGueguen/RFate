### HEADER #####################################################################
##' @title Save data to reproduce building of Plant Functional Groups
##'
##' @name SAVE_FATE.step1_PFG
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to gather all data and parameters 
##' used to build a set of Plant Functional Groups.
##' 
##' @param name.dataset a \code{string} corresponding to the name to give to 
##' archive folder
##' @param mat.observations a \code{data.frame} with at least 3 columns : \cr
##' \code{sites}, \code{species}, \code{abund} (\emph{and optionally, 
##' \code{habitat}}) (see \code{\link{PRE_FATE.selectDominant}})
##' @param rules.selectDominant (\emph{optional}) default \code{NA}. \cr A 
##' \code{vector} containing all the parameter values given to the 
##' \code{\link{PRE_FATE.selectDominant}} function, if used 
##' (\code{doRuleA}, \code{rule.A1}, \code{rule.A2_quantile}, \code{doRuleB}, 
##' \code{rule.B1_percentage}, \code{rule.B1_number}, \code{rule.B2}, 
##' \code{doRuleC}).
##' @param mat.traits a \code{data.frame} with at least 3 columns :
##' \code{species}, \code{GROUP}, \code{...} (one column for each functional 
##' trait) \cr (see \code{\link{PRE_FATE.speciesDistance}})
##' @param mat.overlap (\emph{optional}) default \code{NA}. \cr 
##' Otherwise, two options :
##' \itemize{
##'   \item a \code{data.frame} with 2 columns : \code{species}, \code{raster}
##'   \item a dissimilarity structure representing the niche overlap between 
##'   each pair of species. \cr It can be a \code{dist} object, a 
##'   \code{niolap} object, or simply a \code{matrix}.
##' }
##' (see \code{\link{PRE_FATE.speciesDistance}})
##' @param rules.speciesDistance (\emph{optional}) default \code{NA}. \cr A 
##' \code{vector} containing all the parameter values given to the 
##' \code{\link{PRE_FATE.speciesDistance}} function, if used \cr (
##' \code{opt.maxPercent.NA}, \code{opt.maxPercent.similarSpecies}, 
##' \code{opt.min.sd}).
##' @param mat.species.DIST a \code{dist} object, or a \code{list} of 
##' \code{dist} objects (one for each \code{GROUP} value), corresponding to the 
##' distance between each pair of species. \cr Such an object can be obtained 
##' with the \code{\link{PRE_FATE.speciesDistance}} function.
##' @param clust.evaluation (\emph{optional}) default \code{NA}. \cr A 
##' \code{data.frame} with 4 columns : \cr
##' \code{GROUP}, \code{no.clusters}, \code{variable}, \code{value}. \cr Such an 
##' object can be obtained with the 
##' \code{\link{PRE_FATE.speciesClustering_step1}} function.
##' @param no.clusters an \code{integer}, or a \code{vector} of \code{integer} 
##' (one for each \code{GROUP} value), with the number of clusters to be kept 
##' (see \code{\link{PRE_FATE.speciesClustering_step2}})
##' @param determ.all a \code{data.frame} with 10 columns : \cr
##' \code{PFG}, \code{GROUP}, \code{ID.cluster}, \code{species}, 
##' \code{ID.species}, \code{sp.mean.dist}, \code{allSp.mean}, \code{allSp.min}, 
##' \code{allSp.max}, \code{DETERMINANT}. \cr Such an object can be obtained 
##' with the \code{\link{PRE_FATE.speciesClustering_step2}} function.
##' @param mat.traits.PFG a \code{data.frame} with at least 3 columns :
##' \code{PFG}, \code{no.species}, \code{...} (one column for each functional 
##' trait, computed as the \code{mean} (for numeric traits) or the \code{median} 
##' (for categorical traits) of the values of the determinant species of this 
##' PFG). \cr Such an object can be obtained with the 
##' \code{\link{PRE_FATE.speciesClustering_step3}} function.
##' 
##' 
##' @seealso \code{\link{PRE_FATE.abundBraunBlanquet}},
##' 
##' @examples
##' 
##' ## Load example data
##' 
##' @export
##' 
## END OF HEADER ###############################################################


SAVE_FATE.step1_PFG = function(name.dataset
                               , mat.observations
                               , rules.selectDominant = c("doRuleA" = NA
                                                          , "rule.A1" = NA
                                                          , "rule.A2_quantile" = NA
                                                          , "doRuleB" = NA
                                                          , "rule.B1_percentage" = NA
                                                          , "rule.B1_number" = NA
                                                          , "rule.B2" = NA
                                                          , "doRuleC" = NA)
                               , mat.traits
                               , mat.overlap = NA
                               , rules.speciesDistance = c("opt.maxPercent.NA" = NA
                                                           , "opt.maxPercent.similarSpecies" = NA
                                                           , "opt.min.sd" = NA)
                               , mat.species.DIST
                               , clust.evaluation = NA
                               , no.clusters
                               , determ.all
                               , mat.traits.PFG
){
  
  #############################################################################
  
  ## CHECK parameter mat.observations
  if (.testParam_notDf(mat.observations))
  {
    .stopMessage_beDataframe("mat.observations")
  } else
  {
    mat.observations = as.data.frame(mat.observations)
    if (nrow(mat.observations) == 0 || !(ncol(mat.observations) %in% c(3, 4)))
    {
      .stopMessage_numRowCol("mat.observations", c("sites", "species", "abund", "(habitat)"))
    } else
    {
      notCorrect = switch(as.character(ncol(mat.observations))
                          , "3" = .testParam_notColnames(mat.observations, c("sites", "species", "abund"))
                          , "4" = .testParam_notColnames(mat.observations, c("sites", "species", "abund", "habitat"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("mat.observations", c("sites", "species", "abund", "(habitat)"))
      }
    }
    mat.observations$sites = as.character(mat.observations$sites)
    mat.observations$species = as.character(mat.observations$species)
    .testParam_notNum.m("mat.observations$abund", mat.observations$abund)
  }
  ## CHECK parameter doRuleA / doRuleC
  if (doRuleA || doRuleC)
  {
    .testParam_notNum.m("rule.A1", rule.A1)
    .testParam_notBetween.m("rule.A2_quantile", rule.A2_quantile, 0, 1)
  }
  ## CHECK parameter doRuleB
  if (doRuleB)
  {
    if (.testParam_notInValues(mat.observations$abund, c(NA, 0, 1)))
    {
      .testParam_notNum.m("rule.B1_number", rule.B1_number)
      .testParam_notBetween.m("rule.B1_percentage", rule.B1_percentage, 0, 1)
      .testParam_notBetween.m("rule.B2", rule.B2, 0, 1)
    } else
    {
      doRuleB = FALSE
    }
  }
  ## CHECK parameter doRuleC
  if (doRuleC == FALSE ||
      sum(colnames(mat.observations) == "habitat") == 0 ||
      length(unique(mat.observations$habitat)) == 1)
  {
    doRuleC = FALSE
    mat.observations$habitat = "all"
  }
  mat.observations$habitat = as.character(mat.observations$habitat)
  categories = sort(unique(mat.observations$habitat))
  categories = unique(c("all", categories))
  ## CHECK parameter opt.doRobustness
  if (opt.doRobustness)
  {
    .testParam_notNum.m("opt.robustness_rep", opt.robustness_rep)
    .testParam_notBetween.m("opt.robustness_percent", opt.robustness_percent, 0, 1)
    opt.robustness_percent = sort(unique(round(opt.robustness_percent, 1)))
  }
  
  ## CHECK duplicated rows
  if (nrow(mat.observations) != nrow(unique(mat.observations)))
  {
    warning(paste0("`mat.observations` contains duplicated rows ("
                   , sum(duplicated(mat.observations))
                   , "). These rows have been removed. Please check."))
    mat.observations = unique(mat.observations)
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # PRE_FATE.selectDominant")
  cat("\n #------------------------------------------------------------# \n")
  
  #############################################################################
  ### PREPARATION OF DATA : informations
  #############################################################################
  
  MO = mat.observations
  MO.no_releves = nrow(MO)
  MO.sites = unique(MO$sites)
  MO.no_sites = length(MO.sites)
  MO.species = unique(MO$species)
  MO.no_species = length(MO.species)
  
  ind.notNA = which(!is.na(MO$abund))
  MO.no_releves.notNA = length(ind.notNA)
  MO.no_sites.notNA = length(unique(MO$sites[ind.notNA]))
  MO.no_species.notNA = length(unique(MO$species[ind.notNA]))
  
  {
    cat("\n ---------- INFORMATION : SAMPLING \n")
    cat("\n  Number of releves : ", MO.no_releves)
    cat("\n  Number of sites : ", MO.no_sites)
    cat("\n  Number of species : ", MO.no_species)
    cat("\n")
    
    
    cat("\n ---------- INFORMATION : ABUNDANCE \n")
    cat("\n  Percentage of releves with abundance information : "
        , round(100 * MO.no_releves.notNA / MO.no_releves, 2), "%")
    cat("\n  Percentage of sites with abundance information : "
        , round(100 * MO.no_sites.notNA / MO.no_sites, 2), "%")
    cat("\n  Percentage of species with abundance information : "
        , round(100 * MO.no_species.notNA / MO.no_species, 2), "%")
    cat("\n")
    
    if (MO.no_releves.notNA / MO.no_releves == 0)
    {
      warning(paste0("NO abundance information in your data. "
                     , "Dominant species selection will only be done with "
                     , "the criteria based on number of presences."))
    }
    if (MO.no_species.notNA / MO.no_species < 1)
    {
      warning(paste0("Species with NO abundance information can only be "
                     , "selected with the criteria based on number of presences."))
    }
    
    cat("\n ---------- STATISTICS COMPUTATION \n")
    cat("\n  For each species (site level) :")
    if (doRuleA) {
      cat("\n     - total frequency and abundance (rule A1 & A2)")
    }
    if (doRuleB) {
      cat("\n     - mean relative abundance (rule B2)")
      cat("\n     - frequency (absolute and relative) of each relative abundance class (rule B1)")
    }
    if (doRuleC) {
      cat("\n  For each species (habitat level) :")
      cat("\n     - frequency and abundance (absolute and relative) within each habitat (rule C)")
    }
    if (!doRuleA && !doRuleB && !doRuleC) {
      cat("\n     Nothing! No rule selected! Please check.")
    }
    cat("\n\n")
  }
  
  #############################################################################
  
  ## Prepare combinations to run statistics calculation
  combi = data.frame(rep = 1, percent = 1, type = "releves"
                     , stringsAsFactors = FALSE)
  if (opt.doRobustness)
  {
    opt.robustness_percent = opt.robustness_percent[which(opt.robustness_percent > 0 & 
                                                            opt.robustness_percent < 1)]
    combi = rbind(combi
                  , expand.grid(rep = 1:opt.robustness_rep
                                , percent = opt.robustness_percent
                                , type = c("releves", "sites")
                                , stringsAsFactors = FALSE))
    PROGRESS = txtProgressBar(min = 0, max = nrow(combi), style = 3)
  }
  combi$iter = 1:nrow(combi)
  
  ## Run statistics calculation
  RULES.robustness = foreach(i.type = combi$type
                             , i.rep = combi$rep
                             , i.percent = combi$percent
                             , i.iter = combi$iter
                             , .combine = "rbind"
  ) %do%
    {
      ## If opt.doRobustness, select only a fraction of the observations
      if (i.type == "releves")
      {
        sel.vec = 1:MO.no_releves
        sel.no = round(i.percent * MO.no_releves)
        sel.ind = sample(sel.vec, sel.no)
      } else
      {
        sel.vec = 1:MO.no_sites
        sel.no = round(i.percent * MO.no_sites)
        sel.ind = sample(sel.vec, sel.no)
        sel.ind = which(MO$sites %in% MO.sites[sel.ind])
      }
      
      if (length(sel.ind) > 0)
      {
        mat.observations = MO[sel.ind, , drop = FALSE]
        no.species = length( unique(mat.observations$species))
        
        #########################################################################
        ### PREPARATION OF DATA : relative abundances
        #########################################################################
        
        class_breaks = seq(0, 1, 0.05)
        
        
        ## Calculate relative abundances per site
        list.dat.sites = split(mat.observations, mat.observations$sites)
        mat.obs_rel = foreach(i.dat = 1:length(list.dat.sites)
                              , .combine = "rbind"
        ) %do%
          {
            dat = list.dat.sites[[i.dat]]
            dat$sites.abund = sum(dat$abund, na.rm = TRUE)
            dat$abund_rel.sites = dat$abund / sum(dat$abund, na.rm = TRUE)
            return(dat)
          }
        mat.obs_rel$class_rel.sites = cut(mat.obs_rel$abund_rel.sites
                                          , breaks = class_breaks)
        
        ## Calculate relative abundances per habitat
        list.dat.habitat = split(mat.obs_rel, mat.obs_rel$habitat)
        mat.obs_rel = foreach(i.dat = 1:length(list.dat.habitat)
                              , .combine = "rbind"
        ) %do%
          {
            dat = list.dat.habitat[[i.dat]]
            dat$habitat.abund = sum(dat$abund, na.rm = TRUE)
            dat$abund_rel.habitat = dat$abund / sum(dat$abund, na.rm = TRUE)
            return(dat)
          }
        
        #########################################################################
        ### STATISTICS
        #########################################################################
        
        ## SELECTION RULE A -----------------------------------------------------
        if (doRuleA || doRuleB)
        {
          ## Calculate the number of releves per species
          mat1 = table(mat.obs_rel$species)
          mat1 = data.frame(species = names(mat1)
                            , freq_tot = as.vector(mat1)
                            , stringsAsFactors = FALSE)
          ## Calculate the total abundance per species
          mat2 = tapply(X = mat.obs_rel$abund
                        , INDEX = list(mat.obs_rel$species)
                        , FUN = sum
                        , na.rm = TRUE)
          mat2 = data.frame(species = names(mat2)
                            , abund_tot = as.vector(mat2)
                            , stringsAsFactors = FALSE)
          ## Gather information
          mat.A_B2 = merge(mat1, mat2, by = "species")
          rm(list = c("mat1", "mat2"))
        }
        
        
        ## SELECTION RULE B -----------------------------------------------------
        if (doRuleB)
        {
          ## Calculate mean relative abundance per species
          mat3 = tapply(X = mat.obs_rel$abund_rel.sites
                        , INDEX = list(mat.obs_rel$species)
                        , FUN = mean
                        , na.rm = TRUE)
          mat3 = data.frame(species = names(mat3)
                            , abund_rel.sites.mean = as.vector(mat3)
                            , stringsAsFactors = FALSE)
          mat.A_B2 = merge(mat.A_B2, mat3, by = "species", all.x = TRUE)
          # mat.A_B2$class_rel.sites.mean = cut(mat.A_B2$abund_rel.sites.mean
          #                               , breaks = class_breaks)
          rm(list = c("mat3"))
          
          
          ## Calculate the number of relative abundance class per species
          mat.B1 = table(mat.obs_rel$species, mat.obs_rel$class_rel.sites)
          mat.B1 = as.data.frame(mat.B1, stringsAsFactors = FALSE)
          colnames(mat.B1) = c("species", "class_rel.sites", "freq.class_rel.sites")
          mat.B1 = mat.B1[which(mat.B1$freq.class_rel.sites > 0), ]
          
          ## Calculate the relative frequency
          mat.B1 = merge(mat.B1, mat.A_B2[, c("species", "freq_tot")], by = "species")
          mat.B1$freq_rel.class_rel.sites = mat.B1$freq.class_rel.sites / mat.B1$freq_tot
        }
        
        
        ## SELECTION RULE C -----------------------------------------------------
        if (doRuleC)
        {
          ## Calculate the number of releves per species per habitat
          mat.C = table(mat.obs_rel$species, mat.obs_rel$habitat)
          mat.C = as.data.frame(mat.C, stringsAsFactors = FALSE)
          colnames(mat.C) = c("species", "habitat", "freq.habitat")
          mat.C = mat.C[which(mat.C$freq.habitat > 0), ]
          
          ## Calculate the relative frequency
          mat4 = table(mat.obs_rel$habitat)
          mat4 = data.frame(habitat = names(mat4)
                            , freq_tot.habitat = as.vector(mat4)
                            , stringsAsFactors = FALSE)
          mat.C = merge(mat.C, mat4, by = "habitat")
          mat.C$freq_rel.habitat = mat.C$freq.habitat / mat.C$freq_tot.habitat
          
          ## Calculate the total abundance per species per habitat
          mat5 = tapply(X = mat.obs_rel$abund
                        , INDEX = list(mat.obs_rel$species
                                       , mat.obs_rel$habitat)
                        , FUN = sum
                        , na.rm = TRUE)
          mat5 = as.data.frame(mat5, stringsAsFactors = FALSE)
          mat5$species = rownames(mat5)
          mat5 = melt(mat5, id.vars = "species")
          colnames(mat5) = c("species", "habitat", "abund_tot.habitat")
          mat5 = mat5[which(mat5$abund_tot.habitat > 0), ]
          
          ## Calculate the relative abundance per habitat
          mat.C = merge(mat.C, mat5, by = c("species", "habitat"), all = TRUE)
          mat.C = merge(mat.C, unique(mat.obs_rel[, c("habitat", "habitat.abund")]), by = "habitat")
          mat.C$abund_rel.habitat = mat.C$abund_tot.habitat / mat.C$habitat.abund
          rm(list = c("mat4", "mat5"))
        }
        
        #########################################################################
        ### SELECTION OF DOMINANT SPECIES
        #########################################################################
        
        ## Get information about how each species has been selected
        ## (over all area, or within specific habitat and which one(s), or both)
        
        sp_ruleA1 = sp_ruleA2 = sp_ruleB1 = sp_ruleB2 = sp_ruleCA2 = vector()
        
        if (doRuleA)
        {
          ## A1 : minimum number of occurrences
          sp_ruleA1 = mat.A_B2$species[which(mat.A_B2$freq_tot >= rule.A1)]
          
          ## A2 : minimum number of occurrences (quantile)
          rule.A2_value = quantile(mat.A_B2$freq_tot, p = rule.A2_quantile)
          sp_ruleA2 = mat.A_B2$species[which(mat.A_B2$freq_tot >= rule.A2_value)]
        }
        
        if (doRuleB)
        {
          ## B1 : relative abundance or dominancy in a certain number of sites
          class_B1 = levels(mat.B1$class_rel.sites)[(class_breaks[-1] >= rule.B1_percentage)]
          sp_ruleB1 = mat.B1$species[which(mat.B1$class_rel.sites %in% class_B1 &
                                             mat.B1$freq.class_rel.sites >= rule.B1_number)]
          sp_ruleB1 = unique(as.character(sp_ruleB1))
          
          ## B2 : minimum mean relative abundance
          sp_ruleB2 = mat.A_B2$species[which(mat.A_B2$abund_rel.sites.mean >= rule.B2)]
        }
        
        if (doRuleC)
        {
          ## C2 : similar to A2 for each habitat
          sp_ruleCA2 = foreach(i.hab = categories[-1]) %do%
            {
              tab = mat.C[which(mat.C$habitat == i.hab), ]
              quanti = quantile(tab$freq.habitat, p = rule.A2_quantile)
              return(tab$species[which(tab$freq.habitat >= quanti)])
            }
          names(sp_ruleCA2) = categories[-1]
          
          ## C1 : similar to B1 for each habitat
          # class_C1 = levels(mat.C$class_rel.sites)[(class_breaks[-1] >= rule.B1_percentage)]
          # sp_ruleCB1 = mat.B1$species[which(mat.C$class_rel.sites %in% class_C1 &
          #                                     mat.C$freq.class_rel.sites >= rule.B1_number)]
          # sp_ruleCB1 = unique(as.character(sp_ruleCB1))
        }
        
        
        ## COMBINE all rules ----------------------------------------------------
        sp_rule_ALL = c(list(A1 = sp_ruleA1
                             , A2 = sp_ruleA2
                             , B1 = sp_ruleB1
                             , B2 = sp_ruleB2)
                        , sp_ruleCA2)
        
        RULES = foreach(i.rule = 1:length(sp_rule_ALL), .combine = "rbind") %do%
          {
            mat = matrix(FALSE, nrow = 1, ncol = no.species
                         , dimnames = list(names(sp_rule_ALL)[i.rule]
                                           , unique(mat.observations$species)))
            mat[, which(colnames(mat) %in% sp_rule_ALL[[i.rule]])] = TRUE
            return(mat)
          }
        RULES = as.data.frame(t(RULES))
        tab.sp = rowSums(RULES)
        tab.rules = colSums(RULES)
        
        ## Add information about how each species has been selected or not
        RULES$species = rownames(RULES)
        RULES$SELECTION = sapply(paste0(RULES$A2, "_", RULES$B1, "_", RULES$B2)
                                 , function(x) 
                                 {
                                   switch(x
                                          , "FALSE_FALSE_FALSE" = "Not selected (A2, B1, B2)"
                                          , "TRUE_FALSE_FALSE" = "A2"
                                          , "FALSE_TRUE_FALSE" = "B1"
                                          , "FALSE_FALSE_TRUE" = "B2"
                                          , "TRUE_TRUE_FALSE" = "A2 & B1"
                                          , "FALSE_TRUE_TRUE" = "B1 & B2"
                                          , "TRUE_FALSE_TRUE" = "A2 & B2"
                                          , "TRUE_TRUE_TRUE" = "A2 & B1 & B2")
                                 })
        RULES$SELECTION[which(tab.sp == 0)] = "Not selected"
        RULES$SELECTION[which(tab.sp == 1 &
                                RULES$A1 == TRUE)] = "Not selected"
        RULES$SELECTION[which(tab.sp > 1 &
                                RULES$A1 == TRUE &
                                RULES$SELECTION == "Not selected (A2, B1, B2)")] = "C"
        RULES$SELECTION[which(tab.sp > 0 &
                                RULES$A1 == FALSE &
                                RULES$SELECTION == "Not selected (A2, B1, B2)")] = "C" ## not A1
        RULES$SELECTION = factor(RULES$SELECTION, c("Not selected"
                                                    , "A2"
                                                    , "A2 & B1"
                                                    , "A2 & B2"
                                                    , "A2 & B1 & B2"
                                                    , "B1"
                                                    , "B2"
                                                    , "B1 & B2"
                                                    , "C"))
        
        ## Summary : has each species been selected or not
        RULES$SELECTED = FALSE
        RULES$SELECTED[which(RULES$SELECTION != "Not selected" & RULES$A1 == TRUE)] = TRUE
        
        
        ## SAVE summary tables --------------------------------------------------
        if (i.percent == 1 && i.type == "releves" && i.rep == 1)
        {
          end_filename = end_filenameA = end_filenameB = end_filenameC = ""
          if (doRuleA) {
            end_filenameA = paste0("_A_", rule.A1
                                   , "_", rule.A2_quantile)
            write.csv(mat.A_B2
                      , file = "PRE_FATE_DOMINANT_mat.A_B2.csv"
                      , row.names = FALSE)
          }
          if (doRuleB) {
            end_filenameB = paste0("_B_", rule.B1_number
                                   , "_", rule.B1_percentage
                                   , "_", rule.B2)
            write.csv(mat.B1
                      , file = "PRE_FATE_DOMINANT_mat.B1.csv"
                      , row.names = FALSE)
          }
          if (doRuleC) {
            end_filenameC = paste0("_C_", rule.A1
                                   , "_", rule.A2_quantile)
            write.csv(mat.C
                      , file = "PRE_FATE_DOMINANT_mat.C.csv"
                      , row.names = FALSE)
          }
          end_filename = paste0(end_filenameA, end_filenameB, end_filenameC)
          
          write.csv(RULES
                    , file = paste0("PRE_FATE_DOMINANT_TABLE_complete"
                                    , end_filename
                                    , ".csv")
                    , row.names = FALSE)
          write.csv(RULES[which(RULES$SELECTED == TRUE), "species"]
                    , file = paste0("PRE_FATE_DOMINANT_TABLE_species"
                                    , end_filename
                                    , ".csv")
                    , row.names = FALSE)
          
          
          message(paste0("\n The output files \n"
                         , ifelse(doRuleA
                                  , " > PRE_FATE_DOMINANT_mat.A_B2.csv \n"
                                  , "")
                         , ifelse(doRuleB
                                  , " > PRE_FATE_DOMINANT_mat.B1.csv \n"
                                  , "")
                         , ifelse(doRuleC
                                  , " > PRE_FATE_DOMINANT_mat.C.csv \n"
                                  , "")
                         , " > PRE_FATE_DOMINANT_TABLE_complete"
                         , end_filename
                         , ".csv \n"
                         , " > PRE_FATE_DOMINANT_TABLE_species"
                         , end_filename
                         , ".csv \n"
                         , "have been successfully created !\n"))
        }
        
        ## RETURN table ---------------------------------------------------------
        
        if (opt.doRobustness)
        {
          setTxtProgressBar(pb = PROGRESS, value = i.iter)
          RULES$type = i.type
          RULES$percent = i.percent
          RULES$rep = i.rep
        }
        return(RULES)
      }
    }
  
  
  cat("\n ---------- SELECTION OF DOMINANT SPECIES \n")
  
  ## Keep only part corresponding to full dataset
  if (opt.doRobustness)
  {
    close(PROGRESS)
    RULES = RULES.robustness[which(RULES.robustness$percent == 1 &
                                     RULES.robustness$type == "releves" &
                                     RULES.robustness$rep == 1), ]
    RULES = RULES[, -which(colnames(RULES) %in% c("type", "percent", "rep"))]
  } else
  {
    RULES = RULES.robustness
  }
  
  ## Get selected species and corresponding observations
  RULES.sel = RULES[which(RULES$SELECTION != "Not selected"), ]
  sel.sp = RULES.sel$species[which(RULES.sel$A1 == TRUE)]
  sel.obs = MO[which(MO$species %in% sel.sp), ]
  
  {
    cat("\n   - Number of selected species :", length(sel.sp))
    cat("\n   - Representativity of species :"
        , round(100 * length(sel.sp) / MO.no_species)
        , "%")
    cat("\n   - Representativity of sites :"
        , round(100 * length(unique(sel.obs$sites)) / MO.no_sites)
        , "%")
    cat("\n   - Representativity of total abundance :"
        , round(100 * sum(sel.obs$abund, na.rm = T) / sum(MO$abund, na.rm = T))
        , "%")
    cat("\n  Complete table of information can be find in output files.")
    cat("\n")
  }
  
  
  #############################################################################
  ## GRAPHICS TO HELP ADJUST PARAMETERS TO SELECT DOMINANT SPECIES
  #############################################################################
  
  if (opt.doPlot)
  {
    cat("\n ---------- PRODUCING PLOT(S) \n")
    
    ###########################################################################
    ## ILLUSTRATION of rules A1, A2 and C -------------------------------------
    
    if (doRuleA || doRuleC)
    {
      cat("\n> Illustration of rules A and C")
      
      tab.plot = mat.A_B2[, c("species", "freq_tot", "abund_tot")]
      tab.plot$habitat = "all"
      names.plot = c("tot")
      if (doRuleC)
      {
        tmp = mat.C[, c("habitat", "species", "freq.habitat", "abund_tot.habitat")]
        colnames(tmp) = c("habitat", "species", "freq_tot", "abund_tot")
        tab.plot = rbind(tab.plot, tmp)
        names.plot = c("tot", "hab")
      }
      
      ## Correct total abundance value to 1 when no abundance is provided (NA)
      tab.plot$abund_tot[which(is.na(tab.plot$abund_tot))] = 1
      
      ## Add information about selection rules A1 & A2
      tab.quant = foreach(i.hab = categories, .combine = "rbind") %do%
        {
          tab = tab.plot[which(tab.plot$habitat == i.hab), ]
          quanti = quantile(tab$freq_tot, p = rule.A2_quantile)
          return(data.frame(habitat = i.hab
                            , quantile = quanti
                            , stringsAsFactors = FALSE))
        }
      tab.plot = merge(tab.plot, tab.quant, by = "habitat")
      tab.plot$class_A = factor(ifelse(tab.plot$freq_tot < rule.A1
                                       , 1
                                       , ifelse(tab.plot$freq_tot >= tab.plot$quantile
                                                , 3, 2)))
      rm(list = c("tab.quant"))
      
      doLogA = max(tab.plot$abund_tot, na.rm = TRUE)
      
      ## ----------------------------------------------------------------------
      pp_shared = 'geom_histogram(aes_string("freq_tot")
      , fill = "grey60"
      , alpha = 0.5
      , binwidth = function(x) 2 * IQR(x) / (length(x)^(1/4))
      , na.rm = FALSE) +
      geom_point(aes_string(x = "freq_tot"
      , y = "abund_tot"
      , color = "class_A")
      , alpha = 0.5) +
      geom_vline(xintercept = rule.A1, lty = 2) +
      geom_vline(aes_string(xintercept = "quantile"), lty = 2) +
      geom_label_repel(data = tab.quanti
      , aes_string(x = "quantile"
      , y = "y_quanti"
      , label = "lab_quanti")
      , color = tab.quanti$col_quanti
      , fontface = "bold"
      , direction = "x"
      , label.size = 0
      , label.padding = unit(0.3, "lines")) +
      scale_color_manual(guide = FALSE
      , values = c("1" = "brown"
      , "2" = "chocolate"
      , "3" = "darkgoldenrod")) +
      labs(x = "\nNumber of occurrences"
      , y = "Total abundance\n") +
      .getGraphics_theme() +
      theme(axis.title = element_text(size = 12))'
      
      if (doLogA >= 100)
      {
        pp_shared = paste0(pp_shared, '+
                           scale_y_log10(name = "Total abundance\n")')
      }
      
      ## ----------------------------------------------------------------------
      for(i.plot in names.plot)
      {
        if (i.plot == "tot") {
          tab = tab.plot[which(tab.plot$habitat == "all"), ]
        } else {
          tab = tab.plot[which(tab.plot$habitat != "all"), ]
        }
        
        if(nrow(tab) > 0)
        {
          ## Add quantiles corresponding to rules A1 and A2
          tab.quanti1 = data.frame(habitat = unique(tab$habitat)
                                   , quantile = rule.A1
                                   , col_quanti = "chocolate"
                                   , lab_quanti = "A1"
                                   , stringsAsFactors = FALSE)
          tab.quanti2 = unique(tab[, c("habitat", "quantile")])
          tab.quanti2$col_quanti = "darkgoldenrod"
          tab.quanti2$lab_quanti = "A2"
          tab.quanti = rbind(tab.quanti1, tab.quanti2)
          tab.quanti$y_quanti = ifelse(doLogA >= 100, 0.5, doLogA)
          
          pp = eval(parse(text = paste0('ggplot(tab) +', pp_shared)))
          
          if (i.plot == "tot") {
            pp_tot = pp +
              annotate(geom = "label"
                       , x = max(tab.plot$freq_tot) * c(0.05, 0.6, 0.6)
                       , y = doLogA * c(0.6, 0.6, 0.01)
                       , label = c("Very abundant species \nwith narrow distributions"
                                   , "Very abundant and\n widespread species"
                                   , "Widespread species \nbut poorly abundant")
                       , colour = c("grey30", "darkgoldenrod", "darkgoldenrod")
                       , fontface = c("italic", "bold", "bold")
                       , hjust = "inward"
                       , alpha = 0.5) +
              labs(title = "STEP 1 : Selection of dominant species - rule A"
                   , subtitle = paste0("Criteria concerning occurrences within all sites :\n"
                                       , "  > A1 = minimum number of releves required : "
                                       , rule.A1, "\n"
                                       , "  > A2 = minimum number of occurrences required : quantile("
                                       , rule.A2_quantile * 100, "%) = "
                                       , round(rule.A2_value), "\n"
                                       , "\n"))
            
            ## ----------------------------------------------------------------
            ggsave(filename = paste0("PRE_FATE_DOMINANT_STEP_1_rule"
                                     , end_filenameA, ".pdf")
                   , plot = pp_tot, device = "pdf", width = 10, height = 8)
            
          } else {
            pp_hab = pp +
              facet_wrap(~ habitat, scales = "free_x") +
              labs(title = "STEP 1 : Selection of dominant species - rule C"
                   , subtitle = paste0("Criteria concerning occurrences within all habitats :\n"
                                       , "  > A1 = minimum number of releves required : "
                                       , rule.A1, "\n"
                                       , "  > A2 = minimum number of occurrences required : quantile("
                                       , rule.A2_quantile * 100, "%) = "
                                       , paste0(paste0(tab.quanti2$habitat, ": "
                                                       , round(tab.quanti2$quantile))
                                                , collapse = " / "), "\n"
                                       , "\n"))
            
            ## ----------------------------------------------------------------
            ggsave(filename = paste0("PRE_FATE_DOMINANT_STEP_1_rule"
                                     , end_filenameC, ".pdf")
                   , plot = pp_hab, device = "pdf", width = 12, height = 9)
          }
        }
      }
    }
    
    
    ###########################################################################
    ## ILLUSTRATION of rules B1 -----------------------------------------------
    
    if (doRuleB)
    {
      cat("\n> Illustration of rule B")
      
      tab.plot = mat.B1
      
      ## Associate a color to each species : rule B1
      pal_sp = sequential_hcl(n = length(sp_ruleB1)
                              , palette = "Teal", rev = TRUE)
      pal_sp = data.frame(species = sp_ruleB1
                          , id_color = pal_sp
                          , stringsAsFactors = FALSE)
      tab.plot = merge(tab.plot, pal_sp, by = "species", all.x = TRUE)
      tab.plot$id_color[which(is.na(tab.plot$id_color))] = "darkgray"
      tab.plot$class_B = ifelse(tab.plot$id_color == "darkgray"
                                , 1, 2)
      
      ## Associate a color to each species : rule B2
      pal_sp = sequential_hcl(n = length(sp_ruleB2)
                              , palette = "Greens", rev = TRUE)
      for(i.sp in 1:length(sp_ruleB2))
      {
        tab.plot$id_color[which(tab.plot$species == sp_ruleB2[i.sp])] = pal_sp[i.sp]
        tab.plot$class_B[which(tab.plot$species == sp_ruleB2[i.sp])] = 3
      }
      
      doLogB = max(tab.plot$freq.class_rel.sites, na.rm = TRUE)
      
      x_B1 = (class_breaks < rule.B1_percentage)
      x_B1 = max(which(x_B1 == TRUE))
      
      x_B2 = (class_breaks < rule.B2)
      x_B2 = max(which(x_B2 == TRUE))
      
      ## FREQ -----------------------------------------------------------------
      pp_B = ggplot(tab.plot, aes_string(x = "as.numeric(class_rel.sites) + 0.3 * (class_B - 1)"
                                         , y = "freq.class_rel.sites"
                                         , fill = "id_color")) +
        geom_col(position = "identity"
                 , alpha = 0.5
                 , width = 0.3
                 , na.rm = TRUE) +
        geom_hline(yintercept = rule.B1_number, lty = 2) +
        annotate(geom = "label"
                 , x = 0
                 , y = rule.B1_number
                 , colour = "dodgerblue4"
                 , fontface = "bold"
                 , label = "B1.no"
                 , label.size = 0
                 , label.padding = unit(0.25, "lines")) +
        geom_vline(xintercept = c(x_B1, x_B2), lty = 2) +
        annotate(geom = "label"
                 , x = c(x_B1, x_B2)
                 , y = ifelse(doLogB >= 100, 0.5, doLogB)
                 , colour = c("dodgerblue4", "darkgreen")
                 , fontface = "bold"
                 , label = c("B1.%", "B2")
                 , label.size = 0
                 , label.padding = unit(1, "lines")) +
        annotate(geom = "label"
                 , x = 20 * c(0.15, 0.5, 0.9)
                 , y = doLogB * c(0.8, ifelse(doLogB >= 100, 0.05, 0.5)
                                  , ifelse(doLogB >= 100, 0.003, 0.1))
                 , label = c("Poorly abundant\n species \nbut widespread"
                             , "Species relatively abundant\n or dominant \n in a certain number of sites"
                             , "Dominant species \nbut in few sites")
                 , colour = c("grey30", "dodgerblue4", "darkgreen")
                 , fontface = c("italic", "bold", "bold")
                 , alpha = 0.5) +
        scale_x_continuous(name = "\nWithin-site relative abundance (%)"
                           , breaks = seq(1, length(class_breaks) - 1)
                           , labels = 100 * class_breaks[-1]) +
        scale_fill_identity(guide = FALSE) +
        scale_color_continuous(guide = FALSE) +
        labs(y = "Releves frequency\n"
             , title = "STEP 1 : Selection of dominant species - rule B"
             , subtitle = paste0("Criteria concerning abundances within all concerned sites :\n"
                                 , "  > B1.no = minimum number of sites required : "
                                 , rule.B1_number, "\n"
                                 , "  > B1.% = with a minimum relative abundance of : "
                                 , 100 * rule.B1_percentage, "% \n"
                                 , "  > B2 = minimum mean relative abundance required : "
                                 , 100 * rule.B2, "% \n"
                                 , "\n")) +
        .getGraphics_theme() +
        theme(axis.title = element_text(size = 12))
      
      if (doLogB >= 100)
      {
        pp_B = pp_B +
          scale_y_log10(name = "Releves frequency\n")
      }
      
      
      ## FREQ.REL -------------------------------------------------------------
      pp_B.rel = ggplot(tab.plot, aes_string(x = "as.numeric(class_rel.sites) + 0.3 * (class_B - 1)"
                                             , y = "freq_rel.class_rel.sites"
                                             , fill = "id_color")) +
        geom_col(position = "identity"
                 , alpha = 0.5
                 , width = 0.3
                 , na.rm = TRUE) +
        geom_vline(xintercept = c(x_B1, x_B2), lty = 2) +
        annotate(geom = "label"
                 , x = c(x_B1, x_B2)
                 , y = 1.1
                 , colour = c("dodgerblue4", "darkgreen")
                 , fontface = "bold"
                 , label = c("B1.%", "B2")
                 , label.size = 0
                 , label.padding = unit(1, "lines")) +
        scale_x_continuous(name = "\nWithin-site relative abundance (%)"
                           , breaks = seq(1, length(class_breaks) - 1)
                           , labels = 100 * class_breaks[-1]) +
        scale_y_continuous(name = "Relative releves frequency (representativity) (%)\n"
                           , breaks = seq(0, 1, 0.2)
                           , labels = seq(0, 1, 0.2) * 100) +
        scale_fill_identity(guide = FALSE) +
        scale_color_continuous(guide = FALSE) +
        labs(title = "STEP 1 : Selection of dominant species - rule B"
             , subtitle = paste0("Criteria concerning abundances within all concerned sites :\n"
                                 , "  > B1.no = minimum number of sites required : "
                                 , rule.B1_number, "\n"
                                 , "  > B1.% = with a minimum relative abundance of : "
                                 , 100 * rule.B1_percentage, "% \n"
                                 , "  > B2 = minimum mean relative abundance required : "
                                 , 100 * rule.B2, "% \n"
                                 , "\n")) +
        .getGraphics_theme() +
        theme(axis.title = element_text(size = 12))
      
      
      ## ----------------------------------------------------------------------
      pdf(file = paste0("PRE_FATE_DOMINANT_STEP_1_rule"
                        , end_filenameB, ".pdf")
          , width = 10, height = 8)
      plot(pp_B)
      plot(pp_B.rel)
      dev.off()
    }
    
    
    ###########################################################################
    ## ILLUSTRATION of SELECTED SPECIES ---------------------------------------
    
    {
      cat("\n> Illustration of selected species")
      if (length(sel.sp) <= 1)
      {
        warning("Too few species selected for representation. No phylogeny and PCO graphics.")
      } else
      {
        ## Associate a color to each species according to selection rules
        pal_col = c("Not selected" = "grey"
                    , "A2" = "darkgoldenrod"
                    , "A2 & B1" = "coral2"
                    , "A2 & B2" = "hotpink3"
                    , "A2 & B1 & B2" = "mediumorchid3"
                    , "B1" = "dodgerblue4"
                    , "B2" = "darkgreen"
                    , "B1 & B2" = "darkolivegreen3"
                    , "C" = "lightsalmon4")
        tip_col = pal_col[RULES.sel$SELECTION]
        pal_col = pal_col[which(names(pal_col) %in% unique(RULES.sel$SELECTION))]
        
        ## Transform results of selection rules into distance matrix
        mat.dist = RULES.sel[, -which(colnames(RULES.sel) %in% c("species", "SELECTION"))]
        mat.dist = as.matrix(mat.dist)
        mat.dist[] = as.numeric(mat.dist[])
        mat.dist = as.dist(gowdis(mat.dist))
        
        if (is.na(is.euclid(mat.dist)))
        {
          warning("No euclidean distance. No phylogeny and PCO graphics.")
        } else
        {
          if (!is.euclid(mat.dist)) mat.dist = quasieuclid(mat.dist)
          
          ## ----------------------------------------------------------------------
          ## Transform distance matrix into phylogeny -----------------------------
          hcp = as.phylo(hclust(mat.dist))
          clust = cutree(hclust(mat.dist), h = 0.75)
          clust.table = table(clust)
          
          .getAncestors = function(groups, group.i)
          {
            ## tips corresponding to the class
            ind_group.i = which(groups == group.i)
            ind_tot = vector()
            while(length(ind_tot) < (2 * table(groups)[group.i] - 2))
            {
              ind_tips = which(hcp$edge[, 2] %in% ind_group.i)
              ## ancestors corresponding to the tips
              ind_ancestors = unique(hcp$edge[ind_tips, 1])
              
              ind_tot = unique(c(ind_tot, ind_tips))
              ind_group.i = ind_ancestors
            }
            return(ind_tot)
          }
          
          ## GET edges color
          edge_col = rep("grey", nrow(hcp$edge))
          if (length(clust.table) > 1)
          {
            for(i.class in 1:length(clust.table))
            {
              ind = .getAncestors(groups = clust, group.i = i.class)
              edge_col[ind] = paste0("grey", round(seq(20, 80, length.out = length(clust.table))))[i.class]
            }
          }
          
          ## GET edges linetype
          edge_lty = rep(1, nrow(hcp$edge))
          if (doRuleA && length(which(RULES.sel$A1 == FALSE)) > 0)
          {
            ind = .getAncestors(groups = RULES.sel$A1, group.i = "FALSE")
            edge_lty[ind] = 2
          }
          
          ## GET tips color
          tip_col = foreach(i = 1:length(tip_col), .combine = "c") %do%
            {
              aa = col2rgb(col = tip_col[i])[, 1]
              bb = rgb(red = aa["red"]/255
                       , green = aa["green"]/255
                       , blue = aa["blue"]/255
                       , alpha = ifelse(RULES.sel$A1[i] == TRUE, 1, 0.2))
              return(bb)
            }
          
          ## ----------------------------------------------------------------------
          pdf(file = "PRE_FATE_DOMINANT_STEP_2_selectedSpecies_PHYLO.pdf"
              , width = 10, height = 10)
          pp_phylo = {
            plot(hcp
                 , type = "fan" ## unrooted, phylogram
                 , cex = 0.5
                 , label.offset = 0.01
                 , lab4ut = "axial"
                 , edge.color = edge_col
                 , edge.width = 1.5
                 , edge.lty = edge_lty
                 , tip.color = tip_col
            )
            legend("topleft"
                   , legend = names(pal_col)
                   , col = pal_col
                   , pch = 15
                   , pt.cex = 2
                   , bty ="n"
                   , xpd = TRUE)
            title(main = "STEP 2 : Selected dominant species"
                  , sub = paste0("Colors highlight the rules of selection.\n"
                                 , "Species not meeting any criteria or only A1 have been removed.\n"
                                 , "Priority has been set to A2, B1 and B2 rules, rather than C. \n"
                                 , "Hence, species selected according to A2, B1 and/or B2 can also meet criterion C\n"
                                 , "while species selected according to C do not meet any of the three criteria.\n"
                                 , "Species selected according to one (or more) criterion but not meeting criterion A1 are transparent."
                                 , "\n")
                  , adj = 0)
          }
          dev.off()
          
          ## ----------------------------------------------------------------------
          ## Transform distance matrix into PCO -----------------------------------
          PCO = dudi.pco(mat.dist, scannf = FALSE, nf = 3) ## PCO
          
          if (ncol(PCO$li) > 1)
          {
            PCO.li = PCO$li
            PCO.li$PFG = factor(RULES.sel$SELECTION)
            PCO.li$selected = RULES.sel$A1
            ind_sel = which(PCO.li$selected == TRUE)
            
            ## GET inertia values
            inert = inertia.dudi(PCO)$tot.inertia
            inert = c(inert$`cum(%)`[1]
                      , inert$`cum(%)`[2] - inert$`cum(%)`[1]
                      , inert$`cum(%)`[3] - inert$`cum(%)`[2])
            
            ## --------------------------------------------------------------------
            num.axis = 2:length(which(!is.na(inert)))
            pp_pco.list = foreach(i.axis = num.axis) %do%
              {
                ## GET ellipses when A1 = TRUE
                PCO.ELL = .getELLIPSE(xy = PCO.li[ind_sel, c("A1", paste0("A", i.axis))]
                                      , fac = PCO.li$PFG[ind_sel])
                PCO.ELL$selected = TRUE
                PCO.pts = merge(PCO.li[ind_sel, ]
                                , unique(PCO.ELL[, c("xlabel", "ylabel", "PFG")])
                                , by = "PFG")
                PCO.pts$selected = TRUE
                labels.ELL = unique(PCO.ELL[, c("xlabel", "ylabel", "PFG", "selected")])
                
                ## GET ellipses when A1 = FALSE
                if (length(unique(PCO.li$PFG[-ind_sel])) > 1)
                {
                  tmp.ELL = .getELLIPSE(xy = PCO.li[-ind_sel, c("A1", paste0("A", i.axis))]
                                        , fac = PCO.li$PFG[-ind_sel])
                  tmp.ELL$selected = FALSE
                  tmp.pts = merge(PCO.li[-ind_sel, ]
                                  , unique(tmp.ELL[, c("xlabel", "ylabel", "PFG")])
                                  , by = "PFG")
                  tmp.pts$selected = FALSE
                  
                  PCO.ELL = rbind(PCO.ELL, tmp.ELL)
                  PCO.pts = rbind(PCO.pts, tmp.pts)
                }
                
                
                pp_pco = ggplot(PCO.li, aes_string(x = "A1"
                                                   , y = paste0("A", i.axis)
                                                   , color = "PFG"
                                                   , alpha = "as.numeric(selected)"
                                                   , linetype = "selected")) +
                  geom_point() +
                  geom_hline(yintercept = 0, color = "grey30", lwd = 1) +
                  geom_vline(xintercept = 0, color = "grey30", lwd = 1) +
                  geom_segment(data = PCO.pts, aes_string(xend = "xlabel"
                                                          , yend = "ylabel"
                                                          , size = "selected")) +
                  geom_path(data = PCO.ELL, aes_string(x = "x", y = "y", size = "selected")) +
                  geom_label_repel(data = labels.ELL, aes_string(x = "xlabel"
                                                                 , y = "ylabel"
                                                                 , label = "PFG")) +
                  scale_y_continuous(position = "right", labels = NULL
                                     , sec.axis = sec_axis(~ . + 0)) +
                  scale_color_manual(guide = FALSE, values = pal_col) +
                  scale_alpha(guide = FALSE, range = c(0.2, 1)) +
                  scale_linetype_manual(guide = FALSE, values = c("TRUE" = 1, "FALSE" = 2)) +
                  scale_size_manual(guide = FALSE, values = c("TRUE" = 0.8, "FALSE" = 0.5)) +
                  labs(x = paste0("\nAXIS 1 = ", round(inert[1], 1), "% of inertia")
                       , y = paste0("AXIS ", i.axis, " = ", round(inert[i.axis], 1), "% of inertia\n")
                       , title = "STEP 2 : Selected dominant species"
                       , subtitle = paste0("Colors highlight the rules of selection.\n"
                                           , "Species not meeting any criteria or only A1 have been removed.\n"
                                           , "Priority has been set to A2, B1 and B2 rules, rather than C. \n"
                                           , "Hence, species selected according to A2, B1 and/or B2 can also meet criterion C\n"
                                           , "while species selected according to C do not meet any of the three criteria.\n"
                                           , "Species selected according to one (or more) criterion but not meeting criterion A1 are transparent."
                                           , "\n")) +
                  .getGraphics_theme() +
                  theme(axis.title = element_text(size = 12))
                
                return(pp_pco)
              }
            names(pp_pco.list) = paste0("Axis1_Axis", num.axis)
            pp_pco.list = pp_pco.list[names(pp_pco.list)[which(sapply(pp_pco.list, is.null) == FALSE)]]
            
            ## --------------------------------------------------------------------
            if (!is.null(pp_pco.list))
            {
              pdf(file = "PRE_FATE_DOMINANT_STEP_2_selectedSpecies_PCO.pdf"
                  , width = 10, height = 8)
              for (pp in pp_pco.list) if (!is.null(pp)) plot(pp)
              dev.off()
            }
          } ## END ncol(PCO$li) > 1
        } ## END is.na(is.euclid(mat.dist))
      } ## END length(sel.sp) <= 1
    }
    
    
    ###########################################################################
    ## ILLUSTRATION of robustness ---------------------------------------------
    
    if (opt.doRobustness)
    {
      cat("\n> Illustration of robustness")
      
      names.subset = c("All dataset", levels(RULES.robustness$SELECTION))
      pp_rob.list = foreach(i.subset = names.subset) %do%
        {
          if (i.subset != "Not selected")
          {
            if (i.subset == "All dataset") {
              tab.subset = RULES.robustness
            } else {
              tab.subset = RULES.robustness[which(RULES.robustness$SELECTION == i.subset), ]
            }
            
            if (nrow(tab.subset) > 0)
            {
              ## NO OF SPECIES
              tab.noSp = tapply(X = as.numeric(tab.subset$SELECTED)
                                , INDEX = list(tab.subset$type
                                               , tab.subset$percent
                                               , tab.subset$rep)
                                , FUN = sum
                                , na.rm = TRUE)
              tab.noSp = melt(tab.noSp)
              colnames(tab.noSp) = c("type", "percent", "rep", "value")
              tab.noSp$analysis = "no.sp"
              tab.noSp$value = tab.noSp$value / length(sel.sp)
              
              ## SIMILAR SPECIES
              tab.simSp = foreach(i.type = combi$type
                                  , i.rep = combi$rep
                                  , i.percent = combi$percent
                                  , .combine = "rbind") %do%
                {
                  tmp = tab.subset[which(tab.subset$type == i.type &
                                           tab.subset$percent == i.percent &
                                           tab.subset$rep == i.rep), ]
                  tmp.sp1 = tmp$species[which(tmp$SELECTED == TRUE)]
                  tmp.sp2 = tmp.sp1[which(tmp.sp1 %in% sel.sp)]
                  percent1 = length(tmp.sp2) / length(tmp.sp1)
                  percent2 = length(tmp.sp2) / length(sel.sp)
                  return(data.frame(type = i.type
                                    , percent = i.percent
                                    , rep = i.rep
                                    , analysis = c("percent", "all")
                                    , value = c(percent1, percent2)
                                    , stringsAsFactors = FALSE))
                }
              
              ## PLOT
              tab.plot = rbind(tab.noSp, tab.simSp)
              tab.plot$analysis = factor(tab.plot$analysis, c("no.sp", "all", "percent"))
              tab.plot$percent_fac = factor(tab.plot$percent, seq(0, 1, 0.1))
              tab.plot$percent_num = as.numeric(tab.plot$percent_fac)
              
              pp_rob = ggplot(tab.plot, aes_string(y = "value", color = "type")) +
                geom_boxplot(aes_string(x = "percent_fac"), na.rm = TRUE) +
                geom_smooth(aes_string(x = "percent_num"), method = "loess", na.rm = TRUE) +
                facet_wrap(analysis ~ ., ncol = 3
                           , labeller = as_labeller(c("no.sp" = "a"
                                                      , "all" = "b"
                                                      , "percent" = "c"))) +
                scale_x_discrete(name = "\nPercentage of observations (%)"
                                 , breaks = seq(0, 1, 0.2)
                                 , labels = seq(0, 1, 0.2) * 100
                                 , drop = FALSE) +
                scale_y_continuous(name = "Percentage of similarity (%)\n"
                                   , breaks = seq(0, 1, 0.1)
                                   , labels = seq(0, 1, 0.1) * 100) +
                scale_color_manual(guide = FALSE
                                   , values = c("releves" = "midnightblue"
                                                , "sites" = "brown")) +
                labs(title = paste0("STEP 2 : Selected dominant species - robustness(", i.subset, ")")
                     , subtitle = paste0("Selection is run on a subset S, keeping only a "
                                         , "percentage of releves (blue) "
                                         , "or sites (red) from original data set O :\n"
                                         , "  > a = number(S species) / number(O species) \n"
                                         , "  > b = number(S & O species) / number(O species) \n"
                                         , "  > c = number(S & O species) / number(S species) \n"
                                         , "\n")) +
                .getGraphics_theme() +
                theme(axis.title = element_text(size = 12))
              
              return(pp_rob)
            }
          }
        }
      names(pp_rob.list) = names.subset
      pp_rob.list = pp_rob.list[names(pp_rob.list)[which(sapply(pp_rob.list, is.null) == FALSE)]]
      
      ## ----------------------------------------------------------------------
      if (!is.null(pp_rob.list))
      {
        pdf(file = "PRE_FATE_DOMINANT_STEP_2_selectedSpecies_robustness.pdf"
            , width = 10, height = 8)
        for (pp in pp_rob.list) if (!is.null(pp)) plot(pp)
        dev.off()
      }
    } ## opt.doRobustness
  }
  
  #############################################################################
  
  cat("\n> Done!\n")
  
  results = list(species.selected = sel.sp, tab.rules = RULES)
  if (opt.doRobustness) {
    results$tab.robustness = RULES.robustness
  }
  if (opt.doPlot) {
    if (doRuleA && exists("pp_tot")) results$plot.A = pp_tot
    if (doRuleB && exists("pp_B")) results$plot.B = list(abs = pp_B, rel = pp_B.rel)
    if (doRuleC && exists("pp_hab")) results$plot.C = pp_hab
    if (exists("pp_pco.list")) results$plot.pco = pp_pco.list
    if (opt.doRobustness && exists("pp_rob.list")) results$plot.robustness = pp_rob.list
  }
  return(results)
}

