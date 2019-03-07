
getTraitsFATE_names = function()
{
  ## Life history related traits
  TRAIT_names.lh = c("AGEFLOW_MIN", "AGEFLOW_MAX",
                     # "FECUNDITY", "GERMRATE",
                     "MAX_AGE",
                     "LIFESPAN",
                     "PL_H_flora_MIN",
                     "PL_REPR_H",
                     "PL_VEG_H",
                     "PL_VEG_H_max",
                     "LHIST", #"LHIST_INDK",
                     "WOODY")
  
  ## Dispersal related traits
  TRAIT_names.disp = c("DISP_VITTOZ")
  
  ## Light related traits
  TRAIT_names.light = c("ELLLITE", "ELLLITE_INDK", "ELLLITE_INDK_VAR")
  
  ## Soil related traits
  TRAIT_names.soil = c("ELLHUMUS_INDK", "ELLHUMUS_INDK_VAR",
                       "ELLMOIST", "ELLMOIST_INDK", "ELLMOIST_INDK_VAR",
                       "ELLNIT", "ELLNIT_INDK", "ELLNIT_INDK_VAR",
                       "ELLPH",
                       "ELLREAC_INDK", "ELLREAC_INDK_VAR",
                       "LDMC",
                       "LNC")
  
  ## Disturbances related traits
  TRAIT_names.dist = c("CATTLE",
                       "GRAZ",
                       "MOW", "MOW_TOLERANCE_INDK",
                       "Palatability")
  
  TRAIT_names = c(TRAIT_names.lh
                  , TRAIT_names.disp
                  , TRAIT_names.light
                  , TRAIT_names.soil
                  , TRAIT_names.dist)
  return(TRAIT_names)
}


getTraitsFATE_merge = function(traits.quant, traits.quali, TRAIT_names)
{
  ## Gather qualitative and quantitative traits
  TAB_traits.df = rbind(traits.quant, traits.quali)
  
  ## Keep only traits of interest
  TAB_traits.df = TAB_traits.df[which(TAB_traits.df$CODE %in% TRAIT_names), ]
  # table(TAB_traits.df$CODE)
  
  ## Transform into species x traits matrix
  TAB_traits = tapply(X = TAB_traits.df$value
                      , INDEX = list(TAB_traits.df$code_cbna, TAB_traits.df$CODE)
                      , FUN = median, na.rm = T)
  TAB_traits = as.data.frame(TAB_traits)
  
  no_NA = apply(TAB_traits, 2, function(x) length(which(is.na(x))))
  TAB_traits = TAB_traits[, which(no_NA < nrow(TAB_traits))]
  
  TAB_traits$CODE_CBNA = rownames(TAB_traits)
  # head(TAB_traits)
  # summary(TAB_traits)
  
  return(TAB_traits)
}


getTraitsFATE_reorganize = function(TAB_traits)
{
  TAB_traits_FATE = data.frame(CODE_CBNA = TAB_traits$CODE_CBNA)
  
  ## Maturity ---------------------------------------------------------------------------------- ##
  TAB_traits$AGEFLOW_MAX = as.numeric(as.character(TAB_traits$AGEFLOW_MAX))
  TAB_traits$AGEFLOW_MIN = as.numeric(as.character(TAB_traits$AGEFLOW_MIN))
  TAB_traits_FATE$MATURITY = apply(as.matrix(TAB_traits[, c("AGEFLOW_MIN", "AGEFLOW_MAX")])
                                   , 1, function(x) min(x, na.rm = T))
  TAB_traits_FATE$MATURITY[which(is.infinite(TAB_traits_FATE$MATURITY))] = NA
  
  ## Longevity --------------------------------------------------------------------------------- ##
  TAB_traits$MAX_AGE = as.numeric(as.character(TAB_traits$MAX_AGE))
  
  ind.1 = intersect(which(TAB_traits$MAX_AGE == 1)
                    , grep("biennial|pluriennial", TAB_traits$LIFESPAN))
  if (length(ind.1) > 0) TAB_traits$MAX_AGE[ind.1] = NA
  
  ind.2 = which(TAB_traits$AGEFLOW_MAX > TAB_traits$MAX_AGE)
  if (length(ind.2) > 0) TAB_traits$MAX_AGE[ind.2] = TAB_traits$AGEFLOW_MAX[ind.2]
  
  ind.3 = which(is.na(TAB_traits$AGEFLOW_MAX) & TAB_traits$AGEFLOW_MIN > TAB_traits$MAX_AGE)
  if (length(ind.3) > 0) TAB_traits$MAX_AGE[ind.3] = TAB_traits$AGEFLOW_MIN[ind.3]
  
  TAB_traits_FATE$LONGEVITY = TAB_traits$MAX_AGE
  
  ## si max_age = 1 mais lifespan != annual, max_age = NA
  ## si ageflow_max > max_age, max_age = ageflow_max
  ## si ageflow_min > max_age, max_age = ageflow_min
  # TAB_traits_FATE$MATURITY[which(TAB_traits$LIFESPAN == "annual")] = 0
  # TAB_traits_FATE$MATURITY[which(TAB_traits$MAX_AGE == 1)] = 0
  
  ## Height ------------------------------------------------------------------------------------ ##
  TAB_traits$PL_VEG_H = as.numeric(as.character(TAB_traits$PL_VEG_H))
  
  ind.1 = intersect(which(is.na(TAB_traits$PL_VEG_H))
                    , which(!is.na(TAB_traits$PL_VEG_H_max)))
  if (length(ind.1) > 0) TAB_traits$PL_VEG_H[ind.1] = TAB_traits$PL_VEG_H_max[ind.1]
  
  TAB_traits_FATE$HEIGHT = TAB_traits$PL_VEG_H
  
  ## LH - WOODY -------------------------------------------------------------------------------- ##
  TAB_traits$WOODY = as.character(TAB_traits$WOODY)
  TAB_traits$WOODY[which(is.na(TAB_traits$WOODY))] = "unknown"
  WOODY.table = as.data.frame.matrix(table(TAB_traits$LHIST, TAB_traits$WOODY))
  WOODY.table = WOODY.table[which(WOODY.table$unknown > 0), ]
  WOODY.table = WOODY.table[which(rowSums(WOODY.table[, c("Frutescent", "Suffrutescent", "Herbaceous")]) > 0), ]
  WOODY.table = WOODY.table[which(apply(WOODY.table, 1, function(x) length(which(x == 0))) == 2), ]
  WOODY.table = WOODY.table[which(rowSums(WOODY.table[, c("Frutescent", "Suffrutescent", "Herbaceous")]) > 10), ]

  for(i in 1:nrow(WOODY.table))
  {
    ind.i = which(TAB_traits$WOODY == "unknown" & TAB_traits$LHIST == rownames(WOODY.table)[i])
    TAB_traits$WOODY[ind.i] = colnames(WOODY.table)[1:3][which(WOODY.table[i, 1:3] > 0)]
  }

  TAB_traits_FATE$WOODY = TAB_traits$WOODY
  
  ## LH - LHIST -------------------------------------------------------------------------------- ##
  TAB_traits$LHIST = as.character(TAB_traits$LHIST)
  
  ## Remove non-informative subclasses for FATE PFG
  TAB_traits$LHIST = sub("Climber_|_Climber", "", TAB_traits$LHIST)
  TAB_traits$LHIST = sub("Carnivorous_|_Carnivorous", "", TAB_traits$LHIST)
  TAB_traits$LHIST = sub("Saprophyte_|_Saprophyte", "", TAB_traits$LHIST)
  TAB_traits$LHIST = sub("Parasite_|_Parasite", "", TAB_traits$LHIST)
  TAB_traits$LHIST = sub("Therophyte_|_Therophyte", "", TAB_traits$LHIST) ## if associated, not necessarily annual
  TAB_traits$LHIST = sub("Helophyte_|_Helophyte", "", TAB_traits$LHIST) ## if associated, not necessarily in water
  
  ## Remove non-informative classes for FATE PFG
  TAB_traits$LHIST[grep("Pleustophyte", TAB_traits$LHIST)] = "" ## algae
  TAB_traits$LHIST[grep("Epiphyte", TAB_traits$LHIST)] = "" ## not growing on soil, do not fill height strata
  
  ## Species above 4 meters : phanerophyte
  TAB_traits$LHIST[which(TAB_traits_FATE$HEIGHT >= 400)] = "Phanerophyte"
  
  ## Divide chamaephyte into 2 classes, according to WOODY trait
  TAB_traits$LHIST[grep("Chamaephyte", TAB_traits$LHIST)] = "Chamaephyte"
  TAB_traits$LHIST[grep("Hemicryptophyte_Phanerophyte", TAB_traits$LHIST)] = "Chamaephyte"
  TAB_traits$LHIST[which(TAB_traits$CODE_CBNA == "16806")] = "Chamaephyte"
  TAB_traits$LHIST[which(TAB_traits$LHIST == "Chamaephyte" & TAB_traits$WOODY == "Herbaceous")] = "Chamaephyte_H"
  TAB_traits$LHIST[which(TAB_traits$LHIST == "Chamaephyte" & TAB_traits$WOODY == "unknown")] = "Chamaephyte_H"
  TAB_traits$LHIST[which(TAB_traits$LHIST == "Chamaephyte" & TAB_traits$WOODY == "Suffrutescent")] = "Chamaephyte_S"
  TAB_traits$LHIST[which(TAB_traits$LHIST == "Chamaephyte" & TAB_traits$WOODY == "Frutescent")] = "Chamaephyte_S"
  
  ## Gather aquatic species
  TAB_traits$LHIST[grep("Helophyte", TAB_traits$LHIST)] = "Helophyte_Hydrophyte"
  TAB_traits$LHIST[grep("Hydrophyte", TAB_traits$LHIST)] = "Helophyte_Hydrophyte"
  TAB_traits$LHIST[grep("Hydrotherophyte", TAB_traits$LHIST)] = "Helophyte_Hydrophyte"
  
  ## Gather geophyte and/or hemicryptophyte species
  TAB_traits$LHIST[grep("Geophyte|Geophyte_Hemicryptophyte|Hemicryptophyte", TAB_traits$LHIST)] = "Geophyte_Hemicryptophyte"
  
  TAB_traits_FATE$LHIST = TAB_traits$LHIST
  TAB_traits_FATE$LHIST[which(nchar(TAB_traits_FATE$LHIST) == 0)] = NA
  
  ## Dispersal --------------------------------------------------------------------------------- ##
  TAB_traits$DISP_VITTOZ = ordered(factor(TAB_traits$DISP_VITTOZ, 1:7))
  TAB_traits_FATE$DISPERSAL = TAB_traits$DISP_VITTOZ
  
  ## Light ------------------------------------------------------------------------------------- ##
  TAB_traits$ELLLITE_INDK = ordered(factor(TAB_traits$ELLLITE_INDK, 1:5))
  TAB_traits_FATE$LIGHT = TAB_traits$ELLLITE_INDK
  
  ## Light tolerance --------------------------------------------------------------------------- ##
  TAB_traits$ELLLITE_INDK_VAR = as.numeric(factor(TAB_traits$ELLLITE_INDK_VAR
                                                  , c("small variation (I)", "large variation (II)")))
  TAB_traits_FATE$LIGHT_TOLERANCE = TAB_traits$ELLLITE_INDK_VAR
  
  ## Soil contrib ------------------------------------------------------------------------------ ##
  TAB_traits$ELLNIT_INDK = ordered(factor(TAB_traits$ELLNIT_INDK, seq(1,5,0.5)))
  TAB_traits_FATE$NITROGEN = TAB_traits$ELLNIT_INDK
  
  ## Soil tolerance ---------------------------------------------------------------------------- ##
  TAB_traits$ELLNIT_INDK_VAR = as.numeric(factor(TAB_traits$ELLNIT_INDK_VAR
                                                 , c("small variation (I)", "large variation (II)")))
  TAB_traits_FATE$NITROGEN_TOLERANCE = TAB_traits$ELLNIT_INDK_VAR
  
  ## Soil moisture ----------------------------------------------------------------------------- ##
  TAB_traits$ELLMOIST_INDK = ordered(factor(TAB_traits$ELLMOIST_INDK, seq(1,5,0.5)))
  TAB_traits_FATE$MOISTURE = TAB_traits$ELLMOIST_INDK
  
  ## Grazing and mowing tolerance, Palatability ------------------------------------------------ ##
  TAB_traits$GRAZ = as.numeric(factor(TAB_traits$GRAZ
                                   , c("intolerant"
                                       , "intolerant to sensitive"
                                       , "sensitive"
                                       , "sensitive to moderately tolerant"
                                       , "moderately tolerant"
                                       , "moderately tolerant to well tolerant"
                                       , "well tolerant"
                                       , "well tolerant to very tolerant"
                                       , "very tolerant")))
  TAB_traits$MOW = as.numeric(factor(TAB_traits$MOW
                                   , c("intolerant"
                                       , "intolerant to sensitive"
                                       , "sensitive"
                                       , "sensitive to moderately tolerant"
                                       , "moderately tolerant"
                                       , "moderately tolerant to well tolerant"
                                       , "well tolerant"
                                       , "well tolerant to very tolerant"
                                       , "very tolerant")))
  TAB_traits$MOW_TOLERANCE_INDK = as.numeric(factor(TAB_traits$MOW_TOLERANCE_INDK, 1:5))
  TAB_traits$Palatability = as.numeric(factor(TAB_traits$Palatability, 0:3))
  
  # plot(as.factor(TAB_traits$MOW_TOLERANCE_INDK), TAB_traits$GRAZ)
  # plot(as.factor(TAB_traits$MOW_TOLERANCE_INDK), TAB_traits$MOW)
  # plot(as.factor(TAB_traits$MOW_TOLERANCE_INDK), TAB_traits$Palatability)
  
  ind.1 = intersect(which(is.na(TAB_traits$MOW_TOLERANCE_INDK))
                    , which(!is.na(TAB_traits$MOW)))
  if (length(ind.1) > 0) TAB_traits$MOW[ind.1] = c(1, 1, 2, 2, 3, 3, 4, 4, 5)[TAB_traits$MOW[ind.1]]
  if (length(ind.1) > 0) TAB_traits$MOW_TOLERANCE_INDK[ind.1] = TAB_traits$MOW[ind.1]
  
  TAB_traits_FATE$GRAZ_MOW_TOLERANCE = TAB_traits$MOW_TOLERANCE_INDK
  TAB_traits_FATE$PALATABILITY = TAB_traits$Palatability
  
  
  return(TAB_traits_FATE)
}
