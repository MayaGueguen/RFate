

getTraits_1_merge = function(traits)
{
  ## Merge together similar quantitative traits
  traits.code = data.frame(CODE = sort(unique(traits$code)), stringsAsFactors = F)
  traits.code$CODE_simplified = traits.code$CODE
  traits.code$CODE_simplified[which(traits.code$CODE_simplified %in% c("FECUN HERB (CBNA)", "FECUN WOODY (CBNA)"))] = "FECUNDITY"
  traits.code$CODE_simplified[which(traits.code$CODE_simplified %in% c("L_AREA", "L_AREA_(without_pet)"))] = "L_AREA"
  traits.code$CODE_simplified[which(traits.code$CODE_simplified %in% c("LDMC", "LDMCp", "LDMCst"))] = "LDMC"
  traits.code$CODE_simplified[which(traits.code$CODE_simplified %in% c("LNCp", "LNCst"))] = "LNC"
  traits.code$CODE_simplified[which(traits.code$CODE_simplified %in% c("LPC", "LPCp_molyb"))] = "LPC"
  traits.code$CODE_simplified[which(traits.code$CODE_simplified %in% c("MAX_AGE (CBNA)", "MAX_AGE_INDK"))] = "MAX_AGE"
  traits.code$CODE_simplified[which(traits.code$CODE_simplified %in% c("PL_REPR_H", "PL_REPR_H (ARVES)"))] = "PL_REPR_H"
  traits.code$CODE_simplified[which(traits.code$CODE_simplified %in% c("PL_H_AVG_flora", "PL_VEG_H (ARVES)", "PL_VEG_H_mean"))] = "PL_VEG_H"
  traits.code$CODE_simplified[which(traits.code$CODE_simplified %in% c("PL_H_flora_MAX", "PL_H_MAX_flora", "PL_VEG_H_max"))] = "PL_VEG_H_max"
  traits.code$CODE_simplified[which(traits.code$CODE_simplified %in% c("SLA", "SLAp", "SLAst"))] = "SLA"
  
  traits = merge(traits, traits.code, by.x = "code", by.y = "CODE")
  
  traits$genre = sapply(traits$libelle, function(x) strsplit(x, " ")[[1]][1])
  
  return(traits)
}

traits_genre = function(traits)
{
  traits.genre = unique(traits[, c("code_cbna", "libelle", "genre")])
  # View(data.frame(sort(table(traits.genre$genre))))
  # length(sort(unique(traits.genre$genre)))
  return(traits.genre)
}

getTraits_1_removeUninformative = function(traits)
{
  ## Remove traits with no values
  ind.noValue = which(is.na(traits$valeur) & nchar(traits$nom) == 0)
  head(traits[ind.noValue, ])
  if (length(ind.noValue) > 0) traits = traits[-ind.noValue, ]
  
  ## Remove traits with unknown information
  ind.Doubt = which(is.na(traits$valeur) & traits$nom %in% c("doubtful status"
                                                             , "unknown"
                                                             , "No Data"
                                                             , "value need to be checked"
                                                             , "no reliable information available"
                                                             , "unknown or no reliable information available"
                                                             , "not classified (not listed in the Red List Moser et al 2002)" ## ECOLOGICAL_GRP_INDK
                                                             , "Flowering not regularly" ## FLONSET_BEG_INDK, FLONSET_END_INDK
                                                             , "mentioned in literature, though lacking information" ## POLL_MAIN_INDK
                                                             , "not observed in the area" ## POLL_MAIN_INDK
                                                             , "species hardly grows in grasslands" ## MOW_TOLERANCE_INDK
                                                             , "no indication (epiphytes)" ## ELLMOISTVAR_INDK, ELLNIT_INDK, ELLREAC_INDK
                                                             , "no indication (indifferent taxon or epiphyte)"
                                                             , "no indication (taxa that are indifferent or do not root in the soil)" ## ELLAER_INDK
                                                             , "no indication (taxa that do not root in the soil)" ## ELLAER_INDK, ELLHUMUS_INDK
                                                             , "no roots present" ## ROOT_DEPTH_INDK
                                                             , "root depth unknown" ## ROOT_DEPTH_INDK
  ))
  ind.Doubt = c(ind.Doubt, which(traits$CODE_simplified %in% c("ELLAER_INDK"
                                                               , "ELLCONT_INDK"
                                                               , "ELLHUMUS_INDK"
                                                               , "ELLMOIST_INDK"
                                                               , "ELLMOISTVAR_INDK"
                                                               , "ELLNIT_INDK"
                                                               , "ELLREAC_INDK"
                                                               , "ELLTEMP_INDK") & traits$nom == "indifferent"))
  ind.Doubt = c(ind.Doubt, which(traits$CODE_simplified == "ROOT_DEPTH_INDK" & traits$nom == "Chamaephyte"))
  ind.Doubt = c(ind.Doubt, which(is.na(traits$valeur) & traits$nom == "maximum age unknown"))
  head(traits[ind.Doubt, ])
  if (length(ind.Doubt) > 0) traits = traits[-ind.Doubt, ]
  
  ind.DoubleVal = which(!is.na(traits$valeur) & nchar(traits$nom) > 0)
  head(traits[ind.DoubleVal, ])
  traits$nom[ind.DoubleVal] = NA
  
  ## Remove singularities
  ind.Singularity = which(traits$CODE_simplified == "CHANGE_TENDENCY_INDK")
  head(traits[ind.Singularity, ])
  traits$nom[ind.Singularity] = sub("[*]", "", traits$nom[ind.Singularity])
  
  ## Remove uninformative traits
  # ind.VAR = grep("_INDK_VAR$", traits$CODE_simplified)
  # head(traits[ind.VAR, ])
  # traits = traits[-ind.VAR, ]
  
  return(traits)
}

trait_remove = function(traits, trait.code)
{
  cat("\n ==> REMOVE trait ", trait.code, "\n")
  ind.trait = which(traits$CODE_simplified %in% trait.code)
  if (length(ind.trait) > 0) traits = traits[-ind.trait, ]
  
  return(traits)
}

trait_compare = function(traits, trait.code)
{
  cat("\n ==> COMPARE trait ", trait.code)
  (length(which(traits$CODE_simplified == trait.code)))
  tmp = traits[which(traits$CODE_simplified == trait.code), ]
  (length(table(tmp$code_cbna)))
}

traits_compare = function(traits, trait.code1, trait.code2)
{
  trait_compare(traits, trait.code1)
  trait_compare(traits, trait.code2)
}

getTraits_1_remove = function(traits)
{
  traits = trait_remove(traits, "HABITAT")
  traits = trait_remove(traits, "GEO_DISTRIB")
  traits = trait_remove(traits, "ELLHEAVYMET_INDK")
  
  ## Keep only one trait when 2 traits are redundant
  traits_compare(traits, "STRAT", "STRAT_INDK")
  traits = trait_remove(traits, "STRAT")
  
  traits_compare(traits, "VEGDISP", "VEG_DISP")
  traits = trait_remove(traits, "VEGDISP")
  
  traits_compare(traits, "REPROD", "REPROD_INDK")
  traits = trait_remove(traits, "REPROD")
  
  traits_compare(traits, "POLL", "POLL_MAIN_INDK")
  traits_compare(traits, "POLL", "POLL_2ND_INDK")
  traits = trait_remove(traits, "POLL")
  
  traits_compare(traits, "LHIST", "LHIST_INDK")
  # trait_remove(traits, "LHIST")
  
  return(traits)
}

getTraits_1_change = function(traits)
{
  ## Change from qualitative to quantitative
  ind.CLONRATE = which(traits$CODE_simplified == "CLONRATE")
  head(traits[ind.CLONRATE, ])
  traits$valeur[which(traits$CODE_simplified == "CLONRATE" & traits$nom == "<1")] = 0.5
  traits$valeur[which(traits$CODE_simplified == "CLONRATE" & traits$nom == "1")] = 1
  traits$valeur[which(traits$CODE_simplified == "CLONRATE" & traits$nom == "2-10")] = 6
  traits$valeur[which(traits$CODE_simplified == "CLONRATE" & traits$nom == ">10")] = 10
  traits$nom[ind.CLONRATE] = NA
  
  ## Change from quantitative to qualitative
  ind.PLOIDY = which(traits$CODE_simplified == "PLOIDY (VALUE)")
  head(traits[ind.PLOIDY, ])
  traits$nom[which(traits$CODE_simplified == "PLOIDY (VALUE)" & traits$valeur == 2)] = "diploid"
  traits$nom[which(traits$CODE_simplified == "PLOIDY (VALUE)" & traits$valeur == 3)] = "triploid"
  traits$nom[which(traits$CODE_simplified == "PLOIDY (VALUE)" & traits$valeur == 4)] = "tetraploid"
  traits$nom[which(traits$CODE_simplified == "PLOIDY (VALUE)" & traits$valeur == 5)] = "pentaploid"
  traits$nom[which(traits$CODE_simplified == "PLOIDY (VALUE)" & traits$valeur == 6)] = "hexaploid"
  traits$nom[which(traits$CODE_simplified == "PLOIDY (VALUE)" & traits$valeur == 7)] = "heptaploid"
  traits$nom[which(traits$CODE_simplified == "PLOIDY (VALUE)" & traits$valeur == 8)] = "octoploid"
  traits$nom[which(traits$CODE_simplified == "PLOIDY (VALUE)" & traits$valeur == 10)] = "decaploid"
  traits$nom[which(traits$CODE_simplified == "PLOIDY (VALUE)" & traits$valeur == 16)] = "16-ploid"
  traits$valeur[ind.PLOIDY] = NA
  traits$CODE_simplified[ind.PLOIDY] = "PLOIDY"
  
  ## Change qualitative classes
  ind.POLL = which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK"))
  head(traits[ind.POLL, ])
  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "cantarophilous")] = "entomogamous"
  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "hymenopterophilous")] = "entomogamous"
  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "lepidopterophilous")] = "entomogamous"
  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "melittophilous")] = "entomogamous"
  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "myophilous")] = "entomogamous"
  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "phalaenophilous")] = "entomogamous"
  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "pollinated by ants")] = "entomogamous"
  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "pollinated by bugs")] = "entomogamous"
  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "psychophilous")] = "entomogamous"
  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "sphingophilous")] = "entomogamous"

  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "vespidophilous")] = "ornithophilous"
  
  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "apogamous")] = "autogamous"
  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "mostly without fruits")] = "mostly without fruits-flowers"
  traits$nom[which(traits$CODE_simplified %in% c("POLL_MAIN_INDK", "POLL_2ND_INDK") & traits$nom == "mostly without flowers")] = "mostly without fruits-flowers"

  ## Change qualitative classes
  ind.LHIST = which(traits$CODE_simplified %in% c("LHIST"))
  head(traits[ind.LHIST, ])
  traits$nom[which(traits$CODE_simplified %in% c("LHIST") & traits$nom == "Hydrogeophyte")] = "Hydrophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST") & traits$nom == "Hydrohemicryptophyte")] = "Hydrophyte"
  
  ind.LHIST_INDK = which(traits$CODE_simplified %in% c("LHIST_INDK"))
  head(traits[ind.LHIST_INDK, ])
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "biennial to several-year old hapaxanthic species")] = "Geophyte_Hemicryptophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "carnivorous plants")] = "Carnivorous"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "chamaephyte-hemicryptophyte")] = "Chamaephyte_Hemicryptophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "climber")] = "Climber"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "epiphyte")] = "Epiphyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "full parasite")] = "Parasite"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "hemiparasite")] = "Parasite"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "geophyte")] = "Geophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "hemicryptophyte (long-lived)")] = "Hemicryptophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "hemicryptophyte (short-lived)")] = "Hemicryptophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "herbaceous chamaephyte")] = "Chamaephyte_Hemicryptophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "hydrophyte")] = "Hydrophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "nanophanerophyte")] = "Phanerophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "nanophanerophytes-hemicryptophyte")] = "Hemicryptophyte_Phanerophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "phanerophyte")] = "Phanerophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "pleustophyte")] = "Pleustophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "saprophyte")] = "Saprophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "therophyte")] = "Therophyte"
  traits$nom[which(traits$CODE_simplified %in% c("LHIST_INDK") & traits$nom == "woody chamaephyte")] = "Chamaephyte"
  
  traits$CODE_simplified[ind.LHIST_INDK] = "LHIST"
  
  return(traits)
}



getTraits_2_split = function(traits)
{
  traits_quant = traits[which(!is.na(traits$valeur)), ]
  traits_quali = traits[which(is.na(traits$valeur)), ]
  
  nrow(traits_quant) + nrow(traits_quali) == nrow(traits)
  
  code.quant = sort(unique(traits_quant$CODE_simplified))
  code.quali = sort(unique(traits_quali$CODE_simplified))
  intersect(code.quant, code.quali)
  
  return(list(traits_quant, traits_quali))
}

getTraits_3_quantMedian = function(traits_quant)
{
  traits_quant.split = split(traits_quant, traits_quant$CODE_simplified)
  traits_quant.median = foreach(i = 1:length(traits_quant.split), .combine = "rbind") %dopar%
  {
    cat(" ====> Calculating for trait :", names(traits_quant.split)[i], "\n")
    tab = traits_quant.split[[i]]
    tab.split = split(tab, tab$code_cbna)
    tab.median = foreach(j = 1:length(tab.split), .combine = "rbind") %do%
    {
      tab.sp = tab.split[[j]]
      val.source = sort(unique(c(tab.sp$expert, tab.sp$bibliographie)))
      val.source = val.source[which(nchar(val.source) > 0)]
      val.source = paste0(val.source, collapse = "_")
      return(data.frame(CODE = unique(tab.sp$CODE_simplified)
                        , code_cbna = unique(tab.sp$code_cbna)
                        , libelle = unique(tab.sp$libelle)
                        , genus = unique(tab.sp$genre)
                        , source = val.source
                        , value = median(tab.sp$valeur)))
    }
    return(tab.median)
  }
  
  return(traits_quant.median)
}


getTraits_3_qualiMerged = function(traits_quali)
{
  traits_quali.split = split(traits_quali, traits_quali$CODE_simplified)
  traits_quali.mean = foreach(i = 1:length(traits_quali.split), .combine = "rbind") %dopar%
  {
    cat(" ====> Calculating for trait :", names(traits_quali.split)[i], "\n")
    tab = traits_quali.split[[i]]
    tab.split = split(tab, tab$code_cbna)
    tab.mean = foreach(j = 1:length(tab.split), .combine = "rbind") %do%
    {
      tab.sp = tab.split[[j]]
      val.source = sort(unique(c(tab.sp$expert, tab.sp$bibliographie)))
      val.source = val.source[which(nchar(val.source) > 0)]
      val.source = paste0(val.source, collapse = "_")

      val.nom = sort(unique(tab.sp$nom))
      val.nom = sapply(val.nom, function(x) strsplit(as.character(x), "_")[[1]])
      val.nom = unlist(val.nom)
      val.nom = sort(unique(val.nom))
      val.nom = val.nom[which(nchar(val.nom) > 0)]
      val.nom = paste0(val.nom, collapse = "_")
      return(data.frame(CODE = unique(tab.sp$CODE_simplified)
                        , code_cbna = unique(tab.sp$code_cbna)
                        , libelle = unique(tab.sp$libelle)
                        , genus = unique(tab.sp$genre)
                        , source = val.source
                        , value = val.nom))
    }
    return(tab.mean)
  }
  
  ## Specific changes
  traits_quali.mean$value[which(traits_quali.mean$CODE == "CHANGE_TENDENCY_INDK" & traits_quali.mean$value == "=_>")] = "=->"
  traits_quali.mean$value[which(traits_quali.mean$CODE == "DISP_VITTOZ" & traits_quali.mean$value == "3_4")] = "4"
  traits_quali.mean$value[which(traits_quali.mean$CODE == "STRAT_INDK" & traits_quali.mean$value == "crs_css")] = "crs"
  traits_quali.mean$value[which(traits_quali.mean$value == "large variation (II)_small variation (I)")] = "large variation (II)"
  traits_quali.mean$value[which(traits_quali.mean$CODE == "WOODY" & traits_quali.mean$value == "Herbaceous_Suffrutescent")] = "Herbaceous"
  
  return(traits_quali.mean)
}

getTraits_3_thresholdGenus = function(traits, traits.genre)
{
  tmp = table(traits$CODE, traits$genus)
  tmp = as.data.frame(tmp)
  tmp = tmp[which(tmp$Freq > 0), ]
  tmp.bis = as.data.frame(table(tmp$Var1))
  
  ind.SUPPR = as.character(tmp.bis$Var1[which(tmp.bis$Freq < 0.5 * length(table(traits.genre$genre)))])
  traits = traits[-which(traits$CODE %in% ind.SUPPR), ]
  
  return( traits)
}

getTraits_4_save = function(traits, namefile)
{
  table(traits$CODE)
  table(traits$source)
  summary(traits)
  fwrite(traits, file = namefile, sep = "\t")
  ## TRAITS_quantitative_median_181210.csv
  ## TRAITS_qualitative_181210.csv
}


getTraits_4_graphBarplot = function(traits, namefile)
{
  tmp = table(traits$CODE)
  tmp = data.frame(tmp)
  tmp$colo = 1:nrow(tmp) %% 2
  
  ggplot(tmp, aes(x = Var1, y = Freq, fill = factor(colo))) +
    scale_fill_manual(guide = F, values = c("grey30", "grey60")) +
    geom_bar(stat = "identity") +
    labs(title = "Number of values (species) per trait") +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = c("grey30", "grey60")[tmp$colo + 1]))
  ggsave(file = namefile, width = 12, height = 10)
  ## GRAPH1_numberValuesPerTrait_quanti.pdf
  ## GRAPH2_numberValuesPerTrait_quali.pdf
}

# ## 
# tmp = traits_quant.median
# tmp$CAT = cut(tmp$value, breaks = c(-100,0,1,10,50,100,1000,100000,1000000), include.lowest = T)
# plot(table(tmp$CAT))
# ggplot(tmp, aes(x = CODE, y = value)) +
#   geom_boxplot() +
#   facet_wrap(~ CAT, scales = "free") +
#   theme_fivethirtyeight() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

