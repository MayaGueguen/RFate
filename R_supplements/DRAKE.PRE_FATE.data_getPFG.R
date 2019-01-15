

################################################################################################################
## 1. GET TRAITS
################################################################################################################


select_traits = function(mat.traits)
{
  mat.traits.select = data.frame(species = paste0("X", mat.traits$CODE_CBNA))
  mat.traits.select$GROUP = mat.traits$LHIST

  ## Fix ordered factors
  mat.traits.select$DISPERSAL = ordered(factor(mat.traits$DISPERSAL))
  mat.traits.select$LIGHT = ordered(factor(mat.traits$LIGHT))
  mat.traits.select$NITROGEN = ordered(factor(mat.traits$NITROGEN))
  mat.traits.select$MOISTURE = ordered(factor(mat.traits$MOISTURE))
  # mat.traits.select$PALATABILITY = ifelse(is.na(mat.traits$PALATABILITY), NA, paste0("pal", mat.traits$PALATABILITY))
  # mat.traits.select$PALATABILITY = ordered(factor(mat.traits.select$PALATABILITY))
  
  ## Take the root square of height:
  mat.traits.select$HEIGHT = as.numeric(log(as.numeric(mat.traits$HEIGHT)))
  # mat.traits.select$DISPERSAL = exp(as.numeric(as.character(mat.traits.select$DISPERSAL))) # Disp is not an ordered factor anymore!
  
  return(mat.traits.select)
}


################################################################################################################
## 2. DO CLUSTERING 
################################################################################################################

calc_dist_clust = function(zone.name, mat.traits.select, mat.overlap)
{
  # dir.create(paste0(zone.name, "/PFG_withoutSoil"))
  # setwd(paste0(zone.name, "/PFG_withoutSoil"))
  setwd(zone.name)
  
  mat.traits.DOM = mat.traits.select[which(mat.traits.select$species %in% colnames(mat.overlap)),]
  mat.traits.DOM = mat.traits.DOM[which(mat.traits.DOM$GROUP != ""), ]
  ind.GROUP = table(mat.traits.DOM$GROUP)
  ind.GROUP = names(ind.GROUP)[which(ind.GROUP <= 1)]
  if (length(ind.GROUP) > 0)
  {
    mat.traits.DOM = mat.traits.DOM[-which(mat.traits.DOM$GROUP %in% ind.GROUP), ]
  }
  
  sp.DIST = PRE_FATE.speciesDistance(mat.species.traits = mat.traits.DOM
                                     , mat.species.overlap = mat.overlap
                                     , min.info.thresh = 0.3)
  
  sp.CLUST = PRE_FATE.speciesClustering_step1(mat.species.DIST = sp.DIST)
  
  # setwd("./../../")
  setwd("./../")
  return(list(sp.DIST = sp.DIST, sp.CLUST = sp.CLUST))
}


calc_determ = function(zone.name, sp.DIST, sp.CLUST, no.clusters, species)
{
  # setwd(paste0(zone.name, "/PFG_withoutSoil"))
  setwd(zone.name)
  
  sp.DETERM = PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms
                                               , no.clusters = no.clusters
                                               , mat.species.DIST = sp.DIST)
  
  selected.sp = sp.DETERM$determ.all
  selected.sp$pfg = as.character(selected.sp$pfg)
  selected.sp$sp = as.character(selected.sp$sp)
  selected.sp$sp = sub("X", "", selected.sp$sp)
  
  selected.sp = merge(species, selected.sp, by.x = "numtaxon", by.y = "sp", all.y = TRUE)
  colnames(selected.sp)[c(1:4, 12)] = c("CODE_CBNA", "GENUS", "SPECIES_NAME", "PFG", "TO_REMOVE")
  selected.sp = selected.sp[, c("CODE_CBNA", "GENUS", "SPECIES_NAME", "PFG", "group", "TO_REMOVE"
                                , "sp.mean.dist", "allSp.mean", "allSp.min", "allSp.max")]
  selected.sp = selected.sp[order(selected.sp$PFG, selected.sp$SPECIES_NAME), ]
  
  fwrite(x = selected.sp, file = "determ.all.csv")
  
  determinant_PFG = sp.DETERM$determ.sp
  save(determinant_PFG, file = "determinant_PFG.RData")
  
  # setwd("./../../")
  setwd("./../")
  return(selected.sp)
}

################################################################################################################
## 3. CALCULATE MEDIAN / MEAN VALUES PER PFG
################################################################################################################

get_sites_pfg = function(zone.name, mat.sites.species, selected.sp)
{
  # setwd(paste0(zone.name, "/PFG_withoutSoil"))
  setwd(zone.name)
  
  PFG1 = sapply(selected.sp$PFG, function(x) strsplit(x, "_")[[1]][1])
  PFG1 = sapply(PFG1, function(x) strsplit(x, "")[[1]][1])
  
  PFG2 = sapply(selected.sp$PFG, function(x) strsplit(x, "_")[[1]][2])
  PFG2 = sapply(PFG2, function(x) strsplit(x, "")[[1]][1])
  PFG2 = ifelse(is.na(PFG2), "", PFG2)
  
  PFG3 = sapply(selected.sp$PFG, function(x) strsplit(x, "[.]")[[1]][2])
  
  selected.sp$PFG = paste0(PFG1, PFG2, PFG3)
  
  ind.toKeep = selected.sp$CODE_CBNA[which(selected.sp$TO_REMOVE == 0)]
  mat.sites.species = mat.sites.species[, which(colnames(mat.sites.species) %in% ind.toKeep)]
  
  mat.sites.pfg = foreach (fg = unique(selected.sp$PFG), .combine = "cbind") %do%
  {
    ind.fg = selected.sp$CODE_CBNA[which(selected.sp$PFG == fg)]
    ind.fg = which(colnames(mat.sites.species) %in% ind.fg)
    val.fg = mat.sites.species[, ind.fg, drop = FALSE]
    val.fg = apply(val.fg, 1, function(x){
      if (length(which(is.na(x))) == length(x)){
        return(NA)
      } else if (length(which(x == 1)) > 0){
        return(1)
      } else if (length(which(x == 0)) > 0){
        return(0)
      }
    })
    return(matrix(data = val.fg, ncol = 1, dimnames = list(rownames(mat.sites.species), fg)))
  }
  
  dim(mat.sites.pfg)
  mat.sites.pfg[1:10, 1:10]
  
  save(mat.sites.pfg, file = "mat.sites.pfg.RData")
  
  # setwd("./../../")
  setwd("./../")
  return(mat.sites.pfg)
}

################################################################################################################
## 4. CALCULATE MEDIAN / MEAN VALUES PER PFG
################################################################################################################

