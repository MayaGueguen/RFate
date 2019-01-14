

################################################################################################################
## GET TRAITS
################################################################################################################

setwd("/home/gueguen/Documents/_TUTOS/3_R/_PACKAGES/RFate/data")
zone.name = "Bauges"
load("/home/gueguen/Documents/_TUTOS/3_R/_PACKAGES/RFate/data/Bauges/mat.overlap.DOM.RData")
load("/home/gueguen/Documents/_TUTOS/3_R/_PACKAGES/RFate/data/Bauges/mat.sites.species.DOM.RData")
mat.traits = fread("/home/gueguen/Documents/_TUTOS/3_R/_PACKAGES/RFate/data/TRAITS_FATE_190111.csv")
# mat.traits$LHIST = sub("Climber_", "",mat.traits$LHIST)
# mat.traits$LHIST = sub("Carnivorous_", "",mat.traits$LHIST)
# mat.traits$LHIST = sub("_Saprophyte", "",mat.traits$LHIST)
# mat.traits$LHIST = sub("Parasite_|_Parasite", "",mat.traits$LHIST)
# mat.traits$LHIST = sub("_Therophyte", "",mat.traits$LHIST)
# mat.traits$LHIST = sub("Helophyte_|_Helophyte", "",mat.traits$LHIST)
# 
# mat.traits = mat.traits[-grep("Pleustophyte", mat.traits$LHIST), ]
# mat.traits = mat.traits[-grep("Epiphyte", mat.traits$LHIST), ]
# mat.traits$LHIST[grep("Chamaephyte", mat.traits$LHIST)] = "Chamaephyte"
# # mat.traits$LHIST[grep("Parasite", mat.traits$LHIST)] = "Parasite"
# mat.traits$LHIST[grep("Hemicryptophyte_Phanerophyte", mat.traits$LHIST)] = "Chamaephyte"
# mat.traits$LHIST[which(mat.traits$CODE_CBNA == "16806")] = "Chamaephyte"
# mat.traits$LHIST[which(mat.traits$LHIST == "Chamaephyte" & mat.traits$WOODY == "Herbaceous")] = "Chamaephyte_H"
# mat.traits$LHIST[which(mat.traits$LHIST == "Chamaephyte" & mat.traits$WOODY == "unknown")] = "Chamaephyte_H"
# mat.traits$LHIST[which(mat.traits$LHIST == "Chamaephyte" & mat.traits$WOODY == "Suffrutescent")] = "Chamaephyte_S"
# mat.traits$LHIST[which(mat.traits$LHIST == "Chamaephyte" & mat.traits$WOODY == "Frutescent")] = "Chamaephyte_S"
# mat.traits$LHIST[grep("Helophyte", mat.traits$LHIST)] = "Helophyte_Hydrophyte"
# mat.traits$LHIST[grep("Hydrophyte", mat.traits$LHIST)] = "Helophyte_Hydrophyte"
# mat.traits$LHIST[grep("Hydrotherophyte", mat.traits$LHIST)] = "Helophyte_Hydrophyte"
# mat.traits$LHIST[grep("Geophyte|Geophyte_Hemicryptophyte|Hemicryptophyte", mat.traits$LHIST)] = "Geophyte_Hemicryptophyte"

# ind.geo = grep("Geophyte", mat.traits$LHIST)
# ind.hemi = grep("Hemicryptophyte", mat.traits$LHIST)
# ind.thero = grep("Therophyte", mat.traits$LHIST)
# sort(table(mat.traits$LHIST))
# 
# mat.traits = merge(species, mat.traits, by.x = "numtaxon", by.y = "CODE_CBNA", all.y = T)
# table(mat.traits$LHIST, mat.traits$WOODY)
# loadd(species)
# 
# head(mat.traits)
# hop = mat.traits[, c("DISPERSAL", "LIGHT", "PALATABILITY", "HEIGHT")]
# hop$HEIGHT = as.numeric(log(as.numeric(hop$HEIGHT)))
# hop$DISPERSAL = exp(as.numeric(as.character(hop$DISPERSAL))) # Disp is not an ordered factor anymore!
# boxplot(hop)

tt = function(zone.name, mat.traits, mat.overlap)
{
  mat.traits.select = data.frame(species = paste0("X", mat.traits$CODE_CBNA))
  mat.traits.select$GROUP = mat.traits$LHIST
  
  ## Check correspondance
  # length(setdiff(colnames(mat.overlap), mat.traits$species))
  # length(setdiff(mat.traits$species, colnames(mat.overlap)))
  
  
  ## Fix ordered factors
  mat.traits.select$DISPERSAL = ordered(factor(mat.traits$DISPERSAL))
  mat.traits.select$LIGHT = ordered(factor(mat.traits$LIGHT))
  mat.traits.select$PALATABILITY = ifelse(is.na(mat.traits$PALATABILITY), NA, paste0("pal", mat.traits$PALATABILITY))
  mat.traits.select$PALATABILITY = ordered(factor(mat.traits.select$PALATABILITY))
  
  ## Take the root square of height:
  mat.traits.select$HEIGHT = as.numeric(sqrt(as.numeric(mat.traits$HEIGHT)))
  mat.traits.select$DISPERSAL = exp(as.numeric(as.character(mat.traits.select$DISPERSAL))) # Disp is not an ordered factor anymore!
  
  return(mat.traits.select)
}



################################################################################################################
## DO CLUSTERING : WITHOUT SOIL
################################################################################################################

function(zone.name, mat.traits.select, mat.overlap)
{
  dir.create(paste0(zone.name, "/PFG_withoutSoil"))
  setwd(paste0(zone.name, "/PFG_withoutSoil"))
  
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
  # sp.DIST$Herbaceous = as.matrix(sp.DIST$Herbaceous)
  # nn = apply(sp.DIST$Herbaceous, 2, function(x) length(which(is.na(x))))
  # sp.DIST$Herbaceous = sp.DIST$Herbaceous[which(nn == 0), which(nn == 0)]
  # sp.DIST$Herbaceous = as.dist(sp.DIST$Herbaceous)
  
  sp.CLUST = PRE_FATE.speciesClustering_step1(mat.species.DIST = sp.DIST)
  
  setwd("./../../")
  return(list(sp.DIST = sp.DIST, sp.CLUST = sp.CLUST))
}


function(zone.name, sp.DIST, sp.CLUST, no.clusters, mat.traits)
{
  setwd(paste0(zone.name, "/PFG_withoutSoil"))
  
  sp.DETERM = PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms
                                               , no.clusters = c(4,6,6,9,3)
                                               # , no.clusters = c(7, 4, 7)
                                               # , no.clusters = c(2, 8, 6)
                                               # , no.clusters = c(2, 7, 5)
                                               , mat.species.DIST = sp.DIST)
  
  selected.sp = sp.DETERM$determ.all
  selected.sp$sp = as.character(selected.sp$sp)
  selected.sp$sp = sub("X", "", selected.sp$sp)
  loadd(species)
  selected.sp = merge(selected.sp, species, by.x = "sp", by.y = "numtaxon")
  selected.sp$pfg = as.character(selected.sp$pfg)
  tt = (selected.sp[, c("pfg", "libcbna", "toSuppr")])
  fwrite(x = tt, file = "determ.all.csv")
  # fwrite(x = selected.sp, file = "determ.all.csv")
  
  # selected.sp = sp.DETERM$determ.all
  # selected.sp$sp = as.character(selected.sp$sp)
  # selected.sp = merge(selected.sp, mat.traits[, c("taxon.no", "spec_name")]
  #                     , by.x = "sp", by.y = "taxon.no")
  # fwrite(x = selected.sp, file = "determ.all.csv")
  
  setwd("./../../")
  return(selected.sp)
}

################################################################################################################
## DO CLUSTERING : WITH SOIL
################################################################################################################

# load(paste0(path_save, "TAB_soil_SP"))
# TAB_soil_SP$SP = paste0("X", TAB_soil_SP$SP)
# TAB_soil_SP = TAB_soil_SP[which(TAB_soil_SP$TRAIT == "ELLNIT"),]
# TAB_soil_SP = TAB_soil_SP[, c("SP", "MEAN")]
# colnames(TAB_soil_SP) = c("species", "ELLNIT")
# head(TAB_soil_SP)
# 
# TRAITS_SOIL = merge(TRAITS, TAB_soil_SP, by = "species")
# TRAITS_SOIL$ELLNIT = floor(TRAITS_SOIL$ELLNIT)
# TRAITS_SOIL$ELLNIT = ordered(factor(TRAITS_SOIL$ELLNIT))
# summary(TRAITS_SOIL)
# 
# save(TRAITS_SOIL, file = paste0(path_save, "TRAITS_SOIL"))
# 
# dir.create(paste0(path_save, "PFG_withSoil"))
# setwd(paste0(path_save, "PFG_withSoil"))
# 
# sp.DIST = PRE_FATE.speciesDistance(mat.species.traits = TRAITS_SOIL
#                                    , mat.species.overlap = overlap
#                                    , min.info.thresh = 0.3)
# 
# sp.CLUST = PRE_FATE.speciesClustering_step1(mat.species.DIST = sp.DIST)
# 
# sp.DETERM = PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms
#                                              , no.clusters = c(2, 8, 6)
#                                              # , no.clusters = c(2, 7, 5)
#                                              , mat.species.DIST = sp.DIST)
# 
# selected.sp = sp.DETERM$determ.all
# selected.sp$sp = as.character(selected.sp$sp)
# selected.sp = merge(selected.sp, traits[, c("taxon.no", "spec_name")]
#                     , by.x = "sp", by.y = "taxon.no")
# fwrite(x = selected.sp, file = "determ.all.csv")
# 
# determinant_PFG_soil = sp.DETERM$determ.sp
# save(determinant_PFG_soil, file = "determinant_PFG_soil")

