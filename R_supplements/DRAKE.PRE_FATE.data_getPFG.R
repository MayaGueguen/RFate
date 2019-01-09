

################################################################################################################
## GET TRAITS
################################################################################################################


function(zone.name, mat.traits, mat.overlap)
{
  mat.traits$species = paste0("X", mat.traits$CODE_CBNA)
  mat.traits$GROUP = mat.traits$WOODY
  
  ## Check correspondance
  # length(setdiff(colnames(mat.overlap), mat.traits$species))
  # length(setdiff(mat.traits$species, colnames(mat.overlap)))
  
  
  ## Fix ordered factors
  mat.traits$DISPERSAL = ordered(factor(mat.traits$DISPERSAL))
  mat.traits$LIGHT = ordered(factor(mat.traits$LIGHT))
  mat.traits$palatability = ifelse(is.na(mat.traits$palatability), NA, paste0("pal", mat.traits$palatability))
  mat.traits$palatability = ordered(factor(mat.traits$palatability))
  
  ## Take the root square of height:
  mat.traits$HEIGHT = as.numeric(sqrt(as.numeric(mat.traits$HEIGHT)))
  mat.traits$DISPERSAL = exp(as.numeric(as.character(mat.traits$DISPERSAL))) # Disp is not an ordered factor anymore!
  
  return(mat.traits)
}


################################################################################################################
## DO CLUSTERING : WITHOUT SOIL
################################################################################################################

function(zone.name, mat.traits, mat.overlap)
{
  dir.create(paste0(zone.name, "/PFG_withoutSoil"))
  setwd(paste0(zone.name, "/PFG_withoutSoil"))
  
  sp.DIST = PRE_FATE.speciesDistance(mat.species.traits = mat.traits
                                     , mat.species.overlap = mat.overlap
                                     , min.info.thresh = 0.3)
  
  sp.CLUST = PRE_FATE.speciesClustering_step1(mat.species.DIST = sp.DIST)
  
  return(list(sp.DIST = sp.DIST, sp.CLUST = sp.CLUST))
}


function(zone.name, sp.DIST, sp.CLUST, no.clusters, mat.traits)
{
  
  sp.DETERM = PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms
                                               , no.clusters = no.clusters
                                               # , no.clusters = c(2, 8, 6)
                                               # , no.clusters = c(2, 7, 5)
                                               , mat.species.DIST = sp.DIST)
  
  selected.sp = sp.DETERM$determ.all
  selected.sp$sp = as.character(selected.sp$sp)
  selected.sp = merge(selected.sp, mat.traits[, c("taxon.no", "spec_name")]
                      , by.x = "sp", by.y = "taxon.no")
  fwrite(x = selected.sp, file = "determ.all.csv")
}

################################################################################################################
## DO CLUSTERING : WITH SOIL
################################################################################################################

load(paste0(path_save, "TAB_soil_SP"))
TAB_soil_SP$SP = paste0("X", TAB_soil_SP$SP)
TAB_soil_SP = TAB_soil_SP[which(TAB_soil_SP$TRAIT == "ELLNIT"),]
TAB_soil_SP = TAB_soil_SP[, c("SP", "MEAN")]
colnames(TAB_soil_SP) = c("species", "ELLNIT")
head(TAB_soil_SP)

TRAITS_SOIL = merge(TRAITS, TAB_soil_SP, by = "species")
TRAITS_SOIL$ELLNIT = floor(TRAITS_SOIL$ELLNIT)
TRAITS_SOIL$ELLNIT = ordered(factor(TRAITS_SOIL$ELLNIT))
summary(TRAITS_SOIL)

save(TRAITS_SOIL, file = paste0(path_save, "TRAITS_SOIL"))

dir.create(paste0(path_save, "PFG_withSoil"))
setwd(paste0(path_save, "PFG_withSoil"))

sp.DIST = PRE_FATE.speciesDistance(mat.species.traits = TRAITS_SOIL
                                   , mat.species.overlap = overlap
                                   , min.info.thresh = 0.3)

sp.CLUST = PRE_FATE.speciesClustering_step1(mat.species.DIST = sp.DIST)

sp.DETERM = PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms
                                             , no.clusters = c(2, 8, 6)
                                             # , no.clusters = c(2, 7, 5)
                                             , mat.species.DIST = sp.DIST)

selected.sp = sp.DETERM$determ.all
selected.sp$sp = as.character(selected.sp$sp)
selected.sp = merge(selected.sp, traits[, c("taxon.no", "spec_name")]
                    , by.x = "sp", by.y = "taxon.no")
fwrite(x = selected.sp, file = "determ.all.csv")

determinant_PFG_soil = sp.DETERM$determ.sp
save(determinant_PFG_soil, file = "determinant_PFG_soil")

