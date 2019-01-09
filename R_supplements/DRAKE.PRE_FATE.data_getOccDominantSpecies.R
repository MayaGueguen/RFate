

################################################################################################################
### 1. GET OCCURRENCES
################################################################################################################

getOcc_1_XY = function(stations, zone.name)
{
  stations.XY = stations[, c("numchrono", "longitudel93_rel", "latitudel93_rel")]
  stations.XY = unique(stations.XY)
  XY = data.frame(stations.XY[, c("longitudel93_rel", "latitudel93_rel")]
                  , row.names = paste0("NUMCHRONO-", stations.XY$numchrono))
  colnames(XY) = c("X_L93", "Y_L93")
  
  save(XY, file = paste0(zone.name, "/XY.RData"))
  
  return(XY)
}

getOcc_1_obs = function(observations, stations)
{
  observations$codecover = PRE_FATE.abundBraunBlanquet(observations$codecover)
  observations.xy = merge(observations, stations, by = "numchrono")
  head(observations.xy)
  
  return(observations.xy)
}


################################################################################################################
### 2. SELECT DOMINANT SPECIES
################################################################################################################

getOcc_2_selectDom = function(observations.xy, species, zone.name, selRule1, selRule2, selRule3)
{
  setwd(zone.name)
  
  occ = observations.xy[, c("numchrono", "numtaxon", "codecover", "longitudel93_rel", "latitudel93_rel")]
  occ = unique(occ) ## remove strata redundancy
  colnames(occ) = c("sites", "species", "abund", "X", "Y")
  
  ## SELECT DOMINANT SPECIES
  sp.SELECT = PRE_FATE.selectDominant(mat.site.species.abund = occ
                                      , selectionRule.quanti = selRule1
                                      , selectionRule.min_mean_abund = selRule2
                                      , selectionRule.min_no_abund_over25 = selRule3)
  
  ## Get species names
  sp.SELECT = merge(species, sp.SELECT, by.x = "numtaxon", by.y = "species", all.y = TRUE)
  
  setwd("./../")
  
  return(sp.SELECT)
}


################################################################################################################
### 3. GET DOMINANT SPECIES OBSERVATIONS
################################################################################################################

getOcc_3_matDom = function(sp.SELECT, observations.xy, stations.COMMUNITY, zone.name)
{
  ## Get dominant species observations
  sp.SELECT.occ = merge(sp.SELECT[, c("numtaxon", "genre", "libcbna")], observations.xy, by = "numtaxon")
  
  ## Transform into sites x species matrix
  mat.sites.species = tapply(X = sp.SELECT.occ$codecover
                             , INDEX = list(sp.SELECT.occ$numchrono, sp.SELECT.occ$numtaxon)
                             , FUN = length)
  ind.COMMUNITY = which(rownames(mat.sites.species) %in% stations.COMMUNITY)
  for(i in ind.COMMUNITY)
  {
    mat.sites.species[i, ] = ifelse(is.na(mat.sites.species[i, ]), 0, 1)
  }
  dim(mat.sites.species)
  mat.sites.species[1:10, 1:10]
  
  save(mat.sites.species, file = paste0(zone.name, "/mat.sites.species.DOM.RData"))
  
  return(mat.sites.species)
}

getOcc_3_occDom = function(mat.sites.species, species, zone.name)
{
  ## Create species occurrences files
  dir.create(paste0(zone.name, "/SP_OCC"))
  
  sp.suppr = vector()
  for(sp in colnames(mat.sites.species))
  {
    sp.occ = mat.sites.species[, sp]
    sp.occ = sp.occ[which(!is.na(sp.occ))]
    
    if (length(which(sp.occ == 0)) == 0 || length(which(sp.occ == 1)) < 20) 
    {
      sp.suppr = c(sp.suppr, sp)
    } else
    {
      save(sp.occ, file = paste0(zone.name, "/SP_OCC/OCC_X", sp))
    }
  }
  cat(" ==> No absence data for :", sp.suppr)
  dom_missing = species[which(species$numtaxon %in% sp.suppr), ]
  write.csv(dom_missing, file = paste0(zone.name, '/MISSING_dominant_species_observations.csv'), row.names = F)
}

################################################################################################################
### 4. BUILD SDM FOR DOMINANT SPECIES
################################################################################################################

