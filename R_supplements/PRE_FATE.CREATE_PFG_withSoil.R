
rm(list = ls())

library(foreign)
library(data.table)
library(RFate)


source("RFate/R_supplements/DRAKE.PRE_FATE.data_getDB_occ.R")

zone.name = "Bauges"
zone.extent = c(928136, 966148, 6491251, 6537475)


path.save = paste0("RFate/data/", zone.name)
dir.create(path = path.save)
setwd(path.save)


################################################################################################################
### 1. GET OCCURRENCES
################################################################################################################

OCC.DB = getDB(x.min = zone.extent[1]
               , x.max = zone.extent[2]
               , y.min = zone.extent[3]
               , y.max = zone.extent[4])

## Get species
species = OCC.DB$species[, c("numtaxon", "genre", "libcbna")]

## Get sites informations
stations = OCC.DB$stations[, c("numchrono", "coderqualif", "longitudel93_rel", "latitudel93_rel")]
stations.COMMUNITY = stations$numchrono[which(stations$coderqualif %in% c("R06", "R07"))]
stations.XY = stations[, c("numchrono", "longitudel93_rel", "latitudel93_rel")]
stations.XY = unique(stations.XY)
XY = data.frame(stations.XY[, c("longitudel93_rel", "latitudel93_rel")]
                , row.names = paste0("NUMCHRONO-", stations.XY$numchrono))
colnames(XY) = c("X_L93", "Y_L93")
save(XY, file = "XY.RData")

## Get occurrences
observations = OCC.DB$observations[, c("numchrono", "numtaxon", "codecover")]
observations$codecover = PRE_FATE.abundBraunBlanquet(observations$codecover)
observations.xy = merge(observations, stations, by = "numchrono")
head(observations.xy)


################################################################################################################
### 2. SELECT DOMINANT SPECIES
################################################################################################################

occ = observations.xy[, c("numchrono", "numtaxon", "codecover", "longitudel93_rel", "latitudel93_rel")]
occ = unique(occ) ## remove strata redundancy
colnames(occ) = c("sites", "species", "abund", "X", "Y")

## SELECT DOMINANT SPECIES
sp.SELECT = PRE_FATE.selectDominant(mat.site.species.abund = occ
                                    , selectionRule.quanti = 0.8
                                    , selectionRule.min_mean_abund = 10
                                    , selectionRule.min_no_abund_over25 = 20)

## Get species names
sp.SELECT = merge(species, sp.SELECT, by.x = "numtaxon", by.y = "species", all.y = TRUE)


################################################################################################################
### 3. GET DOMINANT SPECIES OBSERVATIONS
################################################################################################################

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

save(mat.sites.species, file = "mat.sites.species.RData")

## Create species occurrences files
dir.create("SP_OCC")

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
    save(sp.occ, file = paste0("SP_OCC/OCC_X", sp))
  }
}
cat(" ==> No absence data for :", sp.suppr)
dom_missing = species[which(species$numtaxon %in% sp.suppr), ]
write.csv(dom_missing, file = 'MISSING_dominant_species_observations.csv', row.names = F)


################################################################################################################
### 4. BUILD SDM FOR DOMINANT SPECIES
################################################################################################################

################################################################################################################
### 1. GET OCCURRENCES - SELECT DOMINANT
################################################################################################################

################################################################################################################
## GET TRAITS
################################################################################################################

## TRAITS CHOICE ------------------------------------------------------------------------- #
# traits = get(load(file = paste0(path_save, "dominant_list_final")))
# traits$taxon.no = as.character(traits$taxon.no)
# tail(traits)

traits = get(load(file = paste0(path_save, "TAB_traits_COMPLETE")))
head(traits)

traits = traits[, c("DISP_VITTOZ", "")]

## Fix ordered factors
traits$dispersal = ordered(factor(traits$dispersal))
traits$ellen_light = ordered(factor(traits$ellen_light))
traits$palatability = ifelse(is.na(traits$palatability), NA, paste0("pal", traits$palatability))
traits$palatability = ordered(factor(traits$palatability))
traits$cham_pal = ordered(factor(traits$cham_pal))

## Take the root square of height:
traits$height = as.numeric(sqrt(as.numeric(traits$height)))
traits$dispersal = exp(as.numeric(as.character(traits$dispersal))) # Disp is not an ordered factor anymore!
traits$median_elev = as.numeric(traits$median_elev)

## LOAD OVERLAP ENVIRONMENTAL DATA ------------------------------------------------------- #
overlap = get(load(file = paste0(path_save, "overlap-matrix")))

## Check correspondance
length(setdiff(colnames(overlap), traits$taxon.no)) ## 0 species in overlap are missingGrpes in traits
length(setdiff(traits$taxon.no, colnames(overlap))) ## 9 species in traits are missingGrpes in overlap

## TRAITS CHOICE ------------------------------------------------------------------------- #
## For each plant life_form, get dispersal, ellen_light, height, palatability, median_elev and cham_pal traits 
## For each plant type, get the species code

Herbaceous = c(
  "Geophyte",
  "Hemicryptophyte",
  "Hydrogeophyte",
  "Helophyte",
  "Hemicryptophyte/Geophyte",
  "Therophyte",
  "Geophyte/Hemicryptophyte",
  "Hemicryptophyte/Helophyte",
  "Helophyte/Hemicryptophyte",
  "hemicryptophyte (long-lived)",
  "Hemicryptophyte/Geophyte/Hemicryptophyte",
  "hemicryptophyte (long-lived)/hemicryptophyte (long-lived)",
  "Therophyte/Hemicryptophyte",
  "Hemicryptophyte/Hemicryptophyte",
  "geophyte/hemicryptophyte (long-lived)",
  "herbaceous chamaephyte/hemicryptophyte (long-lived)",
  "Hemicryptophyte/Therophyte",
  "Hemicryptophyte/Chamaephyte"
)

Phanerophyte = c("Phanerophyte", "phanerophyte")

Chamaephyte = c(
  "Chamaephyte",
  "Chamaephyte/Chamaephyte",
  "woody chamaephyte",
  "Phanerophyte/Chamaephyte",
  "Chamaephyte/Phanerophyte"
)

pft = character(length(traits$taxon.no))
for (i in 1:length(traits$taxon.no)) {
  if (traits$life_form[i] %in% Herbaceous) {
    pft[i] = "H"
  } else {
    if (traits$life_form[i] %in% Phanerophyte) {
      pft[i] = "P"
    } else {
      if (traits$life_form[i] %in% Chamaephyte) {
        pft[i] = "C"
      } else{
        if (length(grep("Alnus alnobetula", traits$spec_name[i])) > 0) {
          pft[i] = "P"
        } else {
          if (length(grep("Rubus fruticosus", traits$spec_name[i])) > 0) {
            pft[i] = "C"
          } else{
            pft[i] = "H"
          }
        }
      }
    }
  }
}

pft[grep("Sorbus chamaemespilus", traits$spec_name)] = "P"
pft[grep("Sempervivum tectorum", traits$spec_name)] = "H"
pft[grep("Juniperus communis", traits$spec_name)] = "P"
pft[grep("Corylus avellana", traits$spec_name)] = "P"
pft[grep("Teucrium chamaedrys", traits$spec_name)] = "H"
traits$GROUP = pft
traits$species = traits$taxon.no

TRAITS = traits[, c("species"
                    , "GROUP"
                    , "dispersal"
                    , "ellen_light"
                    , "height"
                    , "palatability"
                    , "median_elev"
                    # , "cham_pal"
                    # , "maturity"
)]
summary(TRAITS)

################################################################################################################
## DO CLUSTERING : WITHOUT SOIL
################################################################################################################

dir.create(paste0(path_save, "PFG_withoutSoil"))
setwd(paste0(path_save, "PFG_withoutSoil"))

sp.DIST = PRE_FATE.speciesDistance(mat.species.traits = TRAITS
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

