###############################################################################
##
## FATE EXAMPLE on Parc National des Ecrins (PNE)
##
###############################################################################

path.save = "data-raw/DATASET_PNE/"
setwd(path.save)

library(RFate)
library(data.table)
library(raster)
library(foreach)
library(xlsx)

###############################################################################
### REORGANIZATION OF DATA
###############################################################################

## Load observations of species abundances :
## numchrono = sites
## numtaxon = species ID
## codestrata = height stratum
## codeabonda = Braun-Blanquet value
load("PNE_OBS_sitexSPECIES_BB.RData")
head(PNE_OBS_sitexSPECIES_BB)

## Load information about habitat
## numchrono = sites
## delphCODE = habitat
##    0 : glacier, snow
##    31 : roc
##    40 : grasslands
##    50 : moors
##    60 : open lands, brush
##    70 : semi-closed lands
##    81 : closed lands
##    83 : forests
load("PNE_OBS_siteInfo.RData")
head(PNE_OBS_siteInfo)

## Reorganization of data :
## - Conversion of Braun-Blanquet value into "abundance" (between 0 and 100)
## - Get habitat information
tab = PNE_OBS_sitexSPECIES_BB[, c("numchrono", "numtaxon", "codeabonda")]
tab = merge(tab, PNE_OBS_siteInfo[, c("numchrono", "delphCODE", "X_LII", "Y_LII")], all.x = TRUE)
colnames(tab) = c("sites", "species", "abund_BB", "habitat", "X", "Y")
tab$abund = PRE_FATE.abundBraunBlanquet(tab$abund_BB)
tab = tab[, c("sites", "X", "Y", "habitat", "species", "abund_BB", "abund")]
head(tab)
fwrite(tab, file = "DATASET_PNE_observations.txt", row.names = FALSE, sep = "\t")


## Load traits of dominant species
## CODE_CBNA = species ID
## TYPE = H (herbaceous), C (chamaephytes), P (phanerophytes)
## PFG_ALL = PFG with all dominant species
## PFG_DETERM = PFG with only determinant species
traits = read.table("PNE_SPECIES_dominant_traits_PFG.txt", header = TRUE, sep = "\t")
head(traits)

## Reorganization of data :
tab = traits[, c("CODE_CBNA", "TYPE", "height", "dispersal", "palatability", "light")]
colnames(tab)[1:2] = c("species", "GROUP")
head(tab)
summary(tab)
fwrite(tab, file="DATASET_PNE_traits.txt", row.names = FALSE, sep="\t")

determ = traits[, c("CODE_CBNA", "SPECIES", "TYPE", "PFG_ALL", "PFG_DETERM")]
colnames(determ) = c("species", "name", "GROUP", "PFG", "determinant")
determ$determinant = ifelse(is.na(determ$determinant), FALSE, TRUE)
head(determ)
fwrite(determ, file = "DATASET_PNE_determ.txt", row.names = FALSE, sep = "\t")


###############################################################################
### PNE.PFG
###############################################################################

tab.1 = fread("DATASET_PNE_observations.txt")
tab.2 = fread("DATASET_PNE_traits.txt")
tab.3 = get(load("DATASET_PNE_SPECIES_distanceOverlap_dominant_noX.RData"))
tab.4 = fread("DATASET_PNE_determ.txt")
tab.5 = fread("DATASET_PNE_PFG_traits.txt")
tab.5 = tab.5[, 1:10]
tab.7 = get(load("PNE_OBS_sitexPFG_BB.RData"))
colnames(tab.7)[1:3] = c("sites", "X", "Y")

PNE_PFG = list(sp.observations = as.data.frame(tab.1)
                      , dom.traits = as.data.frame(tab.2)
                      , dom.dist_overlap = tab.3
                      # , dom.dist_total = ?? 
                      , dom.determ = as.data.frame(tab.4)
                      , nb.clusters = c("H" = 10, "C" = 6, "P" = 8)
                      # , hclust = ??
                      , PFG.traits = as.data.frame(tab.5)
                      , PFG.observations = as.data.frame(tab.7)
)
save(PNE_PFG, file = "PNE_PFG.RData")

setwd("../..")
usethis::use_data(PNE_PFG, overwrite = TRUE)
