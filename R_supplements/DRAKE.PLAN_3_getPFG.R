
rm(list = ls())

library(RPostgreSQL)
library(foreign)
library(data.table)
library(foreach)
library(RFate)
library(biomod2)
library(raster)
library(parallel)
library(phyloclim)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(drake)

setwd("/home/gueguen/Documents/_TUTOS/3_R/_PACKAGES")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getPFG.R")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getOccDominantSpecies.R")

path.data = "RFate/data_supplements/"
setwd(path.data)


################################################################################################################

BAUGES = list(zone.name = "Bauges"
              , zone.clusters = c(4, 4, 6, 5)
              , zone.mask = "Bauges/MASK_100m.tif"
              , zone.mask.pert.all = c("Bauges/MASK_grazing.tif", "Bauges/MASK_noPerturb.tif")
              , zone.mask.pert.def = "Bauges/MASK_noPerturb.tif"
              , zone.env.folder = "ENV_VARIABLES/EOBS_1970_2005/"
              , zone.env.variables = c("bio_1_0", "bio_8_0", "bio_12_0", "bio_19_0", "slope"))
ZONE = BAUGES

file_date = "190221"

## ECRINS
## MONTBLANC
## LAUTARET

for(ZONE in list(BAUGES))
{
  
  ################################################################################################################
  ### 1. GET DB VALUES - SELECT DOMINANT SPECIES
  ################################################################################################################
  
  # clean()
  PLAN.getPFG = drake_plan(
    zone.name = ZONE$zone.name
    , zone.mask = raster(file_in(ZONE$zone.mask))
    ## Get data
    , mat.traits = fread(file_in(paste0("TRAITS_FATE_", file_date, ".csv")))
    , mat.overlap = get(load(paste0(zone.name, "/DOM.mat.overlap.RData")))
    , mat.sites.species = get(load(paste0(zone.name, "/DOM.mat.sites.species.RData")))
    , species = get(load(paste0(zone.name, "/DB.species.RData")))
    , XY = get(load(paste0(zone.name, "/DB.XY.RData")))
    ## Select traits
    , mat.traits.select = getPFG_1_selectTraits(mat.traits = mat.traits)
    ## Build PFG
    , sp.DIST.CLUST = getPFG_2_calcDistClust(zone.name = zone.name
                                             , mat.traits.select = mat.traits.select
                                             , mat.overlap = mat.overlap)
    , selected.sp.all = getPFG_2_calcDeterm(zone.name = zone.name
                                        , sp.DIST = sp.DIST.CLUST$sp.DIST
                                        , sp.CLUST = sp.DIST.CLUST$sp.CLUST
                                        , no.clusters = ZONE$zone.clusters
                                        , species = species)
    , selected.sp = getPFG_2_keepDeterm(zone.name = zone.name
                                        , selected.sp = selected.sp.all)
    ## Build PFG sdm
    , pfg.mat = getPFG_3_matSitesPFG(zone.name = zone.name
                                     , mat.sites.species = mat.sites.species
                                     , selected.sp = selected.sp)
    , pfg.occ = getOcc_3_occDom(mat.sites.species = pfg.mat
                                , species = species
                                , zone.name = zone.name
                                , sp.type = "PFG")
    , zone.env.stk = getSDM_env(zone.name = zone.name
                                , zone.env.folder = ZONE$zone.env.folder
                                , zone.env.variables = ZONE$zone.env.variables
                                , maskSimul = zone.mask)
    , pfg.sdm = getSDM_build(zone.name = zone.name
                             , list_sp = pfg.occ
                             , XY = XY
                             , zone.env.stk.CALIB = zone.env.stk$env.CALIB
                             , zone.env.stk.PROJ = zone.env.stk$env.PROJ
                             , sp.type = "PFG")
    ## Calculate PFG parameters
    , mat.traits.pfg = getPFG_4_calcMeanTraits(zone.name = zone.name
                                               , mat.traits = mat.traits
                                               , selected.sp = selected.sp)
    , param.PFG = getPFG_5_FATEparam(zone.name = zone.name
                                     , zone.mask = ZONE$zone.mask
                                     , zone.mask.pert.all = ZONE$zone.mask.pert.all
                                     , zone.mask.pert.def = ZONE$zone.mask.pert.def
                                     , TRAITS_PFG = mat.traits.pfg
                                     , pfg.sdm = pfg.sdm)
    , strings_in_dots = "literals"
  )
  
  vis_drake_graph(drake_config(PLAN.getPFG)
                  , targets_only = TRUE)
  # outdated(drake_config(PLAN.getOCC))
  make(PLAN.getPFG)
  vis_drake_graph(drake_config(PLAN.getPFG)
                  , targets_only = TRUE)
  
}

# zone.name = ZONE$zone.name
# loadd(pfg.occ)
# XY = get(load(paste0(zone.name, "/DB.XY.RData")))
# loadd(zone.env.stk)
# pfg.sdm = getSDM_build(zone.name = zone.name
#                        , list_sp = pfg.occ
#                        , XY = XY
#                        , zone.env.stk.CALIB = zone.env.stk$env.CALIB
#                        , zone.env.stk.PROJ = zone.env.stk$env.PROJ
#                        , sp.type = "PFG")

# zone.name = ZONE$zone.name
# loadd(mat.traits.pfg)
# param.PFG = getPFG_5_FATEparam(zone.name = zone.name
#                                , zone.mask = ZONE$zone.mask
#                                , zone.mask.pert = ZONE$zone.mask.pert
#                                , TRAITS_PFG = mat.traits.pfg)


