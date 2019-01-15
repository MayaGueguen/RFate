
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
              , zone.env.folder = "ENV_VARIABLES/EOBS_1970_2005/"
              , zone.env.variables = c("bio_1_0", "bio_8_0", "bio_12_0", "bio_19_0", "slope"))

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
    , zone.env.folder = ZONE$zone.env.folder
    , zone.env.variables = ZONE$zone.env.variables
    , zone.mask = raster(file_in(ZONE$zone.mask))
    , zone.mask.name = ZONE$zone.mask
    ## Get data
    , mat.traits = fread(file_in("TRAITS_FATE_190111.csv"))
    , mat.overlap = get(load(paste0(zone.name, "/mat.overlap.DOM.RData")))
    , mat.sites.species = get(load(paste0(zone.name, "/mat.sites.species.DOM.RData")))
    , species = get(load(paste0(zone.name, "/species.RData")))
    , XY = get(load(paste0(zone.name, "/XY.RData")))
    ## Select traits
    , mat.traits.select = select_traits(mat.traits = mat.traits)
    ## Build PFG
    , sp.DIST.CLUST = calc_dist_clust(zone.name = zone.name
                                      , mat.traits.select = mat.traits.select
                                      , mat.overlap = mat.overlap)
    , selected.sp = calc_determ(zone.name = zone.name
                                , sp.DIST = sp.DIST.CLUST$sp.DIST
                                , sp.CLUST = sp.DIST.CLUST$sp.CLUST
                                , no.clusters = ZONE$zone.clusters
                                , species = species)
    ## Build PFG sdm
    , pfg.mat = get_sites_pfg(zone.name, mat.sites.species, selected.sp)
    , pfg.occ = getOcc_3_occDom(mat.sites.species = pfg.mat
                                , species = species
                                , zone.name = zone.name
                                , sp.type = "PFG")
    , zone.env.stk = SDM_getEnv(zone.name = zone.name
                                , zone.env.folder = zone.env.folder
                                , zone.env.variables = zone.env.variables
                                , maskSimul = zone.mask)
    , pfg.sdm = SDM_build(zone.name = zone.name
                          , list_sp = pfg.occ
                          , XY = XY
                          , zone.env.stk.CALIB = zone.env.stk$env.CALIB
                          , zone.env.stk.PROJ = zone.env.stk$env.PROJ
                          , sp.type = "PFG")
    ## Calculate PFG parameters
    , mat.traits.PFG = calc_pfg_meanTraits(mat.traits = mat.traits
                                           , selected.sp = selected.sp)
    , param.PFG = create_FATE_param(zone.name = zone.name
                                    , zone.mask = zone.mask.name
                                    , TRAITS_PFG = mat.traits.PFG)
    , strings_in_dots = "literals"
  )
  
  vis_drake_graph(drake_config(PLAN.getPFG)
                  , targets_only = TRUE)
  # outdated(drake_config(PLAN.getOCC))
  make(PLAN.getPFG)
  vis_drake_graph(drake_config(PLAN.getPFG)
                  , targets_only = TRUE)
  
}



