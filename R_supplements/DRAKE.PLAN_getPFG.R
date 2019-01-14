
rm(list = ls())

library(RPostgreSQL)
library(foreign)
library(data.table)
library(RFate)
library(biomod2)
library(raster)
library(parallel)
library(phyloclim)
library(drake)

source("RFate/R_supplements/DRAKE.PRE_FATE.data_getPFG.R")

path.data = "RFate/data_supplements/"
setwd(path.data)


################################################################################################################

BAUGES = list(zone.name = "Bauges"
              , zone.clusters = c(4,4,6,5))

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
    , mat.traits = fread("TRAITS_FATE_190111.csv")
    , mat.overlap = get(load(paste0(zone.name, "/mat.overlap.DOM.RData")))
    , species = get(load(paste0(zone.name, "/species.RData")))
    , mat.traits.select = select_traits(mat.traits = mat.traits)
    , sp.DIST.CLUST = calc_dist_clust(zone.name = zone.name
                                      , mat.traits.select = mat.traits.select
                                      , mat.overlap = mat.overlap)
    , selected.sp = calc_determ(zone.name = zone.name
                                , sp.DIST = sp.DIST.CLUST$sp.DIST
                                , sp.CLUST = sp.DIST.CLUST$sp.CLUST
                                , no.clusters = ZONE$zone.clusters
                                , species = species)
    , strings_in_dots = "literals"
  )
  
  vis_drake_graph(drake_config(PLAN.getPFG)
                  , targets_only = TRUE)
  # outdated(drake_config(PLAN.getOCC))
  make(PLAN.getPFG)
  vis_drake_graph(drake_config(PLAN.getPFG)
                  , targets_only = TRUE)
  
}
