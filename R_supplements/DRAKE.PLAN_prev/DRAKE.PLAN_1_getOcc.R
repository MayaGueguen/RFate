
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

setwd("/home/gueguema/Documents/_TUTOS/3_R/_PACKAGES")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getDB_occ.R")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getOccDominantSpecies.R")

path.data = "RFate/data_supplements/"
setwd(path.data)


################################################################################################################

BAUGES = list(zone.name = "Bauges"
              , zone.extent = c(910795, 983695, 6489093, 6538793)
              , zone.selection.rules = c(0.95, 20, 20, 0.1, 10)
              , zone.mask = "Bauges/MASK_100m.tif"
              , zone.env.folder = "ENV_VARIABLES/EOBS_1970_2005/"
              , zone.env.variables = c("bio_1_0", "bio_8_0", "bio_12_0", "bio_19_0", "slope"))
ZONE = BAUGES
## ECRINS
## MONTBLANC
## LAUTARET

for(ZONE in list(BAUGES))
{
  
  ################################################################################################################
  ### 1. GET DB VALUES - SELECT DOMINANT SPECIES
  ################################################################################################################
  
  # clean()
  PLAN.getOCC = drake_plan(
    zone.name = ZONE$zone.name
    , zone.extent = ZONE$zone.extent
    , zone.selection.rules = ZONE$zone.selection.rules
    , zone.env.folder = ZONE$zone.env.folder
    , zone.env.variables = ZONE$zone.env.variables
    , zone.mask = raster(file_in(ZONE$zone.mask))
    ## Get environmental data
    , zone.env.stk = getSDM_env(zone.name = zone.name
                                , zone.env.folder = zone.env.folder
                                , zone.env.variables = zone.env.variables
                                , maskSimul = zone.mask)
    , zone.env.dem = raster(file_in(paste0(zone.name, "/", zone.env.folder, "/dem.tif")))
    ## Get DB
    , OCC.DB = getDB_CBNA(x.min = zone.extent[1]
                          , x.max = zone.extent[2]
                          , y.min = zone.extent[3]
                          , y.max = zone.extent[4])
    ## Get species
    , species = OCC.DB$species[, c("numtaxon", "genre", "libcbna")]
    , species.saved = save(species, file = paste0(zone.name, "/DB.species.RData"))
    ## Get sites informations
    , stations = OCC.DB$stations[, c("numchrono", "coderqualif", "longitudel93_rel", "latitudel93_rel")]
    , stations.COMMUNITY = stations$numchrono[which(stations$coderqualif %in% c("R06", "R07"))]
    , XY = getOcc_1_XY(stations = stations
                       , zone.name = zone.name)
    ## Get occurrences
    , observations = OCC.DB$observations[, c("numchrono", "numtaxon", "codecover")]
    , observations.xy = getOcc_1_obs(observations = observations
                                     , stations = stations
                                     , maskSimul = zone.mask
                                     , maskDem = zone.env.dem
                                     , zone.name = zone.name)
    ## Get dominant species informations
    , sp.dom = getOcc_2_selectDom(observations.xy = observations.xy
                                  , species = species
                                  , zone.name = zone.name
                                  , selRule1 = zone.selection.rules[1]
                                  , selRule2 = zone.selection.rules[2]
                                  , selRule3 = zone.selection.rules[3]
                                  , selRule4 = zone.selection.rules[4]
                                  , selRule5 = zone.selection.rules[5])
    , sp.dom.Marj = na.exclude(read.csv(file_in(paste0(zone.name, "/DOM.Marjorie_selection.csv"))
                             , fileEncoding = "windows-1252", header = TRUE, sep = "\t")[,1:2])
    , sp.dom.updated = merge(sp.dom, sp.dom.Marj, by = c("numtaxon", "libcbna"), all = TRUE)
    , name.file_DOM = paste0(zone.name, "/DOM_species_", zone.name, ".csv")
    # , sp.dom.updated.written = fwrite(sp.dom.updated, file = name.file_DOM, sep = "\t")
    , sp.dom.mat = getOcc_3_matDom(sp.SELECT = sp.dom.updated
                                   , observations.xy = observations.xy
                                   , stations.COMMUNITY = stations.COMMUNITY
                                   , zone.name = zone.name)
    , sp.dom.occ = getOcc_3_occDom(mat.sites.species = sp.dom.mat
                                   , species = species
                                   , zone.name = zone.name
                                   , sp.type = "SP")
    ## Build dominant species sdm
    , sp.dom.sdm = getSDM_build(zone.name = zone.name
                                , list_sp = sp.dom.occ
                                , XY = XY
                                , zone.env.stk.CALIB = zone.env.stk$env.CALIB
                                , zone.env.stk.PROJ = zone.env.stk$env.PROJ
                                , sp.type = "SP")
    , sp.dom.overlap = getSDM_overlap(zone.name = zone.name
                                      , list_sp = sp.dom.occ
                                      , maskSimul = zone.mask)
    , strings_in_dots = "literals"
  )
  
  vis_drake_graph(drake_config(PLAN.getOCC)
                  , targets_only = TRUE)
  # outdated(drake_config(PLAN.getOCC))
  make(PLAN.getOCC)
  vis_drake_graph(drake_config(PLAN.getOCC)
                  , targets_only = TRUE)
  
}

## Run missing species SDM
# loadd(zone.name)
# loadd(sp.dom.occ)
# sp.dom.occ = sp.dom.occ[-which(sp.dom.occ %in% list.files(paste0(zone.name, "/SP_SDM/")))]
# loadd(XY)
# loadd(zone.env.stk)
# sp.dom.sdm = getSDM_build(zone.name = zone.name
#                           , list_sp = sp.dom.occ
#                           , XY = XY
#                           , zone.env.stk.CALIB = zone.env.stk$env.CALIB
#                           , zone.env.stk.PROJ = zone.env.stk$env.PROJ
#                           , sp.type = "SP")
# 
# loadd(sp.dom.occ)
# loadd(zone.mask)
# sp.dom.overlap = getSDM_overlap(zone.name = zone.name
#                                 , list_sp = sp.dom.occ
#                                 , maskSimul = zone.mask)

