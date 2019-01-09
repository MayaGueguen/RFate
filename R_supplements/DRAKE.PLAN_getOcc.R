
rm(list = ls())

library(RPostgreSQL)
library(foreign)
library(data.table)
library(RFate)
library(drake)

source("RFate/R_supplements/DRAKE.PRE_FATE.data_getDB_occ.R")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getOccDominantSpecies.R")

path.data = "RFate/data/"
setwd(path.data)

################################################################################################################
### 1. GET DB VALUES - GATHER PER SPECIES
################################################################################################################

# clean()
PLAN.getOCC = drake_plan(
  zone.name = "Bauges"
  , zone.extent = c(928136, 966148, 6491251, 6537475)
  , create_wd = dir.create(path = zone.name)
  ## Get DB
  , OCC.DB = getDB(x.min = zone.extent[1]
                   , x.max = zone.extent[2]
                   , y.min = zone.extent[3]
                   , y.max = zone.extent[4])
  ## Get species
  , species = OCC.DB$species[, c("numtaxon", "genre", "libcbna")]
  ## Get sites informations
  , stations = OCC.DB$stations[, c("numchrono", "coderqualif", "longitudel93_rel", "latitudel93_rel")]
  , stations.COMMUNITY = stations$numchrono[which(stations$coderqualif %in% c("R06", "R07"))]
  , XY = getOcc_1_XY(stations = stations
                     , zone.name = zone.name)
  ## Get occurrences
  , observations = OCC.DB$observations[, c("numchrono", "numtaxon", "codecover")]
  , observations.xy = getOcc_1_obs(observations = observations
                                   , stations = stations)
  , sp.dom = getOcc_2_selectDom(observations.xy = observations.xy
                                , species = species
                                , zone.name = zone.name)
  , sp.dom.mat = getOcc_3_matDom(sp.SELECT = sp.dom
                                 , observations.xy = observations.xy
                                 , stations.COMMUNITY = stations.COMMUNITY
                                 , zone.name = zone.name)
  , sp.dom.occ = getOcc_3_occDom(mat.sites.species = sp.dom.mat
                                 , species = species
                                 , zone.name = zone.name)
  , strings_in_dots = "literals"
)

vis_drake_graph(drake_config(PLAN.getOCC)
                , targets_only = TRUE)
# outdated(drake_config(PLAN.getOCC))
make(PLAN.getOCC)
vis_drake_graph(drake_config(PLAN.getOCC)
                , targets_only = TRUE)


################################################################################################################
### 2. KEEP ONLY TRAITS OF INTEREST FOR FATE
################################################################################################################

# clean()
# PLAN.FATE = drake_plan(traits.quant = read.csv(file_in("TRAITS_quantitative_median_190107.csv"), stringsAsFactors = F, sep = "\t")
#                        , traits.quali = read.csv(file_in("TRAITS_qualitative_190107.csv"), stringsAsFactors = F, sep = "\t")
#                        , traits_names = get_traits_names()
#                        , TAB_traits = keep_traits_FATE(traits.quant, traits.quali, traits_names)
#                        , TAB_traits_FATE = reorganize_traits_FATE(TAB_traits)
#                        , TAB_traits_FATE.written = fwrite(TAB_traits_FATE, file = file_out("TRAITS_FATE_190107.csv"), sep = "\t")
#                        , strings_in_dots = "literals"
# )
# 
# vis_drake_graph(drake_config(PLAN.FATE)
#                 , targets_only = TRUE)
# make(PLAN.FATE)
# vis_drake_graph(drake_config(PLAN.FATE)
#                 , targets_only = TRUE)
# 
# loadd(TAB_traits)
# summary(TAB_traits)
# 
# loadd(TAB_traits_FATE)
# summary(TAB_traits_FATE)
