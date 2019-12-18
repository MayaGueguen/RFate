## Load example data
data("PNE_PARAM")

## PNE_PARAM$succ_light : data.frame
## PNE_PARAM$strata_limits : vector
## PNE_PARAM$disp : data.frame
## PNE_PARAM$dist : data.frame
## PNE_PARAM$global : vector
## PNE_PARAM$masks : rasterStack


## Create a skeleton folder
PRE_FATE.skeletonDirectory(name.simulation = "FATE_PNE")

## Create PFG succession parameter files : predefined of strata limits
tab = PNE_PARAM$succ_light[, c("PFG", "type", "height", "maturity", "longevity")]
PRE_FATE.params_PFGsuccession(name.simulation = "FATE_PNE"
                            , mat.PFG.succ = tab
                            , strata.limits = PNE_PARAM$strata_limits
                            , strata.limits_reduce = FALSE)

## Create PFG light parameter files : predefined of strata limits
tab = PNE_PARAM$succ_light[, c("PFG", "type", "height", "maturity", "longevity", "light")]
PRE_FATE.params_PFGlight(name.simulation = "FATE_PNE"
                         , mat.PFG.succ = tab
                         , strata.limits = PNE_PARAM$strata_limits
                         , strata.limits_reduce = FALSE)

## Create PFG dispersal parameter files
PRE_FATE.params_PFGdispersal(name.simulation = "FATE_PNE"
                            , mat.PFG.disp = PNE_PARAM$disp)

## Create PFG disturbance parameter files
PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_PNE"
                               , mat.PFG.dist = PNE_PARAM$dist)

## Create a Global_parameters file
PRE_FATE.params_globalParameters(name.simulation = "FATE_PNE"
                                 , required.no_PFG = PNE_PARAM$global["NB_FG"]
                                 , required.no_STRATA = PNE_PARAM$global["NB_STRATUM"]
                                 , required.simul_duration = PNE_PARAM$global["SIMULATION_DURATION"]
                                 , required.seeding_duration = PNE_PARAM$global["SEEDING_DURATION"]
                                 , required.seeding_timestep = PNE_PARAM$global["SEEDING_TIMESTEP"]
                                 , required.seeding_input = PNE_PARAM$global["SEEDING_INPUT"]
                                 , required.max_by_cohort = PNE_PARAM$global["MAX_BY_COHORT"]
                                 , required.max_abund_low = PNE_PARAM$global["MAX_ABUND_LOW"]
                                 , required.max_abund_medium = PNE_PARAM$global["MAX_ABUND_MEDIUM"]
                                 , required.max_abund_high = PNE_PARAM$global["MAX_ABUND_HIGH"]
                                 , doLight = TRUE
                                 , LIGHT.thresh_medium = PNE_PARAM$global["LIGHT.thresh_medium"]
                                 , LIGHT.thresh_low = PNE_PARAM$global["LIGHT.thresh_low"]
                                 , doDispersal = TRUE
                                 , DISPERSAL.mode = PNE_PARAM$global["DISPERSAL.mode"]
                                 , doHabSuitability = TRUE
                                 , HABSUIT.ref_option = PNE_PARAM$global["HABSUIT.ref_option"]
                                 , doDisturbances = TRUE
                                 , DIST.no = PNE_PARAM$global["DIST.no"]
                                 , DIST.no_sub = PNE_PARAM$global["DIST.no_sub"]
                                 , DIST.freq = rep(PNE_PARAM$global["DIST.freq"]
                                                   , PNE_PARAM$global["DIST.no"])
                                 )

## Create simulation masks
library(raster)
writeRaster(PNE_PARAM$masks$maskEcrins, file = "FATE_PNE/DATA/MASK/mask.tif")
writeRaster(PNE_PARAM$masks$noDisturb, file = "FATE_PNE/DATA/MASK/noDisturb.tif")

## Create simulation parameters file
PRE_FATE.params_simulParameters(name.simulation = "FATE_PNE"
                                , name.mask = "mask.tif"
                                , name.dist = "noDisturb.tif")
