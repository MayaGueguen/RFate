# library(RFate)

## Create a skeleton folder with the default name ('FATE_simulation')
if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
PRE_FATE.skeletonDirectory()

## Create a Namespace_constants parameter file
PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                   , global.abund.low = 1000000
                                   , global.abund.med = 5000000
                                   , global.abund.high = 8000000
                                   , global.max.by.cohort = 5000000
                                   , global.resource.thresh.med = 13000000
                                   , global.resource.thresh.low = 19000000)

## Create a Global_parameters file
PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                 , required.succ_option = "fateh"
                                 , required.no_PFG = 6
                                 , required.no_STRATA = 5
                                 , required.hs_option = 1
                                 , required.seeding_timestep = 1
                                 , required.seeding_duration = 10
                                 , required.simul_duration = 100)

## Create a Changing_times parameter file
PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                              , type.changing = "DIST"
                              , mat.changing = data.frame(year = c(50,50,80,80)
                                                          , order = c(1,2,1,2)
                                                          , file.name = c("MASK_DIST1_50.tif"
                                                                          , "MASK_DIST2_50.tif"
                                                                          , "MASK_DIST1_80.tif"
                                                                          , "MASK_DIST2_80.tif")))

## Create PFG succession parameter files
PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                              , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                          , type = c("C", "C", "H", "H", "P", "P")
                                                          , height = c(10, 250, 36, 68, 1250, 550)
                                                          , maturity = c(5, 5, 3, 3, 8, 9)
                                                          , longevity = c(12, 200, 25, 4, 110, 70)
                                                          , dispersal = 1
                                                          , light = c(4, 6, 3, 6, 5, 5)))

## Create PFG dispersal parameter files
PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                             , mat.PFG.disp = data.frame(PFG = paste0("PFG", 1:6)
                                                         , d50 = rep(c(500, 500, 100),2)
                                                         , d99 = rep(c(10000, 15000, 20000),2)
                                                         , ldd = rep(c(100000, 50000, 100000),2)))

## Create a Simulation parameter file
PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
