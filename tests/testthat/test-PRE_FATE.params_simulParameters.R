# library(RFate)
context("PRE_FATE.params_simulParameters() function")

## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with missing data", {
  expect_error(PRE_FATE.params_simulParameters()
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters(NA)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters(NULL)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})


## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_simulParameters(1)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters("a")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters(factor(1))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})


## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with wrong data : name.mask", {
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  PRE_FATE.skeletonDirectory()
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.mask` does not exist or is not inside the DATA/MASK/ folder")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = NULL)
               , "`name.mask` does not exist or is not inside the DATA/MASK/ folder")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = NA)
               , "`name.mask` does not exist or is not inside the DATA/MASK/ folder")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask")
               , "`name.mask` does not exist or is not inside the DATA/MASK/ folder")
})



## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with wrong data : name.simulation", {
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  system("mkdir FATE_simulation")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  system("mkdir FATE_simulation/PARAM_SIMUL")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/NAMESPACE_CONSTANTS/ folder")
  system("mkdir FATE_simulation/DATA")
  system("mkdir FATE_simulation/DATA/NAMESPACE_CONSTANTS")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  system("mkdir FATE_simulation/DATA/GLOBAL_PARAMETERS")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/SAVE/ folder")
  system("mkdir FATE_simulation/DATA/SAVE")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
  system("mkdir FATE_simulation/DATA/SCENARIO")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/MASK/ folder")
  system("mkdir FATE_simulation/DATA/MASK")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  system("mkdir FATE_simulation/DATA/PFGS")
  system("mkdir FATE_simulation/DATA/PFGS/SUCC")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  system("mkdir FATE_simulation/DATA/PFGS/DISP")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/ENVSUIT/ folder")
  system("mkdir FATE_simulation/DATA/PFGS/ENVSUIT")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a RESULTS/ folder")
  system("mkdir FATE_simulation/RESULTS")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.mask` does not exist or is not inside the DATA/MASK/ folder")
  
  system("cat /dev/null > FATE_simulation/DATA/MASK/mask.tif")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
               , "There is no adequate file (`.txt` file starting with `Namespace_constants`) into the DATA/NAMESPACE_CONSTANTS/ folder"
               , fixed = T)
  system("cat /dev/null > FATE_simulation/DATA/NAMESPACE_CONSTANTS/Namespace_constants.txt")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
               , "There is no adequate file (`.txt` file starting with `Global_parameters`) into the DATA/GLOBAL_PARAMETERS/ folder"
               , fixed = T)
  
  ## Create a Global_parameters file
  PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                   , required.succ_option = "fateh"
                                   , required.no_PFG = 6
                                   , required.no_STRATA = 5
                                   , required.hs_option = 1
                                   , required.seeding_timestep = 1
                                   , required.seeding_duration = c(10,50)
                                   , required.simul_duration = 100)
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
               , "There is not the same number of files (`.txt` file starting with `SUCC`) into the DATA/PFGS/SUCC/ folder as the number of PFG indicated into the file"
               , fixed = T)
  
  ## Create PFG succession parameter files
  PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                            , type = c("C", "C", "H", "H", "P", "P")
                                                            , height = c(10, 250, 36, 68, 1250, 550)
                                                            , maturity = c(5, 5, 3, 3, 8, 9)
                                                            , longevity = c(12, 200, 25, 4, 110, 70)
                                                            , dispersal = 1
                                                            , light = c(4, 6, 3, 6, 5, 5)))
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
               , "There is not the same number of files (`.txt` file starting with `DISP`) into the DATA/PFGS/DISP/ folder as the number of PFG indicated into the file"
               , fixed = T)
  
  ## Create PFG dispersal parameter files
  PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                               , mat.PFG.disp = data.frame(PFG = paste0("PFG", 1:6)
                                                           , d50 = rep(c(500, 500, 100),2)
                                                           , d99 = rep(c(10000, 15000, 20000),2)
                                                           , ldd = rep(c(100000, 50000, 100000),2)))
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
               , "There is no adequate file (`.txt` file starting with `SAVE_YEARS_maps`) into the folder FATE_simulation/DATA/SAVE"
               , fixed = T)
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
                 , "There is no adequate file (`.txt` file starting with `SAVE_YEARS_objects`) into the folder FATE_simulation/DATA/SAVE"
                 , fixed = T)
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
                 , "There is not the same number of files (`.txt` file starting with `DIST`) into the DATA/PFGS/DIST/ folder as the number of PFG indicated into the file"
                 , fixed = T)
  
  expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
                 , "The parameter file FATE_simulation/PARAM_SIMUL/Simul_parameters_V1.txt has been successfully created !")
  expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
                 , "The parameter file FATE_simulation/PARAM_SIMUL/Simul_parameters_V2.txt has been successfully created !")
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
                 , "`params.file` (FATE_simulation/PARAM_SIMUL/Simul_parameters_V1.txt) already exists. It will be replaced."
                 , fixed = T)
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
                 , "`params.file` (FATE_simulation/PARAM_SIMUL/Simul_parameters_V2.txt) already exists. It will be replaced."
                 , fixed = T)
  
  ## Create a SAVE_year_maps or/and SAVE_year_objects parameter file
  PRE_FATE.params_saveYears(name.simulation = "FATE_simulation"
                            , years.maps = c(100, 150, 200)
                            , years.objects = 200)
  system("cat /dev/null > FATE_simulation/DATA/SAVE/SAVE_YEARS_maps_BIS.txt")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
                 , "There is too many adequate files (`.txt` file starting with `SAVE_YEARS_maps`) into the folderFATE_simulation/DATA/SAVE"
               , fixed = T)
  system("rm FATE_simulation/DATA/SAVE/SAVE_YEARS_maps_BIS.txt")
  system("cat /dev/null > FATE_simulation/DATA/SAVE/SAVE_YEARS_objects_BIS.txt")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
               , "There is too many adequate files (`.txt` file starting with `SAVE_YEARS_objects`) into the folderFATE_simulation/DATA/SAVE"
               , fixed = T)
  system("rm FATE_simulation/DATA/SAVE/SAVE_YEARS_objects_BIS.txt")
  
  PRE_FATE.params_saveYears(name.simulation = "FATE_simulation"
                            , years.maps = c(100, 150, 200)
                            , years.objects = 200
                            , opt.folder.name = "Scen1")
  PRE_FATE.params_saveYears(name.simulation = "FATE_simulation"
                            , years.maps = c(100, 150, 200)
                            , years.objects = 200
                            , opt.folder.name = "Scen2")
  expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
                 , "The parameter file FATE_simulation/PARAM_SIMUL/Simul_parameters_V1.txt has been successfully created !")
  
  ## Create a Changing_times parameter file
  PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                , type.changing = "MASK"
                                , mat.changing = data.frame(year = c(50,50,80,80)
                                                            , order = c(1,2,1,2)
                                                            , file.name = c("MASK_50.tif"
                                                                            , "MASK_50.tif"
                                                                            , "MASK_80.tif"
                                                                            , "MASK_80.tif")))
  system("cat /dev/null > FATE_simulation/DATA/SCENARIO/MASK_changing_times_BIS.txt")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
                 , "There is too many adequate files (`.txt` file starting with `MASK_changing_times`) into the folderFATE_simulation/DATA/SCENARIO"
                 , fixed = T) 
  system("rm FATE_simulation/DATA/SCENARIO/MASK_changing_times_BIS.txt")
  
  PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                , type.changing = "HS"
                                , mat.changing = data.frame(year = c(50,50,80,80)
                                                            , order = c(1,2,1,2)
                                                            , file.name = c("MASK_50.tif"
                                                                            , "MASK_50.tif"
                                                                            , "MASK_80.tif"
                                                                            , "MASK_80.tif")))
  system("cat /dev/null > FATE_simulation/DATA/SCENARIO/HS_changing_times_BIS.txt")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
               , "There is too many adequate files (`.txt` file starting with `HS_changing_times`) into the folderFATE_simulation/DATA/SCENARIO"
               , fixed = T)
  system("rm FATE_simulation/DATA/SCENARIO/HS_changing_times_BIS.txt")
  
  PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                , type.changing = "DIST"
                                , mat.changing = data.frame(year = c(50,50,80,80)
                                                            , order = c(1,2,1,2)
                                                            , file.name = c("MASK_50.tif"
                                                                            , "MASK_50.tif"
                                                                            , "MASK_80.tif"
                                                                            , "MASK_80.tif")))
  system("cat /dev/null > FATE_simulation/DATA/SCENARIO/DIST_changing_times_BIS.txt")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
               , "There is too many adequate files (`.txt` file starting with `DIST_changing_times`) into the folderFATE_simulation/DATA/SCENARIO"
               , fixed = T)
  system("rm FATE_simulation/DATA/SCENARIO/DIST_changing_times_BIS.txt")
  
  system("rm FATE_simulation/DATA/SCENARIO/*")
  # system("mkdir FATE_simulation/DATA/SCENARIO/Scen1")
  # expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
  #              , "There is too many adequate files (`.txt` file starting with `DIST_changing_times`) into the folderFATE_simulation/DATA/SCENARIO"
  #              , fixed = T) 
  
  
  PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                , type.changing = "MASK"
                                , mat.changing = data.frame(year = c(50,50,80,80)
                                                            , order = c(1,2,1,2)
                                                            , file.name = c("MASK_50.tif"
                                                                            , "MASK_50.tif"
                                                                            , "MASK_80.tif"
                                                                            , "MASK_80.tif"))
                                , opt.folder.name = "Scen_MASK")
  expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
                 , "The parameter file FATE_simulation/PARAM_SIMUL/Simul_parameters_V1.txt has been successfully created !")
  PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                , type.changing = "HS"
                                , mat.changing = data.frame(year = c(50,50,80,80)
                                                            , order = c(1,2,1,2)
                                                            , file.name = c("MASK_50.tif"
                                                                            , "MASK_50.tif"
                                                                            , "MASK_80.tif"
                                                                            , "MASK_80.tif"))
                                , opt.folder.name = "Scen_HS")
  expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
                 , "The parameter file FATE_simulation/PARAM_SIMUL/Simul_parameters_V1.txt has been successfully created !")
  PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                , type.changing = "DIST"
                                , mat.changing = data.frame(year = c(50,50,80,80)
                                                            , order = c(1,2,1,2)
                                                            , file.name = c("MASK_50.tif"
                                                                            , "MASK_50.tif"
                                                                            , "MASK_80.tif"
                                                                            , "MASK_80.tif"))
                                , opt.folder.name = "Scen_DIST")
  expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask.tif")
                 , "The parameter file FATE_simulation/PARAM_SIMUL/Simul_parameters_V1.txt has been successfully created !")
  
})

