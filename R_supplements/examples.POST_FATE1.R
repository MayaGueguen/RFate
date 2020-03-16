## ----------------------------------------------------------------------------------------- ##
library(RFate)

## Load example data
PNE_PFG = .loadData("PNE_PFG")
PNE_PARAM = .loadData("PNE_PARAM")
PNE_RESULTS = .loadData("PNE_RESULTS")

## PNE_PFG$PFG.observations : data.frame
## PNE_PARAM$succ_light : data.frame
## PNE_PARAM$strata_limits : vector
## PNE_PARAM$disp : data.frame
## PNE_PARAM$dist : data.frame
## PNE_PARAM$global : vector
## PNE_PARAM$masks : rasterStack
## PNE_RESULTS$abund_str.equilibrium : rasterStack

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
writeRaster(PNE_PARAM$masks$maskEcrins
            , file = "FATE_PNE/DATA/MASK/mask.tif"
            , overwrite = TRUE)
writeRaster(PNE_PARAM$masks$noDisturb
            , file = "FATE_PNE/DATA/MASK/noDisturb.tif"
            , overwrite = TRUE)

## Create simulation parameters file
PRE_FATE.params_simulParameters(name.simulation = "FATE_PNE"
                                , name.mask = "mask.tif"
                                , name.dist = "noDisturb.tif")

## Create results folders
name.folder = "FATE_PNE"
name.simul = "SIMUL_V1"
dir1 = paste0(name.folder, "/RESULTS/", name.simul, "/ABUND_perPFG_allStrata")
dir2 = paste0(name.folder, "/RESULTS/", name.simul, "/ABUND_perPFG_perStrata")

dir.create(dir1, recursive = TRUE)
dir.create(dir2, recursive = TRUE)

## Create results files
PFG.names = PNE_PARAM$succ_light$PFG
PFG.short = sapply(PFG.names, function(x) strsplit(x, "_")[[1]][1])
for (pfg in PFG.names)
{
  ind = grep(pfg, names(PNE_RESULTS$abund_str.equilibrium))
  stk = PNE_RESULTS$abund_str.equilibrium[[ind]]
  writeRaster(stk
              , filename = paste0(dir2, "/Abund_YEAR_800_", pfg, "_STRATA_"
                                  , sub(".*str", "", names(stk)), ".tif")
              , overwrite = TRUE
              , bylayer = TRUE)
  ras = sum(stk)
  writeRaster(ras
              , filename = paste0(dir1, "/Abund_YEAR_800_", pfg, "_STRATA_all.tif")
              , overwrite = TRUE)
}

## Create relative abundance maps
POST_FATE.relativeAbund(name.simulation = "FATE_PNE"
                        , file.simulParam = "Simul_parameters_V1.txt"
                        , year = 800
                        , opt.no_CPU = 1)

## Create binary maps
library(reshape2)
tab = PNE_PFG$PFG.observations
tab = melt(tab, id.vars = c("sites", "X", "Y"))
colnames(tab) = c("sites", "X", "Y", "PFG", "obs")
tab = tab[, c("PFG", "X", "Y", "obs")]
tab = tab[which(tab$PFG != "Others"), ]
tab$PFG = sapply(tab$PFG, function(x) names(PFG.short)[which(PFG.short == x)])
tab$obs = ifelse(tab$obs > 0, 1, 0)
str(tab)

validStats = POST_FATE.graphic_validationStatistics(name.simulation = "FATE_PNE"
                                                    , file.simulParam = "Simul_parameters_V1.txt"
                                                    , year = 800
                                                    , mat.PFG.obs = tab
                                                    , opt.no_CPU = 1)

str(validStats$`FATE_PNE/PARAM_SIMUL/Simul_parameters_V1.txt`$tab$`800`)
plot(validStats$`FATE_PNE/PARAM_SIMUL/Simul_parameters_V1.txt`$plot$`800`$ALL)


# setwd("/home/gueguema/Documents/_FATE_SIMULATIONS/")
# POST_FATE.relativeAbund(name.simulation = "FATE_PNE_v233"
#                         , file.simulParam = "paramSimul_1.2CC_Abandon.txt"
#                         , year = 900
#                         , opt.no_CPU = 7)
# validStats2 = POST_FATE.graphic_validationStatistics(name.simulation = "FATE_PNE_v233"
#                                                     , file.simulParam = "paramSimul_1.2CC_Abandon.txt"
#                                                     , year = 900
#                                                     , mat.PFG.obs = tab
#                                                     , opt.no_CPU = 7)
# 
# tab1 = validStats1$`FATE_PNE_v233/PARAM_SIMUL/paramSimul_1.2CC_Abandon.txt`$tab$`900`
# tab1 = tab1[which(tab1$variable == "AUC"), c("PFG", "value")]
# colnames(tab1) = c("PFG", "TSS.1")
# tab2 = validStats2$`FATE_PNE_v233/PARAM_SIMUL/paramSimul_1.2CC_Abandon.txt`$tab$`900`
# tab2 = tab2[which(tab2$variable == "AUC"), c("PFG", "value")]
# colnames(tab2) = c("PFG", "TSS.2")
# tab.all = merge(tab1, tab2, by = "PFG")
# 
# ggplot(tab.all, aes(x = PFG)) +
#   geom_segment(aes(xend = PFG, y = TSS.1, yend = TSS.2)) +
#   geom_point(aes(y = TSS.1), color = "darkgreen") +
#   geom_point(aes(y = TSS.2), color = "brown") +
#   theme_fivethirtyeight() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ----------------------------------------------------------------------------------------- ##
## Comparison of TSS values obtained for :
##  - (1) Habitat Suitability maps (HS)
##  - (2) FATE simulation from Boulangeat et al 2014
##  - (3) FATE simulation from Boulangeat et al 2014 but with new observation points

## Get (3)
tab1 = validStats$`FATE_PNE/PARAM_SIMUL/Simul_parameters_V1.txt`$tab$`800`
tab1 = tab1[which(tab1$variable == "TSS"), c("PFG", "value")]
colnames(tab1) = c("PFG", "TSS.new")
tab1$PFG = sapply(tab1$PFG, function(x) strsplit(as.character(x), "_")[[1]][1])
tab1$TSS.new = round(tab1$TSS.new, 2)

## Get (1) and (2)
tab2 = PNE_RESULTS$evaluation[, c("PFG", "TSS.FATE", "TSS.HS")]

## Merge (1), (2) and (3)
tab.all = merge(tab1, tab2, by = "PFG")
tab.all$HSbetterFATE = (tab.all$TSS.HS > tab.all$TSS.FATE & tab.all$TSS.HS > tab.all$TSS.new)
tab.all$NewbetterFATE = (tab.all$TSS.new > tab.all$TSS.FATE)

library(ggplot2)
library(ggthemes) ## theme_fivethirtyeight
library(grid) ## arrow

ggplot(tab.all, aes(x = PFG)) +
  geom_segment(aes(xend = PFG
                   , y = -0.1
                   , yend = 0.65
                   , alpha = HSbetterFATE)
               , lwd = 5, color = "brown") +
  scale_alpha_discrete("HS > FATE", range = c(0, 0.3)) +
  geom_segment(aes(xend = PFG
                   , y = TSS.FATE
                   , yend = TSS.new + c(1, -1)[as.numeric(tab.all$TSS.FATE < tab.all$TSS.new) + 1] * 0.005
                   , lty = NewbetterFATE
  )
  , arrow = arrow(length = unit(0.1, "inches"), type = "closed")
  , lwd = 0.7
  # , color = "grey30"
  , alpha = 0.7) +
  scale_linetype_discrete("FATE_newOcc > FATE_Boulangeat") +
  geom_point(aes(y = TSS.HS, color = "1"), size = 3) +
  geom_point(aes(y = TSS.FATE, color = "2"), size = 2) +
  geom_point(aes(y = TSS.new, color = "3"), size = 2) +
  scale_color_manual(""
                     , values = c("3" = "darkgreen", "2" = "darkblue", "1" = "brown")
                     , labels = c("3" = "TSS.FATE_newOcc", "2" = "TSS.FATE_Boulangeat", "1" = "TSS.HS")) +
  labs(x = "", y = "", subtitle = "TSS") +
  theme_fivethirtyeight()

ggsave(filename = "/home/gueguema/Documents/_PAPERS/2015_01_FATE_Presentation/Version_2020/FATE_validation_examplePNE.pdf"
       , width = 12, height = 8)
