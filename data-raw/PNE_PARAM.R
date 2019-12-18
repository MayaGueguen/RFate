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
### PNE.PARAM
###############################################################################

projRas = "+proj=lcc +lat_1=45.89891888888889 +lat_2=47.69601444444444 +lat_0=46.8 +lon_0=2.337229166666667 +x_0=600000 +y_0=2200000 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

masks = stack(list.files("a_DATA/MASK/", full.names = TRUE))
masks = masks * 1
projection(masks) = projRas
masks = stack(masks)
masks = masks[[c("maskEcrins", "noDisturb", "mowing", "grazing1", "grazing2", "grazing3", "grazingAll")]]

for(i in seq(0,90,15))
{
  cat(" ", i)
  HS = stack(list.files("a_DATA/PFGS/ENVSUIT_A1B/", pattern = paste0("HS_f", i), full.names = TRUE))
  HS = HS * 1
  HS = round(HS, 3)
  HS = stack(HS)
  projection(HS) = projRas
  assign(x = paste0("HS_", i), HS)
}

param.1 = readLines(con = "a_DATA/Global_parameters.txt")
param.g = c("NB_CPUS" = as.numeric(param.1[grep("NB_CPUS", param.1) + 1])
            , "NB_FG" = as.numeric(param.1[grep("NB_FG", param.1) + 1])
            , "NB_STRATUM" = as.numeric(param.1[grep("NB_STRATUM", param.1) + 1])
            , "SIMULATION_DURATION" = as.numeric(param.1[grep("SIMULATION_TIME", param.1) + 1])
            , "SEEDING_DURATION" = as.numeric(param.1[grep("SEEDING_DURATION", param.1) + 1])
            , "SEEDING_TIMESTEP" = as.numeric(param.1[grep("SEEDING_TIMESTEP", param.1) + 1])
            , "SEEDING_INPUT" = 100
            , "MAX_BY_COHORT" = 7000000
            , "MAX_ABUND_LOW" = 3000000
            , "MAX_ABUND_MEDIUM" = 7000000
            , "MAX_ABUND_HIGH" = 10000000
            , "LIGHT.thresh_medium" = 6000000
            , "LIGHT.thresh_low" = 9000000
            , "DISPERSAL.mode" = 1
            , "HABSUIT.ref_option" = 1
            , "DIST.no" = as.numeric(param.1[grep("NB_DISTURBANCES", param.1) + 1])
            , "DIST.no_sub" = as.numeric(param.1[grep("NB_SUBDISTURBANCES", param.1) + 1])
            , "DIST.freq" = 1
)
param.g = sapply(param.g, as.integer)

param.2 = foreach(i = list.files(path = "a_DATA/PFGS/DISP"
                                 , pattern = "^DISP_"
                                 , full.names = TRUE)
                  , .combine = "rbind") %do%
  {
    ff = readLines(con = i)
    return(data.frame(PFG = gsub("DISP_|.txt", "", basename(i))
                      , d50 = as.numeric(strsplit(ff, " ")[[1]][2])
                      , d99 = as.numeric(strsplit(ff, " ")[[1]][3])
                      , ldd = as.numeric(strsplit(ff, " ")[[1]][4])
                      , stringsAsFactors = FALSE
    ))
  }
param.3 = foreach(i = list.files(path = "a_DATA/PFGS/DIST"
                                 , pattern = "^DIST_"
                                 , full.names = TRUE)
                  , .combine = "rbind") %do%
  {
    ff = readLines(con = i)
    ff_FATES = ff[6]
    ff_FATES = as.numeric(strsplit(ff_FATES, " ")[[1]])
    res = data.frame(PFG = ff[1]
                     , name = rep(c("mowing", "grazing1", "grazing2"," grazing3")
                                  , each = 4)
                     , responseStage = rep(1:4, 4)
                     , KilledIndiv = ff_FATES[seq(1, length(ff_FATES), 2)]
                     , ResproutIndiv = ff_FATES[seq(2, length(ff_FATES), 2)]
                     , stringsAsFactors = FALSE
    )
  }
corres_dist = data.frame(old = 0:6, new = c(0, 1, 5, 9, 10, 4, 8))
param.3$KilledIndiv = sapply(param.3$KilledIndiv, function(x) corres_dist$new[which(corres_dist$old == x)])
param.3$ResproutIndiv = sapply(param.3$ResproutIndiv, function(x) corres_dist$new[which(corres_dist$old == x)])

tab.5 = fread("DATASET_PNE_PFG_traits.txt")
tab.5 = tab.5[, 1:10]
param.4 = tab.5[, c("PFG_name", "type", "height", "maturity", "longevity", "light")]
colnames(param.4)[1] = "PFG"

PNE_PARAM = list(masks = masks
                 , HS_0 = HS_0
                 , HS_15 = HS_15
                 , HS_30 = HS_30
                 , HS_45 = HS_45
                 , HS_60 = HS_60
                 , HS_75 = HS_75
                 , HS_90 = HS_90
                 , strata_limits = c(0, 150, 400, 1000, 2000)
                 , succ_light = param.4
                 , disp = param.2
                 , dist = param.3
                 , global = param.g
)
save(PNE_PARAM, file = "PNE_PARAM.RData")

setwd("../..")
usethis::use_data(PNE_PARAM, overwrite = TRUE)
