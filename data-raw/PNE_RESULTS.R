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
### PNE.RESULTS
###############################################################################

tab.6 = read.xlsx("PNE_PFG_description_delphine_VALIDATION.xls", sheetIndex = 1)

CC_BAU = stack(list.files("b_RESULTS/", pattern = "CC_BAU", full.names = TRUE))
CC_BAU = round(CC_BAU, 3)
CC_BAU = stack(CC_BAU)

CC_Abandon = stack(list.files("b_RESULTS/", pattern = "CC_Abandon", full.names = TRUE))
CC_Abandon = round(CC_Abandon, 3)
CC_Abandon = stack(CC_Abandon)

INIT = CC_BAU[[paste0("PNE_CC_BAU_year_", seq(50, 800, 50))]]
names(INIT) = paste0("PNE_INIT_year_", seq(50, 800, 50))

CC_BAU = CC_BAU[[paste0("PNE_CC_BAU_year_", seq(850, 1500, 50))]]

CC_Abandon = CC_Abandon[[paste0("PNE_CC_Abandon_year_", seq(850, 1500, 50))]]

INIT_800 = stack(list.files("b_RESULTS/", pattern = "_str", full.names = TRUE))
INIT_800 = INIT_800 * 1
INIT_800 = stack(INIT_800)


PNE_RESULTS = list(evaluation = as.data.frame(tab.6)
                   , abund_str.equilibrium = INIT_800
                   , forest_cover.init = INIT
                   , forest_cover.CC_BAU = CC_BAU
                   , forest_cover.CC_Abandon = CC_Abandon
)
save(PNE_RESULTS, file = "PNE_RESULTS.RData")

setwd("../..")
usethis::use_data(PNE_RESULTS, overwrite = TRUE)

