
rm(list = ls())

library(RPostgreSQL)
library(data.table)
library(foreach)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(doParallel)
library(drake)

registerDoParallel(cores = 7)

source("RFate/R_supplements/DRAKE.PRE_FATE.data_getDB.R")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getTraitsPerSpecies.R")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getTraitsFATErelated.R")

path.data = "RFate/data/"
setwd(path.data)

################################################################################################################
### 1. GET DB VALUES - GATHER PER SPECIES
################################################################################################################

# clean()
PLAN.DB = drake_plan(traits = getDB()
                  , data_1 = traits_merge(traits)
                  , data_2 = traits_removeUninformative(data_1)
                  , data_3 = traits_remove(data_2)
                  , data_4 = traits_change(data_3)
                  , data.split = data_split(data_4)
                  # , traits.genre = traits_genre(data_4)
                  , traits.quant = data.split[[1]]
                  , traits.quali = data.split[[2]]
                  , traits.quant.med = get_traits_quant_median(traits.quant)
                  # , traits.quant.med_suppr = traits_thresholdGenus(traits.quant.med, traits.genre)
                  , traits.quali.med = get_traits_quali_merged(traits.quali)
                  # , traits.quali.med_suppr = traits_thresholdGenus(traits.quali.med, traits.genre)
                  , save.quant = traits_save(traits.quant.med, file_out("TRAITS_quantitative_median_190107.csv"))
                  , save.quali = traits_save(traits.quali.med, file_out("TRAITS_qualitative_190107.csv"))
                  , graph.quant = graph_barplot(traits.quant.med, file_out("GRAPH1_numberValuesPerTrait_quanti.pdf"))
                  , graph.quali = graph_barplot(traits.quali.med, file_out("GRAPH2_numberValuesPerTrait_quali.pdf"))
                  , strings_in_dots = "literals"
)
# print(PLAN.DB)

vis_drake_graph(drake_config(PLAN.DB)
                , targets_only = TRUE)
make(PLAN.DB)
vis_drake_graph(drake_config(PLAN.DB)
                , targets_only = TRUE)

loadd(traits.quant.med)
loadd(traits.quali.med)

################################################################################################################
### 2. KEEP ONLY TRAITS OF INTEREST FOR FATE
################################################################################################################

# clean()
PLAN.FATE = drake_plan(traits.quant = read.csv(file_in("TRAITS_quantitative_median_190107.csv"), stringsAsFactors = F, sep = "\t")
                     , traits.quali = read.csv(file_in("TRAITS_qualitative_190107.csv"), stringsAsFactors = F, sep = "\t")
                     , traits_names = get_traits_names()
                     , TAB_traits = keep_traits_FATE(traits.quant, traits.quali, traits_names)
                     , TAB_traits_FATE = reorganize_traits_FATE(TAB_traits)
                     , TAB_traits_FATE.written = fwrite(TAB_traits_FATE, file = file_out("TRAITS_FATE_190107.csv"), sep = "\t")
                     , strings_in_dots = "literals"
)

vis_drake_graph(drake_config(PLAN.FATE)
                , targets_only = TRUE)
make(PLAN.FATE)
vis_drake_graph(drake_config(PLAN.FATE)
                , targets_only = TRUE)

loadd(TAB_traits)
summary(TAB_traits)

loadd(TAB_traits_FATE)
summary(TAB_traits_FATE)
