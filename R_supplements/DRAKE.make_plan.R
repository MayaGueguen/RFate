
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

path.data = "RFate/data/"
setwd(path.data)

# clean()
PLAN = drake_plan(traits = getDB()
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
                  , save.quant = traits_save(traits.quant.med, file_out("TRAITS_quantitative_median_181220.csv"))
                  , save.quali = traits_save(traits.quali.med, file_out("TRAITS_qualitative_181220.csv"))
                  , graph.quant = graph_barplot(traits.quant.med, file_out("GRAPH1_numberValuesPerTrait_quanti.pdf"))
                  , graph.quali = graph_barplot(traits.quali.med, file_out("GRAPH2_numberValuesPerTrait_quali.pdf"))
                  , strings_in_dots = "literals"
)
# print(PLAN)

vis_drake_graph(drake_config(PLAN)
                , targets_only = TRUE)
make(PLAN)
vis_drake_graph(drake_config(PLAN)
                , targets_only = TRUE)

loadd(traits.quant.med)
loadd(traits.quali.med)

