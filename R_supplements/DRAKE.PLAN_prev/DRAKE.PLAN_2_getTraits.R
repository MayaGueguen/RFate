
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

setwd("/home/gueguema/Documents/_TUTOS/3_R/_PACKAGES")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getDB_traits.R")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getTraitsPerSpecies.R")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getTraitsFATErelated.R")

path.data = "RFate/data_supplements/"
setwd(path.data)

file_date = "190307"
file_date = "190510"

################################################################################################################
### 1. GET DB VALUES - GATHER PER SPECIES
################################################################################################################

# clean()
PLAN.DB = drake_plan(traits = getDB_ANDROSACE()
                     , data_1 = getTraits_1_merge.species(traits = traits)
                     , data_2 = getTraits_1_merge.traits(traits = data_1)
                     , data_3 = getTraits_1_removeUninformative(traits = data_2)
                     , data_4 = getTraits_1_remove(traits = data_3)
                     , data_5 = getTraits_1_change(traits = data_4)
                     , data.split = getTraits_2_split(traits = data_5)
                     # , traits.genre = traits_genre(data_4)
                     , traits.quant = data.split[[1]]
                     , traits.quali = data.split[[2]]
                     , traits.quant.med = getTraits_3_quantMedian(traits_quant = traits.quant)
                     , traits.quali.med = getTraits_3_qualiMerged(traits_quali = traits.quali)
                     # , traits.quant.med_suppr = getTraits_3_thresholdGenus(traits.quant.med, traits.genre)
                     # , traits.quali.med_suppr = getTraits_3_thresholdGenus(traits.quali.med, traits.genre)
                     , name.file_quant = paste0("TRAITS_quantitative_median_", file_date, ".csv")
                     , name.file_quali = paste0("TRAITS_qualitative_", file_date, ".csv")
                     , save.quant = getTraits_4_save(traits = traits.quant.med
                                                     , namefile = name.file_quant)
                     , save.quali = getTraits_4_save(traits = traits.quali.med
                                                     , namefile = name.file_quali)
                     , graph.quant = getTraits_4_graphBarplot(traits = traits.quant.med
                                                              , namefile = file_out("GRAPH1_numberValuesPerTrait_quanti.pdf"))
                     , graph.quali = getTraits_4_graphBarplot(traits = traits.quali.med
                                                              , namefile = file_out("GRAPH2_numberValuesPerTrait_quali.pdf"))
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
PLAN.FATE = drake_plan(name.file_quant = paste0("TRAITS_quantitative_median_", file_date, ".csv")
                       , name.file_quali = paste0("TRAITS_qualitative_", file_date, ".csv")
                       , traits.quant = read.csv(name.file_quant, stringsAsFactors = F, sep = "\t")
                       , traits.quali = read.csv(name.file_quali, stringsAsFactors = F, sep = "\t")
                       , traits_names = getTraitsFATE_names()
                       , TAB_traits = getTraitsFATE_merge(traits.quant = traits.quant
                                                          , traits.quali = traits.quali
                                                          , TRAIT_names = traits_names)
                       , TAB_traits_FATE = getTraitsFATE_reorganize(TAB_traits = TAB_traits)
                       , name.file_FATE = paste0("TRAITS_FATE_", file_date, ".csv")
                       , TAB_traits_FATE.written = fwrite(TAB_traits_FATE, file = name.file_FATE, sep = "\t")
                       , strings_in_dots = "literals"
)

# name.file_quant = paste0("TRAITS_quantitative_median_", file_date, ".csv")
# name.file_quali = paste0("TRAITS_qualitative_", file_date, ".csv")
# traits.quant = read.csv(name.file_quant, stringsAsFactors = F, sep = "\t")
# traits.quali = read.csv(name.file_quali, stringsAsFactors = F, sep = "\t")
# traits_names = sort(unique(c(unique(traits.quant$CODE), unique(traits.quali$CODE))))
# TAB_traits = getTraitsFATE_merge(traits.quant = traits.quant
#                                    , traits.quali = traits.quali
#                                    , TRAIT_names = traits_names)
# TAB_traits = TAB_traits[, c("CODE_CBNA", traits_names)]
# TAB_traits.written = fwrite(TAB_traits, file = paste0("TRAITS_ALL_", file_date, ".csv"), sep = "\t")
# TAB_species = rbind(unique(traits.quali[, c("code_cbna", "libelle")]), unique(traits.quant[, c("code_cbna", "libelle")]))
# TAB_species = unique(TAB_species)
# TAB_species.written = fwrite(TAB_species, file = paste0("TRAITS_ALL_", file_date, "_correspondance.csv"), sep = "\t")

vis_drake_graph(drake_config(PLAN.FATE)
                , targets_only = TRUE)
make(PLAN.FATE)
vis_drake_graph(drake_config(PLAN.FATE)
                , targets_only = TRUE)

loadd(TAB_traits)
summary(TAB_traits)

loadd(TAB_traits_FATE)
summary(TAB_traits_FATE)
