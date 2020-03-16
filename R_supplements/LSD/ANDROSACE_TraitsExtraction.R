
library(RPostgreSQL)
library(data.table)
library(foreach)
library(ggplot2)
library(ggthemes)

source("_ANDROSACE_getDB_traits.R")
source("_ANDROSACE_getTraitsPerSpecies.R")

################################################################################################################
## EXTRACT all traits from ANDROSACE database (alpine plants functional traits, LECA)
## and merge information by species to have only value for each trait and species
################################################################################################################

file_date = Sys.Date()



################################################################################################################


## GET all traits from database
TR.traits = getDB_ANDROSACE()
SPECIES = unique(TR.traits[, c("code_cbna", "libelle")])

## MERGE similar species and similar traits
TR.data_1 = getTraits_1_merge.species(traits = TR.traits)
TR.data_2 = getTraits_1_merge.traits(traits = TR.data_1)

## REMOVE uninformative values and similar traits (duplicate)
TR.data_3 = getTraits_1_removeUninformative(traits = TR.data_2)
TR.data_4 = getTraits_1_remove(traits = TR.data_3)

## CORRECT / SIMPLIFY some values
TR.data_5 = getTraits_1_change(traits = TR.data_4)

## SPLIT data to separate qualitative and quantitative traits
TR.data.split = getTraits_2_split(traits = TR.data_5)
TR.traits.quant = TR.data.split$QUANT
TR.traits.quali = TR.data.split$QUALI

## GATHER values per species (paste for qualitative, median for quantitative)
TR.traits.quant.med = getTraits_3_quantMedian(traits_quant = TR.traits.quant)
TR.traits.quali.med = getTraits_3_qualiMerged(traits_quali = TR.traits.quali)

## APPLY threshold according to genus
# TR.traits.genre = traits_genre(TR.data_4)
# TR.traits.quant.med_suppr = getTraits_3_thresholdGenus(TR.traits.quant.med, TR.traits.genre)
# TR.traits.quali.med_suppr = getTraits_3_thresholdGenus(TR.traits.quali.med, TR.traits.genre)

## SAVE traits
TR.traits.quant.med.saved = getTraits_4_save(traits = TR.traits.quant.med
                                             , namefile = paste0("TRAITS_quantitative_median_", file_date, ".csv"))
TR.traits.quali.med.saved = getTraits_4_save(traits = TR.traits.quali.med
                                             , namefile = paste0("TRAITS_qualitative_", file_date, ".csv"))

## PLOT number of values per trait
TR.graph.quant = getTraits_4_graphBarplot(traits = TR.traits.quant.med
                                          , namefile = "GRAPH1_numberValuesPerTrait_quanti.pdf")
TR.graph.quali = getTraits_4_graphBarplot(traits = TR.traits.quali.med
                                          , namefile = "GRAPH2_numberValuesPerTrait_quali.pdf")

## REORGANIZE traits
TAB_traits = getTraits_5_reorganize(traits.quant = TR.traits.quant.med
                                    , traits.quali = TR.traits.quali.med)
TAB_traits = merge(SPECIES, TAB_traits, by.x = "code_cbna", by.y = "row.names")
fwrite(TAB_traits, file = paste0("TRAITS_ALL_", file_date, ".csv"), sep = "\t")
