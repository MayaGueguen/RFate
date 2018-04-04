# library(RFate)

## Load example data
data(MontBlanc)
str(MontBlanc)

## MontBlanc$mat.releves : data.frame

## Transformation of Braun-Blanquet abundances
MontBlanc$mat.releves$abund = PRE_FATE.abundBraunBlanquet(abund = MontBlanc$mat.releves$abund)

## Selection of dominant species

## With default parametrization
sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = MontBlanc$mat.releves)

## Redefinition of global selection rules
sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = MontBlanc$mat.releves
                                 , selectionRule.quanti = 0.9
                                 , selectionRule.min_mean_abund = 25
                                 , selectionRule.min_no_high_abund = 10)

## Add landclass selection with default parametrization
sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = MontBlanc$mat.releves
                               , selectionRule.quanti = 0.9
                               , selectionRule.min_mean_abund = 25
                               , selectionRule.min_no_high_abund = 10
                               , doLandclass = TRUE)

## Redefinition of landclass selection rules
sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = MontBlanc$mat.releves
                                 , selectionRule.quanti = 0.9
                                 , selectionRule.min_mean_abund = 25
                                 , selectionRule.min_no_high_abund = 10
                                 , doLandclass = TRUE
                                 , selectionRule.min_percent_landclass = 0.05
                                 , selectionRule.min_no_landclass = 10)

## 2 .csv and 2 .pdf files have been produced
str(sp.DOM)

## MontBlanc$mat.traits : data.frame
## MontBlanc$mat.nicheOverlap : niolap object

## Calculate distance between species
sp.DIST = PRE_FATE.speciesDistance(mat.species.traits = MontBlanc$mat.traits,
                                   mat.species.overlap = MontBlanc$mat.nicheOverlap)

str(sp.DIST)

## Run hierarchical clustering and number of clusters' selection
sp.CLUST = PRE_FATE.speciesClustering_step1(mat.species.DIST = sp.DIST)

## 2 .pdf files have been produced
str(sp.CLUST)

## Select number of clusters and find determinant species
sp.DETERM = PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms
                                             , no.clusters = c(11, 7, 8)
                                             , mat.species.DIST = sp.DIST)

## 2 .pdf files have been produced
str(sp.DETERM)

file.remove(list.files(pattern = "^PRE_FATE_"))
file.remove(list.files(pattern = "Rplots.pdf"))


