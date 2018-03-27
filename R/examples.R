library(RFate)

## Load example data
data(MontBlanc)
str(MontBlanc)

## MontBlanc$mat.releves : data.frame

## Selection of dominant species
sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = MontBlanc$mat.releves)

## 2 .csv files have been produced
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

file.remove(list.files(pattern = "^PRE_FATE_CLUSTERING_STEP"))

