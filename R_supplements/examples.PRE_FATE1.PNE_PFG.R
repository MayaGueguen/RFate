library(RFate)
setwd("RFate/R_supplements/")

## Load example data
PNE_PFG = .loadData("PNE_PFG")
str(PNE_PFG)


## Transformation of Braun-Blanquet abundances
tab_obs = PNE_PFG$sp.observations
tab_obs$abund = PRE_FATE.abundBraunBlanquet(abund = tab_obs$abund_BB)
head(tab_obs)

## TRICK --------------------
## Calculate relative abundance of species per habitat
## to identify species 'over-represented' in particular habitat
# nb_hab = unique(tab_obs[, c("sites", "habitat")])
# nb_hab = table(tt$habitat)
# barplot(nb_hab)
# 
# ## Create species x habitat table (sum of abundances)
# tt = tapply(X = tab_obs$abund, INDEX = list(tab_obs$species, tab_obs$habitat), FUN = sum, na.rm = TRUE)
# sum_hab = colSums(tt, na.rm = TRUE)
# ## Transform into relative abundances per habitat (species x habitat)
# tt = t(apply(tt, 1, function(x) round(x / sum_hab, 2)))
# ## Keep only species a bit representative of one or several habitats
# # tt = tt[which(rowSums(tt, na.rm = TRUE) > 0), ]
# # tt = tt[which(tt > 0)]
# tt = as.data.frame(tt)
# tt$species = rownames(tt)
# tt = melt(tt, id.vars = "species")
# colnames(tt) = c("species", "habitat", "abund_rel")
# tt = tt[which(tt$abund_rel > 0.01), ]
# nrow(tt)
# length(unique(tt$species))
# length(unique(tt$habitat))
# dom_hab = table(tt$habitat)
# 
# ## Representation of each habitat
# tmp1 = as.data.frame(nb_hab)
# tmp2 = as.data.frame(sum_hab)
# tmp3 = as.data.frame(dom_hab)
# tmp = merge(tmp1, tmp2, by.x = "Var1", by.y = "row.names")
# colnames(tmp) = c("habitat", "nb_sites", "abund")
# tmp = merge(tmp, tmp3, by.x = "habitat", by.y = "Var1")
# colnames(tmp) = c("habitat", "nb_sites", "abund", "nb_dom")
# tmp = tmp[order(tmp$nb_sites), ]
# tmp$habitat = factor(tmp$habitat, tmp$habitat)
# tmp$nb_dom_rescaled = 1500 * tmp$nb_dom / max(tmp$nb_dom)
# 
# library(ggplot2)
# library(ggthemes)
# ggplot(tmp, aes(x = as.numeric(habitat), y = nb_sites)) +
#   geom_col(aes(fill = nb_sites), width = 0.1) +
#   geom_point(aes(size = abund, color = abund)) +
#   geom_line(aes(y = nb_dom_rescaled), lty = 2) +
#   geom_point(aes(y = nb_dom_rescaled)) +
#   scale_x_continuous(breaks = 1:nrow(tmp), labels = tmp$habitat) +
#   scale_fill_gradientn(colours = c("orange", "brown")) +
#   scale_size(guide = FALSE) +
#   labs(x = "", y = "Number of sites\n") +
#   theme_pander()
## TRICK --------------------

## Selection of dominant species

## With default parametrization : 2049
# sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = tab_obs[, c("sites", "species", "abund", "habitat")])

## Redefinition of global selection rules : 210
sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = tab_obs[, c("sites", "species", "abund", "habitat")]
                                 , selectionRule.quanti = 0.9
                                 , selectionRule.min_mean_abund = 25
                                 , selectionRule.min_no_abund_over25 = 10)

length(intersect(PNE_PFG$dom.traits$species, sp.DOM$species))
100 * length(intersect(PNE_PFG$dom.traits$species, sp.DOM$species)) / length(sp.DOM$species)


## Add landclass selection with default parametrization : 210
sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = tab_obs[, c("sites", "species", "abund", "habitat")]
                               , selectionRule.quanti = 0.9
                               , selectionRule.min_mean_abund = 25
                               , selectionRule.min_no_abund_over25 = 10
                               , doHabitatSelection = TRUE)

## Redefinition of landclass selection rules : 210
sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = tab_obs[, c("sites", "species", "abund", "habitat")]
                                 , selectionRule.quanti = 0.9
                                 , selectionRule.min_mean_abund = 25
                                 , selectionRule.min_no_abund_over25 = 10
                                 , doHabitatSelection = TRUE
                                 , selectionRule.min_percent_habitat = 0.05
                                 , selectionRule.min_no_habitat = 10)

sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = tab_obs[, c("sites", "species", "abund", "habitat")]
                                 , selectionRule.quanti = 0.8
                                 , selectionRule.min_mean_abund = 25
                                 , selectionRule.min_no_abund_over25 = 3
                                 , doHabitatSelection = TRUE
                                 , selectionRule.min_percent_habitat = 0.01
                                 , selectionRule.min_no_habitat = 10)
length(intersect(PNE_PFG$dom.traits$species, sp.DOM$species))
100 * length(intersect(PNE_PFG$dom.traits$species, sp.DOM$species)) / length(sp.DOM$species)

## 2 .csv and 2 .pdf files have been produced
str(sp.DOM)


## Calculate distance between species
sp.DIST = PRE_FATE.speciesDistance(mat.species.traits = MontBlanc$mat.traits
                                   , mat.species.overlap = MontBlanc$mat.nicheOverlap
                                   , min.info.thresh = 0.9)

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


