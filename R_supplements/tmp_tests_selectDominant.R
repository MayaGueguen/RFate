
tab = RES
head(tab)

ind_1 = which(tab$stat.no_sites_abund_over25 > 3)

ind_2 = which(tab$stat.no_sites_abund_max / tab$stat.no_sites_abund > 0.2)
ind_3 = which(tab$stat.abund_median / tab$stat.no_sites_abund > 0.01)
ind_23 = intersect(ind_2, ind_3)

ind_123 = sort(unique(c(ind_1, ind_23)))
tab = tab[ind_123, ]
tab = tab[which(tab$stat.no_sites_recorded > 9), ]

length(intersect(PNE_PFG$dom.traits$species, tab$species))
100 * length(intersect(PNE_PFG$dom.traits$species, tab$species)) / length(tab$species)


##################################################################################################"
mat.site.species.abund = tab_obs[, c("sites", "species", "abund", "habitat")]
mat.rel.sites = tapply(X = mat.site.species.abund$abund
                       , INDEX = list(mat.site.species.abund$species
                                      , mat.site.species.abund$sites), FUN = sum, na.rm = TRUE)
mat.rel.sites = t(apply(mat.rel.sites, 1, function(x) x / colSums(mat.rel.sites, na.rm = TRUE)))

mat.rel.hab = tapply(X = mat.site.species.abund$abund
                       , INDEX = list(mat.site.species.abund$species
                                      , mat.site.species.abund$habitat), FUN = sum, na.rm = TRUE)
mat.rel.hab = t(apply(mat.rel.hab, 1, function(x) x / colSums(mat.rel.hab, na.rm = TRUE)))

dim(mat.rel.sites)
dim(mat.rel.hab)

mat.rel.sites[1:10, 1:10]
head(mat.rel.hab)


## cover class > 25%
ind_1 = apply(mat.rel.sites, 1, function(x) length(which(x >= 0.25)))
ind_1 = which(ind_1 >= 3)
length(ind_1)

## max relative abund > 20%
ind_2 = apply(mat.rel.sites, 1, function(x) length(which(x >= 0.2)))
ind_2 = which(ind_2 > 0)
length(ind_2)

## median relative abund > 1%
ind_3 = apply(mat.rel.sites, 1, function(x) median(x, na.rm = TRUE))
ind_3 = which(ind_3 > 0.01)
length(ind_3)

ind_23 = intersect(ind_2, ind_3)
length(ind_23)


ind_123 = sort(unique(c(ind_1, ind_23)))
length(ind_123)
tab = mat.rel.sites
tab = tab[ind_123, ]
ind_nbobs =apply(tab, 1, function(x) length(which(x > 0)))
tab = tab[which(ind_nbobs > 9), ]
dim(tab)

length(intersect(PNE_PFG$dom.traits$species, rownames(tab)))
100 * length(intersect(PNE_PFG$dom.traits$species, rownames(tab))) / length(rownames(tab))

