

################################################################################################################
## 1. GET TRAITS
################################################################################################################


getPFG_1_selectTraits = function(mat.traits)
{
  mat.traits.select = data.frame(species = paste0("X", mat.traits$CODE_CBNA))
  mat.traits.select$GROUP = mat.traits$LHIST
  
  ## Fix ordered factors
  mat.traits.select$DISPERSAL = ordered(factor(mat.traits$DISPERSAL))
  mat.traits.select$LIGHT = ordered(factor(mat.traits$LIGHT))
  mat.traits.select$NITROGEN = ordered(factor(mat.traits$NITROGEN))
  mat.traits.select$MOISTURE = ordered(factor(mat.traits$MOISTURE))
  # mat.traits.select$PALATABILITY = ifelse(is.na(mat.traits$PALATABILITY), NA, paste0("pal", mat.traits$PALATABILITY))
  # mat.traits.select$PALATABILITY = ordered(factor(mat.traits.select$PALATABILITY))
  
  ## Take the root square of height:
  mat.traits.select$HEIGHT = as.numeric(log(as.numeric(mat.traits$HEIGHT)))
  # mat.traits.select$DISPERSAL = exp(as.numeric(as.character(mat.traits.select$DISPERSAL))) # Disp is not an ordered factor anymore!
  
  return(mat.traits.select)
}


################################################################################################################
## 2. DO CLUSTERING 
################################################################################################################

getPFG_2_calcDistClust = function(zone.name, mat.traits.select, mat.overlap)
{
  setwd(zone.name)
  
  mat.traits.DOM = mat.traits.select[which(mat.traits.select$species %in% colnames(mat.overlap)),]
  mat.traits.DOM = mat.traits.DOM[which(mat.traits.DOM$GROUP != ""), ]
  ind.GROUP = table(mat.traits.DOM$GROUP)
  ind.GROUP = names(ind.GROUP)[which(ind.GROUP <= 1)]
  if (length(ind.GROUP) > 0)
  {
    mat.traits.DOM = mat.traits.DOM[-which(mat.traits.DOM$GROUP %in% ind.GROUP), ]
  }
  
  sp.DIST = PRE_FATE.speciesDistance(mat.species.traits = mat.traits.DOM
                                     , mat.species.overlap = mat.overlap
                                     , min.info.thresh = 0.3)
  
  sp.CLUST = PRE_FATE.speciesClustering_step1(mat.species.DIST = sp.DIST)
  
  setwd("./../")
  return(list(sp.DIST = sp.DIST, sp.CLUST = sp.CLUST))
}


getPFG_2_calcDeterm = function(zone.name, sp.DIST, sp.CLUST, no.clusters, species)
{
  setwd(zone.name)
  
  sp.DETERM = PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms
                                               , no.clusters = no.clusters
                                               , mat.species.DIST = sp.DIST)
  
  selected.sp = sp.DETERM$determ.all
  selected.sp$pfg = as.character(selected.sp$pfg)
  selected.sp$sp = as.character(selected.sp$sp)
  selected.sp$sp = sub("X", "", selected.sp$sp)
  
  selected.sp = merge(species, selected.sp, by.x = "numtaxon", by.y = "sp", all.y = TRUE)
  colnames(selected.sp)[c(1:4, 12)] = c("CODE_CBNA", "GENUS", "SPECIES_NAME", "PFG", "TO_REMOVE")
  selected.sp = selected.sp[, c("CODE_CBNA", "GENUS", "SPECIES_NAME", "PFG", "group", "TO_REMOVE"
                                , "sp.mean.dist", "allSp.mean", "allSp.min", "allSp.max")]
  selected.sp = selected.sp[order(selected.sp$PFG, selected.sp$SPECIES_NAME), ]
  
  PFG1 = sapply(selected.sp$PFG, function(x) strsplit(x, "_")[[1]][1])
  PFG1 = sapply(PFG1, function(x) strsplit(x, "")[[1]][1])
  
  PFG2 = sapply(selected.sp$PFG, function(x) strsplit(x, "_")[[1]][2])
  PFG2 = sapply(PFG2, function(x) strsplit(x, "")[[1]][1])
  PFG2 = ifelse(is.na(PFG2), "", PFG2)
  
  PFG3 = sapply(selected.sp$PFG, function(x) strsplit(x, "[.]")[[1]][2])
  
  selected.sp$PFG = paste0(PFG1, PFG2, PFG3)
  fwrite(x = selected.sp, file = "PRE_FATE_PFG_selection_PFG_ALL.csv")
  
  determinant_PFG = selected.sp[which(selected.sp$CODE_CBNA %in% sp.DETERM$determ.sp), ]
  fwrite(x = determinant_PFG, file = "PRE_FATE_PFG_selection_PFG_DETERMINANT.csv")
  
  setwd("./../")
  return(selected.sp)
}

getPFG_2_keepDeterm = function(zone.name, selected.sp)
{
  selected.sp = selected.sp[which(selected.sp$TO_REMOVE == 0), ]
  
  return(selected.sp)
}

################################################################################################################
## 3. GET SITES x PFG occurrences matrix
################################################################################################################

getPFG_3_matSitesPFG = function(zone.name, mat.sites.species, selected.sp)
{
  setwd(zone.name)
  
  ind.toKeep = selected.sp$CODE_CBNA[which(selected.sp$TO_REMOVE == 0)]
  mat.sites.species = mat.sites.species[, which(colnames(mat.sites.species) %in% ind.toKeep)]
  
  mat.sites.pfg = foreach (fg = unique(selected.sp$PFG), .combine = "cbind") %do%
  {
    ind.fg = selected.sp$CODE_CBNA[which(selected.sp$PFG == fg)]
    ind.fg = which(colnames(mat.sites.species) %in% ind.fg)
    val.fg = mat.sites.species[, ind.fg, drop = FALSE]
    val.fg = apply(val.fg, 1, function(x){
      if (length(which(is.na(x))) == length(x)){
        return(NA)
      } else if (length(which(x == 1)) > 0){
        return(1)
      } else if (length(which(x == 0)) > 0){
        return(0)
      }
    })
    return(matrix(data = val.fg, ncol = 1, dimnames = list(rownames(mat.sites.species), fg)))
  }
  
  dim(mat.sites.pfg)
  mat.sites.pfg[1:10, 1:10]
  
  save(mat.sites.pfg, file = "mat.sites.pfg.RData")
  
  setwd("./../")
  return(mat.sites.pfg)
}

################################################################################################################
## 4. CALCULATE MEDIAN / MEAN VALUES PER PFG
################################################################################################################

getPFG_4_calcMeanTraits = function(zone.name, mat.traits, selected.sp)
{
  setwd(zone.name)
  
  ## GET PFG GROUPING
  tab_pfg = selected.sp[, c("CODE_CBNA", "PFG")]
  
  ## GET SPECIES OF INTEREST
  mat.traits = merge(tab_pfg, mat.traits, by = "CODE_CBNA", all.x = TRUE)
  
  ## GET TRAITS OF INTEREST
  mat.traits.sp = data.frame(species = paste0("X", mat.traits$CODE_CBNA))
  mat.traits.sp$PFG = as.character(mat.traits$PFG)
  mat.traits.sp$type = as.character(mat.traits$LHIST)
  mat.traits.sp$type[which(mat.traits.sp$type %in% c("Chamaephyte_S"))] = "C"
  mat.traits.sp$type[which(mat.traits.sp$type %in% c("Chamaephyte_H", "Geophyte_Hemicryptophyte", "Therophyte", "Hydrophyte"))] = "H"
  mat.traits.sp$type[which(mat.traits.sp$type %in% c("Phanerophyte"))] = "P"
  
  mat.traits.sp$height = as.numeric(as.character(mat.traits$HEIGHT))
  mat.traits.sp$longevity = as.numeric(as.character(mat.traits$LONGEVITY))
  mat.traits.sp$maturity = as.numeric(as.character(mat.traits$MATURITY))
  
  mat.traits.sp$palatability = factor(mat.traits$PALATABILITY, 1:4)
  mat.traits.sp$dispersal = factor(mat.traits$DISPERSAL, 1:7)
  mat.traits.sp$light = factor(mat.traits$LIGHT, 1:5)
  
  mat.traits.sp$soil_contrib = as.numeric(as.character(mat.traits$NITROGEN))
  mat.traits.sp$soil_tolerance = as.numeric(as.character(mat.traits$NITROGEN_TOLERANCE))
  
  save(mat.traits.sp, file = "mat.traits.sp.RData")
  
  ## CALCULATE MEDIAN TRAIT VALUE PER PFG
  mat.traits.pfg = PRE_FATE.speciesClustering_step3(mat.species.traits = mat.traits.sp)
  
  setwd("./../")
  return(mat.traits.pfg)
}

################################################################################################################
## 5. CALCULATE FATE PARAMETER FILES
################################################################################################################


getPFG_5_FATEparam = function(zone.name, zone.mask, zone.mask.pert.all, zone.mask.pert.def, TRAITS_PFG)
{
  setwd(zone.name)
  zone.name.simulation = paste0("FATE_", zone.name)
  
  PRE_FATE.skeletonDirectory(name.simulation = zone.name.simulation)
  
  pfg_names = as.character(TRAITS_PFG$PFG)
  pfg_H = pfg_names[grep("^H|^G|^T", pfg_names)]
  pfg_C = pfg_names[grep("^C", pfg_names)]
  pfg_P = pfg_names[grep("^P", pfg_names)]
  
  #################################################################################################
  PRE_FATE.params_PFGsuccession(name.simulation = zone.name.simulation
                                , mat.PFG.succ = TRAITS_PFG[, c("PFG", "type", "height"
                                                                , "maturity", "longevity")])
  
  #################################################################################################
  TRAITS_PFG$light = as.numeric(as.character(TRAITS_PFG$light))
  
  PRE_FATE.params_PFGlight(name.simulation = zone.name.simulation
                           , mat.PFG.succ = TRAITS_PFG[, c("PFG", "type", "height"
                                                           , "maturity", "longevity", "light")])
  
  mat.PFG.succ = paste0(zone.name.simulation, "/DATA/PFGS/SUCC_COMPLETE_TABLE.csv")
  mat.PFG.succ = fread(mat.PFG.succ)
  no.strata = max(mat.PFG.succ$STRATA)
  
  #################################################################################################
  mat.dist = data.frame()
  mat.dist = rbind(mat.dist, data.frame(name = "mowing"
                                        , responseStage = c(rep(1:4, each = length(pfg_H))
                                                            , rep(1:4, each = length(pfg_C))
                                                            , rep(1:4, each = length(pfg_P)))
                                        , variable = c(rep(paste0("KilledIndiv_", pfg_H), 4)
                                                       , rep(paste0("KilledIndiv_", pfg_C), 4)
                                                       , rep(paste0("KilledIndiv_", pfg_P), 4))
                                        , value = c(rep(c(0, 0, 4, 10), each = length(pfg_H))
                                                    , rep(c(0, 10, 5, 10), each = length(pfg_C))
                                                    , rep(c(8, 10, 10, 10), each = length(pfg_P)))))
  mat.dist = rbind(mat.dist, data.frame(name = "graz1"
                                        , responseStage = 1
                                        , variable = paste0("KilledIndiv_", TRAITS_PFG$PFG[which(TRAITS_PFG$palatability > 3)])
                                        , value = 1))
  mat.dist = rbind(mat.dist, data.frame(name = "graz1"
                                        , responseStage = 1
                                        , variable = paste0("KilledIndiv_", pfg_P)
                                        , value = 10))
  mat.dist = rbind(mat.dist, data.frame(name = "graz1"
                                        , responseStage = 2
                                        , variable = paste0("KilledIndiv_", pfg_P)
                                        , value = 0))
  mat.dist = rbind(mat.dist, data.frame(name = "graz1"
                                        , responseStage = 2
                                        , variable = paste0("KilledIndiv_", TRAITS_PFG$PFG[which(TRAITS_PFG$palatability > 3)])
                                        , value = 1))
  mat.dist = rbind(mat.dist, data.frame(name = "graz1"
                                        , responseStage = 3
                                        , variable = paste0("ResproutIndiv_", TRAITS_PFG$PFG[which(TRAITS_PFG$palatability %in% c(4,5))])
                                        , value = 1))
  # mat.dist = rbind(mat.dist, data.frame(name = "graz1"
  #                                       , responseStage = 3
  #                                       , variable = paste0("ResproutIndiv_", TRAITS_PFG$PFG[which(TRAITS_PFG$palatability %in% c(6,7))])
  #                                       , value = 5))
  mat.dist = rbind(mat.dist, data.frame(name = "graz1"
                                        , responseStage = 3
                                        , variable = paste0("ResproutIndiv_", pfg_P)
                                        , value = 0))
  mat.dist = rbind(mat.dist, data.frame(name = "graz1"
                                        , responseStage = 4
                                        , variable = paste0("ResproutIndiv_", pfg_P)
                                        , value = 0))
  mat.dist = rbind(mat.dist, data.frame(name = "graz1"
                                        , responseStage = 4
                                        , variable = paste0("ResproutIndiv_", TRAITS_PFG$PFG[which(TRAITS_PFG$palatability > 3)])
                                        , value = 1))
  mat.dist = tapply(X = mat.dist$value, INDEX = list(interaction(mat.dist$responseStage, mat.dist$name)
                                                     , mat.dist$variable), FUN = mean)
  mat.dist[which(is.na(mat.dist))] = 0
  mat.dist = as.data.frame(mat.dist)
  mat.dist$name = sapply(rownames(mat.dist), function(x) strsplit(x, "[.]")[[1]][2])
  mat.dist$responseStage = as.numeric(sapply(rownames(mat.dist), function(x) strsplit(x, "[.]")[[1]][1]))
  if (sum(colnames(mat.dist) %in% paste0("ResproutIndiv_", pfg_names)) < length(pfg_names))
  {
    for (pfg in pfg_names)
    {
      if(length(which(colnames(mat.dist) == paste0("ResproutIndiv_", pfg))) == 0)
      {
        eval(parse(text = paste0("mat.dist$ResproutIndiv_", pfg, " = 0")))
      }
    }
  }

  PRE_FATE.params_PFGdisturbance(name.simulation = zone.name.simulation
                                 , mat.PFG.dist = mat.dist)
  
  #################################################################################################
  
  # The three lines (parameters) are : d50, d99, LD (cf. Vittoz 2007 and Engler 2009)
  ## !  first dispersal distance has been changed from 0.1 to 1 in order to keep integers
  # disper = matrix(c(c(1, 1, 2, 40, 100, 400, 500),
  #                   c(2, 5, 15, 150, 500, 1500, 5000),
  #                   c(1000, 1000, 1000, 5000, 5000, 10000, 10000))
  #                   , 3, 7, byrow = TRUE)
  # disper = matrix(c(c(1, 1, 2, 40, 100, 400, 500),
  #                     c(78000,78000,78000,78000,78000,78000,78000),
  #                     c(79000,79000,79000,79000,79000,79000,79000))
  #                 , 3, 7, byrow = TRUE)
  disper = matrix(c(c(1, 1, 2, 40, 100, 400, 500),
                    c(2, 5, 15, 150, 500, 1500, 5000),
                    c(79000,79000,79000,79000,79000,79000,79000))
                  , 3, 7, byrow = TRUE)
  colnames(disper) = 1:7
  rownames(disper) = c("d50", "d99", "ldd")
  (disper)
  
  mat.disp = data.frame(PFG = TRAITS_PFG$PFG
                        , MODE = 1
                        , d50 = disper[1, TRAITS_PFG$dispersal]
                        , d99 = disper[2, TRAITS_PFG$dispersal]
                        , ldd = disper[3, TRAITS_PFG$dispersal])
  
  PRE_FATE.params_PFGdispersal(name.simulation = zone.name.simulation
                               , mat.PFG.disp = mat.disp)
  
  #################################################################################################
  
  ras_mask = raster(paste0("./../", zone.mask))
  ras_mask[which(is.na(ras_mask[]))] = 0
  writeRaster(ras_mask
              , filename = paste0(zone.name.simulation, "/DATA/MASK/", basename(zone.mask))
              , overwrite = TRUE)
  
  for (fg in pfg_names)
  {
    ras_from = paste0("PFG_SDM/", fg, "/proj_current/proj_current_", fg, "_ensemble.img")
    ras_to = paste0(zone.name.simulation, "/DATA/PFGS/HABSUIT/HS_", fg, "_0.tif")
    if (!file.exists(ras_to))
    {
      ras = raster(ras_from)
      ras[] = ras[] / 1000
      ras = projectRaster(ras, ras_mask, res = 100)
      ras[which(is.na(ras[]))] = 0
      ras[which(ras[] > 1)] = 1
      writeRaster(ras, filename = ras_to, overwrite = TRUE)
    }
  }
  
  for (pert in zone.mask.pert.all)
  {
    ras = raster(paste0("./../", pert))
    ras = projectRaster(ras, ras_mask, res = 100)
    ras[which(is.na(ras[]))] = 0
    writeRaster(ras
                , filename = paste0(zone.name.simulation, "/DATA/MASK/DIST_", basename(pert))
                , overwrite = TRUE)
  }
  
  ras.names.dist = paste0(zone.name.simulation, "/DATA/MASK/DIST_", basename(zone.mask.pert.all))
  mat.dist.change = data.frame(year = rep(c(600, 601, 800, 801), each = 2)
                               , order = rep(1:2, 4)
                               , file.name = rep(c(ras.names.dist, rev(ras.names.dist)), 2))
  
  PRE_FATE.params_changingYears(name.simulation = zone.name.simulation
                                , type.changing = "DIST"
                                , mat.changing = mat.dist.change)
  
  #################################################################################################
  PRE_FATE.params_PFGsoil(name.simulation = zone.name.simulation
                          , mat.PFG.soil = TRAITS_PFG[, c("PFG", "soil_contrib", "soil_tol_min", "soil_tol_max")]
                          , no.class = max(TRAITS_PFG$soil_tol_max))
  
  #################################################################################################
  PRE_FATE.params_namespaceConstants(name.simulation = zone.name.simulation
                                     , global.abund.low = 1000000
                                     , global.abund.med = 5000000
                                     , global.abund.high = 8000000
                                     , global.max.by.cohort = 5000000
                                     , global.resource.thresh.med = 13000000
                                     , global.resource.thresh.low = 19000000)
  
  #################################################################################################
  PRE_FATE.params_saveYears(name.simulation = zone.name.simulation
                            , years.maps = c(seq(20, 500, 20), seq(510, 850, 10)))
                            # , years.maps = seq(0,850,10)
                            # , years.objects = 850)
  
  #################################################################################################
  
  ## NOTHING (+ dispersal + HS)
  PRE_FATE.params_globalParameters(name.simulation = zone.name.simulation
                                   , opt.no_CPU = 7
                                   , required.no_PFG = nrow(TRAITS_PFG)
                                   , required.no_STRATA = no.strata
                                   , required.simul_duration = 850
                                   , required.seeding_duration = 300
                                   , required.seeding_timestep = 1
                                   , doDispersal = TRUE
                                   , doHabSuitability = TRUE
                                   , HABSUIT.ref_option = 1
                                   , doDisturbances = TRUE
                                   , DIST.no = 2
                                   , DIST.no_sub = 4
                                   , DIST.freq = c(1, 1)
                                   , doLight = FALSE
                                   , doSoil = FALSE)
  
  ## LIGHT (+ dispersal + HS)
  PRE_FATE.params_globalParameters(name.simulation = zone.name.simulation
                                   , opt.no_CPU = 7
                                   , required.no_PFG = nrow(TRAITS_PFG)
                                   , required.no_STRATA = no.strata
                                   , required.simul_duration = 850
                                   , required.seeding_duration = 300
                                   , required.seeding_timestep = 1
                                   , doDispersal = TRUE
                                   , doHabSuitability = TRUE
                                   , HABSUIT.ref_option = 1
                                   , doDisturbances = TRUE
                                   , DIST.no = 2
                                   , DIST.no_sub = 4
                                   , DIST.freq = c(1, 1)
                                   , doLight = TRUE
                                   , doSoil = FALSE)
  
  ## SOIL (+ dispersal + HS)
  PRE_FATE.params_globalParameters(name.simulation = zone.name.simulation
                                   , opt.no_CPU = 7
                                   , required.no_PFG = nrow(TRAITS_PFG)
                                   , required.no_STRATA = no.strata
                                   , required.simul_duration = 850
                                   , required.seeding_duration = 300
                                   , required.seeding_timestep = 1
                                   , doDispersal = TRUE
                                   , doHabSuitability = TRUE
                                   , HABSUIT.ref_option = 1
                                   , doDisturbances = TRUE
                                   , DIST.no = 2
                                   , DIST.no_sub = 4
                                   , DIST.freq = c(1, 1)
                                   , doLight = FALSE
                                   , doSoil = TRUE)
                                   # , SOIL.no_categories = max(TRAITS_PFG$soil_tol_max))
  
  ## LIGHT + SOIL (+ dispersal + HS)
  PRE_FATE.params_globalParameters(name.simulation = zone.name.simulation
                                   , opt.no_CPU = 7
                                   , required.no_PFG = nrow(TRAITS_PFG)
                                   , required.no_STRATA = no.strata
                                   , required.simul_duration = 850
                                   , required.seeding_duration = 300
                                   , required.seeding_timestep = 1
                                   , doDispersal = TRUE
                                   , doHabSuitability = TRUE
                                   , HABSUIT.ref_option = 1
                                   , doDisturbances = TRUE
                                   , DIST.no = 2
                                   , DIST.no_sub = 4
                                   , DIST.freq = c(1, 1)
                                   , doLight = TRUE
                                   , doSoil = TRUE)
                                   # , SOIL.no_categories = max(TRAITS_PFG$soil_tol_max))
  
  #################################################################################################
  
  PRE_FATE.params_simulParameters(name.simulation = zone.name.simulation
                                  , name.mask = basename(zone.mask)
                                  , name.dist = paste0("DIST_", basename(zone.mask.pert.def)))
  
  setwd("./../")
}