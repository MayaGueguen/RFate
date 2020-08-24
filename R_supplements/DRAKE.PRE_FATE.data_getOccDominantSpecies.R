

################################################################################################################
### 1. GET OCCURRENCES
################################################################################################################

getOcc_1_XY = function(stations)
{
  stations.XY = stations[, c("numchrono", "longitudel93_rel", "latitudel93_rel")]
  stations.XY = unique(stations.XY)
  XY = data.frame(stations.XY[, c("longitudel93_rel", "latitudel93_rel")]
                  , row.names = paste0("NUMCHRONO-", stations.XY$numchrono))
  colnames(XY) = c("X_L93", "Y_L93")
  
  return(XY)
}

getOcc_1_obs = function(observations, stations, maskSimul, maskDem)
{
  observations$codecover[which(nchar(observations$codecover) == 0)] = "NA"
  observations$codecover = PRE_FATE.abundBraunBlanquet(observations$codecover)
  observations.xy = merge(observations, stations, by = "numchrono")
  observations.xy$dem = extract(maskDem, observations.xy[, c("longitudel93_rel", "latitudel93_rel")])
  
  # min.dem = maskSimul * maskDem
  # min.dem[which(maskSimul[] == 0)] = NA
  # min.dem = min(min.dem[], na.rm = TRUE)
  # observations.xy = observations.xy[which(observations.xy$dem >= min.dem), ]
  # head(observations.xy)
  
  return(observations.xy)
}


################################################################################################################
### 2. SELECT DOMINANT SPECIES
################################################################################################################

getOcc_2_formatOcc = function(observations.xy, zone.env.hab)
{
  occ = observations.xy[, c("numchrono", "numtaxon", "codecover", "longitudel93_rel", "latitudel93_rel")]
  occ = unique(occ) ## remove strata redundancy
  occ$numchrono = paste0("NUMCHRONO-", occ$numchrono)
  colnames(occ) = c("sites", "species", "abund", "X", "Y")
  occ$habitat = extract(zone.env.hab, occ[, c("X", "Y")])
  occ$habitat = ifelse(is.na(occ$habitat), NA, paste0("HABITAT-", occ$habitat))
  
  return(occ)
}

getOcc_2_selectDom = function(zone.name, occ, selRules)
{
  setwd(zone.name)
  
  ## SELECT DOMINANT SPECIES
  sp.SELECT = PRE_FATE.selectDominant(mat.observations = occ[, c("sites", "species", "abund", "habitat")]
                                      , doRuleA = selRules[['doRuleA']]
                                      , rule.A1 = selRules[['rule.A1']]
                                      , rule.A2_quantile = selRules[['rule.A2_quantile']]
                                      , doRuleB = selRules[['doRuleB']]
                                      , rule.B1_percentage = selRules[['rule.B1_percentage']]
                                      , rule.B1_number = selRules[['rule.B1_number']]
                                      , rule.B2 = selRules[['rule.B2']]
                                      , doRuleC = selRules[['doRuleC']]
                                      , opt.doRobustness = selRules[['opt.doRobustness']]
                                      , opt.robustness_percent = selRules[['opt.robustness_percent']]
                                      , opt.robustness_rep = selRules[['opt.robustness_rep']])
  
  setwd("./../")
  
  return(sp.SELECT)
}


################################################################################################################
### 3. GET DOMINANT SPECIES OBSERVATIONS
################################################################################################################

getOcc_3_matDom = function(occ, stations.COMMUNITY, selected.sp)
{
  ## For relative abundances -----------------------------------------------------

  ## Transform into sites x species matrix
  mat.sites.species.abund = tapply(X = occ$abund
                             , INDEX = list(occ$sites, occ$species)
                             , FUN = function(x) return(sum(x, na.rm = TRUE)))
  dim(mat.sites.species.abund)
  mat.sites.species.abund = mat.sites.species.abund[, which(colSums(mat.sites.species.abund, na.rm = TRUE) > 0)]
  mat.sites.species.abund = mat.sites.species.abund[which(rowSums(mat.sites.species.abund, na.rm = TRUE) > 0), ]
  dim(mat.sites.species.abund)
  
  mat.sites.species.abund = t(apply(mat.sites.species.abund, 1, function(x) x / sum(x, na.rm = TRUE)))
  
  ## Add information of absences from community plots
  ind.COMMUNITY.abund = which(rownames(mat.sites.species.abund) %in% stations.COMMUNITY)
  for(i in ind.COMMUNITY.abund)
  {
    mat.sites.species.abund[i, ] = ifelse(is.na(mat.sites.species.abund[i, ]), 0, mat.sites.species.abund[i, ])
  }
  FULL.mat.sites.species.abund = mat.sites.species.abund
  
  ## Keep only dominant species
  dim(mat.sites.species.abund)
  ind_dom = which(colnames(mat.sites.species.abund) %in% selected.sp)
  mat.sites.species.abund = mat.sites.species.abund[, ind_dom]
  dim(mat.sites.species.abund)
  DOM.mat.sites.species.abund = mat.sites.species.abund
  
  ## For presence / absence ------------------------------------------------------

  ## Transform into sites x species matrix
  occ = occ[which(occ$species %in% selected.sp), ]
  mat.sites.species.PA = tapply(X = occ$abund
                             , INDEX = list(occ$sites, occ$species)
                             , FUN = function(x) return(1))
  
  ## Add information of absences from community plots
  ind.COMMUNITY.PA = which(rownames(mat.sites.species.PA) %in% stations.COMMUNITY)
  for(i in ind.COMMUNITY.PA)
  {
    mat.sites.species.PA[i, ] = ifelse(is.na(mat.sites.species.PA[i, ]), 0, 1)
  }
  dim(mat.sites.species.PA)
  mat.sites.species.PA[1:10, 1:10]
  DOM.mat.sites.species.PA = mat.sites.species.PA
  
  return(list(FULL_abund = FULL.mat.sites.species.abund
              , DOM_abund = DOM.mat.sites.species.abund
              , DOM_PA = DOM.mat.sites.species.PA))
}


getOcc_3_occDom = function(mat.sites.species, species, zone.name, sp.type)
{
  ## Create species occurrences files
  dir.create(paste0(zone.name, "/", sp.type, "_OCC"))
  
  sp.suppr = vector()
  for(sp in colnames(mat.sites.species))
  {
    sp.occ = mat.sites.species[, sp]
    sp.occ = sp.occ[which(!is.na(sp.occ))]
    
    if (length(which(sp.occ == 0)) == 0 || length(which(sp.occ == 1)) < 20) 
    {
      sp.suppr = c(sp.suppr, sp)
    } else
    {
      if (sp.type == "SP"){
        save(sp.occ, file = paste0(zone.name, "/", sp.type, "_OCC/OCC_X", sp))
      } else {
        save(sp.occ, file = paste0(zone.name, "/", sp.type, "_OCC/OCC_", sp))
      }
    }
  }
  cat(" ==> No absence data for :", sp.suppr)
  dom_missing = species[which(species$numtaxon %in% sp.suppr), , drop = FALSE]
  if (nrow(dom_missing) > 0)
  {
    write.csv(dom_missing
              , file = paste0(zone.name, "/MISSING_", sp.type, "_observations.csv")
              , row.names = F)
  }
  
  list_sp = list.files(path = paste0(zone.name, "/", sp.type, "_OCC/"))
  list_sp = sub("OCC_", "", list_sp)
  return(list_sp)
}

################################################################################################################
### 4. BUILD SDM FOR DOMINANT SPECIES
################################################################################################################

WRAPPER <- function(sp.name, zone.name, XY, zone.env.stk.CALIB, zone.env.stk.PROJ, check.computed, sp.type)
{
  load(paste0(zone.name, "/", sp.type, "_OCC/OCC_",sp.name))
  xy = XY[names(sp.occ), c("X_L93","Y_L93")]
  
  setwd(paste0(zone.name, "/", sp.type, "_SDM"))
  
  
  ## formating data in a biomod2 friendly way ------------------------------------
  bm.form <- BIOMOD_FormatingData(resp.var = sp.occ
                                  , expl.var = zone.env.stk.CALIB
                                  , resp.xy = xy
                                  , resp.name = sp.name)
  
  ## define models options -------------------------------------------------------
  bm.opt <- BIOMOD_ModelingOptions(GLM = list(type = "quadratic"
                                              , interaction.level = 0
                                              , test = "AIC")
                                   , GBM = list(n.trees = 5000)
                                   , GAM = list(k = 3))
  
  ## run single models -----------------------------------------------------------
  
  if (check.computed)
  {
    bm.mod.file <- list.files(path = sp.name
                              , pattern = "mod1.models.out$"
                              , full.names = TRUE)
    bm.em.file <- list.files(path = sp.name
                             , pattern = "ensemble.models.out$"
                             , full.names = TRUE)
    bm.ef.file <- list.files(path = paste0(sp.name, "/proj_current")
                             , pattern = "ensemble.projection.out$"
                             , full.names = TRUE)
  } else
  {
    bm.mod.file <- bm.em.file <- bm.ef.file <- NULL
  }
  
  if (check.computed & length(bm.mod.file))
  {
    cat("\n loading previous version of bm.mod..")
    bm.mod <- get(load(bm.mod.file))
  } else
  {
    bm.mod <- BIOMOD_Modeling(data = bm.form
                              , models = c('RF', 'GLM', 'GAM') # c('RF', 'MARS', 'GLM', 'GAM', 'GBM')
                              , models.options = bm.opt
                              , NbRunEval = 5
                              , DataSplit = 70
                              , Prevalence = 0.5
                              , VarImport = 5
                              , models.eval.meth = c('TSS','ROC')
                              , do.full.models = FALSE
                              , modeling.id = 'mod1')
  }
  
  ## run ensemble models ---------------------------------------------------------
  if (check.computed & length(bm.em.file))
  {
    cat("\n loading previous version of bm.em..")
    bm.em <- get(load(bm.em.file))
  } else
  {
    bm.em <- BIOMOD_EnsembleModeling(modeling.output = bm.mod
                                     , chosen.models = "all"
                                     , em.by = "all"
                                     , eval.metric = c('TSS')
                                     , eval.metric.quality.threshold = 0.4
                                     , models.eval.meth = c('TSS', 'ROC')
                                     , prob.mean = FALSE
                                     , prob.mean.weight = TRUE
                                     , prob.mean.weight.decay = 'proportional'
                                     , committee.averaging = FALSE
                                     , VarImport = 5)
  }
  
  ## project ensemble models -----------------------------------------------------
  if (check.computed & length(bm.ef.file))
  {
    cat("\n loading previous version of bm.ef..")
    bm.ef <- get(load(bm.ef.file))
  } else
  {
    bm.ef <- BIOMOD_EnsembleForecasting(EM.output = bm.em
                                        , new.env = zone.env.stk.PROJ
                                        , output.format = ".tif" #".img"
                                        , proj.name = "current"
                                        , selected.models = "all"
                                        , binary.meth = c('TSS'))
  }
  
  setwd("./../../")
  cat("\n\nCompleted!")
}


getSDM_env = function(zone.name, zone.env.folder, zone.env.variables, maskSimul)
{
  env.files = list.files(path = paste0(zone.name, "/", zone.env.folder)
                         , pattern = paste0(paste0(zone.env.variables, ".img", collapse = "|")
                                            , "|"
                                            , paste0(zone.env.variables, ".tif", collapse = "|"))
                         , full.names = TRUE)
  zone.env.stk.CALIB = raster::stack(env.files)
  
  origin(maskSimul) = origin(zone.env.stk.CALIB)
  zone.env.stk.PROJ = stack(zone.env.stk.CALIB * maskSimul)
  names(zone.env.stk.PROJ) = names(zone.env.stk.CALIB)
  
  return(list(env.CALIB = zone.env.stk.CALIB
              , env.PROJ = zone.env.stk.PROJ))
}


getSDM_build = function(no_cores, zone.name, list_sp, XY, zone.env.stk.CALIB, zone.env.stk.PROJ, sp.type)
{
  dir.create(paste0(zone.name, "/", sp.type, "_SDM"))
  
  # mclapply(X = list_sp
  #          , FUN = WRAPPER
  #          , zone.name = zone.name
  #          , XY = XY
  #          , zone.env.stk.CALIB = zone.env.stk.CALIB
  #          , zone.env.stk.PROJ = zone.env.stk.PROJ
  #          , check.computed = TRUE
  #          , sp.type = sp.type
  #          , mc.cores = 4)
  
  registerDoParallel(cores = no_cores)
  wrap = foreach(i.sp = list_sp) %dopar%
    {
      wrap = try(WRAPPER(sp.name = i.sp
                         , zone.name = zone.name
                         , XY = XY
                         , zone.env.stk.CALIB = zone.env.stk.CALIB
                         , zone.env.stk.PROJ = zone.env.stk.PROJ
                         , check.computed = TRUE
                         , sp.type = sp.type))
      return(wrap)
    }
  
  cat("\nended at:", format(Sys.time(), "%a %d %b %Y %X"), "\n")
  return(TRUE)
}


################################################################################################################
### 5. CALCULATE DOMINANT SPECIES SDM OVERLAP
################################################################################################################

getSDM_overlap = function(zone.name, list_sp, maskSimul, SDMbuilt)
{
  setwd(paste0(zone.name, "/SP_SDM/"))

  proj.files = sapply(list_sp, function(x) paste0(x, "/proj_current/proj_current_", x, "_ensemble.img"))
  proj.files = proj.files[file.exists(proj.files)]
  
  for (fi in proj.files)
  {
    ras = raster(fi)
    # ras[] = ras[] * maskSimul
    ras[which(maskSimul[] == 0)] = NA
    ras[] = ras[] / 1000
    writeRaster(ras, filename = sub(".img", ".asc", basename(fi)), overwrite = TRUE)
  }
  
  proj.files = sapply(list_sp, function(x) paste0("proj_current_", x, "_ensemble.asc"))
  proj.files = proj.files[file.exists(proj.files)]

  mat.overlap = niche.overlap(proj.files)
  colnames(mat.overlap) = sub("proj_current_", "", colnames(mat.overlap))
  colnames(mat.overlap) = sub("_ensemble.asc", "", colnames(mat.overlap))
  rownames(mat.overlap) = sub("proj_current_", "", rownames(mat.overlap))
  rownames(mat.overlap) = sub("_ensemble.asc", "", rownames(mat.overlap))
  
  file.remove(proj.files)
  
  setwd("./../../")

  return(mat.overlap)
}
