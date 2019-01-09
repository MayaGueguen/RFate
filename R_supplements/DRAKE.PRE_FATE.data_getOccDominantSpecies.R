

################################################################################################################
### 1. GET OCCURRENCES
################################################################################################################

getOcc_1_XY = function(stations, zone.name)
{
  stations.XY = stations[, c("numchrono", "longitudel93_rel", "latitudel93_rel")]
  stations.XY = unique(stations.XY)
  XY = data.frame(stations.XY[, c("longitudel93_rel", "latitudel93_rel")]
                  , row.names = paste0("NUMCHRONO-", stations.XY$numchrono))
  colnames(XY) = c("X_L93", "Y_L93")
  
  save(XY, file = paste0(zone.name, "/XY.RData"))
  
  return(XY)
}

getOcc_1_obs = function(observations, stations)
{
  observations$codecover = PRE_FATE.abundBraunBlanquet(observations$codecover)
  observations.xy = merge(observations, stations, by = "numchrono")
  head(observations.xy)
  
  return(observations.xy)
}


################################################################################################################
### 2. SELECT DOMINANT SPECIES
################################################################################################################

getOcc_2_selectDom = function(observations.xy, species, zone.name, selRule1, selRule2, selRule3)
{
  setwd(zone.name)
  
  occ = observations.xy[, c("numchrono", "numtaxon", "codecover", "longitudel93_rel", "latitudel93_rel")]
  occ = unique(occ) ## remove strata redundancy
  colnames(occ) = c("sites", "species", "abund", "X", "Y")
  
  ## SELECT DOMINANT SPECIES
  sp.SELECT = PRE_FATE.selectDominant(mat.site.species.abund = occ
                                      , selectionRule.quanti = selRule1
                                      , selectionRule.min_mean_abund = selRule2
                                      , selectionRule.min_no_abund_over25 = selRule3)
  
  ## Get species names
  sp.SELECT = merge(species, sp.SELECT, by.x = "numtaxon", by.y = "species", all.y = TRUE)
  
  setwd("./../")
  
  return(sp.SELECT)
}


################################################################################################################
### 3. GET DOMINANT SPECIES OBSERVATIONS
################################################################################################################

getOcc_3_matDom = function(sp.SELECT, observations.xy, stations.COMMUNITY, zone.name)
{
  ## Get dominant species observations
  sp.SELECT.occ = merge(sp.SELECT[, c("numtaxon", "genre", "libcbna")], observations.xy, by = "numtaxon")
  
  ## Transform into sites x species matrix
  mat.sites.species = tapply(X = sp.SELECT.occ$codecover
                             , INDEX = list(sp.SELECT.occ$numchrono, sp.SELECT.occ$numtaxon)
                             , FUN = length)
  ind.COMMUNITY = which(rownames(mat.sites.species) %in% stations.COMMUNITY)
  for(i in ind.COMMUNITY)
  {
    mat.sites.species[i, ] = ifelse(is.na(mat.sites.species[i, ]), 0, 1)
  }
  dim(mat.sites.species)
  mat.sites.species[1:10, 1:10]
  
  save(mat.sites.species, file = paste0(zone.name, "/mat.sites.species.DOM.RData"))
  
  return(mat.sites.species)
}

getOcc_3_occDom = function(mat.sites.species, species, zone.name)
{
  ## Create species occurrences files
  dir.create(paste0(zone.name, "/SP_OCC"))
  
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
      save(sp.occ, file = paste0(zone.name, "/SP_OCC/OCC_X", sp))
    }
  }
  cat(" ==> No absence data for :", sp.suppr)
  dom_missing = species[which(species$numtaxon %in% sp.suppr), ]
  write.csv(dom_missing, file = paste0(zone.name, '/MISSING_dominant_species_observations.csv'), row.names = F)
}

################################################################################################################
### 4. BUILD SDM FOR DOMINANT SPECIES
################################################################################################################

WRAPPER <- function(sp.name, zone.name, XY, zone.env.stk, check.computed)
{
  load(paste0(zone.name, "/SP_OCC/OCC_",sp.name))
  xy = XY[names(sp.occ), c("X_L93","Y_L93")]
  
  setwd(paste0(zone.name, "/SP_SDM"))
  
  
  ## formating data in a biomod2 friendly way ------------------------------------
  bm.form <- BIOMOD_FormatingData(resp.var = sp.occ
                                  , expl.var = zone.env.stk
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
                              , models = c('RF', 'MARS', 'GLM', 'GAM', 'GBM')
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
                                     , committee.averaging = TRUE)
  }
  
  ## project ensemble models -----------------------------------------------------
  if (check.computed & length(bm.ef.file))
  {
    cat("\n loading previous version of bm.ef..")
    bm.ef <- get(load(bm.ef.file))
  } else
  {
    bm.ef <- BIOMOD_EnsembleForecasting(EM.output = bm.em
                                        , new.env = zone.env.stk
                                        , output.format = ".img"
                                        , proj.name = "current"
                                        , selected.models = "all"
                                        , binary.meth = c('TSS'))
  }
  
  setwd("./../../")
  cat("\n\nCompleted!")
}


SDM_getEnv = function(zone.name, zone.env.folder, zone.env.variables)
{
  env.files = list.files(path = paste0(zone.name, "/", zone.env.folder)
                              , pattern = paste0(paste0(zone.env.variables, ".img", collapse = "|")
                                                 , "|"
                                                 , paste0(zone.env.variables, ".tif", collapse = "|")))
  zone.env.stk = raster::stack(env.files)

  # origin(maskSimul) = origin(env.stk)
  # zone.env.stk = stack(env.stk * maskSimul)
  # names(zone.env.stk) = names(env.stk)
  
  return(zone.env.stk)
}


SDM_build = function(zone.name, XY, zone.env.stk)
{
  list_sp = list.files(path = paste0(zone.name, "/SP_OCC/"))
  list_sp = sub("OCC_", "", list_sp)
  
  dir.create(paste0(zone.name, "/SP_SDM"))
  
  mclapply(X = list_sp
           , FUN = WRAPPER
           , zone.name = zone.name
           , XY = XY
           , zone.env.stk = zone.env.stk
           , check.computed = TRUE
           , mc.cores = 8)
  
  cat("\nended at:", format(Sys.time(), "%a %d %b %Y %X"))
}
