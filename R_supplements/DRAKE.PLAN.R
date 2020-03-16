
rm(list = ls())

library(RPostgreSQL)
library(foreign)
library(data.table)
library(RFate)
library(biomod2)
library(raster)
library(parallel)
library(phyloclim)
library(drake)
library(foreach)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(doParallel)
library(network)
library(sna)
library(igraph)
# devtools::install_github("briatte/ggnet")
library(ggnet)
library(ggnetwork)
library(ggiraph)

setwd("/home/gueguema/Documents/_TUTOS/3_R/_PACKAGES")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getDB_occ.R")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getOccDominantSpecies.R")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getDB_traits.R")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getTraitsPerSpecies.R")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getTraitsFATErelated.R")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getPFG.R")
source("RFate/R_supplements/DRAKE.PRE_FATE.data_getOccDominantSpecies.R")

path.data = "RFate/data_supplements/"
path.data = "RFate/data/"
setwd(path.data)

file_date = "190621"


################################################################################################################

BAUGES = list(zone.name = "Bauges"
              , zone.extent = c(910795, 983695, 6489093, 6538793)
              , zone.selection.rules = c(0.95, 20, 20, 0.1, 10)
              , zone.mask = "Bauges/MASK_100m.tif"
              , zone.env.folder = "ENV_VARIABLES/EOBS_1970_2005/"
              , zone.env.variables = c("bio_1_0", "bio_8_0", "bio_12_0", "bio_19_0", "slope")
              , zone.mask.pert.all = c("Bauges/MASK_grazing.tif", "Bauges/MASK_noPerturb.tif")
              , zone.mask.pert.def = "Bauges/MASK_noPerturb.tif"
)
ZONE = BAUGES
## ECRINS
## MONTBLANC
## LAUTARET


for(ZONE in list(BAUGES))
{
  
  ################################################################################################################
  ### 1. GET DB VALUES - SELECT DOMINANT SPECIES
  ################################################################################################################
  
  # clean()
  PLAN.full = drake_plan(
    zone.name = ZONE$zone.name
    , zone.extent = ZONE$zone.extent
    , zone.selection.rules = ZONE$zone.selection.rules
    , zone.env.folder = ZONE$zone.env.folder
    , zone.env.variables = ZONE$zone.env.variables
    , zone.mask = raster(file_in(ZONE$zone.mask))
    
    ## Get environmental data
    , zone.env.dem = raster(file_in(paste0(zone.name, "/", zone.env.folder, "/dem.tif")))
    , zone.env.stk = getSDM_env(zone.name = zone.name
                                , zone.env.folder = zone.env.folder
                                , zone.env.variables = zone.env.variables
                                , maskSimul = zone.mask)
    , zone.env.stk.CALIB = zone.env.stk$env.CALIB
    , zone.env.stk.CALIB.saved = save(zone.env.stk.CALIB, file = file_out(paste0(zone.name, "/", zone.name, ".zone.env.stk.CALIB.RData")))
    , zone.env.stk.PROJ = zone.env.stk$env.PROJ
    , zone.env.stk.PROJ.saved = save(zone.env.stk.PROJ, file = file_out(paste0(zone.name, "/", zone.name, ".zone.env.stk.PROJ.RData")))

    ## Get DB
    , DB.OCC = getDB_CBNA(x.min = zone.extent[1]
                          , x.max = zone.extent[2]
                          , y.min = zone.extent[3]
                          , y.max = zone.extent[4])
    ## Get species
    , DB.species = DB.OCC$species[, c("numtaxon", "genre", "libcbna")]
    , DB.species.saved = save(DB.species, file = file_out(paste0(zone.name, "/DB.species.RData")))
    ## Get sites informations
    , DB.stations = DB.OCC$stations[, c("numchrono", "coderqualif", "longitudel93_rel", "latitudel93_rel")]
    , DB.stations.COMMUNITY = DB.stations$numchrono[which(DB.stations$coderqualif %in% c("R06", "R07"))]
    , DB.XY = getOcc_1_XY(stations = DB.stations)
    , DB.XY.saved = save(DB.XY, file = file_out(paste0(zone.name, "/DB.XY.RData")))
    ## Get occurrences
    , DB.observations = DB.OCC$observations[, c("numchrono", "numtaxon", "codecover")]
    , DB.observations.xy = getOcc_1_obs(observations = DB.observations
                                        , stations = DB.stations
                                        , maskSimul = zone.mask
                                        , maskDem = zone.env.dem)
    , DB.observations.xy.saved = save(DB.observations.xy, file = file_out(paste0(zone.name, "/DB.observations.xy.RData")))

    ## Get dominant species informations
    , DOM.sp.dom = getOcc_2_selectDom(observations.xy = DB.observations.xy
                                      , species = DB.species
                                      , zone.name = zone.name
                                      , selRule1 = zone.selection.rules[1]
                                      , selRule2 = zone.selection.rules[2]
                                      , selRule3 = zone.selection.rules[3]
                                      , selRule4 = zone.selection.rules[4]
                                      , selRule5 = zone.selection.rules[5])
    , DOM.sp.dom.Marj = na.exclude(read.csv(file_in(paste0(zone.name, "/DOM.Marjorie_selection.csv"))
                                            , fileEncoding = "windows-1252", header = TRUE, sep = "\t")[,1:2])
    , DOM.sp.dom.updated = merge(DOM.sp.dom, DOM.sp.dom.Marj, by = c("numtaxon", "libcbna"), all = TRUE)
    # , name.file_DOM = paste0(zone.name, "/DOM_species_", zone.name, ".csv")
    # , DOM.sp.dom.updated.written = fwrite(DOM.sp.dom.updated, file = name.file_DOM, sep = "\t")
    , DOM.sp.dom.mat = getOcc_3_matDom(sp.SELECT = DOM.sp.dom.updated
                                       , observations.xy = DB.observations.xy
                                       , stations.COMMUNITY = DB.stations.COMMUNITY
                                       , zone.name = zone.name)
    , DOM.sp.dom.occ = getOcc_3_occDom(mat.sites.species = DOM.sp.dom.mat
                                       , species = species
                                       , zone.name = zone.name
                                       , sp.type = "SP")
    ## Build dominant species sdm
    , DOM.sp.dom.sdm = getSDM_build(zone.name = zone.name
                                    , list_sp = DOM.sp.dom.occ
                                    , XY = DB.XY
                                    , zone.env.stk.CALIB = zone.env.stk$env.CALIB
                                    , zone.env.stk.PROJ = zone.env.stk$env.PROJ
                                    , sp.type = "SP")
    , DOM.sp.dom.overlap = getSDM_overlap(zone.name = zone.name
                                          , list_sp = DOM.sp.dom.occ
                                          , maskSimul = zone.mask)
    
    ## Traits
    , TR.traits = getDB_ANDROSACE()
    , TR.data_1 = getTraits_1_merge.species(traits = TR.traits)
    , TR.data_2 = getTraits_1_merge.traits(traits = TR.data_1)
    , TR.data_3 = getTraits_1_removeUninformative(traits = TR.data_2)
    , TR.data_4 = getTraits_1_remove(traits = TR.data_3)
    , TR.data_5 = getTraits_1_change(traits = TR.data_4)
    , TR.data.split = getTraits_2_split(traits = TR.data_5)
    , TR.traits.quant = TR.data.split$QUANT
    , TR.traits.quali = TR.data.split$QUALI
    , TR.traits.quant.med = getTraits_3_quantMedian(traits_quant = TR.traits.quant)
    , TR.traits.quali.med = getTraits_3_qualiMerged(traits_quali = TR.traits.quali)
    # , TR.traits.genre = traits_genre(TR.data_4)
    # , TR.traits.quant.med_suppr = getTraits_3_thresholdGenus(TR.traits.quant.med, TR.traits.genre)
    # , TR.traits.quali.med_suppr = getTraits_3_thresholdGenus(TR.traits.quali.med, TR.traits.genre)
    , TR.traits.quant.med.saved = getTraits_4_save(traits = TR.traits.quant.med
                                                   , namefile = file_out(paste0("TRAITS_quantitative_median_", file_date, ".csv")))
    , TR.traits.quali.med.saved = getTraits_4_save(traits = TR.traits.quali.med
                                                   , namefile = file_out(paste0("TRAITS_qualitative_", file_date, ".csv")))
    , TR.graph.quant = getTraits_4_graphBarplot(traits = TR.traits.quant.med
                                                , namefile = file_out("GRAPH1_numberValuesPerTrait_quanti.pdf"))
    , TR.graph.quali = getTraits_4_graphBarplot(traits = TR.traits.quali.med
                                                , namefile = file_out("GRAPH2_numberValuesPerTrait_quali.pdf"))
    ## Traits for FATE
    , TR_FATE.traits_names = getTraitsFATE_names()
    , TR_FATE.TAB_traits = getTraitsFATE_merge(traits.quant = TR.traits.quant.med
                                               , traits.quali = TR.traits.quali.med
                                               , TRAIT_names = TR_FATE.traits_names)
    , TR_FATE.TAB_traits_FATE = getTraitsFATE_reorganize(TAB_traits = TR_FATE.TAB_traits)
    , TR_FATE.TAB_traits_FATE.written = fwrite(TR_FATE.TAB_traits_FATE
                                               , file = file_out(paste0("TRAITS_FATE_", file_date, ".csv"))
                                               , sep = "\t")
    ## Select traits
    , PFG.mat.traits.select = getPFG_1_selectTraits(mat.traits = TR_FATE.TAB_traits_FATE)
    ## Build PFG
    , selected.sp = fread(file_in(paste0(zone.name, "/PFG_Bauges_Description_2017_BIS.csv")))
    # Build PFG sdm
    , PFG.mat = getPFG_3_matSitesPFG(zone.name = zone.name
                                     , mat.sites.species = DOM.sp.dom.mat
                                     , selected.sp = selected.sp)
    , PFG.occ = getOcc_3_occDom(mat.sites.species = PFG.mat
                                , species = DB.species
                                , zone.name = zone.name
                                , sp.type = "PFG")
    , PFG.sdm = getSDM_build(zone.name = zone.name
                             , list_sp = PFG.occ
                             , XY = DB.XY
                             , zone.env.stk.CALIB = zone.env.stk.CALIB
                             , zone.env.stk.PROJ = zone.env.stk.PROJ
                             , sp.type = "PFG")
    ## Calculate PFG parameters
    , PFG.mat.traits.pfg = getPFG_4_calcMeanTraits(zone.name = zone.name
                                                   , mat.traits = TR_FATE.TAB_traits_FATE
                                                   , selected.sp = selected.sp)
    , PFG.param = getPFG_5_FATEparam(zone.name = zone.name
                                     , zone.mask = ZONE$zone.mask
                                     , zone.mask.pert.all = ZONE$zone.mask.pert.all
                                     , zone.mask.pert.def = ZONE$zone.mask.pert.def
                                     , TRAITS_PFG = PFG.mat.traits.pfg
                                     , pfg.sdm = PFG.sdm)
  )
  
  ### GET AS ADJACENCY MATRIX --------------------------------------------------------------------------
  mat_adj = as.matrix(as_adjacency_matrix(drake_config(PLAN.full)$graph))
  toKeep = which(rownames(mat_adj) %in% PLAN.full$target)
  mat_adj = mat_adj[toKeep, toKeep]
  
  palette_val = c("zone" = "#FDB462"
                  , "DB" = "#8DD3C7"
                  , "DOM" = "#FFFFB3"
                  , "TR" = "#B3DE69"
                  , "TR_FATE" = "#80B1D3"
                  , "selected" = "#FB8072"
                  , "PFG" = "#BEBADA"
  )

  ### WORKFLOW full ------------------------------------------------------------------------------------
  mat_net = network(mat_adj
                   , matrix = "adjacency"
                   , directed = TRUE)
  network.vertex.names(mat_net) = sapply(rownames(mat_adj)
                                        , function(x) paste0(strsplit(as.character(x), "[.]")[[1]][-1]
                                                             , collapse = "."))
  mat_net %v% "color" = sapply(rownames(mat_adj), function(x) strsplit(as.character(x), "[.]")[[1]][1])
  
  pp = ggnet2(mat_net
              , node.size = 15
              , node.shape = 15
              , node.color = "color"
              , color.palette = palette_val
              , color.legend = ""
              , edge.size = 0.5
              , label = TRUE
  )
  print(pp)
  
  ### WORKFLOW step by step ----------------------------------------------------------------------------
  # categories = unique(sapply(PLAN.full$target, function(x) strsplit(as.character(x), "[.]")[[1]][1]))
  # categories = categories[-which(categories == "selected")]
  # categories = categories[-1] ## remove 'zone'
  # categories[1] = "zone|DB"
  categories = c("zone|DB", "DOM", "TR|TR_FATE", "PFG")
  for(cc in categories)
  {
    cat("\n ==>", cc)
    
    ## Get nodes of interest
    toKeep.1 = grep(paste0("^", cc, "[.]"), rownames(mat_adj))
    
    ## Get incoming nodes
    toKeep.2 = rowSums(mat_adj[ , toKeep.1])
    toKeep.2 = toKeep.2[which(toKeep.2 > 0)]
    
    ## Keep both
    toKeep = unique(c(toKeep.1, which(rownames(mat_adj) %in% names(toKeep.2))))
    cc_mat = mat_adj[toKeep, toKeep]
    
    ## Create network
    cc_net = network(cc_mat
                     , matrix = "adjacency"
                     , directed = TRUE)
    
    ## Change vertex names
    tmp = sapply(rownames(cc_mat)
                 , function(x) paste0(strsplit(as.character(x), "[.]")[[1]][-1]
                                      , collapse = "."))
    # tmp = sapply(tmp, function(x) {
    #   tmp = strsplit(x, "")[[1]]
    #   ind = seq(1, length(tmp), 7)
    #   ind = c(ind, length(tmp) + 1)
    #   tmp = sapply(2:length(ind), function(y) paste0(tmp[(1 + (y-2)*7):(ind[y] - 1)], collapse = ""))
    #   return(paste0(tmp, collapse = "\n"))
    # })
    network.vertex.names(cc_net) = tmp
    
    ## Change vertex colors
    cc_net %v% "color" = sapply(rownames(cc_mat), function(x) strsplit(as.character(x), "[.]")[[1]][1])
    
    ## Plot network
    pp = ggnet2(cc_net
                # , size = 0
                # , label.color = "color"
                , node.size = 25
                , node.shape = 15
                , node.color = "color"
                , color.palette = palette_val
                , color.legend = ""
                , edge.size = 1
                , edge.color = c("color", "gray88")
                , arrow.size = 12
                , arrow.gap = 0.05
                , label = TRUE
                , legend.size = 12
                , legend.position = "right"
    )
    pp = pp +
      labs(title = paste0("Workflow to obtain ", cc)) +
      theme(plot.title = element_text(margin = margin(1, 1, 3, 1, "lines")))
    # pp = pp + theme(panel.background = element_rect(fill = "grey15"))
    print(pp)
    
    
    df = ggnetwork(cc_net, layout = "fruchtermanreingold", cell.jitter = 0.75)
    df$tooltip = paste0("Betweenness = ", round(sna::betweenness(cc_net)[df$vertex.names],2))
    gg_point_1 <-
      ggplot(df, aes(x = x, y = y, xend = xend, yend = yend, tooltip = tooltip)) +
      geom_edges(aes(color = color), lwd = 1) +
      geom_nodes(aes(color = color), size = 15, shape = 15) +
      theme_blank() +
      geom_nodetext(aes(label = vertex.names), fontface = "bold") + 
      geom_point_interactive(aes(color = color, alpha = 1), size = 15, shape = 15) + #this make the node interactive
      scale_color_manual("", values = palette_val) +
      scale_alpha(guide = F, range = c(0,0.1))
    
    
    ggiraph(code = {print(gg_point_1)}) 
  }

  ### --------------------------------------------------------------------------------------------------
  vis_drake_graph(config = drake_config(PLAN.full)
                  , targets_only = TRUE
                  , from_scratch = TRUE
  )
  # , clusters = c("pfg.mat", "pfg.sdm"))
  # outdated(drake_config(PLAN.full))
  # make(PLAN.full)
  # vis_drake_graph(drake_config(PLAN.full)
  #                 , targets_only = TRUE)
  
}

## Run missing species SDM
# loadd(zone.name)
# loadd(sp.dom.occ)
# sp.dom.occ = sp.dom.occ[-which(sp.dom.occ %in% list.files(paste0(zone.name, "/SP_SDM/")))]
# loadd(XY)
# loadd(zone.env.stk)
# sp.dom.sdm = getSDM_build(zone.name = zone.name
#                           , list_sp = sp.dom.occ
#                           , XY = XY
#                           , zone.env.stk.CALIB = zone.env.stk$env.CALIB
#                           , zone.env.stk.PROJ = zone.env.stk$env.PROJ
#                           , sp.type = "SP")
# 
# loadd(sp.dom.occ)
# loadd(zone.mask)
# sp.dom.overlap = getSDM_overlap(zone.name = zone.name
#                                 , list_sp = sp.dom.occ
#                                 , maskSimul = zone.mask)

