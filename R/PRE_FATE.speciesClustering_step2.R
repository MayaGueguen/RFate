### HEADER #####################################################################
##' @title Choose clusters and select determinant species
##' 
##' @name PRE_FATE.speciesClustering_step2
##'
##' @author Maya Guéguen
##' 
# @date 21/03/2018
##' 
##' @description This script is designed to obtain functional groups by : 1)
##' select the number of clusters to be kept from an object obtained with the
##' \code{PRE_FATE.speciesClustering_step1} function ; 2) refine these groups
##' by identifying determinant species in each of them.
##'              
##' @param clust.dendograms a dendogram or a \code{list} of dendograms with one
##' for each \code{GROUP} value, as can be obtained with the 
##' \code{PRE_FATE.speciesClustering_step1} function.
##' @param no.clusters a \code{vector} with the number of clusters to be kept
##' in each subset of data (if the data is split).
##' @param mat.species.DIST a \code{dist} object corresponding to the distance
##' between each pair of species, or a \code{list} of \code{dist} objects,
##' one for each \code{GROUP} value. Such an object can be obtained with the
##' \code{PRE_FATE.speciesDistance} function.
##' 
##' 
##' @details 
##' 
##' This function allows one to obtain a classification of \emph{dominant}
##' species into Plant Functional Groups (PFG), and the \emph{determinant}
##' species based on these PFGs.
##' 
##' \strong{What is the difference between \code{dominant} and
##' \code{determinant} species ?}
##' 
##' \itemize{
##'   \item{\strong{Dominant} species are species representative of an
##'   environment or a studied area, in terms of number of releves or
##'   abundance values. They can be found with the \code{PRE_FATE.selectDominant}
##'   function of this package. These dominant species are used to build PFG
##'   with the \code{PRE_FATE.speciesClustering_step1} function.
##'   }
##'   \item{Once PFG are built, \strong{determinant} species are defined as
##'   refined subsets of dominant species within each PFG. The process is
##'   detailed below :
##'   \itemize{
##'     \item each dominant species is assigned to a PFG
##'     \item within each PFG :
##'     \itemize{
##'       \item for each species, compute its mean distance to the other
##'       species within the PFG (\code{sp.mean.dist})
##'       \item calculate the mean value of all these mean distances
##'       (\code{allSp.mean})
##'       \item calculate the deviation values around this mean value
##'       (\code{allSp.min} and \code{allSp.max})
##'       \item determinant species are the ones that are included between
##'       these deviation values
##'     }
##'   }
##'   }
##' }
##' 
##' @return A \code{list} object with 2 elements :
##' 
##' \describe{
##'   \item{determ.sp}{a \code{vector} with the names of all determinant species}
##'   \item{determ.all}{a \code{data.frame} containing all species (determinant
##'   and non-determinant) with 10 columns :
##'   \itemize{
##'     \item \code{pfg} : the ID of the PFG (group + no.cluster)
##'     \item \code{group} : name of sub-dataset
##'     \item \code{no.cluster} : cluster number
##'     \item \code{sp} : name of species
##'     \item \code{ID} : species number in each PFG
##'     \item \code{sp.mean.dist} : species mean distance to other species of
##'     the same PFG
##'     \item \code{allSp.mean} : mean(sp.mean.dist) within the PFG
##'     \item \code{allSp.min} : mean(sp.mean.dist) - 1.64 * sd(sp.mean.dist)
##'     within the PFG
##'     \item \code{allSp.max} : mean(sp.mean.dist) + 1.64 * sd(sp.mean.dist)
##'     within the PFG
##'     \item \code{toSuppr} : 0 if determinant species, 1 otherwise
##'   }
##'   }
##' }
##' 
##' Two \code{PRE_FATE_CLUSTERING_[...].pdf} files are created : 
##' \describe{
##'   \item{\file{STEP_2C \cr distantSpecies}}{to visualize in each PFG the
##'   distribution of mean distance of each species to other species, and
##'   non-determinant species which are outside the distribution}
##'   \item{\file{STEP_2C \cr PCO}}{to visualize in each PFG the distribution
##'   of species, with and without non-determinant species}
##' }
##' 
##' @keywords hierarchical clustering, Principal Component Ordination
##' 
##' @seealso \code{\link{PRE_FATE.speciesDistance}},
##' \code{\link{PRE_FATE.speciesClustering_step1}}
##' 
##' @examples
##' 
##' ## Load example data
##' data(MontBlanc)
##' 
##' ## MontBlanc$mat.traits : data.frame
##' ## MontBlanc$mat.nicheOverlap : niolap object
##' 
##' ## Calculate distance between species
##' sp.DIST = PRE_FATE.speciesDistance(mat.species.traits = MontBlanc$mat.traits,
##'                                    mat.species.overlap = MontBlanc$mat.nicheOverlap)
##'                                    
##' str(sp.DIST)
##'              
##' ## Run hierarchical clustering and number of clusters' selection                      
##' sp.CLUST = PRE_FATE.speciesClustering_step1(mat.species.DIST = sp.DIST)
##' 
##' ## 2 .pdf files have been produced
##' str(sp.CLUST)
##' 
##' ## Select number of clusters and find determinant species
##' sp.DETERM = PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms
##'                                              , no.clusters = c(11, 7, 8)
##'                                              , mat.species.DIST = sp.DIST)
##' 
##' ## 2 .pdf files have been produced
##' str(sp.DETERM)
##' 
##' @export
##' 
##' @importFrom grDevices colorRampPalette pdf dev.off
##' @importFrom graphics abline hist par plot text title
##' @importFrom methods as
##' @importFrom stats as.dist cutree model.matrix sd
##' 
##' @importFrom ggplot2 ggplot aes aes_string ggsave
##' geom_line geom_point geom_hline geom_vline geom_label geom_errorbar geom_path
##' element_text element_blank
##' scale_color_discrete scale_color_manual scale_shape_manual facet_grid labs theme
##' @importFrom ggthemes theme_fivethirtyeight
##' @importFrom ggrepel geom_label_repel
##' @importFrom ade4 quasieuclid dudi.pco
##' 
## END OF HEADER ###############################################################


PRE_FATE.speciesClustering_step2 = function(clust.dendograms
                                            , no.clusters ## vector
                                            , mat.species.DIST
){
  
  group_names = names(clust.dendograms)
  
  ################################################################################################################################
  ## DEFINITION OF CLUSTERED GROUPS
  ################################################################################################################################
  
  ### CUT DENDOGRAMS (or trees) RESULTING FROM hclust INTO SEVERAL GROUPS (nb = k)
  ### Number of groups for each group has been chosen according to the previous plot (CLUSTERING_STEP_1B)
  clust.groups = lapply(1:length(group_names), function(x) cutree(clust.dendograms[[x]], k = no.clusters[x]))
  clust.groups = lapply(1:length(group_names), function(x) {
    tmp.names = names(clust.groups[[x]])
    tmp = paste0(group_names[x], ".", clust.groups[[x]])
    names(tmp) = tmp.names
    return(tmp)
  })
  clust.groups = unlist(clust.groups)
  
  PFG_names = sort(unique(clust.groups))
  
  ################################################################################################################################
  ## IDENTIFY DETERMINANT SPECIES
  ################################################################################################################################
  
  cat("\n ############## DETERMINANT SPECIES ############## \n")
  cat("\n Identification...")
  cat("\n")
  
  ## DETERMINANT SPECIES are the ones whose mean distance to other species
  ## is in the distribution of mean distances of every species to other species
  
  pfg = NULL
  determ = foreach(pfg = PFG_names) %do% {
    ## get the SPECIES names
    sp = names(clust.groups)[which(clust.groups == pfg)]
    
    if (length(sp) > 1)
    {
      ## get the GROUP information
      group = strsplit(pfg, "[.]")[[1]][1]
      no.cluster = strsplit(pfg, "[.]")[[1]][2]
      ## get the DISTANCE information
      mat = as.matrix(mat.species.DIST[[group]])[sp, sp]
      
      ## compute for each species its mean distance to other species
      sp.mean.dist = apply(mat, 1, mean, na.rm = T)
      
      return(data.frame(pfg, group, no.cluster, sp, ID = 1:length(sp), sp.mean.dist,
                        allSp.mean = mean(sp.mean.dist),
                        allSp.min = mean(sp.mean.dist) - 1.64 * sd(sp.mean.dist),
                        allSp.max = mean(sp.mean.dist) + 1.64 * sd(sp.mean.dist)))
    }
  }
  determ = do.call(rbind, determ)
  determ$toSuppr = 0
  determ$toSuppr[which(determ$sp.mean.dist > determ$allSp.max)] = 1
  determ$toSuppr[which(determ$sp.mean.dist < determ$allSp.min)] = 1
  
  ## SAVE DETERM
  ## CAT INFO ABOUT HOW MANY SPECIES ARE REMOVED
  ## CAT INFO ABOUT HOW MANY SPECIES in each PFG
  
  ################################################################################################################################
  ## GRAPHICAL REPRESENTATIONS
  ################################################################################################################################
  
  ## GRAPHICAL REPRESENTATION 1
  colRamp = colorRampPalette(c('#8e0152','#c51b7d','#de77ae','#7fbc41','#4d9221','#276419'))
  
  pp3 = ggplot(determ, aes_string(x = "pfg", y = "sp.mean.dist", color = interaction("group", "toSuppr"), shape = factor("toSuppr"))) +
    geom_errorbar(aes_string(ymin = "allSp.min", ymax = "allSp.max"), color = "darkblue") +
    geom_point(position = "jitter") +
    geom_point(aes_string(y = "allSp.mean"), pch = 18, lwd = 5, color = "darkblue") +
    facet_grid("~ group", scales = "free_x") +
    scale_color_manual(guide = F, values = colRamp(length(levels(interaction(determ$toSuppr, determ$group))))) +
    scale_shape_manual(guide = F, values = c("0" = 20, "1" = 8)) +
    labs(x = "", y = "Mean distance to other species", title = "STEP C : Removal of distant species",
         subtitle = paste0("Only species whose mean distance to other species is included in the distribution\n",
                           "of all PFG's species mean distances to other species are kept.\n",
                           "Species indicated with * will be removed from PFGs.\n",
                           "Non-represented PFG might be one-species-only.")) +
    theme_fivethirtyeight() +
    theme(axis.ticks.x = element_blank())
  
  plot(pp3)
  
  ggsave(filename = "PRE_FATE_CLUSTERING_STEP_2C_distantSpecies.pdf", plot = pp3, width = 10, height = 8)
  
  ## GRAPHICAL REPRESENTATION 2
  ## Compute Principal Coordinates Analysis (PCO) for the determinantes of each group
  ## to see the position in "distance space" (overlap + traits) between species and groups
  
  colRamp = colorRampPalette(c('#8e0152','#c51b7d','#de77ae','#7fbc41','#4d9221','#276419'))
  
  pdf(file = "PRE_FATE_CLUSTERING_STEP_2C_PCO.pdf", width = 10, height = 8)
  for(group in group_names)
  {
    tmp = determ[which(determ$group == group),]
    mat = mat.species.DIST[[group]]
    mat = quasieuclid(as.dist(mat))
    
    PCO = dudi.pco(mat, scannf = FALSE, nf = 3) ## PCO
    PCO.li = PCO$li
    PCO.li$det = ifelse(rownames(PCO.li) %in% tmp$sp[which(tmp$toSuppr == 1)], 0, 1)
    PCO.li$PFG = clust.groups[rownames(PCO.li)]
    
    PCO.li.ELL = .getELLIPSE(xy = PCO.li[, c("A1", "A2")], fac = PCO.li$PFG)
    PCO.li.ELL.det = .getELLIPSE(xy = PCO.li[which(PCO.li$det == 1), c("A1", "A2")], fac = PCO.li$PFG[which(PCO.li$det == 1)])
    
    pp4 = ggplot(PCO.li, aes_string(x = "A1", y = "A2", color = "PFG")) +
      geom_hline(yintercept = 0, color = "grey30", lwd = 1) +
      geom_vline(xintercept = 0, color = "grey30", lwd = 1) +
      geom_point(aes_string(shape = factor("det")), lwd = 3) +
      geom_path(data = PCO.li.ELL, aes_string(x = "x", y = "y"), lty = 2) +
      geom_path(data = PCO.li.ELL.det, aes_string(x = "x", y = "y")) +
      geom_label_repel(data = unique(PCO.li.ELL[, c("xlabel", "ylabel", "PFG")])
                       , aes_string(x = "xlabel", y = "ylabel", label = "PFG")) +
      scale_shape_manual(guide = F, values = c("0" = 8, "1" = 20)) +
      scale_color_discrete(guide = F) +
      labs(x = "", y = "", title = paste0("STEP C : Removal of distant species : group ", group),
           subtitle = paste0("Only species whose mean distance to other species is included in the distribution\n",
                             "of all PFG's species mean distances to other species are kept.\n",
                             "Species indicated with * will be removed from PFGs.\n",
                             "Inertia ellipse are represented, with (solid) and without (dashed) non-determinant species.")) +
      theme_fivethirtyeight()
    
    plot(pp4)
  }
  dev.off()
  
  
  ################################################################################################################################
  
  return(list(determ.sp = determ$sp[which(determ$toSuppr == 0)], determ.all = determ))
}
  
  