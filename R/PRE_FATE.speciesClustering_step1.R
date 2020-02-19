### HEADER #####################################################################
##' @title Create clusters based on dissimilarity matrix
##' 
##' @name PRE_FATE.speciesClustering_step1
##'
##' @author Maya Guéguen
##' 
# @date 21/03/2018
##' 
##' @description This script is designed to create clusters of species based on
##' a distance matrix between those species. Several metrics are computed to
##' evaluate these clusters and a graphic is produced to help the user to choose
##' the best number of clusters (by subset of data if distance matrices are
##' given for each of them).
##'              
##' @param mat.species.DIST a \code{dist} object corresponding to the distance
##' between each pair of species, or a \code{list} of \code{dist} objects, one
##' for each \code{GROUP} value. Such an object can be obtained with the
##' \code{PRE_FATE.speciesDistance} function.
##' 
##' 
##' @details 
##' 
##' This function allows one to obtain dendograms based on a distance matrix
##' between species.
##' 
##' As for the \code{PRE_FATE.speciesDistance} method, clustering can be made
##' for sub-datasets, conditioning that \code{mat.species.DIST} parameter is
##' given as a \code{list} of \code{dist} object (instead of a \code{dist}
##' object alone).
##' 
##' The process is as follows :
##' 
##' \describe{
##'   \item{\strong{1. Choice of the \cr optimal \cr clustering method}}{
##'   hierarchical clustering on the dissimilarity matrix is realized with the
##'   \code{hclust} function.
##'   \itemize{
##'     \item Several methods are available for the agglomeration : complete,
##'     ward.D, ward.D2, single, average (UPGMA), mcquitty (WPGMA), median
##'     (WPGMC) and centroid (UPGMC).
##'     \item Mouchet et el. (2008) proposed of measure of similarity between
##'     the input distance and the one obtained with the clustering which must
##'     be minimized to help finding the best clustering method :
##'     \deqn{ 1 - cor( input.DIST, clustering.DIST ) ^ 2}
##'   }
##'   \strong{For each agglomeration method, this measure is calculated. The
##'   method that minimizes it is kept and used for further analyses. A graphic
##'   (\code{STEP_1A}) is made to account for the comparison of these methods.}
##'   }
##'   
##'   \item{\strong{2. Evaluation of the \cr clustering}}{once the hierarchical
##'   clustering is done, the number of clusters to keep should be chosen.
##'   
##'   To do that, several metrics are computed :
##'   \itemize{
##'     \item{\emph{Dunn index (\code{mdunn}) : }}{ratio of the smallest distance
##'     between observations not in the same cluster to the largest intra-cluster
##'     distance. Value between zero and infinity, and should be maximized.}
##'     \item{\emph{Meila's Variation of Information index (\code{mVI}) : }}
##'     {measures the amount of information lost and gained in changing between
##'     2 clusterings. Should be minimized.}
##'     \item{\emph{Coefficient of determination (\code{R2}) : }}{value between
##'     zero and one. Should be maximized.}
##'     \item{\emph{Calinski and Harabasz index (\code{ch}) : }}{the higher
##'     the value, the "better" is the solution.}
##'     \item{\emph{Corrected rand index (\code{Rand}) : }}{measures the
##'     similarity between two data clusterings. Value between 0 and 1, with 0
##'     indicating that the two data clusters do not agree on any pair of points
##'     and 1 indicating that the data clusters are exactly the same.}
##'     \item{\emph{Average silhouette width (\code{av.sil}) : }}{Observations 
##'     with a large s(i) (almost 1) are very well clustered, a small s(i)
##'     (around 0) means that the observation lies between two clusters, and
##'     observations with a negative s(i) are probably placed in the wrong cluster.
##'     Should be maximized.}
##'   }
##'   \strong{A graphic (\code{STEP_1B}) is produced, giving the values of these
##'   metrics in function of the number of clusters used. Number of clusters with
##'   evaluation metrics' values among the 3 best are highlighted to help the user
##'   to make his/her optimal choice.}
##'   }
##' }
##' 
##' @return A \code{list} object with 2 elements :
##' 
##' \describe{
##'   \item{clust.dendograms}{a \code{list} object with as many objects of class
##'   \code{hclust} as subset of data}
##'   \item{clust.evaluation}{a \code{data.frame} with 4 columns :
##'   \itemize{
##'     \item \code{group} : name of sub-dataset
##'     \item \code{nb.cluster} : number of clusters used for the clustering
##'     \item \code{variable} : evaluation metrics' name
##'     \item \code{value} : value of evaluation metric
##'   }
##'   }
##' }
##' 
##' Two \code{PRE_FATE_CLUSTERING_[...].pdf} files are created : 
##' \describe{
##'   \item{\file{STEP_1A \cr clusteringMethod}}{to account for the chosen
##'   clustering method}
##'   \item{\file{STEP_1B \cr numberOfClusters}}{for decision support, to help
##'   the user to choose the adequate number of clusters to be used into
##'   the \code{hclust} method}
##' }
##' 
##' 
##' @note \strong{The function does not return ONE dendogram} (or as many as given
##' dissimilarity structures) \strong{but a list with all tested numbers of clusters.}
##' One final dendogram can then be obtained using this result as a parameter in
##' the \code{PRE_FATE.speciesClustering_step2} function.
##' 
##' @keywords hierarchical clustering, Dunn index, Meila's Variation of Information index,
##'  R2, Calinski and Harabasz index, Corrected rand index, Average silhouette width
##' 
##' @seealso \code{\link[stats]{hclust}},
##' \code{\link{PRE_FATE.speciesDistance}},
##' \code{\link{PRE_FATE.speciesClustering_step2}}
##' 
##' @examples
##' 
##' ## Load example data
##' PNE_PFG = .loadData("PNE_PFG")
##' 
##' ## PNE_PFG$dom.traits : data.frame
##' ## PNE_PFG$dom.dist_overlap : niolap object
##' 
##' ## Calculate distance between species
##' sp.DIST = PRE_FATE.speciesDistance(mat.species.traits = PNE_PFG$dom.traits
##'                                    , mat.species.overlap = PNE_PFG$dom.dist_overlap
##'                                    , opt.max.percent.NA = 0.9
##'                                    , opt.max.percent.similarSpecies = 0.25
##'                                    , opt.min.sd = 0.3)
##'                                    
##' str(sp.DIST)
##'              
##' ## Run hierarchical clustering and number of clusters' selection                      
##' sp.CLUST = PRE_FATE.speciesClustering_step1(mat.species.DIST = sp.DIST)
##' 
##' ## 2 .pdf files have been produced
##' str(sp.CLUST)
##' 
##' @export
##' 
##' @importFrom grDevices colorRampPalette
##' @importFrom methods as
##' @importFrom stats as.dist cophenetic cor cutree hclust
##' @importFrom utils tail
##' 
##' @importFrom foreach foreach %do% %dopar%
##' @importFrom reshape2 melt
##' @importFrom ggplot2 ggplot aes aes_string ggsave
##' geom_line geom_point geom_vline geom_label
##' element_text element_blank element_rect
##' scale_color_manual scale_linetype_discrete facet_grid labs theme
##' @importFrom ggthemes theme_fivethirtyeight
##' @importFrom fpc cluster.stats
##' @importFrom clValid dunn
##' 
## END OF HEADER ###############################################################


PRE_FATE.speciesClustering_step1 = function(mat.species.DIST)
{
  
  #################################################################################################
  
  ## Check existence of parameters
  if (missing(mat.species.DIST) || is.null(mat.species.DIST))
  {
    stop("No data given!\n (missing `mat.species.DIST` information)")
  }
  ## Control form of parameters : mat.species.DIST
  if(is.list(mat.species.DIST))
  {
    if (length(mat.species.DIST) > 0)
    {
      for (i in 1:length(mat.species.DIST))
      {
        if (class(mat.species.DIST[[i]]) %in% c("dist", "niolap"))
        {
          mat.species.DIST[[i]] = as.matrix(mat.species.DIST[[i]])
        } else if (is.matrix(mat.species.DIST[[i]]))
        {
          if (ncol(mat.species.DIST[[i]]) != nrow(mat.species.DIST[[i]]))
          {
            stop(paste0("Wrong dimension(s) of data!\n `mat.species.DIST[[",
                        i,
                        "]]` does not have the same number of rows (",
                        nrow(mat.species.DIST[[i]]),
                        ") and columns (",
                        ncol(mat.species.DIST[[i]]),
                        ")"
            ))
          }
        } else {
          stop(paste0("Wrong type of data!\n `mat.species.DIST[["
                      , i
                      , "]]` must be a dissimilarity object (`dist`, `niolap`, `matrix`)"))
        }
      }
    } else
    {
      stop("Wrong dimension(s) of data!\n `mat.species.DIST` must be of length > 0")
    }
    if(!is.null(names(mat.species.DIST)))
    {
      group_names = names(mat.species.DIST)
    } else {
      group_names = paste0("GROUP", 1:length(mat.species.DIST))
    }
  } else {
    if (class(mat.species.DIST) %in% c("dist", "niolap"))
    {
      mat.species.DIST = as.matrix(mat.species.DIST)
    } else if (is.matrix(mat.species.DIST))
    {
      if (ncol(mat.species.DIST) != nrow(mat.species.DIST))
      {
        stop(paste0("Wrong dimension(s) of data!\n `mat.species.DIST` does not have the same number of rows (",
                    nrow(mat.species.DIST),
                    ") and columns (",
                    ncol(mat.species.DIST),
                    ")"
        ))
      }
    } else {
      stop(paste0("Wrong type of data!\n `mat.species.DIST` must be a dissimilarity object"
                  , " (`dist`, `niolap`, `matrix`) or a list of dissimilarity objects"))
    }
    mat.species.DIST = list(mat.species.DIST)
    group_names = paste0("GROUP", 1:length(mat.species.DIST))
  }
  no_NA_values = sapply(mat.species.DIST, function(mat) sum(is.na(mat)))
  if (length(which(no_NA_values > 0)) > 0)
  {
    stop(paste0("Missing data!\n `mat.species.DIST` contain NA values ("
                , paste0(no_NA_values, collapse = ", ")
                , "), clustering with `hclust` function might have problems dealing with this data"))
  }
  
  #################################################################################################
  ### CLUSTERING
  #################################################################################################
  
  ## HOW TO CHOOSE the best clustering method (complete, ward, single, average) ?
  ## Measure of similarity between input distance (mat.species.DIST)
  ## and the one obtained with the clustering (clust.DIST)
  ## WHICH MUST BE MINIMIZED
  ## (Mouchet et al. 2008)
  
  avail.methods = c("complete", "ward.D", "ward.D2", "single",
                    "average", "mcquitty", "median", "centroid")
  clust.choice = foreach(clust.method = avail.methods) %do% {
    ## CALCULATE DENDOGRAMS from distance matrices
    clust.dendograms = lapply(mat.species.DIST, function(x) {
      hclust(as.dist(x), method = clust.method)
    })
    ## CALCULATE THE DISTANCES corresponding to these dendograms
    clust.DIST = lapply(clust.dendograms, cophenetic)
    
    ## CALCULATE Mouchet measure
    clust.choice = sapply(1:length(clust.DIST), function(x){
      return(1 - (cor(as.dist(clust.DIST[[x]]), as.dist(mat.species.DIST[[x]])) *
                    cor(as.dist(clust.DIST[[x]]), as.dist(mat.species.DIST[[x]]))))
    })
    
    return(data.frame(clust.method = clust.method
                      , group = group_names
                      , metric = clust.choice
                      , stringsAsFactors = FALSE))
  }
  clust.choice = do.call(rbind, clust.choice)
  if (length(group_names) == 1)
  {
    no_NA_values = length(which(is.na(clust.choice$metric)))
    no_NA_values = (no_NA_values == nrow(clust.choice))
  } else {
    no_NA_values = sapply(group_names, function(x) length(which(is.na(clust.choice$metric[which(clust.choice$group == x)]))))
    no_NA_values = (no_NA_values == sapply(group_names, function(x) length(which(clust.choice$group == x))))
    no_NA_values = (sum(no_NA_values) >= 1)
  }
  if (no_NA_values)
  {
    stop(paste0("All clustering methods (maybe for a specific group) give NA values for Mouchet measure.\n"
                , "Please check if you have sufficient values to run `hclust` function"))
  }
  
  ## GRAPHICAL REPRESENTATION
  pp1 = ggplot(clust.choice, aes_string(x = "group", y = "metric", group = "clust.method", lty = "clust.method")) +
    geom_line(lwd = 0.8) + geom_point() +
    geom_label(data = clust.choice[which(clust.choice$group == tail(levels(clust.choice$group),1)),],
               aes_string(label = "clust.method"), hjust = -0.1) +
    scale_linetype_discrete(guide = F) +
    labs(x="", y = "", title = "STEP A : Choice of clustering method",
         subtitle = "Similarity between input and clustering distances (must be minimized, Mouchet et al. 2008)\ndepending on clustering method.") +
    theme_fivethirtyeight() +
    theme(axis.ticks = element_blank()
          , axis.text.y = element_text(angle = 0)
          , panel.background = element_rect(fill = "transparent", colour = NA)
          , plot.background = element_rect(fill = "transparent", colour = NA)
          , legend.background = element_rect(fill = "transparent", colour = NA)
          , legend.box.background = element_rect(fill = "transparent", colour = NA)
          , legend.key = element_rect(fill = "transparent", colour = NA))
  
  plot(pp1)
  
  ## CHOICE OF CLUSTERING METHOD
  clust.method = sapply(split(clust.choice, clust.choice$group), function(x){
    x$clust.method[which.min(x$metric)]
  })
  clust.method = names(which.max(table(clust.method)))
  
  ## CALCULATE DENDOGRAMS from distance matrices
  clust.dendograms = lapply(mat.species.DIST, function(x) {
    hclust(as.dist(x), method = clust.method)
  })
  
  cat("\n ############## CLUSTERING ############## \n")
  cat("\n Clustering method : ", clust.method)
  cat("\n Clustering evaluation...")
  cat("\n")
  
  #################################################################################################
  ### EVALUATION OF CLUSTERING
  #################################################################################################
  
  ## COMPUTATION OF SEVERAL INDICES TO EVALUATE THE 'QUALITY' OF CLUSTERING
  ## Calculated for each group, and varying the number of clusters
  
  # min_no_species_in_group = min(sapply(mat.species.DIST, function(x) ncol(as.matrix(x))))
  # combi = expand.grid(nb.cluster = 2:(min_no_species_in_group - 1), group = 1:length(group_names))
  
  min_no_species_in_group = sapply(mat.species.DIST, function(x) ncol(as.matrix(x)))
  min_no_species_in_group = sapply(min_no_species_in_group, function(x) min(x, 15))
  combi = foreach(group = 1:length(group_names), .combine = "rbind") %do% {
    expand.grid(nb.cluster = 2:(min_no_species_in_group[group] - 1), group = group)
  }
  
  group = nb.cluster = NULL
  clust.evaluation = foreach(group = combi$group, nb.cluster = combi$nb.cluster) %do%
    {
      
      k1 = nb.cluster
      k2 = nb.cluster + 1
      c1 = cutree(clust.dendograms[[group]], k = k1)
      c2 = cutree(clust.dendograms[[group]], k = k2)
      stats = cluster.stats(mat.species.DIST[[group]], c1, c2)
      
      ## Dunn index : ratio of the smallest distance between observations
      ## not in the same cluster to the largest intra-cluster distance.
      ## Value between zero and infinity, and should be maximized.
      mdunn = dunn(mat.species.DIST[[group]], c1)
      
      ## Meila's VI index (Variation of Information) : measures the amount of information lost and gained in changing between 2 clusterings.
      ## Should be minimized (?)
      mVI = stats$vi
      
      ## Value between zero and one. Should be maximized.
      R2 = stats$average.between / (stats$average.between + stats$average.within)
      
      ## Calinski and Harabasz index : 
      ## The higher the value, the "better" is the solution.
      ch = stats$ch
      
      ## Corrected rand index : measure of the similarity between two data clusterings.
      ## Value between 0 and 1, with 0 indicating that the two data clusters do not agree
      ## on any pair of points and 1 indicating that the data clusters are exactly the same.
      Rand = stats$corrected.rand
      
      ## Average silhouette width :
      ## Observations with a large s(i) (almost 1) are very well clustered,
      ## a small s(i) (around 0) means that the observation lies between two clusters,
      ## and observations with a negative s(i) are probably placed in the wrong cluster.
      ## Should be maximized.
      av.sil = stats$avg.silwidth
      
      return(data.frame(group = group_names[group]
                        , nb.cluster, mdunn, mVI, R2, ch, Rand, av.sil
                        , stringsAsFactors = FALSE))
    }
  clust.evaluation = do.call(rbind, clust.evaluation)
  clust.evaluation = melt(clust.evaluation, id.vars = c("group","nb.cluster"))
  
  ## Find number of cluster which give optimal variable values
  combi = expand.grid(group = group_names, variable = unique(clust.evaluation$variable))
  
  group = variable = NULL
  clust.evaluation.optim = foreach(group = combi$group, variable = combi$variable) %do%
    {
      tmp = clust.evaluation[which(clust.evaluation$group == group & clust.evaluation$variable == variable),]
      if(variable == "mVI")
      {
        # ind.optim = which.min(tmp$value)
        optim = unique(sort(tmp$value, decreasing = F))[1:3]
        ind.optim = which(tmp$value %in% optim) #order(tmp$value, decreasing = F)[1:3]
      } else {
        # ind.optim = which.max(tmp$value)
        optim = unique(sort(tmp$value, decreasing = T))[1:3]
        ind.optim = which(tmp$value %in% optim) #order(tmp$value, decreasing = T)[1:3]
      }
      optim.clust = tmp$nb.cluster[ind.optim]
      optim.val = tmp$value[ind.optim]
      return(data.frame(group
                        , variable
                        , optim.clust
                        , optim.val
                        , stringsAsFactors = FALSE))
    }
  clust.evaluation.optim = do.call(rbind, clust.evaluation.optim)
  
  ## GRAPHICAL REPRESENTATION
  colRamp = colorRampPalette(c('#8e0152','#c51b7d','#de77ae','#7fbc41','#4d9221','#276419'))
  
  pp2 = ggplot(clust.evaluation, aes_string(x = "nb.cluster", y = "value")) +
    facet_grid("variable ~ group", scales = "free") +
    geom_line() + geom_point() +
    geom_vline(data = clust.evaluation.optim, aes_string(xintercept = "optim.clust", color = "group"), lwd = 4, alpha = 0.3) +
    # geom_point(data = clust.evaluation.optim, aes(x = optim.clust, y = optim.val),
    #            # pch = 1, lwd = 5, col = "darkred") +
    #            lwd = 3, col = "darkblue") +
    scale_color_manual(guide = F, values = colRamp(length(group_names))) +
    labs(x = "", y = "", title = "STEP B : Choice of number of clusters",
         subtitle = paste0("Evolution of clustering evaluation variables with the number of clusters in each group.\n",
                           "All values except that of mVI must be maximized (check function's help for more details about the measures).\n",
                           "The number of clusters with values among the 3 best are highlighted.")) +
    theme_fivethirtyeight() +
    theme(panel.background = element_rect(fill = "transparent", colour = NA)
          , plot.background = element_rect(fill = "transparent", colour = NA)
          , legend.background = element_rect(fill = "transparent", colour = NA)
          , legend.box.background = element_rect(fill = "transparent", colour = NA)
          , legend.key = element_rect(fill = "transparent", colour = NA))
  
  plot(pp2)
  
  ggsave(filename = "PRE_FATE_CLUSTERING_STEP_1A_clusteringMethod.pdf", plot = pp1, width = 8, height = 8)
  ggsave(filename = "PRE_FATE_CLUSTERING_STEP_1B_numberOfClusters.pdf", plot = pp2, width = 10, height = 8)
  
  return(list(clust.dendograms = clust.dendograms, clust.evaluation = clust.evaluation))
  
}

