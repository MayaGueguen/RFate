### HEADER #####################################################################
##' @title Computation of distances between species
##' based on traits and niche overlap
##' 
##' @name PRE_FATE.speciesDistance
##'
##' @author Maya Guéguen
##' 
 # @date 21/03/2018
##' 
##' @description This script is designed to create a distance matrix between
##' species, combining functional distances (based on functional trait values)
##' and niche overlap (based on co-occurrence of species). 
##'              
##' @param mat.species.traits A \code{data.frame} with at least 3 columns :
##' \describe{
##' \item{\code{species}}{the ID of each studied species}
##' \item{\code{GROUP}}{a factor variable containing grouping information to
##'   divide the species into sub data sets (see \code{Details})}
##' \item{\code{...}}{one column for each functional trait}
##' }
##' 
##' @param mat.species.overlap Two options :
##' \itemize{
  # \item a \code{data.frame} with a column for each species containing
  # presence-absence information (0 or 1) or probability values (between 0 and 1)
##'   \item a \code{data.frame} with 2 columns :
##'   \describe{
##'     \item{\code{species}}{the ID of each studied species}
##'     \item{\code{raster}}{path to raster file with species distribution}
##'   }
##'   \item a dissimilarity structure representing the niche overlap between
##'   each pair of species. It can be a \code{dist} object, a \code{niolap}
##'   object, or simply a \code{matrix}.
##' }
##' 
##' @param min.info.thresh minimum percentage of values for each trait (between
##' 0 and 1)
##' @param opt.traits.selection (\emph{optional}) \cr a \code{vector} containing
##' 2 values to select traits used to form species clusters : the maximum
##' percentage of null distances between pairs of species (between 0 and 1), and
##' the standard deviation (between 0 and 1)
##' 
##' @details 
##' 
##' This function allows one to obtain a distance matrix between species, based
##' on two types of distance information :
##' 
##' \enumerate{
##'   \item{\strong{Functional traits : }}{
##'   \itemize{
##'     \item The \code{GROUP} column is required if species must be separated
##'     to have one final distance matrix per \code{GROUP} value. If the column
##'     is missing, all species will be considered as part of a unique dataset.
##'     \item The traits can be qualitative or quantitative, but previously
##'     identified as such (i.e. with the use of functions such as
##'     \code{as.numeric}, \code{as.factor} and \code{ordered}).
##'     \item Functional distance matrix is calculated with Gower dissimilarity,
##'     using the \code{gowdis} function from \pkg{FD} package.
##'     \item This function allows \code{NA} values. In order to let the user
##'     decide what importance to give to missing values, the \code{min.info.thresh}
##'     defines which minimum percentage of species should have values for each trait. 
##'   }
##'   }
##'   \item{\strong{Niche overlap : }}{
##'   \itemize{
##'     \item If a \code{data.frame} is given, the degree of niche overlap will
##'     be computed using the \code{niche.overlap} function from \pkg{phyloclim}
##'     package.
##'   }
##'   }
##' }
##' 
##' Functional distances and niche overlap informations are then \strong{combined}
##' according to the following formula :
##' 
##' \deqn{\text{mat.DIST}_{sub-group} = [ \text{mat.OVERLAP}_{sub-group} +
##' \text{mat.FUNCTIONAL}_{sub-group} * n_{traits} ] / [ n_{traits} + 1 ]}
##' 
##' meaning that distance matrix obtained from functional information is
##' weighted by the number of traits used.
##' 
##' 
##' @return A \code{dist} object corresponding to the distance between each pair
##' of species, or a \code{list} of \code{dist} objects, one for each
##' \code{GROUP} value.
##'
##'  
##' @keywords Gower distance
##' 
##' @seealso \code{\link[FD]{gowdis}}
##' 
##' @examples
##' ## Load example data
##' data(MontBlanc)
##' str(MontBlanc)
##' 
##' ## MontBlanc$mat.traits : data.frame
##' ## MontBlanc$mat.nicheOverlap : niolap object
##' # sp.DIST = PRE_FATE.speciesDistance(mat.species.traits = MontBlanc$mat.traits
##' #                                    , mat.species.overlap = MontBlanc$mat.nicheOverlap
##' #                                    , min.info.thresh = 1)
##'                                    
##' sp.DIST = PRE_FATE.speciesDistance(mat.species.traits = MontBlanc$mat.traits
##'                                    , mat.species.overlap = MontBlanc$mat.nicheOverlap
##'                                    , min.info.thresh = 0.9)
##' 
##' str(sp.DIST)
##' 
##' @export
##' 
##' @importFrom stats as.dist na.exclude var
##' 
##' @importFrom raster raster
##' @importFrom phyloclim niche.overlap
##' @importFrom FD gowdis
##' 
## END OF HEADER ###############################################################


PRE_FATE.speciesDistance = function(mat.species.traits ## data.frame with columns : species, GROUP and one for each trait
                                    , mat.species.overlap ## species x species matrix / or table with raster file names
                                    , min.info.thresh = 1
                                    , opt.traits.selection = c(0.25, 0.30)
){
  
  #################################################################################################
  
  ## Check existence of parameters
  if (missing(mat.species.traits) || missing(mat.species.overlap) ||
      is.null(mat.species.traits) || is.null(mat.species.overlap))
  {
    stop("No data given!\n (missing `mat.species.traits` or `mat.species.overlap` information)")
  }
  ## Control form of parameters : min.info.thresh
  if (!is.numeric(min.info.thresh) || min.info.thresh < 0 || min.info.thresh > 1)
  {
    stop("Wrong type of data!\n `min.info.thresh` must be a number between 0 and 1")
  }
  ## Control form of parameters : mat.species.traits
  if (!is.data.frame(mat.species.traits))
  {
    .stopMessage_beDataframe("mat.species.traits")
  }
  if (ncol(mat.species.traits) <= 2 )
  {
    stop("Wrong dimension(s) of data!\n `mat.species.traits` does not have the appropriate number of cols (>=3, at least 2 traits)")
  }
  if (sum(colnames(mat.species.traits) == "species") != 1)
  {
    stop("Wrong data given!\n `mat.species.traits` must contain a column whose name is `species`")
  }
  if (sum(colnames(mat.species.traits) == "GROUP") == 0)
  {
    warning("`mat.species.traits` does not contain any column with `GROUP` information\n
            Data will be considered as one unique dataset.")
    mat.species.traits$GROUP = "AllSpecies"
  }
  if (nrow(mat.species.traits) <= 1)
  {
    stop("Wrong dimension(s) of data!\n `mat.species.traits` does not have the appropriate number of rows (>=2)")
  }
  ## Control form of parameters : mat.species.overlap
  if (class(mat.species.overlap) %in% c("dist", "niolap"))
  {
    mat.species.overlap = as.matrix(mat.species.overlap)
  } else if (is.matrix(mat.species.overlap))
  {
    if (ncol(mat.species.overlap) != nrow(mat.species.overlap))
    {
      stop(paste0("Wrong dimension(s) of data!\n `mat.species.overlap` does not have the same number of rows ("
                  ,nrow(mat.species.overlap)
                  ,") and columns ("
                  ,ncol(mat.species.overlap)
                  ,")"))
    }
  } else if (is.data.frame(mat.species.overlap))
  {
    # if (length(which(mat.species.overlap < 0)) > 0 || length(which(mat.species.overlap > 1)) > 0)
    # {
    #   stop("Wrong data given!\n `mat.species.overlap` must contain values between 0 and 1
    #        (either presence-absence or probability values)")
    # }
    if (sum(colnames(mat.species.overlap) == "species") != 1)
    {
      stop("Wrong data given!\n `mat.species.overlap` must contain a column whose name is `species`")
    } else if (sum(colnames(mat.species.overlap) == "raster") != 1)
    {
      stop("Wrong data given!\n `mat.species.overlap` must contain a column whose name is `raster`")
    } else if (sum(file.exists(as.character(mat.species.overlap$raster))) < nrow(mat.species.overlap))
    {
      stop("Wrong data given!\n `mat.species.overlap$raster` must contain file names which exist")
    }
    mat.species.overlap = mat.species.overlap[which(file.exists(mat.species.overlap$raster)), ]
    raster.list = lapply(mat.species.overlap$raster, function(x) as(raster(x), "SpatialGridDataFrame"))
    overlap.mat = as.matrix(niche.overlap(raster.list))
    rownames(overlap.mat) = colnames(overlap.mat) = mat.species.overlap$species
    mat.species.overlap = overlap.mat
  } else {
    stop("Wrong type of data!\n `mat.species.overlap` must be either a data.frame or a dissimilarity object (`dist`, `niolap`, `matrix`)")
  }
  
  
  #################################################################################################
  ### PREPARATION OF DATA
  #################################################################################################
  
  ## TRAITS ------------------------------------------------------------------------------------- #
  rownames(mat.species.traits) = mat.species.traits$species
  species_names.traits = sort(unique(as.character(mat.species.traits$species)))
  traits_names = colnames(mat.species.traits)[which(!(colnames(mat.species.traits) %in% c("species","GROUP")))]
  group_names = sort(unique(as.character(mat.species.traits$GROUP)))
  mat.species.traits$GROUP = factor(mat.species.traits$GROUP, group_names)
  no_NA_values = apply(as.matrix(mat.species.traits[,traits_names]), 2, function(x) sum(is.na(x)))
  no_NA_values = no_NA_values / nrow(mat.species.traits)
  ind_NA_values = which(no_NA_values > (1 - min.info.thresh))
  if (length(ind_NA_values) > 0)
  {
    stop(paste0("Missing data!\n `mat.species.traits` contain trait with too many missing values : "
                , paste0(round(no_NA_values[ind_NA_values], 4) * 100, " % for ", traits_names[ind_NA_values]), "\n"))
  }
  
  # no_NA_values = apply(as.matrix(mat.species.traits[,traits_names]), 1, function(x) sum(is.na(x)))
  # ind_NA_values = which(no_NA_values >= length(traits_names) - 1)
  # if (length(ind_NA_values) > 0)
  # {
  #   mat.species.traits = mat.species.traits[-ind_NA_values, ]
  #   # warning(paste0("Missing data!\n `mat.species.traits` contain trait with too many missing values : "
  #               # , paste0(round(no_NA_values[ind_NA_values], 4) * 100, " % for ", traits_names[ind_NA_values]), "\n"))
  # }
  
  ## SPLIT INFORMATION by species type
  species.split = split(as.character(mat.species.traits$species), f = mat.species.traits$GROUP)
  
  ## OVERLAP ------------------------------------------------------------------------------------ #
  species_names.overlap = sort(unique(as.character(colnames(mat.species.overlap))))
  
  ## SPLIT INFORMATION by species type
  mat.species.overlap.split = lapply(species.split, function(x) {
    ind = which(rownames(mat.species.overlap) %in% x)
    return(mat.species.overlap[ind, ind])
  })
  
  ## Transform into similarity distances (instead of dissimilarity)
  mat.species.overlap.split = lapply(mat.species.overlap.split, function(x) {
    return(as.dist(1 - x)) ## 1- (x/max(x[upper.tri(x)]))
  })
  
  ## TRAITS & OVERLAP --------------------------------------------------------------------------- #
  
  ## Check for correspondence :
  cat("\n Number of species with traits : ", length(species_names.traits))
  cat("\n Number of species with traits and no overlap information : "
      , length(setdiff(species_names.traits, species_names.overlap)))
  cat("\n Number of species with overlap : ", length(species_names.overlap))
  cat("\n Number of species with overlap and no traits information : "
      , length(setdiff(species_names.overlap, species_names.traits)))
  cat("\n")
  
  ## Check for correspondence : DIM mat.species.gower.split = DIM mat.species.overlap.split ?
  cat("\n Comparison of groups' dimensions : \n")
  for(x in 1:length(group_names)){
    cat("\n Group ", x, ":\n")
    cat("Trait distances : ", length(species.split[[x]]), "\n")
    cat("Overlap distances : ", dim(as.matrix(mat.species.overlap.split[[x]])), "\n")
  }
  species_names.traits_overlap = intersect(species_names.traits, species_names.overlap)
  cat("\n Number of species with both trait and overlap distances: ", length(species_names.traits_overlap))
  cat("\n")
  
  # Keep only species present in both distance matrices (trait & overlap)
  mat.species.traits = mat.species.traits[which(mat.species.traits$species %in% species_names.traits_overlap), ]

  
  #################################################################################################
  ### CALCULATE TRAITS DISTANCES
  #################################################################################################
  
  ## CHOOSE which traits to keep by species type
  traits_toKeep = foreach(tr = traits_names, .combine = "rbind") %do%
  {
    mat.species.traits.split = split(mat.species.traits[, tr, drop = FALSE], f = mat.species.traits$GROUP)
    mat.species.gower.split = lapply(mat.species.traits.split, FD::gowdis)
    res = foreach(x = names(mat.species.gower.split), .combine = "rbind") %do%
    {
      mat = as.matrix(mat.species.gower.split[[x]])
      mat[upper.tri(mat, diag = TRUE)] = NA
      mat = as.vector(mat)
      std.dev = sqrt(var(na.exclude(mat)))
      percent.0 = length(which(mat == 0)) / length(which(!is.na(mat)))
      return(data.frame(GROUP = x, TRAIT = tr, std.dev, percent.0))
    }
    return(res)
  }
  traits_toKeep$toKeep1 = (traits_toKeep$percent.0 < opt.traits.selection[1])
  traits_toKeep$toKeep2 = (traits_toKeep$std.dev > opt.traits.selection[2])
  traits_toKeep$toKeep = ifelse(traits_toKeep$toKeep1 == TRUE
                                , TRUE
                                , ifelse(traits_toKeep$toKeep2 == TRUE, TRUE, FALSE))
  # traits_toKeep$toKeep = ifelse((traits_toKeep$toKeep1 + traits_toKeep$toKeep2) > 0, TRUE, FALSE)
  # traits_toKeep$toKeep = ifelse((traits_toKeep$toKeep1 + traits_toKeep$toKeep2) == 2, TRUE, FALSE)
  
  ## SPLIT INFORMATION by species type
  cat("\n Traits used to calculate functional distances : \n")
  mat.species.traits.split = split(mat.species.traits[,traits_names, drop = FALSE], f = mat.species.traits$GROUP)
  for (gp in 1:length(mat.species.traits.split))
  {
    tmp = traits_toKeep[which(traits_toKeep$GROUP == names(mat.species.traits.split)[gp]), ]
    tmp = tmp$TRAIT[which(tmp$toKeep == TRUE)]
    mat.species.traits.split[[gp]] = mat.species.traits.split[[gp]][, tmp, drop = FALSE]
    cat("\n Group ", names(mat.species.traits.split)[gp], ":\n")
    cat("Traits : ", as.character(tmp), "\n")
  }

  ## GOWER DISSIMILARITY FOR MIXED VARIABLES
  mat.species.gower.split = lapply(mat.species.traits.split, FD::gowdis)
  
  for (gp in 1:length(mat.species.gower.split))
  {
    if (length(which(is.na(as.matrix(mat.species.gower.split[[gp]])))) > 0)
    {
      ## remove NA values
      mat.species.gower.split[[gp]] = as.matrix(mat.species.gower.split[[gp]])
      nn = apply(mat.species.gower.split[[gp]], 2, function(x) length(which(is.na(x))))
      nn = which(nn == 0)
      mat.species.gower.split[[gp]] = mat.species.gower.split[[gp]][nn, nn]
      mat.species.gower.split[[gp]] = as.dist(mat.species.gower.split[[gp]])
    }
  }
  species.split = lapply(mat.species.gower.split, function(x) colnames(as.matrix(x)))
  
  # Keep only species present in both distance matrices (trait & overlap)
  species_names.traits_overlap = intersect(unlist(species.split), species_names.overlap)
  
  mat.species.overlap.split = lapply(1:length(group_names), function(x) {
    tmp = as.matrix(mat.species.overlap.split[[x]])
    ind = which(colnames(tmp) %in% species_names.traits_overlap)
    return(as.dist(tmp[ind, ind]))
  })
  
  cat("\n ############## TRAIT INFORMATIONS ############## \n")
  cat("\n Number of species : ", nrow(mat.species.traits))
  cat("\n Measured traits : ", paste0(traits_names, collapse = ", "))
  cat("\n Groups : ", paste0(group_names, collapse = ", "))
  cat("\n Number of NA values due to `gowdis` function : ", nrow(mat.species.traits) - sum(sapply(species.split, length)))
  cat("\n Number of species in each group : ", sapply(species.split, length))
  cat("\n")
  
  
  #################################################################################################
  ### COMBINE TRAITS & OVERLAP DISTANCES
  #################################################################################################
  
  ## ADD OVERLAP as PART OF THE DISTANCE BETWEEN SPECIES
  ## 1 PART for each trait (Disp, Light, Height, Palatability...)
  ## 1 PART for climatic distance between species (overlap)
  
  ## COMBINE TRAIT & OVERLAP DISTANCES
  mat.species.DIST = lapply(1:length(group_names), function(x) {
    tmp.gower = as.matrix(mat.species.gower.split[[x]])
    tmp.overlap = as.matrix(mat.species.overlap.split[[x]])
    n.traits = ncol(mat.species.traits.split[[x]])
    mat = (tmp.overlap + n.traits * tmp.gower) / (n.traits + 1)
    return(as.dist(mat))
  })
  names(mat.species.DIST) = group_names
  
  if(length(mat.species.DIST) == 1)
  {
    mat.species.DIST = mat.species.DIST[[1]]
  }
  
  return(mat.species.DIST)
  
}

