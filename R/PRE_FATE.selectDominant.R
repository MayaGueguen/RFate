### HEADER #####################################################################
##' @title Selection of dominant species from abundance releves
##'
##' @name PRE_FATE.selectDominant
##'
##' @author Maya Guéguen
##' 
 # @date 15/03/2018
##' 
##' @description This script is designed to select dominant species from
##'              abundance records, and landclass if the information is available.
##'              
##' @param mat.site.species.abund a data.frame with at least 3 columns : sites ID, species ID, abundance values (e.g. Braun Blanquet) with NA when no information, landscape ID (optional)
##' @param sites a vector with sites ID
##' @param species a vector with species ID
##' @param abund a vector with abundance values (e.g. Braun Blanquet) with NA when no information
##' @param landclass a vector with landscape ID
##' 
##' @param selectionRule.quanti minimum quantile of total number of sites that must occupy the species (between 0 and 1)
##' @param selectionRule.min_mean_abund minimal average abundance of the species
##' @param selectionRule.min_no_high_abund minimum number of sites where species is dominant (>= 25 \% of coverage)
##' @param doLandclass default FALSE. If TRUE, selection is also done including constraints on landscape class
##' @param selectionRule.min_percent_landclass minimum percentage of the landclass occupied by the species (between 0 and 1)
##' @param selectionRule.min_no_landclass minimum number of sites of a landclass occupied by the species
##' 
##' @details 
##' 
##' This function provides a way to \emph{select dominant species based on abundance sampling information}.
##' This information can be given either directly in the form of a data.frame, or indirectly with a vector
##' for each required information (sites, species, abundance, landscape class - optional).
##' 
##' 
##' 3 rules are applied to make the species selection :
##' \describe{
##'   \item{\strong{on number of sites}}{the species should be found in at least \code{n} sites,
##'   which corresponds to the quantile \code{selectionRule.quanti} of all the number of records per species}
##'   \item{\strong{on average abundance}}{the species should have a mean abundance superior or equal to \code{selectionRule.min_mean_abund}}
##'   \item{\strong{on dominancy}}{the species should be dominant (i.e. represent at least 25 \% of the coverage of the site) 
##'   in at least \code{selectionRule.min_no_high_abund} sites}
##' }
##' 
##' If landscape information is available (e.g. type of environment : urban, desert, grassland... ;
##' type of vegetation : shrubs, forest, alpine grasslands... ; etc), 2 rules can be added to the selection steps,
##' \emph{in order to keep species that are not dominant at the large scale but could be representative of a specific environment} :
##' \describe{
##'   \item{\strong{on occupancy}}{the species should occupy at least \code{selectionRule.min_percent_landclass} \% of a landclass}
##'   \item{\strong{on representation}}{the species should be found in at least \code{selectionRule.min_no_landclass} sites of a landclass}
##' }
##' 
##' 
##' @return a data.frame with all the species selected and the values of parameters used to make the selection :
##' 
##' \item{stat.no_sites_recorded}{number of sites with information (presence-absence or abundance)}
##' \item{stat.no_sites_abund}{number of sites with abundance information}
##' \item{stat.abund_median}{median abundance (coverage percentage)}
##' \item{stat.abund_mean}{mean abundance (coverage percentage)}
##' \item{stat.abund_max}{maximal abundance (coverage percentage)}
##' \item{stat.no_sites_abund_max}{number of sites with maximal abundance}
##' \item{stat.no_sites_abund_over25}{number of sites with maximal abundance >= 25 \%}
##' 
##' The information is also written in .csv files :
##' 
##' \describe{
##'   \item{\file{DOMINANT_species_selected_COMPLETE_TABLE}}{the complete table of selected species with all the statistics described above}
##'   \item{\file{DOMINANT_species_selected_SPECIES_ONLY}}{only the names / ID of the species selected}
##' }
##' 
##' @keywords abundance, dominant species, qunatile, landscape class
##' 
##' @seealso \code{\link{PRE_FATE.abundBraunBlanquet}}
##' 
##' @examples
##' 
##' ## Load example data
##' data(MontBlanc)
##' str(MontBlanc)
##' 
##' ## MontBlanc$mat.releves : data.frame
##' 
##' ## Selection of dominant species
##' sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = MontBlanc$mat.releves)
##' 
##' ## 2 .csv files have been produced
##' str(sp.DOM)
##' 
##' @export
##' 
##' @importFrom graphics abline hist par plot text title
##' @importFrom stats median na.omit quantile
##' @importFrom utils write.csv
##' 
## END OF HEADER ###############################################################


PRE_FATE.selectDominant = function(mat.site.species.abund = NULL ## data.frame
                                   , sites = NULL ## vector
                                   , species = NULL ## vector
                                   , abund = NULL ## vector
                                   , landclass = NULL ## vector
                                   , selectionRule.quanti = 0 ## minimum quantile of total number of sites that must occupy the species
                                   , selectionRule.min_mean_abund = 0 ## minimum mean abundance of the species
                                   , selectionRule.min_no_high_abund = 0 ## minimum number of sites where species is dominant (>= 25 % of coverage)
                                   , doLandclass = F ## landclasses are given in the data.frame
                                   , selectionRule.min_percent_landclass = 0.05 ## minimum percentage of the landclass occupied by the species 
                                   , selectionRule.min_no_landclass = 5 ## minimum number of sites of the landclass occupied by the species
){
  
  #################################################################################################
  
  ## Check existence of parameters
  if (is.null(mat.site.species.abund) &&
      (is.null(sites) || is.null(species)))
  {
    stop("No data given!\n (neither `mat.site.species.abund` or separated `sites` / `species` information)")
  } else if (!is.null(mat.site.species.abund))
  {
    ## CASE 1 : Control form of parameters : mat.site.species.abund
    if (!is.data.frame(mat.site.species.abund))
    {
      
      stop("Wrong type of data!\n `mat.site.species.abund` must be a data.frame")
    }
    if (nrow(mat.site.species.abund) == 0 || ncol(mat.site.species.abund) < 3)
    {
      stop("Wrong dimension(s) of data!\n `mat.site.species.abund` does not have the appropriate number of rows (>0)
           or columns (sites, species, abund, landclass - optional)")
    }
    if (ncol(mat.site.species.abund) == 3)
    {
      if (sum(colnames(mat.site.species.abund) == c("sites", "species", "abund")) < 3)
      {
        colnames(mat.site.species.abund) = c("sites", "species", "abund")
      }
    } else if (ncol(mat.site.species.abund) == 4)
    {
      if (sum(colnames(mat.site.species.abund) == c("sites", "species", "abund", "landclass")) < 3)
      {
        colnames(mat.site.species.abund) = c("sites", "species", "abund", "landclass")
      }
    }
  } else if (is.null(mat.site.species.abund) &&
             (!is.null(sites) && !is.null(species) && !is.null(abund)))
  {
    ## CASE 2 : Control form of parameters : sites, species, abund, landclass - optional
    mat.site.species.abund = data.frame(sites = sites, species = species, abund = abund)
    if (!is.null(landclass))
    {
      mat.site.species.abund$landclass = landclass
    }
  }
  
  if( selectionRule.quanti < 0 || selectionRule.quanti > 1)
  {
    stop("Wrong data given!\n `selectionRule.quanti` must be between 0 and 1")
  }
  if( selectionRule.min_mean_abund < 0 || selectionRule.min_no_high_abund < 0 || selectionRule.min_no_landclass < 0)
  {
    stop("Wrong data given!\n `selectionRule.min_mean_abund`, `selectionRule.min_no_high_abund` and `selectionRule.min_no_landclass` must be >= 0")
  }
  if( selectionRule.min_percent_landclass < 0 || selectionRule.min_percent_landclass > 1)
  {
    stop("Wrong data given!\n `selectionRule.min_percent_landclass` must be between 0 and 1")
  }

  
  #################################################################################################
  ### PREPARATION OF DATA
  #################################################################################################
  
  no.releves = nrow(mat.site.species.abund)
  no.sites = length(unique(mat.site.species.abund$sites))
  no.species = length(unique(mat.site.species.abund$species))
  
  cat("\n ############## SAMPLING INFORMATIONS ############## \n")
  cat("\n Number of releves : ", no.releves)
  cat("\n Number of sites : ", no.sites)
  cat("\n Number of species : ", no.species)
  cat("\n")
  
  mat.site.species.abund$abund = as.numeric(as.character(mat.site.species.abund$abund))
  
  cat("\n ############## ABUNDANCE INFORMATIONS ############## \n")
  cat("\n Percentage of releves with abundance information : ", 100 * length(which(!is.na(mat.site.species.abund$abund))) / no.releves, "%")
  
  if (length(which(!is.na(mat.site.species.abund$abund))) / no.releves < 1)
  {
    warning("Data with NO abundance information will NOT be used for the dominant species selection...")
  }
  if (length(which(!is.na(mat.site.species.abund$abund))) / no.releves == 0)
  {
    stop("NO abundance information in your data. Dominant species selection can not be applied...")
  }
  
  cat("\n Percentage of sites with abundance information : ", 100 * length(unique(mat.site.species.abund$sites[which(!is.na(mat.site.species.abund$abund))])) / no.sites, "%")
  cat("\n Percentage of sites without abundance information : ", 100 * length(unique(mat.site.species.abund$sites[which(is.na(mat.site.species.abund$abund))])) / no.sites, "%")
  cat("\n Percentage of species with abundance information : ", 100 * length(unique(mat.site.species.abund$species[which(!is.na(mat.site.species.abund$abund))])) / no.species, "%")
  cat("\n Percentage of species without abundance information : ", 100 * length(unique(mat.site.species.abund$species[which(is.na(mat.site.species.abund$abund))])) / no.species, "%")
  cat("\n")
  
  if(doLandclass)
  {
    ## Calculate the number of sites in each landclass
    list.landclass.sites = split(mat.site.species.abund$sites, mat.site.species.abund$landclass)
    no_sites_habitat = sapply(list.landclass.sites, FUN=length)
    names(no_sites_habitat) = names(list.landclass.sites)
  } else
  {
    mat.site.species.abund$landclass = NA
  }
  
  cat("\n ############## STATISTICS COMPUTATION ############## \n")
  cat("\n For each species, calculate :")
  cat("\n     - stat.no_sites_abund = number of sites")
  cat("\n     - stat.abund_median = median abundance (coverage percentage)")
  cat("\n     - stat.abund_mean = mean abundance (coverage percentage)")
  cat("\n     - stat.abund_max = maximal abundance (coverage percentage)")
  cat("\n     - stat.no_sites_abund_max = number of sites with maximal abundance")
  cat("\n     - stat.no_sites_abund_over25 = number of sites with maximal abundance >= 25%")
  cat("\n  This is done over all sites, and for each landclass")
  cat("\n")
  
  categories = unique(mat.site.species.abund$landclass)
  if (length(categories) == 1 && is.na(categories[1])) {
    categories = "all"
  } else {
    categories = c("all", categories)
  }
  
  mat.species.stat.landclass = list()
  for(landclass_i in categories)
  {
    cat("\n> Landclass : ",landclass_i, "...")
    if (landclass_i == "all")
    {
      mat.site.species_landclass = mat.site.species.abund
    } else
    {
      mat.site.species_landclass = mat.site.species.abund[which(mat.site.species.abund$landclass == landclass_i),]
    }
    ## Split abundance data by species
    list.species = unique(mat.site.species_landclass$species)
    list.dat = split(mat.site.species_landclass$abund, mat.site.species_landclass$species)
    list.number = sapply(list.dat,length)
    list.dat = lapply(list.dat, na.omit)
    mat.species.stat = as.data.frame(t(sapply(list.dat, function(dat)
    {
      ## For each species :
      if (length(dat) > 0)
      {
        return(c(max(dat)
                 , length(which(dat == max(dat)))
                 , length(dat)
                 , length(dat[which(dat >= 25)])
                 , median(dat)
                 , mean(dat)))
      } else
      {
        return(rep(NA, 6))
      }
    })))
    colnames(mat.species.stat) = c("stat.abund_max"
                                   , "stat.no_sites_abund_max"
                                   , "stat.no_sites_abund"
                                   , "stat.no_sites_abund_over25"
                                   , "stat.abund_median"
                                   , "stat.abund_mean")
    mat.species.stat = data.frame(species = list.species
                                  , mat.species.stat
                                  , stat.no_sites_recorded = list.number)
    mat.species.stat.landclass[[landclass_i]] = mat.species.stat
  }
  cat("\n")
  

  #################################################################################################
  ### SELECTION OF DOMINANT SPECIES
  #################################################################################################
  
  cat("\n ############## SELECTION OF DOMINANT SPECIES ############## \n")
  
  ## OVER ALL SITES -------------------------------------------------------------------------------
  
  cat("\n> Over all sites...")
  mat.all = mat.species.stat.landclass[["all"]]
  
  ## Select species located above chosen quantile
  quant = quantile(mat.all$stat.no_sites_recorded, prob = selectionRule.quanti)
  
  ## GRAPHICS TO HELP ADJUST PARAMETERS TO SELECT DOMINANT SPECIES
  par(mfrow=c(2,2))
  plot(0, 0, col = "white", main = "", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  title(main = "Graphics to vizualise\n which species will be\n selected with given parameters :")
  text(0, 0, paste("USED CRITERIA for each species:\n"
                   , "Total number of presences :", selectionRule.quanti * 100, "%\n"
                   , "Mean abundance :", selectionRule.min_mean_abund, "\n"
                   , "Number of sites with abundance > 25 % :", selectionRule.min_no_high_abund))
  hist(
    mat.all$stat.no_sites_recorded,
    breaks = 100,
    main = "Histogram of\n total number of presences",
    xlab = "Total number of presences"
  )
  abline(v = quant, col = "red", lwd = 2)
  hist(
    mat.all$stat.abund_mean,
    breaks = 100,
    main = "Histogram of\n mean abundance of species",
    xlab = "Mean abundance of species"
  )
  abline(v = selectionRule.min_mean_abund, col = "red", lwd = 2)
  hist(
    mat.all$stat.no_sites_abund_over25,
    breaks = 100,
    main = "Histogram of\n number of sites with abundance > 25%",
    xlab = "Number of sites with abundance > 25%"
  )
  abline(v = selectionRule.min_no_high_abund, col = "red", lwd = 2)
  
  ## Select species that occur with a minimum number of sites, a minimum mean abundance and
  ## a minimum number of sites with high abundance (>= 25%)
  select_1 = which(mat.all$stat.no_sites_recorded >= quant)
  select_2 = which(mat.all$stat.abund_mean >= selectionRule.min_mean_abund)
  select_3 = which(mat.all$stat.no_sites_abund_over25 >= selectionRule.min_no_high_abund)
  select_all = union(select_1, intersect(select_2,select_3))
  if (length(select_all) > 0)
  {
    mat.select.species.all = mat.all[select_all, ]
  }
  
  ## FOR EACH LANDCLASS ---------------------------------------------------------------------------
  
  if(doLandclass)
  {
    cat("\n> Over each landclass...")
    
    ## Select species that occur on at least a certain percentage of landclass
    mat.select.species.landclass = data.frame()
    for(i in 2:length(mat.species.stat.landclass))
    {
      mat.land = mat.species.stat.landclass[[i]]
      ## Select species which are present in the landclass with a certain percentage
      select_1 = which(mat.land$stat.no_sites_recorded >= (no_sites_habitat[i] * selectionRule.min_percent_landclass))
      ## Select species which are present in the landclass with a minimum number of sites
      select_2 = which(mat.land$stat.no_sites_recorded >= selectionRule.min_no_landclass)
      select_12 = intersect(select_1,select_2)
      if(length(select_12)>0){
        mat.select.species.landclass = rbind(mat.select.species.landclass,
                                             data.frame(species = mat.land$species[intersect(select_1,select_2)],
                                                        landclass = names(mat.species.stat.landclass)[i]))
      }
    }
  }
  
  ## COMBINE SELECTIONS ----------------------------------------------------------
  
  cat("\n> Combine selections...")
  
  ## Get information about how each species has been selected
  ## (over all area, or within specific landclass and which one(s), or both)
  
  if(doLandclass)
  {
    selected.species = c(mat.select.species.all$species, mat.select.species.landclass$species)
    selected.species.doubled = intersect(mat.select.species.all$species, mat.select.species.landclass$species)
    selected.species.all = setdiff(mat.select.species.all$species, mat.select.species.landclass$species)
    selected.species.landclass = setdiff(mat.select.species.landclass$species, mat.select.species.all$species)
    
    mat.species.dominant = mat.all[which(mat.all$species %in% unique(selected.species)),]
    mat.species.dominant$SELECTION = NA
    for(i in 1:nrow(mat.species.dominant))
    {
      if (mat.species.dominant$species[i] %in% selected.species.all)
      {
        mat.species.dominant$SELECTION[i] = "all"
      } else if (mat.species.dominant$species[i] %in% selected.species.landclass)
      {
        ind_sp = which(mat.select.species.landclass == mat.species.dominant$species[i])
        mat.species.dominant$SELECTION[i] = paste0(mat.select.species.landclass$landclass[ind_sp]
                                                   , collapse = "_")
      } else if (mat.species.dominant$species[i] %in% selected.species.doubled)
      {
        ind_sp = which(mat.select.species.landclass == mat.species.dominant$species[i])
        mat.species.dominant$SELECTION[i] = paste0(c("all", mat.select.species.landclass$landclass[ind_sp])
                                                   , collapse = "_")
      }
    }
  } else {
    selected.species = mat.select.species.all$species
    
    mat.species.dominant = mat.all[which(mat.all$species %in% unique(selected.species)),]
    mat.species.dominant$SELECTION = "all"
  }
  
  write.csv(mat.species.dominant
            , file = "DOMINANT_species_selected_COMPLETE_TABLE.csv"
            , row.names = F)
  write.csv(mat.species.dominant$species
            , file = "DOMINANT_species_selected_SPECIES_ONLY.csv"
            , row.names = F)
  
  cat("\n> Done!\n")
  cat("\n ", length(unique(mat.species.dominant$species)), "species have been selected with the given criteria.")
  cat("\n  Complete table of information about selected species can be find in output files.")
  cat("\n")
  
  return(mat.species.dominant)
  
  #################################################################################################
}


#################################################################################################

# PRE_FATE.abundBraun-Blanquet = function(abund){
#   ## Convert Braun-Blanquet abundance classes into median coverage percentage
#   abund = as.character(abund)
#   abund[which(abund %in% c("+","r"))] = 0.5
#   abund[which(abund=="1")] = 3
#   abund[which(abund=="2")] = 15
#   abund[which(abund=="3")] = 37.5
#   abund[which(abund=="4")] = 62.5
#   abund[which(abund=="5")] = 87.5
#   abund = as.numeric(abund)
#   return(abund)
# }

# library(foreign)
# mat.site.species.abund = read.dbf(file="/home/gueguen/Documents/_PERSO/06_Cours/TUTOS/3_R/_DATA/Observations.dbf")
# mat.site.species.abund = mat.site.species.abund[,c("numchrono","numtaxon","codeabonda")]
# colnames(mat.site.species.abund) = c("sites","species","abund")
# mat.site.species.abund$abund = PRE_FATE.abundRaunkier(mat.site.species.abund$abund)
# # mat.site.species.abund$abund = NA
# 
# SELECTION = PRE_FATE.selectDominant(mat.site.species.abund = mat.site.species.abund)
# SELECTION = PRE_FATE.selectDominant(mat.site.species.abund = mat.site.species.abund
#                                     , selectionRule.min_mean_abund = 90
#                                     , selectionRule.min_no_high_abund = 5)





