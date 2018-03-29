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
##' @param mat.site.species.abund a \code{data.frame} with at least 3 columns :
##' sites ID, species ID, abundance values (e.g. Braun Blanquet) with \code{NA} when
##' no information, landscape ID (optional)
##' @param sites a \code{vector} with sites ID
##' @param species a \code{vector} with species ID
##' @param abund a \code{vector} with abundance values (e.g. Braun Blanquet) with \code{NA}
##' when no information
##' @param landclass a \code{vector} with landscape ID
##' 
##' @param selectionRule.quanti minimum quantile of total number of sites that
##' must occupy the species (between 0 and 1)
##' @param selectionRule.min_mean_abund minimal average abundance of the species
##' @param selectionRule.min_no_high_abund minimum number of sites where species
##' is dominant (>= 25 \% of coverage)
##' @param doLandclass default \code{FALSE}. If \code{TRUE}, selection is also done including
##' constraints on landscape class
##' @param selectionRule.min_percent_landclass minimum percentage of the landclass
##' occupied by the species (between 0 and 1)
##' @param selectionRule.min_no_landclass minimum number of sites of a landclass
##' occupied by the species
##' 
##' @details 
##' 
##' This function provides a way to \emph{select dominant species based on
##' abundance sampling information}.
##' This information can be given either directly in the form of a \code{data.frame},
##' or indirectly with a \code{vector} for each required information (sites,
##' species, abundance, landscape class - optional).
##' 
##' 
##' 2 rules are applied to make the species selection :
##' 
##' \describe{
##'   \item{\strong{1. Presence releves}}{
##'   \describe{
##'     \item{\strong{on number of sites}}{the species should be found in at
##'     least \code{n} sites, which corresponds to the quantile \code{selectionRule.quanti}
##'     of all the number of records per species}
##'   }
##'   }
##'   \item{\strong{2. Abundance releves : }}{both conditions must be fullfilled
##'   \describe{
##'     \item{\strong{on average abundance}}{the species should have a mean
##'     abundance superior or equal to \code{selectionRule.min_mean_abund}}
##'     \item{\strong{on dominancy}}{the species should be dominant (i.e. represent
##'     at least 25 \% of the coverage of the site) in at least
##'     \code{selectionRule.min_no_high_abund} sites}
##'   }
##'   }
##' }
##' 
##' If landscape information is available (e.g. type of environment : urban,
##' desert, grassland... ; type of vegetation : shrubs, forest, alpine grasslands... ;
##' etc), 1 rule with 2 conditions can be added to the selection steps,
##' \emph{in order to keep species that are not dominant at the large scale
##' but could be representative of a specific environment} :
##' 
##' \describe{
##'   \item{\strong{3. Releves per habitat : }}{both conditions must be fullfilled
##'   \describe{
##'     \item{\strong{on occupancy}}{the species should occupy at least
##'     \code{selectionRule.min_percent_landclass} \% of a landclass}
##'     \item{\strong{on representation}}{the species should be found in at least
##'     \code{selectionRule.min_no_landclass} sites of a landclass}
##'   }
##'   }
##' }
##'   
##' @return A \code{data.frame} with all the species selected and the values of
##' parameters used to make the selection :
##' 
##' \describe{
##'   \item{stat.no_sites_recorded}{number of sites with information
##'   (presence-absence or abundance)}
##'   \item{stat.no_sites_abund}{number of sites with abundance information}
##'   \item{stat.abund_median}{median abundance (coverage percentage)}
##'   \item{stat.abund_mean}{mean abundance (coverage percentage)}
##'   \item{stat.abund_max}{maximal abundance (coverage percentage)}
##'   \item{stat.no_sites_abund_max}{number of sites with maximal abundance}
##'   \item{stat.no_sites_abund_over25}{number of sites with maximal abundance >= 25}
##'   \item{SELECTION}{dataset in which the species has been selected as dominant
##'   \cr (global = all data, or landclass)}
##' }
##' 
##\cr
##' 
##' The information is written in \code{PRE_FATE_DOMINANT_species_selected_[...].csv} files :
##' \describe{
##'   \item{\file{COMPLETE_TABLE}}{the complete
##'   table of selected species with all the statistics described above}
##'   \item{\file{SPECIES_ONLY}}{only the names /
##'   ID of the species selected}
##' }
##' 
##' Two \code{PRE_FATE_DOMINANT_[...].pdf} files are also created : 
##' \describe{
##'   \item{\file{STEP_1_selectionCriteria}}{to visualize the values of
##'   species metrics, and the criteria used to make the selection}
##'   \item{\file{STEP_2_selectedSpecies}}{to visualize the values of
##'   species metrics, for the selected dominant species}
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
##' ## Transformation of Braun-Blanquet abundances
##' MontBlanc$mat.releves$abund = PRE_FATE.abundBraunBlanquet(abund = MontBlanc$mat.releves$abund)
##' 
##' ## Selection of dominant species
##' 
##' #########################################################################################
##' ## EXAMPLE 1 : With default parametrization
##' #########################################################################################
##' 
##' sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = MontBlanc$mat.releves)
##' 
##' #########################################################################################
##' ## EXAMPLE 2 : Redefinition of global selection rules
##' #########################################################################################
##' 
##' sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = MontBlanc$mat.releves
##'                                  , selectionRule.quanti = 0.9
##'                                  , selectionRule.min_mean_abund = 25
##'                                  , selectionRule.min_no_high_abund = 10)
##' 
##' #########################################################################################
##' ## EXAMPLE 3 : Add landclass selection with default parametrization
##' #########################################################################################
##' 
##' sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = MontBlanc$mat.releves
##'                                  , selectionRule.quanti = 0.9
##'                                  , selectionRule.min_mean_abund = 25
##'                                  , selectionRule.min_no_high_abund = 10
##'                                  , doLandclass = TRUE)
##'                           
##' #########################################################################################       
##' ## EXAMPLE 4 : Redefinition of landclass selection rules
##' #########################################################################################
##' 
##' sp.DOM = PRE_FATE.selectDominant(mat.site.species.abund = MontBlanc$mat.releves
##'                                  , selectionRule.quanti = 0.9
##'                                  , selectionRule.min_mean_abund = 25
##'                                  , selectionRule.min_no_high_abund = 10
##'                                  , doLandclass = TRUE
##'                                  , selectionRule.min_percent_landclass = 0.05
##'                                  , selectionRule.min_no_landclass = 10)
##'                                  
##' #########################################################################################
##' 
##' ## 2 .csv and 2 .pdf files have been produced
##' str(sp.DOM)
##' 
##' @export
##' 
##' @importFrom grDevices colorRampPalette
##' @importFrom stats median na.omit quantile
##' @importFrom utils write.csv
##' 
##' @importFrom reshape2 melt
##' @importFrom ggplot2 ggplot aes aes_string ggsave
##' geom_line geom_point geom_vline geom_label geom_histogram
##' element_text element_blank
##' as_labeller facet_wrap facet_grid
##' scale_color_manual scale_fill_manual scale_linetype_discrete
##' labs theme
##' @importFrom ggthemes theme_fivethirtyeight
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
                                   , doLandclass = FALSE ## landclasses are given in the data.frame
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
  
  mat.site.species.abund$species = as.character(mat.site.species.abund$species)
  mat.site.species.abund$abund = as.numeric(as.character(mat.site.species.abund$abund))
  
  cat("\n ############## ABUNDANCE INFORMATIONS ############## \n")
  cat("\n Percentage of releves with abundance information : ", 100 * length(which(!is.na(mat.site.species.abund$abund))) / no.releves, "%")
  
  if (length(which(!is.na(mat.site.species.abund$abund))) / no.releves < 1)
  {
    warning("Species with NO abundance information can only be selected with the criteria based on number of presences...")
  }
  if (length(which(!is.na(mat.site.species.abund$abund))) / no.releves == 0)
  {
    warning("NO abundance information in your data. Dominant species selection will only be done with the criteria based on number of presences...")
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
    if(!is.na(landclass_i))
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
      list.dat = split(mat.site.species_landclass$abund, mat.site.species_landclass$species)
      list.number = sapply(list.dat,length)
      mat.species.stat = as.data.frame(t(sapply(list.dat, function(dat)
      {
        dat = na.omit(dat)
        col.names = c("stat.abund_max"
                      , "stat.no_sites_abund_max"
                      , "stat.no_sites_abund"
                      , "stat.no_sites_abund_over25"
                      , "stat.abund_median"
                      , "stat.abund_mean")
        ## For each species :
        if (length(dat) > 0)
        {
          res = c(max(dat)
                  , length(which(dat == max(dat)))
                  , length(dat)
                  , length(dat[which(dat >= 25)])
                  , median(dat)
                  , mean(dat))
        } else
        {
          res = rep(NA, 6)
        }
        names(res) = col.names
        return(res)
      })))
      mat.species.stat = data.frame(species = names(list.dat)
                                    , mat.species.stat
                                    , stat.no_sites_recorded = list.number)
      mat.species.stat.landclass[[landclass_i]] = mat.species.stat
    }
  }
  cat("\n")
  

  #################################################################################################
  ### SELECTION OF DOMINANT SPECIES
  #################################################################################################
  
  cat("\n ############## SELECTION OF DOMINANT SPECIES ############## \n")
  
  ## OVER ALL SITES -------------------------------------------------------------------------------
  
  cat("\n> Over all sites...")
  mat.all = mat.species.stat.landclass[["all"]]
  mat.all$species = as.character(mat.all$species)
  
  ## Select species located above chosen quantile
  quant = quantile(mat.all$stat.no_sites_recorded, prob = selectionRule.quanti)
  
  ## Select species that occur with a minimum number of sites, a minimum mean abundance and
  ## a minimum number of sites with high abundance (>= 25%)
  select_1 = which(mat.all$stat.no_sites_recorded >= quant)
  select_2 = which(mat.all$stat.abund_mean >= selectionRule.min_mean_abund)
  select_3 = which(mat.all$stat.no_sites_abund_over25 >= selectionRule.min_no_high_abund)
  select_23 = intersect(select_2,select_3)
  select_all = union(select_1, select_23)
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
      mat.land$species = as.character(mat.land$species)
      ## Select species which are present in the landclass with a certain percentage
      select_1 = which(mat.land$stat.no_sites_recorded >= (no_sites_habitat[i] * selectionRule.min_percent_landclass))
      ## Select species which are present in the landclass with a minimum number of sites
      select_2 = which(mat.land$stat.no_sites_recorded >= selectionRule.min_no_landclass)
      select_12 = intersect(select_1,select_2)
      if (length(select_12) > 0)
      {
        mat.select.species.landclass = rbind(mat.select.species.landclass,
                                             data.frame(species = mat.land$species[select_12],
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
  
  #################################################################################################
  write.csv(mat.species.dominant
            , file = "PRE_FATE_DOMINANT_species_selected_COMPLETE_TABLE.csv"
            , row.names = F)
  write.csv(mat.species.dominant$species
            , file = "PRE_FATE_DOMINANT_species_selected_SPECIES_ONLY.csv"
            , row.names = F)
  
  
  #################################################################################################
  ## GRAPHICS TO HELP ADJUST PARAMETERS TO SELECT DOMINANT SPECIES
  #################################################################################################

  colRamp = colorRampPalette(c('#8e0152','#c51b7d','#de77ae','#7fbc41','#4d9221','#276419'))
  
  variables.labeller = c("stat.no_sites_recorded" = "stat.no_sites_recorded"
                         , "Fake1" = "", "Fake2" = ""
                         , "stat.abund_median" = "stat.abund_median"
                         , "stat.abund_mean" = "stat.abund_mean"
                         , "stat.abund_max" = "stat.abund_max"
                         , "stat.no_sites_abund" = "stat.no_sites_abund"
                         , "stat.no_sites_abund_max" = "stat.no_sites_abund_max"
                         , "stat.no_sites_abund_over25" = "stat.no_sites_abund_over25"
  )
  
  ## STEP 1 : Selection -----------------------------------------------------------
  
  # tmp = mat.species.stat.landclass#[-1]
  # tmp = lapply(1:length(tmp), function(x) data.frame(tmp[[x]], landclass = names(tmp)[x]))
  # tmp = do.call(rbind, tmp)
  
  tmp = mat.all
  tmp$landclass = "all"
  
  mat.plot = melt(tmp, id.vars = c("species","landclass"))
  mat.plot = rbind(mat.plot, data.frame(species = "SP.ghost", landclass = NA, variable = c("Fake1","Fake2"), value = NA))
  mat.plot$variable = factor(mat.plot$variable, c("stat.no_sites_recorded"
                                                  , "Fake1", "Fake2"
                                                  , "stat.abund_median"
                                                  , "stat.abund_mean"
                                                  , "stat.abund_max"
                                                  , "stat.no_sites_abund"
                                                  , "stat.no_sites_abund_max"
                                                  , "stat.no_sites_abund_over25"
  ))
  mat.plot$landclass = as.character(mat.plot$landclass)
  mat.plot$landclass[which(mat.plot$landclass == "all")] = NA
  mat.plot$limit = NA
  mat.plot$limit[which(mat.plot$variable == "stat.no_sites_recorded")] = quant
  mat.plot$limit[which(mat.plot$variable == "stat.abund_mean")] = selectionRule.min_mean_abund
  mat.plot$limit[which(mat.plot$variable == "stat.no_sites_abund_over25")] = selectionRule.min_no_high_abund
  
  pp1 = ggplot(mat.plot, aes_string(x = "value")) +
    geom_histogram(na.rm = T) +
    # geom_histogram(aes(x = value, fill = landclass)) +
    # geom_density(aes(x = value, ..count.., group = landclass)) +
    # scale_color_discrete("Landclass", na.value = NA, breaks = levels(factor(mat.plot$landclass))) +
    geom_vline(aes_string(xintercept = "limit"), lwd = 1, color = "#fb6a4a", na.rm = TRUE) +
    facet_wrap("variable", scales="free", labeller = as_labeller(variables.labeller)) +
    labs(x="", y = "", title = "STEP 1 : Selection of dominant species",
         subtitle = paste0("Criteria used for each species (highlighted with colored vertical lines) :\n"
                           , "Total number of presences >= quantile( ", selectionRule.quanti * 100, "% )\n"
                           , "Mean abundance >= ", selectionRule.min_mean_abund, "\n"
                           , "Number of releves with (abundance > 25) >= ", selectionRule.min_no_high_abund
                           , "\n")) +
    theme_fivethirtyeight() +
    theme(legend.position = c(0.7, 0.85),
          legend.title = element_text(size=10))
  
  ggsave(filename = "PRE_FATE_DOMINANT_STEP_1_selectionCriteria.pdf", plot = pp1, width = 10, height = 8)
  
  ## STEP 2 : Selected -----------------------------------------------------------

  mat.plot = melt(mat.species.dominant, id.vars = c("species","SELECTION"))
  mat.plot = rbind(mat.plot, data.frame(species = "SP.ghost", SELECTION = NA, 
                                        variable = c("Fake1","Fake2"), value = NA))
  mat.plot$variable = factor(mat.plot$variable, c("stat.no_sites_recorded"
                                                  , "Fake1", "Fake2"
                                                  , "stat.abund_median"
                                                  , "stat.abund_mean"
                                                  , "stat.abund_max"
                                                  , "stat.no_sites_abund"
                                                  , "stat.no_sites_abund_max"
                                                  , "stat.no_sites_abund_over25"
  ))
  mat.plot$SELECTION = sub("all_","global + landclass ",mat.plot$SELECTION)
  mat.plot$SELECTION = sub("all","global",mat.plot$SELECTION)
  mat.plot$limit = NA
  mat.plot$limit[which(mat.plot$variable == "stat.no_sites_recorded")] = quant
  mat.plot$limit[which(mat.plot$variable == "stat.abund_mean")] = selectionRule.min_mean_abund
  mat.plot$limit[which(mat.plot$variable == "stat.no_sites_abund_over25")] = selectionRule.min_no_high_abund
  
  if(length(na.exclude(unique(mat.plot$SELECTION))) == 1) {
    colos = "grey35"
  } else {
    colos = colRamp(length(na.exclude(unique(mat.plot$SELECTION))))
  }
  
  pp2 = ggplot(mat.plot, aes_string(x = "value", fill = "SELECTION")) +
    geom_histogram(position = "dodge", na.rm = TRUE) +
    geom_vline(aes_string(xintercept = "limit"), lwd = 1, color = "#fb6a4a", na.rm = TRUE) +
    scale_fill_manual("Species selected in dataset :", values = colos) +
    facet_wrap("variable", scales="free", labeller = as_labeller(variables.labeller)) +
    labs(x="", y = "", title = paste0("STEP 2 : selected dominant species (", nrow(mat.plot),")"),
         subtitle = paste0("Criteria used for each species (highlighted with colored vertical lines) :\n"
                           , "Total number of presences >= quantile( ", selectionRule.quanti * 100, "% )\n"
                           , "Mean abundance >= ", selectionRule.min_mean_abund, "\n"
                           , "Number of releves with (abundance > 25) >= ", selectionRule.min_no_high_abund
                           , "\n")) +
    theme_fivethirtyeight() +
    theme(legend.position = c(0.7, 1),
          legend.title = element_text(size=10),
          legend.direction = "vertical")
  
  ggsave(filename = "PRE_FATE_DOMINANT_STEP_2_selectedSpecies.pdf", plot = pp2, width = 10, height = 8)

  #################################################################################################
  
  cat("\n> Done!\n")
  cat("\n ", length(unique(mat.species.dominant$species)), "species have been selected with the given criteria.")
  cat("\n  Complete table of information about selected species can be find in output files.")
  cat("\n")
  
  suppressMessages(plot(pp1))
  suppressMessages(plot(pp2))
  
  return(mat.species.dominant)
  
}

