### HEADER #####################################################################
##' @title Calculate PFG traits values based on determinant species traits 
##' values
##' 
##' @name PRE_FATE.speciesClustering_step3
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create 
##'              
##' @param mat.species.traits A \code{data.frame} with at least 3 columns :
##' \describe{
##'   \item{\code{species}}{the ID of each determinant species (see 
##'   \code{\link{PRE_FATE.speciesClustering_step2}})}
##'   \item{\code{PFG}}{a factor variable containing grouping information to
##'   divide the species into Plant Functional Groups (see 
##'   \code{\link{PRE_FATE.speciesClustering_step2}})}
##'   \item{\code{...}}{one column for each functional trait (see 
##'   \code{Details})}
##' }
##' 
##' @details
##' 
##' This function allows one to obtain 'average' functional trait values for
##' each Plant Functional Group, based on values at the determinant species
##' level.
##' 
##' Those functional traits can be :
##' 
##' \describe{
##'   \item{type}{or life-form, based on Raunkier. It should be either \code{H} 
##'   (herbaceous), \code{C} (chamaephyte) or \code{P} (phanerophyte) for now}
##'   \item{height}{the maximum or average height that reach the species}
##'   \item{maturity}{the age from which the species can reproduce}
##'   \item{longevity}{the maximum or average lifespan of the species}
##'   \item{dispersal}{the age from which the species can reproduce}
##'   \item{light}{a value corresponding to the light preference of the species
##'   (from preference for shade to full light)}
##'   \item{soil_contrib}{a value between 0 and 10 corresponding to the 
##'   Ellenberg nitrogen value of the PFG}
##'   \item{soil_tolerance}{the range of soil tolerance : small(1) or large(2)}
##'   \item{palatability}{the appetence of each species from null to high
##'    \cr \cr}
##' }
##' 
##' 
##' 
##' 
##' @return A \code{.txt} file per PFG into the 
##' \code{name.simulation/DATA/PFGS/SUCC/} directory with the following 
##' parameters :
##' 
##' \itemize{
##'   \item NAME : name of the PFG
##'   \item MATURITY : the maturity age of the PFG \emph{(in years)}
##'   \item LONGEVITY : the PFG life span \emph{(in years)}
##'   \item MAX_ABUNDANCE : the maximal (qualitative) shade that the PFG is able
##'   to produce \cr \emph{(1: Low 2: Medium 3: High)}
##'   \item IMM_SIZE : the relative size of the immature PFG \cr
##'   \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 
##'   8: 80\% 9: 90\% 10: 100\%)}
##'   \item CHANG_STR_AGES : the ages at which the PFG goes in the upper stratum
##'   \cr \emph{(in years, put a value higher than the PFG life span if it is 
##'   not supposed to rise a stratum)}
##'   \item SEED_POOL_LIFE : the maximal number of years seeds are able to
##'   survive (for active and dormant pool)
##'   \item SEED_DORMANCY : are the seeds dormant or not \emph{(0: No 1: Yes)
##'   \cr \cr}
##' }
##' 
##' A \code{SUCC_COMPLETE_TABLE.csv} file summarizing information for all groups
##' into the \code{name.simulation/DATA/PFGS/} directory.  
##' This file can be used to parameterize the disturbance files.
##' 
##' 
##' @examples
##' 
##' 
##' @export
##' 
##' @importFrom utils write.table
##'
## END OF HEADER ###############################################################


PRE_FATE.speciesClustering_step3 = function(
  mat.species.traits
){
  
  ## Check existence of parameters
  if (.testParam_notDf(mat.species.traits))
  {
    .stopMessage_beDataframe("mat.species.traits")
  }
  if (nrow(mat.species.traits) == 0 || ncol(mat.species.traits) <= 2)
  {
    .stopMessage_numRowCol("mat.species.traits", c("species", "PFG", "(type)"
                                                   , "(height)", "(maturity)"
                                                   , "(longevity)", "(dispersal)"
                                                   , "(light)", "(soil_contrib)"
                                                   , "(soil_tol_min)", "(soil_tol_max)"
                                                   , "(palatability)"))
    # stop("Wrong dimension(s) of data!\n `mat.species.traits` does not have the appropriate number of cols (>=3, at least 1 trait)")
  }
  if (ncol(mat.species.traits) > 2)
  {
    if (sum(colnames(mat.species.traits) %in% c("species", "PFG")) != 2)
    {
      .stopMessage_columnNames("mat.species.traits", c("species", "PFG"))
    }
  }
  ## Test PFG values
  mat.species.traits$PFG = as.character(mat.species.traits$PFG)
  if (length(which(is.na(mat.species.traits$PFG))) > 0){
    .stopMessage_columnNoNA("mat.species.traits", c("PFG"))
  }
  if (.testParam_notChar(mat.species.traits$PFG))
  {
    .stopMessage_beChar("mat.species.traits$PFG")
  }
  ## Test type values
  if (sum(colnames(mat.species.traits) == "type") == 1)
  {
    mat.species.traits$type = as.character(mat.species.traits$type)
    if (length(which(is.na(mat.species.traits$type))) > 0){
      .stopMessage_columnNoNA("mat.species.traits", c("type"))
    }
    if (.testParam_notInChar(mat.species.traits$type, inList = c("H", "C", "P")))
    {
      .stopMessage_content("mat.species.traits$type", c("H", "C", "P"))
    }
    isThere.type = TRUE
  } else
  {
    isThere.type = FALSE
  }
  ## Test height values
  if (sum(colnames(mat.species.traits) == "height") == 1)
  {
    if (!is.numeric(mat.species.traits$height))
    {
      .stopMessage_columnNumeric("mat.species.traits", c("height"))
    }
    isThere.height = TRUE
  } else
  {
    isThere.height = FALSE
  }
  ## Test maturity values
  if (sum(colnames(mat.species.traits) == "maturity") == 1)
  {
    if (!is.numeric(mat.species.traits$maturity))
    {
      .stopMessage_columnNumeric("mat.species.traits", c("maturity"))
    }
    isThere.maturity = TRUE
  } else
  {
    isThere.maturity = FALSE
  }
  ## Test longevity values
  if (sum(colnames(mat.species.traits) == "longevity") == 1)
  {
    if (!is.numeric(mat.species.traits$longevity))
    {
      .stopMessage_columnNumeric("mat.species.traits", c("longevity"))
    }
    isThere.longevity = TRUE
  } else
  {
    isThere.longevity = FALSE
  }
  ## Test soil values
  if (sum(colnames(mat.species.traits) == "soil_contrib") == 1)
  {
    if (!is.numeric(mat.species.traits$soil_contrib))
    {
      .stopMessage_columnNumeric("mat.species.traits", c("soil_contrib"))
    }
    if (sum(colnames(mat.species.traits) == "soil_tolerance") == 1)
    {
      if (sum(mat.species.traits$soil_tolerance %in% c(1,2)) < nrow(mat.species.traits))
      {
        stop("Wrong type of data!\n Column `soil_tolerance` of `mat.species.traits` must contain values between 1 and 2")
      }
      isThere.soil = TRUE
    } else
    {
      stop("Missing data!\n Column names of `mat.species.traits` must contain `soil_contrib` and `soil_tolerance`")
    }
  } else
  {
    isThere.soil = FALSE
  }
  isThere.dispersal = (length(which(colnames(mat.species.traits) == "dispersal")) == 1)
  isThere.light = (length(which(colnames(mat.species.traits) == "light")) == 1)
  isThere.palatability = (length(which(colnames(mat.species.traits) == "palatability")) == 1)
  
  
  #################################################################################################
  
  if (isThere.longevity && isThere.maturity)
  {
    ## Set new values for longevity
    tab.longevity = rowSums(table(mat.species.traits$PFG, mat.species.traits$longevity))
    tab.longevity.pfg = names(tab.longevity)[which(tab.longevity == 0)]
    if (length(tab.longevity.pfg) > 0)
    {
      ind.pfg = which(mat.species.traits$PFG %in% tab.longevity.pfg)
      mat.species.traits$longevity[ind.pfg] = mat.species.traits$maturity[ind.pfg] * 2
    }
    
    ## Set new values for maturity
    tab.maturity = rowSums(table(mat.species.traits$PFG, mat.species.traits$maturity))
    tab.maturity.pfg = names(tab.maturity)[which(tab.maturity == 0)]
    if (length(tab.maturity.pfg) > 0)
    {
      ind.pfg = which(mat.species.traits$PFG %in% tab.maturity.pfg)
      mat.species.traits$maturity[ind.pfg] = mat.species.traits$longevity[ind.pfg] / 2
    }
  }
  
  if (isThere.soil)
  {
    mat.species.traits$soil_tol_min = as.numeric(mat.species.traits$soil_contrib) - c(1, 2)[as.numeric(mat.species.traits$soil_tolerance)]
    mat.species.traits$soil_tol_max = as.numeric(mat.species.traits$soil_contrib) + c(1, 2)[as.numeric(mat.species.traits$soil_tolerance)]
  }
  
  
  ## CALCULATE MEDIAN TRAIT VALUE PER PFG
  mat.traits.pfg = split(mat.species.traits, mat.species.traits$PFG)
  mat.traits.pfg = foreach(tab = mat.traits.pfg, .combine = "rbind") %do%
  {
    res = data.frame(PFG = unique(tab$PFG)
                     , no.species = length(unique(tab$species)))
    
    # res.val = apply(tab[-which(colnames(tab) %in% c("species", "PFG"), )], 2, function(x)
    # {
    tab.val = tab[-which(colnames(tab) %in% c("species", "PFG")), ]
    res.val = foreach(i = 1:ncol(tab.val), .combine = "cbind") %do%
    {
      val = tab.val[, i]
      # val.names = colnames(tab.val)[i]
      if (is.factor(val))
      {
        res = round(median(as.numeric(val), na.rm = TRUE))
      } else
      {
        res = round(mean(as.numeric(val), na.rm = TRUE))
      }
      names(res) = colnames(tab.val)[i]
      return(res)
    }
    
    return(data.frame(res, res.val))
  }

  
  #################################################################################################
  
  # mat.species.traits.melt = melt(mat.species.traits, id.vars = c("species", "PFG", "type"))
  # mat.species.traits.melt$value = as.numeric(as.character(mat.species.traits.melt$value))
  # head(mat.species.traits.melt)
  # 
  # mat.traits.pfg.melt = melt(mat.traits.pfg, id.vars = c("PFG", "type"))
  # mat.traits.pfg.melt$value = as.numeric(as.character(mat.traits.pfg.melt$value))
  # head(mat.traits.pfg.melt)
  # 
  # # ggplot(mat.species.traits.melt, aes(x = PFG, y = value, fill = type)) +
  # #   geom_boxplot(varwidth = TRUE) +
  # #   geom_point(data = mat.traits.pfg.melt, color = "brown") +
  # #   facet_wrap(~ variable, scales = "free_y") +
  # #   scale_fill_discrete(guide = F) +
  # #   labs(x = "", y = "") +
  # #   theme_fivethirtyeight()
  # 
  # ## Longevity - Maturity graph
  # ind.keep.sp = which(mat.species.traits.melt$variable %in% c("longevity", "maturity"))
  # ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("longevity", "maturity"))
  # ggplot(mat.species.traits.melt[ind.keep.sp, ]
  #        , aes(x = PFG, y = value, fill = variable)) +
  #   geom_boxplot(color = "grey60") + #varwidth = TRUE) +
  #   geom_segment(data = mat.traits.pfg
  #                , aes(x = PFG
  #                      , xend = PFG
  #                      , y = maturity
  #                      , yend = longevity)
  #                , color = "#525252"
  #                , lwd = 1
  #                , inherit.aes = FALSE) +
  #   geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
  #              , aes(x = PFG
  #                    , y = value
  #                    , color = variable)
  #              , size = 2
  #              , inherit.aes = FALSE) +
  #   scale_y_log10() +
  #   scale_fill_manual(guide = FALSE, values = c("longevity" = "#ffffff", "maturity" = "#ffffff")) +
  #   scale_color_manual("", values = c("longevity" = "#377eb8", "maturity" = "#ff7f00")) +
  #   labs(x = "", y = "") +
  #   theme_fivethirtyeight()
  # 
  # ## Soil contribution - tolerance
  # ind.keep.sp = which(mat.species.traits.melt$variable %in% c("soil_contrib", "soil_tol_min", "soil_tol_max"))
  # ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("soil_contrib", "soil_tol_min", "soil_tol_max"))
  # ggplot(mat.species.traits.melt[ind.keep.sp, ]
  #        , aes(x = PFG
  #              , y = value
  #              , fill = factor(variable, c("soil_tol_min", "soil_contrib", "soil_tol_max")))) +
  #   geom_boxplot(color = "grey60") + #varwidth = TRUE) +
  #   geom_linerange(data = mat.traits.pfg
  #                  , aes(x = as.numeric(PFG) + 0.5
  #                        , ymin = soil_tol_min
  #                        , ymax = soil_tol_max)
  #                  , color = "#525252"
  #                  , lwd = 1
  #                  , inherit.aes = FALSE) +
  #   geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
  #              , aes(x = as.numeric(PFG) + 0.5
  #                    , y = value
  #                    , color = variable)
  #              , size = 2
  #              , inherit.aes = FALSE) +
  #   scale_fill_manual(guide = FALSE
  #                     , values = c("soil_tol_min" = "#ffffff"
  #                                  , "soil_contrib" = "#ffffff"
  #                                  , "soil_tol_max" = "#ffffff")) +
  #   scale_color_manual("", values = c("soil_tol_min" = "#ec7014"
  #                                     , "soil_contrib" = "#b15928"
  #                                     , "soil_tol_max" = "#cb181d")) +
  #   labs(x = "", y = "") +
  #   theme_fivethirtyeight()
  # 
  # #################################################################################################
  # 
  # ## Height panel
  # ind.keep.sp = which(mat.species.traits.melt$variable %in% c("height"))
  # ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("height"))
  # pp1 = ggplot(mat.species.traits.melt[ind.keep.sp, ]
  #              , aes(x = PFG
  #                    , y = value))+
  #   geom_boxplot(color = "grey60") + #varwidth = TRUE) +
  #   geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
  #              , aes(x = as.numeric(PFG)
  #                    , y = value
  #                    , color = variable)
  #              , size = 2
  #              , inherit.aes = FALSE) +
  #   scale_color_manual(guide = F, values = c("height" = "#238b45"
  #                                            , "light" = "#1d91c0")) +
  #   scale_y_continuous(breaks = c(1, 10, 100, 1000, 5000)
  #                      , trans = "log") +
  #   labs(x = "", y = "") +
  #   theme_fivethirtyeight() +
  #   theme(axis.text.x = element_blank()
  #         , panel.background = element_rect(fill = "transparent", colour = NA)
  #         , plot.background = element_rect(fill = "transparent", colour = NA))
  # 
  # ## Light panel
  # ind.keep.sp = which(mat.species.traits.melt$variable %in% c("light"))
  # ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("light"))
  # pp2 = ggplot(mat.species.traits.melt[ind.keep.sp, ]
  #              , aes(x = PFG
  #                    , y = value))+
  #   geom_boxplot(color = "grey60") + #varwidth = TRUE) +
  #   geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
  #              , aes(x = as.numeric(PFG)
  #                    , y = value
  #                    , color = factor(variable, c("height", "light")))
  #              , size = 2
  #              , inherit.aes = FALSE) +
  #   scale_color_manual("", values = c("height" = "#238b45"
  #                                     , "light" = "#1d91c0")
  #                      , drop = FALSE) +
  #   scale_y_continuous(breaks = 1:max(mat.traits.pfg$light)) +
  #   labs(x = "", y = "") +
  #   theme_fivethirtyeight() +
  #   theme(panel.background = element_rect(fill = "transparent", colour = NA)
  #         , plot.background = element_rect(fill = "transparent", colour = NA)
  #         , legend.background = element_rect(fill = "transparent", colour = NA)
  #         , legend.box.background = element_rect(fill = "transparent", colour = NA)
  #         , legend.key = element_rect(fill = "transparent", colour = NA)
  #         , axis.text.y = element_text(margin = margin(t = 0, r = 2, b = 0, l = 0, unit = "lines")))
  # 
  # grid.arrange(grobs = list(pp1, pp2)
  #              , layout_matrix = matrix(c(1,1,1,2,2), ncol = 1, byrow = TRUE))
  

  
  #################################################################################################


  
  # write.table(params.csv
  #           , file = paste0(name.simulation, "/DATA/PFGS/SUCC_COMPLETE_TABLE.csv")
  #           , row.names = F
  #           , col.names = T)
  
  #################################################################################################
  
  return(mat.traits.pfg)
}

