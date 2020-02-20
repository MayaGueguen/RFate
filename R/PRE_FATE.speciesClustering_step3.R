### HEADER #####################################################################
##' @title Calculate PFG traits values based on determinant species traits 
##' values
##' 
##' @name PRE_FATE.speciesClustering_step3
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to calculate PFG traits values based 
##' on determinant species traits values. Either the \code{mean} or the 
##' \code{median} is used depending on the trait class (i.e. numeric or 
##' categorical).
##'              
##' @param mat.species.traits a \code{data.frame} with at least 3 columns :
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
##' This function allows one to obtain '\emph{average}' functional trait values 
##' for each Plant Functional Group, based on values at the determinant species
##' level.
##' 
##' Those functional traits can be :
##' 
##' \describe{
##'   \item{PFG}{name of the PFG}
##'   \item{type}{or life-form, based on Raunkier. \cr It should be either 
##'   \code{H} (herbaceous), \code{C} (chamaephyte) or \code{P} (phanerophyte) 
##'   for now}
##'   \item{height}{the maximum or average height that reach the species}
##'   \item{maturity}{the age from which the species can reproduce}
##'   \item{longevity}{the maximum or average lifespan of the species}
##'   \item{dispersal}{the age from which the species can reproduce}
##'   \item{light}{a value corresponding to the light preference of the species 
##'   (from shade-lover to preference for full light)}
##'   \item{soil_contrib}{a value corresponding to the preference of the 
##'   species for soil fertility (e.g. Ellenberg, Flora Indicativa)}
##'   \item{soil_tolerance}{the range of soil tolerance : 1 (small) or 2 
##'   (large)}
##'   \item{palatability}{the appetence of each species from null to high
##'    \cr \cr}
##' }
##' 
##' 
##' @return A \code{data.frame} containing new trait values for each Plant 
##' Functional Group (PFG) :
##' 
##' \describe{
##'   \item{type}{either \code{H} (herbaceous), \code{C} (chamaephyte) or 
##'   \code{P} (phanerophyte) for now}
##'   \item{height}{mean}
##'   \item{maturity}{mean}
##'   \item{longevity}{mean}
##'   \item{dispersal}{median}
##'   \item{light}{median}
##'   \item{soil_contrib}{mean + round to one digit}
##'   \item{soil_tolerance}{mean + round to one digit}
##'   \item{palatability}{mean + round to one digit
##'    \cr \cr}
##' }
##' 
##' The information is written in \code{PRE_FATE_PFG_TRAITS_TABLE.csv} file. \cr
##' This file can be used to build parameter files to run a \code{FATE-HD} 
##' simulation.
##' 
##' @keywords functional group, traits
##' 
##' @seealso \code{\link{PRE_FATE.speciesClustering_step1}},
##' \code{\link{PRE_FATE.speciesClustering_step2}}
##' 
##' @examples
##' 
##' ## Load example data
##' PNE_PFG = .loadData("PNE_PFG")
##' 
##' ## PNE_PFG$dom.traits : data.frame
##' ## PNE_PFG$dom.determ : data.frame
##' 
##' tab1 = PNE_PFG$dom.determ[which(PNE_PFG$dom.determ$determinant == TRUE), c("species", "PFG")]
##' tab2 = PNE_PFG$dom.traits[, c("species", "height", "dispersal", "palatability", "light")]
##' determ.traits = merge(tab1, tab2, by = "species")
##' 
##' PFG.traits = PRE_FATE.speciesClustering_step3(mat.species.traits = determ.traits)
##' summary(PFG.traits)
##' 
##' @export
##' 
##' @importFrom utils write.csv
##' @importFrom reshape2 melt
##' @importFrom foreach foreach
##' @importFrom ggplot2 aes_string labs theme element_text element_rect margin
##' scale_y_log10 scale_y_continuous scale_fill_manual scale_color_manual
##' ggplot geom_boxplot geom_point geom_segment geom_linerange
##' @importFrom ggthemes theme_fivethirtyeight
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
      if (sum(mat.species.traits$soil_tolerance %in% c(1,2)) < length(which(!is.na(mat.species.traits$soil_tolerance))))
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
    if (sum(colnames(mat.species.traits) == "soil_tolerance") == 1)
    {
      stop("Missing data!\n Column names of `mat.species.traits` must contain `soil_contrib` and `soil_tolerance`")
    }
    isThere.soil = FALSE
  }
  isThere.dispersal = (length(which(colnames(mat.species.traits) == "dispersal")) == 1)
  isThere.light = (length(which(colnames(mat.species.traits) == "light")) == 1)
  isThere.palatability = (length(which(colnames(mat.species.traits) == "palatability")) == 1)
  
  
  #################################################################################################
  
  cat("\n ############## PLANT FUNCTIONAL GROUP values ############## \n")
  cat("\n Computation...")
  cat("\n")
  
  if (isThere.longevity && isThere.maturity)
  {
    ## Identify missing longevity values
    tab.longevity = rowSums(table(mat.species.traits$PFG, mat.species.traits$longevity))
    tab.longevity.pfg = names(tab.longevity)[which(tab.longevity == 0)]
    if (length(tab.longevity.pfg) > 0)
    {
      ind.pfg.longevity = which(mat.species.traits$PFG %in% tab.longevity.pfg)
    }
    
    ## Identify missing maturity values
    tab.maturity = rowSums(table(mat.species.traits$PFG, mat.species.traits$maturity))
    tab.maturity.pfg = names(tab.maturity)[which(tab.maturity == 0)]
    if (length(tab.maturity.pfg) > 0)
    {
      ind.pfg.maturity = which(mat.species.traits$PFG %in% tab.maturity.pfg)
    }

    ## Set new values for longevity
    if (length(tab.longevity.pfg) > 0 && length(ind.pfg.longevity) > 0)
    {
      mat.species.traits$longevity[ind.pfg.longevity] = mat.species.traits$maturity[ind.pfg.longevity] * 2
    }
    ## Set new values for maturity
    if (length(tab.maturity.pfg) > 0 && length(ind.pfg.maturity) > 0)
    {
      mat.species.traits$maturity[ind.pfg.maturity] = mat.species.traits$longevity[ind.pfg.maturity] / 2
    }
    ## Set new values for both
    if (length(tab.longevity.pfg) > 0 && length(tab.maturity.pfg) > 0 &&
        length(intersect(ind.pfg.longevity, ind.pfg.maturity)) > 0)
    {
      ind.pfg.longevity.maturity = intersect(ind.pfg.longevity, ind.pfg.maturity)
      mat.species.traits$longevity[ind.pfg.longevity.maturity] = 5
      mat.species.traits$maturity[ind.pfg.longevity.maturity] = 2
      warning(paste0("No trait values are available for `longevity` and `maturity` for the PFG "
                     , paste0(mat.species.traits$PFG[ind.pfg.longevity.maturity], collapse = ", ")
                     , ".\n Default values have been set to 5 (`longevity`) and 2 (`maturity`), but YOU BETTER CHECK THAT !"
      ))
    }
  }
  
  if (isThere.soil)
  {
    mat.species.traits$soil_tol_min = as.numeric(mat.species.traits$soil_contrib) - c(1, 2)[as.numeric(mat.species.traits$soil_tolerance)]
    mat.species.traits$soil_tol_max = as.numeric(mat.species.traits$soil_contrib) + c(1, 2)[as.numeric(mat.species.traits$soil_tolerance)]
    mat.species.traits = mat.species.traits[, -which(colnames(mat.species.traits) == "soil_tolerance")]
  }
  
  
  ## CALCULATE MEDIAN TRAIT VALUE PER PFG
  mat.traits.pfg = split(mat.species.traits, mat.species.traits$PFG)
  mat.traits.pfg = foreach(tab = mat.traits.pfg, .combine = "rbind") %do%
  {
    res.pfg = data.frame(PFG = unique(tab$PFG)
                         , no.species = length(unique(tab$species)))
    
    tab.val = tab[ ,-which(colnames(tab) %in% c("species", "PFG"))]
    res.val = foreach(i = 1:ncol(tab.val), .combine = "cbind") %do%
    {
      val = tab.val[, i]
      if (is.factor(val) || is.character(val))
      {
        res = median(as.numeric(as.factor(val)), na.rm = TRUE)
        res = levels(as.factor(val))[res]
      } else
      {
        res = mean(as.numeric(val), na.rm = TRUE)
        if (colnames(tab.val)[i] %in% c("soil_contrib", "soil_tol_min", "soil_tol_max"))
        {
          res = round(res, 1)
        # } else if (colnames(tab.val)[i] == "soil_tol_max")
        # {
        #   res = ceiling(res)
        } else
        {
          res = round(res)
        }
      }
      res = data.frame(res)
      colnames(res) = colnames(tab.val)[i]
      return(res)
    }
    
    return(data.frame(res.pfg, res.val))
  }
  
  
  #################################################################################################
  
  cat("\n Graphical representations...")
  cat("\n")
  
  melt.vars = c("PFG")
  if (isThere.type) {
    melt.vars = c("PFG", "type")
  }
  
  mat.species.traits.melt = melt(mat.species.traits, id.vars = c("species", melt.vars))
  mat.species.traits.melt$value = as.numeric(as.character(mat.species.traits.melt$value))
  
  mat.traits.pfg.melt = melt(mat.traits.pfg, id.vars = melt.vars)
  mat.traits.pfg.melt$value = as.numeric(as.character(mat.traits.pfg.melt$value))
  
  #################################################################################################
  ## Longevity - Maturity graph
  if (isThere.longevity && isThere.maturity)
  {
    cat("\n >> Longevity and maturity...")
    
    ind.keep.sp = which(mat.species.traits.melt$variable %in% c("longevity", "maturity"))
    ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("longevity", "maturity"))
    
    pp.1 = ggplot(mat.species.traits.melt[ind.keep.sp,]
                  , aes_string(x = "PFG", y = "value", fill = "variable")) +
      geom_boxplot(color = "grey60") +
      geom_segment(data = mat.traits.pfg
                   , aes_string(x = "PFG"
                                , xend = "PFG"
                                , y = "maturity"
                                , yend = "longevity")
                   , color = "#525252"
                   , lwd = 1
                   , inherit.aes = FALSE) +
      geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
                 , aes_string(x = "PFG"
                              , y = "value"
                              , color = "variable")
                 , size = 2
                 , inherit.aes = FALSE) +
      scale_y_log10() +
      scale_fill_manual(guide = FALSE
                        , values = c("longevity" = "#ffffff"
                                     , "maturity" = "#ffffff")) +
      scale_color_manual(""
                         , values = c("longevity" = "#377eb8"
                                      , "maturity" = "#ff7f00")) +
      labs(x = "", y = "", title = "STEP D : Computation of PFG traits values : longevity & maturity"
           , subtitle = paste0("PFG traits values are calculated as the average of the PFG determinant species traits values.\n"
                               , "If the trait is factorial or categorical, median value is taken.\n"
                               , "Light-grey boxplot represent determinant species values.\n"
                               , "Colored points represent the PFG calculated values.\n\n"
                               , "If there is no values for longevity within one PFG, and some maturity values are available,\n"
                               , "some values are inferred as maturity * 2.\n"
                               , "If there is no values for maturity within one PFG, and some longevity values are available,\n"
                               , "some values are inferred as longevity / 2.\n")) +
      theme_fivethirtyeight() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA)
            , plot.background = element_rect(fill = "transparent", colour = NA)
            , legend.background = element_rect(fill = "transparent", colour = NA)
            , legend.box.background = element_rect(fill = "transparent", colour = NA)
            , legend.key = element_rect(fill = "transparent", colour = NA))
    
  } else
  {
    pp.1 = NULL
  }
  
  #################################################################################################
  ## Soil contribution - tolerance
  if (isThere.soil)
  {
    cat("\n >> Soil contribution and tolerance...")
    
    ind.keep.sp = which(mat.species.traits.melt$variable %in% c("soil_contrib", "soil_tol_min", "soil_tol_max"))
    ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("soil_contrib", "soil_tol_min", "soil_tol_max"))
    
    pp.2 = ggplot(mat.species.traits.melt[ind.keep.sp,]
                  , aes_string(x = "PFG"
                               , y = "value"
                               , fill = "factor(variable
                                               , c('soil_tol_min'
                                                   , 'soil_contrib'
                                                   , 'soil_tol_max'))")) +
      geom_boxplot(color = "grey60") +
      geom_linerange(data = mat.traits.pfg
                     , aes_string(x = "as.numeric(PFG) + 0.5"
                                  , ymin = "soil_tol_min"
                                  , ymax = "soil_tol_max")
                     , color = "#525252"
                     , lwd = 1
                     , inherit.aes = FALSE) +
      geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
                 , aes_string(x = "as.numeric(PFG) + 0.5"
                              , y = "value"
                              , color = "factor(variable
                                               , c('soil_tol_min'
                                                   , 'soil_contrib'
                                                   , 'soil_tol_max'))")
                 , size = 2
                 , inherit.aes = FALSE) +
      scale_y_continuous(breaks = 1:max(as.numeric(as.character(mat.traits.pfg$soil_tol_max)), na.rm = TRUE)) +
      scale_fill_manual(guide = FALSE
                        , values = c("soil_tol_min" = "#ffffff"
                                     , "soil_contrib" = "#ffffff"
                                     , "soil_tol_max" = "#ffffff")) +
      scale_color_manual(""
                         , values = c("soil_tol_min" = "#ec7014"
                                      , "soil_contrib" = "#b15928"
                                      , "soil_tol_max" = "#cb181d")) +
      labs(x = "", y = "", title = "STEP D : Computation of PFG traits values : soil contribution & tolerance"
           , subtitle = paste0("PFG traits values are calculated as the average of the PFG determinant species traits values.\n"
                               , "If the trait is factorial or categorical, median value is taken.\n"
                               , "Light-grey boxplot represent determinant species values.\n"
                               , "Colored points represent the PFG calculated values.\n\n")) +
      theme_fivethirtyeight() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA)
            , plot.background = element_rect(fill = "transparent", colour = NA)
            , legend.background = element_rect(fill = "transparent", colour = NA)
            , legend.box.background = element_rect(fill = "transparent", colour = NA)
            , legend.key = element_rect(fill = "transparent", colour = NA))
    
  } else
  {
    pp.2 = NULL
  }
  
  #################################################################################################
  ## Height - Light
  if (isThere.height && isThere.light)
  {
    cat("\n >> Height and light...")
    
    ## Height panel
    ind.keep.sp = which(mat.species.traits.melt$variable %in% c("height"))
    ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("height"))
    
    pp.3.1 = ggplot(mat.species.traits.melt[ind.keep.sp,]
                    , aes_string(x = "PFG", y = "value")) +
      geom_boxplot(color = "grey60") +
      geom_point(data = mat.traits.pfg.melt[ind.keep.pfg,]
                 , aes_string(x = "PFG"
                              , y = "value"
                              , color = "variable")
                 , size = 2
                 , inherit.aes = FALSE) +
      scale_color_manual(guide = F
                         , values = c("height" = "#238b45"
                                      , "light" = "#1d91c0")) +
      scale_y_continuous(breaks = c(1, 10, 100, 1000, 5000)
                         , trans = "log") +
      labs(x = "", y = "", title = "STEP D : Computation of PFG traits values : height & light"
           , subtitle = paste0("PFG traits values are calculated as the average of the PFG determinant species traits values.\n"
                               , "If the trait is factorial or categorical, median value is taken.\n"
                               , "Light-grey boxplot represent determinant species values.\n"
                               , "Colored points represent the PFG calculated values.\n\n")) +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_blank()
            , panel.background = element_rect(fill = "transparent", colour = NA)
            , plot.background = element_rect(fill = "transparent", colour = NA)
            , legend.background = element_rect(fill = "transparent", colour = NA)
            , legend.box.background = element_rect(fill = "transparent", colour = NA)
            , legend.key = element_rect(fill = "transparent", colour = NA))
    
    ## Light panel
    ind.keep.sp = which(mat.species.traits.melt$variable %in% c("light"))
    ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("light"))
    
    pp.3.2 = ggplot(mat.species.traits.melt[ind.keep.sp,]
                    , aes_string(x = "PFG", y = "value")) +
      geom_boxplot(color = "grey60") +
      geom_point(data = mat.traits.pfg.melt[ind.keep.pfg,]
                 , aes_string(x = "PFG"
                              , y = "value"
                              , color = "factor(variable, c('height', 'light'))")
                 , size = 2
                 , inherit.aes = FALSE) +
      scale_color_manual(""
                         , values = c("height" = "#238b45"
                                      , "light" = "#1d91c0")
                         , drop = FALSE) +
      scale_y_continuous(breaks = 1:max(as.numeric(as.character(mat.traits.pfg$light)), na.rm = TRUE)) +
      labs(x = "", y = "") +
      theme_fivethirtyeight() +
      theme(axis.text.y = element_text(margin = margin(t = 0
                                                       , r = 2
                                                       , b = 0
                                                       , l = 0
                                                       , unit = "lines"))
            , panel.background = element_rect(fill = "transparent", colour = NA)
            , plot.background = element_rect(fill = "transparent", colour = NA)
            , legend.background = element_rect(fill = "transparent", colour = NA)
            , legend.box.background = element_rect(fill = "transparent", colour = NA)
            , legend.key = element_rect(fill = "transparent", colour = NA))
    
    pp.3 = list(pp.3.1, pp.3.2)
    
  } else
  {
    pp.3 = NULL
  }
  
  #################################################################################################
  ## Dispersal graph
  if (isThere.dispersal)
  {
    cat("\n >> Dispersal...")
    
    ind.keep.sp = which(mat.species.traits.melt$variable %in% c("dispersal"))
    ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("dispersal"))
    
    pp.4 = ggplot(mat.species.traits.melt[ind.keep.sp,]
                  , aes_string(x = "PFG", y = "value")) +
      geom_boxplot(color = "grey60") +
      geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
                 , aes_string(x = "PFG"
                              , y = "value"
                              , color = "variable")
                 , size = 2
                 , inherit.aes = FALSE) +
      scale_y_continuous(breaks = 1:max(as.numeric(as.character(mat.traits.pfg$dispersal)), na.rm = TRUE)) +
      scale_color_manual("", values = c("dispersal" = "#88419d")) +
      labs(x = "", y = "", title = "STEP D : Computation of PFG traits values : dispersal"
           , subtitle = paste0("PFG traits values are calculated as the average of the PFG determinant species traits values.\n"
                               , "If the trait is factorial or categorical, median value is taken.\n"
                               , "Light-grey boxplot represent determinant species values.\n"
                               , "Colored points represent the PFG calculated values.\n\n")) +
      theme_fivethirtyeight() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA)
            , plot.background = element_rect(fill = "transparent", colour = NA)
            , legend.background = element_rect(fill = "transparent", colour = NA)
            , legend.box.background = element_rect(fill = "transparent", colour = NA)
            , legend.key = element_rect(fill = "transparent", colour = NA))
    
  } else
  {
    pp.4 = NULL
  }
  
  #################################################################################################
  ## Palatability graph
  if (isThere.palatability)
  {
    cat("\n >> Palatability...")
    
    ind.keep.sp = which(mat.species.traits.melt$variable %in% c("palatability"))
    ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% c("palatability"))
    
    pp.5 = ggplot(mat.species.traits.melt[ind.keep.sp,]
                  , aes_string(x = "PFG", y = "value")) +
      geom_boxplot(color = "grey60") +
      geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
                 , aes_string(x = "PFG"
                              , y = "value"
                              , color = "variable")
                 , size = 2
                 , inherit.aes = FALSE) +
      scale_y_continuous(breaks = 1:max(as.numeric(as.character(mat.traits.pfg$palatability)), na.rm = TRUE)) +
      scale_color_manual("", values = c("palatability" = "#02818a")) +
      labs(x = "", y = "", title = "STEP D : Computation of PFG traits values : palatability"
           , subtitle = paste0("PFG traits values are calculated as the average of the PFG determinant species traits values.\n"
                               , "If the trait is factorial or categorical, median value is taken.\n"
                               , "Light-grey boxplot represent determinant species values.\n"
                               , "Colored points represent the PFG calculated values.\n\n")) +
      theme_fivethirtyeight() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA)
            , plot.background = element_rect(fill = "transparent", colour = NA)
            , legend.background = element_rect(fill = "transparent", colour = NA)
            , legend.box.background = element_rect(fill = "transparent", colour = NA)
            , legend.key = element_rect(fill = "transparent", colour = NA))
    
  } else
  {
    pp.5 = NULL
  }
  
  #################################################################################################
  
  if (sum(unlist(lapply(list(pp.1, pp.2, pp.3, pp.4, pp.5), is.null))) < 5)
  {
    pdf(file = "PRE_FATE_CLUSTERING_STEP_3D_PFGtraitsValues.pdf"
        , width = 10, height = 10)
    if (!is.null(pp.1)) plot(pp.1)
    if (!is.null(pp.2)) plot(pp.2)
    if (!is.null(pp.3)) grid.arrange(grobs = pp.3
                                     , layout_matrix = matrix(c(1, 1, 1, 2, 2)
                                                              , ncol = 1
                                                              , byrow = TRUE))
    if (!is.null(pp.4)) plot(pp.4)
    if (!is.null(pp.5)) plot(pp.5)
    dev.off()
  }
  
  
  #################################################################################################
  
  write.csv(mat.traits.pfg
            , file = "PRE_FATE_PFG_TRAITS_TABLE.csv"
            , row.names = F)
  
  message(paste0("\n The parameter file PRE_FATE_PFG_TRAITS_TABLE.csv has been successfully created !\n"))
  
  return(mat.traits.pfg)
}

