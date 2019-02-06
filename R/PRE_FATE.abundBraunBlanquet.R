### HEADER #####################################################################
##' @title Transform Braun-Blanquet values into relative abundances
##' 
##' @name PRE_FATE.abundBraunBlanquet
##'
##' @author Maya Guéguen
##' 
# @date 15/03/2018
##' 
##' @description This script is designed to transform Braun-Blanquet abundance
##' information into relative abundances or average recovery values
##' (between 0 and 100).
##'              
##' @param abund a \code{vector} with abundance values from Braun-Blanquet
##' (+, r, 1, 2, 3, 4, 5), and with \code{NA} when no information
##' 
##' @details 
##' 
##' Braun-Blanquet values allow to estimate the abundance-dominance of a species
##' based on an estimation of the number of individuals and the covering surface.
##' A correspondence has been defined between this index and average recovery values :
##' 
##' 
##' \tabular{crr}{
##' \strong{Braun-Blanquet} \tab \strong{Recovery class (\%)} \tab \strong{Average recovery (\%)}\cr
##' + \tab     <1 \tab  0.5\cr
##' 1 \tab    1-5 \tab    3\cr
##' 2 \tab   5-25 \tab   15\cr
##' 3 \tab  25-50 \tab 37.5\cr
##' 4 \tab  50-75 \tab 62.5\cr
##' 5 \tab 75-100 \tab 87.5
##' }
##' 
##' 
##' \emph{\cr \cr
##' Braun-Blanquet J., Roussine N. & Nègre R., 1952. Les groupements végétaux de la France méditerranéenne.
##' Dir. Carte Group. Vég. Afr. Nord , CNRS, 292 p.}
##' 
##' \emph{Baudière A. & Serve L., 1975. Les groupements végétaux du Plade Gorra-Blanc (massif du Puigmal, Pyrénées Orientales).
##' Essai d'interprétation phytosociologique et phytogéographique. Nat. Monsp., sér. Bot., 25, 5-21.}
##' 
##' \emph{Foucault B. (de), 1980. Les prairies du bocage virois (Basse-Normandie, France). Typologie phytosociologique et 
##' essai de reconstitution des séries évolutives herbagères. Doc. Phytosoc., N.S., 5, 1-109.}
##' 
##' @return A \code{vector} with numerical values between 0 and 100 corresponding to the median of each recovery class.
##' 
##' @keywords abundance, Braun-Blanquet
##' 
##' @examples
##' 
##' ## Load example data
##' data(MontBlanc)
##' str(MontBlanc)
##'  
##' ## MontBlanc$mat.releves : data.frame
##' summary(MontBlanc$mat.releves$abund)
##' 
##' ## Transformation of Braun-Blanquet abundances
##' MontBlanc$mat.releves$abund = PRE_FATE.abundBraunBlanquet(abund = MontBlanc$mat.releves$abund)
##' 
##' summary(MontBlanc$mat.releves$abund)
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.abundBraunBlanquet = function(abund){
  if (.testParam_notInClass(abund, inList = c("character", "factor", "numeric", "logical")))
  {
    stop("Wrong type of data!\n `abund` must be a vector with character values")
  } else
  {
    if (class(abund) == "factor")
    {
      abund = as.character(abund)
    }
    if (sum(abund %in% c(NA,"NA","","+","r",1:5)) < length(abund))
    {
      stop("Wrong type of data!\n `abund` must contain values such as NA, +, r, 1, 2, 3, 4 or 5")
    }
  }

  
  ## Convert Braun-Blanquet abundance classes into median coverage percentage
  if (length(na.exclude(abund)) > 0) {
    abund = sapply(abund, function(x){
      x = as.character(x)
      if (is.na(x) | x == "") {
        return(NA)
      } else if (x == "+" | x == "r") {
        return(0.5)
      } else if (x == "1") {
        return(3)
      } else if (x == "2") {
        return(15)
      } else if (x == "3") {
        return(37.5)
      } else if (x == "4") {
        return(62.5)
      } else if (x == "5") {
        return(87.5)
      } else {
        return(NA)
      }
    })
  }
  
  abund = as.numeric(abund)
  return(abund)
}
