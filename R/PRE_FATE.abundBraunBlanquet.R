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
##' \tabular{ccrrrr}{
##'    \tab \strong{Braun-Blanquet} \tab ..... \tab \strong{Recovery class (\%)} \tab ..... \tab \strong{Average recovery (\%)} \cr
##'    \tab + \tab  \tab <1 \tab  \tab 0.5 \cr
##'    \tab 1 \tab  \tab 1-5 \tab  \tab 3 \cr
##'    \tab 2 \tab  \tab 5-25 \tab  \tab 15 \cr
##'    \tab 3 \tab  \tab 25-50 \tab  \tab 37.5 \cr
##'    \tab 4 \tab  \tab 50-75 \tab  \tab 62.5 \cr
##'    \tab 5 \tab  \tab 75-100 \tab  \tab 87.5 \cr
##' }
##' 
## \cr
##' 
##' \emph{Braun-Blanquet J., Roussine N. & Nègre R., 1952. Les groupements végétaux de la France méditerranéenne.
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
  ## Convert Braun-Blanquet abundance classes into median coverage percentage
  if (!(is.vector(abund) | is.factor(abund)) |
      is.list(abund) | is.matrix(abund) | is.data.frame(abund)){
    stop("Wrong type of data!\n `abund` must be a vector")
  }
  
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
