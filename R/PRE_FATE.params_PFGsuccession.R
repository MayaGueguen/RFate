### HEADER #####################################################################
##' @title Create \emph{SUCCESSION} parameter files for a \code{FATE-HD}
##' simulation
##' 
##' @name PRE_FATE.params_PFGsuccession
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create parameter files containing 
##' succession parameters for each PFG (one file for each of them) used in the
##' core module of \code{FATE-HD}.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param mat.PFG.succ a \code{data.frame} with 7 columns : PFG, type, height,
##' maturity, longevity, dispersal, light
##' 
##' 
##' @details
##' 
##' The core module of \code{FATE-HD} allows the user to simulate a primary vegetation
##' succession based on light competition.
##' Several parameters are required for each PFG in order to set up this life cycle :
##' 
##' \describe{
##'   \item{type}{or life-form, based on Raunkier. It should be either \code{H} 
##'   (herbaceous), \code{C} (chamaephyte) or \code{P} (phanerophyte) for now}
##'   \item{height}{the maximum or average height that reach the PFG}
##'   \item{maturity}{the age from which the PFG can reproduce}
##'   \item{longevity}{the maximum or average lifespan of the PFG}
##'   \item{dispersal}{the method of dispersal of the PFG
##'   \itemize{
##'     \item 0 = no dispersal
##'     \item 1 = homogeneous dispersal within the d50, d99 and ldd circles
##'     \item 2 = negative exponential kernel within the d50, d99 and ldd circles
##'     \item 3 = negative exponential kernel + probability decreasing with distance
##'     within the d50, d99 and ldd circles
##'     \item 4 = homogeneous dispersal EVERYWHERE \emph{(!not available YET!)}
##'   }
##'   }
##'   \item{light}{a value between 0 and 10 corresponding to the Ellenberg value
##'   of the PFG \cr \cr}
##' }
##' 
##' 
##' 
##' 
##' These values will allow to calculate or define a set of characteristics for each
##' PFG :
##' 
##' \describe{
##'   \item{STRATA_LIMITS}{= the height values that define each stratum. \cr \cr
##'   The steps are detailed below and try to homogenize the number of PFG
##'   within each stratum :
##'   \itemize{
##'     \item the average number of PFG per stratum should be close to 
##'     the square root of the total number of PFG (\code{no.PFG.perStrata})
##'     \item \code{strata limits} should go exponentially and will be selected among \cr
##'     \code{0, 20, 50, 150, 400, 1000, 2000, 5000, 10000}
##'     \item PFG are divided according to their \code{height} and these \code{strata limits}
##'     and then grouped in order to have per stratum a number of PFG \code{>= (no.PFG.perStrata - 2)}
##'   }
##'   }
##'   \item{STRATA}{the maximum stratum that each PFG can reach}
##'   \item{MAX_ABUNDANCE}{= maximum abundance of mature PFG in favorable conditions \cr
##'   = maximum shade a PFG can make in a pixel corresponding to a number of individuals \cr \cr
##'   It is defined according to the number of strata potentially occupied by a PFG :
##'   \itemize{
##'     \item herbaceous make little shade (1), chamaephytes make medium shade (2) and
##'     phanerophytes make lot of shade (3)
##'     \item all plants in first stratum make little shade (1)
##'     \item plants other than herbaceous in stratum 2 make medium shade (2)
##'     \item herbaceous in stratum > 2 make medium shade (2)
##'     \item chamaephytes in stratum > 3 make lot of shade (3)
##'   }
##'   }
##'   \item{IMM_SIZE}{= relative shade of immature plants \cr
##'   \itemize{
##'     \item immature herbaceous contribute to shade in the same way than mature
##'     herbaceous (100 \%)
##'     \item immature chamaephytes contribute to shade half less than mature
##'     herbaceous (50 \%)
##'     \item immature phanerophytes contribute to shade only by 10 \% of their
##'      full capacity
##'     \item intermediate percentages for herbaceous in stratum 2 (80 \%) and in 
##'     stratum > 2 (50 \%)
##'     \item immature chamaephytes in 1st stratum contribute to shade in the same 
##'     way than mature chamaephytes (100 \%)
##'     \item immature phanerophytes with height < 10m contribute to shade half less 
##'     than mature phanerophytes (50 \%)
##'   }
##'   }
##'   \item{CHANG_STR_AGES}{= at what age each PFG goes into the upper stratum. \cr \cr
##'   It is defined using a logistic growth curve with 2 points to parameterize it :
##'   \enumerate{
##'     \item at \code{age = maturity/2}, \code{height = IMM_SIZE * height}
##'     \item at \code{age = longevity}, \code{height = height}
##'   }
##'   }
##'   \item{SHADE_TOL}{ defined for each life stage (Germinant, Immature, Mature) and 
##'   each light condition (Low, Medium, High) :
##'   \itemize{
##'     \item PFG are tolerant to low light if \code{light <= 3}
##'     \item PFG are tolerant to medium light if \code{light <= 4}
##'     \item PFG are tolerant to high light if \code{light >= 3}
##'     \item all germinants are assumed to be tolerant to Low and Medium light
##'     \item all mature trees are assumed to be tolerant to High light in the upper strata
##'     \item all immature trees that grow in the penultimate stratum are assumed to
##'     be tolerant to High light
##'   }
##'   }
##' }
##' 
##' 
##' 
##' @return A \code{.txt} file per PFG into the \code{name.simulation/DATA/PFGS/SUCC/}
##' directory with the following parameters :
##' 
##' \itemize{
##'   \item NAME : name of the PFG
##'   \item MATURITY : the maturity age of the PFG \emph{(in years)}
##'   \item LONGEVITY : the PFG life span \emph{(in years)}
##'   \item MAX_ABUNDANCE : the maximal (qualitative) shade that the PFG is able to
##'   produce \cr \emph{(1: Low 2: Medium 3: High)}
##'   \item IMM_SIZE : the relative size of the immature PFG \cr
##'   \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 8: 80\% 9: 
##'   90\% 10: 100\%)}
##'   \item CHANG_STR_AGES : the ages at which the PFG goes in the upper stratum  \cr
##'   \emph{(in years, put a value higher than the PFG life span if it is not supposed to
##'   rise a stratum)}
##'   \item WIDE_DISPERS : is the PFG able to disperse everywhere (i.e. no dispersal
##'   limits) \emph{(0: No 1: Yes)}
##'   \item ACTIVE_GERM : the germination rates depending on light conditions \cr
##'   \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 8: 80\% 9: 
##'   90\% 10: 100\%)}
##'   \item SHADE_TOL : the PFG shade tolerance table (in a single row). \cr This is a
##'   vector of 9 numbers \emph{(0: Die 1: Survive)} corresponding to the ability of the PFG
##'   to survive or not :
##'   \itemize{
##'     \item at different life stages \emph{(Germinant (Ge), Immature (Im), Mature (Ma))}
##'     \item under different light conditions \emph{(Low (L), Medium (M) or High (H))}.
##'   }
##'   These parameters should be given in this order : GeL, GeM, GeH, ImL, ImM, ImH,
##'   MaL, MaM, MaH.
##'   \item SEED_POOL_LIFE : the maximal number of years seeds are able to survive
##'   (for active and dormant pool)
##'   \item SEED_DORMANCY : are the seeds dormant or not \emph{(0: No 1: Yes) \cr \cr}
##' }
##' 
##' A \code{SUCC_COMPLETE_TABLE.csv} file summarizing information for all groups into the
##' \code{name.simulation/DATA/PFGS/} directory.  
##' This file can be used to parameterize the disturbance files.
##' 
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create PFG succession parameter files
##' PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
##'                             , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
##'                                                         , type = c("C", "C", "H", "H", "P", "P")  
##'                                                         , height = c(10, 250, 36, 68, 1250, 550)
##'                                                         , maturity = c(5, 5, 3, 3, 8, 9)
##'                                                         , longevity = c(12, 200, 25, 4, 110, 70)
##'                                                         , dispersal = 1
##'                                                         , light = c(4, 6, 3, 6, 5, 5)))
##' 
##' @export
##' 
##' @importFrom utils write.table
##'
## END OF HEADER ###############################################################


PRE_FATE.params_PFGsuccession = function(
  name.simulation
  , mat.PFG.succ
){
  
  if (missing(name.simulation) ||
      !is.character(name.simulation) ||
      !dir.exists(paste0(name.simulation, "/DATA/PFGS/SUCC/"))){
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  }
  if (missing(mat.PFG.succ) || !is.data.frame(mat.PFG.succ))
  {
    stop("Wrong type of data!\n `mat.PFG.succ` must be a data.frame")
  }
  if (nrow(mat.PFG.succ) == 0 || ncol(mat.PFG.succ) != 7)
  {
    stop(paste0("Wrong dimension(s) of data!\n `mat.PFG.succ` does not have "
                , "the appropriate number of rows (>0) or columns "
                , "(PFG, type, height, maturity, longevity, dispersal, light)"))
  }
  if (ncol(mat.PFG.succ) == 7)
  {
    if (sum(colnames(mat.PFG.succ) == c("PFG", "type","height", "maturity", "longevity", "dispersal", "light")) == 7)
    {
      mat.PFG.succ = mat.PFG.succ[ , c("PFG", "type","height", "maturity", "longevity", "dispersal", "light")]
    } else {
      stop(paste0("Wrong type of data!\n Column names of `mat.PFG.succ` must be "
                  , "`PFG`, `type`, height`, `maturity`, `longevity`, `dispersal` and `light`"))
    }
  }
  if (length(which(is.na(mat.PFG.succ$PFG))) > 0 ||
      length(unique(mat.PFG.succ$PFG)) < nrow(mat.PFG.succ)){
    stop("Wrong type of data!\n Column `PFG` of `mat.PFG.succ` must contain different values")
  }
  if (sum(mat.PFG.succ$type %in% c("H", "C", "P")) < nrow(mat.PFG.succ)){
    stop("Wrong type of data!\n Column `type` of `mat.PFG.succ` must contain values such as `H`, `C` or `P`")
  }
  if (!is.numeric(mat.PFG.succ$height) ||
      !is.numeric(mat.PFG.succ$maturity) ||
      !is.numeric(mat.PFG.succ$longevity) ||
      !is.numeric(mat.PFG.succ$dispersal) ||
      !is.numeric(mat.PFG.succ$light)) {
    stop(paste0("Wrong type of data!\n Columns `height`, `maturity`, `longevity`, "
                , "`dispersal` and `light` of `mat.PFG.succ` must contain numeric values"))
  }
  if (length(which(is.na(mat.PFG.succ$height))) > 0 ||
      length(which(is.na(mat.PFG.succ$maturity))) > 0 ||
      length(which(is.na(mat.PFG.succ$longevity))) > 0 ||
      length(which(is.na(mat.PFG.succ$dispersal))) > 0 ||
      length(which(is.na(mat.PFG.succ$light))) > 0) {
    stop(paste0("Wrong type of data!\n Columns `height`, `maturity`, `longevity`, "
                , "`dispersal` and `light` of `mat.PFG.succ` must not contain NA values"))
  }
  if (sum(mat.PFG.succ$dispersal %in% seq(0,3)) < nrow(mat.PFG.succ)){
    stop("Wrong type of data!\n Column `dispersal` of `mat.PFG.succ` must contain values between 0 and 3")
  }
  if (sum(mat.PFG.succ$light %in% seq(0,10)) < nrow(mat.PFG.succ)){
    stop("Wrong type of data!\n Column `light` of `mat.PFG.succ` must contain values between 0 and 10")
  }
  
  #################################################################################################
  
  no.PFG = nrow(mat.PFG.succ)
  
  ## GET PFG NAME
  NAME = as.character(mat.PFG.succ$PFG)
  
  ## GET PFG TYPE
  TYPE = as.character(mat.PFG.succ$type)

  ## GET MATURITY AGE values
  MATURITY = mat.PFG.succ$maturity
  
  ## GET LONGEVITY values
  ## Death precedes seed productivity in the model thus longevity param = longevity + 1
  LONGEVITY = mat.PFG.succ$longevity + 1
  
  cat("\n ############## GROUP INFORMATIONS ############## \n")
  cat("\n Number of groups : ", no.PFG)
  cat("\n Number of PFG of each type : "
      , length(which(TYPE == "H")), " H, "
      , length(which(TYPE == "C")), " C, "
      , length(which(TYPE == "P")), " P, ")
  cat("\n")
  
  #################################################################################################
  
  ## GET height strata limits (for light competition and PFG growth)
  ## n strata (+ germinants = 0)
  no.PFG.perStrata = round(sqrt(no.PFG))
  strata.limits = c(0, 20, 50, 150, 400, 1000, 2000, 5000, 10000)
  categories = cut(mat.PFG.succ$height, breaks = strata.limits)
  categories.table = table(categories)
  STRATA_LIMITS = 0
  tmp = categories.table[1]
  for (categ in 2:length(strata.limits))
  {
    if (tmp >= max(c(2, (no.PFG.perStrata - 2))))
    {
      STRATA_LIMITS = c(STRATA_LIMITS, strata.limits[categ])
      tmp = categories.table[categ]
    } else 
    {
      tmp = tmp + categories.table[categ]
    }
  }
  # barplot(table(cut(mat.PFG.succ$height, breaks = STRATA_LIMITS)))
  
  cat("\n ############## STRATA INFORMATIONS ############## \n")
  cat("\n Number of strata : ", length(STRATA_LIMITS))
  cat("\n Height limits of selected strata : ", STRATA_LIMITS)
  cat("\n Number of PFG within each stratum : ", table(cut(mat.PFG.succ$height, breaks = STRATA_LIMITS)))
  cat("\n")
  
  ## GET STRATA attribution
  STRATA = sapply(mat.PFG.succ$height, function(h) {
    max(which(STRATA_LIMITS < h), na.rm = T)
  })
  
  #################################################################################################
  
  ## GET MAX ABUNDANCE
  ##  = maximum abundance of mature PFG in favorable conditions
  ##  = maximum shade a PFG can make in a pixel corresponding to a number of individuals
  ## Defined according to the number of strata potentially occupied by a PFG
  ## 3 levels : 1 = Low, 2 = Medium or 3 = High
  MAX_ABUNDANCE = rep(NA, no.PFG)
  MAX_ABUNDANCE[which(mat.PFG.succ$type == "H")] = 1 ## herbaceous make little shade 
  MAX_ABUNDANCE[which(mat.PFG.succ$type == "C")] = 2 ## chamaephytes make medium shade 
  MAX_ABUNDANCE[which(mat.PFG.succ$type == "P")] = 3 ## phanerophytes make lot of shade 
  MAX_ABUNDANCE[which(STRATA == 1)] = 1 ## all plants in first stratum make little shade
  MAX_ABUNDANCE[which(STRATA == 2 & mat.PFG.succ$type != "H")] = 2 ## plants other than herbaceous in stratum 2 make medium shade
  MAX_ABUNDANCE[which(STRATA > 2 & mat.PFG.succ$type == "H")] = 2 ## herbaceous in stratum > 2 make medium shade
  MAX_ABUNDANCE[which(STRATA > 3 & mat.PFG.succ$type == "C")] = 3 ## chamaephytes in stratum > 3 make lot of shade
  
  #################################################################################################
  
  ## GET IMMATURE SIZE
  ##   = relative shade of immature plants
  ## 11 levels : 0 = 0 %
  ##             1 = 10 %
  ##             2 = 20 %
  ##             3 = 30 %
  ##             4 = 40 %
  ##             5 = 50 %
  ##             6 = 60 %
  ##             7 = 70 %
  ##             8 = 80 %
  ##             9 = 90 %
  ##             10 = 100 %
  IMM_SIZE = rep(10, no.PFG)
  IMM_SIZE[which(mat.PFG.succ$type == "H")] = 10 ## immature herbaceous contribute to shade in the same way than mature herbaceous
  IMM_SIZE[which(mat.PFG.succ$type == "C")] = 5 ## immature chamaephytes contribute to shade half less than mature herbaceous
  IMM_SIZE[which(mat.PFG.succ$type == "P")] = 1 ## immature phanerophytes contribute to shade only by 10 % of their full capacity
  IMM_SIZE[which(mat.PFG.succ$type == "H" & STRATA == 2)] = 8 ## intermediate percentage for herbaceous in stratum 2
  IMM_SIZE[which(mat.PFG.succ$type == "H" & STRATA > 2)] = 5 ## intermediate percentage for herbaceous in stratum > 2
  IMM_SIZE[which(mat.PFG.succ$type == "C" & STRATA == 1)] = 10 ## immature chamaephytes in 1st stratum contribute to shade in the same way than mature chamaephytes
  IMM_SIZE[which(mat.PFG.succ$type == "P" & mat.PFG.succ$height < 1000)] = 5 ## immature phanerophytes with height < 10m contribute to shade half less than mature phanerophytes
  
  #################################################################################################
  
  ## GET CHANGE STRATA AGES
  ## Logistic growth curve with 2 points to parameterize it :
  ## at age = maturity/2, height = IMM_SIZE * height	
  ## at age = longevity, height = height
  CHANG_STR_AGES = matrix(0, nrow = length(STRATA_LIMITS), ncol = no.PFG)
  CHANG_STR_AGES[2:length(STRATA_LIMITS), ] = 10000
  for (i in 1:no.PFG)
  {
    ## If not in first stratum / herbaceous (or potentially chamaephytes) :
    if (!(IMM_SIZE[i] == 10))
    {
      k = -log(1 - IMM_SIZE[i] / 10) / (MATURITY[i] / 2)
      A = 1:LONGEVITY[i]
      
      ## negative binomiale curve
      H = mat.PFG.succ$height[i] * (1 - exp(-k * A))
      
      # calculation of transition ages depending on strata heights
      for (str in 2:length(STRATA_LIMITS)) {
        age.brk = A[which(H >= STRATA_LIMITS[str])][1]
        CHANG_STR_AGES[str, i] = ifelse(is.na(age.brk), CHANG_STR_AGES[str, i], age.brk)
      }
    }
    # else if (mat.PFG.succ$height[i] > STRATA_LIMITS[2])
    # {
    #   CHANG_STR_AGES[2, i] = 0 ## direct into strata max
    # }
  }
  
  #################################################################################################
  
  ## GET DISPERSAL MODE : is PFG widely dispersed ?
  ## 0 = no
  ## 1 = yes
  # WIDE_DISPERS = rep(0, no.PFG)
  
  ## GET DISPERSAL MODULE
  ## 0 = no dispersal
  ## 1 = homogeneous dispersal within the d50, d99 and ldd circles
  ## 2 = negative exponential kernel within the d50, d99 and ldd circles
  ## 3 = negative exponential kernel + probability decreasing with distance within the d50, d99 and ldd circles
  ## 4 = homogeneous dispersal EVERYWHERE (!not available YET!)
  MODE_DISPERS = mat.PFG.succ$dispersal
  
  #################################################################################################
  
  ## GET GERMINATION RATE depending on light conditions
  ##   = these rates should express a deviation from the
  ##     germination rate in optimal conditions (=100%)
  ##   = for each light condition (Low, Medium, High)
  ## REMOVE : 3 levels : 1 = Low, 2 = Medium or 3 = High
  ## 11 levels : 0 = 0 %
  ##             1 = 10 %
  ##             2 = 20 %
  ##             3 = 30 %
  ##             4 = 40 %
  ##             5 = 50 %
  ##             6 = 60 %
  ##             7 = 70 %
  ##             8 = 80 %
  ##             9 = 90 %
  ##             10 = 100 %
  ACTIVE_GERM = matrix(0, nrow = 3, ncol = no.PFG)
  ## woody species have little variation in germination rate depending on light conditions
  ACTIVE_GERM[, which(mat.PFG.succ$type %in% c("C", "P"))] = 9
  ## herbaceous germinate less in the shadow
  ACTIVE_GERM[1, which(mat.PFG.succ$type == "H")] = 5 ## low light conditions
  ACTIVE_GERM[2, which(mat.PFG.succ$type == "H")] = 8 ## medium light conditions
  ACTIVE_GERM[3, which(mat.PFG.succ$type == "H")] = 9 ## high light conditions
  
  #################################################################################################
  
  ## GET SHADE TOLERANCE
  ##    = for each life stage (Germinant, Immature, Mature)
  ##    = and for each light condition (Low, Medium, High)
  ## 0 = non tolerant
  ## 1 = tolerant
  SHADE_TOL = matrix(0, nrow = 3 * 3, ncol = no.PFG)
  
  for (i in 1:no.PFG){
    ## Low light condition
    if (mat.PFG.succ$light[i] <= 3)
    {
      SHADE_TOL[c(1, 4, 7), i] = 1
    }
    ## Medium light condition
    if (mat.PFG.succ$light[i] <= 4)
    {
      SHADE_TOL[c(2, 5, 8), i] = 1
    }
    ## High light condition
    if (mat.PFG.succ$light[i] >= 3)
    {
      SHADE_TOL[c(3, 6, 9), i] = 1
    }
  }
  
  ## All germinants are assumed to be tolerant to Low and Medium light
  SHADE_TOL[c(1, 2),] = 1
  ## All mature trees are assumed to be tolerant to High light in the upper strata
  SHADE_TOL[c(9), which(mat.PFG.succ$type == "P")] = 1
  ## All immature trees that grow in the penultimate stratum are assumed to be tolerant to High light
  SHADE_TOL[c(6), which(mat.PFG.succ$type == "P" & CHANG_STR_AGES[nrow(CHANG_STR_AGES) - 1,] < MATURITY)] = 1
  
  #################################################################################################
  
  ## GET SEED POOLS (active and dormant) LIFE SPAN
  ##   = available seeds will exponentially decrease according to seed pool life span parameter
  SEED_POOL_LIFE = matrix(0, nrow = 2, ncol = no.PFG)
  
  ## GET SEED DORMANCY
  ## 0 = no
  ## 1 = yes
  SEED_DORMANCY = rep(0, no.PFG)
  
  #################################################################################################

  names.params.list = get("NAME")
  names.params.list.sub = c("NAME"
                            , "TYPE"
                            , "LONGEVITY"
                            , "MATURITY"
                            , "STRATA"
                            , "MAX_ABUNDANCE"
                            , "IMM_SIZE"
                            , "CHANG_STR_AGES"
                            , "MODE_DISPERS"
                            , "ACTIVE_GERM"
                            , "SHADE_TOL"
                            , "SEED_POOL_LIFE"
                            , "SEED_DORMANCY")
  
  params.list = lapply(names.params.list.sub, function(x) { return(get(x)) })
  
  params.csv = t(do.call(rbind, params.list))
  colnames(params.csv) = c("NAME"
                           , "TYPE"
                           , "LONGEVITY"
                           , "MATURITY"
                           , "STRATA"
                           , "MAX_ABUNDANCE"
                           , "IMM_SIZE"
                           , paste0("CHANG_STR_AGES_to_str_", 1:length(STRATA_LIMITS))
                           , "MODE_DISPERS"
                           , paste0("ACTIVE_GERM_for_", c("L", "M", "H"))
                           , paste0("SHADE_TOL_for_",
                                    c("GeL", "GeM", "GeH", "ImL", "ImM", "ImH", "MaL", "MaM", "MaH"))
                           , paste0("SEED_POOL_LIFE_", c("active", "dormant"))
                           , "SEED_DORMANCY")
  
  write.table(params.csv
            , file = paste0(name.simulation, "/DATA/PFGS/SUCC_COMPLETE_TABLE.csv")
            , row.names = F
            , col.names = T)
  
  #################################################################################################
  
  params.list = lapply(1:no.PFG, function(x) {
    lapply(names.params.list.sub, function(y) {
      val = get(y)
      if (is.null(nrow(val))){
        val = val[x]
      } else {
        val = val[, x]
      }
      return(val)
    })
  })

  for (i in 1:length(params.list)) {
    params = params.list[[i]]
    names(params) = names.params.list.sub

    .createParams(params.file = paste0(name.simulation,
                                       "/DATA/PFGS/SUCC/SUCC_",
                                       names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }
  
  cat("\n> Done!\n")
  cat("\n  Complete table of information about PFG succession parameters can be find in "
      , paste0(name.simulation, "/DATA/PFGS/"), "folder.")
  cat("\n")
  
}

