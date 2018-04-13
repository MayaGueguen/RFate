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
##' @param mat.PFG.succ a \code{data.frame} with 5 columns : PFG, type, height, maturity, longevity
##' 
##' 
##' @details
##' 
##' For each PFG, 
##' 
##' 
##' @return A \code{.txt} file per PFG into the \code{name.simulation/DATA/PFGS/SUCC/}
##' directory with the following parameters :
##' 
##' \itemize{
##'   \item MATURITY : the maturity age of the PFG (in years)
##'   \item LONGEVITY : the PFG life span (in years)
##'   \item MAX_ABUNDANCE : the maximal (qualitative) shade that the PFG is able to produce (1: Low 2: Medium 3: High)
##'   \item IMM_SIZE : the relative size of the immature PFG (0: 0% 1: 10% 2: 50% 3: 90% 4: 100% 5: 40% 6: 80%)
##'   \item CHANG_STR_AGES : the ages at which the PFG goes in the upper stratum (in years, put a value higher than the PFG life span if it is not supposed to rise a stratum)
##'   \item WIDE_DISPERS : is the PFG able to disperse everywhere (i.e. no dispersal limits) (0: No 1: Yes)
##'   \item ACTIVE_GERM : the germination rates depending on light conditions (0: 0% 1: 10% 2: 50% 3: 90% 4: 100% 5: 40% 6: 80%)
##'   \item SHADE_TOL : the PFG shade tolerance table (in a single row). This is a vector of 9 numbers (0: Die 1: Survive) corresponding to the ability of the PFG to survive or not at different life stages (Germinate (Ge), Immature (Im), Mature (Ma)) under different light conditions (Low (L), Medium (M) or High (H) light resources). These parameters should be given in this order : GeL, GeM, GeH, ImL, ImM, ImH, MaL, MaM, MaH.
##'   \item SEED_POOL_LIFE : the maximal number of years seeds are able to survive
##'   \item SEED_DORMANCY
##' }
##' 
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create a Namespace_constants parameter file
##' PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
##'                             , mat.PFG.succ = data.frame(PFG = c("PFG1", "PFG2", "PFG3")
##'                                                         , height = c()
##'                                                         , maturity = c()
##'                                                         , longevity = c()))
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_PFGsuccession = function(
  name.simulation
  , mat.PFG.disp
){
  
  if (!dir.exists(paste0(name.simulation, "/DATA/PFGS/SUCC/"))){
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  }
  if (!is.data.frame(mat.PFG.succ))
  {
    stop("Wrong type of data!\n `mat.PFG.succ` must be a data.frame")
  }
  if (nrow(mat.PFG.succ) == 0 || ncol(mat.PFG.succ) != 5)
  {
    stop("Wrong dimension(s) of data!\n `mat.PFG.succ` does not have the appropriate number of rows (>0)
           or columns (PFG, type, height, maturity, longevity)")
  }
  if (ncol(mat.PFG.succ) == 5)
  {
    if (sum(colnames(mat.PFG.succ) == c("PFG", "type","height", "maturity", "longevity")) == 5)
    {
      mat.PFG.succ = mat.PFG.succ[ , c("PFG", "type","height", "maturity", "longevity")]
    } else {
      stop("Wrong type of data!\n Column names of `mat.PFG.succ` must be `PFG`, `type`, height`, `maturity` and `longevity`")
    }
  }
  if (length(unique(mat.PFG.succ$PFG)) < nrow(mat.PFG.succ)){
    stop("Wrong type of data!\n Column `PFG` of mat.PFG.succ` must contain different values")
  }
  if (!is.numeric(mat.PFG.succ$height) ||
      !is.numeric(mat.PFG.succ$maturity) ||
      !is.numeric(mat.PFG.succ$longevity)) {
    stop("Wrong type of data!\n Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must contain numeric values")
  }
  if (length(which(is.na(mat.PFG.succ$height))) > 0 ||
      length(which(is.na(mat.PFG.succ$maturity))) > 0 ||
      length(which(is.na(mat.PFG.succ$longevity))) > 0) {
    stop("Wrong type of data!\n Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must not contain NA values")
  }
  
  no.PFG = nrow(mat.PFG.succ)

  ## GET MATURITY AGE values
  MATURITY = mat.PFG.succ$maturity
  # names(MATURITY) = mat.PFG.succ$PFG
  
  ## GET LONGEVITY values
  ## Death precedes seed productivity in the model thus longevity param = longevity + 1
  LONGEVITY = mat.PFG.succ$longevity + 1
  # names(LONGEVITY) = mat.PFG.succ$PFG
  
  ## GET height strata limits (for light competition and PFG growth)
  ## n strata (+ germinants = 0)
  no.PFG.perStrata = round(sqrt(no.PFG))
  strata.limits = c(round(10 * seq(0, 6, 0.5) ^ 2), 10000)
  categories = cut(mat.PFG.succ$height, breaks = strata.limits)
  categories.table = table(categories)
  STRATA_LIMITS = 0
  tmp = categories.table[1]
  for (categ in 2:length(strata.limits))
  {
    if (tmp >= (no.PFG.perStrata - 2))
    {
      STRATA_LIMITS = c(STRATA_LIMITS, strata.limits[categ])
      tmp = categories.table[categ]
    } else 
    {
      tmp = tmp + categories.table[categ]
    }
  }
  # barplot(table(cut(mat.PFG.succ$height, breaks = STRATA_LIMITS)))
  
  ## GET STRATA attribution
  STRATA = sapply(mat.PFG.succ$height, function(h) {
    max(which(STRATA_LIMITS < h), na.rm = T)
  })
  
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
  # names(MAX_ABUNDANCE) = mat.PFG.succ$PFG
  
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
  
  # params.list = lapply(1:nrow(mat.PFG.disp), function(x) {
  #   list(as.integer(mat.PFG.disp[x , c("d50", "d99", "ldd")]))
  # })
  # names.params.list = mat.PFG.disp$PFG
  # names.params.list.sub = "DISPERS_DIST"
  # 
  # for (i in 1:length(params.list)) {
  #   params = params.list[[i]]
  #   names(params) = names.params.list.sub
  #   
  #   .createParams(params.file = paste0(name.simulation,
  #                                      "/DATA/PFGS/DISP/DISP_",
  #                                      names.params.list[i],
  #                                      ".txt")
  #                 , params.list = params)
  # }
}

