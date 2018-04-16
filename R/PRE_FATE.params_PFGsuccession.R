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
##' @param mat.PFG.succ a \code{data.frame} with 6 columns : PFG, type, height,
##' maturity, longevity, dispersal
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
##'                                                         , type = c("H", "C", "P")  
##'                                                         , height = c(50, 20, 100)
##'                                                         , maturity = c(5, 5, 12)
##'                                                         , longevity = c(25, 9, 54)
##'                                                         , dispersal = c(1, 1, 1)))
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_PFGsuccession = function(
  name.simulation
  , mat.PFG.succ
){
  
  if (!dir.exists(paste0(name.simulation, "/DATA/PFGS/SUCC/"))){
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  }
  if (!is.data.frame(mat.PFG.succ))
  {
    stop("Wrong type of data!\n `mat.PFG.succ` must be a data.frame")
  }
  if (nrow(mat.PFG.succ) == 0 || ncol(mat.PFG.succ) != 6)
  {
    stop("Wrong dimension(s) of data!\n `mat.PFG.succ` does not have the appropriate number of rows (>0)
           or columns (PFG, type, height, maturity, longevity, dispersal)")
  }
  if (ncol(mat.PFG.succ) == 6)
  {
    if (sum(colnames(mat.PFG.succ) == c("PFG", "type","height", "maturity", "longevity", "dispersal")) == 6)
    {
      mat.PFG.succ = mat.PFG.succ[ , c("PFG", "type","height", "maturity", "longevity", "dispersal")]
    } else {
      stop("Wrong type of data!\n Column names of `mat.PFG.succ` must be `PFG`, `type`, height`, `maturity`, `longevity` and `dispersal`")
    }
  }
  if (length(unique(mat.PFG.succ$PFG)) < nrow(mat.PFG.succ)){
    stop("Wrong type of data!\n Column `PFG` of mat.PFG.succ` must contain different values")
  }
  if (!is.numeric(mat.PFG.succ$height) ||
      !is.numeric(mat.PFG.succ$maturity) ||
      !is.numeric(mat.PFG.succ$longevity) ||
      !is.numeric(mat.PFG.succ$dispersal)) {
    stop("Wrong type of data!\n Columns `height`, `maturity`, `longevity` and `dispersal` of `mat.PFG.succ` must contain numeric values")
  }
  if (length(which(is.na(mat.PFG.succ$height))) > 0 ||
      length(which(is.na(mat.PFG.succ$maturity))) > 0 ||
      length(which(is.na(mat.PFG.succ$longevity))) > 0 ||
      length(which(is.na(mat.PFG.succ$dispersal))) > 0) {
    stop("Wrong type of data!\n Columns `height`, `maturity`, `longevity` and `dispersal` of `mat.PFG.succ` must not contain NA values")
  }
  if (sum(mat.PFG.succ$dispersal %in% seq(0,3)) < nrow(mat.PFG.succ)){
    stop("Wrong type of data!\n Column `dispersal` of mat.PFG.succ` must contain values between 0 and 3")
  }
  
  no.PFG = nrow(mat.PFG.succ)

  ## GET MATURITY AGE values
  MATURITY = mat.PFG.succ$maturity
  
  ## GET LONGEVITY values
  ## Death precedes seed productivity in the model thus longevity param = longevity + 1
  LONGEVITY = mat.PFG.succ$longevity + 1
  
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
  
  ## GET SHADE TOLERANCE
  ##    = for each life stage (Germinant, Immature, Mature)
  ##    = and for each light condition (Low, Medium, High)
  ## 0 = non tolerant
  ## 1 = tolerant
  SHADE_TOL = matrix(0, nrow = 3 * 3, ncol = no.PFG)
  
  # ## parameterisation according to Landolt classes and parcimony
  # for (i in 1:n){
  #   if(dat$light[i]==2) {
  #     SHADE_TOL["GeL",i] <- SHADE_TOL["ImL",i] <- SHADE_TOL["MaL",i] <- 1
  #     SHADE_TOL["GeM",i] <- SHADE_TOL["ImM",i] <- SHADE_TOL["MaM",i] <- 1
  #     SHADE_TOL["GeH",i] <- SHADE_TOL["ImH",i] <- SHADE_TOL["MaH",i] <- 0
  #   }
  #   if(dat$light[i]==3) {
  #     SHADE_TOL["GeL",i] <- SHADE_TOL["ImL",i] <- SHADE_TOL["MaL",i] <- 1
  #     SHADE_TOL["GeM",i] <- SHADE_TOL["ImM",i] <- SHADE_TOL["MaM",i] <- 1
  #     SHADE_TOL["GeH",i] <- SHADE_TOL["ImH",i] <- SHADE_TOL["MaH",i] <- 1
  #   }
  #   if(dat$light[i]==4) {
  #     SHADE_TOL["GeL",i] <- SHADE_TOL["ImL",i] <- SHADE_TOL["MaL",i] <- 0
  #     SHADE_TOL["GeM",i] <- SHADE_TOL["ImM",i] <- SHADE_TOL["MaM",i] <- 1
  #     SHADE_TOL["GeH",i] <- SHADE_TOL["ImH",i] <- SHADE_TOL["MaH",i] <- 1
  #   }
  #   if(dat$light[i]==5) {
  #     SHADE_TOL["GeL",i] <- SHADE_TOL["ImL",i] <- SHADE_TOL["MaL",i] <- 0
  #     SHADE_TOL["GeM",i] <- SHADE_TOL["ImM",i] <- SHADE_TOL["MaM",i] <- 0
  #     SHADE_TOL["GeH",i] <- SHADE_TOL["ImH",i] <- SHADE_TOL["MaH",i] <- 1
  #   }
  # }
  # 
  # 
  # ## we assum that all germinants are tolerant to LOW and MEDIUM light
  # SHADE_TOL["GeL",] <- SHADE_TOL["GeM",] <- 1
  # 
  # ## we have to adjust so that big trees continue growing when they are in the upper strata
  # SHADE_TOL["MaH",which(dat$type=="P")] <- 1 # last stratum in which all trees must tolerate the light
  # # the stratum before the last stratum must also tolerate the light (for the highest trees) because there will never be enough shadow in this stratum
  # SHADE_TOL["ImH",which(dat$type=="P" & CHANG_STR_AGES[3,] < dat$maturity )] <- 1 
  # ## Marvin: Maybe change CHANG_STR_AGES[3,] because we have more strata now??
  # 
  # ## update SHADE_TOL rownames
  # rownames(SHADE_TOL) <- paste("SHADE_TOL", "_for_", rownames(SHADE_TOL), sep="")
  
  
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

