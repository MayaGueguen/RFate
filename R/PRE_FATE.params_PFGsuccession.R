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
##' @param name.simulation a \code{string} that corresponds to the main 
##' directory or simulation name of the \code{FATE-HD} simulation
##' @param mat.PFG.succ a \code{data.frame} with 5 columns : PFG, type, height,
##' maturity, longevity
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} that
##' corresponds to the name of the folder that will be created into the 
##' \code{name.simulation/DATA/PFGS/SUCC/} directory to store the results
##' 
##' 
##' @details
##' 
##' The core module of \code{FATE-HD} allows the user to simulate a primary 
##' vegetation succession based on life history traits.
##' Several parameters are required for each PFG in order to set up this life 
##' cycle :
##' 
##' \describe{
##'   \item{type}{or life-form, based on Raunkier. It should be either \code{H} 
##'   (herbaceous), \code{C} (chamaephyte) or \code{P} (phanerophyte) for now}
##'   \item{height}{the maximum or average height that reach the PFG}
##'   \item{maturity}{the age from which the PFG can reproduce}
##'   \item{longevity}{the maximum or average lifespan of the PFG \cr \cr}
##' }
##' 
##' 
##' 
##' 
##' These values will allow to calculate or define a set of characteristics for
##' each PFG :
##' 
##' \describe{
##'   \item{STRATA_LIMITS}{= the height values that define each stratum.\cr \cr
##'   The steps are detailed below and try to homogenize the number of PFG
##'   within each stratum :
##'   \itemize{
##'     \item the average number of PFG per stratum should be close to 
##'     the square root of the total number of PFG (\code{no.PFG.perStrata})
##'     \item \code{strata limits} should go exponentially and will be selected
##'     among \cr \code{0, 20, 50, 150, 400, 1000, 2000, 5000, 10000}
##'     \item PFG are divided according to their \code{height} and these 
##'     \code{strata limits} and then grouped in order to have per stratum a 
##'     number of PFG \code{>= (no.PFG.perStrata - 2)}
##'   }
##'   }
##'   \item{STRATA}{the maximum stratum that each PFG can reach}
##'   \item{MAX_ABUNDANCE}{= maximum abundance of mature PFG in favorable 
##'   conditions \cr \cr
##'   It can be seen as a proxy of maximum carrying capacity (\emph{and 
##'   therefore as a proxy a quantity of shade produced as well, if the light
##'   module is activated}), and it is defined according to the number of
##'   strata potentially occupied by a PFG and its life form :
##'   \itemize{
##'     \item herbaceous take little place (\emph{and make little shade}) (1),
##'     \cr chamaephytes take medium place (\emph{and make medium shade}) (2)
##'     \cr and phanerophytes take a lot of place (\emph{and make lot of shade})
##'     (3)
##'     \item all plants in first stratum take little place (\emph{and make 
##'     little shade}) (1)
##'     \item plants other than herbaceous in stratum 2 take medium place 
##'     (\emph{and make medium shade}) (2)
##'     \item herbaceous in stratum > 2 take medium place (\emph{and make 
##'     medium shade}) (2)
##'     \item chamaephytes in stratum > 3 take lot of place (\emph{and make 
##'     lot of shade}) (3)
##'   }
##'   }
##'   \item{IMM_SIZE}{= relative size of immature versus mature plants \cr
##'   \itemize{
##'     \item immature herbaceous take as much space as mature herbaceous \cr
##'     (\emph{and contribute to shade in the same way}) (100 \%)
##'     \item immature chamaephytes take two times less space than mature
##'     chamaephytes \cr (\emph{and contribute to shade half less}) (50 \%)
##'     \item immature phanerophytes take only 10 \% of their full space
##'     (\emph{and shade}) capacity
##'     
##'     \item intermediate percentages for herbaceous in stratum 2 (80 \%) and 
##'     in stratum > 2 (50 \%)
##'     \item immature chamaephytes in 1st stratum take as much space as mature
##'     chamaephytes \cr (\emph{and contribute to shade in the same way})
##'     (100 \%)
##'     \item immature phanerophytes with height < 10m take two times less space
##'     than mature phanerophytes \cr (\emph{and contribute to shade half less})
##'     (50 \%)
##'   }
##'   }
##'   \item{CHANG_STR_AGES}{= at what age each PFG goes into the upper stratum.
##'   \cr \cr
##'   It is defined using a logistic growth curve with 2 points to parameterize
##'   it :
##'   \enumerate{
##'     \item at \code{age = maturity/2}, \code{height = IMM_SIZE * height}
##'     \item at \code{age = longevity}, \code{height = height}
##'   }
##'   }
##'   \item{POTENTIAL_FECUNDITY}{= maximum number of seeds produced by the PFG.
##'   \cr \cr
##'   By default, defined to the same value for all PFG (100), but can be
##'   changed by hand directly into the produced files.
##'   }
##' }
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
##'   \item MAX_ABUNDANCE : the maximal (qualitative) abundance / space that 
##'   the PFG is able to produce / occupy \cr (qualitative) 
##'   \emph{(1: Low 2: Medium 3: High)}
##'   \item IMM_SIZE : the relative size of the immature PFG \cr
##'   \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 
##'   8: 80\% 9: 90\% 10: 100\%)}
##'   \item CHANG_STR_AGES : the ages at which the PFG goes in the upper stratum
##'   \cr \emph{(in years, put a value higher than the PFG life span if it is 
##'   not supposed to rise a stratum)}
##'   \item SEED_POOL_LIFE : the maximal number of years seeds are able to
##'   survive (for active and dormant pool)
##'   \item SEED_DORMANCY : are the seeds dormant or not \emph{(0: No 1: Yes)}
##'   \item POTENTIAL_FECUNDITY : the maximum number of seeds produced by the
##'   PFG \cr \cr
##' }
##' 
##' A \code{SUCC_COMPLETE_TABLE.csv} file summarizing information for all groups
##' into the \code{name.simulation/DATA/PFGS/} directory.  
##' This file can be used to parameterize the disturbance files.
##' 
##' If the \code{opt.folder.name} has been used, the files will be into the folder
##' \code{name.simulation/DATA/PFGS/SUCC/opt.folder.name/}
##' 
##' 
##' @keywords FATE, simulation, height, longevity, maturity
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}}
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
##'                                                         , longevity = c(12, 200, 25, 4, 110, 70)))
##' 
##' @export
##' 
##' @importFrom utils write.table
##'
## END OF HEADER ###############################################################


PRE_FATE.params_PFGsuccession = function(
  name.simulation
  , mat.PFG.succ
  , opt.folder.name = NULL
){
  
  .testParam_existFolder(name.simulation, "DATA/PFGS/SUCC/")
  
  if (.testParam_notDf(mat.PFG.succ))
  {
    .stopMessage_beDataframe("mat.PFG.succ")
  }
  if (nrow(mat.PFG.succ) == 0 || ncol(mat.PFG.succ) != 5)
  {
    .stopMessage_numRowCol("mat.PFG.succ", c("PFG", "type","height", "maturity", "longevity"))
  }
  if (ncol(mat.PFG.succ) == 5)
  {
    if (sum(colnames(mat.PFG.succ) == c("PFG", "type","height", "maturity", "longevity")) == 5)
    {
      mat.PFG.succ = mat.PFG.succ[ , c("PFG", "type","height", "maturity", "longevity")]
    } else {
      .stopMessage_columnNames("mat.PFG.succ", c("PFG", "type","height", "maturity", "longevity"))
    }
  }
  mat.PFG.succ$PFG = as.character(mat.PFG.succ$PFG)
  mat.PFG.succ$type = as.character(mat.PFG.succ$type)
  if (length(which(is.na(mat.PFG.succ$PFG))) > 0 ||
      length(unique(mat.PFG.succ$PFG)) < nrow(mat.PFG.succ)){
    stop("Wrong type of data!\n Column `PFG` of `mat.PFG.succ` must contain different values")
  }
  if (.testParam_notChar(mat.PFG.succ$PFG))
  {
    .stopMessage_beChar("mat.PFG.succ$PFG")
  }
  if (.testParam_notInChar(mat.PFG.succ$type, inList = c("H", "C", "P")))
  {
    .stopMessage_content("mat.PFG.succ$type", c("H", "C", "P"))
  }
  if (!is.numeric(mat.PFG.succ$height) ||
      !is.numeric(mat.PFG.succ$maturity) ||
      !is.numeric(mat.PFG.succ$longevity))
  {
    .stopMessage_columnNumeric("mat.PFG.succ", c("height", "maturity", "longevity"))
  }
  if (length(which(is.na(mat.PFG.succ$height))) > 0 ||
      length(which(is.na(mat.PFG.succ$maturity))) > 0 ||
      length(which(is.na(mat.PFG.succ$longevity))) > 0)
  {
    .stopMessage_columnNoNA("mat.PFG.succ", c("height", "maturity", "longevity"))
  }
  ## CHECKS for parameter opt.folder.name
  if (is.null(opt.folder.name)){
    opt.folder.name = ""
  } else if (!is.null(opt.folder.name) && !is.character(opt.folder.name)){
    warning("As `opt.folder.name` does not contain character value, it will be ignored")
    opt.folder.name = ""
  } else if (nchar(opt.folder.name) > 0){
    opt.folder.name = paste0(opt.folder.name, "/")
    dir.create(paste0(name.simulation, "/DATA/PFGS/SUCC/", opt.folder.name))
  } else {
    opt.folder.name = ""
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
  if (no.PFG == 1)
  {
    categ = which(categories.table == 1)
    STRATA_LIMITS = c(0, strata.limits[categ], strata.limits[categ + 1])    
  } else
  {
    STRATA_LIMITS = 0
    tmp = categories.table[1]
    for (categ in 2:length(strata.limits))
    {
      if (tmp >= max(c(1, (no.PFG.perStrata - 2))))
      {
        STRATA_LIMITS = c(STRATA_LIMITS, strata.limits[categ])
        tmp = categories.table[categ]
      } else 
      {
        tmp = tmp + categories.table[categ]
      }
    }
  }
  STRATA_LIMITS = sort(unique(STRATA_LIMITS))
  # barplot(table(cut(mat.PFG.succ$height, breaks = STRATA_LIMITS)))
  
  ## GET STRATA attribution
  STRATA = sapply(mat.PFG.succ$height, function(h) {
    max(which(STRATA_LIMITS < h), na.rm = T)
  })
  
  no.strata = max(STRATA)
  
  cat("\n ############## STRATA INFORMATIONS ############## \n")
  cat("\n Number of strata : ", no.strata)
  cat("\n Height limits of selected strata : ", STRATA_LIMITS)
  cat("\n Number of PFG within each stratum : ", table(cut(mat.PFG.succ$height, breaks = STRATA_LIMITS)))
  cat("\n")
  
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
  CHANG_STR_AGES = matrix(0, nrow = no.strata, ncol = no.PFG)
  if (no.strata > 1)
  {
    CHANG_STR_AGES[2:no.strata, ] = 10000
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
        for (str in 2:no.strata) {
          age.brk = A[which(H >= STRATA_LIMITS[str])][1]
          CHANG_STR_AGES[str, i] = ifelse(is.na(age.brk), CHANG_STR_AGES[str, i], age.brk)
        }
      }
      # else if (mat.PFG.succ$height[i] > STRATA_LIMITS[2])
      # {
      #   CHANG_STR_AGES[2, i] = 0 ## direct into strata max
      # }
    }
  }
  
  #################################################################################################
  
  ## GET SEED POOLS (active and dormant) LIFE SPAN
  ##   = available seeds will exponentially decrease according to seed pool life span parameter
  SEED_POOL_LIFE = matrix(0, nrow = 2, ncol = no.PFG)
  
  ## GET SEED DORMANCY
  ## 0 = no
  ## 1 = yes
  SEED_DORMANCY = rep(0, no.PFG)
  
  ## GET POTENTIAL FECUNDITY
  POTENTIAL_FECUNDITY = rep(100, no.PFG)
  
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
                            , "SEED_POOL_LIFE"
                            , "SEED_DORMANCY"
                            , "POTENTIAL_FECUNDITY")
  
  params.list = lapply(names.params.list.sub, function(x) { return(get(x)) })
  
  params.csv = t(do.call(rbind, params.list))
  colnames(params.csv) = c("NAME"
                           , "TYPE"
                           , "LONGEVITY"
                           , "MATURITY"
                           , "STRATA"
                           , "MAX_ABUNDANCE"
                           , "IMM_SIZE"
                           , paste0("CHANG_STR_AGES_to_str_", 1:no.strata, "_", STRATA_LIMITS[1:no.strata])
                           , paste0("SEED_POOL_LIFE_", c("active", "dormant"))
                           , "SEED_DORMANCY"
                           , "POTENTIAL_FECUNDITY")
  
  write.table(params.csv
            , file = paste0(name.simulation
                            , "/DATA/PFGS/"
                            , ifelse(opt.folder.name == "", "", sub("/$", "_", opt.folder.name))
                            , "SUCC_COMPLETE_TABLE.csv")
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

    .createParams(params.file = paste0(name.simulation
                                       , "/DATA/PFGS/SUCC/"
                                       , opt.folder.name
                                       , "SUCC_"
                                       , names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }
  
  cat("\n> Done!\n")
  cat("\n  Complete table of information about PFG succession parameters can be find in "
      , paste0(name.simulation, "/DATA/PFGS/"), "folder.")
  cat("\n")
  
}

