### HEADER #####################################################################
##' @title Mont Blanc dataset
##' 
##' @name MontBlanc
##'
##' @author Maya Guéguen
##' 
##' @description MontBlanc dataset
##' 
##' @format A \code{list} object with 3 elements :
##' 
##' \itemize{
##'   \item 
##'   \item 
##'   \item 
##' }
##' 
## END OF HEADER ###############################################################
"MontBlanc"

### HEADER #####################################################################
##' @title Bauges simulation folder
##' 
##' @name FATE_Bauges
##'
##' @author Maya Guéguen
##' 
##' @description Bauges simulation folder
##' 
##' @format A \code{list} object with 2 elements :
##' 
##' \itemize{
##'   \item 
##'   \item 
##' }
##' 
## END OF HEADER ###############################################################
"FATE_Bauges"


### HEADER #####################################################################
##' @title Ecrins National Park (PNE) data : building of Plant Functional Groups
##' 
##' @name PNE_PFG
##'
##' @author Maya Guéguen
##' 
##' @description This dataset contains elements to create the Plant Functional 
##' Groups (PFG) over the Ecrins National Park (PNE) 
##' (\emph{\href{https://mayagueguen.github.io/FATE-WEBSITE/papers/Boulangeat_2012_GCB.pdf}{Boulangeat et al. 2012 GCB}}).
##' 
##' @format A \code{list} object with 6 elements to help building 
##'   the Plant Functional Group :
##'   
##'   \describe{
##'     \item{sp.observations}{a \code{data.frame} of dimension \code{168313 x 7} \cr
##'     containing releves data about plant species in the PNE \cr
##'     to be used with the \link{PRE_FATE.selectDominant} function \cr
##'     \itemize{
##'       \item \strong{sites} : sites ID
##'       \item \strong{X} : x-axis coordinates in Lambers (lcc)
##'       \item \strong{Y} : y-axis coordinates in Lambers (lcc)
##'       \item \strong{habitat} : habitat ID from 
##'       \href{http://www.ecrins-parcnational.fr/sites/ecrins-parcnational.com/files/fiche_doc/12083/2006-atlas-delphine.pdf}{DELPHINE} :
##'       \itemize{
##'         \item 0 : glaciers and eternal snows
##'         \item 31 : uncolonized rocks
##'         \item 40 : lawns and meadows
##'         \item 50 : low moors
##'         \item 60 : open areas, brush
##'         \item 70 : semi-closed areas
##'         \item 81 : closed areas
##'         \item 83 : forests
##'       }
##'       \item \strong{species} : species ID
##'       \item \strong{abund_BB} : Braun-Blanquet abundance \cr 
##'       (see \code{\link{PRE_FATE.abundBraunBlanquet}} function for details)
##'       \item \strong{abund} : relative abundance obtained from the 
##'       \code{abund_BB} column with the 
##'       \code{\link{PRE_FATE.abundBraunBlanquet}} function
##'     }
##'     }
##'     \item{dom.traits}{\code{data.frame} of dimension \code{359 x 6} \cr
##'     containing traits for dominant species \cr
##'     to be used with the \link{PRE_FATE.speciesDistance} function \cr
##'     \itemize{
##'       \item \strong{species} : species ID
##'       \item \strong{GROUP} : rough generalization of Raunkier life-forms :
##'       \itemize{
##'         \item H : herbaceous
##'         \item C : chamaephytes
##'         \item P : phanerophytes
##'       }
##'       \item \strong{height} : mean plant height (cm)
##'       \item \strong{dispersal} : classes (from 1 to 7) based on 
##'       dispersal distances and types (Vittoz & Engler)
##'       \item \strong{palatability} : classes (from 0 to 3) (CBNA)
##'       \item \strong{light} : Ellenberg indicator value for light (from 1 
##'       to 9)
##'     }
##'     }
##'     \item{dom.dist_overlap}{\code{matrix} of dimension \code{358 x 358} \cr
##'     containing niche overlap distance for dominant species \cr
##'     to be used with the \link{PRE_FATE.speciesDistance} function \cr}
##'     \item{nb.clusters}{\code{vector} of length \code{3} \cr
##'     number of groups kept for each life-form class, obtained by cutting the 
##'     hierarchical tree obtained from species distances}
##'     \item{dom.determ}{\code{data.frame} of dimension \code{359 x 5} \cr
##'     containing dominant species information relative to PFG \cr
##'     obtained from the \link{PRE_FATE.speciesClustering_step2} function \cr
##'     \itemize{
##'       \item \strong{species} : species ID
##'       \item \strong{name} : species name (taxonomic)
##'       \item \strong{GROUP} : rough generalization of Raunkier life-forms :
##'       \itemize{
##'         \item H : herbaceous
##'         \item C : chamaephytes
##'         \item P : phanerophytes
##'       }
##'       \item \strong{PFG} : name of assigned Plant Functional Group
##'       \item \strong{determinant} : is the species kept as determinant 
##'       species within the PFG ? (see \link{PRE_FATE.speciesClustering_step2} 
##'       function for details)
##'     }
##'     }
##'     \item{PFG.traits}{\code{data.frame} of dimension \code{24 x 14} \cr
##'     containing traits for plant functional groups \cr
##'     obtained from the \link{PRE_FATE.speciesClustering_step3} function \cr
##'     \itemize{
##'       \item \strong{PFG_name} : full descriptive Plant Functional Group name
##'       \item \strong{PFG} : Plant Functional Group short name
##'       \item \strong{type} : rough generalization of Raunkier life-forms :
##'       \itemize{
##'         \item H : herbaceous
##'         \item C : chamaephytes
##'         \item P : phanerophytes
##'       }
##'       \item \strong{strata} : maximum height stratum that can be reached by 
##'       the PFG (from 1 to 5)
##'       \item \strong{disp} : MEDIAN classes (from 1 to 7) based on 
##'       dispersal distances and types (Vittoz & Engler)
##'       \item \strong{light} : MEDIAN Ellenberg indicator value for light 
##'       (from 1 to 9)
##'       \item \strong{height} : MEAN PFG height (cm)
##'       \item \strong{palatability} : MEDIAN classes (from 0 to 3) (CBNA)
##'       \item \strong{longevity} : MEAN age of lifespan
##'       \item \strong{maturity} : MEAN age of maturity
##'     }
##'     }
##'   }
##' 
##' @source 
##' 
## END OF HEADER ###############################################################

data("PNE_PFG")

### HEADER #####################################################################
##' @title Ecrins National Park (PNE) data : building of Plant Functional Groups
##' 
##' @name PNE_PARAM
##'
##' @author Maya Guéguen
##' 
##' @description This dataset contains all data for the Ecrins National Park 
##' (PNE) area. It contains both the data to create the Plant Functional 
##' Groups (PFG) over this area 
##' (\emph{\href{https://mayagueguen.github.io/FATE-WEBSITE/papers/Boulangeat_2012_GCB.pdf}{Boulangeat et al. 2012 GCB}}), 
##' and the necessary files to build the \code{FATE-HD} simulation folder as well as 
##' the parameter files 
##' (\emph{\href{https://mayagueguen.github.io/FATE-WEBSITE/papers/Boulangeat_2014_GCB.pdf}{Boulangeat et al. 2014 GCB}}).
##' 
##' @format A \code{list} object with 2 elements :
##' 
##' \itemize{
##'   \item \strong{FATE_PARAM} : a \code{list} object with 7 elements to help 
##'   building the simulation files and folders to run a \code{FATE-HD} 
##'   simulation :
##'   \describe{
##'     \item{masks}{a \code{\link[raster]{stack}} object of dimension 
##'     \code{782 x 619} with a resolution of \code{100m} and Lambers (lcc) 
##'     projection, containing 8 mask layers with binary values (0 or 1) to be 
##'     used in a \code{FATE-HD} simulation :
##'     \itemize{
##'       \item \strong{maskEcrins} : simulation map, where occurs succession
##'       \item \strong{noDisturb} : perturbation map, when there is none
##'       \item \strong{mowing} : perturbation map, where occurs mowing
##'       \item \strong{grazing1} : perturbation map, where occurs light grazing
##'       \item \strong{grazing2} : perturbation map, where occurs extensive 
##'       grazing
##'       \item \strong{grazing3} : perturbation map, where occurs intensive 
##'       grazing
##'       \item \strong{grazingAll} :  perturbation map, where occurs grazing, 
##'       all types combined
##'     }
##'     }
##'     \item{HS_0}{a \code{\link[raster]{stack}} object of dimension 
##'     \code{782 x 619} with a resolution of \code{100m} and Lambers (lcc) 
##'     projection, containing 24 layers with probability values (between 0 and 
##'     1) representing Habitat Suitability for current time (0) for each PFG 
##'     and to be used in a \code{FATE-HD} simulation. \cr
##'     These maps are coming from Species Distribution Models and methods to 
##'     obtain them are described in Supplementary Materials of 
##'     \emph{\href{https://mayagueguen.github.io/FATE-WEBSITE/papers/Boulangeat_2014_GCB.pdf}{Boulangeat et al. 2014 GCB}}.}
##'     \item{HS_15}{same as HS_0 but 15 years after current time}
##'     \item{HS_30}{same as HS_0 but 30 years after current time}
##'     \item{HS_45}{same as HS_0 but 45 years after current time}
##'     \item{HS_60}{same as HS_0 but 60 years after current time}
##'     \item{HS_75}{same as HS_0 but 75 years after current time}
##'     \item{HS_90}{same as HS_0 but 90 years after current time}
##'     \item{strata_limits}{a \code{vector} of length \code{5} \cr
##'     containing height of stratum limits in centimeters \cr
##'     to be used with the \link{PRE_FATE.params_PFGsuccession} and 
##'     \link{PRE_FATE.params_PFGlight} functions \cr}
##'     \item{succ_light}{a \code{data.frame} of dimension \code{24 x 6} \cr
##'     containing traits for plant functional groups \cr
##'     obtained from the \link{PRE_FATE.speciesClustering_step3} function \cr
##'     to be used with the \link{PRE_FATE.params_PFGsuccession} and 
##'     \link{PRE_FATE.params_PFGlight} functions \cr}
##'     \item{disp}{a \code{data.frame} of dimension \code{24 x 4} \cr
##'     containing dispersal values (in meters) for plant functional groups \cr
##'     to be used with the \link{PRE_FATE.params_PFGdispersal} function \cr}
##'     \item{dist}{a \code{data.frame} of dimension \code{384 x 5} \cr
##'     containing response of plant functional groups to disturbances \cr
##'     to be used with the \link{PRE_FATE.params_PFGdisturbance} function \cr}
##'     \item{global}{}
##'   }
##' }
##' 
##' @source 
##' 
## END OF HEADER ###############################################################

data("PNE_PARAM")

### HEADER #####################################################################
##' @title Ecrins National Park (PNE) data : building of Plant Functional Groups
##' 
##' @name PNE_RESULTS
##'
##' @author Maya Guéguen
##' 
##' @description This dataset contains all data for the Ecrins National Park 
##' (PNE) area. It contains both the data to create the Plant Functional 
##' Groups (PFG) over this area 
##' (\emph{\href{https://mayagueguen.github.io/FATE-WEBSITE/papers/Boulangeat_2012_GCB.pdf}{Boulangeat et al. 2012 GCB}}), 
##' and the necessary files to build the \code{FATE-HD} simulation folder as well as 
##' the parameter files 
##' (\emph{\href{https://mayagueguen.github.io/FATE-WEBSITE/papers/Boulangeat_2014_GCB.pdf}{Boulangeat et al. 2014 GCB}}).
##' 
##' @format A \code{list} object with 2 elements :
##' 
##' 
##' @source 
##' 
## END OF HEADER ###############################################################

data("PNE_RESULTS")
