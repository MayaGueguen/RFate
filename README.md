[![Travis build status](https://travis-ci.org/MayaGueguen/RFate.svg?branch=master)](https://travis-ci.org/MayaGueguen/RFate)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/MayaGueguen/RFate?branch=master&svg=true)](https://ci.appveyor.com/project/MayaGueguen/RFate)
[![Coverage status](https://codecov.io/gh/MayaGueguen/RFate/branch/master/graph/badge.svg)](https://codecov.io/github/MayaGueguen/RFate?branch=master)

___________________________________________________________________________________________________

# <font color=#52962b> <i class="fa fa-pagelines"></i> `RFate` package - to be used with `FATE-HD` </font>


This package aims at presenting support functions to the software `FATE-HD`.  
Functions are classified in two categories :

- `PRE_FATE.` functions :
    - to build Plant Functional Groups
    - to prepare parameter files

- `POST_FATE.` functions :
    - to organize results from `FATE-HD`
    - to plot graphics
    - etc
    
___________________________________________________________________________________________________

___________________________________________________________________________________________________

## <font color="#068f96"><i class="fa fa-battery-quarter"></i> `PRE_FATE` - build Plant Functional Groups (PFG)</font>

___________________________________________________________________________________________________

"*The recurring suggestions are that models should explicitly (i) include spatiotemporal dynamics; (ii) consider
multiple species in interactions and (iii) account for the processes shaping biodiversity distribution.*"

`FATE-HD` is a "*a biodiversity model that meets this challenge at regional scale by combining phenomenological and process-based approaches and using well-defined* **_plant_ _functional_ _group_** ". ([Boulangeat, 2014](http://www.will.chez-alice.fr/pdf/BoulangeatGCB2014.pdf "Boulangeat, I., Georges, D., Thuiller, W., FATE-HD: A spatially and temporally explicit integrated model for predicting vegetation structure and diversity at regional scale. Global Change Biology, 20, 2368–2378."))

___________________________________________________________________________________________________

A plant functional group, or **PFG**, is "*A set of representative species is classified based on key biological characteristics, to determine groups of species sharing ecological strategies*" ([Boulangeat, 2012](http://j.boulangeat.free.fr/pdfs/Boulangeat2012_GCB_published.pdf "Boulangeat, I., Philippe, P., Abdulhak, S., Douzet, R., Garraud, L., Lavergne, S., Lavorel, S., Van Es J., Vittoz, P. and Thuiller, W. Improving plant functional groups for dynamic models of biodiversity: at the crossroad between functional and community ecology. Global Change Biology, 18, 3464-3475.")).
PFGs are based on their distribution, physiological characteristics, competition traits...


### What are the main steps of this process ?

1. **Selection of dominant species**  
with the function [PRE_FATE.selectDominant](https://mayagueguen.github.io/RFate/reference/PRE_FATE.selectDominant.html)  

2. **Overlap of species climatic niches**  
with either Principal Component Analysis (PCA) or Species Distribution Models (SDM)

3. **Calculation of species pairwise distance**  
by combining overlap and functional distances with the function [PRE_FATE.speciesDistance](https://mayagueguen.github.io/RFate/reference/PRE_FATE.speciesDistance.html)

4. **Clustering of species :**  
- calculate all possible clusters, and the corresponding evaluation metrics  
with the function [PRE_FATE.speciesClustering_step1](https://mayagueguen.github.io/RFate/reference/PRE_FATE.speciesClustering_step1.html)
- choose the best number of clusters from the previous step and find determinant species  
with the function [PRE_FATE.speciesClustering_step2](https://mayagueguen.github.io/RFate/reference/PRE_FATE.speciesClustering_step2.html)


### What do you need ?

*1. Selection of dominant species*
- Gather **occurrences** for all species within the studied area
- Identify **dominant species** based on abundances and frequençy of sampling

*2. Overlap of species climatic niches :* 
- *Option 1: Principal Component analysis*
    - Gather **environmental data** for the studied area
    - Compute **PCA** over environment to create a *climatic space*
    - Calculate the **density of each species** within this *climatic space* from the PCA
    - For each pair of species, compute the **overlap** of the 2 considered species within the *climatic space*
- *Option 2: Species Distribution Models*
    - Gather **environmental data** for the studied area
    - For each dominant species, compute a **species distribution model** (SDM)  
    combining environmental data and occurrences to determine the *climatic niche* of the species
    - With these SDMs, calculate the **niche overlap** of each pair of species

*3. Calculation of species pairwise distance*  
- Gather **traits data** for all dominant species within the studied area  
(traits need to be related to fundamental process of growth : light tolerance, dispersal, height...)
- Compute **dissimilarity distances** between pairs of species based on these traits and taking also into account the overlap of the 2 species within the *climatic space* (see previous step)

*4. Clustering of species :*  
- Using the **dissimilarity distances** from previous step, apply hierarchical clustering

___________________________________________________________________________________________________

___________________________________________________________________________________________________

## <font color="#068f96"> <i class="fa fa-battery-half"></i> `PRE_FATE` - build parameter files </font>

### The different type of parameters and *flags*

`FATE-HD` requires a quite large number of parameters, which are stored into `.txt` files, presented to and recovered by the software. These **parameters** can be of 3 types :

1. **Filenames**, to guide the application to other parameter files that should be read
2. These filenames either correspond to :
    - other parameter files that contain **values** to be actually read and used
    - **raster** files, with the extension `.tif` (lighter) or `.img`

<br/>

To enumerate these settings, **2 types of flag** can be found and used within the parameter files :

1. To give one or several links to other files containing parameter values or to raster files : **`--PARAM_NAME--`**
```Shell
--GLOBAL_PARAMS--
SAVE_OBJ/DATA/Global_parameters.txt
--MASK--
SAVE_OBJ/DATA/MASK/mask_studiedArea.asc
--PFG_LIFE_HISTORY_PARAMS--
SAVE_OBJ/DATA/PFGS/SUCC/SUCC_PFG1.txt
SAVE_OBJ/DATA/PFGS/SUCC/SUCC_PFG2.txt
...
```
In this way, each parameter can have several values (filenames), and **each line corresponds to a value**. The transition to a new parameter is made thanks to the presence of a new flag on the next line.

2. To give parameter values : **`PARAM_NAME`**
```Shell
NAME H2_dryGrass
MATURITY 3
LONGEVITY 11
MAX_ABUNDANCE 1
IMM_SIZE 4
CHANG_STR_AGES 0 10000 10000 10000 10000
```
Each line corresponds to a parameter, given by the **flag** (parameter name in capital letters) **followed by all values linked to this flag on the same line**. Each value has to be separated from another by a **space**.


### Which files for which settings ?

The function [PRE_FATE.skeletonDirectory](https://mayagueguen.github.io/RFate/reference/PRE_FATE.skeletonDirectory.html) allows to create a user-friendly directory tree to store all parameter files and data.

<br/>

*1. Simulation parameterization*
- **Global parameters** : related to the simulation definition  
(number of PFG and strata, simulation duration, computer resources, manage abundance values, modules loaded...)  
with the function [PRE_FATE.params_globalParameters](https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_globalParameters.html)
- **Years to save abundance rasters and simulation outputs** with the function [PRE_FATE.params_saveYears](https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_saveYears.html)
- **Years and files to change rasters** for the succession, habitat suitability or disturbance modules  
with the function [PRE_FATE.params_changingYears](https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_changingYears.html)

*2. For each PFG : behavior and characteristics*
- **Succession files** : related to the life history with the function [PRE_FATE.params_PFGsuccession](https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGsuccession.html)
- **Dispersal files** : related to the dispersal ability with the function [PRE_FATE.params_PFGdispersal](https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGdispersal.html)
- **Light files** : related to the light competition with the function [PRE_FATE.params_PFGlight](https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGlight.html)
- **Soil files** : related to the soil competition with the function [PRE_FATE.params_PFGsoil](https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGsoil.html)
- **Disturbance files** : related to the response to perturbations in terms of resprouting and mortality  
with the function [PRE_FATE.params_PFGdisturbance](https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGdisturbance.html)

*3. Parameter management*
- **ParamSimulation file** : containing all links to the files created with the previous functions.  
This is the file that will be given as the only argument to the `FATE-HD` executable file into the command line.  
It can be created with the function [PRE_FATE.params_simulParameters](https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_simulParameters.html)

___________________________________________________________________________________________________

___________________________________________________________________________________________________

## <font color="#068f96"> <i class="fa fa-battery-three-quarters"></i> `POST_FATE` - evaluation of simulation </font>

<br/>

*0. Transformation of outputs*
- **Create binary maps** : transform abundance values into presence / absence
with the function [POST_FATE.relativeAbund_presenceAbsence](https://mayagueguen.github.io/RFate/reference/POST_FATE.relativeAbund_presenceAbsence.html)

*A. Evolution of simulation through time*
- **Abundance over all studied area** : with the function [POST_FATE.graphic_evolutionCoverage](https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_evolutionCoverage.html)
- **Abundance within some pixels** : with the function [POST_FATE.graphic_evolutionAbund_pixels](https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_evolutionAbund_pixels.html)
- **Light within some pixels over all studied area** : with the function [POST_FATE.graphic_evolutionLight_pixels](https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_evolutionLight_pixels.html)
- **Soil within some pixels over all studied area** : with the function [POST_FATE.graphic_evolutionSoil_pixels](https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_evolutionSoil_pixels.html)

*B. Visualization of outputs*
- **PFG richness** : create map of number of PFG alive within each pixel  
with the function [POST_FATE.graphic_mapPFGrichness](https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_mapPFGrichness.html)
- **PFG cover** : create map of PFG abundance above 1.5 meters within each pixel  
with the function [POST_FATE.graphic_mapPFGcover](https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_mapPFGcover.html)
- **PFG CWM of light** : create map of light community weighted mean within each pixel  
with the function [POST_FATE.graphic_mapPFGlight](https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_mapPFGlight.html)
- **PFG CWM of soil** : create map of soil community weighted mean within each pixel  
with the function [POST_FATE.graphic_mapPFGsoil](https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_mapPFGsoil.html)

*C. Evaluation of outputs*
- **PFG outputs vs Habitat suitability** : compare binary maps (outputs) with habitat suitability maps (inputs)  
with the function [POST_FATE.graphic_mapPFGvsHS](https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_mapPFGvsHS.html)
- **Validation statistics** : calculate evaluation metrics (TSS, AUC) for each PFG  
with the function [POST_FATE.graphic_validationStatistics](https://mayagueguen.github.io/RFate/reference/POST_FATE.graphic_validationStatistics.html)
