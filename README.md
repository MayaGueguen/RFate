[![Travis build status](https://travis-ci.org/MayaGueguen/RFate.svg?branch=master)](https://travis-ci.org/MayaGueguen/RFate)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/MayaGueguen/RFate?branch=master&svg=true)](https://ci.appveyor.com/project/MayaGueguen/RFate)
[![Coverage status](https://codecov.io/gh/MayaGueguen/RFate/branch/master/graph/badge.svg)](https://codecov.io/github/MayaGueguen/RFate?branch=master)

# RFate package - to be used with `FATE-HD`

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

## 1. PRE_FATE - build Plant Functional Groups (PFG)    

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

## 2. PRE_FATE - build parameter files  

___________________________________________________________________________________________________

## 3. POST_FATE - evaluation of simulation  

