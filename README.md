[![Travis build status](https://travis-ci.org/MayaGueguen/RFate.svg?branch=master)](https://travis-ci.org/MayaGueguen/RFate)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/MayaGueguen/RFate?branch=master&svg=true)](https://ci.appveyor.com/project/MayaGueguen/RFate)
[![Coverage status](https://codecov.io/gh/MayaGueguen/RFate/branch/master/graph/badge.svg)](https://codecov.io/github/MayaGueguen/RFate?branch=master)

# RFate
RFate package - to be used with FATE-HD

---
title: "FATE-HD tutorial - Building PFGs"
author: "LECA Grenoble"
date: "08/12/2015"
output:
  html_document:
    highlight: espresso
    number_sections: yes
    theme: readable
    toc: yes
---

# Presentation
This tutorial has to be linked with other tutorials about `FATE-HD`.

___________________________________________________________________________________________________

"*The recurring suggestions are that models should explicitly (i) include spatiotemporal dynamics; (ii) consider
multiple species in interactions and (iii) account for the processes shaping biodiversity distribution.*"

`FATE-HD` is a "*a biodiversity model that meets this challenge at regional scale by combining phenomenological and process-based approaches and using well-defined* **_plant_ _functional_ _group_** ". ([Boulangeat, 2014](http://www.will.chez-alice.fr/pdf/BoulangeatGCB2014.pdf "Boulangeat, I., Georges, D., Thuiller, W., FATE-HD: A spatially and temporally explicit integrated model for predicting vegetation structure and diversity at regional scale. Global Change Biology, 20, 2368–2378."))

___________________________________________________________________________________________________

A plant functional group, or **PFG**, is "*A set of representative species is classified based on key biological characteristics, to determine groups of species sharing ecological strategies*" ([Boulangeat, 2012](http://j.boulangeat.free.fr/pdfs/Boulangeat2012_GCB_published.pdf "Boulangeat, I., Philippe, P., Abdulhak, S., Douzet, R., Garraud, L., Lavergne, S., Lavorel, S., Van Es J., Vittoz, P. and Thuiller, W. Improving plant functional groups for dynamic models of biodiversity: at the crossroad between functional and community ecology. Global Change Biology, 18, 3464-3475.")).
PFGs are based on their distribution, physiological characteristics, competition traits...

___________________________________________________________________________________________________

This tutorial aims at presenting a method to build such PFGs that can then be used in a `FATE-HD` simulation.

# What are the main steps of this process ?

*1. Overlap of species climatic niches :*

- Gather **environmental data** for the studied area
- Compute **PCA** over environment to create a *climatic space*

- Gather **occurrences** for all species within the studied area
- Identify **dominant species** based on abundances and frequençy of sampling
- Calculate the **density of each species** within this *climatic space* from the PCA

- For each pair of species, compute the **overlap** of the 2 considered species within the *climatic space*

OR

- Gather **environmental data** for the studied area
- Gather **occurrences** for all species within the studied area
- Identify **dominant species** based on abundances and frequençy of sampling

- For each dominant species, compute a **species distribution model** (SDM)  
combining environmental data and occurrences to determine the *climatic niche* of the species
- With these SDMs, calculate the **niche overlap** of each pair of species


*2. Clustering of species :*

- Gather **traits data** for all dominant species within the studied area  
(traits need to be related to fundamental process of growth : light tolerance, dispersal, height...)
- Compute **dissimilarity distances** between pairs of species based on these traits and taking also into account the overlap of the 2 species within the *climatic space*

# What do you need ?

*1. Overlap of species climatic niches :*

- a **mask** of the studied area (any raster format, with the defined area coded with 1)
- **environmental data** (any raster format, with preferentially same extent and projection)
- **sampled data** (per plot : coordinates, species sampled and abundances recorded)
    
*2. Clustering of species :*

- **traits data** :
    - height
    - light tolerance ([Ellenberg light values](https://www.brc.ac.uk/plantatlas/index.php?q=light-help))
    - palatability
    - dispersal ability ([Vittoz et Engler, 2007](http://www.wsl.ch/staff/niklaus.zimmermann/papers/Ecography_Engler_2009_SupMat.pdf "Vittoz P. and Engler R. 2007. Seed dispersal distances: a typology based on dispersal modes and plant traits. Botanica Helvetica, 117 (2), 109–124."))

*For further details about the data, please refer to [Boulangeat, 2012](http://j.boulangeat.free.fr/pdfs/Boulangeat2012_GCB_published.pdf "Boulangeat, I., Philippe, P., Abdulhak, S., Douzet, R., Garraud, L., Lavergne, S., Lavorel, S., Van Es J., Vittoz, P. and Thuiller, W. Improving plant functional groups for dynamic models of biodiversity: at the crossroad between functional and community ecology. Global Change Biology, 18, 3464-3475."). 

- **overlap** obtained from the first section

