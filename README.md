[![Travis build status](https://travis-ci.org/MayaGueguen/RFate.svg?branch=master)](https://travis-ci.org/MayaGueguen/RFate)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/MayaGueguen/RFate?branch=master&svg=true)](https://ci.appveyor.com/project/MayaGueguen/RFate)
[![Coverage status](https://codecov.io/gh/MayaGueguen/RFate/branch/master/graph/badge.svg)](https://codecov.io/github/MayaGueguen/RFate?branch=master)

<style>
pre.bash {
 background-color: black;
 color: #9ea1a3;
 font-family: Consolas,Monaco,Lucida Console,Liberation Mono,DejaVu Sans Mono,Bitstream Vera Sans Mono,Courier New, monospace;
}
pre.grey {
 background-color: white;
 border-style: solid;
 border-color: #8b8d8f;
 color: #8b8d8f;
 font-family: Consolas,Monaco,Lucida Console,Liberation Mono,DejaVu Sans Mono,Bitstream Vera Sans Mono,Courier New, monospace;
}
</style>


<br/>


## <i class="fa fa-tools"></i> Installing `RFate` package

From [GitHub](https://github.com/mayagueguen/RFate) using [devtools](https://cran.r-project.org/web/packages/devtools/index.html) :

<pre class = "bash">
library(devtools)
devtools::install_github(repo="mayagueguen/RFate")
</pre>

<br/><br/>



## <i class="fas fa-shoe-prints"></i> `RFate` workflow

<br/>

**0. Understand how `FATE-HD` works :**

- [the litterature](articles/fate-hd_tutorial_0_publications.html)
- [and the software](articles/fate-hd_tutorial_0_modelling_framework.html)

**1. Building PFG :**

- [the principle](articles/fate-hd_tutorial_1_PFG.html)
- [and the tools](articles/rfate_tutorial_1_PFG.html)
    
**2. Run a `FATE-HD` simulation :**

- [understand how to run a simulation,](articles/fate-hd_tutorial_2_RUN_SIMULATION.html)
- [the different modules that can be used,](articles/fate-hd_tutorial_3_MODULES.html)
- [and how to prepare the corresponding parameter files](articles/rfate_tutorial_2_params.html)
    
**3. Analyze the outputs :**

- [transform results and produce graphics](articles/rfate_tutorial_3_graphics.html)


<img src="articles/pictures/SCHEMA_FATE_WORKFLOW.png" alt="Main workflow" style="width:800px;"></img>
