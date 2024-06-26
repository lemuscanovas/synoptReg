synoptReg <img src="img/logo.png" align="right" alt="" width="140" />
=========================================================
# `synoptReg`: Synoptic Climate Classification and Spatial Regionalization of Environmental Data


[![CRAN status](https://www.r-pkg.org/badges/version/synoptReg)](https://cran.r-project.org/package=synoptReg)
[![](http://cranlogs.r-pkg.org/badges/grand-total/synoptReg)](http://cran.rstudio.com/web/packages/synoptReg/index.html)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

## Overview

**synoptReg** is an open source package for computing synoptic climate classifications and spatial regionalizations of environmental data. Package website: lemuscanovas.github.io/synoptreg/

----

Contents:

* [Why this package](#why-this-package)
* [How it works](#how-it-works)
  * [Installation](#installation)
  * [Vignettes](#vignettes)
* [Package citation](#package-citation)
* [Contact](#contact)

----

## Why this package
The interaction between the troposphere and the environment is well known, and how this can condition human activities on some occasions. These situations can occur with an anticyclonic block that causes an increase in the concentration of NO<sub>2</sub>; or when a low-pressure area causes abundant rainfall over urban areas, etc.
This package is intended to be a roadmap for territorial management on a regional scale and to anticipate the management of adverse situations for the population, due to pollution, as well as extreme weather conditions.
In short, `synoptReg` allows to:

- Compute an objective **synoptic classification** to obtain the main atmospheric patterns, the so-called weather types, of a given region. Two approaches are provided:

     - ***Circulation-To-Environment***: First establishes the main circulation         types for a long time series and then characterises an environmental/meteorological
     variable (i.e. precipitation,NO<sub>2</sub>,O<sub>3</sub>,...) based on
     the previous circulation types.
  
     - ***Environment-To-Circulation***: characterises the synoptic patterns prevailing under extreme specific environmental conditions. (e.g. torrential rainfall days).
    
     
     * Methods implemented: PCA-based, Automatic Lamb (Jenkinson & Collison)
  
- Represent the **impact of each weather type** on an environmental/meteorological variable: precipitation, temperature, pollutants, ...

- Compute a **clustering** to get a spatial summary of the main regions of such a variable.

## How it works

### Installation
<span style="color:darkred;">Major update in the latest version due to the removal of rgdal, maptools and sp dependencies!</span>

``` r
# To install the latest version from Github (1.3.0):
# install.packages("remotes")
remotes::install_github("lemuscanovas/synoptReg")
```

### Vignettes

Interested in learning how to use `synoptReg`? Visit the package website and read the articles:

* [synoptReg website](https://lemuscanovas.github.io/synoptreg/)


## Package citation

Using synoptReg for research publication?  Please **cite it**!

***Lemus-Canovas, M., Lopez-Bustins, J.A., Martin-Vide, J., Royé, D.***, 2019. *synoptReg: An R package for computing a synoptic climate classification and a spatial regionalization of environmental data*. Environmental Modelling & Software, Vol. 118,114-119pp, ISSN 1364-8152, https://doi.org/10.1016/j.envsoft.2019.04.006

## Contact

Feel free to contact me: lemuscanovas@gmail.com
