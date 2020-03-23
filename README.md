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

-    Compute an objective **synoptic classification** to obtain the main atmospheric patterns, the so-called weather types, of a given region. Two approaches are provided:

     - ***Circulation-To-Environment***: First establishes the main circulation         types for a long time series and then characterises an environmental
     variable (i.e. precipitation,NO<sub>2</sub>,O<sub>3</sub>,...) based on
     the previous circulation types.
  
     - ***Environment-To-Circulation***: First categorises the environmental
     variable (e.g. precipitation, temperature,...) and then characterises
     the synoptic patterns prevailing under specific environmental
     conditions (e.g. days with elevated temperatures, torrential rainfall
     events). 
  
- Represent the **impact of each weather type** on an environmental variable (continuous): precipitation, temperature, pollutants, ...

- Define a **categorical regionalization** of this environmental variable. Each region will be independent and with specific characteristics.  

## How it works

### Installation

``` r
# To install the CRAN version (1.0.1):
install.packages("synoptReg")

# To install the latest version from Github:
# install.packages("remotes")
remotes::install_github("lemuscanovas/synoptReg")
```

### Vignettes

Interested in learning how to use `synoptReg`? Visit the package website and read the articles:

* [synoptReg website](https://lemuscanovas.github.io/synoptreg/)


## Package citation

Using synoptReg for research publication?  Please **cite it**! I'm an early career scientist and every citation matters.

***Lemus-Canovas, M., Lopez-Bustins, J.A., Martin-Vide, J., Royé, D.***, 2019. *synoptReg: An R package for computing a synoptic climate classification and a spatial regionalization of environmental data*. Environmental Modelling & Software, Vol. 118,114-119pp, ISSN 1364-8152, https://doi.org/10.1016/j.envsoft.2019.04.006

## Contact

Feel free to contact me: mlemus@ub.edu
