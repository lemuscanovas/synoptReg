synoptReg<img src="img/logo.png" align="right"  height="160" />
=========================================================

Overview
--------

**synoptReg** is an open source package fot computing synoptic climate classifications and spatial regionalizations of environmental data.

Installation
------------

``` r
# To install the latest version from Github:
# install.packages("devtools")
devtools::install_github("lemuscanovas/synoptReg")
```

Functions
---------

synoptReg has two functions related to read and format data:

-   `read_nc` reads a NetCDF file to extract the atmospheric or environmental variable, longitudes, latitudes and dates. A continuous NetCDF without date gaps is required.
-   `tidy_cuttime_nc` formats the 3D-array output from \code{read_nc} function to an S-mode dataframe (variables = grid points, observations = days). Optionally, you can set the time period between specific years and/or specify if you want work with the full year or only with 3 - month season.


synoptReg also has two functions to performe the PCA:

-   `pca_decision` abc.
-   `synoptclas` abc.


Usage
-----

```r
library(rayshader)
library(magrittr)

#Here, I load a map for the River Derwent in Tasmania with the raster package:
localtif = raster::raster("tasmania.tif")

#And convert it to a matrix:
elmat = matrix(raster::extract(localtif,raster::extent(localtif),buffer=1000),
               nrow=ncol(localtif),ncol=nrow(localtif))

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()
```
![](tools/readme/first.jpg)

```r
#sphere_shade can shift the sun direction:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  plot_map()
```
![](tools/readme/second.jpg)

```r
#detect_water and add_water adds a water layer to the map:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  plot_map()
```

![](tools/readme/third.jpg)
``` r
#And we can add a raytraced layer from that sun direction as well:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat)) %>%
  plot_map()
```

![](tools/readme/fourth.jpg)
``` r
elmat %>%
  sphere_shade( texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat)) %>%
  add_shadow(ambient_shade(elmat)) %>%
  plot_map()
```

![](tools/readme/fifth.jpg)

Rayshader also supports 3D mapping by passing a texture map (either external or one produced by rayshader) into the `plot_3d` function.

``` r
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat)) %>%
  add_shadow(ambient_shade(elmat)) %>%
  plot_3d(elmat)
```

![](tools/readme/sixth.jpg)

You can also easily add a water layer by setting `water = TRUE` (and setting `waterdepth` if the water level is not 0). You can customize the appearance and transparancy of the water layer via arguments to `plot_3d`. Here's an example using the built-in example bathymetric/topographic data of Monterey Bay, CA `montereybay` (zscale for a 1-to-1 ratio  using `montereybay` would be 200, but in plot_3d is set to 50 to give a 4x exaggerated height in the z-direction) :

``` r
montereybay %>% 
    sphere_shade(texture = "imhof1") %>% 
    add_shadow(ray_shade(montereybay,zscale=200)) %>%
    add_shadow(ambient_shade(montereybay,zscale=200)) %>%
    plot_3d(montereybay, water=TRUE, zscale=50, theta=-45,
            waterdepth = 0, wateralpha = 0.6, watercolor = "#88DDFF",
            waterlinecolor = "white", waterlinealpha = 0.5)
```

![](tools/readme/seventh.jpg)
