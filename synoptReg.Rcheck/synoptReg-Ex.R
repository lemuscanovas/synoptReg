pkgname <- "synoptReg"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "synoptReg-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('synoptReg')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("cwt_env_raststack")
### * cwt_env_raststack

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cwt_env_raststack
### Title: Raster conversion of environmental data based on CWT
### Aliases: cwt_env_raststack

### ** Examples

# Load data (precp_grid)
data(precp_grid)
# Converting our data, but without modifying time period
smode_mslp <- tidy_cuttime_nc(mslp, only_convert = TRUE)
precp_data <- tidy_cuttime_nc(precp_grid, only_convert = TRUE)
# classification performance
smode_clas <- synoptclas(smode_mslp$smode_data, ncomp = 6)
# convert all the precipitation maps based on CWT to a raster stack
raster_precp <- cwt_env_raststack(longitude = precp_grid$lon,
                latitude = precp_grid$lat, grid_data = precp_data$smode_data,
                cluster_data = smode_clas$clas, option = 2)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cwt_env_raststack", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mslp")
### * mslp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mslp
### Title: Mean Sea Level pressure files
### Aliases: mslp
### Keywords: datasets

### ** Examples

data(mslp)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mslp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pca_decision")
### * pca_decision

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pca_decision
### Title: PCA decision
### Aliases: pca_decision

### ** Examples

# Load data (mslp)
data(mslp)
# Converting our data into a S-mode, but without modifying time period
smode_mslp <- tidy_cuttime_nc(mslp, only_convert = TRUE)
# PCA decision performance
info_pca <- pca_decision(smode_mslp$smode_data)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pca_decision", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_clas")
### * plot_clas

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_clas
### Title: Synoptic classification plot
### Aliases: plot_clas

### ** Examples

# Load data (mslp)
data(mslp)
# Converting our data into a S-mode, but without modifying time period
smode_mslp <- tidy_cuttime_nc(mslp, only_convert = TRUE)
# classification performance
smode_clas <- synoptclas(smode_mslp$smode_data, ncomp = 6)
# Plot circulation weather type number 3
plot_clas(longitude = mslp$lon, latitude = mslp$lat,
          grouped_data = smode_clas$grouped_data,
          cwt_number = 3)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_clas", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_env")
### * plot_env

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_env
### Title: Environmental data plot based on CWT
### Aliases: plot_env

### ** Examples

# Load data (precp_grid and mslp)
data(precp_grid)
data(mslp)
# Converting our data, but without modifying time period
smode_mslp <- tidy_cuttime_nc(mslp, only_convert = TRUE)
precp_data <- tidy_cuttime_nc(precp_grid, only_convert = TRUE)
# classification performance
smode_clas <- synoptclas(smode_mslp$smode_data, ncomp = 6)
# Plot precipitation data based on cwt 3
plot_env(longitude = precp_grid$lon, latitude = precp_grid$lat,
         cluster_data = smode_clas$clas, grid_data = precp_data$smode_data,
         cwt_number = 3, option = 2, divide_units = 10, legend.lab = "mm")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_env", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("precp_grid")
### * precp_grid

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: precp_grid
### Title: Daily precipitation grid of Balearic Islands (Spain)
### Aliases: precp_grid
### Keywords: datasets

### ** Examples

data(precp_grid)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("precp_grid", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("synoptclas")
### * synoptclas

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: synoptclas
### Title: Synoptic classification
### Aliases: synoptclas

### ** Examples

# Load data (mslp)
data(mslp)
# Converting our data into a S-mode, but without modifying time period
smode_mslp <- tidy_cuttime_nc(mslp, only_convert = TRUE)
# classification performance
smode_clas <- synoptclas(smode_mslp$smode_data, ncomp = 6)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("synoptclas", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidy_cuttime_nc")
### * tidy_cuttime_nc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidy_cuttime_nc
### Title: Format a 3D-array to a S-mode data frame and set the time period
### Aliases: tidy_cuttime_nc

### ** Examples

# Load data (mslp or precp_grid)
data(mslp)
# Converting our data into a S-mode, but without modifying time period
smode_mslp <- tidy_cuttime_nc(mslp, only_convert = TRUE)
# matrix conversion and setting time period for winters between 2001 and 2010
smode_mslp <- tidy_cuttime_nc(mslp, only_convert = FALSE, season = "winter",
                             initial_year = 2001, end_year = 2010)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidy_cuttime_nc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
