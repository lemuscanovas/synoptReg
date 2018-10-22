#' Raster conversion of the Synoptic Classification
#'
#' This function converts the dataframe of the synoptic classification data into a Raster Stack format.
#'
#' @param longitude Numeric vector containing longitudes
#' @param latitude Numeric vector containing latitudes
#' @param grouped_data Data frame. S-mode data frame containing an integer column with the weather types. i.e. output obtained from \code{synoptclas} function.
#'
#' @return a Raster Stack containing the circulation weather types.
#'
#' @examples
#' # Load data (mslp)
#' data(mslp)
#' # Converting our data, but without modifying time period
#' smode_mslp <- tidy_cuttime_nc(mslp, only_convert = TRUE)
#' # classification performance
#' smode_clas <- synoptclas(smode_mslp$smode_data, ncomp = 6)
#' # convert all the precipitation maps based on CWT to a raster stack
#' raster_clas(longitude = mslp$lon, latitude = mslp$lat,
#'             grouped_data = smode_clas$grouped_data)
#'
#' @export


raster_clas <- function(longitude, latitude, grouped_data) {

  mean_cwt <- list()

  for (ii in 1:length(unique(grouped_data$CWT))) {
    CWT <- subset(grouped_data, CWT == ii)
    CWT <- CWT[, -c(1:2)]
    CWT <- colMeans(CWT)/100
    mean_cwt[[ii]] <- CWT
  }

  # store results in df with all mean cwt
  df_all_cwt <- do.call(cbind.data.frame, mean_cwt)
  colnames(df_all_cwt) <- 1:length(unique(grouped_data$CWT))

  # lonlat generation
  lonlat <- expand.grid(lon=longitude,lat=latitude)

  # stacking raster cwt
  raster_cwt <- raster::stack()

  for (zz in 1:ncol(df_all_cwt)) {
    spatial_cwt <- raster::rasterFromXYZ(cbind.data.frame(lonlat, df_all_cwt[,zz]))
    raster_cwt <- raster::stack(raster_cwt, spatial_cwt)
  }
  names(raster_cwt) <- paste0("CWT", 1:ncol(df_all_cwt))

  return(raster_cwt)
}
