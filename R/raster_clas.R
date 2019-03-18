#' Raster conversion of the Synoptic Classification
#'
#' This function converts the dataframe of the synoptic classification data into a Raster Stack format.
#'
#' @param longitude Numeric vector containing longitudes
#' @param latitude Numeric vector containing latitudes
#' @param grouped_data Data frame. S-mode data frame containing an integer column with the circulation types. i.e. output obtained from \code{synoptclas} function.
#' @param option Integer (1 or 2), to manage latitude and longitude data when convert to raster. Try 2 if 1 is wrong and viceversa. Default is 1.
#'
#' @return a Raster Stack containing the circulation types.
#'
#' @examples
#' # Load data (mslp)
#' data(mslp)
#' # Converting our data, but without modifying time period
#' smode_mslp <- tidy_cuttime_nc(mslp, only_convert = TRUE)
#' # classification performance
#' smode_clas <- synoptclas(smode_mslp$smode_data, ncomp = 6)
#' # convert all the precipitation maps based on CT to a raster stack
#' raster_clas(longitude = mslp$lon, latitude = mslp$lat,
#'             grouped_data = smode_clas$grouped_data)
#'
#' @export


raster_clas <- function (longitude, latitude, grouped_data, option = 1) {
  mean_ct <- list()
  for (ii in 1:length(unique(grouped_data$CT))) {
    CT <- subset(grouped_data, CT == ii)
    CT <- CT[, -c(1:2)]
    CT <- colMeans(CT)
    mean_ct[[ii]] <- CT
  }

  # store results in df with all mean ct
  df_all_ct <- do.call(cbind.data.frame, mean_ct)
  colnames(df_all_ct) <- 1:length(unique(grouped_data$CT))

  if(option == 1){

    # lonlat generation
    lonlat <- expand.grid(lon = longitude, lat = latitude)

    # stacking raster ct
    raster_ct <- raster::stack()

    for (zz in 1:ncol(df_all_ct)) {
      spatial_ct <- raster::rasterFromXYZ(cbind.data.frame(lonlat,
                                                           df_all_ct[, zz]))
      raster_ct <- raster::stack(raster_ct, spatial_ct)
    }

  } else if(option == 2){

    lonlat <- expand.grid(lon = latitude, lat = longitude)
    raster_ct <- raster::stack()

    for (zz in 1:ncol(df_all_ct)) {
      spatial_ct <- raster::rasterFromXYZ(cbind.data.frame(lonlat,
                                                           df_all_ct[, zz]))
      raster_ct <- raster::stack(raster_ct, spatial_ct)
    }

    raster_ct <- raster::flip(raster::flip(raster::t(raster_ct),2),1)

  }

  names(raster_ct) <- paste0("CT", 1:ncol(df_all_ct))
  return(raster_ct)
}
