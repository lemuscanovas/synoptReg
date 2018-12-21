#' Raster conversion of the Synoptic Classification
#'
#' This function converts the dataframe of the synoptic classification data into a Raster Stack format.
#'
#' @param longitude Numeric vector containing longitudes
#' @param latitude Numeric vector containing latitudes
#' @param grouped_data Data frame. S-mode data frame containing an integer column with the circulation types. i.e. output obtained from \code{synoptclas} function.
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


raster_clas <- function(longitude, latitude, grouped_data) {

  mean_ct <- list()

  for (ii in 1:length(unique(grouped_data$CT))) {
    CT <- subset(grouped_data, CT == ii)
    CT <- CT[, -c(1:2)]
    CT <- colMeans(CT)/100
    mean_ct[[ii]] <- CT
  }

  # store results in df with all mean ct
  df_all_ct <- do.call(cbind.data.frame, mean_ct)
  colnames(df_all_ct) <- 1:length(unique(grouped_data$CT))

  # lonlat generation
  lonlat <- expand.grid(lon=longitude,lat=latitude)

  # stacking raster ct
  raster_ct <- raster::stack()

  for (zz in 1:ncol(df_all_ct)) {
    spatial_ct <- raster::rasterFromXYZ(cbind.data.frame(lonlat, df_all_ct[,zz]))
    raster_ct <- raster::stack(raster_ct, spatial_ct)
  }
  names(raster_ct) <- paste0("CT", 1:ncol(df_all_ct))

  return(raster_ct)
}
