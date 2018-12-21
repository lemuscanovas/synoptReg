#' Raster conversion of environmental data based on CT
#'
#' This function converts the dataframe of the environmental data based on the synoptic classification into a Raster Stack format.
#'
#' @param longitude Numeric vector containing longitudes
#' @param latitude Numeric vector containing latitudes
#' @param clas Integer containing the results of the synoptic classification.
#' @param grid_data Data frame containing the environmental data (i.e. precipitation, temperature, PM10, etc.)
#' @param option Integer (1 or 2), to manage latitude and longitude data when convert to raster. Try 2 if 1 is wrong and viceversa. Default is 1.
#' @param na.rm Logical. If TRUE, all the grid points are used to calculate the daily mean although NA exists. If FALSE, only grid points with the complete serie are used to compute the daily mean. Default is TRUE.
#'
#' @return a Raster Stack containing the environmental grids based on the weather types.
#'
#' @examples
#' # Load data (precp_grid)
#' data(precp_grid)
#' # Converting our data, but without modifying time period
#' smode_mslp <- tidy_cuttime_nc(mslp, only_convert = TRUE)
#' precp_data <- tidy_cuttime_nc(precp_grid, only_convert = TRUE)
#' # classification performance
#' smode_clas <- synoptclas(smode_mslp$smode_data, ncomp = 6)
#' # convert all the precipitation maps based on CT to a raster stack
#' raster_precp <- raster_ct2env(longitude = precp_grid$lon,
#'                 latitude = precp_grid$lat, clas = smode_clas$clas,
#'                 grid_data = precp_data$smode_data, option = 2)
#'
#' @export

raster_ct2env <- function(longitude, latitude, clas, grid_data, option = 1,
                           na.rm = TRUE) {

  mean_ct <- list()

  for (ii in 1:length(unique(clas))) {
    grid_ct <- cbind.data.frame(clas, grid_data)
    colnames(grid_ct)[1] <- "CT"
    CT <- subset(grid_ct, CT == ii)
    CT <- CT[, -c(1)]
    CT <- colMeans(CT, na.rm = na.rm)
    mean_ct[[ii]] <- CT
  }

  # store results in df with all mean_ct
  df_all_ct <- do.call(cbind.data.frame, mean_ct)
  colnames(df_all_ct) <- 1:length(unique(clas))

  spatial_matrix_pcp_ct <- list()
  message("converting grid_data to raster...")

  if (option == 1) {
    for (ii in 1:length(unique(clas))) {
      spatial_matrix_pcp_ct[[ii]] <- apply(matrix(df_all_ct[, ii], nrow = length(longitude),
                                                   ncol = length(latitude)), 1, rev)
    }

    warning("Process completed! BUT if error plot is displayed when you use rasterPCA, try option = 2")

  } else if (option == 2) {
    for (ii in 1:length(unique(clas))) {
      spatial_matrix_pcp_ct[[ii]] <- matrix(df_all_ct[, ii], nrow = length(latitude),
                                             ncol = length(longitude))
    }

    warning("Process completed! BUT if error plot is displayed when you use rasterPCA, try option = 1")
  }

  grid_list <- spatial_matrix_pcp_ct
  rastlist <- list()

  for (ii in 1:length(grid_list)) {
    mat2rast <- grid_list[[ii]]
    matraster <- raster::raster(mat2rast)
    rastlist[[ii]] <- matraster

  }

  #raster creation
  raststack <- raster::stack(rastlist)
  if (option == 2) {
    raster_ct_env <- raster::flip(raststack, 2)
  } else {
    raster_ct_env <- raststack
  }

  # seting raster extent
  ext <- raster::extent(min(longitude), max(longitude), min(latitude), max(latitude))
  raster::extent(raster_ct_env) <- ext
  names(raster_ct_env) <- paste0("CT", 1:length(grid_list))

  return(raster_ct_env)
}
