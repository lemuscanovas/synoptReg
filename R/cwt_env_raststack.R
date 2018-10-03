#' Raster conversion of environmental data based on CWT
#'
#' This function convert the dataframe of the environmental data based on the synoptic classification into a raster stack format.
#'
#' @param longitude Numeric vector containing longitudes
#' @param latitude Numeric vector containing latitudes
#' @param grid_data Data frame containing the environmental data (i.e. precipitation, temperature, PM10, etc.)
#' @param cluster_data Integer containing the results of the synoptic classification.
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
#' # convert all the precipitation maps based on CWT to a raster stack
#' raster_precp <- cwt_env_raststack(longitude = precp_grid$lon,
#'                 latitude = precp_grid$lat, grid_data = precp_data$smode_data,
#'                 cluster_data = smode_clas$clas, option = 2)
#' @export

cwt_env_raststack <- function(longitude, latitude, grid_data, cluster_data, option = 1,
                      na.rm = T) {

    all_pcp_by_cwt <- list()
    message("subsetting grid_data by cwt...")

    for (ii in 1:length(unique(cluster_data))) {
        grid_cwt <- cbind.data.frame(cluster_data, grid_data)
        colnames(grid_cwt)[1] <- "cwt"
        pcp_by_cwt <- subset(grid_cwt, grid_cwt$cwt == ii)
        pcp_by_cwt <- pcp_by_cwt[, -1]
        all_pcp_by_cwt[[ii]] <- colMeans(pcp_by_cwt, na.rm = na.rm)
    }

    df_all_pcp_by_cwt <- do.call(cbind.data.frame, all_pcp_by_cwt)
    colnames(df_all_pcp_by_cwt) <- 1:length(unique(cluster_data))

    spatial_matrix_pcp_cwt <- list()
    message("converting grid_data to raster...")

    if (option == 1) {
        for (ii in 1:length(unique(cluster_data))) {
            spatial_matrix_pcp_cwt[[ii]] <- apply(matrix(df_all_pcp_by_cwt[, ii], nrow = length(longitude),
                ncol = length(latitude)), 1, rev)
        }

        warning("Process completed! BUT if error plot is displayed when you use rasterPCA, try option = 2")

    } else if (option == 2) {
        for (ii in 1:length(unique(cluster_data))) {
          spatial_matrix_pcp_cwt[[ii]] <- matrix(df_all_pcp_by_cwt[, ii], nrow = length(latitude),
                ncol = length(longitude))
        }

        warning("Process completed! BUT if error plot is displayed when you use rasterPCA, try option = 1")
    }

    grid_list <- spatial_matrix_pcp_cwt
    rastlist <- list()

    for (ii in 1:length(grid_list)) {
        mat2rast <- grid_list[[ii]]
        matraster <- raster::raster(mat2rast)
        rastlist[[ii]] <- matraster

    }

    #raster creation
    raststack <- raster::stack(rastlist)
    if (option == 2) {
        raststack <- raster::flip(raststack, 2)
    } else {
        raststack <- raststack
    }

    # seting raster extent
    ext <- raster::extent(min(longitude), max(longitude), min(latitude), max(latitude))
    raster::extent(raststack) <- ext
    names(raststack) <- paste0("CWT", 1:length(grid_list))

    return(raststack)
}
